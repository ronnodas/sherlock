#![allow(dead_code, clippy::todo, reason = "in progress")]

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::{assert_matches, error, fmt, mem};

use anyhow::Result;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools as _;
use mitsein::slice1::Slice1;

use crate::grid::Grid;
use crate::models::{Coord, Judgment};
use crate::solver::Engine;
use crate::solver::board::coordinates::{Set, Set1, SetExpr1, SetOp, SetOp1};
use crate::solver::hint::{BoundOrNot, Comparison, Hint, Number};
use crate::solver::inference::count::{CountSet, EndPoint, Interval};
use crate::solver::inference::split::{ByJudgment, SetSimplify};

mod count;
mod split;

const MAX_BIFURCATION_DEPTH: u8 = 1;

#[derive(Debug, Default, Clone)]
struct InferenceSolver {
    known: Grid<Option<Judgment>>,
    new_knowledge: Vec<(Coord, Judgment)>,
    criminal_count: IndexMap<Set1, (CountSet, TheoremState)>,
    connected: IndexSet<(Set1, Judgment)>,
    difference: IndexMap<[Set1; 2], Interval<i8>>,
    sum: IndexMap<[Set1; 2], Interval<Number>>,
    generic: Vec<Hint>,
    depth: u8,
}

impl InferenceSolver {
    fn update_theorems_or_contradict(&mut self) -> Result<bool, Contradiction> {
        let updated = { self.process_generic_hints()? }
            || {
                // eprintln!("finding tight counts");
                self.extract_tight_counts()?
            }
            || {
                // eprintln!("simplifying counts");
                self.simplify_criminal_counts()?
            }
            || {
                // eprintln!("simplifying differences");
                self.simplify_differences()?
            }
            || {
                // eprintln!("simplifying connected hints");
                self.simplify_connected()?
            }
            || {
                // eprintln!("inferring counts from differences");
                self.counts_from_differences()?
            }
            || {
                // eprintln!("simplifying count hints");
                self.simplify_count_hints()?
            }
            || {
                // eprintln!("comparing count pairs");
                self.combine_criminal_counts()?
                // }
                // || {
                //     eprintln!("adding bounds from inclusion-exclusion");
                //     self.inclusion_exclusion_bounds()?
            }
            || {
                (self.depth <= MAX_BIFURCATION_DEPTH) && {
                    // eprintln!("bifurcating");
                    self.bifurcate()?
                }
            };
        Ok(updated)
    }

    fn add_count(&mut self, set: SetOp1, count: BoundOrNot) -> Result<bool, Contradiction> {
        let updated = match &set {
            SetOp1::Judged(inner, judgment) => match inner.as_ref() {
                &SetExpr1::Regular(set) => return self.add_criminal_count(set, *judgment, count),
                SetExpr1::Op(inner) => {
                    let minimum = count.minimum();
                    let updated = (minimum > 0)
                        && self.try_add_hint(&Hint::Count(
                            inner.clone(),
                            BoundOrNot::AtLeast(minimum),
                        ))?;
                    self.generic.push(Hint::Count(set, count));
                    updated
                }
            },
            SetOp1::Shift(inner, direction) => {
                let minimum = count.minimum();
                let updated = (minimum > 0) && {
                    let Some(pre_shift) = Set1::all().shift(direction.flip()).non_empty() else {
                        return Err(Contradiction);
                    };
                    let SetOp::NonEmpty(pre_shift) = inner.clone().intersect_set1(pre_shift) else {
                        return Err(Contradiction);
                    };
                    self.try_add_hint(&Hint::Count(pre_shift, BoundOrNot::AtLeast(minimum)))?
                };
                self.generic.push(Hint::Count(set, count));
                updated
            }
            SetOp1::Intersection(vec, fixed) => {
                let minimum = count.minimum();
                let updated = (minimum > 0) && {
                    if let Some(fixed) = fixed
                        && fixed.len().get() < minimum
                    {
                        return Err(Contradiction);
                    }
                    vec.into_iter().try_fold(false, |updated, inner| {
                        Ok(updated
                            | self.try_add_hint(&Hint::Count(
                                inner.clone(),
                                BoundOrNot::AtLeast(minimum),
                            ))?)
                    })?
                };
                self.generic.push(Hint::Count(set, count));
                updated
            }
        };

        Ok(updated)
    }

    fn add_theorem(&mut self, thm: Theorem) -> Result<bool, Contradiction> {
        let updated = match thm {
            Theorem::CriminalCount(set, count) => self
                .criminal_count
                .entry(set)
                .or_insert_with(|| (CountSet::max(set.len().get()), TheoremState::Active))
                .0
                .intersect(count)?,
            Theorem::Connected(set) => match set {
                SetOp1::Judged(inner, judgment) => {
                    if let &SetExpr1::Regular(set) = inner.as_ref() {
                        self.connected.insert((set, judgment))
                    } else {
                        todo!()
                    }
                }
                SetOp1::Shift(_, _) => todo!(),
                SetOp1::Intersection(_, _) => todo!(),
            },
            Theorem::All(set, judgment) => {
                set.into_iter().try_fold(false, move |updated, coord| {
                    Ok(updated | self.try_add_hint(&Hint::Judgment(coord, judgment))?)
                })?
            }
            Theorem::Difference(sets, compare) => self.add_complex_diff(sets, compare)?,
            Theorem::CriminalDifference(sets, difference) => {
                self.add_difference(sets, difference)?
            }
            Theorem::Generic(hint) => self.try_add_hint(&hint)?,
        };
        Ok(updated)
    }

    fn add_complex_diff(
        &mut self,
        sets: [SetOp1; 2],
        compare: Comparison,
    ) -> Result<bool, Contradiction> {
        let updated = if let [SetOp1::Judged(a, judgment_a), SetOp1::Judged(b, judgment_b)] = sets
            && let &SetExpr1::Regular(a) = a.as_ref()
            && let &SetExpr1::Regular(b) = b.as_ref()
        {
            match [judgment_a, judgment_b] {
                [Judgment::Innocent, Judgment::Innocent] => {
                    // (|a| - crim(a)) - (|b| - crim(b)) = excess
                    // crim(a) - crim(b) = |a| - |b| - excess
                    let len_diff = 0_i8
                        .strict_add_unsigned(a.len().get())
                        .strict_sub_unsigned(b.len().get());
                    let difference = match compare {
                        Comparison::ExactDifference(excess) => {
                            Interval::exact(len_diff.strict_sub_unsigned(excess))
                        }
                        Comparison::More => Interval::range(
                            0_i8.strict_sub_unsigned(b.len().get()),
                            len_diff.strict_sub(1),
                        )
                        .ok_or(Contradiction)?,
                    };
                    self.add_difference([a, b], difference)?
                }
                [Judgment::Innocent, Judgment::Criminal] => {
                    // (|a| - crim(a)) - crim(b) = excess
                    // crim(a) + crim(b) = |a| -  excess
                    let sum = match compare {
                        Comparison::ExactDifference(excess) => {
                            Interval::exact(a.len().get().strict_sub(excess))
                        }
                        Comparison::More => Interval::zero_to(a.len().get().strict_sub(1)),
                    };
                    self.add_sum([a, b], sum)
                }
                [Judgment::Criminal, Judgment::Innocent] => {
                    // symm
                    let sum = match compare {
                        Comparison::ExactDifference(excess) => {
                            Interval::exact(b.len().get().strict_sub(excess))
                        }
                        Comparison::More => Interval::zero_to(b.len().get().strict_sub(1)),
                    };
                    self.add_sum([a, b], sum)
                }
                [Judgment::Criminal, Judgment::Criminal] => {
                    // crim(a) - crim(b) = count
                    let difference = match compare {
                        Comparison::ExactDifference(excess) => {
                            Interval::exact(excess.try_into().expect("at most 20"))
                        }
                        Comparison::More => {
                            Interval::range(1_i8, a.len().get().try_into().expect("at most 20"))
                                .ok_or(Contradiction)?
                        }
                    };
                    self.add_difference([a, b], difference)?
                }
            }
        } else {
            todo!()
        };
        Ok(updated)
    }

    fn add_criminal_count(
        &mut self,
        set: Set1,
        judgment: Judgment,
        count: BoundOrNot,
    ) -> Result<bool, Contradiction> {
        let criminal_count = match judgment {
            Judgment::Criminal => CountSet::from_cardinal(count, set.len()),
            Judgment::Innocent => CountSet::from_cardinal(count, set.len())
                .and_then(|count| CountSet::Exact(set.len().get()).checked_sub(count)),
        }
        .ok_or(Contradiction)?;
        self.add_theorem(Theorem::CriminalCount(set, criminal_count))
    }

    fn process_generic_hints(&mut self) -> Result<bool, Contradiction> {
        let hints = mem::take(&mut self.generic);
        hints.into_iter().try_fold(false, |updated, hint| {
            Ok(updated | self.try_add_hint(&hint)?)
        })
    }

    fn simplify_criminal_counts(&mut self) -> Result<bool, Contradiction> {
        let mut discard = Vec::new();
        let mut new = Vec::new();
        for (index, (&set, (count, _))) in self.criminal_count.iter().enumerate() {
            let [variable, fixed] = ByJudgment::split(set, &self.known).only(Judgment::Criminal);
            let count = count.checked_sub(fixed.len()).ok_or(Contradiction)?;
            if let Some(variable) = variable.non_empty() {
                if variable != set {
                    discard.push(index);
                    new.push(Theorem::CriminalCount(variable, count));
                }
            } else if count.matches(0) {
                discard.push(index);
            } else {
                return Err(Contradiction);
            }
        }
        let updated = !discard.is_empty();
        for index in discard.into_iter().rev() {
            let discard = self.criminal_count.swap_remove_index(index);
            assert_matches!(discard, Some(_), "invalid discard");
        }
        Ok(updated | self.add_theorems(new)?)
    }

    fn simplify_differences(&mut self) -> Result<bool, Contradiction> {
        let mut discard = BTreeSet::new();
        let mut new = Vec::new();
        for (index, (sets, difference)) in self.difference.iter().enumerate() {
            let [[a_var, a_fixed], [b_var, b_fixed]] =
                sets.map(|set| ByJudgment::split(set, &self.known).only(Judgment::Criminal));
            let difference = difference
                .strict_sub_unsigned(a_fixed.len())
                .strict_add_unsigned(b_fixed.len());
            match [a_var, b_var].map(Set::non_empty) {
                [None, None] => {
                    if !difference.matches(&0) {
                        return Err(Contradiction);
                    }
                }
                [None, Some(b_var)] => {
                    let count = difference
                        .strict_neg()
                        .non_negative()
                        .ok_or(Contradiction)?
                        .into();
                    new.push(Theorem::CriminalCount(b_var, count));
                }
                [Some(a_var), None] => {
                    let count = difference.non_negative().ok_or(Contradiction)?.into();
                    new.push(Theorem::CriminalCount(a_var, count));
                }
                [Some(a_var), Some(b_var)] => {
                    if &[a_var, b_var] == sets {
                        continue;
                    }
                    let sets = [a_var, b_var];
                    let [a_len, b_len] =
                        sets.map(|set| i8::try_from(set.len().get()).expect("at most 20"));
                    let difference = difference
                        .intersection(
                            Interval::range(b_len.strict_neg(), a_len).expect("neg ..= pos"),
                        )
                        .ok_or(Contradiction)?;
                    new.push(Theorem::CriminalDifference(sets, difference));
                }
            }
            _ = discard.insert(index);
        }
        let updated = !discard.is_empty();
        for index in discard.into_iter().rev() {
            assert_matches!(self.difference.swap_remove_index(index), Some(_));
        }
        Ok(updated | self.add_theorems(new)?)
    }

    fn simplify_connected(&mut self) -> Result<bool, Contradiction> {
        let mut discard = Vec::new();
        let mut new = Vec::new();
        for (index, &(set, judgment)) in self.connected.iter().enumerate() {
            let [variable, fixed] = ByJudgment::split(set, &self.known).only(judgment);
            if let Some(variable) = variable.non_empty() {
                if variable == set {
                    continue;
                }
                let count = self.count_judged(variable, judgment)?;
                let possibilities = variable
                    .power_set()
                    .filter(|sub| count.matches(sub.len()) && (*sub | fixed).connected())
                    .collect_vec();
                let Ok(possibilities) = Slice1::try_from_slice(possibilities.as_slice()) else {
                    return Err(Contradiction);
                };
                if let &[unique] = possibilities.as_slice() {
                    discard.push(index);
                    if let Some(unique) = (unique | fixed).non_empty() {
                        new.push(Theorem::All(unique, judgment));
                    }
                } else {
                    let all = possibilities.iter1().copied().reduce(|a, b| a & b);
                    let any = possibilities.iter1().copied().reduce(|a, b| a | b);

                    if let Some(all) = all.non_empty() {
                        new.push(Theorem::All(all, judgment));
                    }
                    let difference = variable.difference(any);
                    if let Some(difference) = difference.non_empty() {
                        new.push(Theorem::All(difference, !judgment));
                    }
                }
            } else {
                discard.push(index);
                if !fixed.connected() {
                    return Err(Contradiction);
                }
            }
        }
        let updated = !discard.is_empty();
        for index in discard.into_iter().rev() {
            assert_matches!(self.connected.swap_remove_index(index), Some(_));
        }
        Ok(updated | self.add_theorems(new)?)
    }

    fn extract_tight_counts(&mut self) -> Result<bool, Contradiction> {
        let mut discard = Vec::new();
        let new: Vec<Theorem> = self
            .criminal_count
            .iter()
            .enumerate()
            .map(|(index, (&set, &(count, _)))| {
                Ok(count.tight(set)?.map(|endpoint| {
                    discard.push(index);
                    let judgment = match endpoint {
                        EndPoint::Minimum => Judgment::Criminal,
                        EndPoint::Maximum => Judgment::Innocent,
                    };
                    Theorem::All(set, judgment)
                }))
            })
            .flatten_ok()
            .try_collect()?;
        for index in discard.into_iter().rev() {
            let discard = self.criminal_count.swap_remove_index(index);
            assert_matches!(discard, Some(_), "invalid discard");
        }

        self.add_theorems(new)
    }

    fn combine_criminal_counts(&mut self) -> Result<bool, Contradiction> {
        let mut discard = BTreeSet::new();
        let mut new = Vec::new();
        // TODO array_permutations()
        for [(a_index, (a, (a_count, _))), (b_index, (b, (b_count, _)))] in self
            .criminal_count
            .iter()
            .enumerate()
            .filter(|(_, (_, (_, state)))| state.is_active())
            .array_combinations()
        {
            if a <= b {
                if a_count.is_exact() {
                    _ = discard.insert(b_index);
                }
                let count = b_count.checked_sub(*a_count).ok_or(Contradiction)?;
                new.push(Theorem::CriminalCount(
                    b.difference(Set::from(*a)).non_empty().expect("a != b"),
                    count,
                ));
            } else if b <= a {
                if b_count.is_exact() {
                    _ = discard.insert(a_index);
                }
                let count = a_count.checked_sub(*b_count).ok_or(Contradiction)?;
                new.push(Theorem::CriminalCount(
                    a.difference(Set::from(*b)).non_empty().expect("a != b"),
                    count,
                ));
            }
            if let [Some(a_parity), Some(b_parity)] = [a_count, b_count].map(|count| count.parity())
            {
                let symm_diff = (*a ^ *b).non_empty().expect("a != b");
                let count = CountSet::from_parity(a_parity ^ b_parity, symm_diff.len());
                new.push(Theorem::CriminalCount(symm_diff, count));
            }
        }
        for index in discard.into_iter().rev() {
            self.criminal_count[index].1 = TheoremState::Discarded;
        }
        self.add_theorems(new)
    }

    fn inclusion_exclusion_bounds(&mut self) -> Result<bool, Contradiction> {
        let mut new = Vec::new();
        for [(&a, (a_count, _)), (&b, (b_count, _))] in self
            .criminal_count
            .iter()
            .filter(|(_, (_, state))| state.is_active())
            .array_combinations()
        {
            let union = a | b;
            let sum = a_count.strict_add(*b_count);
            if let Some(intersection) = (a & b).non_empty() {
                new.push(Theorem::CriminalCount(
                    union,
                    sum.checked_sub(self.gather_criminal_count(intersection)?)
                        .ok_or(Contradiction)?,
                ));
                new.push(Theorem::CriminalCount(
                    intersection,
                    sum.checked_sub(self.gather_criminal_count(union)?)
                        .ok_or(Contradiction)?,
                ));
            } else if !a_count.is_exact() && !b_count.is_exact() {
                new.push(Theorem::CriminalCount(union, sum));
            }
        }
        self.add_theorems(new)
    }

    fn bifurcate(&mut self) -> Result<bool, Contradiction> {
        let mut new = Vec::new();
        for (coord, _) in self.known.iter().filter(|(_, judgment)| judgment.is_none()) {
            for judgment in [Judgment::Innocent, Judgment::Criminal] {
                let mut clone = self.clone();
                clone.depth = clone.depth.strict_add(1);
                clone.known[coord] = Some(judgment);
                if matches!(clone.update_until_stuck(), Err(Contradiction)) {
                    new.push(Hint::Judgment(coord, !judgment).into());
                    break;
                }
            }
        }
        self.add_theorems(new)
    }

    fn update_until_stuck(&mut self) -> Result<(), Contradiction> {
        while self.update_theorems_or_contradict()? {}
        Ok(())
    }

    fn count_judged(&self, set: Set1, judgment: Judgment) -> Result<CountSet, Contradiction> {
        if let Some((count, _)) = self.criminal_count.get(&set) {
            match judgment {
                Judgment::Innocent => CountSet::Exact(set.len().get())
                    .checked_sub(*count)
                    .ok_or(Contradiction),
                Judgment::Criminal => Ok(*count),
            }
        } else {
            let min = match judgment {
                Judgment::Innocent => set
                    .len()
                    .get()
                    .checked_sub(self.max_criminals(set))
                    .ok_or(Contradiction)?,
                Judgment::Criminal => self.min_criminals(set),
            };
            let max = match judgment {
                Judgment::Innocent => set
                    .len()
                    .get()
                    .checked_sub(self.min_criminals(set))
                    .ok_or(Contradiction)?,
                Judgment::Criminal => self.max_criminals(set),
            };
            CountSet::complex(min, max, None).ok_or(Contradiction)
        }
    }

    fn max_criminals(&self, set: Set1) -> Number {
        self.criminal_count
            .iter()
            .map(|(other, (count, _))| {
                count
                    .maximum()
                    .min((set & *other).len())
                    .strict_add(set.difference(*other).len())
            })
            .min()
            .unwrap_or_else(|| set.len().get())
    }

    fn min_criminals(&self, set: Set1) -> Number {
        self.criminal_count
            .iter()
            .map(|(other, (count, _))| {
                count
                    .minimum()
                    .saturating_sub(other.difference(Set::from(set)).len())
            })
            .max()
            .unwrap_or(0)
    }

    fn add_difference(
        &mut self,
        [a, b]: [Set1; 2],
        difference: Interval<i8>,
    ) -> Result<bool, Contradiction> {
        let sets = [a, b].map(|set| set.difference(Set::from(a) & Set::from(b)).non_empty());
        let updated = match sets {
            [None, None] => {
                if !difference.matches(&0) {
                    return Err(Contradiction);
                }
                false
            }
            [None, Some(b)] => {
                let count = difference
                    .strict_neg()
                    .non_negative()
                    .ok_or(Contradiction)?
                    .into();
                self.add_theorem(Theorem::CriminalCount(b, count))?
            }
            [Some(a), None] => {
                let count = difference.non_negative().ok_or(Contradiction)?.into();
                self.add_theorem(Theorem::CriminalCount(a, count))?
            }
            [Some(a), Some(b)] => {
                let (sets, difference) = match a.total_cmp(b) {
                    Ordering::Less => ([b, a], difference.strict_neg()),
                    Ordering::Equal => {
                        unreachable!("ensured disjoint");
                    }
                    Ordering::Greater => ([a, b], difference),
                };
                self.difference
                    .entry(sets)
                    .or_insert(Interval::neg_to_pos(
                        sets[1].len().get(),
                        sets[0].len().get(),
                    ))
                    .intersect(difference)?
            }
        };
        Ok(updated)
    }

    fn add_sum(&self, _sets: [Set1; 2], _sum: Interval<Number>) -> bool {
        todo!()
    }

    fn counts_from_differences(&mut self) -> Result<bool, Contradiction> {
        let mut new = Vec::with_capacity(self.difference.len().strict_mul(2));
        for (&[a, b], difference) in &self.difference {
            let b_min = self
                .min_criminals(a)
                .saturating_sub_signed(difference.max());
            let b_max = self
                .max_criminals(a)
                .checked_sub_signed(difference.min())
                .ok_or(Contradiction)?;
            let count = CountSet::complex(b_min, b_max, None).ok_or(Contradiction)?;
            new.push(Theorem::CriminalCount(b, count));

            let a_min = self
                .min_criminals(b)
                .saturating_add_signed(difference.min());
            let a_max = self
                .max_criminals(b)
                .checked_add_signed(difference.max())
                .ok_or(Contradiction)?;
            let count = CountSet::complex(a_min, a_max, None).ok_or(Contradiction)?;
            new.push(Theorem::CriminalCount(a, count));
        }
        self.add_theorems(new)
    }

    fn simplify_count_hints(&mut self) -> Result<bool, Contradiction> {
        let mut discard = BTreeSet::new();
        let new: Vec<Hint> = self
            .generic
            .iter()
            .enumerate()
            .map(|(index, hint)| {
                let Hint::Count(set, count) = hint else {
                    return Ok(None);
                };
                let Some(split) = SetSimplify::split(set, &self.known) else {
                    return Ok(None);
                };
                assert!(discard.insert(index), "index monotonic");
                match split {
                    SetSimplify::Fixed(fixed) if count.matches(fixed.len().get()) => Ok(None),
                    SetSimplify::Empty if count.matches(0) => Ok(None),
                    SetSimplify::Fixed(_) | SetSimplify::Empty => Err(Contradiction),
                    SetSimplify::Variable(variable) => Ok(Some(Hint::Count(variable, *count))),
                    SetSimplify::Split(variable, fixed) => {
                        let count = count::reduce_cardinal(*count, fixed.len().get())
                            .ok_or(Contradiction)?;
                        Ok(Some(Hint::Count(variable, count)))
                    }
                }
            })
            .flatten_ok()
            .try_collect()?;
        for index in discard.into_iter().rev() {
            assert_matches!(self.generic.swap_remove(index), Hint::Count(..));
        }
        self.add_theorems(new.into_iter().map(Theorem::Generic))
    }

    fn add_theorems(
        &mut self,
        new: impl IntoIterator<Item = Theorem>,
    ) -> Result<bool, Contradiction> {
        new.into_iter().try_fold(false, |updated, theorem| {
            Ok(updated | self.add_theorem(theorem)?)
        })
    }

    fn try_add_hint(&mut self, hint: &Hint) -> Result<bool, Contradiction> {
        let updated = match hint {
            &Hint::Judgment(coord, judgment) => match self.known[coord] {
                Some(known) if known != judgment => return Err(Contradiction),
                Some(_) => false,
                None => {
                    self.known[coord] = Some(judgment);
                    self.new_knowledge.push((coord, judgment));
                    true
                }
            },
            Hint::Count(set, count) => self.add_count(set.clone(), *count)?,
            Hint::CountTotal(_, _) => todo!(),
            Hint::Connected(set) => match set {
                SetOp1::Judged(inner, judgment) => {
                    if let &SetExpr1::Regular(set) = inner.as_ref() {
                        self.connected.insert((set, *judgment))
                    } else {
                        todo!()
                    }
                }
                SetOp1::Shift(_, _) => todo!(),
                SetOp1::Intersection(_, _) => todo!(),
            },
            Hint::CompareSets(sets, compare) => self.add_complex_diff(sets.clone(), *compare)?,
            Hint::UniqueWithCount { .. } => todo!(),
            Hint::EachNeighbors(set, count, judgment) => {
                if let &SetExpr1::Regular(set) = set {
                    set.into_iter().try_fold(false, |updated, coord| {
                        Ok(updated
                            | self
                                .add_count(coord.neighbors().judged(*judgment), (*count).into())?)
                    })?
                } else {
                    todo!();
                }
            }
            Hint::CountWithNeighbors { .. } => todo!(),
            Hint::IfThen(_) => todo!(),
        };
        Ok(updated)
    }

    fn gather_criminal_count(&self, set: Set1) -> Result<CountSet, Contradiction> {
        if let Some(&(count, _)) = self.criminal_count.get(&set) {
            Ok(count)
        } else {
            CountSet::complex(self.min_criminals(set), self.max_criminals(set), None)
                .ok_or(Contradiction)
        }
    }
}

impl Engine for InferenceSolver {
    fn add_parsed_hint(&mut self, hint: &Hint) {
        self.generic.push(hint.clone());
    }

    fn updates(&mut self) -> Result<Vec<(Coord, Judgment)>> {
        loop {
            if self.known.iter().all(|(_, judgment)| judgment.is_some())
                || !self.update_theorems_or_contradict()?
            {
                return Ok(mem::take(&mut self.new_knowledge));
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Theorem {
    All(Set1, Judgment),
    CriminalCount(Set1, CountSet),
    Connected(SetOp1),
    Difference([SetOp1; 2], Comparison),
    CriminalDifference([Set1; 2], Interval<i8>),
    Generic(Hint),
}

impl From<Hint> for Theorem {
    fn from(v: Hint) -> Self {
        Self::Generic(v)
    }
}

enum Retain {
    Keep,
    Discard,
}

impl Retain {
    /// Returns `true` if the retain is [`Keep`].
    ///
    /// [`Keep`]: Retain::Keep
    #[must_use]
    fn is_keep(&self) -> bool {
        matches!(self, Self::Keep)
    }

    /// Returns `true` if the retain is [`Discard`].
    ///
    /// [`Discard`]: Retain::Discard
    #[must_use]
    fn is_discard(&self) -> bool {
        matches!(self, Self::Discard)
    }
}

#[derive(Debug, Clone, Copy)]
enum TheoremState {
    Discarded,
    Active,
}

impl TheoremState {
    /// Returns `true` if the theorem state is [`Discarded`].
    ///
    /// [`Discarded`]: TheoremState::Discarded
    #[must_use]
    fn is_discarded(self) -> bool {
        matches!(self, Self::Discarded)
    }

    /// Returns `true` if the theorem state is [`Active`].
    ///
    /// [`Active`]: TheoremState::Active
    #[must_use]
    fn is_active(self) -> bool {
        matches!(self, Self::Active)
    }
}

#[derive(Debug, Clone, Copy)]
struct Contradiction;

impl fmt::Display for Contradiction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "reached contradiction!")
    }
}

impl error::Error for Contradiction {}

#[cfg(test)]
mod tests {
    use test_each_file::test_each_file;

    use crate::solver::tests::solve;

    use super::*;

    test_each_file! { in "./archive" as puzzle => inference}

    fn inference(puzzle: &str) {
        let puzzle = ron::from_str(puzzle).unwrap();
        solve::<InferenceSolver>(&puzzle);
    }
}
