#![allow(dead_code, clippy::todo, reason = "in progress")]

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::{assert_matches, mem};

use anyhow::{Result, bail};
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools as _;
use mitsein::slice1::Slice1;

use crate::grid::Grid;
use crate::models::{Coord, Judgment};
use crate::solver::Engine;
use crate::solver::board::coordinates::{Set, Set1, SetExpr1, SetOp, SetOp1};
use crate::solver::hint::{BoundOrNot, Comparison, Hint, Number};
use crate::solver::inference::count::{CountSet, Interval};
use crate::solver::inference::split::{ByJudgment, SetSimplify};

mod count;
mod split;

#[derive(Debug, Default)]
struct InferenceSolver {
    known: Grid<Option<Judgment>>,
    new_knowledge: Vec<(Coord, Judgment)>,
    criminal_count: IndexMap<Set1, CountSet>,
    connected: IndexSet<(Set1, Judgment)>,
    difference: IndexMap<[Set1; 2], Interval<i8>>,
    sum: IndexMap<[Set1; 2], Interval<Number>>,
    generic: Vec<Hint>,
    contradictory: bool,
    updated: bool,
}

impl InferenceSolver {
    fn add_count(&mut self, set: SetOp1, count: BoundOrNot) -> Result<(), Contradiction> {
        match &set {
            SetOp1::Judged(inner, judgment) => match inner.as_ref() {
                &SetExpr1::Regular(set) => return self.add_criminal_count(set, *judgment, count),
                SetExpr1::Op(inner) => {
                    let minimum = count.minimum();
                    if minimum > 0 {
                        self.try_add_hint(&Hint::Count(
                            inner.clone(),
                            BoundOrNot::AtLeast(minimum),
                        ))?;
                    }
                    self.generic.push(Hint::Count(set, count));
                }
            },
            SetOp1::Shift(inner, direction) => {
                let minimum = count.minimum();
                if minimum > 0 {
                    let Some(pre_shift) = Set1::all().shift(direction.flip()).non_empty() else {
                        return Err(Contradiction);
                    };
                    let SetOp::NonEmpty(pre_shift) = inner.clone().intersect_set1(pre_shift) else {
                        return Err(Contradiction);
                    };
                    self.try_add_hint(&Hint::Count(pre_shift, BoundOrNot::AtLeast(minimum)))?;
                }
                self.generic.push(Hint::Count(set, count));
            }
            SetOp1::Intersection(vec, fixed) => {
                let minimum = count.minimum();
                if minimum > 0 {
                    if let Some(fixed) = fixed
                        && fixed.len().get() < minimum
                    {
                        return Err(Contradiction);
                    }
                    for inner in vec {
                        self.try_add_hint(&Hint::Count(
                            inner.clone(),
                            BoundOrNot::AtLeast(minimum),
                        ))?;
                    }
                }
                self.generic.push(Hint::Count(set, count));
            }
        }

        Ok(())
    }

    fn add_theorem(&mut self, thm: Theorem) -> Result<(), Contradiction> {
        match thm {
            Theorem::CriminalCount(set, count) => {
                self.criminal_count
                    .entry(set)
                    .or_insert_with(|| CountSet::max(set.len().get()))
                    .intersect(count)?;
            }
            Theorem::Connected(set) => match set {
                SetOp1::Judged(inner, judgment) => {
                    if let &SetExpr1::Regular(set) = inner.as_ref() {
                        _ = self.connected.insert((set, judgment));
                    } else {
                        todo!()
                    }
                }
                SetOp1::Shift(_, _) => todo!(),
                SetOp1::Intersection(_, _) => todo!(),
            },
            Theorem::All(set, judgment) => set
                .into_iter()
                .try_for_each(move |coord| self.try_add_hint(&Hint::Judgment(coord, judgment)))?,
            Theorem::Difference(sets, compare) => {
                self.add_complex_diff(sets, compare)?;
            }
            Theorem::CriminalDifference(sets, difference) => {
                self.add_difference(sets, difference)?;
            }
            Theorem::Generic(hint) => self.try_add_hint(&hint)?,
        }
        Ok(())
    }

    fn add_complex_diff(
        &mut self,
        sets: [SetOp1; 2],
        compare: Comparison,
    ) -> Result<(), Contradiction> {
        if let [SetOp1::Judged(a, judgment_a), SetOp1::Judged(b, judgment_b)] = sets
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
                    self.add_difference([a, b], difference)?;
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
                    self.add_sum([a, b], sum);
                }
                [Judgment::Criminal, Judgment::Innocent] => {
                    // symm
                    let sum = match compare {
                        Comparison::ExactDifference(excess) => {
                            Interval::exact(b.len().get().strict_sub(excess))
                        }
                        Comparison::More => Interval::zero_to(b.len().get().strict_sub(1)),
                    };
                    self.add_sum([a, b], sum);
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
                    self.add_difference([a, b], difference)?;
                }
            }
        } else {
            todo!()
        }
        Ok(())
    }

    fn add_criminal_count(
        &mut self,
        set: Set1,
        judgment: Judgment,
        count: BoundOrNot,
    ) -> Result<(), Contradiction> {
        let criminal_count = match judgment {
            Judgment::Criminal => CountSet::from_cardinal(count, set.len().get()),
            Judgment::Innocent => CountSet::from_cardinal(count, set.len().get())
                .and_then(|count| CountSet::Exact(set.len().get()).checked_sub(count)),
        }
        .ok_or(Contradiction)?;
        self.add_theorem(Theorem::CriminalCount(set, criminal_count))
    }

    fn add_theorems(
        &mut self,
        new: impl IntoIterator<Item = Theorem>,
    ) -> Result<(), Contradiction> {
        new.into_iter()
            .try_for_each(|theorem| self.add_theorem(theorem))
    }

    fn try_add_hint(&mut self, hint: &Hint) -> Result<(), Contradiction> {
        match hint {
            &Hint::Judgment(coord, judgment) => match self.known[coord] {
                Some(known) if known != judgment => return Err(Contradiction),
                Some(_) => {}
                None => {
                    self.known[coord] = Some(judgment);
                    self.new_knowledge.push((coord, judgment));
                }
            },
            Hint::Count(set, count) => self.add_count(set.clone(), *count)?,
            Hint::CountTotal(_, _) => todo!(),
            Hint::Connected(set) => {
                self.add_theorem(Theorem::Connected(set.clone()))?;
            }
            Hint::CompareSets(sets, compare) => {
                self.add_theorem(Theorem::Difference(sets.clone(), *compare))?;
            }
            Hint::UniqueWithCount { .. } => todo!(),
            Hint::EachNeighbors(set, count, judgment) => {
                if let &SetExpr1::Regular(set) = set {
                    set.into_iter().try_for_each(|coord| {
                        self.add_count(coord.neighbors().judged(*judgment), (*count).into())
                    })?;
                } else {
                    todo!();
                }
            }
            Hint::CountWithNeighbors { .. } => todo!(),
            Hint::IfThen(_) => todo!(),
        }
        Ok(())
    }

    fn update_theorems_or_contradict(&mut self) -> Result<(), Contradiction> {
        self.simplify_complex_counts()?;
        self.simplify_connected()?;
        self.simplify_criminal_counts()?;
        self.simplify_differences()?;
        self.combine_criminal_counts()?;
        self.extract_tight_bounds()?;
        Ok(())
    }

    fn simplify_complex_counts(&mut self) -> Result<(), Contradiction> {
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
            self.updated = true;
            drop(self.generic.swap_remove(index));
        }
        self.add_theorems(new.into_iter().map(Theorem::Generic))
    }

    fn simplify_criminal_counts(&mut self) -> Result<(), Contradiction> {
        let new: Vec<Theorem> = mem::take(&mut self.criminal_count)
            .into_iter()
            .map(|(set, count)| {
                let [variable, fixed] =
                    ByJudgment::split(set, &self.known).only(Judgment::Criminal);
                let count = count.checked_sub(fixed.len()).ok_or(Contradiction)?;
                if let Some(variable) = variable.non_empty() {
                    self.updated |= variable != set;
                    Ok(Some(Theorem::CriminalCount(variable, count)))
                } else if count.matches(0) {
                    self.updated = true;
                    Ok(None)
                } else {
                    Err(Contradiction)
                }
            })
            .flatten_ok()
            .try_collect()?;
        self.add_theorems(new)
    }

    fn simplify_differences(&mut self) -> Result<(), Contradiction> {
        let mut discard = BTreeSet::new();
        let new: Vec<Theorem> = self
            .difference
            .iter()
            .enumerate()
            .map(|(index, (sets, difference))| {
                let [[a_var, a_fixed], [b_var, b_fixed]] =
                    sets.map(|set| ByJudgment::split(set, &self.known).only(Judgment::Criminal));
                let difference = difference
                    .strict_sub_unsigned(a_fixed.len())
                    .strict_add_unsigned(b_fixed.len());

                let new = match [a_var, b_var].map(Set::non_empty) {
                    [None, None] => {
                        if !difference.matches(&0) {
                            return Err(Contradiction);
                        }
                        None
                    }
                    [None, Some(b_var)] => {
                        let count = difference
                            .strict_neg()
                            .non_negative()
                            .ok_or(Contradiction)?
                            .into();
                        Some(Theorem::CriminalCount(b_var, count))
                    }
                    [Some(a_var), None] => {
                        let count = difference.non_negative().ok_or(Contradiction)?.into();
                        Some(Theorem::CriminalCount(a_var, count))
                    }
                    [Some(a_var), Some(b_var)] => {
                        if &[a_var, b_var] == sets {
                            return Ok(None);
                        }
                        Some(Theorem::CriminalDifference([a_var, b_var], difference))
                    }
                };
                _ = discard.insert(index);
                Ok(new)
            })
            .flatten_ok()
            .try_collect()?;
        if !new.is_empty() || !discard.is_empty() {
            self.updated = true;
        }
        for index in discard.into_iter().rev() {
            assert_matches!(self.difference.swap_remove_index(index), Some(_));
        }
        self.add_theorems(new)
    }

    fn simplify_connected(&mut self) -> Result<(), Contradiction> {
        let new: Vec<Theorem> = mem::take(&mut self.connected)
            .into_iter()
            .map(|(set, judgment)| {
                let [variable, fixed] = ByJudgment::split(set, &self.known).only(judgment);
                if let Some(variable) = variable.non_empty() {
                    let count = self.count_judged(variable, judgment)?;
                    let possibilities = variable
                        .power_set()
                        .filter(|sub| count.matches(sub.len()) && (*sub | fixed).connected())
                        .collect_vec();
                    let Ok(possibilities) = Slice1::try_from_slice(possibilities.as_slice()) else {
                        return Err(Contradiction);
                    };
                    if let &[unique] = possibilities.as_slice() {
                        self.updated = true;
                        Ok((unique | fixed)
                            .non_empty()
                            .map(|unique| Theorem::All(unique, judgment))
                            .into_iter()
                            .collect())
                    } else {
                        let all = possibilities.iter1().copied().reduce(|a, b| a & b);
                        let any = possibilities.iter1().copied().reduce(|a, b| a | b);

                        let mut theorems = vec![Theorem::Connected(set.judged(judgment))];
                        if let Some(all) = all.non_empty() {
                            self.updated = true;
                            theorems.push(Theorem::All(all, judgment));
                        }
                        let difference = variable.difference(any);
                        if let Some(difference) = difference.non_empty() {
                            self.updated = true;
                            theorems.push(Theorem::All(difference, !judgment));
                        }
                        Ok(theorems)
                    }
                } else {
                    self.updated = true;
                    if fixed.connected() {
                        Ok(vec![])
                    } else {
                        Err(Contradiction)
                    }
                }
            })
            .flatten_ok()
            .try_collect()?;
        self.add_theorems(new)
    }

    fn extract_tight_bounds(&mut self) -> Result<(), Contradiction> {
        let mut discard = BTreeSet::new();
        let new: Vec<Theorem> = self
            .criminal_count
            .iter()
            .enumerate()
            .map(|(index, (&set, &count))| {
                Ok(Theorem::tight(set, count)?.map(|judgment| {
                    _ = discard.insert(index);
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

    fn combine_criminal_counts(&mut self) -> Result<(), Contradiction> {
        let mut discard = BTreeSet::new();
        let mut new = Vec::new();
        // TODO array_permutations()
        for [(a_index, (a, a_count)), (b_index, (b, b_count))] in
            self.criminal_count.iter().enumerate().array_combinations()
        {
            if a <= b {
                if a_count.is_exact() || b_count.is_exact() {
                    _ = discard.insert(b_index);
                }
                let count = b_count.checked_sub(*a_count).ok_or(Contradiction)?;
                new.push(Theorem::CriminalCount(
                    b.difference(Set::from(*a)).non_empty().expect("a != b"),
                    count,
                ));
            } else if b <= a {
                if a_count.is_exact() || b_count.is_exact() {
                    _ = discard.insert(a_index);
                }
                let count = a_count.checked_sub(*b_count).ok_or(Contradiction)?;
                new.push(Theorem::CriminalCount(
                    a.difference(Set::from(*b)).non_empty().expect("a != b"),
                    count,
                ));
            }
        }
        for index in discard.into_iter().rev() {
            assert_matches!(self.criminal_count.swap_remove_index(index), Some(_));
        }
        self.add_theorems(new)
    }

    fn count_judged(&self, set: Set1, judgment: Judgment) -> Result<CountSet, Contradiction> {
        if let Some(count) = self.criminal_count.get(&set) {
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
            .map(|(other, count)| {
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
            .map(|(other, count)| {
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
    ) -> Result<(), Contradiction> {
        let (sets, difference) = match a.total_cmp(b) {
            Ordering::Less => ([b, a], difference.strict_neg()),
            Ordering::Equal => {
                return if difference.matches(&0) {
                    Ok(())
                } else {
                    Err(Contradiction)
                };
            }
            Ordering::Greater => ([a, b], difference),
        };
        let entry = self
            .difference
            .entry(sets)
            .or_insert(Interval::neg_to_pos(b.len().get(), a.len().get()));
        *entry = entry.intersection(difference).ok_or(Contradiction)?;

        Ok(())
    }

    fn add_sum(&self, _sets: [Set1; 2], _sum: Interval<Number>) {
        todo!()
    }
}

impl Engine for InferenceSolver {
    fn add_parsed_hint(&mut self, hint: &Hint) {
        if matches!(self.try_add_hint(hint), Err(Contradiction)) {
            self.contradictory = true;
        }
    }

    fn updates(&mut self) -> Result<Vec<(Coord, Judgment)>> {
        loop {
            if self.known.iter().all(|(_, judgment)| judgment.is_some()) {
                return Ok(mem::take(&mut self.new_knowledge));
            }
            self.updated = false;
            if self.contradictory
                || matches!(self.update_theorems_or_contradict(), Err(Contradiction))
            {
                bail!("reached contradiction");
            }
            if !self.new_knowledge.is_empty() {
                return Ok(mem::take(&mut self.new_knowledge));
            }
            if !self.updated {
                bail!("infinite loop");
            }
        }
    }
}

// TODO replace the ones which are just a hint with `Hint` variant
#[derive(Debug, Clone)]
enum Theorem {
    All(Set1, Judgment),
    CriminalCount(Set1, CountSet),
    Connected(SetOp1),
    Difference([SetOp1; 2], Comparison),
    CriminalDifference([Set1; 2], Interval<i8>),
    Generic(Hint),
}

impl Theorem {
    fn tight(set: Set1, count: CountSet) -> Result<Option<Judgment>, Contradiction> {
        match set.len().get().cmp(&count.minimum()) {
            Ordering::Less => return Err(Contradiction),
            Ordering::Equal => return Ok(Some(Judgment::Criminal)),
            Ordering::Greater => {}
        }
        if count.maximum() == 0 {
            return Ok(Some(Judgment::Innocent));
        }
        Ok(None)
    }
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
struct Contradiction;

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
