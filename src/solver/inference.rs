#![allow(dead_code, clippy::todo, reason = "in progress")]

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::ops::BitAnd;
use std::{assert_matches, error, fmt, mem};

use anyhow::Result;
use indexmap::{IndexMap, IndexSet};
use itertools::Itertools as _;
use mitsein::slice1::Slice1;
use mitsein::vec1::Vec1;

use crate::grid::Grid;
#[cfg(test)]
use crate::models::Solution;
use crate::models::{Coord, Judgment};
use crate::solver::Engine;
use crate::solver::board::coordinates::{Set, Set1, SetExpr1, SetOp, SetOp1};
use crate::solver::hint::{Bound, BoundOrNot, Comparison, Hint, Number, Parity};
use crate::solver::inference::count::{CountSet, EndPoint, Interval};
use crate::solver::inference::split::{ByJudgment, SetSimplify};

mod count;
mod split;

const MAX_BIFURCATION_DEPTH: u8 = 2;

#[derive(Debug, Default, Clone)]
struct InferenceSolver {
    known: Grid<Option<Judgment>>,
    new_knowledge: Vec<(Coord, Judgment)>,
    criminal_count: IndexMap<Set1, (CountSet, TheoremState)>,
    connected: IndexSet<(Set1, Judgment)>,
    difference: IndexMap<[Set1; 2], Interval<i8>>,
    sum: IndexMap<[Set1; 2], Interval<Number>>,
    generic: Vec<Hint>,
    allowed_depth: Option<u8>,
    #[cfg(test)]
    validation_solution: Option<Solution>,
}

impl InferenceSolver {
    fn update_phase(
        &mut self,
        tactic: Tactic,
        update: impl FnOnce(&mut Self) -> Result<bool, Contradiction>,
    ) -> Result<bool, Contradiction> {
        if self.allowed_depth.is_none() {
            eprintln!("{}", tactic.name());
        }
        let updated = update(self)?;
        #[cfg(test)]
        if self.validation_solution.is_some() {
            self.validate_theorems(tactic);
        }
        Ok(updated)
    }

    fn update_theorems_or_contradict(&mut self) -> Result<bool, Contradiction> {
        let updated = self
            .update_phase(Tactic::ProcessGenericHints, Self::process_generic_hints)?
            || self.update_phase(Tactic::ExtractTightCounts, Self::extract_tight_counts)?
            || self.update_phase(
                Tactic::SimplifyCriminalCounts,
                Self::simplify_criminal_counts,
            )?
            || self.update_phase(Tactic::SimplifyDifferences, Self::simplify_differences)?
            || self.update_phase(Tactic::SimplifyConnected, Self::simplify_connected)?
            || self.update_phase(Tactic::CountsFromDifferences, Self::counts_from_differences)?
            || self.update_phase(Tactic::CombineCriminalCounts, Self::combine_criminal_counts)?
            || self.update_phase(Tactic::InclusionExclusion, Self::inclusion_exclusion_bounds)?
            || self.update_phase(Tactic::Bifurcate, Self::bifurcate)?;
        Ok(updated)
    }

    fn process_generic_hints(&mut self) -> Result<bool, Contradiction> {
        let hints = mem::take(&mut self.generic);
        hints.into_iter().try_fold(
            false,
            |updated, hint| Ok(updated | self.try_add_hint(hint)?),
        )
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
                let count = self.count_judged(variable, judgment)?;
                let mut matches_count = 0_usize;
                let possibilities = variable
                    .power_set()
                    .filter(|sub| count.matches(sub.len()))
                    .inspect(|_| matches_count = matches_count.strict_add(1))
                    .filter(|sub| (*sub | fixed).connected())
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
                    if possibilities.len().get() == matches_count {
                        discard.push(index);
                    }
                    let all = possibilities.iter1().copied().reduce(|a, b| a & b);
                    let any = possibilities.iter1().copied().reduce(|a, b| a | b);

                    if let Some(all) = all.non_empty() {
                        new.push(Theorem::All(all, judgment));
                    }
                    let difference = variable.difference(any);
                    if let Some(difference) = difference.non_empty() {
                        new.push(Theorem::All(difference, !judgment));
                    }
                    for a_judgment in [Judgment::Innocent, Judgment::Criminal] {
                        for b_judgment in [Judgment::Innocent, Judgment::Criminal] {
                            let a_matches = a_judgment == judgment;
                            let b_matches = b_judgment == judgment;

                            for [a, b] in any.difference(all).into_iter().array_combinations() {
                                if possibilities.iter().all(|&possibility| {
                                    possibility.contains(a) != a_matches
                                        || possibility.contains(b) == b_matches
                                }) {
                                    new.push(
                                        Hint::IfThen([(a, a_judgment), (b, b_judgment)]).into(),
                                    );
                                }
                            }
                        }
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

    /// This is quite a conservative version, only propagating the bounds if there are already known
    /// bounds for both the union and the intersection
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
                if let Some(&(union_bound, _)) = self.criminal_count.get(&union)
                    && let Some(&(intersection_bound, _)) = self.criminal_count.get(&intersection)
                {
                    new.push(Theorem::CriminalCount(
                        union,
                        sum.checked_sub(intersection_bound).ok_or(Contradiction)?,
                    ));
                    new.push(Theorem::CriminalCount(
                        intersection,
                        sum.checked_sub(union_bound).ok_or(Contradiction)?,
                    ));
                }
            } else if !a_count.is_exact()
                && !b_count.is_exact()
                && self.criminal_count.contains_key(&union)
            {
                new.push(Theorem::CriminalCount(union, sum));
            }
        }
        self.add_theorems(new)
    }

    fn bifurcate(&mut self) -> Result<bool, Contradiction> {
        if let Some(depth) = self.allowed_depth {
            depth
                .checked_sub(1)
                .map_or(Ok(false), |depth| self.bifurcate_to(depth))
        } else {
            for depth in 0..MAX_BIFURCATION_DEPTH {
                if self.bifurcate_to(depth)? {
                    return Ok(true);
                }
            }
            Ok(false)
        }
    }

    fn bifurcate_to(&mut self, depth: u8) -> Result<bool, Contradiction> {
        let mut new = Vec::new();
        for (coord, _) in self.known.iter().filter(|(_, judgment)| judgment.is_none()) {
            for judgment in [Judgment::Innocent, Judgment::Criminal] {
                let mut clone = self.clone();
                clone.allowed_depth = Some(depth);
                clone.known[coord] = Some(judgment);
                #[cfg(test)]
                if clone
                    .validation_solution
                    .as_ref()
                    .is_some_and(|solution| solution[coord] != judgment)
                {
                    clone.validation_solution = None;
                }
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

    fn update_until_stuck_or_new_knowledge(&mut self) -> Result<(), Contradiction> {
        while self.new_knowledge.is_empty() && self.update_theorems_or_contradict()? {}
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

    fn add_sum(
        &mut self,
        mut sets: [Set1; 2],
        sum: Interval<Number>,
    ) -> Result<bool, Contradiction> {
        sets.sort_by(|a, &b| a.total_cmp(b));
        if sets[0] == sets[1] {
            let count = sum.checked_div_2().ok_or(Contradiction)?;
            self.add_theorem(Theorem::CriminalCount(sets[0], count.into()))
        } else if (sets[0] & sets[1]).is_empty() {
            let set = sets[0] | sets[1];
            self.add_theorem(Theorem::CriminalCount(set, sum.into()))
        } else {
            let total_len = sets[0].len().get().strict_add(sets[1].len().get());
            self.sum
                .entry(sets)
                .or_insert(Interval::zero_to(total_len))
                .intersect(sum)
        }
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

    fn simplify_and_add_count(
        &mut self,
        set: &SetOp1,
        count: BoundOrNot,
    ) -> Result<bool, Contradiction> {
        if matches!(count, BoundOrNot::AtLeast(0)) {
            return Ok(false);
        }
        let (set, count) = match SetSimplify::split(set, &self.known) {
            Some(SetSimplify::Fixed(fixed)) if count.matches(fixed.len().get()) => {
                return Ok(false);
            }
            Some(SetSimplify::Empty) if count.matches(0) => return Ok(false),
            Some(SetSimplify::Fixed(_) | SetSimplify::Empty) => {
                return Err(Contradiction);
            }
            Some(SetSimplify::Variable(variable)) => (variable, count),
            Some(SetSimplify::Split(variable, fixed)) => {
                let count =
                    count::reduce_bound_or_not(count, fixed.len().get()).ok_or(Contradiction)?;
                (variable, count)
            }
            None => (set.clone(), count),
        };
        self.add_count_and_implied_inner_counts(set, count)
    }

    fn add_count_and_implied_inner_counts(
        &mut self,
        set: SetOp1,
        count: BoundOrNot,
    ) -> Result<bool, Contradiction> {
        let updated = match &set {
            SetOp1::Judged(inner, judgment) => match inner.as_ref() {
                &SetExpr1::Regular(set) => {
                    return self.add_criminal_count(set, *judgment, count);
                }
                SetExpr1::Op(inner) => {
                    let minimum = count.minimum();
                    let updated = (minimum > 0)
                        && self.simplify_and_add_count(inner, BoundOrNot::AtLeast(minimum))?;
                    self.generic.push(Hint::Count(set, count));
                    updated
                }
            },
            SetOp1::Shift(inner, direction) => {
                let minimum = count.minimum();
                let updated = (minimum > 0) && {
                    match SetExpr1::Regular(Set1::all())
                        .shift(direction.flip())
                        .intersect1(inner.as_ref().clone())
                        .regular()
                    {
                        Err(pre_shift) => {
                            self.simplify_and_add_count(&pre_shift, BoundOrNot::AtLeast(minimum))?
                        }
                        Ok(pre_shift) if pre_shift.len() >= minimum => false,
                        Ok(_) => return Err(Contradiction),
                    }
                };
                self.generic.push(Hint::Count(set, count));
                updated
            }
            SetOp1::Intersection(vec, fixed) => {
                let minimum = count.minimum();
                if let Some(fixed) = fixed
                    && fixed.len().get() < minimum
                {
                    return Err(Contradiction);
                }
                if let Some(fixed) = fixed
                    && fixed.len().get() < minimum
                {
                    return Err(Contradiction);
                }
                let updated = (minimum > 0)
                    && vec.iter().try_fold(false, |updated, set| {
                        Ok(updated
                            | self.simplify_and_add_count(set, BoundOrNot::AtLeast(minimum))?)
                    })?;
                self.generic.push(Hint::Count(set, count));
                updated
            }
        };

        Ok(updated)
    }

    fn add_if_then(&mut self, clauses: [(Coord, Judgment); 2]) -> Result<bool, Contradiction> {
        let (coord, judgment) = match clauses
            .map(|(coord, judgment)| self.known[coord].map(|known| known == judgment))
        {
            [None, None] => {
                self.generic.push(Hint::IfThen(clauses));
                return Ok(false);
            }
            [_, Some(true)] | [Some(false), _] => return Ok(false),
            [None, Some(false)] => {
                let [(coord, judgment), _] = clauses;
                (coord, !judgment)
            }
            [Some(true), None] => clauses[1],
            [Some(true), Some(false)] => return Err(Contradiction),
        };
        self.try_add_hint(Hint::Judgment(coord, judgment))
    }

    fn add_count_with_neighbors(
        &mut self,
        each: Bound,
        count: Bound,
        judgment: Judgment,
        set: Set1,
    ) -> Result<bool, Contradiction> {
        let mut yes = Set::empty();
        let mut maybe = Set::empty();
        for coord in set {
            match TriState::has_size_1_op(
                &coord.neighbors().judged(judgment),
                each.into(),
                &self.known,
            ) {
                TriState::No => {}
                TriState::Maybe => maybe |= coord,
                TriState::Yes => yes |= coord,
            }
        }

        let count = count::reduce_bound(count, yes.len()).ok_or(Contradiction)?;
        if matches!(count, Bound::AtMost(0) | Bound::Exact(0)) {
            let not_bound = each.not().ok_or(Contradiction)?;
            for coord in maybe {
                _ =
                    self.try_add_hint(Hint::Count(coord.neighbors().judged(judgment), not_bound))?;
            }
            Ok(true)
        } else if let Bound::AtLeast(min) | Bound::Exact(min) = count
            && min == maybe.len()
        {
            for coord in maybe {
                _ = self
                    .try_add_hint(Hint::Count(coord.neighbors().judged(judgment), each.into()))?;
            }
            Ok(true)
        } else if let Some(maybe) = maybe.non_empty() {
            self.generic.push(Hint::CountWithNeighbors {
                set: SetExpr1::Regular(maybe),
                each,
                count,
                judgment,
            });
            Ok(maybe != set)
        } else if count.matches(0) {
            Ok(false)
        } else {
            Err(Contradiction)
        }
    }

    fn add_theorem(&mut self, thm: Theorem) -> Result<bool, Contradiction> {
        let updated = match thm {
            Theorem::CriminalCount(set, count) => self
                .criminal_count
                .entry(set)
                .or_insert_with(|| (CountSet::max(set.len().get()), TheoremState::Active))
                .0
                .intersect(count)?,
            Theorem::All(set, judgment) => {
                set.into_iter().try_fold(false, move |updated, coord| {
                    Ok(updated | self.try_add_hint(Hint::Judgment(coord, judgment))?)
                })?
            }
            Theorem::CriminalDifference(sets, difference) => {
                self.add_difference(sets, difference)?
            }
            Theorem::CriminalSum(sets, sum) => self.add_sum(sets, sum)?,
            Theorem::Connected(set, judgment) => self.connected.insert((set, judgment)),
            Theorem::Generic(hint) => self.try_add_hint(hint)?,
            Theorem::True => false,
            Theorem::False => return Err(Contradiction),
        };
        Ok(updated)
    }

    fn add_complex_sum(&mut self, sets: [SetOp1; 2], sum: Bound) -> Result<bool, Contradiction> {
        let updated: bool = if let [SetOp1::Judged(a, judgment_a), SetOp1::Judged(b, judgment_b)] =
            sets
            && let [SetExpr1::Regular(a), SetExpr1::Regular(b)] = [a, b].map(|set| *set)
        {
            let sizes = [a, b].map(|set| set.len().get());
            let judgments = [judgment_a, judgment_b];
            match SumOrDiff::from_sum(sum, sizes, judgments)? {
                Ok(SumOrDiff::Sum(sum)) => self.add_sum([a, b], sum)?,
                Ok(SumOrDiff::Diff(difference)) => self.add_difference([a, b], difference)?,
                Err(parity) => match (a ^ b).non_empty() {
                    Some(set) => {
                        let count = CountSet::from_parity(parity, set.len());
                        self.add_theorem(Theorem::CriminalCount(set, count))?
                    }
                    None if parity.matches(0) => false,
                    None => return Err(Contradiction),
                },
            }
        } else {
            todo!()
        };
        Ok(updated)
    }

    fn add_complex_diff(
        &mut self,
        sets: [SetOp1; 2],
        compare: Comparison,
    ) -> Result<bool, Contradiction> {
        let updated = if let [SetOp1::Judged(a, judgment_a), SetOp1::Judged(b, judgment_b)] = sets
            && let [SetExpr1::Regular(a), SetExpr1::Regular(b)] = [a, b].map(|set| *set)
        {
            let sets = [a, b];
            let sizes = sets.map(|set| set.len().get());
            let judgments = [judgment_a, judgment_b];
            let sum_or_diff = SumOrDiff::from_diff(compare, sizes, judgments)?;

            match sum_or_diff {
                SumOrDiff::Diff(difference) => self.add_difference(sets, difference)?,
                SumOrDiff::Sum(sum) => self.add_sum(sets, sum)?,
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

    fn add_theorems(
        &mut self,
        new: impl IntoIterator<Item = Theorem>,
    ) -> Result<bool, Contradiction> {
        new.into_iter().try_fold(false, |updated, theorem| {
            Ok(updated | self.add_theorem(theorem)?)
        })
    }

    fn try_add_hint(&mut self, hint: Hint) -> Result<bool, Contradiction> {
        let updated = match hint {
            Hint::Judgment(coord, judgment) => match self.known[coord] {
                Some(known) if known != judgment => return Err(Contradiction),
                Some(_) => false,
                None => {
                    self.known[coord] = Some(judgment);
                    self.new_knowledge.push((coord, judgment));
                    true
                }
            },
            Hint::Count(set, count) => self.simplify_and_add_count(&set, count)?,
            Hint::CountTotal(sets, count) => self.add_complex_sum(sets, count)?,
            Hint::Connected(set) => match set {
                SetOp1::Judged(inner, judgment) => {
                    if let &SetExpr1::Regular(set) = inner.as_ref() {
                        self.connected.insert((set, judgment))
                    } else {
                        todo!()
                    }
                }
                SetOp1::Shift(inner, dir) => {
                    match inner.intersect_set1(Set1::shift_preimage(dir)) {
                        SetOp::Empty if Set::empty().connected() => false,
                        SetOp::Empty => return Err(Contradiction),
                        SetOp::NonEmpty(set) => self.try_add_hint(Hint::Connected(set))?,
                    }
                }
                SetOp1::Intersection(..) => todo!(),
            },
            Hint::CompareSets(sets, compare) => self.add_complex_diff(sets, compare)?,
            Hint::UniqueWithCount { sets, bound } => {
                let mut yes = None;
                let mut maybe = Vec::new();
                for set in sets {
                    match TriState::has_size_1_op(&set, bound.into(), &self.known) {
                        TriState::No => {}
                        TriState::Maybe => maybe.push(set),
                        TriState::Yes => {
                            if yes.replace(set).is_some() {
                                return Err(Contradiction);
                            }
                        }
                    }
                }
                if yes.is_some() {
                    !maybe.is_empty() && {
                        let count = bound.not().ok_or(Contradiction)?;
                        self.add_theorems(
                            maybe.into_iter().map(|set| Hint::Count(set, count).into()),
                        )?
                    }
                } else {
                    match <[_; 1]>::try_from(maybe) {
                        Ok([unique]) => self.simplify_and_add_count(&unique, bound.into())?,
                        Err(maybe) => {
                            let Ok(sets) = Vec1::try_from(maybe) else {
                                return Err(Contradiction);
                            };
                            self.generic.push(Hint::UniqueWithCount { sets, bound });
                            false
                        }
                    }
                }
            }
            Hint::EachNeighbors(set, count, judgment) => match set {
                SetExpr1::Regular(set) => set.into_iter().try_fold(false, |updated, coord| {
                    Ok(updated
                        | self.simplify_and_add_count(
                            &coord.neighbors().judged(judgment),
                            count.into(),
                        )?)
                })?,
                SetExpr1::Op(_) => todo!(),
            },
            Hint::CountWithNeighbors {
                set,
                each,
                count,
                judgment,
            } => {
                if let SetExpr1::Regular(set) = set {
                    self.add_count_with_neighbors(each, count, judgment, set)?
                } else {
                    todo!()
                }
            }
            Hint::IfThen(clauses) => self.add_if_then(clauses)?,
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

#[cfg(test)]
impl InferenceSolver {
    fn solve_with_validation(
        &mut self,
        solution: &Solution,
    ) -> Result<Vec<(Coord, Judgment)>, Contradiction> {
        self.validation_solution = Some(solution.clone());
        self.update_until_stuck_or_new_knowledge()?;
        Ok(mem::take(&mut self.new_knowledge))
    }

    fn validate_theorems(&self, tactic: Tactic) {
        let solution = self
            .validation_solution
            .as_ref()
            .expect("validation solution");
        for theorem in self.theorems() {
            assert!(
                theorem.validate(solution),
                "invalid inference theorem after {}: {theorem:?}",
                tactic.name()
            );
        }
    }

    fn theorems(&self) -> Vec<Theorem> {
        self.criminal_count
            .iter()
            .map(|(&set, &(count, _))| Theorem::CriminalCount(set, count))
            .chain(
                self.connected
                    .iter()
                    .copied()
                    .map(|(set, judgment)| Theorem::Connected(set, judgment)),
            )
            .chain(
                self.difference
                    .iter()
                    .map(|(&sets, &difference)| Theorem::CriminalDifference(sets, difference)),
            )
            .chain(
                self.sum
                    .iter()
                    .map(|(&sets, &sum)| Theorem::CriminalSum(sets, sum)),
            )
            .chain(self.generic.iter().cloned().map(Theorem::Generic))
            .chain(
                self.known
                    .iter()
                    .filter_map(|(coord, judgment)| judgment.map(|judgment| (coord, judgment)))
                    .map(|(coord, judgment)| Hint::Judgment(coord, judgment).into()),
            )
            .chain(
                self.new_knowledge
                    .iter()
                    .copied()
                    .map(|(coord, judgment)| Hint::Judgment(coord, judgment).into()),
            )
            .collect()
    }
}

#[derive(Clone, Copy, Debug)]
enum Tactic {
    ProcessGenericHints,
    ExtractTightCounts,
    SimplifyCriminalCounts,
    SimplifyDifferences,
    SimplifyConnected,
    CountsFromDifferences,
    CombineCriminalCounts,
    Bifurcate,
    InclusionExclusion,
}

impl Tactic {
    fn name(self) -> &'static str {
        match self {
            Self::ProcessGenericHints => "processing hints",
            Self::ExtractTightCounts => "finding tight counts",
            Self::SimplifyCriminalCounts => "simplifying counts",
            Self::SimplifyDifferences => "simplifying differences",
            Self::SimplifyConnected => "simplifying connected hints",
            Self::CountsFromDifferences => "inferring counts from differences",
            Self::CombineCriminalCounts => "comparing count pairs",
            Self::InclusionExclusion => "generating bounds from inclusion-exclusion",
            Self::Bifurcate => "bifurcating",
        }
    }
}

#[derive(Debug, Clone)]
enum Theorem {
    All(Set1, Judgment),
    CriminalCount(Set1, CountSet),
    CriminalDifference([Set1; 2], Interval<i8>),
    CriminalSum([Set1; 2], Interval<Number>),
    Generic(Hint),
    True,
    False,
    Connected(Set1, Judgment),
}

#[cfg(test)]
impl Theorem {
    fn validate(&self, solution: &Solution) -> bool {
        match self {
            Self::All(set, judgment) => set.into_iter().all(|coord| solution[coord] == *judgment),
            Self::CriminalCount(set, count) => {
                count.matches(solution.select(&set.judged(Judgment::Criminal)).len())
            }
            Self::CriminalDifference([a, b], difference) => {
                let a = solution.select(&a.judged(Judgment::Criminal)).len();
                let b = solution.select(&b.judged(Judgment::Criminal)).len();
                difference.matches(&0_i8.strict_add_unsigned(a).strict_sub_unsigned(b))
            }
            Self::CriminalSum([a, b], sum) => {
                let total = solution
                    .select(&a.judged(Judgment::Criminal))
                    .len()
                    .strict_add(solution.select(&b.judged(Judgment::Criminal)).len());
                sum.matches(&total)
            }
            Self::Generic(hint) => hint.evaluate(solution),
            Self::Connected(set, judgment) => set
                .into_iter()
                .filter(|&coord| solution[coord] == *judgment)
                .collect::<Set>()
                .connected(),
            Self::True => true,
            Self::False => false,
        }
    }
}

impl From<Hint> for Theorem {
    fn from(v: Hint) -> Self {
        Self::Generic(v)
    }
}

impl From<bool> for Theorem {
    fn from(value: bool) -> Self {
        if value { Self::True } else { Self::False }
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

enum TriState {
    No,
    Maybe,
    Yes,
}

impl TriState {
    fn has_size_1_op(set: &SetOp1, count: BoundOrNot, known: &Grid<Option<Judgment>>) -> Self {
        let split = SetSimplify::split(set, known);
        let (set, count) = match &split {
            None => (set, count),
            Some(SetSimplify::Variable(set)) => (set, count),
            Some(SetSimplify::Fixed(set)) => return count.matches(set.len().get()).into(),
            Some(SetSimplify::Split(variable, fixed)) => {
                let Some(count) = count::reduce_bound_or_not(count, fixed.len().get()) else {
                    return Self::No;
                };
                (variable, count)
            }
            Some(SetSimplify::Empty) => return count.matches(0).into(),
        };
        let minimum = count.minimum();
        if minimum == 0 {
            return Self::Maybe;
        }
        let inner_count = BoundOrNot::AtLeast(minimum);
        match set {
            SetOp1::Judged(inner, _) => Self::Maybe & Self::has_size_1(inner, inner_count, known),
            SetOp1::Shift(inner, dir) => {
                Self::Maybe
                    & (Set1::all().shift(dir.flip()).len() >= minimum)
                    & Self::has_size_1_op(inner, inner_count, known)
            }
            SetOp1::Intersection(vec, fixed) => {
                Self::from(fixed.is_none_or(|fixed| fixed.len().get() >= minimum))
                    & vec
                        .iter1()
                        .map(|set| Self::has_size_1_op(set, count, known))
                        .reduce(|a, b| a & b)
            }
        }
    }

    fn has_size_1(set: &SetExpr1, count: BoundOrNot, known: &Grid<Option<Judgment>>) -> Self {
        match set {
            SetExpr1::Regular(set) => count.matches(set.len().get()).into(),
            SetExpr1::Op(set) => Self::has_size_1_op(set, count, known),
        }
    }
}

impl BitAnd for TriState {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match [self, rhs] {
            [Self::No | Self::Maybe | Self::Yes, Self::No]
            | [Self::No, Self::Maybe | Self::Yes] => Self::No,
            [Self::Maybe | Self::Yes, Self::Maybe] | [Self::Maybe, Self::Yes] => Self::Maybe,
            [Self::Yes, Self::Yes] => Self::Yes,
        }
    }
}

impl BitAnd<bool> for TriState {
    type Output = Self;

    fn bitand(self, rhs: bool) -> Self::Output {
        self & Self::from(rhs)
    }
}

impl From<bool> for TriState {
    fn from(value: bool) -> Self {
        if value { Self::Yes } else { Self::No }
    }
}

enum SumOrDiff {
    Sum(Interval<Number>),
    Diff(Interval<i8>),
}

impl SumOrDiff {
    fn from_sum(
        sum: Bound,
        sizes: [Number; 2],
        judgments: [Judgment; 2],
    ) -> Result<Result<Self, Parity>, Contradiction> {
        let sum_or_diff = match judgments {
            [Judgment::Innocent, Judgment::Innocent] => {
                // (|a| - crim(a)) + (|b| - crim(b)) = sum
                // crim(a) + crim(b) = |a| + |b| - sum
                let max = sizes[0].strict_add(sizes[1]);
                match sum {
                    Bound::Exact(value) => Ok(Interval::exact(
                        max.checked_sub(value).ok_or(Contradiction)?,
                    )),
                    Bound::AtLeast(value) => Ok(Interval::zero_to(
                        max.checked_sub(value).ok_or(Contradiction)?,
                    )),
                    Bound::AtMost(value) => {
                        let min = max.checked_sub(value).ok_or(Contradiction)?;
                        Ok(Interval::range(min, max).expect("min = max - value"))
                    }
                    Bound::Parity(parity) => Err(parity),
                }
                .map(Self::Sum)
            }
            [Judgment::Innocent, Judgment::Criminal] => {
                // (|a| - crim(a)) + crim(b) = sum
                // crim(a) - crim(b) = |a| -  sum
                match sum {
                    Bound::Exact(value) => {
                        let exact = 0_i8
                            .strict_add_unsigned(sizes[0])
                            .strict_sub_unsigned(value);
                        Ok(Interval::exact(exact))
                    }
                    Bound::AtLeast(value) => Ok(Interval::range(
                        sizes[1].try_into().expect("at most i8::MAX"),
                        0_i8.strict_add_unsigned(sizes[0])
                            .strict_sub_unsigned(value),
                    )
                    .ok_or(Contradiction)?),
                    Bound::AtMost(value) => {
                        let max: i8 = sizes[0].try_into().expect("at most i8::MAX");
                        Ok(Interval::range(max.strict_sub_unsigned(value), max)
                            .expect("min = max - value"))
                    }
                    Bound::Parity(parity) => Err(parity),
                }
                .map(Self::Diff)
            }
            [Judgment::Criminal, Judgment::Innocent] => {
                // crim(a) + (|b| - crim(b)) = sum
                // crim(a) - crim(b) = sum - |b|
                match sum {
                    Bound::Exact(value) => {
                        let exact = 0_i8
                            .strict_add_unsigned(value)
                            .strict_sub_unsigned(sizes[1]);
                        Ok(Interval::exact(exact))
                    }
                    Bound::AtLeast(value) => Ok(Interval::range(
                        0_i8.strict_sub_unsigned(sizes[1])
                            .strict_add_unsigned(value),
                        sizes[0].try_into().expect("at most i8::MAX"),
                    )
                    .ok_or(Contradiction)?),
                    Bound::AtMost(value) => {
                        let min = i8::try_from(sizes[1])
                            .expect("at most i8::MAX")
                            .strict_neg();
                        Ok(Interval::range(min, min.strict_add_unsigned(value))
                            .expect("min = max - value"))
                    }
                    Bound::Parity(parity) => Err(parity),
                }
                .map(Self::Diff)
            }
            [Judgment::Criminal, Judgment::Criminal] => {
                // crim(a) + crim(b) = sum
                match sum {
                    Bound::Exact(value) => Ok(Interval::exact(value)),
                    Bound::AtLeast(value) => {
                        let max = sizes[0].strict_add(sizes[1]);
                        Ok(Interval::range(value, max).ok_or(Contradiction)?)
                    }
                    Bound::AtMost(value) => Ok(Interval::zero_to(value)),
                    Bound::Parity(parity) => Err(parity),
                }
                .map(Self::Sum)
            }
        };
        Ok(sum_or_diff)
    }

    fn from_diff(
        compare: Comparison,
        sizes: [Number; 2],
        judgments: [Judgment; 2],
    ) -> Result<Self, Contradiction> {
        let sum_or_diff = match judgments {
            [Judgment::Innocent, Judgment::Innocent] => {
                // (|a| - crim(a)) - (|b| - crim(b)) = excess
                // crim(a) - crim(b) = |a| - |b| - excess
                let len_diff = 0_i8
                    .strict_add_unsigned(sizes[0])
                    .strict_sub_unsigned(sizes[1]);
                let difference = match compare {
                    Comparison::ExactDifference(excess) => {
                        Interval::exact(len_diff.strict_sub_unsigned(excess))
                    }
                    Comparison::More => {
                        Interval::range(0_i8.strict_sub_unsigned(sizes[1]), len_diff.strict_sub(1))
                            .ok_or(Contradiction)?
                    }
                };
                Self::Diff(difference)
            }
            [Judgment::Innocent, Judgment::Criminal] => {
                // (|a| - crim(a)) - crim(b) = excess
                // crim(a) + crim(b) = |a| -  excess
                let sum = match compare {
                    Comparison::ExactDifference(excess) => {
                        Interval::exact(sizes[0].strict_sub(excess))
                    }
                    Comparison::More => Interval::zero_to(sizes[0].strict_sub(1)),
                };
                Self::Sum(sum)
            }
            [Judgment::Criminal, Judgment::Innocent] => {
                // crim(a) - (|b| - crim(b)) = excess
                // crim(a) + crim(b) = |b| + excess
                let sum = match compare {
                    Comparison::ExactDifference(excess) => {
                        Interval::exact(sizes[1].strict_add(excess))
                    }
                    Comparison::More => {
                        Interval::range(sizes[1].strict_add(1), sizes[0].strict_add(sizes[1]))
                            .ok_or(Contradiction)?
                    }
                };
                Self::Sum(sum)
            }
            [Judgment::Criminal, Judgment::Criminal] => {
                // crim(a) - crim(b) = count
                let difference = match compare {
                    Comparison::ExactDifference(excess) => {
                        Interval::exact(excess.try_into().expect("at most 20"))
                    }
                    Comparison::More => {
                        Interval::range(1_i8, sizes[0].try_into().expect("at most 20"))
                            .ok_or(Contradiction)?
                    }
                };
                Self::Diff(difference)
            }
        };
        Ok(sum_or_diff)
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

    use crate::models::Puzzle;
    use crate::solver::board::Lookup;
    use crate::solver::hint::Sentence;
    use crate::solver::hint::recipes::AddContext as _;

    use super::*;

    // TODO reuse `solver::test::solve`
    fn solve_with_validation(puzzle: &Puzzle) {
        eprintln!("solving puzzle: {}", puzzle.metadata);
        let solution = puzzle.solution();
        let mut engine = InferenceSolver::default();
        engine.add_parsed_hint(&Hint::Judgment(
            puzzle.start,
            puzzle.cards[puzzle.start].judgment,
        ));
        let mut pending = vec![puzzle.start];
        let mut marked = Grid::filled(false);
        let lookup = Lookup::new(&puzzle.cards);

        while !pending.is_empty() {
            for speaker in pending {
                if let Some(hint) = puzzle.cards[speaker].hint.as_logical() {
                    Sentence::parse(hint)
                        .unwrap()
                        .add_context(lookup.context(speaker))
                        .unwrap()
                        .into_iter()
                        .for_each(|hint| engine.add_parsed_hint(&hint));
                }
            }
            pending = engine
                .solve_with_validation(&solution)
                .unwrap()
                .into_iter()
                .filter(|&(coord, judgment)| {
                    assert_eq!(puzzle.cards[coord].judgment, judgment);
                    !marked[coord] && {
                        marked[coord] = true;
                        true
                    }
                })
                .map(|(coord, _)| coord)
                .collect();
        }

        let marked = marked.into_values().filter(|&marked| marked).count();
        assert_eq!(marked, 20, "state: {engine:#?}");
    }

    test_each_file! { in "./archive" as puzzle => inference}

    fn inference(puzzle: &str) {
        let puzzle = ron::from_str(puzzle).unwrap();
        solve_with_validation(&puzzle);
    }
}
