use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::ops::BitAnd;
use std::{assert_matches, error, fmt, mem, result};

use indexmap::{IndexMap, IndexSet};
use itertools::Itertools as _;
use mitsein::EmptyError;
use mitsein::slice1::Slice1;
use mitsein::vec1::Vec1;
use strum::VariantArray;

use crate::grid::Grid;
#[cfg(test)]
use crate::models::Solution;
use crate::models::{Coord, Judged, Judgment};
use crate::solver::Engine;
use crate::solver::board::coordinates::{Set, Set1};
use crate::solver::hint::{
    Bound, BoundOrNot, Comparison, CountWithNeighbors, Hint, Number, Parity, ShiftedCount,
    UniqueWithCount,
};
use crate::solver::inference::count::{CountSet, EndPoint, Interval};
use crate::solver::inference::split::ByJudgment;

mod count;
mod split;

type Result<T, E = Contradiction> = result::Result<T, E>;

const MAX_BIFURCATION_DEPTH: u8 = 2;

#[derive(Debug, Default, Clone)]
pub(super) struct InferenceSolver {
    known: Grid<Option<Judgment>>,
    new_knowledge: Vec<(Coord, Judgment)>,
    criminal_count: IndexMap<Set1, CountSet>,
    connected: IndexSet<(Set1, Judgment)>,
    difference: IndexMap<[Set1; 2], Interval<i8>>,
    sum: IndexMap<[Set1; 2], Interval<Number>>,
    disjunctions: IndexSet<[Judged<Coord>; 2]>,
    shifted_counts: Vec<ShiftedCount>,
    count_with_neighbors: Vec<CountWithNeighbors>,
    unique_with_count: Vec<UniqueWithCount>,
    unprocessed: Vec<Hint>,
    allowed_depth: Option<u8>,
    #[cfg(test)]
    validation_solution: Option<Solution>,
}

impl InferenceSolver {
    fn update_phase(&mut self, tactic: Tactic) -> Result<bool> {
        #[cfg(test)]
        if self.allowed_depth.is_none() {
            eprintln!("{}", tactic.name());
        }
        let updated = self.apply_tactic(tactic)?;
        #[cfg(test)]
        if self.validation_solution.is_some() {
            self.validate_theorems(tactic);
        }
        Ok(updated)
    }

    fn apply_tactic(&mut self, tactic: Tactic) -> Result<bool> {
        match tactic {
            Tactic::ProcessHints => self.process_hints(),
            Tactic::SimplifyDisjunctions => self.simplify_disjunctions(),
            Tactic::SimplifyCriminalCounts => self.simplify_criminal_counts(),
            Tactic::ExtractTightCounts => self.extract_tight_counts(),
            Tactic::SimplifySums => self.simplify_sums(),
            Tactic::SimplifyDifferences => self.simplify_differences(),
            Tactic::SimplifyConnected => self.simplify_connected(),
            Tactic::SimplifyShiftedCounts => self.simplify_shifted_counts(),
            Tactic::SimplifyCountsWithNeighbors => self.simplify_count_with_neighbors(),
            Tactic::SimplifyUniqueWithCount => self.simplify_unique_with_count(),
            Tactic::CountsFromDifferences => self.counts_from_differences(),
            Tactic::CountsFromSums => self.counts_from_sums(),
            Tactic::CombineCriminalCounts => self.combine_criminal_counts(),
            Tactic::InclusionExclusion => self.inclusion_exclusion_bounds(),
            Tactic::Bifurcate => self.bifurcate(),
        }
    }

    fn update_theorems_or_contradict(&mut self) -> Result<bool> {
        for &tactic in Tactic::VARIANTS {
            if self.update_phase(tactic)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn process_hints(&mut self) -> Result<bool> {
        let hints = mem::take(&mut self.unprocessed);
        hints.into_iter().try_fold(
            false,
            |updated, hint| Ok(updated | self.try_add_hint(hint)?),
        )
    }

    fn simplify_criminal_counts(&mut self) -> Result<bool> {
        let mut discard = Vec::new();
        let theorems: Vec<Theorem> = self
            .criminal_count
            .iter()
            .enumerate()
            .map(|(index, (&set, count))| {
                let [variable, fixed] =
                    ByJudgment::split(set, &self.known).only(Judgment::Criminal);
                let Some(count) = count.checked_sub(fixed.len()) else {
                    return Theorem::False;
                };
                if let Some(variable) = variable.non_empty() {
                    if variable != set {
                        discard.push(index);
                    }
                    Theorem::CriminalCount(variable, count)
                } else {
                    discard.push(index);
                    count.matches(0).into()
                }
            })
            .collect();
        for index in discard.into_iter().rev() {
            assert_matches!(self.criminal_count.shift_remove_index(index), Some(_));
        }
        self.add_theorems(theorems)
    }

    fn simplify_differences(&mut self) -> Result<bool> {
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

    fn simplify_sums(&mut self) -> Result<bool> {
        let mut discard = BTreeSet::new();
        let mut new = Vec::new();
        for (index, (sets, sum)) in self.sum.iter().enumerate() {
            let [[a_var, a_fixed], [b_var, b_fixed]] =
                sets.map(|set| ByJudgment::split(set, &self.known).only(Judgment::Criminal));
            let sum = sum
                .checked_sub(a_fixed.len())
                .and_then(|sum| sum.checked_sub(b_fixed.len()))
                .ok_or(Contradiction)?;
            match [a_var, b_var].map(Set::non_empty) {
                [None, None] => {
                    if !sum.matches(&0) {
                        return Err(Contradiction);
                    }
                }
                [None, Some(set)] | [Some(set), None] => {
                    new.push(Theorem::CriminalCount(set, sum.into()));
                }
                [Some(a_var), Some(b_var)] => {
                    if &[a_var, b_var] == sets
                        && (a_var & b_var).non_empty().is_none_or(|intersection| {
                            self.criminal_count
                                .get(&intersection)
                                .is_none_or(|intersection| !intersection.is_exact())
                        })
                    {
                        continue;
                    }
                    let sets = [a_var, b_var];
                    let [a_len, b_len] = sets.map(|set| set.len().get());
                    let sum = sum
                        .intersection(Interval::zero_to(b_len.strict_add(a_len)))
                        .ok_or(Contradiction)?;
                    new.push(Theorem::CriminalSum(sets, sum));
                }
            }
            _ = discard.insert(index);
        }
        let updated = !discard.is_empty();
        for index in discard.into_iter().rev() {
            assert_matches!(self.sum.swap_remove_index(index), Some(_));
        }
        Ok(updated | self.add_theorems(new)?)
    }

    fn simplify_connected(&mut self) -> Result<bool> {
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
                    new.extend(Theorem::all(unique | fixed, judgment));
                    new.extend(Theorem::all(variable.difference(unique), !judgment));
                } else {
                    if possibilities.len().get() == matches_count {
                        discard.push(index);
                    }
                    let all = possibilities.iter1().copied().reduce(|a, b| a & b);
                    let any = possibilities.iter1().copied().reduce(|a, b| a | b);

                    new.extend(Theorem::all(all, judgment));
                    new.extend(Theorem::all(variable.difference(any), !judgment));

                    for a_judgment in [Judgment::Innocent, Judgment::Criminal] {
                        for b_judgment in [Judgment::Innocent, Judgment::Criminal] {
                            let a_matches = a_judgment == judgment;
                            let b_matches = b_judgment == judgment;

                            for [a, b] in any.difference(all).into_iter().array_combinations() {
                                if possibilities.iter().all(|&possibility| {
                                    possibility.contains(a) == a_matches
                                        || possibility.contains(b) == b_matches
                                }) {
                                    new.push(Theorem::Disjunction([
                                        Judged(a, a_judgment),
                                        Judged(b, b_judgment),
                                    ]));
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

    fn extract_tight_counts(&mut self) -> Result<bool> {
        let mut discard = Vec::new();
        let new: Vec<Theorem> = self
            .criminal_count
            .iter()
            .enumerate()
            .map(|(index, (&set, &count))| {
                Ok(count.tight(set)?.map(|endpoint| {
                    discard.push(index);
                    let judgment = match endpoint {
                        EndPoint::Minimum => Judgment::Criminal,
                        EndPoint::Maximum => Judgment::Innocent,
                    };
                    Theorem::all(set, judgment)
                }))
            })
            .flatten_ok()
            .flatten_ok()
            .try_collect()?;
        for index in discard.into_iter().rev() {
            let discard = self.criminal_count.swap_remove_index(index);
            assert_matches!(discard, Some(_), "invalid discard");
        }

        self.add_theorems(new)
    }

    fn combine_criminal_counts(&mut self) -> Result<bool> {
        let mut new = Vec::new();
        // TODO array_permutations()
        for [(a, a_count), (b, b_count)] in self.criminal_count.iter().array_combinations() {
            if a <= b {
                let count = b_count.checked_sub(*a_count).ok_or(Contradiction)?;
                new.push(Theorem::CriminalCount(
                    b.difference(Set::from(*a)).non_empty().expect("a != b"),
                    count,
                ));
            } else if b <= a {
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
        self.add_theorems(new)
    }

    /// This is quite a conservative version, only propagating the bounds if there are already known
    /// bounds for both the union and the intersection
    fn inclusion_exclusion_bounds(&mut self) -> Result<bool> {
        let mut new = Vec::new();
        for [(&a, a_count), (&b, b_count)] in self.criminal_count.iter().array_combinations() {
            let union = a | b;
            let sum = a_count.strict_add(*b_count);
            if let Some(intersection) = (a & b).non_empty() {
                if let Some(&union_bound) = self.criminal_count.get(&union)
                    && let Some(&intersection_bound) = self.criminal_count.get(&intersection)
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

    fn bifurcate(&mut self) -> Result<bool> {
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

    fn bifurcate_to(&mut self, depth: u8) -> Result<bool> {
        let mut new = Vec::new();
        let variable: Set = self
            .known
            .iter()
            .filter(|(_, judgment)| judgment.is_none())
            .map(|(coord, _)| coord)
            .collect();
        for coord in variable {
            let Ok(innocent) = self.bifurcation_branch(depth, coord, Judgment::Innocent) else {
                new.push((coord, Judgment::Criminal));
                continue;
            };
            let Ok(criminal) = self.bifurcation_branch(depth, coord, Judgment::Criminal) else {
                new.push((coord, Judgment::Innocent));
                continue;
            };
            for coord in variable {
                if let Some(innocent) = innocent[coord]
                    && let Some(criminal) = criminal[coord]
                    && innocent == criminal
                {
                    new.push((coord, innocent));
                }
            }
        }
        self.add_theorems(
            new.into_iter()
                .map(|(coord, judgment)| Theorem::Judgment(coord, judgment)),
        )
    }

    fn bifurcation_branch(
        &self,
        depth: u8,
        coord: Coord,
        judgment: Judgment,
    ) -> Result<Grid<Option<Judgment>>> {
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
        clone.update_until_stuck()?;
        Ok(clone.known)
    }

    fn update_until_stuck(&mut self) -> Result<()> {
        while self.update_theorems_or_contradict()? {}
        Ok(())
    }

    fn count_judged(&self, set: Set1, judgment: Judgment) -> Result<CountSet> {
        let [variable, fixed] = ByJudgment::split(set, &self.known).only(judgment);
        let variable_count = if let Some(variable) = variable.non_empty() {
            let criminal_count = self.gather_criminal_count(variable)?;
            match judgment {
                Judgment::Criminal => criminal_count,
                Judgment::Innocent => CountSet::Exact(variable.len().get())
                    .checked_sub(criminal_count)
                    .ok_or(Contradiction)?,
            }
        } else {
            CountSet::Exact(0)
        };
        Ok(variable_count.strict_add(CountSet::Exact(fixed.len())))
    }

    fn count_status(&self, set: Set1, judgment: Judgment, bound: Bound) -> Result<TriState> {
        Ok(self.count_judged(set, judgment)?.match_bound(bound))
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

    fn add_difference(&mut self, [a, b]: [Set1; 2], difference: Interval<i8>) -> Result<bool> {
        let (sets, difference) = match a.total_cmp(b) {
            Ordering::Less => ([b, a], difference.strict_neg()),
            Ordering::Equal if difference.matches(&0) => return Ok(false),
            Ordering::Equal => return Err(Contradiction),
            Ordering::Greater => ([a, b], difference),
        };
        self.difference
            .entry(sets)
            .or_insert_with(|| Interval::neg_to_pos(sets[1].len().get(), sets[0].len().get()))
            .intersect(difference)
    }

    fn add_sum(&mut self, mut sets: [Set1; 2], sum: Interval<Number>) -> Result<bool> {
        if let Some(intersection) = (sets[0] & sets[1]).non_empty() {
            if let CountSet::Exact(intersection) = self.gather_criminal_count(intersection)? {
                let sum = sum
                    .checked_sub(intersection.strict_mul(2))
                    .ok_or(Contradiction)?;
                let set = sets[0] ^ sets[1];
                let theorem = set.non_empty().map_or_else(
                    || sum.matches(&0).into(),
                    |set| Theorem::CriminalCount(set, sum.into()),
                );
                self.add_theorem(theorem)
            } else {
                sets.sort_by(|a, &b| a.total_cmp(b));
                self.sum
                    .entry(sets)
                    .or_insert_with(|| {
                        Interval::zero_to(sets[0].len().get().strict_add(sets[1].len().get()))
                    })
                    .intersect(sum)
            }
        } else {
            let set = sets[0] | sets[1];
            self.add_theorem(Theorem::CriminalCount(set, sum.into()))
        }
    }

    fn counts_from_differences(&mut self) -> Result<bool> {
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

    fn counts_from_sums(&mut self) -> Result<bool> {
        let mut new = Vec::with_capacity(self.sum.len().strict_mul(2));
        for (&[a, b], sum) in &self.sum {
            let a_min = sum.min().saturating_sub(self.max_criminals(b));
            let a_max = sum
                .max()
                .checked_sub(self.min_criminals(b))
                .ok_or(Contradiction)?;
            let count = CountSet::complex(a_min, a_max, None).ok_or(Contradiction)?;
            new.push(Theorem::CriminalCount(a, count));

            let b_min = sum.min().saturating_sub(self.max_criminals(a));
            let b_max = sum
                .max()
                .checked_sub(self.min_criminals(a))
                .ok_or(Contradiction)?;
            let count = CountSet::complex(b_min, b_max, None).ok_or(Contradiction)?;
            new.push(Theorem::CriminalCount(b, count));
        }
        self.add_theorems(new)
    }

    fn simplify_shifted_counts(&mut self) -> Result<bool> {
        let mut new = Vec::new();
        for hint in mem::take(&mut self.shifted_counts) {
            let mut guaranteed_matches = 0_u8;
            let mut possible_pairs = Vec::new();
            let mut source_only = Set::empty();
            let mut target_only = Set::empty();
            let mut has_unresolved_pair = false;
            for source_coord in hint.source {
                let Some(target_coord) = source_coord.step(hint.direction) else {
                    continue;
                };
                let source_matches =
                    self.known[source_coord].map(|known| known == hint.source_judgment);
                let target_matches =
                    self.known[target_coord].map(|known| known == hint.target_judgment);
                match [source_matches, target_matches] {
                    [Some(true), Some(true)] => {
                        guaranteed_matches = guaranteed_matches.strict_add(1);
                    }
                    [Some(false), _] | [_, Some(false)] => {}
                    [None, Some(true)] => {
                        source_only |= source_coord;
                        possible_pairs.push((source_coord, target_coord));
                    }
                    [Some(true), None] => {
                        target_only |= target_coord;
                        possible_pairs.push((source_coord, target_coord));
                    }
                    [None, None] => {
                        has_unresolved_pair = true;
                        possible_pairs.push((source_coord, target_coord));
                    }
                }
            }

            let bound = hint
                .bound
                .checked_sub(guaranteed_matches)
                .ok_or(Contradiction)?;
            if !has_unresolved_pair {
                new.extend(Theorem::shifted_count_pairs(
                    source_only.judged(hint.source_judgment),
                    target_only.judged(hint.target_judgment),
                    bound,
                ));
            }

            let minimum = bound.minimum();
            if minimum == 0 {
                new.push(Theorem::ShiftedCount(hint));
                continue;
            }
            let possible_pairs =
                Vec1::try_from(possible_pairs).map_err(|EmptyError { .. }| Contradiction)?;

            let possible_sources: Set1 =
                possible_pairs.iter1().map(|&(source, _)| source).collect1();
            let possible_targets: Set1 =
                possible_pairs.iter1().map(|&(_, target)| target).collect1();

            let source_count = self.count_judged(possible_sources, hint.source_judgment)?;
            let target_count = self.count_judged(possible_targets, hint.target_judgment)?;
            if minimum > source_count.maximum() || minimum > target_count.maximum() {
                return Err(Contradiction);
            }

            if minimum == possible_pairs.len().get().try_into().expect("at most 20") {
                new.extend(Theorem::all(possible_sources, hint.source_judgment));
                new.extend(Theorem::all(possible_targets, hint.target_judgment));
            } else {
                let source_matches = minimum >= source_count.maximum();
                let target_matches = minimum >= target_count.maximum();
                for (source_coord, target_coord) in possible_pairs {
                    let source = Judged(source_coord, hint.source_judgment);
                    let target = Judged(target_coord, hint.target_judgment);
                    if source_matches {
                        new.push(Theorem::if_then([source, target]));
                    }
                    if target_matches {
                        new.push(Theorem::if_then([target, source]));
                    }
                }
                new.push(Theorem::ShiftedCount(hint));
            }
        }
        self.add_theorems(new)
    }

    fn simplify_unique_with_count(&mut self) -> Result<bool> {
        let mut new = Vec::new();
        for hint in mem::take(&mut self.unique_with_count) {
            let mut yes = None;
            let mut maybe = Vec::new();
            for set in hint.sets {
                match self.count_status(set, hint.judgment, hint.bound)? {
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
                if !maybe.is_empty() {
                    let bound = hint.bound.not().ok_or(Contradiction)?;
                    for set in maybe {
                        new.push(Theorem::criminal_count(set, hint.judgment, bound));
                    }
                }
            } else {
                match <[_; 1]>::try_from(maybe) {
                    Ok([unique]) => {
                        new.push(Theorem::criminal_count(
                            unique,
                            hint.judgment,
                            hint.bound.into(),
                        ));
                    }
                    Err(maybe) => {
                        let sets = maybe
                            .try_into()
                            .map_err(|EmptyError { .. }| Contradiction)?;
                        new.push(Theorem::UniqueWithCount(UniqueWithCount { sets, ..hint }));
                    }
                }
            }
        }
        self.add_theorems(new)
    }

    fn simplify_disjunctions(&mut self) -> Result<bool> {
        let theorems: Vec<Theorem> = mem::take(&mut self.disjunctions)
            .into_iter()
            .map(|clauses @ [left, right]| {
                if left == right {
                    left.into()
                } else if left.0 == right.0 {
                    Theorem::False
                } else {
                    match clauses.map(|Judged(coord, judgment)| {
                        self.known[coord].map(|known| known == judgment)
                    }) {
                        [None, None] => Theorem::Disjunction(clauses),
                        [Some(true), _] | [_, Some(true)] => Theorem::True,
                        [None, Some(false)] => left.into(),
                        [Some(false), None] => right.into(),
                        [Some(false), Some(false)] => Theorem::False,
                    }
                }
            })
            .collect();
        self.add_theorems(theorems)
    }

    fn simplify_count_with_neighbors(&mut self) -> Result<bool> {
        let mut new = Vec::new();
        for hint in mem::take(&mut self.count_with_neighbors) {
            let mut yes = Set::empty();
            let mut maybe = Set::empty();
            for coord in hint.set {
                match self.count_status(coord.neighbors(), hint.judgment, hint.each)? {
                    TriState::No => {}
                    TriState::Maybe => maybe |= coord,
                    TriState::Yes => yes |= coord,
                }
            }

            let count = count::reduce_bound(hint.count, yes.len()).ok_or(Contradiction)?;
            if matches!(count, Bound::AtMost(0) | Bound::Exact(0)) {
                let not_bound = hint.each.not().ok_or(Contradiction)?;
                for coord in maybe {
                    new.push(Theorem::criminal_count(
                        coord.neighbors(),
                        hint.judgment,
                        not_bound,
                    ));
                }
            } else if let Bound::AtLeast(min) | Bound::Exact(min) = count
                && min == maybe.len()
            {
                for coord in maybe {
                    new.push(Theorem::criminal_count(
                        coord.neighbors(),
                        hint.judgment,
                        hint.each.into(),
                    ));
                }
            } else if let Some(set) = maybe.non_empty() {
                let hint = CountWithNeighbors { set, count, ..hint };
                new.push(Theorem::CountWithNeighbors(hint));
            } else if !count.matches(0) {
                return Err(Contradiction);
            }
        }
        self.add_theorems(new)
    }

    fn add_theorem(&mut self, thm: Theorem) -> Result<bool> {
        let mut updated = false;
        match thm {
            Theorem::CriminalCount(set, count) => {
                let [set, fixed] = ByJudgment::split(set, &self.known).only(Judgment::Criminal);
                let count = count.checked_sub(fixed.len()).ok_or(Contradiction)?;
                if let Some(set) = set.non_empty() {
                    updated = self
                        .criminal_count
                        .entry(set)
                        .or_insert_with(|| CountSet::max(set.len().get()))
                        .intersect(count)?;
                } else if !count.matches(0) {
                    return Err(Contradiction);
                }
            }
            Theorem::Judgment(coord, judgment) => match self.known[coord] {
                Some(known) if known != judgment => return Err(Contradiction),
                Some(_) => {}
                None => {
                    self.known[coord] = Some(judgment);
                    self.new_knowledge.push((coord, judgment));
                    updated = true;
                }
            },
            Theorem::CriminalDifference(sets, difference) => {
                updated = self.add_difference(sets, difference)?;
            }
            Theorem::CriminalSum(sets, sum) => updated = self.add_sum(sets, sum)?,
            Theorem::Connected(set, judgment) => _ = self.connected.insert((set, judgment)),
            Theorem::Disjunction(mut clauses) => {
                clauses.sort_unstable_by_key(|&Judged(coord, _)| coord);
                _ = self.disjunctions.insert(clauses);
            }
            Theorem::ShiftedCount(hint) => self.shifted_counts.push(hint),
            Theorem::CountWithNeighbors(hint) => self.count_with_neighbors.push(hint),
            Theorem::UniqueWithCount(hint) => self.unique_with_count.push(hint),
            Theorem::True => {}
            Theorem::False => return Err(Contradiction),
        }
        Ok(updated)
    }

    fn add_theorems(&mut self, new: impl IntoIterator<Item = Theorem>) -> Result<bool> {
        new.into_iter().try_fold(false, |updated, theorem| {
            Ok(updated | self.add_theorem(theorem)?)
        })
    }

    fn try_add_hint(&mut self, hint: Hint) -> Result<bool> {
        self.add_theorems(Theorem::process_hint(hint))
    }

    fn gather_criminal_count(&self, set: Set1) -> Result<CountSet> {
        if let Some(&count) = self.criminal_count.get(&set) {
            Ok(count)
        } else {
            CountSet::complex(self.min_criminals(set), self.max_criminals(set), None)
                .ok_or(Contradiction)
        }
    }
}

impl Engine for InferenceSolver {
    fn add_parsed_hint(&mut self, hint: &Hint) {
        self.unprocessed.push(hint.clone());
    }

    fn updates(&mut self) -> anyhow::Result<Vec<(Coord, Judgment)>> {
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
    fn solve_with_validation(&mut self, solution: &Solution) -> Result<Vec<(Coord, Judgment)>> {
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
            .map(|(&set, &count)| Theorem::CriminalCount(set, count))
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
            .chain(
                self.disjunctions
                    .iter()
                    .map(|&clauses| Theorem::Disjunction(clauses)),
            )
            .chain(
                self.known
                    .iter()
                    .filter_map(|(coord, judgment)| judgment.map(|judgment| (coord, judgment)))
                    .map(|(coord, judgment)| Theorem::Judgment(coord, judgment)),
            )
            .collect()
    }

    fn update_until_stuck_or_new_knowledge(&mut self) -> Result<()> {
        while self.new_knowledge.is_empty() && self.update_theorems_or_contradict()? {}
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, VariantArray)]
enum Tactic {
    ProcessHints,
    SimplifyDisjunctions,
    SimplifyCriminalCounts,
    ExtractTightCounts,
    SimplifySums,
    SimplifyDifferences,
    SimplifyConnected,
    SimplifyShiftedCounts,
    SimplifyCountsWithNeighbors,
    SimplifyUniqueWithCount,
    CountsFromDifferences,
    CountsFromSums,
    CombineCriminalCounts,
    InclusionExclusion,
    Bifurcate,
}

#[cfg(test)]
impl Tactic {
    fn name(self) -> &'static str {
        match self {
            Self::ProcessHints => "processing hints",
            Self::SimplifyDisjunctions => "simplifying or clauses",
            Self::ExtractTightCounts => "finding tight counts",
            Self::SimplifyCriminalCounts => "simplifying counts",
            Self::SimplifySums => "simplifying sums",
            Self::SimplifyDifferences => "simplifying differences",
            Self::SimplifyConnected => "simplifying connected hints",
            Self::SimplifyShiftedCounts => "simplifying clues involving shifted suspects",
            Self::SimplifyCountsWithNeighbors => {
                "simplifying clues about suspects with a given number of neighbors"
            }
            Self::SimplifyUniqueWithCount => "simplifying clues about unique counts",
            Self::CountsFromDifferences => "inferring counts from differences",
            Self::CountsFromSums => "inferring counts from sums",
            Self::CombineCriminalCounts => "comparing count pairs",
            Self::InclusionExclusion => "generating bounds from inclusion-exclusion",
            Self::Bifurcate => "bifurcating",
        }
    }
}

#[derive(Debug, Clone)]
enum Theorem {
    Judgment(Coord, Judgment),
    CriminalCount(Set1, CountSet),
    CriminalDifference([Set1; 2], Interval<i8>),
    CriminalSum([Set1; 2], Interval<Number>),
    Connected(Set1, Judgment),
    Disjunction([Judged<Coord>; 2]),
    ShiftedCount(ShiftedCount),
    CountWithNeighbors(CountWithNeighbors),
    UniqueWithCount(UniqueWithCount),
    True,
    False,
}

impl Theorem {
    fn all(set: impl Into<Set>, judgment: Judgment) -> impl Iterator<Item = Self> {
        set.into()
            .into_iter()
            .map(move |coord| Self::Judgment(coord, judgment))
    }

    fn if_then([premise, conclusion]: [Judged<Coord>; 2]) -> Self {
        Self::Disjunction([premise.flip(), conclusion])
    }

    fn criminal_count(set: Set1, judgment: Judgment, bound: BoundOrNot) -> Self {
        let count = CountSet::from_bound_or_not(bound, set.len());
        (match judgment {
            Judgment::Criminal => count,
            Judgment::Innocent => {
                count.and_then(|count| CountSet::Exact(set.len().get()).checked_sub(count))
            }
        })
        .map_or(Self::False, |count| Self::CriminalCount(set, count))
    }

    fn total_count(sets: [Judged<Set1>; 2], sum: Bound) -> Vec<Self> {
        let judgments = sets.map(|Judged(_, judgment)| judgment);
        let sets = sets.map(|Judged(set, _)| set);
        let sizes = sets.map(|set| set.len().get());
        let Ok((sum_or_diff, parity)) = SumOrDiff::from_sum(sum, sizes, judgments) else {
            return vec![Self::False];
        };

        let mut theorems = Vec::new();
        if let Some(sum_or_diff) = sum_or_diff {
            theorems.push(Self::sum_or_diff(sets, sum_or_diff));
        }

        if let Some(parity) = parity {
            theorems.push(Self::symm_diff_parity(sets, parity));
        }

        theorems
    }

    fn compare(sets: [Judged<Set1>; 2], compare: Comparison) -> Vec<Self> {
        let judgments = sets.map(|Judged(_, judgment)| judgment);
        let sets = sets.map(|Judged(set, _)| set);
        let sizes = sets.map(|set| set.len().get());
        let Ok((sum_or_diff, parity)) = SumOrDiff::from_diff(compare, sizes, judgments) else {
            return vec![Self::False];
        };

        let mut theorems = vec![Self::sum_or_diff(sets, sum_or_diff)];

        if let Some(parity) = parity {
            theorems.push(Self::symm_diff_parity(sets, parity));
        }

        theorems
    }

    fn sum_or_diff(sets: [Set1; 2], sum_or_diff: SumOrDiff) -> Self {
        match sum_or_diff {
            SumOrDiff::Sum(sum) => Self::criminal_sum(sets, sum),
            SumOrDiff::Diff(difference) => Self::criminal_difference(sets, difference),
        }
    }

    fn criminal_difference([a, b]: [Set1; 2], difference: Interval<i8>) -> Self {
        let intersection = Set::from(a) & Set::from(b);
        match [a, b].map(|set| set.difference(intersection).non_empty()) {
            [None, None] => difference.matches(&0).into(),
            [None, Some(b)] => difference
                .strict_neg()
                .non_negative()
                .map_or(Self::False, |count| Self::CriminalCount(b, count.into())),
            [Some(a), None] => difference
                .non_negative()
                .map_or(Self::False, |count| Self::CriminalCount(a, count.into())),
            [Some(a), Some(b)] => Self::CriminalDifference([a, b], difference),
        }
    }

    fn criminal_sum(sets: [Set1; 2], sum: Interval<u8>) -> Self {
        if sets[0] == sets[1] {
            sum.checked_div_2().map_or(Self::False, |count| {
                Self::CriminalCount(sets[0], count.into())
            })
        } else {
            Self::CriminalSum(sets, sum)
        }
    }

    fn symm_diff_parity([a, b]: [Set1; 2], parity: Parity) -> Self {
        (a ^ b).non_empty().map_or_else(
            || parity.matches(0).into(),
            |set| Self::CriminalCount(set, CountSet::from_parity(parity, set.len())),
        )
    }

    fn process_hint(hint: Hint) -> Vec<Self> {
        match hint {
            Hint::Judgment(coord, judgment) => vec![Self::Judgment(coord, judgment)],
            Hint::Count(set, judgment, count) => {
                vec![Self::criminal_count(set, judgment, count)]
            }
            Hint::CountTotal(sets, bound) => Self::total_count(sets, bound),
            Hint::Connected(set, judgment) => vec![Self::Connected(set, judgment)],
            Hint::CompareSets(sets, compare) => Self::compare(sets, compare),
            Hint::UniqueWithCount(hint) => vec![hint.into()],
            Hint::CountWithNeighbors(hint) => vec![hint.into()],
            Hint::IfThen(clauses) => vec![Self::if_then(clauses)],
            Hint::ShiftedCount(hint) => vec![hint.into()],
        }
    }

    fn shifted_count_pairs(source: Judged<Set>, target: Judged<Set>, bound: Bound) -> Vec<Self> {
        match [source.non_empty(), target.non_empty()] {
            [None, None] => vec![bound.matches(0).into()],
            [Some(Judged(set, judgment)), None] | [None, Some(Judged(set, judgment))] => {
                vec![Self::criminal_count(set, judgment, bound.into())]
            }
            [Some(source), Some(target)] => Self::total_count([source, target], bound),
        }
    }

    #[cfg(test)]
    fn validate(&self, solution: &Solution) -> bool {
        match self {
            &Self::Judgment(coord, judgment) => solution[coord] == judgment,
            &Self::Connected(set, judgment) => solution.select(&set.judged(judgment)).connected(),
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
            Self::Disjunction(clauses) => clauses
                .iter()
                .any(|&Judged(coord, judgment)| solution[coord] == judgment),
            Self::ShiftedCount(hint) => hint.evaluate(solution),
            Self::CountWithNeighbors(hint) => hint.evaluate(solution),
            Self::UniqueWithCount(hint) => hint.evaluate(solution),
            Self::True => true,
            Self::False => false,
        }
    }
}

impl From<bool> for Theorem {
    fn from(value: bool) -> Self {
        if value { Self::True } else { Self::False }
    }
}

impl From<Judged<Coord>> for Theorem {
    fn from(Judged(coord, judgment): Judged<Coord>) -> Self {
        Self::Judgment(coord, judgment)
    }
}

impl From<ShiftedCount> for Theorem {
    fn from(v: ShiftedCount) -> Self {
        Self::ShiftedCount(v)
    }
}

impl From<CountWithNeighbors> for Theorem {
    fn from(v: CountWithNeighbors) -> Self {
        Self::CountWithNeighbors(v)
    }
}

impl From<UniqueWithCount> for Theorem {
    fn from(v: UniqueWithCount) -> Self {
        Self::UniqueWithCount(v)
    }
}

enum TriState {
    No,
    Maybe,
    Yes,
}

impl TriState {
    fn union(self, rhs: Self) -> Self {
        match [self, rhs] {
            [Self::No, Self::No] => Self::No,
            [Self::Yes, Self::Yes] => Self::Yes,
            [_, Self::Maybe] | [Self::Maybe, _] | [Self::No, Self::Yes] | [Self::Yes, Self::No] => {
                Self::Maybe
            }
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

#[derive(Clone, Copy)]
enum SumOrDiff {
    Sum(Interval<Number>),
    Diff(Interval<i8>),
}

impl SumOrDiff {
    fn from_sum(
        sum: Bound,
        sizes: [Number; 2],
        judgments: [Judgment; 2],
    ) -> Result<(Option<Self>, Option<Parity>)> {
        let result = match judgments {
            [Judgment::Innocent, Judgment::Innocent] => {
                // (|a| - crim(a)) + (|b| - crim(b)) = sum
                // crim(a) + crim(b) = |a| + |b| - sum
                let max = sizes[0].strict_add(sizes[1]);
                let parity = sum
                    .parity()
                    .map(|parity| parity ^ Parity::of(sizes[0]) ^ Parity::of(sizes[1]));
                let sum = match sum {
                    Bound::Exact(value) => Some(Interval::exact(
                        max.checked_sub(value).ok_or(Contradiction)?,
                    )),
                    Bound::AtLeast(value) => Some(Interval::zero_to(
                        max.checked_sub(value).ok_or(Contradiction)?,
                    )),
                    Bound::AtMost(value) => {
                        let min = max.checked_sub(value).ok_or(Contradiction)?;
                        Some(Interval::range(min, max).expect("min = max - value"))
                    }
                    Bound::Parity(_) => None,
                }
                .map(Self::Sum);
                (sum, parity)
            }
            [Judgment::Innocent, Judgment::Criminal] => {
                // (|a| - crim(a)) + crim(b) = sum
                // crim(a) - crim(b) = |a| -  sum
                let parity = sum.parity().map(|parity| parity ^ Parity::of(sizes[0]));
                let diff = match sum {
                    Bound::Exact(value) => {
                        let exact = 0_i8
                            .strict_add_unsigned(sizes[0])
                            .strict_sub_unsigned(value);
                        Some(Interval::exact(exact))
                    }
                    Bound::AtLeast(value) => Some(
                        Interval::range(
                            sizes[1].try_into().expect("at most i8::MAX"),
                            0_i8.strict_add_unsigned(sizes[0])
                                .strict_sub_unsigned(value),
                        )
                        .ok_or(Contradiction)?,
                    ),
                    Bound::AtMost(value) => {
                        let max: i8 = sizes[0].try_into().expect("at most i8::MAX");
                        Some(
                            Interval::range(max.strict_sub_unsigned(value), max)
                                .expect("min = max - value"),
                        )
                    }
                    Bound::Parity(_) => None,
                }
                .map(Self::Diff);
                (diff, parity)
            }
            [Judgment::Criminal, Judgment::Innocent] => {
                // crim(a) + (|b| - crim(b)) = sum
                // crim(a) - crim(b) = sum - |b|
                let parity = sum.parity().map(|parity| parity ^ Parity::of(sizes[1]));
                let diff = match sum {
                    Bound::Exact(value) => {
                        let exact = 0_i8
                            .strict_add_unsigned(value)
                            .strict_sub_unsigned(sizes[1]);
                        Some(Interval::exact(exact))
                    }
                    Bound::AtLeast(value) => Some(
                        Interval::range(
                            0_i8.strict_sub_unsigned(sizes[1])
                                .strict_add_unsigned(value),
                            sizes[0].try_into().expect("at most i8::MAX"),
                        )
                        .ok_or(Contradiction)?,
                    ),
                    Bound::AtMost(value) => {
                        let min = i8::try_from(sizes[1])
                            .expect("at most i8::MAX")
                            .strict_neg();
                        Some(
                            Interval::range(min, min.strict_add_unsigned(value))
                                .expect("min = max - value"),
                        )
                    }
                    Bound::Parity(_) => None,
                }
                .map(Self::Diff);
                (diff, parity)
            }
            [Judgment::Criminal, Judgment::Criminal] => {
                // crim(a) + crim(b) = sum
                let parity = sum.parity();
                let sum = match sum {
                    Bound::Exact(value) => Some(Interval::exact(value)),
                    Bound::AtLeast(value) => {
                        let max = sizes[0].strict_add(sizes[1]);
                        Some(Interval::range(value, max).ok_or(Contradiction)?)
                    }
                    Bound::AtMost(value) => Some(Interval::zero_to(value)),
                    Bound::Parity(_) => None,
                }
                .map(Self::Sum);
                (sum, parity)
            }
        };
        Ok(result)
    }

    fn from_diff(
        compare: Comparison,
        sizes: [Number; 2],
        judgments: [Judgment; 2],
    ) -> Result<(Self, Option<Parity>)> {
        let sum_or_diff = match judgments {
            [Judgment::Innocent, Judgment::Innocent] => {
                // (|a| - crim(a)) - (|b| - crim(b)) = excess
                // crim(a) - crim(b) = |a| - |b| - excess
                let len_diff = 0_i8
                    .strict_add_unsigned(sizes[0])
                    .strict_sub_unsigned(sizes[1]);

                let parity = compare
                    .exact()
                    .map(|exact| Parity::of(exact) ^ Parity::of(sizes[0]) ^ Parity::of(sizes[1]));
                let difference = match compare {
                    Comparison::ExactDifference(excess) => {
                        Interval::exact(len_diff.strict_sub_unsigned(excess))
                    }
                    Comparison::More => {
                        Interval::range(0_i8.strict_sub_unsigned(sizes[1]), len_diff.strict_sub(1))
                            .ok_or(Contradiction)?
                    }
                };
                (Self::Diff(difference), parity)
            }
            [Judgment::Innocent, Judgment::Criminal] => {
                // (|a| - crim(a)) - crim(b) = excess
                // crim(a) + crim(b) = |a| -  excess
                let parity = compare
                    .exact()
                    .map(|exact| Parity::of(exact) ^ Parity::of(sizes[0]));

                let sum = match compare {
                    Comparison::ExactDifference(excess) => {
                        Interval::exact(sizes[0].strict_sub(excess))
                    }
                    Comparison::More => Interval::zero_to(sizes[0].strict_sub(1)),
                };
                (Self::Sum(sum), parity)
            }
            [Judgment::Criminal, Judgment::Innocent] => {
                // crim(a) - (|b| - crim(b)) = excess
                // crim(a) + crim(b) = |b| + excess
                let parity = compare
                    .exact()
                    .map(|exact| Parity::of(exact) ^ Parity::of(sizes[1]));
                let sum = match compare {
                    Comparison::ExactDifference(excess) => {
                        Interval::exact(sizes[1].strict_add(excess))
                    }
                    Comparison::More => {
                        Interval::range(sizes[1].strict_add(1), sizes[0].strict_add(sizes[1]))
                            .ok_or(Contradiction)?
                    }
                };
                (Self::Sum(sum), parity)
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
                (Self::Diff(difference), compare.exact().map(Parity::of))
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
    use std::collections::BTreeMap;
    use std::fmt::Write as _;

    use test_each_file::test_each_file;

    use crate::models::{Column, Puzzle, Row};
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
        assert_eq!(
            marked,
            20,
            "stuck after discovering {marked}/20 cells:\n{}",
            summarize(&engine)
        );
    }

    #[expect(clippy::use_debug, reason = "test")]
    fn summarize(engine: &InferenceSolver) -> String {
        let mut summary = String::new();
        writeln!(&mut summary, "grid (?: unknown, C: criminal, I: innocent):").unwrap();
        writeln!(&mut summary, "    A B C D").unwrap();
        for row in Row::ALL {
            write!(&mut summary, "  {row} ").unwrap();
            for col in Column::ALL {
                let cell = match engine.known[Coord { row, col }] {
                    None => '?',
                    Some(Judgment::Criminal) => 'C',
                    Some(Judgment::Innocent) => 'I',
                };
                write!(&mut summary, "{cell} ").unwrap();
            }
            writeln!(&mut summary).unwrap();
        }

        writeln!(&mut summary, "active criminal counts:").unwrap();
        for (set, count) in &engine.criminal_count {
            writeln!(&mut summary, "  {set:?}: {count:?}").unwrap();
        }

        writeln!(&mut summary, "connected: {:?}", engine.connected).unwrap();
        writeln!(&mut summary, "differences: {:?}", engine.difference).unwrap();
        writeln!(&mut summary, "sums: {:?}", engine.sum).unwrap();
        writeln!(&mut summary, "shifted counts: {:?}", engine.shifted_counts).unwrap();
        writeln!(
            &mut summary,
            "counts with neighbors: {:?}",
            engine.count_with_neighbors
        )
        .unwrap();
        writeln!(
            &mut summary,
            "unique with count: {:?}",
            engine.unique_with_count
        )
        .unwrap();

        writeln!(&mut summary, "pending disjunctions:").unwrap();
        for &[left, right] in &engine.disjunctions {
            writeln!(&mut summary, "  ({left:?}) | ({right:?})").unwrap();
        }

        let mut generic = BTreeMap::<String, usize>::new();
        for hint in &engine.unprocessed {
            let count = generic.entry(format!("{hint:?}")).or_insert(0);
            *count = count.strict_add(1);
        }
        writeln!(&mut summary, "pending hints ({} unique):", generic.len()).unwrap();
        for (hint, count) in generic {
            writeln!(&mut summary, "  {count}x {hint}").unwrap();
        }
        summary
    }

    test_each_file! { in "./archive" as puzzle => inference}

    fn inference(puzzle: &str) {
        let puzzle = ron::from_str(puzzle).unwrap();
        solve_with_validation(&puzzle);
    }
}
