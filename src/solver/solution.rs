use std::ops::Index;

use crate::solver::Judgment;
use crate::solver::grid::Coordinate;
use crate::solver::hint::Set;

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) struct Solution([Judgment; 20]);

impl Solution {
    pub(crate) const fn as_array(&self) -> &[Judgment; 20] {
        &self.0
    }

    pub(crate) fn select(&self, set: &Set, judgment: Judgment) -> impl Iterator<Item = Coordinate> {
        set.iter()
            .filter(move |&&coord| self[coord] == judgment)
            .copied()
    }

    pub(crate) fn all(fixed_values: impl IntoIterator<Item = (usize, Judgment)>) -> Vec<Self> {
        Generator::new(fixed_values).collect()
    }
}

impl From<[Judgment; 20]> for Solution {
    fn from(array: [Judgment; 20]) -> Self {
        Self(array)
    }
}

impl Index<Coordinate> for Solution {
    type Output = Judgment;

    fn index(&self, index: Coordinate) -> &Self::Output {
        &self.0[index.to_index()]
    }
}

struct Generator {
    counter: u32,
    template: [Judgment; 20],
    free_indices: Vec<usize>,
}

impl Generator {
    pub(crate) fn new(fixed_values: impl IntoIterator<Item = (usize, Judgment)>) -> Self {
        let mut template = [Judgment::Innocent; 20];
        let mut fixed_mask = [false; 20];

        for (idx, val) in fixed_values {
            template[idx] = val;
            fixed_mask[idx] = true;
        }

        let free_indices: Vec<usize> = (0..20).filter(|i| !fixed_mask[*i]).collect();

        Self {
            counter: 0,
            template,
            free_indices,
        }
    }

    pub(crate) const fn max_counter(&self) -> u32 {
        1_u32 << self.free_indices.len()
    }
}

impl Iterator for Generator {
    type Item = Solution;

    fn next(&mut self) -> Option<Self::Item> {
        if self.counter >= self.max_counter() {
            return None;
        }

        let mut current = self.template;

        for (bit_pos, &target_idx) in self.free_indices.iter().enumerate() {
            // Check if the nth bit of the counter is set
            if (self.counter >> bit_pos) & 1 == 1 {
                current[target_idx] = Judgment::Criminal;
            } else {
                current[target_idx] = Judgment::Innocent;
            }
        }

        self.counter += 1;
        Some(current.into())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.max_counter() - self.counter)
            .try_into()
            .map_or((usize::MAX, None), |remaining| (remaining, Some(remaining)))
    }
}
