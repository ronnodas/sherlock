use std::ops::Index;

use mitsein::iter1::IntoIterator1 as _;

use crate::grid::Grid;
use crate::models::{Coordinate, Judgment};
use crate::solver::board::coordinates::{ModifiedSet, Modifier, Set};

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) struct Solution(Grid<Judgment>);

impl Solution {
    pub(crate) fn as_grid(&self) -> &Grid<Judgment> {
        &self.0
    }

    pub(crate) fn select(&self, set: &ModifiedSet) -> Set {
        match set {
            ModifiedSet::Empty => Set::empty(),
            &ModifiedSet::Regular(set) => set,
            ModifiedSet::Modified(inner, modifier) => {
                let inner = self.select(inner);
                match *modifier {
                    Modifier::Shift(direction) => inner.shift(direction),
                    Modifier::Judgment(judgment) => inner
                        .into_iter()
                        .filter(move |&coord| self[coord] == judgment)
                        .collect(),
                }
            }
            ModifiedSet::Intersection(sets) => sets
                .into_iter1()
                .map(|set| self.select(set))
                .reduce(|a, b| a & b),
        }
    }

    pub(crate) fn all(fixed_values: impl IntoIterator<Item = (Coordinate, Judgment)>) -> Vec<Self> {
        Generator::new(fixed_values).collect()
    }
}

impl From<Grid<Judgment>> for Solution {
    fn from(grid: Grid<Judgment>) -> Self {
        Self(grid)
    }
}

impl Index<Coordinate> for Solution {
    type Output = Judgment;

    fn index(&self, index: Coordinate) -> &Self::Output {
        &self.0[index]
    }
}

struct Generator {
    counter: u32,
    template: Grid<Judgment>,
    free_indices: Vec<Coordinate>,
}

impl Generator {
    fn new(fixed_values: impl IntoIterator<Item = (Coordinate, Judgment)>) -> Self {
        let mut template = Grid::filled(Judgment::Innocent);
        let mut fixed_mask = Grid::filled(false);

        for (idx, val) in fixed_values {
            template[idx] = val;
            fixed_mask[idx] = true;
        }

        let free_indices: Vec<Coordinate> = Coordinate::all()
            .into_iter()
            .filter(|i| !fixed_mask[*i])
            .collect();

        Self {
            counter: 0,
            template,
            free_indices,
        }
    }

    fn max_counter(&self) -> u32 {
        1_u32 << self.free_indices.len()
    }
}

impl Iterator for Generator {
    type Item = Solution;

    fn next(&mut self) -> Option<Self::Item> {
        if self.counter >= self.max_counter() {
            return None;
        }

        let mut current = self.template.clone();

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
