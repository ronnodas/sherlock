use std::ops::Index;

use crate::grid::Grid;
use crate::models::{Coord, Judgment};
use crate::solver::board::coordinates::Set;

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) struct Solution(Grid<Judgment>);

impl Solution {
    pub(crate) fn as_grid(&self) -> &Grid<Judgment> {
        &self.0
    }

    pub(crate) fn select<E: SetEval>(&self, set: &E) -> Set {
        set.eval(self)
    }

    pub(crate) fn all(fixed_values: impl IntoIterator<Item = (Coord, Judgment)>) -> Vec<Self> {
        Generator::new(fixed_values).collect()
    }
}

impl From<Grid<Judgment>> for Solution {
    fn from(grid: Grid<Judgment>) -> Self {
        Self(grid)
    }
}

impl Index<Coord> for Solution {
    type Output = Judgment;

    fn index(&self, index: Coord) -> &Self::Output {
        &self.0[index]
    }
}

struct Generator {
    bitmask: u32,
    template: Grid<Judgment>,
    free_indices: Set,
}

impl Generator {
    fn new(fixed_values: impl IntoIterator<Item = (Coord, Judgment)>) -> Self {
        let mut template = Grid::filled(Judgment::Innocent);
        let mut fixed_mask = Grid::filled(false);

        for (idx, val) in fixed_values {
            template[idx] = val;
            fixed_mask[idx] = true;
        }

        let free_indices: Set = Coord::all()
            .into_iter()
            .filter(|i| !fixed_mask[*i])
            .collect();

        Self {
            bitmask: 1_u32 << free_indices.len(),
            template,
            free_indices,
        }
    }
}

impl Iterator for Generator {
    type Item = Solution;

    fn next(&mut self) -> Option<Self::Item> {
        self.bitmask = self.bitmask.checked_sub(1)?;

        let mut current = self.template.clone();

        for (bit_pos, coord) in self.free_indices.into_iter().enumerate() {
            // Check if the nth bit of the counter is set
            current[coord] = if (self.bitmask >> bit_pos) & 1 == 0 {
                Judgment::Criminal
            } else {
                Judgment::Innocent
            };
        }

        Some(current.into())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.bitmask
            .try_into()
            .map_or((usize::MAX, None), |remaining| (remaining, Some(remaining)))
    }
}

pub(crate) trait SetEval {
    fn eval(&self, solution: &Solution) -> Set;
}
