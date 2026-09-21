use std::iter;

use anyhow::{Result, bail};

use crate::models::{Coord, Judgment, Solution};
use crate::solver::Engine;
use crate::solver::board::Board;
use crate::solver::hint::Hint;

#[derive(Clone, Debug)]
pub(crate) struct BruteForceSolver {
    solutions: Vec<Solution>,
}

impl BruteForceSolver {
    #[cfg(test)]
    pub(crate) fn verify_only_solution(&self, solution: Solution) {
        assert_eq!(self.solutions, [solution]);
    }
}

impl Default for BruteForceSolver {
    fn default() -> Self {
        let solutions = Solution::all(iter::empty());
        Self { solutions }
    }
}

impl Engine for BruteForceSolver {
    fn for_board(board: &Board) -> Self {
        let fixed = board
            .fixed()
            .into_iter()
            .filter_map(|(coord, judgment)| Some((coord, judgment?)));
        let solutions = Solution::all(fixed);

        Self { solutions }
    }

    fn add_parsed_hint(&mut self, hint: &Hint) {
        self.solutions.retain(|solution| hint.evaluate(solution));
    }

    fn updates(&mut self) -> Result<Vec<(Coord, Judgment)>> {
        let Some((first, rest)) = self.solutions.split_first() else {
            bail!("no solutions!")
        };

        let mut fixed = first.as_grid().clone().map(Some);
        for solution in rest {
            for coord in Coord::all() {
                let fixed = &mut fixed[coord];
                if let Some(val) = *fixed
                    && val != solution.as_grid()[coord]
                {
                    *fixed = None;
                }
            }
        }

        Ok(fixed
            .into_iter()
            .filter_map(|(coord, judgment)| Some((coord, judgment?)))
            .collect())
    }
}
