mod grid;
mod hint;

use std::fmt;
use std::iter::repeat;

use anyhow::{Result, bail};
use inquire::Editor;
use itertools::Itertools as _;

use crate::solver::grid::{Coordinate, Grid};
use crate::solver::hint::Set;
use crate::solver::hint::recipes::{HintRecipe, Recipe as _};

use hint::Hint;

type Name = String;
type Profession = String;

#[derive(Clone, Debug)]
pub(crate) struct Puzzle {
    grid: Grid,
    hints: Vec<Hint>,
    solutions: Vec<Solution>,
}

type Solution = [Judgment; 20];

impl Puzzle {
    pub(crate) fn prompt() -> Result<Self> {
        // TODO allow manual entry
        loop {
            let html = Editor::new("Enter html:").prompt()?;
            match Self::parse(&html) {
                Ok(puzzle) => return Ok(puzzle),
                Err(e) => eprintln!("{e}"),
            }
        }
    }

    pub(crate) fn parse(html: &str) -> Result<Self> {
        let grid = Grid::parse(html)?;
        Self::new(grid)
    }

    fn new(grid: Grid) -> Result<Self> {
        let hints: Vec<(Name, HintRecipe)> = grid
            .iter()
            .filter_map(|card| Some((card.name().clone(), card.hint()?)))
            .map(|(name, hint)| HintRecipe::parse(hint).map(|hints| repeat(name).zip(hints)))
            .flatten_ok()
            .try_collect()?;

        let old = grid.fixed();
        let fixed_values = old
            .iter()
            .enumerate()
            .filter_map(|(index, &judgment)| Some((index, judgment?)));
        let solutions = SolutionIterator::new(fixed_values).collect();

        let mut puzzle = Self {
            grid,
            hints: Vec::new(),
            solutions,
        };

        hints
            .into_iter()
            .try_for_each(|(speaker, hint)| puzzle.add_parsed_hint(hint, &speaker))?;

        Ok(puzzle)
    }

    pub(crate) fn solved(&self) -> bool {
        self.grid.solved()
    }

    pub(crate) fn infer(&mut self) -> Result<Vec<(Name, Judgment)>> {
        let Some((first, rest)) = self.solutions.split_first() else {
            bail!("no solutions!")
        };

        let mut fixed = first.map(Some);
        for solution in rest {
            for i in 0..20 {
                let fixed = &mut fixed[i];
                if let Some(val) = *fixed
                    && val != solution[i]
                {
                    *fixed = None;
                }
            }
        }
        Ok(fixed
            .into_iter()
            .enumerate()
            .filter_map(|(index, fixed)| {
                let fixed = fixed?;
                let name = self.grid.set_new(index, fixed)?.name().to_owned();
                Some((name, fixed))
            })
            .sorted_by(|(a, _), (b, _)| a.cmp(b))
            .collect())
    }

    pub(crate) fn add_hint(&mut self, hint: &str, speaker: &Name) -> Result<()> {
        HintRecipe::parse(hint)?
            .into_iter()
            .try_for_each(|hint| self.add_parsed_hint(hint, speaker))
    }

    fn add_parsed_hint(&mut self, hint: HintRecipe, speaker: &Name) -> Result<()> {
        let hint = hint.contextualize(&self.grid, speaker)?;
        self.solutions.retain(|solution| hint.evaluate(solution));
        self.hints.push(hint);
        Ok(())
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Judgment {
    Innocent,
    Criminal,
}

impl Judgment {
    fn filter(self, set: &Set, solution: &Solution) -> impl Iterator<Item = Coordinate> {
        set.iter()
            .filter(move |coord| solution[coord.to_index()] == self)
            .copied()
    }
}

impl fmt::Display for Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Innocent => write!(f, "Innocent"),
            Self::Criminal => write!(f, "Criminal"),
        }
    }
}

struct SolutionIterator {
    counter: u32,
    template: [Judgment; 20],
    free_indices: Vec<usize>,
}

impl SolutionIterator {
    fn new(fixed_values: impl IntoIterator<Item = (usize, Judgment)>) -> Self {
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

    const fn max_counter(&self) -> u32 {
        1_u32 << self.free_indices.len()
    }
}

impl Iterator for SolutionIterator {
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
        Some(current)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.max_counter() - self.counter)
            .try_into()
            .map_or((usize::MAX, None), |remaining| (remaining, Some(remaining)))
    }
}

#[cfg(test)]
mod tests {
    use crate::read_from_file;
    use crate::solver::Judgment;

    #[test]
    fn sample_2026_02_08() {
        use Judgment::{Criminal, Innocent};
        let mut puzzle = read_from_file("samples/2026-02-08.html").unwrap();
        let solution = [
            Innocent, Criminal, Criminal, Criminal, Criminal, Criminal, Innocent, Criminal,
            Innocent, Criminal, Criminal, Criminal, Criminal, Innocent, Criminal, Criminal,
            Criminal, Innocent, Criminal, Innocent,
        ];

        assert_eq!(
            puzzle.infer().unwrap(),
            vec![
                ("Betsy".to_owned(), Criminal),
                ("Emma".to_owned(), Criminal)
            ]
        );
        puzzle
            .add_hint(
                "Only 1 of the 3 innocents neighboring Kyle is my neighbor",
                &"Betsy".to_owned(),
            )
            .unwrap();
        puzzle
            .add_hint(
                "Only 1 of the 2 innocents neighboring Betsy is Donna's neighbor",
                &"Emma".to_owned(),
            )
            .unwrap();
        assert!(puzzle.solutions.contains(&solution));

        assert_eq!(
            puzzle.infer().unwrap(),
            vec![("Floyd".to_owned(), Criminal),]
        );
        puzzle
            .add_hint(
                "Row&nbsp;5 is the only row with exactly 2 criminals",
                &"Floyd".to_owned(),
            )
            .unwrap();
        assert!(puzzle.solutions.contains(&solution));

        assert_eq!(
            puzzle.infer().unwrap(),
            vec![("Isaac".to_owned(), Criminal),]
        );
        puzzle
            .add_hint(
                "Only 1 of the 3 innocents neighboring Gabe is Donna's neighbor",
                &"Isaac".to_owned(),
            )
            .unwrap();
        assert!(puzzle.solutions.contains(&solution));

        assert_eq!(
            puzzle.infer().unwrap(),
            vec![
                ("Gabe".to_owned(), Criminal),
                ("Hank".to_owned(), Innocent),
                ("Nick".to_owned(), Criminal),
            ]
        );
        puzzle
            .add_hint(
                "Kyle and Wally have only one innocent neighbor in common",
                &"Gabe".to_owned(),
            )
            .unwrap();
        puzzle
            .add_hint(
                "Only one person in a corner has exactly 2 innocent neighbors",
                &"Hank".to_owned(),
            )
            .unwrap();
        puzzle
            .add_hint(
                "Exactly 2 of the 3 innocents neighboring Ruth are in row&nbsp;5",
                &"Nick".to_owned(),
            )
            .unwrap();
        assert!(puzzle.solutions.contains(&solution));

        assert_eq!(
            puzzle.infer().unwrap(),
            vec![
                ("Kyle".to_owned(), Criminal),
                ("Oscar".to_owned(), Criminal),
                ("Sarah".to_owned(), Criminal),
                ("Uma".to_owned(), Criminal),
            ]
        );
        puzzle
            .add_hint(
                "There's an odd number of innocents neighboring Vera",
                &"Oscar".to_owned(),
            )
            .unwrap();
        assert!(puzzle.solutions.contains(&solution));

        assert_eq!(
            puzzle.infer().unwrap(),
            vec![
                ("Vera".to_owned(), Innocent),
                ("Wally".to_owned(), Criminal),
            ]
        );
        puzzle
            .add_hint("Paul has exactly 2 innocent neighbors", &"Vera".to_owned())
            .unwrap();
        assert!(puzzle.solutions.contains(&solution));

        assert_eq!(
            puzzle.infer().unwrap(),
            vec![
                ("Alice".to_owned(), Innocent),
                ("Donna".to_owned(), Criminal),
                ("Jane".to_owned(), Innocent),
                ("Mary".to_owned(), Criminal),
                ("Paul".to_owned(), Innocent),
                ("Ruth".to_owned(), Criminal),
            ]
        );
        assert_eq!(puzzle.solutions, [solution]);
    }
}
