mod grid;
mod hint;
mod solution;

use std::fmt;
use std::iter::repeat;

use anyhow::{Result, bail};
use inquire::Editor;
use itertools::Itertools as _;

use grid::Grid;
use hint::Hint;
use hint::recipes::{HintRecipe, Recipe as _};
use solution::Solution;

pub(crate) type Name = String;
type Profession = String;

#[derive(Clone, Debug)]
pub(crate) struct Puzzle {
    grid: Grid,
    hints: Vec<Hint>,
    solutions: Vec<Solution>,
}

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
        let solutions = Solution::all(fixed_values);

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

    pub(crate) fn infer(&mut self) -> Result<Vec<Update>> {
        let Some((first, rest)) = self.solutions.split_first() else {
            bail!("no solutions!")
        };

        let mut fixed = first.as_array().map(Some);
        for solution in rest {
            for i in 0..20 {
                let fixed = &mut fixed[i];
                if let Some(val) = *fixed
                    && val != solution.as_array()[i]
                {
                    *fixed = None;
                }
            }
        }
        Ok(fixed
            .into_iter()
            .enumerate()
            .filter_map(|(index, judgment)| {
                let judgment = judgment?;
                let name = self.grid.set_new(index, judgment)?.name().to_owned();
                Some(Update { name, judgment })
            })
            .sorted_by(|a, b| a.name.cmp(&b.name))
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

impl fmt::Display for Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Innocent => write!(f, "Innocent"),
            Self::Criminal => write!(f, "Criminal"),
        }
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) struct Update {
    name: Name,
    judgment: Judgment,
}

impl Update {
    pub(crate) fn into_name(self) -> Name {
        self.name
    }
}

impl fmt::Display for Update {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} as {}", self.name, self.judgment)
    }
}

#[cfg(test)]
mod tests {
    use crate::read_from_file;

    use super::solution::Solution;
    use super::{Judgment, Update};

    #[test]
    fn sample_2026_02_08() {
        use Judgment::{Criminal as C, Innocent as I};
        let mut puzzle = read_from_file("samples/2026-02-08.html").unwrap();
        let solution = Solution::from([I, C, C, C, C, C, I, C, I, C, C, C, C, I, C, C, C, I, C, I]);

        let steps: &[&[(&str, Judgment, Option<&str>)]] = &[
            &[
                (
                    "Betsy",
                    C,
                    Some("Only 1 of the 3 innocents neighboring Kyle is my neighbor"),
                ),
                (
                    "Emma",
                    C,
                    Some("Only 1 of the 2 innocents neighboring Betsy is Donna's neighbor"),
                ),
            ],
            &[(
                "Floyd",
                C,
                Some("Row&nbsp;5 is the only row with exactly 2 criminals"),
            )],
            &[(
                "Isaac",
                C,
                Some("Only 1 of the 3 innocents neighboring Gabe is Donna's neighbor"),
            )],
            &[
                (
                    "Gabe",
                    C,
                    Some("Kyle and Wally have only one innocent neighbor in common"),
                ),
                (
                    "Hank",
                    I,
                    Some("Only one person in a corner has exactly 2 innocent neighbors"),
                ),
                (
                    "Nick",
                    C,
                    Some("Exactly 2 of the 3 innocents neighboring Ruth are in row&nbsp;5"),
                ),
            ],
            &[
                ("Kyle", C, None),
                (
                    "Oscar",
                    C,
                    Some("There's an odd number of innocents neighboring Vera"),
                ),
                ("Sarah", C, None),
                ("Uma", C, None),
            ],
            &[
                ("Vera", I, Some("Paul has exactly 2 innocent neighbors")),
                ("Wally", C, None),
            ],
            &[
                ("Alice", I, None),
                ("Donna", C, None),
                ("Jane", I, None),
                ("Mary", C, None),
                ("Paul", I, None),
                ("Ruth", C, None),
            ],
        ];

        for &changes in steps {
            let deductions: Vec<Update> = changes
                .iter()
                .map(|&(name, judgment, _)| Update {
                    name: name.to_owned(),
                    judgment,
                })
                .collect();
            assert_eq!(puzzle.infer().unwrap(), deductions);
            for &(speaker, _, hint) in changes {
                if let Some(hint) = hint {
                    puzzle.add_hint(hint, &speaker.to_owned()).unwrap();
                }
            }
        }

        assert_eq!(puzzle.solutions, [solution]);
    }
}
