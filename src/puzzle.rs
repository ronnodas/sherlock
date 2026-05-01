pub(crate) mod grid;
mod hint;
mod solution;

use std::fmt;
use std::ops::Not;

use anyhow::{Result, bail};
use colored::{Color, Colorize as _};
use itertools::Itertools as _;

use grid::Grid;
use hint::Hint;
use hint::recipes::AddContext as _;
use ron::extensions::Extensions;
use ron::ser::{PrettyConfig, to_string_pretty};
use serde::{Deserialize, Serialize};
use solution::Solution;

use crate::puzzle::grid::Format;
use crate::puzzle::grid::coordinate::Coordinate;
use crate::puzzle::hint::Sentence;
use crate::puzzle::hint::recipes::Context;

pub(crate) type Name = String;
pub(crate) type Profession = String;

#[derive(Clone, Debug)]
pub(crate) struct Puzzle {
    name: Option<String>,
    grid: Grid,

    solutions: Vec<Solution>,
}

impl Puzzle {
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
                Some(Update {
                    name: self.grid.set_new(index, judgment)?.name().to_owned(),
                    coord: Coordinate::from_index(index),
                    judgment,
                })
            })
            .sorted_by(|a, b| a.name.cmp(&b.name))
            .collect())
    }

    pub(crate) fn add_hint(&mut self, hint: String, coord: Coordinate) -> Result<()> {
        Sentence::parse(&hint)?
            .add_context(Context::new(&self.grid, self.grid[coord].name()))?
            .into_iter()
            .for_each(|hint| self.add_parsed_hint(&hint));
        self.grid.add_hint(hint, coord)
    }

    fn add_parsed_hint(&mut self, hint: &Hint) {
        self.solutions.retain(|solution| hint.evaluate(solution));
    }

    pub(crate) fn name(&self) -> Option<&str> {
        self.name.as_deref()
    }

    pub(crate) fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }

    pub(crate) fn save_grid(&self) -> Result<String> {
        let config = PrettyConfig::new().extensions(
            Extensions::IMPLICIT_SOME
                | Extensions::UNWRAP_NEWTYPES
                | Extensions::UNWRAP_VARIANT_NEWTYPES,
        );
        to_string_pretty(&self.grid, config).map_err(Into::into)
    }

    pub(crate) fn mark_as_flavor(&mut self, coord: Coordinate) -> Result<()> {
        self.grid.mark_as_flavor(coord)
    }

    pub(crate) fn emoji_summary(&self) -> String {
        self.grid.emoji_summary()
    }
}

// TODO Could separate this into a LoadedPuzzle but probably needs to be unified before use anyway
#[derive(Debug)]
pub(crate) struct ParsedPuzzle {
    pub puzzle: Puzzle,
    pub unknown_if_flavor: Vec<(Name, Coordinate, String)>,
    pub pending_hints: Vec<Suspect>,
}

impl ParsedPuzzle {
    pub(crate) fn parse(html: &str, name: Option<String>) -> Result<Self> {
        let grid = Grid::parse(html)?;
        Self::new(grid, name)
    }

    pub(crate) fn new(grid: Grid, name: Option<String>) -> Result<Self> {
        let pending_hints = grid.pending_hints();

        let maybe_parsed = grid
            .iter()
            .enumerate()
            .filter_map(|(index, card)| {
                let coord = Coordinate::from_index(index);
                Some((coord, card.name().clone(), card.logical_hint()?))
            })
            .map(|(coord, speaker, hint)| {
                let hint = Sentence::parse(hint)
                    .and_then(|sentence| sentence.add_context(Context::new(&grid, &speaker)))
                    .map_err(|e| (e, hint));
                (coord, speaker, hint)
            });
        let (hints, unknown_if_flavor) = match grid.format() {
            Format::Original => {
                let mut hints = Vec::new();
                let mut unknown = Vec::new();

                for (coord, name, maybe_parsed) in maybe_parsed {
                    match maybe_parsed {
                        Ok(parsed) => hints.extend(parsed),
                        Err((_, hint)) => unknown.push((name, coord, hint.to_owned())),
                    }
                }
                (hints, unknown)
            }
            Format::Sep2025 => {
                let hints: Vec<Hint> = maybe_parsed
                    .map(|(_, _, hint)| hint.map_err(|(e, _)| e))
                    .flatten_ok()
                    .try_collect()?;
                (hints, Vec::new())
            }
        };

        let old = grid.fixed();
        let fixed_values = old
            .iter()
            .enumerate()
            .filter_map(|(index, &judgment)| Some((index, judgment?)));
        let solutions = Solution::all(fixed_values);

        let mut puzzle = Puzzle {
            name,
            grid,

            solutions,
        };

        for hint in hints {
            puzzle.add_parsed_hint(&hint);
        }

        Ok(Self {
            puzzle,
            unknown_if_flavor,
            pending_hints,
        })
    }

    pub(crate) fn load(contents: &str, name: Option<String>) -> Result<Self> {
        let grid = ron::from_str(contents)?;
        Self::new(grid, name)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub(crate) enum Judgment {
    Innocent,
    Criminal,
}

impl Judgment {
    pub(crate) fn color(self) -> Color {
        match self {
            Self::Innocent => Color::Green,
            Self::Criminal => Color::Red,
        }
    }
}

impl Not for Judgment {
    type Output = Self;

    fn not(self) -> Self {
        match self {
            Self::Innocent => Self::Criminal,
            Self::Criminal => Self::Innocent,
        }
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

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone)]
pub(crate) struct Update {
    name: Name,
    coord: Coordinate,
    judgment: Judgment,
}

impl fmt::Display for Update {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let color = self.judgment.color();
        write!(
            f,
            "{} ({}) as {}",
            self.name.color(color),
            self.coord,
            self.judgment.to_string().color(color)
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Suspect {
    name: Name,
    coord: Coordinate,
    judgment: Judgment,
}

impl Suspect {
    pub(crate) fn coord(&self) -> Coordinate {
        self.coord
    }

    pub(crate) fn name(&self) -> &Name {
        &self.name
    }
}

impl fmt::Display for Suspect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let color = self.judgment.color();
        write!(f, "{} ({})", self.name.color(color), self.coord)
    }
}

impl From<Update> for Suspect {
    fn from(update: Update) -> Self {
        Self {
            name: update.name,
            coord: update.coord,
            judgment: update.judgment,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, io};

    use anyhow::Context as _;
    use itertools::Itertools as _;

    use crate::puzzle::solution::Solution;

    use super::{Judgment, ParsedPuzzle};

    #[test]
    fn sample_2026_02_08() {
        use Judgment::{Criminal as C, Innocent as I};
        let contents = match fs::read_to_string("samples/2026-02-08-6f3e400c1d18.html") {
            Ok(contents) => contents,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return,
            Err(e) => panic!("Failed to read sample: {e}"),
        };
        let parsed = ParsedPuzzle::parse(&contents, None).unwrap();
        assert!(parsed.pending_hints.is_empty());
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

        let mut puzzle = parsed.puzzle;
        for &changes in steps {
            let deductions = changes
                .iter()
                .map(|&(name, judgment, _)| (name.to_owned(), judgment))
                .collect_vec();
            let inferences = puzzle
                .infer()
                .unwrap()
                .into_iter()
                .map(|update| (update.name, update.judgment))
                .collect_vec();
            assert_eq!(inferences, deductions);
            for &(speaker, _, hint) in changes {
                if let Some(hint) = hint {
                    let coord = puzzle.grid.coord(speaker).unwrap();
                    puzzle.add_hint(hint.to_owned(), coord).unwrap();
                }
            }
        }

        assert_eq!(puzzle.solutions, [solution]);
    }

    #[test]
    fn parse_all_samples() {
        let read_dir = match fs::read_dir("samples") {
            Ok(read_dir) => read_dir,
            Err(e)
                if e.kind() == io::ErrorKind::NotFound
                    || e.kind() == io::ErrorKind::NotADirectory =>
            {
                return;
            }
            Err(e) => panic!("error reading `samples` directory: {e}"),
        };
        for entry in read_dir {
            let entry = entry.unwrap();
            #[expect(
                clippy::filetype_is_file,
                reason = "actual tests should be plain files"
            )]
            if !entry.file_type().unwrap().is_file() {
                continue;
            }
            let path = entry.path();
            let contents = fs::read_to_string(&path).unwrap();
            drop(
                ParsedPuzzle::parse(&contents, None)
                    .with_context(|| format!("parsing {}", path.to_string_lossy()))
                    .unwrap(),
            );
        }
    }
}
