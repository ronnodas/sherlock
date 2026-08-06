use std::fs::File;
use std::io::{self, Write as _};
use std::path::{Path, PathBuf};
use std::{fmt, fs, mem};

use anyhow::{Context as _, Result, bail};
use colored::Colorize as _;
use inquire::list_option::ListOption;
use inquire::{Confirm, MultiSelect, Select, Text};
use itertools::Itertools as _;
use mitsein::str1::Str1;
use mitsein::string1::String1;
use ron::extensions::Extensions;
use ron::ser::{PrettyConfig, to_string_pretty};

use crate::SAVE_DIRECTORY;
use crate::models::{Coordinate, FullCard, Judgment, Name, Puzzle};
use crate::solver::board::{Board, Format, SolvedBoard};
use crate::solver::hint::recipes::{AddContext as _, Context};
use crate::solver::hint::{Hint, Sentence};
use crate::solver::solution::Solution;

pub(crate) mod board;
mod hint;
mod solution;

const ARCHIVE_DIR: &str = "archive";

#[derive(Clone, Debug)]
struct Solver {
    title: Option<String1>,
    board: Board,

    solutions: Vec<Solution>,
}

impl Solver {
    fn solve(mut self, mut pending_hints: Vec<Suspect>) -> Result<()> {
        loop {
            let new = self.infer()?;
            print_inferences(&new);

            println!("{}", self.board.emoji_summary());
            // TODO parse, don't validate
            if self.board.solved() {
                let solved = self.into_solved().expect("solved");
                println!("Puzzle solved!");
                return solved.save_puzzle();
            }
            pending_hints.extend(new.into_iter().map_into());
            pending_hints.sort_unstable_by_key(Suspect::coord);

            loop {
                let selected = Select::new(
                    "Add a logical hint:",
                    pending_hints
                        .iter()
                        .map(HintOption::Suspect)
                        .chain(HintOption::FIXED)
                        .collect(),
                )
                .prompt()?;
                match selected {
                    HintOption::Suspect(suspect) => {
                        if let Some(hint) = Text::new(&format!("Enter {}'s hint:", suspect.name()))
                            .prompt_skippable()?
                        {
                            match self.add_hint(hint, suspect.coord()) {
                                Ok(()) => {
                                    let coord = suspect.coord();
                                    pending_hints.retain(|pending| pending.coord() != coord);
                                    break;
                                }
                                Err(e) => {
                                    println!("I didn't understand that hint :(\n{e}");
                                }
                            }
                        }
                    }
                    HintOption::MarkAsFlavor => self.handle_mark_flavor(&mut pending_hints)?,
                    HintOption::Save => self.save()?,
                }
            }
        }
    }

    fn into_solved(self) -> Option<Solved> {
        Some(Solved {
            board: self.board.into_solved()?,
            title: self.title,
        })
    }

    fn infer(&mut self) -> Result<Vec<Update>> {
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
                Some(Update::new(
                    Coordinate::from_index(index),
                    self.board.set_new(index, judgment)?.name().to_owned(),
                    judgment,
                ))
            })
            .sorted_by(|a, b| a.name.cmp(&b.name))
            .collect())
    }

    fn handle_unknown_hints(&mut self, unknown: Vec<(Name, Coordinate, String)>) -> Result<()> {
        for (suspect, coord, hint) in unknown {
            let flavor = Confirm::new(&format!(
                "Is {suspect}'s ({coord}) hint, \"{hint}\", just flavor text?"
            ))
            .prompt()?;
            if flavor {
                self.mark_as_flavor(coord)?;
            } else {
                self.add_hint(hint, coord)?;
            }
        }
        Ok(())
    }

    fn add_hint(&mut self, hint: String, coord: Coordinate) -> Result<()> {
        Sentence::parse(&hint)?
            .add_context(Context::new(&self.board, coord))?
            .into_iter()
            .for_each(|hint| self.add_parsed_hint(&hint));
        self.board.add_hint(hint, coord)
    }

    fn add_parsed_hint(&mut self, hint: &Hint) {
        self.solutions.retain(|solution| hint.evaluate(solution));
    }

    pub(crate) fn set_title(&mut self, title: String1) {
        self.title = Some(title);
    }

    fn save_board(&self) -> Result<String> {
        let config = ron_config();
        to_string_pretty(&self.board, config).map_err(Into::into)
    }

    fn handle_mark_flavor(&mut self, pending: &mut Vec<Suspect>) -> Result<()> {
        let flavor = MultiSelect::new("Select characters with flavor text", pending.clone())
            .prompt_skippable()?
            .unwrap_or_default();
        pending.retain(|p| !flavor.iter().any(|f| f.coord() == p.coord()));
        for f in flavor {
            self.mark_as_flavor(f.coord())?;
        }
        Ok(())
    }

    fn mark_as_flavor(&mut self, coord: Coordinate) -> Result<()> {
        self.board.mark_as_flavor(coord)
    }

    fn save(&mut self) -> Result<()> {
        let save = self.save_board()?;
        let path = self.title.as_ref().map_or_else(
            || SAVE_DIRECTORY.to_owned(),
            |title| {
                Path::new(SAVE_DIRECTORY)
                    .join(title.as_str())
                    .with_added_extension("ron")
                    .display()
                    .to_string()
            },
        );
        let path = Text::new("Save file:").with_initial_value(&path).prompt()?;
        let path = PathBuf::from(path);
        if let Some(file_stem) = path
            .file_stem()
            .and_then(|name| Str1::try_from_str(name.to_str()?).ok())
        {
            self.set_title(file_stem.to_owned());
        }
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let mut file = File::create(path)?;
        file.write_all(save.as_bytes())?;
        Ok(())
    }
}

struct Solved {
    board: SolvedBoard,
    title: Option<String1>,
}

impl Solved {
    fn save_puzzle(self) -> Result<()> {
        let name = self.title.as_ref().map_or("", |name| name.as_str());

        fs::create_dir_all(ARCHIVE_DIR)?;

        let (path, mut file) = loop {
            let name = Text::new("Save puzzle as (empty to cancel):")
                .with_initial_value(name)
                .with_placeholder("do not save")
                .prompt()?;
            let Ok(name) = String1::try_from(name) else {
                return Ok(());
            };

            let path = Path::new(ARCHIVE_DIR).join(format!("{name}.ron"));
            match File::create_new(&path) {
                Ok(file) => break (path, file),
                Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {}
                Err(e) => return Err(e).context("creating puzzle file"),
            }
        };

        let puzzle = self.extract_puzzle()?;

        let config = ron_config();
        let serialized = to_string_pretty(&puzzle, config)?;
        file.write_all(serialized.as_bytes())?;
        drop(file);
        println!("Saved puzzle to {}", path.display());
        Ok(())
    }

    fn extract_puzzle(mut self) -> Result<Puzzle> {
        let start = if let Some(coord) = self.board.start() {
            coord
        } else {
            let options = Coordinate::all()
                .into_iter()
                .map(|coord| {
                    let card = &self.board[coord];
                    Suspect::new(coord, card.name().to_owned(), card.judgment())
                })
                .collect_vec();
            Select::new("Which card is revealed at the start?", options)
                .prompt()?
                .coord
        };
        let mut unknown = Vec::with_capacity(20);
        for coord in Coordinate::all() {
            let card = &self.board[coord];
            if card.back().hint().is_unknown() {
                let judgment = card.judgment();
                unknown.push(Suspect::new(coord, card.name().clone(), judgment));
            }
        }
        let mut text = String::new();
        while !unknown.is_empty() {
            let message = format!(
                "Are all of the following suspects' hints flavor text (y/n): {}",
                unknown.iter().format(", ")
            );
            if Confirm::new(&message).prompt()? {
                for suspect in unknown {
                    self.board[suspect.coord].back_mut().mark_as_flavor();
                }
                break;
            }
            if let Some(ListOption {
                index,
                value: suspect,
            }) = Select::new(
                "Select suspect with logical hint:",
                unknown.iter().collect(),
            )
            .raw_prompt_skippable()?
            {
                text = Text::new(&format!("Enter {}'s (logical) hint:", suspect.name))
                    .with_initial_value(&text)
                    .prompt()?;

                if let Ok(sentence) = Sentence::parse(&text)
                    && sentence
                        .add_context(Context::new(&self.board, suspect.coord))
                        .is_ok()
                {
                    self.board[suspect.coord]
                        .back_mut()
                        .set_hint(mem::take(&mut text));
                    drop(unknown.remove(index));
                }
                println!("I didn't understand that hint :(\n{text}");
            }
        }
        let mut cards = Vec::with_capacity(20);
        for coord in Coordinate::all() {
            let card = &self.board[coord];
            // TODO should deconstruct here rather than clone
            let name = card.name().clone();
            let profession = card.profession().clone();
            let judgment = card.judgment();
            let hint = card.back().hint().known().expect("set above");
            cards.push(FullCard::new(name, profession, judgment, hint));
        }
        let cards: [FullCard; 20] = cards.try_into().unwrap_or_else(|_| unreachable!());
        let puzzle = Puzzle::new(cards, start);
        Ok(puzzle)
    }
}

fn ron_config() -> PrettyConfig {
    PrettyConfig::new().extensions(
        Extensions::IMPLICIT_SOME
            | Extensions::UNWRAP_NEWTYPES
            | Extensions::UNWRAP_VARIANT_NEWTYPES,
    )
}

#[derive(Debug)]
pub(crate) struct SolverWithUpdates {
    solver: Solver,
    unknown_if_flavor: Vec<(Name, Coordinate, String)>,
    pending_hints: Vec<Suspect>,
}

impl SolverWithUpdates {
    pub(crate) fn parse(html: &str, name: Option<String1>) -> Result<Self> {
        let board = Board::parse(html)?;
        Self::new(board, name)
    }

    pub(crate) fn new(board: Board, title: Option<String1>) -> Result<Self> {
        let pending_hints = board.pending_hints();

        let maybe_parsed = board
            .iter()
            .enumerate()
            .filter_map(|(index, card)| {
                let coord = Coordinate::from_index(index);
                Some((coord, card.name().clone(), card.logical_hint()?))
            })
            .map(|(coord, speaker, hint)| {
                let hint = Sentence::parse(hint)
                    .and_then(|sentence| sentence.add_context(Context::new(&board, coord)))
                    .map_err(|e| (e, hint));
                (coord, speaker, hint)
            });
        let (hints, unknown_if_flavor) = match board.format() {
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

        let old = board.fixed();
        let fixed_values = old
            .iter()
            .enumerate()
            .filter_map(|(index, &judgment)| Some((index, judgment?)));
        let solutions = Solution::all(fixed_values);

        let mut solver = Solver {
            title,
            board,

            solutions,
        };

        for hint in hints {
            solver.add_parsed_hint(&hint);
        }

        Ok(Self {
            solver,
            unknown_if_flavor,
            pending_hints,
        })
    }

    pub(crate) fn load(contents: &str, title: Option<String1>) -> Result<Self> {
        let board = ron::from_str(contents)?;
        Self::new(board, title)
    }

    pub(crate) fn solve(mut self) -> Result<()> {
        self.solver.handle_unknown_hints(self.unknown_if_flavor)?;
        self.solver.solve(self.pending_hints)
    }

    pub(crate) fn set_title(&mut self, title: String1) {
        self.solver.set_title(title);
    }
}

fn print_inferences(new: &[Update]) {
    if let Some((last, rest)) = new.split_last() {
        if rest.is_empty() {
            println!("Mark {last}");
        } else {
            println!("Mark {} and {last}", rest.iter().format(", "));
        }
    }
}

enum HintOption<'suspect> {
    Suspect(&'suspect Suspect),
    MarkAsFlavor,
    Save,
}

impl HintOption<'_> {
    const FIXED: [Self; 2] = [Self::MarkAsFlavor, Self::Save];
}

impl fmt::Display for HintOption<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Suspect(name) => write!(f, "{name}"),
            Self::MarkAsFlavor => write!(f, "mark hints as flavor"),
            Self::Save => write!(f, "save progress to file"),
        }
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone)]
struct Update {
    name: Name,
    coord: Coordinate,
    judgment: Judgment,
}

impl Update {
    fn new(coord: Coordinate, name: Name, judgment: Judgment) -> Self {
        Self {
            name,
            coord,
            judgment,
        }
    }
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
    coord: Coordinate,
    name: Name,
    judgment: Judgment,
}

impl Suspect {
    pub(crate) fn new(coord: Coordinate, name: Name, judgment: Judgment) -> Self {
        Self {
            coord,
            name,
            judgment,
        }
    }

    fn coord(&self) -> Coordinate {
        self.coord
    }

    fn name(&self) -> &Name {
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
        Self::new(update.coord, update.name, update.judgment)
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, io};

    use anyhow::Context as _;
    use itertools::Itertools as _;

    use crate::solver::solution::Solution;

    use super::{Judgment, SolverWithUpdates};

    #[test]
    fn sample_2026_02_08() {
        use Judgment::{Criminal as C, Innocent as I};
        let contents = match fs::read_to_string("samples/2026-02-08-6f3e400c1d18.html") {
            Ok(contents) => contents,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return,
            Err(e) => panic!("Failed to read sample: {e}"),
        };
        let parsed = SolverWithUpdates::parse(&contents, None).unwrap();
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

        let mut solver = parsed.solver;
        for &changes in steps {
            let deductions = changes
                .iter()
                .map(|&(name, judgment, _)| (name.to_owned(), judgment))
                .collect_vec();
            let inferences = solver
                .infer()
                .unwrap()
                .into_iter()
                .map(|update| (update.name, update.judgment))
                .collect_vec();
            assert_eq!(inferences, deductions);
            for &(speaker, _, hint) in changes {
                if let Some(hint) = hint {
                    let coord = solver.board.coord(speaker).unwrap();
                    solver.add_hint(hint.to_owned(), coord).unwrap();
                }
            }
        }

        assert_eq!(solver.solutions, [solution]);
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
                SolverWithUpdates::parse(&contents, None)
                    .with_context(|| format!("parsing {}", path.to_string_lossy()))
                    .unwrap(),
            );
        }
    }
}
