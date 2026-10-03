use std::borrow::Cow;
use std::io::{self, Write as _};
use std::path::{Path, PathBuf};
use std::{fmt, fs, mem};

use anyhow::{Context as _, Result, bail};
use colored::Colorize as _;
use inquire::list_option::ListOption;
use inquire::{Confirm, CustomType, MultiSelect, Select, Text};
use itertools::Itertools as _;
use mitsein::string1::String1;
use ron::extensions::Extensions;
use ron::ser::{PrettyConfig, to_string_pretty};
use serde::{Deserialize, Serialize};
use strum::VariantArray as _;

use crate::grid::Grid;
use crate::models::{
    Card, CardFront, Coord, Difficulty, Judgment, Metadata, Name, PartialMetadata, Puzzle,
    PuzzleId, PuzzleIdDiscriminants,
};
use crate::solver::board::{Board, Format, HtmlBoard, SolvedBoard};
use crate::solver::brute_force::BruteForceSolver;
use crate::solver::hint::recipes::AddContext as _;
use crate::solver::hint::{Hint, Sentence};
use crate::{ARCHIVE_DIR, SAVE_DIR};

pub(crate) mod board;
mod brute_force;
pub(crate) mod hint;

pub(crate) struct Solver<E> {
    board: Board,
    metadata: PartialMetadata,
    save_name: Option<String>,
    engine: E,
}

impl<E: Engine> Solver<E> {
    fn new(board: Board, save_name: Option<String>, metadata: PartialMetadata) -> Self {
        let engine = E::for_board(&board);
        Self::with_engine(board, save_name, metadata, engine)
    }

    fn with_engine(
        board: Board,
        save_name: Option<String>,
        metadata: PartialMetadata,
        engine: E,
    ) -> Self {
        Self {
            board,
            metadata,
            save_name,
            engine,
        }
    }

    fn solve(mut self, mut pending_hints: Vec<Suspect>) -> Result<()> {
        loop {
            let new = self.updates()?;
            Update::print_all(&new);

            println!("{}", self.board.emoji_summary());
            // TODO parse, don't validate
            if self.board.solved() {
                let solved = self.into_solved().expect("solved");
                println!("Puzzle solved!");
                return solved.save_puzzle();
            }
            pending_hints.extend(new.into_iter().map_into());
            pending_hints.sort_unstable_by_key(Suspect::coord);

            let mut failed_hint: Option<(Coord, String)> = None;
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
                        let coord = suspect.coord();
                        let message = if let Some((coord_f, hint)) = failed_hint.as_ref()
                            && coord_f == &coord
                        {
                            hint.as_str()
                        } else {
                            ""
                        };
                        if let Some(hint) = Text::new(&format!("Enter {}'s hint:", suspect.name()))
                            .with_initial_value(message)
                            .prompt_skippable()?
                        {
                            match self.add_hint(hint, suspect.coord()) {
                                Ok(()) => {
                                    pending_hints.retain(|pending| pending.coord() != coord);
                                    break;
                                }
                                Err((hint, e)) => {
                                    failed_hint = Some((coord, hint));
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
            save_name: self.save_name,
            metadata: self.metadata,
        })
    }

    fn updates(&mut self) -> Result<Vec<Update>> {
        #[expect(
            clippy::filter_map_bool_then,
            reason = "FP, https://github.com/rust-lang/rust-clippy/issues/17629"
        )]
        Ok(self
            .engine
            .updates()?
            .into_iter()
            .filter_map(|(coord, judgment)| {
                self.board.try_judge(coord, judgment).then(|| {
                    let name = self.board.front(coord).name.clone();
                    Update::new(coord, name, judgment)
                })
            })
            .collect())
    }

    fn add_hint(&mut self, hint: String, speaker: Coord) -> Result<(), (String, anyhow::Error)> {
        match Sentence::parse(&hint)
            .and_then(|sentence| sentence.add_context(self.board.context(speaker)))
        {
            Ok(hints) => {
                for hint in hints {
                    self.engine.add_parsed_hint(&hint);
                }
                self.board.add_hint(hint, speaker)
            }
            Err(err) => Err((hint, err)),
        }
    }

    fn add_parsed_hint(&mut self, hint: &Hint) {
        self.engine.add_parsed_hint(hint);
    }

    pub(crate) fn set_save_name(&mut self, save_name: String) {
        self.save_name = Some(save_name);
    }

    fn save_string(&self) -> Result<String> {
        let save = SaveRef {
            board: &self.board,
            metadata: &self.metadata,
        };
        let config = ron_config();
        to_string_pretty(&save, config).map_err(Into::into)
    }

    fn handle_mark_flavor(&mut self, pending: &mut Vec<Suspect>) -> Result<()> {
        let flavor = MultiSelect::new("Select characters with flavor text", pending.clone())
            .prompt_skippable()?
            .unwrap_or_default();
        pending.retain(|p| !flavor.iter().any(|f| f.coord() == p.coord()));
        for f in flavor {
            self.board.mark_as_flavor(f.coord())?;
        }
        Ok(())
    }

    fn save(&mut self) -> Result<()> {
        let save = self.save_string()?;
        let save_name = self
            .save_name
            .as_deref()
            .map_or_else(|| self.metadata.id.save_name(), Cow::Borrowed);
        let path = Path::new(SAVE_DIR)
            .join(save_name.as_ref())
            .with_added_extension("ron")
            .display()
            .to_string();
        let path = Text::new("Save file:").with_initial_value(&path).prompt()?;
        let path = PathBuf::from(path);
        if let Some(file_stem) = path.file_stem().and_then(|name| name.to_str()) {
            self.set_save_name(file_stem.to_owned());
        }
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let mut file = fs::File::create(path)?;
        file.write_all(save.as_bytes())?;
        Ok(())
    }
}

pub(crate) trait Engine: Sized + Default {
    fn add_parsed_hint(&mut self, hint: &Hint);
    fn updates(&mut self) -> Result<Vec<(Coord, Judgment)>>;

    fn for_board(board: &Board) -> Self {
        let mut this = Self::default();
        for (coord, judgment) in board.fixed().into_iter() {
            if let Some(judgment) = judgment {
                this.add_parsed_hint(&Hint::Judgment(coord, judgment));
            }
        }
        this
    }
}

struct Solved {
    board: SolvedBoard,
    metadata: PartialMetadata,
    save_name: Option<String>,
}

impl Solved {
    fn save_puzzle(self) -> Result<()> {
        let name = self
            .save_name
            .as_deref()
            .map_or_else(|| self.metadata.id.save_name(), Cow::Borrowed);

        fs::create_dir_all(ARCHIVE_DIR)?;

        let (path, mut file) = loop {
            let name = Text::new("Save puzzle as (empty to cancel):")
                .with_initial_value(&name)
                .with_placeholder("do not save")
                .prompt()?;
            let Ok(name) = String1::try_from(name) else {
                return Ok(());
            };

            let path = Path::new(ARCHIVE_DIR)
                .join(name.as_str())
                .with_added_extension("ron");
            match fs::File::create_new(&path) {
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
        let mut unknown = Vec::with_capacity(20);
        for coord in Coord::all() {
            let back = self.board.back(coord);
            if back.hint().is_unknown() {
                let name = self.board.front(coord).name.clone();
                let judgment = back.judgment();
                unknown.push(Suspect::new(coord, name, judgment));
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
                    self.board.back_mut(suspect.coord).mark_as_flavor();
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
                        .add_context(self.board.context(suspect.coord))
                        .is_ok()
                {
                    self.board
                        .back_mut(suspect.coord)
                        .set_hint(mem::take(&mut text));
                    drop(unknown.remove(index));
                }
                println!("I didn't understand that hint :(\n{text}");
            }
        }

        let metadata = Self::complete_metadata(self.metadata)?;

        let start = if let Some(coord) = self.board.start() {
            coord
        } else {
            let options = Coord::all()
                .into_iter()
                .map(|coord| {
                    let name = self.board.front(coord).name.clone();
                    let judgment = self.board.back(coord).judgment();
                    Suspect::new(coord, name, judgment)
                })
                .collect_vec();
            Select::new("Which card is revealed at the start?", options)
                .prompt()?
                .coord
        };

        let cards = Grid::from_fn(|coord| {
            // TODO should deconstruct here rather than clone
            let CardFront { name, profession } = self.board.front(coord).clone();
            let back = self.board.back(coord);
            let judgment = back.judgment();
            let hint = back.hint().known().expect("set above");
            Card::new(name, profession, judgment, hint)
        });

        Puzzle::new(cards, start, metadata)
    }

    fn complete_metadata(metadata: PartialMetadata) -> Result<Metadata> {
        let difficulty = || {
            metadata.difficulty.map_or_else(
                || {
                    Select::new(
                        "what difficulty was this puzzle rated at?",
                        Difficulty::VARIANTS.to_vec(),
                    )
                    .prompt()
                },
                Ok,
            )
        };
        let metadata = match metadata.id {
            PuzzleId::Daily(date) => Metadata::Daily {
                date,
                difficulty: {
                    match metadata.difficulty {
                        Some(t) => Some(t),
                        None => Select::new(
                            "what difficulty was this puzzle rated at?",
                            Difficulty::VARIANTS.to_vec(),
                        )
                        .prompt_skippable()?,
                    }
                },
            },
            PuzzleId::Archive(id) => Metadata::Archive {
                id,
                difficulty: difficulty()?,
            },
            PuzzleId::PuzzlePack { pack, puzzle } => Metadata::PuzzlePack {
                pack,
                puzzle,
                difficulty: difficulty()?,
            },
            PuzzleId::Community { user, id } => {
                if let Some(difficulty) = metadata.difficulty {
                    bail!("unexpected difficulty {difficulty} for community puzzle")
                }
                Metadata::Community { user, id }
            }
            PuzzleId::Custom => {
                if let Some(difficulty) = metadata.difficulty {
                    bail!("unexpected difficulty {difficulty} for custom puzzle")
                }
                Metadata::Custom
            }
        };
        Ok(metadata)
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
pub(crate) struct ParsedBoard {
    pub board: Board,
    pub hints: Vec<Hint>,
    pub pending_hints: Vec<Suspect>,

    pub metadata: PartialMetadata,
    pub save_name: Option<String>,
}

impl ParsedBoard {
    pub(crate) fn new(
        board: Board,
        metadata: PartialMetadata,
        save_name: Option<String>,
    ) -> Result<Self> {
        let pending_hints = board.pending_hints();

        let hints = board.parse_all_hints()?;

        Ok(Self {
            board,
            hints,
            pending_hints,
            metadata,
            save_name,
        })
    }

    pub(crate) fn from_html(
        html: &str,
        save_name: Option<String>,
        default_id: PuzzleId,
    ) -> Result<Self> {
        let HtmlBoard {
            board,
            format,
            metadata,
        } = HtmlBoard::parse(html)?;
        let metadata = metadata.map_or(
            PartialMetadata {
                id: default_id,
                difficulty: None,
            },
            PartialMetadata::from,
        );
        Self::from_html_common(board, format, metadata, save_name)
    }

    pub(crate) fn from_html_interactive(html: &str, save_name: Option<String>) -> Result<Self> {
        let HtmlBoard {
            board,
            format,
            metadata,
        } = HtmlBoard::parse(html)?;
        let metadata = if let Some(metadata) = metadata {
            metadata.into()
        } else {
            let kind = Select::new("enter puzzle id", PuzzleIdDiscriminants::VARIANTS.to_vec())
                .prompt()?;
            let id = match kind {
                PuzzleIdDiscriminants::Daily => {
                    let date = CustomType::new("enter a date as YYYY-MM-DD").prompt()?;
                    PuzzleId::Daily(date)
                }
                PuzzleIdDiscriminants::Archive => {
                    let id = Text::new("enter puzzle id").prompt()?;
                    PuzzleId::Archive(id)
                }
                PuzzleIdDiscriminants::PuzzlePack => {
                    let pack = CustomType::new("enter pack number").prompt()?;
                    let puzzle = CustomType::new("enter puzzle number").prompt()?;
                    PuzzleId::PuzzlePack { pack, puzzle }
                }
                PuzzleIdDiscriminants::Community => {
                    let id = Text::new("enter puzzle id (<user>-<id>)").prompt()?;
                    let (user, id) = id.split_once('-').context("incorrect_format")?;
                    PuzzleId::Community {
                        user: user.to_owned(),
                        id: id.to_owned(),
                    }
                }
                PuzzleIdDiscriminants::Custom => PuzzleId::Custom,
            };
            PartialMetadata {
                id,
                difficulty: None,
            }
        };

        Self::from_html_common(board, format, metadata, save_name)
    }

    fn from_html_common(
        mut board: Board,
        format: Format,
        metadata: PartialMetadata,
        save_name: Option<String>,
    ) -> Result<Self> {
        let pending_hints = board.pending_hints();

        let hints = match format {
            Format::Original => board.parse_hints_and_confirm_flavor()?,
            Format::Sep2025 => board.parse_all_hints()?,
        };

        Ok(Self {
            board,
            hints,
            pending_hints,
            metadata,
            save_name,
        })
    }

    pub(crate) fn load(contents: &str, save_name: Option<String>) -> Result<Self> {
        let Save { board, metadata } = ron::from_str(contents)?;
        Self::new(board, metadata, save_name)
    }

    pub(crate) fn solve<E: Engine>(self) -> Result<()> {
        let (solver, pending) = self.into_solver::<E>();

        solver.solve(pending)
    }

    fn into_solver<E: Engine>(self) -> (Solver<E>, Vec<Suspect>) {
        let mut solver = Solver::new(self.board, self.save_name, self.metadata);

        for hint in self.hints {
            solver.add_parsed_hint(&hint);
        }

        (solver, self.pending_hints)
    }

    pub(crate) fn solve_brute_force(self) -> Result<()> {
        self.solve::<BruteForceSolver>()
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
    coord: Coord,
    judgment: Judgment,
}

impl Update {
    fn new(coord: Coord, name: Name, judgment: Judgment) -> Self {
        Self {
            name,
            coord,
            judgment,
        }
    }

    fn print_all(list: &[Self]) {
        if let Some((last, rest)) = list.split_last() {
            if rest.is_empty() {
                println!("Mark {last}");
            } else {
                println!("Mark {} and {last}", rest.iter().format(", "));
            }
        }
    }
}

impl fmt::Display for Update {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let color = self.judgment.color();
        write!(
            f,
            "{} ({}) as {}",
            self.name.as_str().color(color),
            self.coord,
            self.judgment.to_string().color(color)
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Suspect {
    coord: Coord,
    name: Name,
    judgment: Judgment,
}

impl Suspect {
    pub(crate) fn new(coord: Coord, name: Name, judgment: Judgment) -> Self {
        Self {
            coord,
            name,
            judgment,
        }
    }

    fn coord(&self) -> Coord {
        self.coord
    }

    fn name(&self) -> &Name {
        &self.name
    }
}

impl fmt::Display for Suspect {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let color = self.judgment.color();
        write!(f, "{} ({})", self.name.as_str().color(color), self.coord)
    }
}

impl From<Update> for Suspect {
    fn from(update: Update) -> Self {
        Self::new(update.coord, update.name, update.judgment)
    }
}

#[derive(Deserialize)]
struct Save {
    board: Board,
    metadata: PartialMetadata,
}

#[derive(Serialize)]
struct SaveRef<'solver> {
    board: &'solver Board,
    metadata: &'solver PartialMetadata,
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use test_each_file::test_each_file;

    use crate::models::Solution;
    use crate::solver::board::Lookup;

    use super::*;

    test_each_file! { in "./archive" as puzzle => brute_force}

    pub(crate) fn solve<E: Engine>(puzzle: &Puzzle) {
        let mut engine = E::default();

        engine.add_parsed_hint(&Hint::Judgment(
            puzzle.start,
            puzzle.cards[puzzle.start].judgment,
        ));
        let mut pending = vec![puzzle.start];
        let mut marked = Grid::from_fn(|coord| coord == puzzle.start);

        let lookup = Lookup::new(&puzzle.cards);
        while let Some(speaker) = pending.pop() {
            let Some(hint) = puzzle.cards[speaker].hint.as_logical() else {
                continue;
            };
            Sentence::parse(hint)
                .unwrap()
                .add_context(lookup.context(speaker))
                .unwrap()
                .into_iter()
                .for_each(|hint| engine.add_parsed_hint(&hint));

            for (coord, judgment) in engine.updates().unwrap() {
                assert_eq!(puzzle.cards[coord].judgment, judgment);
                if !marked[coord] {
                    marked[coord] = true;
                    pending.push(coord);
                }
            }
        }
        assert_eq!(marked.into_iter().filter(|&(_, marked)| marked).count(), 20);
    }

    fn brute_force(puzzle: &str) {
        let puzzle = ron::from_str(puzzle).unwrap();
        solve::<BruteForceSolver>(&puzzle);
    }

    #[test]
    fn sample_2026_02_08() {
        use Judgment::{Criminal as C, Innocent as I};
        let contents = match fs::read_to_string("samples/2026-02-08-6f3e400c1d18.html") {
            Ok(contents) => contents,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return,
            Err(e) => panic!("Failed to read sample: {e}"),
        };
        let parsed = ParsedBoard::from_html(
            &contents,
            None,
            PuzzleId::Archive("6f3e400c1d18".to_owned()),
        )
        .unwrap();
        assert!(parsed.pending_hints.is_empty());
        let solution = Solution::from(Grid::from([
            [I, C, C, C],
            [C, C, I, C],
            [I, C, C, C],
            [C, I, C, C],
            [C, I, C, I],
        ]));

        let steps: &[&[(&str, Judgment)]] = &[
            &[("Betsy", C), ("Emma", C)],
            &[("Floyd", C)],
            &[("Isaac", C)],
            &[("Gabe", C), ("Hank", I), ("Nick", C)],
            &[("Kyle", C), ("Oscar", C), ("Sarah", C), ("Uma", C)],
            &[("Vera", I), ("Wally", C)],
            &[
                ("Alice", I),
                ("Donna", C),
                ("Jane", I),
                ("Mary", C),
                ("Paul", I),
                ("Ruth", C),
            ],
        ];
        let hints = HashMap::from([
            (
                "Betsy",
                "Only 1 of the 3 innocents neighboring Kyle is my neighbor",
            ),
            (
                "Emma",
                "Only 1 of the 2 innocents neighboring Betsy is Donna's neighbor",
            ),
            (
                "Floyd",
                "Row&nbsp;5 is the only row with exactly 2 criminals",
            ),
            (
                "Isaac",
                "Only 1 of the 3 innocents neighboring Gabe is Donna's neighbor",
            ),
            (
                "Gabe",
                "Kyle and Wally have only one innocent neighbor in common",
            ),
            (
                "Hank",
                "Only one person in a corner has exactly 2 innocent neighbors",
            ),
            (
                "Nick",
                "Exactly 2 of the 3 innocents neighboring Ruth are in row&nbsp;5",
            ),
            (
                "Oscar",
                "There's an odd number of innocents neighboring Vera",
            ),
            ("Vera", "Paul has exactly 2 innocent neighbors"),
        ]);

        let (mut solver, _pending) = parsed.into_solver::<BruteForceSolver>();
        for &changes in steps {
            let deductions = changes
                .iter()
                .map(|&(name, judgment)| (Name::from(name), judgment))
                .collect_vec();
            let inferences = solver
                .updates()
                .unwrap()
                .into_iter()
                .map(|update| (update.name, update.judgment))
                .collect_vec();
            assert_eq!(inferences, deductions);
            for &(speaker, _) in changes {
                if let Some(&hint) = hints.get(speaker) {
                    let coord = solver.board.coord(&Name::from(speaker)).unwrap();
                    solver.add_hint(hint.to_owned(), coord).unwrap();
                }
            }
        }

        solver.engine.verify_only_solution(solution);
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
            let board = ParsedBoard::from_html(&contents, None, PuzzleId::Custom)
                .with_context(|| format!("parsing {}", path.to_string_lossy()))
                .unwrap();
            if matches!(board.metadata.id, PuzzleId::Custom) && board.metadata.difficulty.is_none()
            {
                eprintln!("no metadata parsed in {}", path.display());
            }
        }
    }
}
