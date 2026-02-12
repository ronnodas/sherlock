mod puzzle;

use std::ffi::OsStr;
use std::fs::File;
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::{fmt, fs};

use anyhow::{Result, anyhow};
use bpaf::{Bpaf, Parser as _};
use chrono::Utc;
use chrono_tz::America::New_York;
use inquire::{Confirm, MultiSelect, Select, Text};
use itertools::Itertools as _;

use puzzle::{Name, ParsedPuzzle, Update};

const API_KEY_FILE: &str = "browserless_api_key";
const SAVE_DIRECTORY: &str = "saved/";

fn main() -> Result<()> {
    let args = args().run();

    let parsed = match args {
        Args::Menu => main_menu(),
        Args::Html { path } => read_from_file(path, Some(FileType::Html)),
        Args::Load { path } => read_from_file(path, Some(FileType::Hjson)),
        Args::Today => fetch_today(),
        Args::Archive { id } => archive(id),
    }?;

    play(parsed)?;
    Ok(())
}

fn main_menu() -> Result<ParsedPuzzle> {
    let mode = Select::new(
        "Which puzzle do you want to solve?",
        InputMode::ALL.to_vec(),
    )
    .prompt()?;
    match mode {
        InputMode::Today => fetch_today(),
        InputMode::Fetch => {
            let archive_id = Text::new("Enter puzzle archive id")
                .with_placeholder("a0b1c2d3e4f5")
                .prompt()?;
            archive(archive_id)
        }
        // TODO split into two instead of being clever with extensions
        InputMode::File => {
            let path = Text::new("Enter path to html or hjson:")
                .with_initial_value(SAVE_DIRECTORY)
                .prompt()?;
            read_from_file(path, None)
        }
        InputMode::Paste => ParsedPuzzle::prompt(),
    }
}

fn archive(archive_id: String) -> Result<ParsedPuzzle> {
    let target_url = format!("https://cluesbysam.com/s/archive/{archive_id}/");
    fetch_from_url(&target_url, Some(archive_id))
}

fn fetch_today() -> Result<ParsedPuzzle> {
    fetch_from_url("https://cluesbysam.com/", Some(date_string()))
}

fn date_string() -> String {
    Utc::now()
        .with_timezone(&New_York)
        .date_naive()
        .format("%F")
        .to_string()
}

fn fetch_from_url(target_url: &str, name: Option<Name>) -> Result<ParsedPuzzle> {
    let api_key = read_api_key()?;
    let json = format!(r#"{{"url": "{target_url}"}}"#);
    let html = ureq::post(format!(
        "https://production-sfo.browserless.io/content?token={api_key}"
    ))
    .content_type("application/json")
    .send(&json)?
    .body_mut()
    .read_to_string()?;
    ParsedPuzzle::parse(&html, name)
}

fn read_api_key() -> Result<String> {
    let api_key = if let Ok(api_key) = fs::read_to_string(API_KEY_FILE) {
        api_key.trim().to_owned()
    } else {
        let key = Text::new("Enter an API token from [browserless.io]:")
            .prompt()?
            .trim()
            .to_owned();
        let save = Confirm::new("Save key to disk (current directory)?")
            .with_default(true)
            .prompt()?;
        if save {
            fs::write(API_KEY_FILE, &key)?;
        }
        key
    };
    Ok(api_key)
}

fn read_from_file(path: impl AsRef<Path>, file_type: Option<FileType>) -> Result<ParsedPuzzle> {
    let path = path.as_ref();
    let contents = fs::read_to_string(path)?;
    let name = path
        .file_stem()
        .and_then(|name| name.to_str())
        .map(str::to_owned);
    let file_type = file_type.or_else(|| FileType::from_extension(path.extension()?));
    let parsed = match file_type {
        Some(FileType::Hjson) => ParsedPuzzle::load(&contents, name)?,
        Some(FileType::Html) => ParsedPuzzle::parse(&contents, name)?,
        None => {
            let mut parsed = ParsedPuzzle::load(&contents, None).or_else(|e_save| {
                ParsedPuzzle::parse(&contents, None).map_err(|e_html| {
                    anyhow!(
                        "Could not parse as either as a saved game ({e_save}) or html ({e_html})"
                    )
                })
            })?;
            if let Some(name) = name {
                parsed.puzzle.set_name(name);
            }
            parsed
        }
    };
    Ok(parsed)
}

fn play(puzzle: ParsedPuzzle) -> Result<()> {
    let ParsedPuzzle {
        mut puzzle,
        unknown_if_flavor,
        mut pending_hints,
    } = puzzle;
    for (name, hint) in unknown_if_flavor {
        let flavor =
            Confirm::new(&format!("Is {name}'s hint, \"{hint}\", just flavor text?")).prompt()?;
        if flavor {
            puzzle.mark_as_flavor(&name)?;
        } else {
            puzzle.add_hint(&hint, &name)?;
        }
    }

    loop {
        let new = puzzle.infer()?;
        if let Some((last, rest)) = new.split_last() {
            if rest.is_empty() {
                println!("Mark {last}");
            } else {
                println!("Mark {} and {last}", rest.iter().format(", "));
            }
        }
        if puzzle.solved() {
            println!("Puzzle solved!");
            break;
        }
        pending_hints.extend(new.into_iter().map(Update::into_name));
        pending_hints.sort_unstable();

        loop {
            let selected = Select::new(
                "Add a logical hint:",
                pending_hints
                    .iter()
                    .map(HintOption::Name)
                    .chain(HintOption::FIXED)
                    .collect(),
            )
            .prompt()?;
            match selected {
                HintOption::Name(name) => {
                    if let Some(hint) =
                        Text::new(&format!("Enter {name}'s hint:")).prompt_skippable()?
                    {
                        match puzzle.add_hint(&hint, name) {
                            Ok(()) => {
                                let name = name.clone();
                                pending_hints.retain(|pending| pending != &name);
                                break;
                            }
                            Err(e) => {
                                println!("I didn't understand that hint :(\n\n{e}");
                            }
                        }
                    }
                }
                HintOption::MarkAsFlavor => {
                    let flavor = MultiSelect::new(
                        "Select characters with flavor text",
                        pending_hints.clone(),
                    )
                    .prompt_skippable()?
                    .unwrap_or_default();
                    pending_hints.retain(|pending| !flavor.contains(pending));
                    for name in flavor {
                        puzzle.mark_as_flavor(&name)?;
                    }
                }
                HintOption::Save => {
                    let save = puzzle.save_grid()?;
                    let name = puzzle.name();
                    let prompt = Text::new("Save file:");
                    let path;
                    let prompt = if let Some(name) = name {
                        path = Path::new(SAVE_DIRECTORY)
                            .join(name)
                            .with_extension("hjson")
                            .display()
                            .to_string();
                        prompt.with_initial_value(&path)
                    } else {
                        prompt
                    };
                    let path = PathBuf::from(prompt.prompt()?);
                    if let Some(file_stem) = path.file_stem().and_then(OsStr::to_str) {
                        puzzle.set_name(file_stem.to_owned());
                    }
                    if let Some(parent) = path.parent() {
                        fs::create_dir_all(parent)?;
                    }
                    let mut file = File::create(path)?;
                    file.write_all(save.as_bytes())?;
                }
            }
        }
    }
    Ok(())
}

#[derive(Debug, Clone, Bpaf)]
#[bpaf(options)]
#[bpaf(fallback(Args::Menu))]
enum Args {
    /// Show the main menu (default)
    #[bpaf(command)]
    Menu,

    /// Load a save from the specified path
    #[bpaf(command("load"), short('l'))]
    Load {
        #[bpaf(positional("PATH"))]
        path: PathBuf,
    },

    /// Parse an HTML file as a puzzle
    #[bpaf(command("html"), short('h'))]
    Html {
        #[bpaf(positional("PATH"))]
        path: PathBuf,
    },

    /// Load today's puzzle
    #[bpaf(command("today"), short('t'))]
    Today,

    /// Load a puzzle from the online archive
    #[bpaf(command("archive"), short('a'))]
    Archive {
        #[bpaf(positional("ID"))]
        id: String,
    },
}

enum FileType {
    Html,
    Hjson,
}

impl FileType {
    fn from_extension(extension: &OsStr) -> Option<Self> {
        if extension == "html" || extension == "htm" {
            Some(Self::Html)
        } else if extension == "hjson" {
            Some(Self::Hjson)
        } else {
            None
        }
    }
}

#[derive(Clone, Copy)]
enum InputMode {
    Today,
    File,
    Fetch,
    Paste,
}

impl InputMode {
    const ALL: [Self; 4] = [Self::Today, Self::File, Self::Fetch, Self::Paste];
}

impl fmt::Display for InputMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Today => write!(f, "download today's daily puzzle"),
            Self::Fetch => write!(f, "download puzzle from archive"),
            Self::File => write!(f, "load from file"),
            Self::Paste => write!(f, "paste html"),
        }
    }
}

enum HintOption<'name> {
    Name(&'name Name),
    MarkAsFlavor,
    Save,
}

impl HintOption<'_> {
    const FIXED: [Self; 2] = [Self::MarkAsFlavor, Self::Save];
}

impl fmt::Display for HintOption<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => write!(f, "{name}"),
            Self::MarkAsFlavor => write!(f, "mark hints as flavor"),
            Self::Save => write!(f, "save progress to file"),
        }
    }
}
