mod puzzle;

use std::ffi::OsStr;
use std::fs::File;
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::{fmt, fs};

use anyhow::{Result, anyhow};
use chrono::Utc;
use chrono_tz::America::New_York;
use clap::Parser;
use inquire::{Confirm, MultiSelect, Select, Text};
use itertools::Itertools as _;

use puzzle::{Name, Puzzle, Update};

const API_KEY_FILE: &str = "browserless_api_key";
const SAVE_DIRECTORY: &str = "saved/";

fn main() -> Result<()> {
    let args = Args::parse();

    let (puzzle, pending) = match args.html {
        Some(path) => read_from_file(path)?,
        None => main_menu()?,
    };
    play(puzzle, pending)?;
    Ok(())
}

fn main_menu() -> Result<(Puzzle, Vec<Name>)> {
    let mode = Select::new(
        "Which puzzle do you want to solve?",
        InputMode::ALL.to_vec(),
    )
    .prompt()?;
    match mode {
        InputMode::Today => fetch_from_url("https://cluesbysam.com/", Some(date_string())),
        InputMode::Fetch => {
            let archive_id = Text::new("Enter puzzle archive id")
                .with_placeholder("a0b1c2d3e4f5")
                .prompt()?;
            // TODO puzzles from before 2026-09-01 seem to not have the `has-hint` class, manually confirm if unparseable hints are flavor?
            let target_url = format!("https://cluesbysam.com/s/archive/{archive_id}/");
            fetch_from_url(&target_url, Some(archive_id))
        }
        InputMode::File => {
            let path = Text::new("Enter path to html or hjson:")
                .with_initial_value(SAVE_DIRECTORY)
                .prompt()?;
            read_from_file(path)
        }
        InputMode::Paste => Puzzle::prompt(),
    }
}

fn date_string() -> String {
    Utc::now()
        .with_timezone(&New_York)
        .date_naive()
        .format("%F")
        .to_string()
}

fn fetch_from_url(target_url: &str, name: Option<Name>) -> Result<(Puzzle, Vec<Name>)> {
    let api_key = read_api_key()?;
    let json = format!(r#"{{"url": "{target_url}"}}"#);
    let html = ureq::post(format!(
        "https://production-sfo.browserless.io/content?token={api_key}"
    ))
    .content_type("application/json")
    .send(&json)?
    .body_mut()
    .read_to_string()?;
    Puzzle::parse(&html, name)
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

fn read_from_file(path: impl AsRef<Path>) -> Result<(Puzzle, Vec<Name>)> {
    let path = path.as_ref();
    let contents = fs::read_to_string(path)?;
    let name = path
        .file_stem()
        .and_then(|name| name.to_str())
        .map(str::to_owned);
    let (puzzle, pending) = match path.extension() {
        Some(extension) if extension == "hjson" => Puzzle::load(&contents, name)?,
        Some(extension) if extension == "html" || extension == "htm" => {
            Puzzle::parse(&contents, name)?
        }
        Some(_) | None => {
            let (mut puzzle, pending) = Puzzle::load(&contents, None).or_else(|e_save| {
                Puzzle::parse(&contents, None).map_err(|e_html| {
                    anyhow!(
                        "Could not parse as either as a saved game ({e_save}) or html ({e_html})"
                    )
                })
            })?;
            if let Some(name) = name {
                puzzle.set_name(name);
            }
            (puzzle, pending)
        }
    };
    Ok((puzzle, pending))
}

fn play(mut puzzle: Puzzle, mut pending: Vec<Name>) -> Result<()> {
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
        pending.extend(new.into_iter().map(Update::into_name));
        pending.sort_unstable();

        loop {
            let selected = Select::new(
                "Add a logical hint:",
                pending
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
                                pending.retain(|pending| pending != &name);
                                break;
                            }
                            Err(e) => {
                                println!("I didn't understand that hint :(\n\n{e}");
                            }
                        }
                    }
                }
                HintOption::MarkAsFlavor => {
                    let flavor =
                        MultiSelect::new("Select characters with flavor text", pending.clone())
                            .prompt_skippable()?
                            .unwrap_or_default();
                    pending.retain(|pending| !flavor.contains(pending));
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

#[derive(Parser)]
struct Args {
    html: Option<PathBuf>,
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
