mod solver;

use std::path::{Path, PathBuf};
use std::{fmt, fs};

use anyhow::Result;
use clap::Parser;
use inquire::{Confirm, MultiSelect, Select, Text};
use itertools::Itertools as _;

use solver::{Name, Puzzle, Update};

const API_KEY_FILE: &str = "browserless_api_key";

fn main() -> Result<()> {
    let args = Args::parse();

    let puzzle = match args.html {
        Some(path) => read_from_file(path)?,
        None => main_menu()?,
    };
    play(puzzle)?;
    Ok(())
}

fn main_menu() -> Result<Puzzle> {
    let mode = Select::new(
        "Which puzzle do you want to solve?",
        InputMode::ALL.to_vec(),
    )
    .prompt()?;
    match mode {
        InputMode::Today => fetch_from_url("https://cluesbysam.com/"),
        InputMode::Fetch => {
            let target_url = Text::new("Enter puzzle url").prompt()?;
            fetch_from_url(&target_url)
        }
        InputMode::File => {
            let path = Text::new("Enter path to html:").prompt()?;
            read_from_file(path)
        }
        InputMode::Paste => Puzzle::prompt(),
    }
}

fn fetch_from_url(target_url: &str) -> Result<Puzzle> {
    let api_key = read_api_key()?;
    let json = format!(r#"{{"url": "{target_url}"}}"#);
    let html = ureq::post(format!(
        "https://production-sfo.browserless.io/content?token={api_key}"
    ))
    .content_type("application/json")
    .send(&json)?
    .body_mut()
    .read_to_string()?;
    Puzzle::parse(&html)
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

fn read_from_file(path: impl AsRef<Path>) -> Result<Puzzle> {
    let html = fs::read_to_string(path)?;
    Puzzle::parse(&html)
}

fn play(mut puzzle: Puzzle) -> Result<()> {
    let mut pending = vec![];
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
    Paste,
    Fetch,
}

impl InputMode {
    const ALL: [Self; 4] = [Self::Today, Self::Fetch, Self::File, Self::Paste];
}

impl fmt::Display for InputMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Today => write!(f, "download today's daily puzzle"),
            Self::Fetch => write!(f, "download puzzle from url"),
            Self::File => write!(f, "load from html file"),
            Self::Paste => write!(f, "paste html"),
        }
    }
}

enum HintOption<'name> {
    Name(&'name Name),
    MarkAsFlavor,
}

impl HintOption<'_> {
    const FIXED: [Self; 1] = [Self::MarkAsFlavor];
}

impl fmt::Display for HintOption<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => write!(f, "{name}"),
            Self::MarkAsFlavor => write!(f, "mark hints as flavor"),
        }
    }
}
