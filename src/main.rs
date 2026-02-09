mod solver;

use std::path::{Path, PathBuf};
use std::{fmt, fs};

use anyhow::Result;
use clap::Parser;
use inquire::{MultiSelect, Select, Text};
use itertools::Itertools as _;

use solver::{Name, Puzzle, Update};

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
        InputMode::Today => {
            let api_key = match fs::read_to_string("browserless_api_key") {
                Ok(api_key) => api_key,
                Err(e) => {
                    println!("add a token from [browserless.io] to a `browserless_api_key` file");
                    return Err(e.into());
                }
            };
            let api_key = api_key.trim();
            let target_url = "https://cluesbysam.com/";
            // let selector = ".card-grid #grid";
            let json = format!(r#"{{"url": "{target_url}"}}"#,);
            // dbg!(&json);
            let html = ureq::post(format!(
                "https://production-sfo.browserless.io/content?token={api_key}"
            ))
            .content_type("application/json")
            .send(&json)?
            .body_mut()
            .read_to_string()?;
            Puzzle::parse(&html)
        }
        InputMode::File => {
            let path = Text::new("enter path to html:").prompt()?;
            read_from_file(path)
        }
        InputMode::Direct => Puzzle::prompt(),
    }
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
                                println!("I didn't understand that hint :(\n{e}");
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
    Direct,
}

impl InputMode {
    const ALL: [Self; 3] = [Self::Today, Self::File, Self::Direct];
}

impl fmt::Display for InputMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Today => write!(f, "download today's daily puzzle"),
            Self::File => write!(f, "load from html file"),
            Self::Direct => write!(f, "paste html"),
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
