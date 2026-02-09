pub(crate) mod html;
mod solver;

use std::path::{Path, PathBuf};
use std::{fmt, fs};

use anyhow::{Result, bail};
use clap::Parser;
use inquire::{Select, Text};
use itertools::Itertools as _;
use solver::Puzzle;

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
            let api_key = include_str!("../browserless_api_key").trim();
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
        if pending.is_empty() {
            pending.extend(puzzle.infer()?);
        }
        if puzzle.solved() {
            if let Some(((name, judgment), rest)) = pending.split_last() {
                println!(
                    "Mark {} and {name} as {judgment} to solve puzzle!",
                    rest.iter().format_with(", ", |(name, judgment), f| {
                        f(&format_args!("{name} as {judgment}"))
                    }),
                );
            } else {
                println!("Puzzle solved!");
            }
            break;
        }
        let Some((name, judgment)) = pending.pop() else {
            if pending.is_empty() {
                bail!("Stuck! Puzzle state: {puzzle:?}")
            }
            continue;
        };
        match Select::new(
            &format!("Mark {name} as {judgment}"),
            HintKind::ALL.to_vec(),
        )
        .prompt()?
        {
            HintKind::Logical => {}
            HintKind::Flavor => continue,
        }
        let hint = Text::new("Enter new hint:").prompt()?;
        puzzle.add_hint(&hint, &name)?;
    }
    Ok(())
}

#[derive(Parser)]
struct Args {
    html: Option<PathBuf>,
}

#[derive(Clone, Copy)]
enum HintKind {
    Logical,
    Flavor,
}

impl HintKind {
    const ALL: [Self; 2] = [Self::Logical, Self::Flavor];
}

impl fmt::Display for HintKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Logical => write!(f, "logical hint"),
            Self::Flavor => write!(f, "flavor text"),
        }
    }
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
