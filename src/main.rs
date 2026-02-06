pub(crate) mod html;
mod solver;

use std::path::PathBuf;
use std::{fmt, fs};

use anyhow::{Result, bail};
use clap::Parser;

use inquire::{Select, Text};
use solver::Puzzle;

fn main() -> Result<()> {
    let args = Args::parse();

    let puzzle = match args.html {
        Some(path) => {
            let html = fs::read_to_string(path)?;
            Puzzle::parse(&html)?
        }
        None => Puzzle::prompt()?,
    };
    let mut pending = vec![];
    while !pending.is_empty() || !puzzle.solved() {
        let Some((name, judgment)) = pending.pop() else {
            pending.extend(puzzle.infer());
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
        let hint = Text::new("Enter new hint").prompt()?;
        puzzle.add_hint(hint)?;
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
