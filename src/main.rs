pub(crate) mod html;
mod solver;

use std::fs;
use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;

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
    Ok(())
}

#[derive(Parser)]
struct Args {
    html: Option<PathBuf>,
}
