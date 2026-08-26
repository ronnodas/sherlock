#![allow(dead_code, clippy::todo, reason = "in progress")]

use anyhow::Result;

use crate::models::{Coord, Judgment};
use crate::solver::Engine;
use crate::solver::hint::Hint;

#[derive(Default)]
struct InferenceSolver {
    theorems: Vec<Theorem>,
}

impl Engine for InferenceSolver {
    fn add_parsed_hint(&mut self, hint: &Hint) {
        match hint {
            &Hint::Judgment(coord, judgment) => self.theorems.push(Theorem::Is(coord, judgment)),
            Hint::Count(..) => todo!(),
            Hint::CountTotal(..) => todo!(),
            Hint::Connected(..) => todo!(),
            Hint::CompareSets(..) => todo!(),
            Hint::UniqueWithCount { .. } => todo!(),
            Hint::EachNeighbors(..) => todo!(),
            Hint::CountWithNeighbors { .. } => todo!(),
            Hint::IfThen(_) => todo!(),
        }
    }

    fn updates(&mut self) -> Result<Vec<(Coord, Judgment)>> {
        todo!()
    }
}

enum Theorem {
    Is(Coord, Judgment),
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;

    use crate::ARCHIVE_DIR;
    use crate::solver::tests::solve;

    use super::*;

    #[test]
    fn sample() {
        let path = Path::new(ARCHIVE_DIR).join("2026-08-03.ron");
        println!("Trying to solve: {}", path.display());
        let contents = fs::read_to_string(path).unwrap();
        let puzzle = ron::from_str(&contents).unwrap();
        solve::<InferenceSolver>(&puzzle);
    }
}
