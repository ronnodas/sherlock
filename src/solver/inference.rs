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
