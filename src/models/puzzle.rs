use serde::{Deserialize, Serialize};

use crate::grid::Grid;
use crate::models::{Coord, Judgment, Name, Profession};

#[derive(Serialize, Deserialize)]
pub(crate) struct Puzzle {
    cards: Grid<Card>,
    start: Coord,
}

impl Puzzle {
    pub(crate) fn new(cards: Grid<Card>, start: Coord) -> Self {
        Self { cards, start }
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct Card {
    name: Name,
    profession: Profession,
    judgment: Judgment,
    hint: HintText,
}

impl Card {
    pub(crate) fn new(
        name: Name,
        profession: Profession,
        judgment: Judgment,
        hint: HintText,
    ) -> Self {
        Self {
            name,
            profession,
            judgment,
            hint,
        }
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) enum HintText {
    Flavor,
    Logical(String),
}
