use serde::{Deserialize, Serialize};

use crate::grid::Grid;
use crate::models::{CardBack, Coord, Judgment, Name, Profession};

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
        name: String,
        profession: String,
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

#[derive(Clone, Debug)]
pub(crate) struct JudgedCard {
    name: Name,
    profession: Profession,
    back: CardBack,
}

impl JudgedCard {
    pub(crate) fn name(&self) -> &Name {
        &self.name
    }

    pub(crate) fn judgment(&self) -> Judgment {
        self.back.judgment()
    }

    pub(crate) fn back(&self) -> &CardBack {
        &self.back
    }

    pub(crate) fn profession(&self) -> &Profession {
        &self.profession
    }

    pub(crate) fn new(name: Name, profession: Profession, back: CardBack) -> Self {
        Self {
            name,
            profession,
            back,
        }
    }

    pub(crate) fn set_hint(&mut self, hint: String) {
        self.back.set_hint(hint);
    }

    pub(crate) fn mark_as_flavor(&mut self) {
        self.back.mark_as_flavor();
    }
}
