use serde::{Deserialize, Serialize};

use crate::models::{CardBack, Coordinate, Judgment, Name, Profession};

#[derive(Serialize, Deserialize)]
pub(crate) struct Puzzle {
    cards: [FullCard; 20],
    start: Coordinate,
}

impl Puzzle {
    pub(crate) fn new(cards: [FullCard; 20], start: Coordinate) -> Self {
        Self { cards, start }
    }
}

#[derive(Serialize, Deserialize)]
pub(crate) struct FullCard {
    name: Name,
    profession: Profession,
    judgment: Judgment,
    hint: HintText,
}

impl FullCard {
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
pub(crate) struct FlippedCard {
    name: Name,
    profession: Profession,
    back: CardBack,
}

impl FlippedCard {
    pub(crate) fn name(&self) -> &Name {
        &self.name
    }

    pub(crate) fn judgment(&self) -> Judgment {
        self.back.judgment()
    }

    pub(crate) fn back_mut(&mut self) -> &mut CardBack {
        &mut self.back
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
}
