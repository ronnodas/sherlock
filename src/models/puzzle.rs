use serde::{Deserialize, Serialize};

use crate::models::{Coordinate, Judgment, Name, Profession};

#[derive(Serialize, Deserialize)]
pub(crate) struct Puzzle {
    cards: [FullCard; 20],
    start: Coordinate,
}

#[derive(Serialize, Deserialize)]
pub(crate) struct FullCard {
    name: Name,
    profession: Profession,
    judgment: Judgment,
    hint: HintText,
}

#[derive(Serialize, Deserialize)]
pub(crate) enum HintText {
    Flavor,
    Logical(String),
}
