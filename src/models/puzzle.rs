use anyhow::{Result, bail};
use serde::{Deserialize, Serialize};

use crate::grid::Grid;
use crate::models::{CardFront, Coord, Judgment, Name, Profession};

#[derive(Serialize, Deserialize)]
pub(crate) struct Puzzle {
    pub cards: Grid<Card>,
    pub start: Coord,
}

impl Puzzle {
    pub(crate) fn new(cards: Grid<Card>, start: Coord) -> Result<Self> {
        if cards[start].hint.is_flavor() {
            bail!("Starting hint is flavor text")
        }
        Ok(Self { cards, start })
    }

    pub(crate) fn starting_hint(&self) -> &str {
        self.cards[self.start]
            .hint
            .as_logical()
            .expect("checked at construction")
    }
}

#[derive(Serialize, Deserialize, Clone)]
#[serde(from = "Flattened", into = "Flattened")]
pub(crate) struct Card {
    pub front: CardFront,
    pub judgment: Judgment,
    pub hint: HintText,
}

impl Card {
    pub(crate) fn new(
        name: Name,
        profession: Profession,
        judgment: Judgment,
        hint: HintText,
    ) -> Self {
        let front = CardFront { name, profession };
        Self {
            front,
            judgment,
            hint,
        }
    }
}

impl AsRef<CardFront> for Card {
    fn as_ref(&self) -> &CardFront {
        &self.front
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub(crate) enum HintText {
    Flavor,
    Logical(String),
}

impl HintText {
    fn is_flavor(&self) -> bool {
        matches!(self, Self::Flavor)
    }

    #[must_use]
    pub(crate) fn as_logical(&self) -> Option<&str> {
        if let Self::Logical(hint) = self {
            Some(hint)
        } else {
            None
        }
    }
}

#[derive(Serialize, Deserialize)]
struct Flattened {
    name: Name,
    profession: Profession,
    judgment: Judgment,
    hint: HintText,
}

impl From<Card> for Flattened {
    fn from(card: Card) -> Self {
        Self {
            name: card.front.name,
            profession: card.front.profession,
            judgment: card.judgment,
            hint: card.hint,
        }
    }
}

impl From<Flattened> for Card {
    fn from(flat: Flattened) -> Self {
        Self::new(flat.name, flat.profession, flat.judgment, flat.hint)
    }
}
