use std::borrow::Cow;

use anyhow::{Result, bail};
use jiff::civil::Date;
use serde::{Deserialize, Serialize};
use strum::{Display, EnumDiscriminants, EnumString, VariantArray};

use crate::grid::Grid;
use crate::models::{CardFront, Coord, Judgment, Name, Profession};

#[derive(Serialize, Deserialize)]
pub(crate) struct Puzzle {
    pub cards: Grid<Card>,
    pub start: Coord,
    pub metadata: Metadata,
}

impl Puzzle {
    pub(crate) fn new(cards: Grid<Card>, start: Coord, metadata: Metadata) -> Result<Self> {
        if cards[start].hint.is_flavor() {
            bail!("Starting hint is flavor text")
        }
        Ok(Self {
            cards,
            start,
            metadata,
        })
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

#[derive(Serialize, Deserialize)]
pub(crate) enum Metadata {
    Daily {
        date: Date,
        difficulty: Difficulty,
    },
    Archive {
        id: String,
        difficulty: Difficulty,
    },
    PuzzlePack {
        pack: u8,
        puzzle: u8,
        difficulty: Difficulty,
    },
    Community {
        user: String,
        id: String,
    },
    Custom,
}

impl Metadata {
    pub(crate) fn split(self) -> PartialMetadata {
        let (id, difficulty) = match self {
            Self::Daily { date, difficulty } => (PuzzleId::Daily(date), Some(difficulty)),
            Self::Archive { id, difficulty } => (PuzzleId::Archive(id), Some(difficulty)),
            Self::PuzzlePack {
                pack,
                puzzle,
                difficulty,
            } => (PuzzleId::PuzzlePack { pack, puzzle }, Some(difficulty)),
            Self::Community { user, id } => (PuzzleId::Community { user, id }, None),
            Self::Custom => (PuzzleId::Custom, None),
        };
        PartialMetadata { id, difficulty }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct PartialMetadata {
    pub id: PuzzleId,
    pub difficulty: Option<Difficulty>,
}

impl From<Metadata> for PartialMetadata {
    fn from(metadata: Metadata) -> Self {
        metadata.split()
    }
}

#[derive(Serialize, Deserialize, Debug, EnumDiscriminants, PartialEq, Eq)]
#[strum_discriminants(derive(VariantArray, Display))]
#[strum_discriminants(strum(serialize_all = "title_case"))]
pub(crate) enum PuzzleId {
    Daily(Date),
    Archive(String),
    PuzzlePack { pack: u8, puzzle: u8 },
    Community { user: String, id: String },
    Custom,
}

impl PuzzleId {
    pub(crate) fn save_name(&self) -> Cow<'_, str> {
        match self {
            Self::Daily(date) => Cow::Owned(date.to_string()),
            Self::Archive(id) => Cow::Borrowed(id.as_str()),
            Self::PuzzlePack { pack, puzzle } => Cow::Owned(format!("puzzle-pack-{pack}-{puzzle}")),
            Self::Custom => Cow::Owned(String::new()),
            Self::Community { user, id } => Cow::Owned(format!("community-{user}-{id}")),
        }
    }

    pub(crate) fn date_from_month_b(year: &str, month: &str, day: &str) -> Option<Date> {
        let month = match month {
            "Jan" => 1,
            "Feb" => 2,
            "Mar" => 3,
            "Apr" => 4,
            "May" => 5,
            "Jun" => 6,
            "Jul" => 7,
            "Aug" => 8,
            "Sep" => 9,
            "Oct" => 10,
            "Nov" => 11,
            "Dec" => 12,
            _ => return None,
        };
        Date::new(year.parse().ok()?, month, day.parse().ok()?).ok()
    }
}

#[derive(Serialize, Deserialize, VariantArray, Display, Clone, Copy, Debug, EnumString)]
#[strum(serialize_all = "title_case")]
pub(crate) enum Difficulty {
    Easy,
    Medium,
    Tricky,
    Hard,
    Brutal,
    Evil,
    SuperEvil,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn title_case() {
        assert_eq!(PuzzleIdDiscriminants::PuzzlePack.to_string(), "Puzzle Pack");
    }
}
