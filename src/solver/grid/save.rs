use std::array;
use std::borrow::Cow;

use serde::{Deserialize, Serialize};

use crate::models::{Card, CardBack, Coordinate, Name, Profession};
use crate::solver::grid::{Format, Grid};

#[derive(Serialize, Deserialize)]
pub(crate) struct CardList<'card> {
    cards: [IndexedCard<'card>; 20],
    format: Format,
    #[serde(skip_serializing_if = "Option::is_none")]
    start: Option<Coordinate>,
}

impl From<CardList<'_>> for Grid {
    fn from(mut card_list: CardList) -> Self {
        card_list.cards.sort_by_key(|a| a.coord);
        let cards = card_list.cards.map(Card::from);
        Self::new(cards, card_list.format, card_list.start)
    }
}

impl<'card> From<&'card Grid> for CardList<'card> {
    fn from(grid: &'card Grid) -> Self {
        let cards = array::from_fn(|i| {
            let card = &grid.cards[i];
            IndexedCard {
                coord: Coordinate::from_index(i),
                name: Cow::Borrowed(card.name()),
                profession: Cow::Borrowed(card.profession()),
                back: card.back().map(Cow::Borrowed),
            }
        });
        Self {
            cards,
            format: grid.format,
            start: grid.start,
        }
    }
}

#[derive(Serialize, Deserialize)]
struct IndexedCard<'card> {
    coord: Coordinate,

    name: Cow<'card, Name>,
    profession: Cow<'card, Profession>,
    #[serde(skip_serializing_if = "Option::is_none")]
    back: Option<Cow<'card, CardBack>>,
}

impl From<IndexedCard<'_>> for Card {
    fn from(card: IndexedCard) -> Self {
        Self::new(
            card.name.into_owned(),
            card.profession.into_owned(),
            card.back.map(Cow::into_owned),
        )
    }
}
