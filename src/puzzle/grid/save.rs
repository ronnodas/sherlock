use std::array;
use std::borrow::Cow;

use serde::{Deserialize, Serialize};
use serde_with::{DisplayFromStr, serde_as};

use crate::puzzle::grid::card::{Card, CardBack};
use crate::puzzle::grid::{Coordinate, Format, Grid};
use crate::puzzle::{Name, Profession};

#[derive(Serialize, Deserialize)]
pub(crate) struct CardList<'card> {
    cards: [IndexedCard<'card>; 20],
    format: Format,
}

impl From<CardList<'_>> for Grid {
    fn from(mut card_list: CardList) -> Self {
        card_list.cards.sort_by(|a, b| a.coord.cmp(&b.coord));
        let cards = card_list.cards.map(Card::from);
        Self::new(cards, card_list.format)
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
        }
    }
}

#[serde_as]
#[derive(Serialize, Deserialize)]
struct IndexedCard<'card> {
    #[serde_as(as = "DisplayFromStr")]
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
