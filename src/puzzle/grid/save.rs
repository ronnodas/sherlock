use std::array;
use std::borrow::Cow;

use serde::{Deserialize, Serialize};
use serde_with::{DisplayFromStr, serde_as};

use crate::puzzle::grid::card::Card;
use crate::puzzle::grid::{Coordinate, Format, Grid};

#[derive(Serialize, Deserialize)]
pub(crate) struct CardList<'card> {
    cards: [IndexedCard<'card>; 20],
    format: Format,
}

impl From<CardList<'_>> for Grid {
    fn from(mut card_list: CardList) -> Self {
        card_list.cards.sort_by(|a, b| a.coord.cmp(&b.coord));
        let cards = card_list.cards.map(|card| card.card.into_owned());
        Self::new(cards, card_list.format)
    }
}

impl<'card> From<&'card Grid> for CardList<'card> {
    fn from(grid: &'card Grid) -> Self {
        let cards = array::from_fn(|i| IndexedCard {
            coord: Coordinate::from_index(i),
            card: Cow::Borrowed(&grid.cards[i]),
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
    #[serde(flatten)]
    card: Cow<'card, Card>,
}
