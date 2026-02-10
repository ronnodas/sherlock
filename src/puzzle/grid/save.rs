use std::array;
use std::borrow::Cow;

use serde::{Deserialize, Serialize};
use serde_with::{DisplayFromStr, serde_as};

use crate::puzzle::grid::card::Card;
use crate::puzzle::grid::{Coordinate, Grid};


#[derive(Serialize, Deserialize)]
pub(crate) struct CardList<'card> {
    cards: [IndexedCard<'card>; 20],
}

impl From<CardList<'_>> for Grid {
    fn from(mut cards: CardList) -> Self {
        cards.cards.sort_by(|a, b| a.coord.cmp(&b.coord));
        let cards = cards.cards.map(|card| card.card.into_owned());
        Self::new(cards)
    }
}

impl<'card> From<&'card Grid> for CardList<'card> {
    fn from(grid: &'card Grid) -> Self {
        let cards = array::from_fn(|i| IndexedCard {
            coord: Coordinate::from_index(i),
            card: Cow::Borrowed(&grid.cards[i]),
        });
        Self { cards }
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
