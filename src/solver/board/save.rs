use std::borrow::Cow;

use serde::{Deserialize, Serialize};

use crate::grid::Grid;
use crate::models::{CardBack, CardFront, Coord, Name, Profession};
use crate::solver::board::Board;

#[derive(Serialize, Deserialize)]
pub(crate) struct CardList<'card> {
    cards: Grid<RefCard<'card>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    start: Option<Coord>,
}

impl From<CardList<'_>> for Board {
    fn from(mut card_list: CardList) -> Self {
        let backs = card_list
            .cards
            .each_mut()
            .map(|card| card.back.take().map(Cow::into_owned));
        let fronts = card_list.cards.map(|card| CardFront {
            name: card.name.into_owned(),
            profession: card.profession.into_owned(),
        });
        Self::new(fronts, backs, card_list.start)
    }
}

impl<'card> From<&'card Board> for CardList<'card> {
    fn from(board: &'card Board) -> Self {
        let cards = Grid::from_fn(|coord| {
            let CardFront { name, profession } = board.front(coord);
            let back = board.back(coord);
            RefCard {
                name: Cow::Borrowed(name),
                profession: Cow::Borrowed(profession),
                back: back.map(Cow::Borrowed),
            }
        });
        Self {
            cards,
            start: board.start(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct RefCard<'card> {
    name: Cow<'card, Name>,
    profession: Cow<'card, Profession>,
    #[serde(skip_serializing_if = "Option::is_none")]
    back: Option<Cow<'card, CardBack>>,
}
