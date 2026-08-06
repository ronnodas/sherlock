use std::collections::HashMap;
use std::ops::{Index, IndexMut};

#[cfg(test)]
use anyhow::anyhow;
use anyhow::{Result, bail};
use itertools::Itertools as _;
use mitsein::btree_map1::BTreeMap1;
use mitsein::iter1::IteratorExt as _;
use select::document::Document;
use select::predicate::{Any, Attr, Predicate as _};
use serde::{Deserialize, Serialize};

use crate::grid::Grid;
use crate::models::{Card, CardBack, Coordinate, FlippedCard, Judgment, Name, Profession};
use crate::solver::Suspect;
use crate::solver::board::coordinates::Set1;
use crate::solver::board::parsers::{Class, ClassName, Div, NodeExt as _, parse_card};

pub(crate) mod coordinates;
pub(crate) mod editor;
mod parsers;
mod save;

pub(crate) type SolvedBoard = Board<FlippedCard>;

#[derive(Clone, Debug, Deserialize)]
#[serde(from = "save::CardList", bound = "Self: From<save::CardList<'de>>")]
pub(crate) struct Board<C = Card> {
    cards: Grid<C>,
    coordinates: HashMap<Name, Coordinate>,
    // TODO maybe change this to `IndexMap` or `HashMap` once `mitsein` supports that
    by_profession: BTreeMap1<Profession, Set1>,
    format: Format,
    start: Option<Coordinate>,
}

impl<C> Board<C> {
    pub(crate) fn start(&self) -> Option<Coordinate> {
        self.start
    }

    pub(crate) fn coordinates(&self) -> &HashMap<Name, Coordinate> {
        &self.coordinates
    }

    pub(crate) fn by_profession(&self) -> &BTreeMap1<String, Set1> {
        &self.by_profession
    }

    #[cfg(test)]
    pub(crate) fn coord(&self, name: &str) -> Result<Coordinate> {
        self.coordinates
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("{name} not in grid"))
    }
}

impl Board {
    pub(crate) fn parse(html: &str) -> Result<Self> {
        let document = Document::from(html);
        let Ok(cards) = document
            .find(Div.and(Class(ClassName::CardGrid)).and(Attr("id", "grid")))
            .exactly_one()
        else {
            bail!("expecting unique element in {html}");
        };
        let cards: Grid<(Card, bool)> = Grid::from_flattened(
            cards
                .expect_children::<20>(Any)?
                .iter()
                .map(|card| parse_card(card))
                .collect::<Result<Vec<(Card, bool)>>>()?
                .try_into()
                .unwrap_or_else(|_| unreachable!()),
        );

        // A valid puzzle must have at least one actual hint
        let format = if cards.iter().any(|&(_, has_hint)| has_hint) {
            Format::Sep2025
        } else {
            Format::Original
        };
        let cards = match format {
            Format::Original => cards.map(|(card, _)| card),
            Format::Sep2025 => cards.map(|(mut card, has_hint)| {
                if !has_hint && let Some(back) = card.back_mut() {
                    back.mark_as_flavor();
                }
                card
            }),
        };
        Ok(Self::new(cards, format, None))
    }

    fn new(cards: Grid<Card>, format: Format, start: Option<Coordinate>) -> Self {
        let coordinates = cards
            .iter()
            .enumerate()
            .map(|(index, card)| (card.name().to_owned(), Coordinate::from_index(index)))
            .collect();
        let by_profession = cards
            .iter()
            .enumerate()
            .map(|(index, card)| (card.profession().to_owned(), Coordinate::from_index(index)))
            .into_grouping_map()
            .aggregate(|set: Option<Set1>, _, coord| {
                let set = set.map_or_else(|| Set1::from_one(coord), |set| set | coord);
                Some(set)
            })
            .into_iter()
            .try_collect1()
            .expect("total len 20");
        let mut board = Self {
            cards,
            coordinates,
            by_profession,
            format,
            start,
        };
        board.set_start();
        board
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Card> {
        self.cards.iter()
    }

    pub(crate) fn solved(&self) -> bool {
        self.cards.iter().all(Card::flipped)
    }

    pub(crate) fn into_solved(self) -> Option<Board<FlippedCard>> {
        // TODO use try_map()
        let cards = Grid::from_flattened(
            self.cards
                .into_iter()
                .map(Card::into_flipped)
                .collect::<Option<Vec<FlippedCard>>>()?
                .try_into()
                .expect("length unchanged"),
        );
        Some(Board {
            cards,
            coordinates: self.coordinates,
            by_profession: self.by_profession,
            format: self.format,
            start: self.start,
        })
    }

    pub(crate) fn fixed(&self) -> Grid<Option<Judgment>> {
        self.cards.each_ref().map(Card::judgment)
    }

    pub(crate) fn set_new(&mut self, coord: Coordinate, judgment: Judgment) -> Option<&Card> {
        self.cards[coord].reveal(judgment)
    }

    pub(crate) fn add_hint(&mut self, hint: String, coord: Coordinate) -> Result<()> {
        self.card_back(coord)?.set_hint(hint);
        Ok(())
    }

    pub(crate) fn mark_as_flavor(&mut self, coord: Coordinate) -> Result<()> {
        self.card_back(coord)?.mark_as_flavor();
        self.set_start();
        Ok(())
    }

    pub(crate) fn pending_hints(&self) -> Vec<Suspect> {
        self.cards
            .iter()
            .enumerate()
            .filter_map(|(index, card)| card.hint_pending(Coordinate::from_index(index)))
            .collect()
    }

    pub(crate) fn format(&self) -> Format {
        self.format
    }

    pub(crate) fn emoji_summary(&self) -> String {
        self.cards
            .rows()
            .map(|row| row.each_ref().map(Card::emoji))
            .format_with("\n", |row, f| f(&row.iter().format_with("", |c, g| g(c))))
            .to_string()
    }

    fn card_back(&mut self, coord: Coordinate) -> Result<&mut CardBack> {
        if self[coord].back().is_none() {
            bail!("{}'s card is not flipped", self[coord].name())
        }
        // https://github.com/rust-lang/rust/issues/54663
        Ok(self[coord].back_mut().expect("checked above"))
    }

    fn set_start(&mut self) {
        self.start = self.start.or_else(|| {
            self.cards
                .iter()
                .enumerate()
                .filter(|(_, card)| card.logical_hint().is_some())
                .exactly_one()
                .ok()
                .map(|(index, _)| Coordinate::from_index(index))
        });
    }
}

impl Serialize for Board {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        save::CardList::from(self).serialize(serializer)
    }
}

impl<C> Index<Coordinate> for Board<C> {
    type Output = C;

    fn index(&self, index: Coordinate) -> &C {
        &self.cards[index]
    }
}

impl<C> IndexMut<Coordinate> for Board<C> {
    fn index_mut(&mut self, index: Coordinate) -> &mut C {
        &mut self.cards[index]
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub(crate) enum Format {
    Original,
    Sep2025,
}
