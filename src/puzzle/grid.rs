pub(crate) mod card;
pub(crate) mod coordinate;
pub(crate) mod editor;
mod html;
mod save;

use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use anyhow::{Result, anyhow, bail};
use itertools::Itertools as _;
use mitsein::btree_map1::BTreeMap1;
use mitsein::iter1::IteratorExt as _;
use mitsein::vec1::Vec1;
use select::document::Document;
use select::predicate::{Any, Attr, Predicate as _};
use serde::{Deserialize, Serialize};

use crate::puzzle::grid::card::CardBack;
use crate::puzzle::grid::coordinate::{Coordinate, Set1};
use crate::puzzle::{Judgment, Name, Profession, Suspect};

use card::Card;
use html::{Class, ClassName, Div, NodeExt as _};

#[derive(Clone, Debug, Deserialize)]
#[serde(from = "save::CardList")]
pub(crate) struct Grid {
    cards: [Card; 20],
    coordinates: HashMap<Name, Coordinate>,
    // TODO maybe change this to `IndexMap` or `HashMap` once `mitsein` supports that
    by_profession: BTreeMap1<Profession, Set1>,
    format: Format,
    start: Option<Coordinate>,
}

impl Grid {
    pub(crate) fn parse(html: &str) -> Result<Self> {
        let document = Document::from(html);
        let Ok(cards) = document
            .find(Div.and(Class(ClassName::CardGrid)).and(Attr("id", "grid")))
            .exactly_one()
        else {
            bail!("expecting unique element in {html}");
        };
        let cards: [(Card, bool); 20] = cards
            .expect_children::<20>(Any)?
            .iter()
            .map(|card| Card::parse(card))
            .collect::<Result<Vec<(Card, bool)>>>()?
            .try_into()
            .unwrap_or_else(|_| unreachable!());
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

    fn new(cards: [Card; 20], format: Format, start: Option<Coordinate>) -> Self {
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
        let mut grid = Self {
            cards,
            coordinates,
            by_profession,
            format,
            start,
        };
        grid.set_start();
        grid
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Card> {
        self.cards.iter()
    }

    pub(crate) fn into_cards(self) -> [Card; 20] {
        self.cards
    }

    pub(crate) fn coord(&self, name: &str) -> Result<Coordinate> {
        self.coordinates
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("{name} not in grid"))
    }

    pub(crate) fn solved(&self) -> bool {
        self.cards.iter().all(Card::flipped)
    }

    pub(crate) fn fixed(&self) -> [Option<Judgment>; 20] {
        self.cards.each_ref().map(Card::judgment)
    }

    pub(crate) fn set_new(&mut self, index: usize, judgment: Judgment) -> Option<&Card> {
        self.cards[index].reveal(judgment)
    }

    pub(crate) fn profession_as_set(&self, profession: &Profession) -> Result<&Set1> {
        self.by_profession
            .get(profession)
            .ok_or_else(|| anyhow!("{profession} not in grid"))
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
            .filter_map(|(index, card)| card.to_suspect(Coordinate::from_index(index)))
            .collect()
    }

    pub(crate) fn other_professions(&self, profession: &str) -> Result<Vec1<Set1>> {
        self.by_profession
            .as_btree_map()
            .iter()
            .filter(move |&(other, _)| other != profession)
            .map(|(_, &set)| set)
            .try_collect1()
            .map_err(|_empty| anyhow!("only {profession}s on grid"))
    }

    pub(crate) fn format(&self) -> Format {
        self.format
    }

    pub(crate) fn emoji_summary(&self) -> String {
        let (rows, _) = self.cards.as_chunks::<4>();
        rows.iter()
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

    pub(crate) fn by_profession(&self) -> &BTreeMap1<Profession, Set1> {
        &self.by_profession
    }

    pub(crate) fn set_start(&mut self) {
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

#[derive(Clone, Copy, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub(crate) enum Format {
    Original,
    Sep2025,
}

impl Serialize for Grid {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        save::CardList::from(self).serialize(serializer)
    }
}

impl Index<Coordinate> for Grid {
    type Output = Card;

    fn index(&self, index: Coordinate) -> &Card {
        &self.cards[index.to_index()]
    }
}

impl IndexMut<Coordinate> for Grid {
    fn index_mut(&mut self, index: Coordinate) -> &mut Card {
        &mut self.cards[index.to_index()]
    }
}
