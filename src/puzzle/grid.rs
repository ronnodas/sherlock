pub(crate) mod card;
pub(crate) mod coordinate;
pub(crate) mod editor;
mod html;
mod save;

use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use anyhow::{Result, anyhow, bail};
use itertools::Itertools as _;
use mitsein::NonEmpty;
use mitsein::iter1::IteratorExt as _;
use mitsein::vec1::Vec1;
use select::document::Document;
use select::predicate::{Any, Attr, Predicate as _};
use serde::{Deserialize, Serialize};

use crate::puzzle::grid::card::CardBack;
use crate::puzzle::hint::Set;
use crate::puzzle::{Judgment, Name, Profession};

use card::Card;
use html::{Class, ClassName, Div, NodeExt as _};

#[derive(Clone, Debug, Deserialize)]
#[serde(from = "save::CardList")]
pub(crate) struct Grid {
    cards: [Card; 20],
    coordinates: HashMap<Name, coordinate::Coordinate>,
    // TODO make this non-empty once mitsein supports that
    by_profession: HashMap<Profession, NonEmpty<Set>>,
    format: Format,
    start: Option<coordinate::Coordinate>,
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

    fn new(cards: [Card; 20], format: Format, start: Option<coordinate::Coordinate>) -> Self {
        let coordinates = cards
            .iter()
            .enumerate()
            .map(|(index, card)| {
                (
                    card.name().to_owned(),
                    coordinate::Coordinate::from_index(index),
                )
            })
            .collect();
        let by_profession = cards
            .iter()
            .enumerate()
            .map(|(index, card)| {
                (
                    card.profession().to_owned(),
                    coordinate::Coordinate::from_index(index),
                )
            })
            .into_grouping_map()
            .aggregate(|set: Option<NonEmpty<Set>>, _, item| {
                let set = set.map_or_else(
                    || NonEmpty::<Set>::from_one(item),
                    |mut set| {
                        _ = set.insert(item);
                        set
                    },
                );
                Some(set)
            });
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

    pub(crate) fn coord(&self, name: &Name) -> Result<coordinate::Coordinate> {
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

    pub(crate) fn profession_as_set(&self, profession: &Profession) -> Result<&NonEmpty<Set>> {
        self.by_profession
            .get(profession)
            .ok_or_else(|| anyhow!("{profession} not in grid"))
    }

    pub(crate) fn add_hint(&mut self, hint: String, suspect: &Name) -> Result<()> {
        self.card_back(suspect)?.set_hint(hint);
        Ok(())
    }

    pub(crate) fn mark_as_flavor(&mut self, suspect: &Name) -> Result<()> {
        self.card_back(suspect)?.mark_as_flavor();
        self.set_start();
        Ok(())
    }

    pub(crate) fn pending_hints(&self) -> Vec<String> {
        self.cards
            .iter()
            .filter(|card| card.hint_pending())
            .map(|card| card.name().clone())
            .collect()
    }

    pub(crate) fn other_professions(&self, profession: &str) -> Result<Vec1<Set>> {
        self.by_profession
            .iter()
            .filter(move |&(other, _)| other != profession)
            .map(|(_, set)| set.clone().into_hash_set())
            .try_collect1()
            .map_err(|_empty| anyhow!("only {profession}s on grid"))
    }

    pub(crate) fn format(&self) -> Format {
        self.format
    }

    fn card_back(&mut self, suspect: &Name) -> Result<&mut CardBack> {
        let index = self.coord(suspect)?;
        self.cards[index.to_index()]
            .back_mut()
            .ok_or_else(|| anyhow!("{suspect}'s card is not flipped"))
    }

    pub(crate) fn by_profession(&self) -> &HashMap<Profession, NonEmpty<Set>> {
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
                .map(|(index, _)| coordinate::Coordinate::from_index(index))
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

impl Index<coordinate::Coordinate> for Grid {
    type Output = Card;

    fn index(&self, index: coordinate::Coordinate) -> &Card {
        &self.cards[index.to_index()]
    }
}

impl IndexMut<coordinate::Coordinate> for Grid {
    fn index_mut(&mut self, index: coordinate::Coordinate) -> &mut Card {
        &mut self.cards[index.to_index()]
    }
}
