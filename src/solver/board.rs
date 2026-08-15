use std::collections::HashMap;
use std::ops::{Index, IndexMut};

#[cfg(test)]
use anyhow::anyhow;
use anyhow::{Result, bail};
use inquire::Confirm;
use itertools::Itertools as _;
use mitsein::btree_map1::BTreeMap1;
use mitsein::iter1::IteratorExt as _;
use select::document::Document;
use select::predicate::{Any, Attr, Predicate as _};
use serde::{Deserialize, Serialize};

use crate::grid::Grid;
use crate::models::{CardBack, Coord, JudgedCard, Judgment, Name, Profession, SolveCard};
use crate::solver::Suspect;
use crate::solver::board::coordinates::Set1;
use crate::solver::board::parsers::{Class, ClassName, Div, NodeExt as _, parse_card};
use crate::solver::hint::recipes::{AddContext as _, Context};
use crate::solver::hint::{Hint, Sentence};

pub(crate) mod coordinates;
pub(crate) mod editor;
mod parsers;
mod save;

pub(crate) type SolvedBoard = Board<JudgedCard>;

#[derive(Clone, Debug, Deserialize)]
#[serde(from = "save::CardList", bound = "Self: From<save::CardList<'de>>")]
pub(crate) struct Board<C = SolveCard> {
    cards: Grid<C>,
    coordinates: HashMap<Name, Coord>,
    // TODO maybe change this to `IndexMap` or `HashMap` once `mitsein` supports that
    by_profession: BTreeMap1<Profession, Set1>,
    start: Option<Coord>,
}

impl<C> Board<C> {
    pub(crate) fn context(&self, speaker: Coord) -> Context<'_> {
        Context {
            coordinates: &self.coordinates,
            by_profession: &self.by_profession,
            speaker,
        }
    }

    pub(crate) fn start(&self) -> Option<Coord> {
        self.start
    }

    #[cfg(test)]
    pub(crate) fn coord(&self, name: &str) -> Result<Coord> {
        self.coordinates
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("{name} not in grid"))
    }

    pub(crate) fn card(&self, coord: Coord) -> &C {
        &self.cards[coord]
    }
}

impl Board {
    fn new(cards: Grid<SolveCard>, start: Option<Coord>) -> Self {
        let coordinates = cards
            .iter()
            .enumerate()
            .map(|(index, card)| (card.name().to_owned(), Coord::from_index(index)))
            .collect();
        let by_profession = cards
            .iter()
            .enumerate()
            .map(|(index, card)| (card.profession().to_owned(), Coord::from_index(index)))
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
            start,
        };
        board.set_start();
        board
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &SolveCard> {
        self.cards.iter()
    }

    pub(crate) fn solved(&self) -> bool {
        self.cards.iter().all(SolveCard::flipped)
    }

    pub(crate) fn into_solved(self) -> Option<Board<JudgedCard>> {
        // TODO use try_map()
        let cards = Grid::from_flattened(
            self.cards
                .into_iter()
                .map(SolveCard::judged)
                .collect::<Option<Vec<JudgedCard>>>()?
                .try_into()
                .expect("length unchanged"),
        );
        Some(Board {
            cards,
            coordinates: self.coordinates,
            by_profession: self.by_profession,
            start: self.start,
        })
    }

    pub(crate) fn fixed(&self) -> Grid<Option<Judgment>> {
        self.cards.each_ref().map(SolveCard::judgment)
    }

    pub(crate) fn set_new(&mut self, coord: Coord, judgment: Judgment) -> Option<&SolveCard> {
        self.cards[coord].reveal(judgment)
    }

    pub(crate) fn add_hint(&mut self, hint: String, coord: Coord) -> Result<()> {
        self.card_back(coord)?.set_hint(hint);
        Ok(())
    }

    pub(crate) fn mark_as_flavor(&mut self, coord: Coord) -> Result<()> {
        self.card_back(coord)?.mark_as_flavor();
        self.set_start();
        Ok(())
    }

    pub(crate) fn pending_hints(&self) -> Vec<Suspect> {
        self.cards
            .iter()
            .enumerate()
            .filter_map(|(index, card)| card.hint_pending(Coord::from_index(index)))
            .collect()
    }

    pub(crate) fn emoji_summary(&self) -> String {
        self.cards
            .rows()
            .map(|row| row.each_ref().map(SolveCard::emoji))
            .format_with("\n", |row, f| f(&row.iter().format_with("", |c, g| g(c))))
            .to_string()
    }

    fn card_back(&mut self, coord: Coord) -> Result<&mut CardBack> {
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
                .map(|(index, _)| Coord::from_index(index))
        });
    }

    fn hints_by_coord(&self) -> impl Iterator<Item = (Coord, &str)> {
        self.iter()
            .enumerate()
            .filter_map(|(index, card)| Some((Coord::from_index(index), card.logical_hint()?)))
    }

    pub(crate) fn parse_all_hints(&self) -> Result<Vec<Hint>> {
        self.hints_by_coord()
            .map(|(speaker, hint)| Sentence::parse(hint)?.add_context(self.context(speaker)))
            .flatten_ok()
            .collect()
    }

    pub(crate) fn parse_hints_and_confirm_flavor(&mut self) -> Result<Vec<Hint>> {
        let mut hints = Vec::new();
        let mut unknown = Vec::new();
        for (speaker, hint) in self.hints_by_coord() {
            if let Ok(sentence) = Sentence::parse(hint)
                && let Ok(parsed) = sentence.add_context(self.context(speaker))
            {
                hints.extend(parsed);
            } else {
                unknown.push((speaker, hint.to_owned()));
            }
        }
        for (coord, hint) in unknown {
            let name = self.card(coord).name();
            let message = format!("Is {name}'s ({coord}) hint, \"{hint}\", just flavor text?");
            if Confirm::new(&message).prompt()? {
                self.mark_as_flavor(coord)?;
            } else {
                hints.extend(Sentence::parse(&hint)?.add_context(self.context(coord))?);
            }
        }
        Ok(hints)
    }
}

impl Serialize for Board {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        save::CardList::from(self).serialize(serializer)
    }
}

impl<C> Index<Coord> for Board<C> {
    type Output = C;

    fn index(&self, index: Coord) -> &C {
        &self.cards[index]
    }
}

impl<C> IndexMut<Coord> for Board<C> {
    fn index_mut(&mut self, index: Coord) -> &mut C {
        &mut self.cards[index]
    }
}

pub(crate) struct HtmlBoard {
    pub board: Board,
    pub format: Format,
}

impl HtmlBoard {
    pub(crate) fn parse(html: &str) -> Result<Self> {
        let document = Document::from(html);
        let Ok(cards) = document
            .find(Div.and(Class(ClassName::CardGrid)).and(Attr("id", "grid")))
            .exactly_one()
        else {
            bail!("expecting unique element in {html}");
        };

        let mut cards: [(SolveCard, bool); 20] = cards
            .expect_children::<20>(Any)?
            .iter()
            .map(|card| parse_card(card))
            .collect::<Result<Vec<(SolveCard, bool)>>>()?
            .try_into()
            .unwrap_or_else(|_| unreachable!());

        // A valid puzzle must have at least one actual hint
        let format = if cards.iter().any(|&(_, has_hint)| has_hint) {
            // Can't be done at the same time as the `iter()` above since we only change something
            // if the current `has_hint` is `false` and *some* `has_hint` is `true`
            for (card, has_hint) in &mut cards {
                if !*has_hint && let Some(back) = card.back_mut() {
                    back.mark_as_flavor();
                }
            }
            Format::Sep2025
        } else {
            Format::Original
        };

        // TODO also parse a title and/or date

        let cards = Grid::from_flattened(cards.map(|(card, _)| card));
        let board = Board::new(cards, None);
        Ok(Self { board, format })
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub(crate) enum Format {
    Original,
    Sep2025,
}
