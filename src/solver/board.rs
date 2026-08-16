use std::collections::HashMap;

#[cfg(test)]
use anyhow::anyhow;
use anyhow::{Context as _, Result, bail};
use inquire::Confirm;
use itertools::Itertools as _;
use mitsein::btree_map1::BTreeMap1;
use mitsein::iter1::IteratorExt as _;
use select::document::Document;
use select::predicate::{Any, Attr, Predicate as _};
use serde::{Deserialize, Serialize};

use crate::grid::Grid;
use crate::models::{CardBack, CardFront, Coord, Judgment, Name, Profession};
use crate::solver::Suspect;
use crate::solver::board::coordinates::Set1;
use crate::solver::board::parsers::{Class, ClassName, Div, NodeExt as _, parse_card};
use crate::solver::hint::recipes::{AddContext as _, Context};
use crate::solver::hint::{Hint, Sentence};

pub(crate) mod coordinates;
pub(crate) mod editor;
mod parsers;
mod save;

pub(crate) type SolvedBoard = Board<CardBack>;

#[derive(Clone, Debug, Deserialize)]
#[serde(from = "save::CardList", bound = "Self: From<save::CardList<'de>>")]
pub(crate) struct Board<B = Option<CardBack>> {
    fixed: BoardFixed,
    backs: Grid<B>,
}

impl<B> Board<B> {
    pub(crate) fn context(&self, speaker: Coord) -> Context<'_> {
        self.fixed.context(speaker)
    }

    #[cfg(test)]
    pub(crate) fn coord(&self, name: &Name) -> Result<Coord> {
        self.fixed.coord(name)
    }

    pub(crate) fn front(&self, coord: Coord) -> &CardFront {
        &self.fixed.fronts[coord]
    }

    pub(crate) fn start(&self) -> Option<Coord> {
        self.fixed.start
    }

    fn fronts(&self) -> impl Iterator<Item = (Coord, &CardFront)> {
        self.fixed.fronts.iter()
    }
}

impl Board {
    fn new(fronts: Grid<CardFront>, backs: Grid<Option<CardBack>>, start: Option<Coord>) -> Self {
        let fixed = BoardFixed::new(fronts, start);
        let mut board = Self { fixed, backs };
        board.set_start();
        board
    }

    pub(crate) fn solved(&self) -> bool {
        self.backs.values().all(Option::is_some)
    }

    pub(crate) fn into_solved(self) -> Option<Board<CardBack>> {
        // TODO use try_map()
        let backs = self
            .backs
            .into_values()
            .collect::<Option<Vec<CardBack>>>()?
            .try_into()
            .expect("length unchanged");
        let backs = Grid::from_flattened(backs);
        Some(Board {
            backs,
            fixed: self.fixed,
        })
    }

    pub(crate) fn fixed(&self) -> Grid<Option<Judgment>> {
        self.backs
            .each_ref()
            .map(|card| Some(card.as_ref()?.judgment()))
    }

    // TODO error if already set to the other judgment
    pub(crate) fn try_judge(&mut self, coord: Coord, judgment: Judgment) -> bool {
        let back = &mut self.backs[coord];
        if back.is_none() {
            *back = Some(CardBack::new(judgment));
            true
        } else {
            false
        }
    }

    pub(crate) fn add_hint(&mut self, hint: String, coord: Coord) -> Result<()> {
        self.card_back_mut(coord)
            .with_context(|| format!("{coord} is not flipped"))?
            .set_hint(hint);
        Ok(())
    }

    pub(crate) fn mark_as_flavor(&mut self, coord: Coord) -> Result<()> {
        self.card_back_mut(coord)
            .with_context(|| format!("{coord} is not flipped"))?
            .mark_as_flavor();
        self.set_start();
        Ok(())
    }

    pub(crate) fn pending_hints(&self) -> Vec<Suspect> {
        self.backs
            .iter()
            .filter_map(|(coord, card)| {
                let judgment = card.as_ref()?.hint_pending()?;
                let name = self.front(coord).name.clone();
                Some(Suspect::new(coord, name, judgment))
            })
            .collect()
    }

    pub(crate) fn emoji_summary(&self) -> String {
        self.backs
            .rows()
            .map(|row| {
                row.each_ref()
                    .map(|back| back.as_ref().map_or('⬛', |back| back.judgment().emoji()))
            })
            .format_with("\n", |row, f| f(&row.iter().format_with("", |c, g| g(c))))
            .to_string()
    }

    fn card_back_mut(&mut self, coord: Coord) -> Option<&mut CardBack> {
        self.backs[coord].as_mut()
    }

    fn set_start(&mut self) {
        self.fixed.start = self.fixed.start.or_else(|| {
            self.backs
                .iter()
                .filter(|(_, card)| card.as_ref().is_some_and(|card| card.hint().is_logical()))
                .exactly_one()
                .ok()
                .map(|(coord, _)| coord)
        });
    }

    fn hints_by_coord(&self) -> impl Iterator<Item = (Coord, &str)> {
        self.backs
            .iter()
            .filter_map(|(coord, card)| Some((coord, card.as_ref()?.logical_hint()?)))
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
            let name = self.front(coord).name.clone();
            let message = format!("Is {name}'s ({coord}) hint, \"{hint}\", just flavor text?");
            if Confirm::new(&message).prompt()? {
                self.mark_as_flavor(coord)?;
            } else {
                hints.extend(Sentence::parse(&hint)?.add_context(self.context(coord))?);
            }
        }
        Ok(hints)
    }

    pub(crate) fn back(&self, coord: Coord) -> Option<&CardBack> {
        self.backs[coord].as_ref()
    }
}

impl Board<CardBack> {
    pub(crate) fn back(&self, coord: Coord) -> &CardBack {
        &self.backs[coord]
    }

    pub(crate) fn back_mut(&mut self, coord: Coord) -> &mut CardBack {
        &mut self.backs[coord]
    }
}

impl Serialize for Board {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        save::CardList::from(self).serialize(serializer)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct BoardFixed {
    fronts: Grid<CardFront>,
    coordinates: HashMap<Name, Coord>,
    start: Option<Coord>,
    // TODO maybe change this to `IndexMap` or `HashMap` once `mitsein` supports that
    by_profession: BTreeMap1<Profession, Set1>,
}

impl BoardFixed {
    fn context(&self, speaker: Coord) -> Context<'_> {
        Context {
            coordinates: &self.coordinates,
            by_profession: &self.by_profession,
            speaker,
        }
    }

    #[cfg(test)]
    fn coord(&self, name: &Name) -> Result<Coord> {
        self.coordinates
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("{name} not in grid"))
    }

    fn new(fronts: Grid<CardFront>, start: Option<Coord>) -> Self {
        let coordinates = fronts
            .iter()
            .map(|(coord, card)| (card.name.clone(), coord))
            .collect();
        let by_profession = fronts
            .iter()
            .map(|(coord, card)| (&card.profession, coord))
            .into_grouping_map()
            .aggregate(|set: Option<Set1>, _, coord| {
                let set = set.map_or_else(|| Set1::from_one(coord), |set| set | coord);
                Some(set)
            })
            .into_iter()
            .map(|(profession, set)| (profession.clone(), set))
            .try_collect1()
            .expect("total len 20");
        Self {
            fronts,
            coordinates,
            start,
            by_profession,
        }
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

        let mut cards: [(CardFront, Option<CardBack>, bool); 20] = cards
            .expect_children::<20>(Any)?
            .iter()
            .map(|card| parse_card(card))
            .collect::<Result<Vec<(CardFront, Option<CardBack>, bool)>>>()?
            .try_into()
            .unwrap_or_else(|_| unreachable!());

        // A valid puzzle must have at least one actual hint
        let format = if cards.iter().any(|&(_, _, has_hint)| has_hint) {
            // Can't be done at the same time as the `iter()` above since we only change something
            // if the current `has_hint` is `false` and *some* `has_hint` is `true`
            for (_, back, has_hint) in &mut cards {
                if !*has_hint && let Some(back) = back {
                    back.mark_as_flavor();
                }
            }
            Format::Sep2025
        } else {
            Format::Original
        };

        // TODO also parse a title and/or date
        let backs = Grid::from_flattened(cards.each_mut().map(|(_, back, _)| back.take()));
        let fronts = Grid::from_flattened(cards.map(|(front, _, _)| front));
        let board = Board::new(fronts, backs, None);
        Ok(Self { board, format })
    }
}

#[derive(Clone, Copy, Serialize, Deserialize, Debug, PartialEq, Eq)]
pub(crate) enum Format {
    Original,
    Sep2025,
}
