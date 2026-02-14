mod card;
mod html;
mod save;

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::iter::successors;
use std::ops::{Index, IndexMut};
use std::str::FromStr;
use std::{cmp, fmt};

use anyhow::{Result, anyhow, bail};
use itertools::Itertools as _;
use mitsein::NonEmpty;
use mitsein::hash_set1::HashSet1;
use mitsein::iter1::{IntoIterator1 as _, Iterator1, IteratorExt as _};
use mitsein::vec1::Vec1;
use select::document::Document;
use select::predicate::{Any, Attr, Predicate as _};
use serde::{Deserialize, Serialize};
use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::puzzle::grid::card::CardBack;
use crate::puzzle::hint::Set;
use crate::puzzle::{Judgment, Name, Profession};

use card::Card;
use html::{Class, ClassName, Div, NodeExt as _};

#[derive(Clone, Debug, Deserialize)]
#[serde(from = "save::CardList")]
pub(crate) struct Grid {
    cards: [Card; 20],
    coordinates: HashMap<Name, Coordinate>,
    // TODO make this non-empty once mitsein supports that
    by_profession: HashMap<Profession, NonEmpty<Set>>,
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
            .aggregate(|set: Option<NonEmpty<Set>>, _, item| {
                let set = set.map_or_else(
                    || HashSet1::from_one(item),
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

    pub(crate) fn coord(&self, name: &Name) -> Result<Coordinate> {
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

    pub(crate) fn _by_profession(&self) -> &HashMap<Profession, NonEmpty<Set>> {
        &self.by_profession
    }

    pub(crate) fn set_start(&mut self) {
        self.start = self.start.or_else(|| {
            self.cards
                .iter()
                .enumerate()
                .filter(|(_, card)| card.hint().is_some())
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SerializeDisplay, DeserializeFromStr)]
pub(crate) struct Coordinate {
    row: Row,
    col: Column,
}

impl Coordinate {
    fn from_index(index: usize) -> Self {
        Self {
            row: Row::from_index(index / 4),
            col: Column::from_index(index % 4),
        }
    }

    pub(crate) fn to_index(self) -> usize {
        4 * self.row.to_index() + self.col.to_index()
    }

    pub(crate) fn row_all(row: Row) -> impl Iterator<Item = Self> {
        Column::ALL.into_iter().map(move |col| Self { row, col })
    }

    pub(crate) fn column_all(col: Column) -> impl Iterator<Item = Self> {
        Row::ALL.into_iter().map(move |row| Self { row, col })
    }

    pub(crate) fn connected(set: &HashSet<Self>) -> bool {
        if set.len() == 1 {
            return true;
        }
        let Some(&start) = set.iter().next() else {
            return true;
        };
        let mut seen = HashSet::new();
        let mut frontier = vec![start];
        while let Some(node) = frontier.pop() {
            if !seen.insert(node) {
                continue;
            }
            frontier.extend(
                Direction::ALL
                    .into_iter()
                    .filter_map(|dir| node.step(dir))
                    .filter(|node| set.contains(node)),
            );
        }
        seen.len() == set.len()
    }

    pub(crate) fn step(self, direction: Direction) -> Option<Self> {
        let coord = match direction {
            Direction::Above => Self {
                row: self.row.prev()?,
                col: self.col,
            },
            Direction::Below => Self {
                row: self.row.next()?,
                col: self.col,
            },
            Direction::Left => Self {
                row: self.row,
                col: self.col.prev()?,
            },
            Direction::Right => Self {
                row: self.row,
                col: self.col.next()?,
            },
        };
        Some(coord)
    }

    pub(crate) fn direction(start: Self, direction: Direction) -> impl Iterator<Item = Self> {
        successors(start.step(direction), move |coord| coord.step(direction))
    }

    pub(crate) fn neighbors(self) -> impl Iterator<Item = Self> {
        use Direction::{Above, Below, Left, Right};
        [self.step(Above), self.step(Below)]
            .into_iter()
            .flatten()
            .flat_map(|vert| [Some(vert), vert.step(Right), vert.step(Left)])
            .chain([self.step(Left), self.step(Right)])
            .flatten()
    }

    pub(crate) fn edges() -> impl Iterator<Item = Self> {
        [Column::A, Column::D]
            .into_iter()
            .cartesian_product(Row::ALL)
            .chain(
                [Column::B, Column::C]
                    .into_iter()
                    .cartesian_product([Row::One, Row::Five]),
            )
            .map(|(col, row)| Self { row, col })
    }

    pub(crate) fn corners() -> impl Iterator<Item = Self> {
        [Row::One, Row::Five]
            .into_iter()
            .cartesian_product([Column::A, Column::D])
            .map(|(row, col)| Self { row, col })
    }

    fn parse(string: &str) -> Option<Self> {
        let [col, row] = string.chars().collect_array()?;
        Some({
            Self {
                row: Row::parse(row)?,
                col: Column::parse(col)?,
            }
        })
    }

    pub(crate) fn between([a, b]: [Self; 2]) -> Result<HashSet<Self>> {
        if a.row == b.row {
            Ok(Column::between([a.col, b.col])
                .map(|col| Self { row: a.row, col })
                .collect())
        } else if a.col == b.col {
            Ok(Row::between([a.row, b.row])
                .map(|row| Self { row, col: a.col })
                .collect())
        } else {
            Err(anyhow!("{a} and {b} not on the same line"))
        }
    }

    pub(crate) fn all() -> Iterator1<impl Iterator<Item = Self>> {
        // TODO replace with `cartesian_product()`
        Row::ALL
            .into_iter1()
            .flat_map(|row| Column::ALL.into_iter1().map(move |col| Self { row, col }))
    }
}

impl fmt::Display for Coordinate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.col, self.row)
    }
}

impl FromStr for Coordinate {
    type Err = ParseCoordinateError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s).ok_or(ParseCoordinateError)
    }
}

impl Ord for Coordinate {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        self.row.cmp(&other.row).then(self.col.cmp(&other.col))
    }
}

impl PartialOrd for Coordinate {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub(crate) struct ParseCoordinateError;

impl fmt::Display for ParseCoordinateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "string does not represent a grid coordinate")
    }
}

impl Error for ParseCoordinateError {}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub(crate) enum Row {
    One,
    Two,
    Three,
    Four,
    Five,
}

impl Row {
    pub(crate) const ALL: [Self; 5] = [Self::One, Self::Two, Self::Three, Self::Four, Self::Five];
    fn from_index(index: usize) -> Self {
        match index {
            0 => Self::One,
            1 => Self::Two,
            2 => Self::Three,
            3 => Self::Four,
            4 => Self::Five,
            5.. => unreachable!(),
        }
    }

    fn to_index(self) -> usize {
        match self {
            Self::One => 0,
            Self::Two => 1,
            Self::Three => 2,
            Self::Four => 3,
            Self::Five => 4,
        }
    }

    fn prev(self) -> Option<Self> {
        match self {
            Self::One => None,
            Self::Two => Some(Self::One),
            Self::Three => Some(Self::Two),
            Self::Four => Some(Self::Three),
            Self::Five => Some(Self::Four),
        }
    }

    fn next(self) -> Option<Self> {
        match self {
            Self::One => Some(Self::Two),
            Self::Two => Some(Self::Three),
            Self::Three => Some(Self::Four),
            Self::Four => Some(Self::Five),
            Self::Five => None,
        }
    }

    pub(crate) fn others(&self) -> impl Iterator<Item = Self> {
        Self::ALL.into_iter().filter(move |other| other != self)
    }

    fn parse(row: char) -> Option<Self> {
        let row = match row {
            '1' => Self::One,
            '2' => Self::Two,
            '3' => Self::Three,
            '4' => Self::Four,
            '5' => Self::Five,
            _ => return None,
        };
        Some(row)
    }

    fn between(mut pair: [Self; 2]) -> impl Iterator<Item = Self> {
        pair.sort_unstable();
        let [a, b] = pair;
        successors(a.next(), |r| r.next()).take_while(move |&r| r != b)
    }
}

impl fmt::Display for Row {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = match self {
            Self::One => '1',
            Self::Two => '2',
            Self::Three => '3',
            Self::Four => '4',
            Self::Five => '5',
        };
        write!(f, "{c}")
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
pub(crate) enum Column {
    A,
    B,
    C,
    D,
}

impl Column {
    pub(crate) const ALL: [Self; 4] = [Self::A, Self::B, Self::C, Self::D];

    fn from_index(index: usize) -> Self {
        match index {
            0 => Self::A,
            1 => Self::B,
            2 => Self::C,
            3 => Self::D,
            4.. => unreachable!(),
        }
    }

    fn to_index(self) -> usize {
        match self {
            Self::A => 0,
            Self::B => 1,
            Self::C => 2,
            Self::D => 3,
        }
    }

    fn prev(self) -> Option<Self> {
        match self {
            Self::A => None,
            Self::B => Some(Self::A),
            Self::C => Some(Self::B),
            Self::D => Some(Self::C),
        }
    }

    fn next(self) -> Option<Self> {
        match self {
            Self::A => Some(Self::B),
            Self::B => Some(Self::C),
            Self::C => Some(Self::D),
            Self::D => None,
        }
    }

    pub(crate) fn others(&self) -> impl Iterator<Item = Self> {
        Self::ALL.into_iter().filter(move |other| other != self)
    }

    fn parse(col: char) -> Option<Self> {
        let col = match col {
            'A' => Self::A,
            'B' => Self::B,
            'C' => Self::C,
            'D' => Self::D,
            _ => return None,
        };
        Some(col)
    }

    fn between(mut pair: [Self; 2]) -> impl Iterator<Item = Self> {
        pair.sort_unstable();
        let [a, b] = pair;
        successors(a.next(), |r| r.next()).take_while(move |&r| r != b)
    }
}

impl fmt::Display for Column {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let c = match self {
            Self::A => 'A',
            Self::B => 'B',
            Self::C => 'C',
            Self::D => 'D',
        };
        write!(f, "{c}")
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Direction {
    Above,
    Below,
    Left,
    Right,
}

impl Direction {
    pub(crate) const ALL: [Self; 4] = [Self::Above, Self::Below, Self::Left, Self::Right];
}
