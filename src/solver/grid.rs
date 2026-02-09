mod card;
mod html;

use std::collections::{HashMap, HashSet};
use std::iter::successors;
use std::ops::{Index, IndexMut};

use anyhow::{Result, anyhow, bail};
use itertools::Itertools as _;
use mitsein::NonEmpty;
use mitsein::hash_set1::HashSet1;
use select::document::Document;
use select::predicate::{Attr, Predicate as _};

use super::hint::Set;
use super::{Judgment, Name, Profession};
use card::Card;
use html::{Class, ClassName, Div, NodeExt as _};

#[derive(Clone, Debug)]
pub(crate) struct Grid {
    cards: [Card; 20],
    coordinates: HashMap<Name, Coordinate>,
    by_profession: HashMap<Profession, NonEmpty<Set>>,
}

impl Grid {
    pub(crate) fn parse(html: &str) -> Result<Self> {
        let html = Document::from(html);
        let Ok(cards) = html
            .find(Div.and(Class(ClassName::CardGrid)).and(Attr("id", "grid")))
            .exactly_one()
        else {
            bail!("expecting unique element");
        };
        let cards: [Card; 20] = cards
            .expect_children::<20>()?
            .iter()
            .map(|card| Card::parse(card))
            .collect::<Result<Vec<Card>>>()?
            .try_into()
            .unwrap_or_else(|_| unreachable!());
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
        Ok(Self {
            cards,
            coordinates,
            by_profession,
        })
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
        self.cards.iter().all(Card::is_judged)
    }

    pub(crate) fn fixed(&self) -> [Option<Judgment>; 20] {
        self.cards.each_ref().map(Card::status)
    }

    pub(crate) fn set_new(&mut self, index: usize, judgment: Judgment) -> Option<&Card> {
        self.cards[index].set(judgment)
    }

    pub(crate) fn by_profession(&self, profession: &Profession) -> Result<&NonEmpty<Set>> {
        self.by_profession
            .get(profession)
            .ok_or_else(|| anyhow!("{profession} not in grid"))
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

    pub(crate) const fn to_index(self) -> usize {
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

    pub(crate) fn neighbors(center: Self) -> impl Iterator<Item = Self> {
        use Direction::{Above, Below, Left, Right};
        [center.step(Above), center.step(Below)]
            .into_iter()
            .flatten()
            .flat_map(|vert| [Some(vert), vert.step(Right), vert.step(Left)])
            .chain([center.step(Left), center.step(Right)])
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
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
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

    const fn to_index(self) -> usize {
        match self {
            Self::One => 0,
            Self::Two => 1,
            Self::Three => 2,
            Self::Four => 3,
            Self::Five => 4,
        }
    }

    const fn prev(self) -> Option<Self> {
        match self {
            Self::One => None,
            Self::Two => Some(Self::One),
            Self::Three => Some(Self::Two),
            Self::Four => Some(Self::Three),
            Self::Five => Some(Self::Four),
        }
    }

    const fn next(self) -> Option<Self> {
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
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
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

    const fn to_index(self) -> usize {
        match self {
            Self::A => 0,
            Self::B => 1,
            Self::C => 2,
            Self::D => 3,
        }
    }

    const fn prev(self) -> Option<Self> {
        match self {
            Self::A => None,
            Self::B => Some(Self::A),
            Self::C => Some(Self::B),
            Self::D => Some(Self::C),
        }
    }

    const fn next(self) -> Option<Self> {
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
