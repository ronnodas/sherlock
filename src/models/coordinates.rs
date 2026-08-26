#![expect(unsafe_code, reason = "external derive macro")]

use std::cmp::Ordering;
use std::error::Error;
use std::str::FromStr;
use std::{fmt, iter};

use itertools::Itertools as _;
use linearize::Linearize;
use mitsein::iter1::{IntoIterator1 as _, Iterator1};
use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::solver::board::coordinates::{Set1, set1};

macro_rules! coord {
    ($c:ident $r:tt) => {
        Coord {
            row: row!($r),
            col: col!($c),
        }
    };
}

macro_rules! row {
    (1) => {
        Row::One
    };
    (2) => {
        Row::Two
    };
    (3) => {
        Row::Three
    };
    (4) => {
        Row::Four
    };
    (5) => {
        Row::Five
    };
}

macro_rules! col {
    (A) => {
        Column::A
    };
    (B) => {
        Column::B
    };
    (C) => {
        Column::C
    };
    (D) => {
        Column::D
    };
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, SerializeDisplay, DeserializeFromStr)]
pub(crate) struct Coord {
    pub row: Row,
    pub col: Column,
}

impl Coord {
    pub(crate) fn from_index(index: usize) -> Self {
        Self {
            row: Row::from_index(index / 4),
            col: Column::from_index(index % 4),
        }
    }

    pub(crate) const fn to_index(self) -> usize {
        4 * self.row.to_index() + self.col.to_index()
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
        iter::successors(start.step(direction), move |coord| coord.step(direction))
    }

    pub(crate) fn neighbors(self) -> Set1 {
        match self {
            coord!(A 1) => set1!(B 1 | A 2 | B 2),
            coord!(B 1) => set1!(A 1 | C 1 | A 2 | B 2 | C 2),
            coord!(C 1) => set1!(B 1 | D 1 | B 2 | C 2 | D 2),
            coord!(D 1) => set1!(C 1 | C 2 | D 2),
            coord!(A 2) => set1!(A 1 | B 1 | B 2 | A 3 | B 3),
            coord!(B 2) => set1!(A 1 | B 1 | C 1 | A 2 | C 2 | A 3 | B 3 | C 3),
            coord!(C 2) => set1!(B 1 | C 1 | D 1 | B 2 | D 2 | B 3 | C 3 | D 3),
            coord!(D 2) => set1!(C 1 | D 1 | C 2 | C 3 | D 3),
            coord!(A 3) => set1!(A 2 | B 2 | B 3 | A 4 | B 4),
            coord!(B 3) => set1!(A 2 | B 2 | C 2 | A 3 | C 3 | A 4 | B 4 | C 4),
            coord!(C 3) => set1!(B 2 | C 2 | D 2 | B 3 | D 3 | B 4 | C 4 | D 4),
            coord!(D 3) => set1!(C 2 | D 2 | C 3 | C 4 | D 4),
            coord!(A 4) => set1!(A 3 | B 3 | B 4 | A 5 | B 5),
            coord!(B 4) => set1!(A 3 | B 3 | C 3 | A 4 | C 4 | A 5 | B 5 | C 5),
            coord!(C 4) => set1!(B 3 | C 3 | D 3 | B 4 | D 4 | B 5 | C 5 | D 5),
            coord!(D 4) => set1!(C 3 | D 3 | C 4 | C 5 | D 5),
            coord!(A 5) => set1!(A 4 | B 4 | B 5),
            coord!(B 5) => set1!(A 4 | B 4 | C 4 | A 5 | C 5),
            coord!(C 5) => set1!(B 4 | C 4 | D 4 | B 5 | D 5),
            coord!(D 5) => set1!(C 4 | D 4 | C 5),
            }
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

    pub(crate) fn parse(string: &str) -> Option<Self> {
        let [col, row] = string.chars().collect_array()?;
        Some({
            Self {
                row: Row::parse(row)?,
                col: Column::parse(col)?,
            }
        })
    }

    pub(crate) fn all() -> Iterator1<impl Iterator<Item = Self>> {
        // TODO replace with `cartesian_product()`
        Row::ALL.into_iter1().flat_map(Row::all)
    }

    pub(crate) fn as_tuple(self) -> (usize, usize) {
        (self.row.to_index(), self.col.to_index())
    }
}

impl fmt::Display for Coord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.col, self.row)
    }
}

impl fmt::Debug for Coord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.col, self.row)
    }
}

impl FromStr for Coord {
    type Err = ParseCoordinateError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s).ok_or(ParseCoordinateError)
    }
}

impl Ord for Coord {
    fn cmp(&self, other: &Self) -> Ordering {
        self.row.cmp(&other.row).then(self.col.cmp(&other.col))
    }
}

impl PartialOrd for Coord {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Linearize)]
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

    pub(crate) const fn to_index(self) -> usize {
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

    pub(crate) fn all(self) -> [Coord; 4] {
        Column::ALL.map(move |col| Coord { row: self, col })
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

    pub(crate) fn between(mut pair: [Self; 2]) -> impl Iterator<Item = Self> {
        pair.sort_unstable();
        let [a, b] = pair;
        iter::successors(a.next(), |r| r.next()).take_while(move |&r| r != b)
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord, Linearize)]
pub(crate) enum Column {
    A,
    B,
    C,
    D,
}

impl Column {
    pub(crate) const ALL: [Self; 4] = [Self::A, Self::B, Self::C, Self::D];

    pub(crate) fn from_index(index: usize) -> Self {
        match index {
            0 => Self::A,
            1 => Self::B,
            2 => Self::C,
            3 => Self::D,
            4.. => unreachable!(),
        }
    }

    pub(crate) const fn to_index(self) -> usize {
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

    pub(crate) fn all(self) -> [Coord; 5] {
        Row::ALL.map(move |row| Coord { row, col: self })
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

    pub(crate) fn between(mut pair: [Self; 2]) -> impl Iterator<Item = Self> {
        pair.sort_unstable();
        let [a, b] = pair;
        iter::successors(a.next(), |r| r.next()).take_while(move |&r| r != b)
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
    pub(crate) fn flip(self) -> Self {
        match self {
            Self::Above => Self::Below,
            Self::Below => Self::Above,
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }
}
