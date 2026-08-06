#![expect(unsafe_code, reason = "external derive macro")]

use std::cmp::Ordering;
use std::error::Error;
use std::str::FromStr;
use std::{fmt, iter};

use itertools::Itertools as _;
use linearize::Linearize;
use mitsein::iter1::{IntoIterator1 as _, Iterator1};
use serde_with::{DeserializeFromStr, SerializeDisplay};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, SerializeDisplay, DeserializeFromStr)]
pub(crate) struct Coordinate {
    pub row: Row,
    pub col: Column,
}

impl Coordinate {
    pub(crate) fn from_index(index: usize) -> Self {
        Self {
            row: Row::from_index(index / 4),
            col: Column::from_index(index % 4),
        }
    }

    pub(crate) fn to_index(self) -> usize {
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

    // TODO return Iterator1
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
    fn cmp(&self, other: &Self) -> Ordering {
        self.row.cmp(&other.row).then(self.col.cmp(&other.col))
    }
}

impl PartialOrd for Coordinate {
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

    pub(crate) fn to_index(self) -> usize {
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

    pub(crate) fn all(self) -> [Coordinate; 4] {
        Column::ALL.map(move |col| Coordinate { row: self, col })
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

    pub(crate) fn to_index(self) -> usize {
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

    pub(crate) fn all(self) -> [Coordinate; 5] {
        Row::ALL.map(move |row| Coordinate { row, col: self })
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
