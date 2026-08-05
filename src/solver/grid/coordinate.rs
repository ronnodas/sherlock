use std::error::Error;
use std::iter::successors;
use std::ops::{BitAnd, BitOr};
use std::str::FromStr;
use std::{cmp, fmt};

use anyhow::{Result, anyhow};
use bitvec::order::Lsb0;
use bitvec::view::BitView as _;
use itertools::Itertools as _;
use mitsein::iter1::{IntoIterator1, Iterator1};
use mitsein::vec1::{Vec1, vec1};
use serde_with::{DeserializeFromStr, SerializeDisplay};

use crate::solver::Judgment;
use crate::solver::hint::Line;

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

    pub(crate) fn row_all(row: Row) -> impl Iterator<Item = Self> {
        Column::ALL.into_iter().map(move |col| Self { row, col })
    }

    pub(crate) fn column_all(col: Column) -> impl Iterator<Item = Self> {
        Row::ALL.into_iter().map(move |row| Self { row, col })
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

    pub(crate) fn between([a, b]: [Self; 2]) -> Result<Set> {
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

//TODO custom Debug
#[derive(Clone, Copy, Debug)]
pub(crate) struct Set(u32);

impl Set {
    const CONNECTED: &[u8; 1 << 17] = include_bytes!("connected.bin");

    pub(crate) fn connected(self) -> bool {
        let index: usize = self.0.try_into().expect("Self::CONNECTED fits into memory");
        Self::CONNECTED.view_bits::<Lsb0>()[index]
    }

    pub(crate) fn contains(self, coord: Coordinate) -> bool {
        self.0 & (1 << coord.to_index()) != 0
    }

    pub(crate) fn len(self) -> usize {
        self.0.count_ones().try_into().expect("at most 20")
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        self.into_iter()
            .filter_map(|coord| coord.step(direction))
            .collect()
    }

    pub(crate) fn judged(self, judgment: Judgment) -> ModifiedSet {
        ModifiedSet::Modified(Box::new(self.into()), Modifier::Judgment(judgment))
    }

    pub(crate) fn empty() -> Self {
        Self(0)
    }

    pub(crate) fn complement(self) -> Self {
        Self(((1 << 20) - 1) ^ self.0)
    }
}

impl BitAnd<Self> for Set {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl FromIterator<Coordinate> for Set {
    fn from_iter<T: IntoIterator<Item = Coordinate>>(iter: T) -> Self {
        let bits = iter
            .into_iter()
            .fold(0, |set, coord| set | (1 << coord.to_index()));
        Self(bits)
    }
}

impl IntoIterator for Set {
    type Item = Coordinate;

    type IntoIter = SetIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        SetIntoIter { bits: self.0 }
    }
}

impl From<Set1> for Set {
    fn from(set: Set1) -> Self {
        Self(set.0)
    }
}

pub(crate) struct SetIntoIter {
    bits: u32,
}

impl Iterator for SetIntoIter {
    type Item = Coordinate;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bits == 0 {
            return None;
        }
        let index = self.bits.trailing_zeros();
        self.bits ^= 1 << index;
        Some(Coordinate::from_index(
            index.try_into().expect("at most 20"),
        ))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.bits.count_ones().try_into().expect("at most 20");
        (len, Some(len))
    }
}

impl ExactSizeIterator for SetIntoIter {}

//TODO custom Debug
#[derive(Clone, Copy, Debug)]
pub(crate) struct Set1(u32);

impl Set1 {
    pub(crate) fn from_one(coord: Coordinate) -> Self {
        Self(1 << coord.to_index())
    }
}

impl BitOr<Coordinate> for Set1 {
    type Output = Self;

    fn bitor(self, rhs: Coordinate) -> Self::Output {
        Self(self.0 | (1 << rhs.to_index()))
    }
}

impl TryFrom<Set> for Set1 {
    type Error = Set;

    fn try_from(set: Set) -> Result<Self, Self::Error> {
        if set.0 == 0 {
            Err(set)
        } else {
            Ok(Self(set.0))
        }
    }
}

impl IntoIterator for Set1 {
    type Item = Coordinate;

    type IntoIter = SetIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        SetIntoIter { bits: self.0 }
    }
}

impl IntoIterator1 for Set1 {
    #[expect(
        unsafe_code,
        reason = "There is no other way to infallibly create an Iterator1<SetIntoIter>"
    )]
    fn into_iter1(self) -> Iterator1<Self::IntoIter> {
        // SAFETY
        unsafe { Iterator1::from_iter_unchecked(self) }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum ModifiedSet {
    Empty,
    Regular(Set),
    Modified(Box<Self>, Modifier),
    Intersection(Vec1<Self>),
}

impl ModifiedSet {
    #[must_use]
    pub(crate) fn as_regular(&self) -> Option<Set> {
        if let &Self::Regular(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub(crate) fn judged(self, judgment: Judgment) -> Self {
        match self {
            Self::Modified(this, Modifier::Judgment(other)) if other == judgment => {
                Self::Modified(this, judgment.into())
            }
            Self::Modified(_, Modifier::Judgment(_)) | Self::Empty => Self::Empty,
            Self::Regular(_) | Self::Modified(_, Modifier::Shift(_)) | Self::Intersection(_) => {
                Self::Modified(Box::new(self), judgment.into())
            }
        }
    }

    pub(crate) fn intersect(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Empty, _) | (_, Self::Empty) => Self::Empty,

            (Self::Regular(this), Self::Regular(rhs)) => this
                .into_iter()
                .filter(|&coord| rhs.contains(coord))
                .collect(),
            (this, Self::Modified(rhs, Modifier::Judgment(judgment)))
            | (Self::Modified(rhs, Modifier::Judgment(judgment)), this) => {
                this.intersect(*rhs).judged(judgment)
            }

            (Self::Modified(this, Modifier::Shift(a)), Self::Modified(rhs, Modifier::Shift(b)))
                if a == b =>
            {
                this.intersect(*rhs).shift(a)
            }
            (
                this @ (Self::Regular(..) | Self::Modified(..)),
                rhs @ (Self::Modified(..) | Self::Regular(..)),
            ) => Self::Intersection(vec1![this, rhs]),
            (this @ (Self::Regular(..) | Self::Modified(..)), Self::Intersection(mut vec))
            | (Self::Intersection(mut vec), this @ (Self::Regular(..) | Self::Modified(..))) => {
                vec.push(this);
                Self::Intersection(vec)
            }
            (Self::Intersection(mut this), Self::Intersection(rhs)) => {
                this.extend(rhs);
                Self::Intersection(this)
            }
        }
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        match self {
            Self::Empty => Self::Empty,
            Self::Regular(set) => Self::Regular(set.shift(direction)),
            set @ Self::Modified(..) => Self::Modified(Box::new(set), direction.into()),
            Self::Intersection(vec) => vec
                .into_iter1()
                .map(|set| set.shift(direction))
                .reduce(Self::intersect),
        }
    }
}

impl From<Set> for ModifiedSet {
    fn from(v: Set) -> Self {
        Self::Regular(v)
    }
}

impl From<Set1> for ModifiedSet {
    fn from(set: Set1) -> Self {
        Self::Regular(set.into())
    }
}

impl From<Line> for ModifiedSet {
    fn from(line: Line) -> Self {
        Self::Regular(line.into())
    }
}

impl FromIterator<Coordinate> for ModifiedSet {
    fn from_iter<T: IntoIterator<Item = Coordinate>>(iter: T) -> Self {
        Self::Regular(iter.into_iter().collect())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Modifier {
    Shift(Direction),
    Judgment(Judgment),
}

impl From<Direction> for Modifier {
    fn from(v: Direction) -> Self {
        Self::Shift(v)
    }
}

impl From<Judgment> for Modifier {
    fn from(v: Judgment) -> Self {
        Self::Judgment(v)
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
    pub(crate) fn from_index(index: usize) -> Self {
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

    pub(crate) fn prev(self) -> Option<Self> {
        match self {
            Self::One => None,
            Self::Two => Some(Self::One),
            Self::Three => Some(Self::Two),
            Self::Four => Some(Self::Three),
            Self::Five => Some(Self::Four),
        }
    }

    pub(crate) fn next(self) -> Option<Self> {
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

    pub(crate) fn parse(row: char) -> Option<Self> {
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

    pub(crate) fn prev(self) -> Option<Self> {
        match self {
            Self::A => None,
            Self::B => Some(Self::A),
            Self::C => Some(Self::B),
            Self::D => Some(Self::C),
        }
    }

    pub(crate) fn next(self) -> Option<Self> {
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

    pub(crate) fn parse(col: char) -> Option<Self> {
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

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    #[test]
    fn coordinate_all_order() {
        let coords = Coordinate::all().into_iter().collect_vec();
        assert_eq!(coords.len(), 20);
        assert_eq!(coords[0].to_string(), "A1");
        assert_eq!(coords[1].to_string(), "B1");
        assert_eq!(coords[2].to_string(), "C1");
        assert_eq!(coords[3].to_string(), "D1");
        assert_eq!(coords[4].to_string(), "A2");
        assert_eq!(coords[19].to_string(), "D5");
    }
}
