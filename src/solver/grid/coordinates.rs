use std::ops::{BitAnd, BitOr};

use anyhow::{Result, anyhow};
use bitvec::order::Lsb0;
use bitvec::view::BitView as _;
use mitsein::iter1::{IntoIterator1, Iterator1};
use mitsein::vec1::{Vec1, vec1};

use crate::models::{Column, Coordinate, Direction, Row};
use crate::solver::Judgment;
use crate::solver::hint::Line;

//TODO custom Debug
#[derive(Clone, Copy, Debug)]
pub(crate) struct Set(u32);

impl Set {
    const CONNECTED: &[u8; 1 << 17] = include_bytes!("connected.bin");

    pub(crate) fn between([a, b]: [Coordinate; 2]) -> Result<Self> {
        if a.row == b.row {
            Ok(Column::between([a.col, b.col])
                .map(|col| Coordinate { row: a.row, col })
                .collect())
        } else if a.col == b.col {
            Ok(Row::between([a.row, b.row])
                .map(|row| Coordinate { row, col: a.col })
                .collect())
        } else {
            Err(anyhow!("{a} and {b} not on the same line"))
        }
    }

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

#[cfg(test)]
mod tests {
    use itertools::Itertools as _;

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
