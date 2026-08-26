use std::cmp::Ordering;
use std::fmt;
use std::num::NonZero;
use std::ops::{BitAnd, BitOr};

use anyhow::{Context as _, Result, anyhow};
use bitvec::order::Lsb0;
use bitvec::view::BitView as _;
use itertools::Itertools as _;
use mitsein::iter1::{FromIterator1, IntoIterator1, Iterator1};
use mitsein::vec1::{Vec1, vec1};

use crate::models::{Column, Coord, Direction, Row};
pub(crate) use crate::set1;
use crate::solver::Judgment;
use crate::solver::hint::{Hint, Line};

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct Set(u32);

impl Set {
    const CONNECTED: &[u8; 1 << 17] = include_bytes!("connected.bin");

    pub(crate) fn between([a, b]: [Coord; 2]) -> Result<Self> {
        if a.row == b.row {
            Ok(Column::between([a.col, b.col])
                .map(|col| Coord { row: a.row, col })
                .collect())
        } else if a.col == b.col {
            Ok(Row::between([a.row, b.row])
                .map(|row| Coord { row, col: a.col })
                .collect())
        } else {
            Err(anyhow!("{a} and {b} not on the same line"))
        }
    }

    pub(crate) fn connected(self) -> bool {
        let index: usize = self.0.try_into().expect("Self::CONNECTED fits into memory");
        Self::CONNECTED.view_bits::<Lsb0>()[index]
    }

    pub(crate) fn len(self) -> u8 {
        self.0.count_ones().try_into().expect("at most 20")
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        self.into_iter()
            .filter_map(|coord| coord.step(direction))
            .collect()
    }

    pub(crate) fn empty() -> Self {
        Self(0)
    }

    pub(crate) fn complement(self) -> Self {
        Self(((1 << 20) - 1) ^ self.0)
    }

    pub(crate) fn non_empty(self) -> Option<Set1> {
        NonZero::new(self.0).map(Set1)
    }
}

impl Default for Set {
    fn default() -> Self {
        Self::empty()
    }
}

impl PartialOrd for Set {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let intersection = *self & *other;
        match [&intersection == self, &intersection == other] {
            [true, true] => Some(Ordering::Equal),
            [true, false] => Some(Ordering::Less),
            [false, true] => Some(Ordering::Greater),
            [false, false] => None,
        }
    }

    fn le(&self, other: &Self) -> bool {
        self.0 & other.0 == self.0
    }

    fn ge(&self, other: &Self) -> bool {
        self.0 & other.0 == other.0
    }
}

impl BitAnd<Self> for Set {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl BitOr<Self> for Set {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl FromIterator<Coord> for Set {
    fn from_iter<T: IntoIterator<Item = Coord>>(iter: T) -> Self {
        let mut this = Self::empty();
        this.extend(iter);
        this
    }
}

impl Extend<Coord> for Set {
    fn extend<T: IntoIterator<Item = Coord>>(&mut self, iter: T) {
        let bits = iter
            .into_iter()
            .fold(self.0, |set, coord| set | (1 << coord.to_index()));
        *self = Self(bits);
    }
}

impl IntoIterator for Set {
    type Item = Coord;

    type IntoIter = SetIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        SetIntoIter { bits: self.0 }
    }
}

impl From<Set1> for Set {
    fn from(set: Set1) -> Self {
        Self(set.0.get())
    }
}

impl fmt::Debug for Set {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(*self).finish()
    }
}

pub(crate) struct SetIntoIter {
    bits: u32,
}

impl Iterator for SetIntoIter {
    type Item = Coord;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bits == 0 {
            return None;
        }
        let index = self.bits.trailing_zeros();
        self.bits ^= 1 << index;
        Some(Coord::from_index(index.try_into().expect("at most 20")))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.bits.count_ones().try_into().expect("at most 20");
        (len, Some(len))
    }
}

impl ExactSizeIterator for SetIntoIter {}

//TODO custom Debug
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct Set1(NonZero<u32>);

impl Set1 {
    pub(crate) fn from_one(coord: Coord) -> Self {
        Self(NonZero::new(1 << coord.to_index()).expect("no overflow"))
    }

    pub(crate) fn len(self) -> NonZero<u8> {
        self.0.count_ones().try_into().expect("at most 20")
    }

    pub(crate) fn contains(self, coord: Coord) -> bool {
        self.0.get() & (1 << coord.to_index()) != 0
    }

    pub(crate) fn shift(self, direction: Direction) -> Set {
        self.into_iter()
            .filter_map(|coord| coord.step(direction))
            .collect()
    }

    pub(crate) fn judged(self, judgment: Judgment) -> ModifiedSet1 {
        ModifiedSet1::Modified(Box::new(ModifiedSet1::Regular(self)), judgment.into())
    }
}

impl PartialOrd for Set1 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Set::from(*self).partial_cmp(&Set::from(*other))
    }

    fn le(&self, other: &Self) -> bool {
        self.0.get() & other.0.get() == self.0.get()
    }

    fn ge(&self, other: &Self) -> bool {
        self.0.get() & other.0.get() == other.0.get()
    }
}

impl BitOr<Coord> for Set1 {
    type Output = Self;

    fn bitor(self, rhs: Coord) -> Self::Output {
        Self(self.0 | (1 << rhs.to_index()))
    }
}

impl BitOr<Set> for Set1 {
    type Output = Self;

    fn bitor(self, rhs: Set) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl IntoIterator for Set1 {
    type Item = Coord;

    type IntoIter = SetIntoIter;

    fn into_iter(self) -> Self::IntoIter {
        SetIntoIter { bits: self.0.get() }
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

impl FromIterator1<Coord> for Set1 {
    fn from_iter1<I>(items: I) -> Self
    where
        I: IntoIterator1<Item = Coord>,
    {
        let (head, tail) = items.into_iter1().into_head_and_tail();
        Self::from_one(head) | Set::from_iter(tail)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum ModifiedSet {
    Empty,
    NonEmpty(ModifiedSet1),
}

impl ModifiedSet {
    pub(crate) fn judged(self, judgment: Judgment) -> Self {
        if let Self::NonEmpty(set) = self {
            set.judged(judgment)
        } else {
            Self::Empty
        }
    }

    pub(crate) fn intersect(self, rhs: Self) -> Self {
        if let Self::NonEmpty(rhs) = rhs {
            self.intersect1(rhs)
        } else {
            Self::Empty
        }
    }

    pub(crate) fn intersect1(self, rhs: ModifiedSet1) -> Self {
        if let Self::NonEmpty(set) = self {
            set.intersect(rhs)
        } else {
            Self::Empty
        }
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        if let Self::NonEmpty(set) = self {
            set.shift(direction)
        } else {
            Self::Empty
        }
    }

    pub(crate) fn from_regular(set: Set) -> Self {
        set.non_empty()
            .map_or(Self::Empty, |set| Self::NonEmpty(set.into()))
    }
}

impl From<ModifiedSet1> for ModifiedSet {
    fn from(set: ModifiedSet1) -> Self {
        Self::NonEmpty(set)
    }
}

impl From<Set> for ModifiedSet {
    fn from(v: Set) -> Self {
        Self::from_regular(v)
    }
}

impl From<Set1> for ModifiedSet {
    fn from(set: Set1) -> Self {
        Self::NonEmpty(set.into())
    }
}

impl From<Line> for ModifiedSet {
    fn from(line: Line) -> Self {
        Self::from_regular(line.into())
    }
}

impl FromIterator<Coord> for ModifiedSet {
    fn from_iter<T: IntoIterator<Item = Coord>>(iter: T) -> Self {
        Self::from_regular(iter.into_iter().collect())
    }
}

#[derive(Clone, Debug)]
pub(crate) enum ModifiedSet1 {
    Regular(Set1),
    Modified(Box<Self>, Modifier),
    Intersection(Vec1<Self>),
}

impl ModifiedSet1 {
    pub(crate) fn judged(self, judgment: Judgment) -> ModifiedSet {
        match self {
            Self::Modified(this, Modifier::Judgment(other)) if other == judgment => {
                Self::Modified(this, judgment.into()).into()
            }
            Self::Modified(_, Modifier::Judgment(_)) => ModifiedSet::Empty,
            Self::Regular(_) | Self::Modified(_, Modifier::Shift(_)) | Self::Intersection(_) => {
                Self::Modified(Box::new(self), judgment.into()).into()
            }
        }
    }

    pub(crate) fn intersect(self, rhs: Self) -> ModifiedSet {
        match (self, rhs) {
            (Self::Regular(this), Self::Regular(rhs)) => this
                .into_iter()
                .filter(|&coord| rhs.contains(coord))
                .collect(),
            (this, Self::Modified(rhs, Modifier::Judgment(judgment)))
            | (Self::Modified(rhs, Modifier::Judgment(judgment)), this) => {
                this.intersect(*rhs).judged(judgment)
            }

            // TODO only for some cases of this, otherwise not well-founded
            // (this, ModifiedSet1::Modified(rhs, Modifier::Shift(dir)))
            // | (ModifiedSet1::Modified(rhs, Modifier::Shift(dir)), this) => {
            //     this.shift(dir.flip()).intersect1(*rhs).shift(dir)
            // }
            (Self::Modified(this, Modifier::Shift(a)), Self::Modified(rhs, Modifier::Shift(b)))
                if a == b =>
            {
                this.intersect(*rhs).shift(a)
            }
            (
                this @ (Self::Regular(..) | Self::Modified(..)),
                rhs @ (Self::Modified(..) | Self::Regular(..)),
            ) => Self::Intersection(vec1![this, rhs]).into(),
            (this @ (Self::Regular(..) | Self::Modified(..)), Self::Intersection(mut vec))
            | (Self::Intersection(mut vec), this @ (Self::Regular(..) | Self::Modified(..))) => {
                vec.push(this);
                Self::Intersection(vec).into()
            }
            (Self::Intersection(mut this), Self::Intersection(rhs)) => {
                this.extend(rhs);
                Self::Intersection(this).into()
            }
        }
    }

    pub(crate) fn shift(self, direction: Direction) -> ModifiedSet {
        match self {
            Self::Regular(set) => ModifiedSet::from_regular(set.shift(direction)),
            set @ Self::Modified(..) => Self::Modified(Box::new(set), direction.into()).into(),
            Self::Intersection(vec) => vec
                .into_iter1()
                .map(|set| set.shift(direction))
                .reduce(ModifiedSet::intersect),
        }
    }

    #[must_use]
    pub(crate) fn as_regular(&self) -> Option<&Set1> {
        if let Self::Regular(set) = self {
            Some(set)
        } else {
            None
        }
    }

    pub(crate) fn conditions_to_contain(&self, coord: Coord) -> Result<Vec<Hint>> {
        match self {
            Self::Regular(set) => {
                if set.contains(coord) {
                    Ok(Vec::new())
                } else {
                    Err(anyhow!("{self:?} does not contain {coord}"))
                }
            }
            Self::Modified(inner, Modifier::Judgment(judgment)) => {
                let mut hints = inner.conditions_to_contain(coord)?;
                hints.push(Hint::Judgment(coord, *judgment));
                Ok(hints)
            }
            Self::Modified(inner, Modifier::Shift(direction)) => {
                let coord = coord
                    .step(direction.flip())
                    .with_context(|| format!("{coord} is not {direction:?} of anything"))?;
                inner.conditions_to_contain(coord)
            }
            Self::Intersection(sets) => sets
                .into_iter()
                .map(|set| set.conditions_to_contain(coord))
                .flatten_ok()
                .collect(),
        }
    }
}

impl From<Set1> for ModifiedSet1 {
    fn from(set: Set1) -> Self {
        Self::Regular(set)
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

#[macro_export]
macro_rules! set1 {
    ($c:tt $r:tt) => {
        Set1::from_one(coord!($c $r))
    };

    // Recursive / iterative case:
    // Matches the first pair, followed by `|`, and then a repetition of remaining pairs
    ($c:tt $r:tt | $($rest_c:tt $rest_r:tt)|+) => {
        Set1::from_one(coord!($c $r)) $(| coord!($rest_c $rest_r))+
    };}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coordinate_all_order() {
        let coords = Coord::all().into_iter().collect_vec();
        assert_eq!(coords.len(), 20);
        assert_eq!(coords[0].to_string(), "A1");
        assert_eq!(coords[1].to_string(), "B1");
        assert_eq!(coords[2].to_string(), "C1");
        assert_eq!(coords[3].to_string(), "D1");
        assert_eq!(coords[4].to_string(), "A2");
        assert_eq!(coords[19].to_string(), "D5");
    }
}
