use std::cmp::Ordering;
use std::fmt;
use std::num::NonZero;
use std::ops::{BitAnd, BitOr, BitOrAssign, BitXor};

use anyhow::{Result, anyhow};
use bitvec::order::Lsb0;
use bitvec::view::BitView as _;
use mitsein::iter1::{FromIterator1, IntoIterator1, Iterator1};

use crate::macros::set1;
use crate::models::{Column, Coord, Direction, Judged, Row};
use crate::solver::Judgment;

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
        Self(Set1::ALL_BITS.get() ^ self.0)
    }

    pub(crate) fn non_empty(self) -> Option<Set1> {
        NonZero::new(self.0).map(Set1)
    }

    // TODO could be Iterator1
    pub(crate) fn power_set(self) -> impl Iterator<Item = Self> {
        (0..=self.0).map(Self).filter(move |sub| sub <= &self)
    }

    /// elements that are in self but not in rhs
    pub(crate) fn difference(self, rhs: impl Into<Self>) -> Self {
        Self(self.0 & !(rhs.into().0))
    }

    pub(crate) fn judged(self, judgment: Judgment) -> Judged<Self> {
        Judged(self, judgment)
    }

    pub(crate) fn contains(self, coord: Coord) -> bool {
        self.0 & (1 << coord.to_index()) != 0
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

impl BitOrAssign<Coord> for Set {
    fn bitor_assign(&mut self, rhs: Coord) {
        self.0 |= 1 << rhs.to_index();
    }
}

impl BitXor for Set {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.0 ^ rhs.0)
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Set1(NonZero<u32>);

impl Set1 {
    const ALL_BITS: NonZero<u32> = const { NonZero::new((1 << 20_u32) - 1).unwrap() };

    pub(crate) fn from_one(coord: Coord) -> Self {
        Self(NonZero::new(1 << coord.to_index()).expect("no overflow"))
    }

    pub(crate) fn all() -> Self {
        Self(Self::ALL_BITS)
    }

    pub(crate) fn edges() -> Self {
        set1!(A 1 | B 1 | C 1 | D 1 | A 2 | D 2 | A 3 | D 3 | A 4 | D 4 | A 5 | B 5 | C 5 | D 5)
    }

    pub(crate) fn corners() -> Self {
        set1!(A 1 | D 1 | A 5 | D 5)
    }

    pub(crate) fn len(self) -> NonZero<u8> {
        self.0.count_ones().try_into().expect("at most 20")
    }

    pub(crate) fn contains(self, coord: Coord) -> bool {
        self.0.get() & (1 << coord.to_index()) != 0
    }

    pub(crate) fn judged(self, judgment: Judgment) -> Judged<Self> {
        Judged(self, judgment)
    }

    pub(crate) fn complement(self) -> Set {
        Set::from(self).complement()
    }

    pub(crate) fn assert_contains(self, coord: Coord) -> Result<()> {
        if self.contains(coord) {
            Ok(())
        } else {
            Err(anyhow!("{self:?} does not contain {coord}"))
        }
    }

    // TODO could be Iterator1
    pub(crate) fn power_set(self) -> impl Iterator<Item = Set> {
        Set::from(self).power_set()
    }

    /// elements that are in self but not in rhs
    pub(crate) fn difference(self, rhs: impl Into<Set>) -> Set {
        Set::from(self).difference(rhs)
    }

    pub(crate) fn total_cmp(self, b: Self) -> Ordering {
        self.0.cmp(&b.0)
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

impl BitAnd for Set1 {
    type Output = Set;

    fn bitand(self, rhs: Self) -> Self::Output {
        Set::from(self) & Set::from(rhs)
    }
}

impl BitAnd<Set> for Set1 {
    type Output = Set;

    fn bitand(self, rhs: Set) -> Self::Output {
        Set::from(self) & rhs
    }
}

impl BitOr for Set1 {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
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

impl BitXor for Set1 {
    type Output = Set;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Set::from(self) ^ Set::from(rhs)
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
        // SAFETY yields self.0.count_ones() many items
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

impl fmt::Debug for Set1 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(*self).finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn coordinate_all_order() {
        let coords: Vec<_> = Coord::all().into_iter().collect();
        assert_eq!(coords.len(), 20);
        assert_eq!(coords[0].to_string(), "A1");
        assert_eq!(coords[1].to_string(), "B1");
        assert_eq!(coords[2].to_string(), "C1");
        assert_eq!(coords[3].to_string(), "D1");
        assert_eq!(coords[4].to_string(), "A2");
        assert_eq!(coords[19].to_string(), "D5");
    }
}
