use std::cmp::Ordering;
use std::fmt;
use std::num::NonZero;
use std::ops::{BitAnd, BitOr};

use anyhow::{Context as _, Result, anyhow, bail};
use bitvec::order::Lsb0;
use bitvec::view::BitView as _;
use itertools::Itertools as _;
use mitsein::iter1::{FromIterator1, IntoIterator1, Iterator1};
use mitsein::vec1::{Vec1, vec1};

use crate::macros::set1;
use crate::models::{Column, Coord, Direction, Row, SetEval, Solution};
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
        Self(Set1::ALL_BITS.get() ^ self.0)
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

    pub(crate) fn shift_preimage(direction: Direction) -> Self {
        Self::all()
            .shift(direction.flip())
            .non_empty()
            .expect("each shift is non-trivial")
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

    pub(crate) fn judged(self, judgment: Judgment) -> Set1Op {
        Set1Op::Judged(Box::new(Set1Expr::Regular(self)), judgment)
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
pub(crate) enum SetExpr {
    Empty,
    NonEmpty(Set1Expr),
}

impl SetExpr {
    pub(crate) fn judged(self, judgment: Judgment) -> SetOp {
        if let Self::NonEmpty(set) = self {
            set.judged(judgment)
        } else {
            SetOp::Empty
        }
    }

    pub(crate) fn intersect(self, rhs: Self) -> Self {
        if let Self::NonEmpty(rhs) = rhs {
            self.intersect1(rhs)
        } else {
            Self::Empty
        }
    }

    pub(crate) fn intersect1(self, rhs: Set1Expr) -> Self {
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

    pub(crate) fn regular(self) -> Result<Set, Set1Op> {
        match self {
            Self::Empty => Ok(Set::empty()),
            Self::NonEmpty(Set1Expr::Regular(set)) => Ok(set.into()),
            Self::NonEmpty(Set1Expr::Op(set)) => Err(set),
        }
    }
}

impl From<Set1Expr> for SetExpr {
    fn from(set: Set1Expr) -> Self {
        Self::NonEmpty(set)
    }
}

impl From<Set> for SetExpr {
    fn from(v: Set) -> Self {
        Self::from_regular(v)
    }
}

impl From<Set1> for SetExpr {
    fn from(set: Set1) -> Self {
        Self::NonEmpty(set.into())
    }
}

impl From<Line> for SetExpr {
    fn from(line: Line) -> Self {
        Self::from_regular(line.into())
    }
}

impl SetEval for SetExpr {
    fn eval(&self, solution: &Solution) -> Set {
        match self {
            Self::Empty => Set::empty(),
            Self::NonEmpty(set) => set.eval(solution),
        }
    }
}

impl FromIterator<Coord> for SetExpr {
    fn from_iter<T: IntoIterator<Item = Coord>>(iter: T) -> Self {
        Self::from_regular(iter.into_iter().collect())
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Set1Expr {
    Regular(Set1),
    Op(Set1Op),
}

impl Set1Expr {
    pub(crate) fn judged(self, judgment: Judgment) -> SetOp {
        match self {
            Self::Op(set) => set.judged(judgment),
            Self::Regular(_) => Set1Op::Judged(Box::new(self), judgment).into(),
        }
    }

    pub(crate) fn intersect(self, rhs: Self) -> SetExpr {
        match (self, rhs) {
            (Self::Regular(this), Self::Regular(rhs)) => this
                .into_iter()
                .filter(|&coord| rhs.contains(coord))
                .collect(),
            (Self::Regular(set), Self::Op(op)) | (Self::Op(op), Self::Regular(set)) => {
                op.intersect_set(set).into()
            }
            (Self::Op(this), Self::Op(rhs)) => this.intersect(rhs).into(),
        }
    }

    pub(crate) fn shift(self, direction: Direction) -> SetExpr {
        match self {
            Self::Regular(set) => SetExpr::from_regular(set.shift(direction)),
            Self::Op(op) => op.shift(direction).into(),
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
            Self::Op(op) => op.conditions_to_contain(coord),
        }
    }
}

impl From<Set1> for Set1Expr {
    fn from(set: Set1) -> Self {
        Self::Regular(set)
    }
}

impl From<Set1Op> for Set1Expr {
    fn from(v: Set1Op) -> Self {
        Self::Op(v)
    }
}

impl SetEval for Set1Expr {
    fn eval(&self, solution: &Solution) -> Set {
        match self {
            &Self::Regular(set) => set.into(),
            Self::Op(set1_op) => set1_op.eval(solution),
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum Set1Op {
    Judged(Box<Set1Expr>, Judgment),
    Shift(Box<Self>, Direction),
    // the `Option` could be replaced by `Set1::all()` but this seems semantically better
    Intersection(Vec1<Self>, Option<Set1>),
}

impl Set1Op {
    pub(crate) fn judged(self, judgment: Judgment) -> SetOp {
        match self {
            Self::Judged(this, other) if other == judgment => Self::Judged(this, other).into(),
            Self::Judged(_, _) => SetOp::Empty,
            Self::Shift(_, _) | Self::Intersection(..) => {
                Self::Judged(Box::new(self.into()), judgment).into()
            }
        }
    }

    pub(crate) fn shift(self, direction: Direction) -> SetOp {
        match self {
            Self::Judged(..) | Self::Shift(..) => {
                let inner = self.intersect_set(Set1::shift_preimage(direction));
                if let SetOp::NonEmpty(inner) = inner {
                    Self::Shift(Box::new(inner), direction).into()
                } else {
                    SetOp::Empty
                }
            }
            Self::Intersection(vec, fixed) => {
                let intersection = vec
                    .into_iter1()
                    .map(|set| set.shift(direction))
                    .reduce(SetOp::intersect);
                if let Some(fixed) = fixed {
                    fixed
                        .shift(direction)
                        .non_empty()
                        .map_or(SetOp::Empty, |fixed| intersection.intersect_set(fixed))
                } else {
                    intersection
                }
            }
        }
    }

    fn intersect_set(self, rhs: Set1) -> SetOp {
        match self {
            Self::Judged(this, judgment) => (*this).intersect(rhs.into()).judged(judgment),
            Self::Shift(this, direction) => {
                let Some(pre_shift) = rhs.shift(direction.flip()).non_empty() else {
                    return SetOp::Empty;
                };
                (*this).intersect_set(pre_shift).shift(direction)
            }
            Self::Intersection(vec, fixed) => {
                let fixed = match fixed {
                    Some(fixed) => match (fixed & rhs).non_empty() {
                        Some(fixed) => fixed,
                        None => return SetOp::Empty,
                    },
                    None => rhs,
                };
                Self::Intersection(vec, Some(fixed)).into()
            }
        }
    }

    fn intersect(self, rhs: Self) -> SetOp {
        match [self, rhs] {
            [this, Self::Judged(rhs, judgment)] | [Self::Judged(rhs, judgment), this] => match *rhs
            {
                Set1Expr::Regular(rhs) => this.intersect_set(rhs),
                Set1Expr::Op(rhs) => this.intersect(rhs),
            }
            .judged(judgment),

            [Self::Shift(this, a), Self::Shift(rhs, b)] if a == b => this.intersect(*rhs).shift(a),
            [this @ Self::Shift(..), rhs @ Self::Shift(..)] => {
                Self::Intersection(vec1![this, rhs], None).into()
            }
            // TODO this may not be well-founded
            [
                Self::Intersection(vec, fixed),
                rhs @ (Self::Shift(..) | Self::Intersection(..)),
            ]
            | [rhs @ Self::Shift(..), Self::Intersection(vec, fixed)] => {
                let intersection = vec.into_iter().fold(SetOp::from(rhs), |intersection, set| {
                    intersection.intersect(set.into())
                });
                if let Some(fixed) = fixed {
                    intersection.intersect_set(fixed)
                } else {
                    intersection
                }
            }
        }
    }

    pub(crate) fn conditions_to_contain(&self, coord: Coord) -> Result<Vec<Hint>> {
        match self {
            Self::Judged(inner, judgment) => {
                let mut hints = inner.conditions_to_contain(coord)?;
                hints.push(Hint::Judgment(coord, *judgment));
                Ok(hints)
            }
            Self::Shift(inner, direction) => {
                let coord = coord
                    .step(direction.flip())
                    .with_context(|| format!("{coord} is not {direction:?} of anything"))?;
                inner.conditions_to_contain(coord)
            }
            Self::Intersection(vec, fixed) => {
                if let Some(fixed) = fixed
                    && !fixed.contains(coord)
                {
                    bail!("{self:?} does not contain {coord}")
                }
                vec.into_iter()
                    .map(|set| set.conditions_to_contain(coord))
                    .flatten_ok()
                    .collect()
            }
        }
    }
}

impl From<Set1Op> for SetExpr {
    fn from(set: Set1Op) -> Self {
        Self::NonEmpty(set.into())
    }
}

impl SetEval for Set1Op {
    fn eval(&self, solution: &Solution) -> Set {
        match self {
            Self::Judged(inner, judgment) => inner
                .eval(solution)
                .into_iter()
                .filter(move |&coord| &solution[coord] == judgment)
                .collect(),
            Self::Shift(inner, direction) => inner.eval(solution).shift(*direction),
            Self::Intersection(vec, fixed) => {
                let intersection = vec
                    .into_iter1()
                    .map(|set| set.eval(solution))
                    .reduce(|a, b| a & b);
                if let &Some(fixed) = fixed {
                    fixed & intersection
                } else {
                    intersection
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum SetOp {
    Empty,
    NonEmpty(Set1Op),
}

impl SetOp {
    fn intersect(self, rhs: Self) -> Self {
        if let [Self::NonEmpty(this), Self::NonEmpty(rhs)] = [self, rhs] {
            this.intersect(rhs)
        } else {
            Self::Empty
        }
    }

    fn intersect_set(self, rhs: Set1) -> Self {
        match self {
            Self::Empty => Self::Empty,
            Self::NonEmpty(set) => set.intersect_set(rhs),
        }
    }

    fn shift(self, direction: Direction) -> Self {
        match self {
            Self::Empty => Self::Empty,
            Self::NonEmpty(set) => set.shift(direction),
        }
    }

    fn judged(self, judgment: Judgment) -> Self {
        match self {
            Self::Empty => Self::Empty,
            Self::NonEmpty(set) => set.judged(judgment),
        }
    }
}

impl From<Set1Op> for SetOp {
    fn from(v: Set1Op) -> Self {
        Self::NonEmpty(v)
    }
}

impl From<SetOp> for SetExpr {
    fn from(set: SetOp) -> Self {
        match set {
            SetOp::Empty => Self::Empty,
            SetOp::NonEmpty(set) => set.into(),
        }
    }
}

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
