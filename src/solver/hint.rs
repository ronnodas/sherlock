mod parsers;
pub(crate) mod recipes;

use std::collections::HashSet;

use mitsein::array_vec1::ArrayVec1;
use mitsein::iter1::IntoIterator1 as _;
use mitsein::vec1::Vec1;

use super::grid::{Column, Coordinate, Direction, Row};
use super::solution::Solution;
use super::{Judgment, Profession};

pub(crate) type Set = HashSet<Coordinate>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Hint {
    Judgment(Coordinate, Judgment),
    Count(Set, Judgment, Quantity),
    Connected(Set, Judgment),
    Equal(Set, Set, Judgment),
    Bigger {
        big: Set,
        small: Set,
        judgment: Judgment,
    },
    UniqueWithCount(Vec1<Set>, Judgment, Quantity),
    Not(Box<Self>),
}

impl Hint {
    pub(crate) fn evaluate(&self, solution: &Solution) -> bool {
        match self {
            &Self::Judgment(coord, judgment) => solution[coord] == judgment,
            Self::Count(set, judgment, quantity) => {
                quantity.matches(solution.select(set, *judgment).count())
            }
            Self::Connected(set, judgment) => {
                Coordinate::connected(&solution.select(set, *judgment).collect())
            }
            Self::Equal(a, b, judgment) => {
                solution.select(a, *judgment).count() == solution.select(b, *judgment).count()
            }
            Self::Bigger {
                big,
                small,
                judgment,
            } => {
                solution.select(big, *judgment).count() > solution.select(small, *judgment).count()
            }
            Self::UniqueWithCount(sets, judgment, quantity) => {
                sets.iter()
                    .filter(|set| quantity.matches(solution.select(set, *judgment).count()))
                    .count()
                    == 1
            }
            Self::Not(hint) => !hint.evaluate(solution),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Line {
    Row(Row),
    Column(Column),
}

impl Line {
    const fn kind(self) -> LineKind {
        match self {
            Self::Row(_) => LineKind::Row,
            Self::Column(_) => LineKind::Column,
        }
    }

    fn others(self) -> Vec<Self> {
        match self {
            Self::Row(row) => row.others().map(Self::Row).collect(),
            Self::Column(column) => column.others().map(Self::Column).collect(),
        }
    }
}

impl From<Row> for Line {
    fn from(v: Row) -> Self {
        Self::Row(v)
    }
}

impl From<Column> for Line {
    fn from(v: Column) -> Self {
        Self::Column(v)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum LineKind {
    Row,
    Column,
}

impl LineKind {
    fn all(self) -> ArrayVec1<Line, 5> {
        match self {
            Self::Row => Row::ALL.map(Line::Row).into(),
            Self::Column => Column::ALL.map(Line::Column).into_iter1().collect1(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Quantity {
    Exact(u8),
    AtLeast(u8),
    Parity(Parity),
}

impl Quantity {
    pub(crate) fn matches(self, len: usize) -> bool {
        match self {
            Self::Exact(value) => len == usize::from(value),
            Self::AtLeast(value) => len >= usize::from(value),
            Self::Parity(parity) => parity.matches(len),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Parity {
    Even,
    Odd,
}

impl Parity {
    const fn matches(self, len: usize) -> bool {
        match self {
            Self::Even => len.is_multiple_of(2),
            Self::Odd => !len.is_multiple_of(2),
        }
    }
}
