mod parsers;
pub(crate) mod recipes;

use std::collections::HashSet;
use std::ops::Not;

use mitsein::array_vec1::ArrayVec1;
use mitsein::iter1::{IntoIterator1 as _, IteratorExt as _};
use mitsein::vec1::Vec1;

use super::grid::{Column, Coordinate, Direction, Row};
use super::solution::Solution;
use super::{Judgment, Profession};

pub(crate) type Set = HashSet<Coordinate>;
pub(crate) type Number = u8;
pub(crate) use parsers::Sentence;

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone)]
pub(crate) struct WithJudgment<T> {
    pub kind: T,
    pub judgment: Judgment,
}

impl<T> WithJudgment<Vec<T>> {
    pub(crate) fn spread(self) -> impl Iterator<Item = WithJudgment<T>> {
        self.kind.into_iter().map(move |kind| WithJudgment {
            kind,
            judgment: self.judgment,
        })
    }
}

pub(crate) type Hint = WithJudgment<HintKind>;

impl Hint {
    pub(crate) fn evaluate(&self, solution: &Solution) -> bool {
        self.kind.evaluate(self.judgment, solution)
    }
}

#[derive(Clone, Debug)]
pub(crate) enum HintKind {
    Judgment(Coordinate),
    Count(Set, Quantity),
    Connected(Set),
    Equal([Set; 2]),
    Bigger { big: Set, small: Set },
    Majority(Set),
    UniqueWithCount(Vec1<Set>, Quantity),
    Not(Box<Self>),
}

impl HintKind {
    fn evaluate(&self, judgment: Judgment, solution: &Solution) -> bool {
        match self {
            &Self::Judgment(coord) => solution[coord] == judgment,
            Self::Count(set, quantity) => quantity.matches(solution.select(set, judgment).count()),
            Self::Connected(set) => {
                Coordinate::connected(&solution.select(set, judgment).collect())
            }
            Self::Equal([a, b]) => {
                solution.select(a, judgment).count() == solution.select(b, judgment).count()
            }
            Self::Bigger { big, small } => {
                solution.select(big, judgment).count() > solution.select(small, judgment).count()
            }
            Self::UniqueWithCount(sets, quantity) => {
                sets.iter()
                    .filter(|set| quantity.matches(solution.select(set, judgment).count()))
                    .count()
                    == 1
            }
            Self::Majority(set) => {
                let count = solution.select(set, judgment).count();
                count > set.len() - count
            }
            Self::Not(hint) => !hint.evaluate(judgment, solution),
        }
    }
}

impl Not for HintKind {
    type Output = Self;

    fn not(self) -> Self {
        if let Self::Not(reverse) = self {
            *reverse
        } else {
            Self::Not(Box::new(self))
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Line {
    Row(Row),
    Column(Column),
}

impl Line {
    fn kind(self) -> LineKind {
        match self {
            Self::Row(_) => LineKind::Row,
            Self::Column(_) => LineKind::Column,
        }
    }

    fn others(self) -> Vec1<Self> {
        match self {
            Self::Row(row) => row.others().map(Self::Row).try_collect1().ok(),
            Self::Column(column) => column.others().map(Self::Column).try_collect1().ok(),
        }
        .unwrap_or_else(|| unreachable!())
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

impl From<Line> for Set {
    fn from(line: Line) -> Self {
        match line {
            Line::Row(row) => Coordinate::row_all(row).collect(),
            Line::Column(column) => Coordinate::column_all(column).collect(),
        }
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
    Exact(Number),
    AtLeast(Number),
    AtMost(Number),
    Parity(Parity),
}

impl Quantity {
    pub(crate) fn matches(self, len: usize) -> bool {
        match self {
            Self::Exact(value) => len == usize::from(value),
            Self::AtLeast(value) => len >= usize::from(value),
            Self::AtMost(value) => len <= usize::from(value),
            Self::Parity(parity) => parity.matches(len),
        }
    }
}

impl From<Parity> for Quantity {
    fn from(v: Parity) -> Self {
        Self::Parity(v)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Parity {
    Even,
    Odd,
}

impl Parity {
    fn matches(self, len: usize) -> bool {
        match self {
            Self::Even => len.is_multiple_of(2),
            Self::Odd => !len.is_multiple_of(2),
        }
    }
}
