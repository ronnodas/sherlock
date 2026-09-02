use std::ops::Not;

use mitsein::array_vec1::ArrayVec1;
use mitsein::iter1::{IntoIterator1 as _, IteratorExt as _};
use mitsein::vec1::Vec1;

use crate::models::{Column, Coord, Judgment, Row};
use crate::solver::board::coordinates::{Set, Set1, Set1Expr, Set1Op};
use crate::solver::solution::Solution;

mod parsers;
pub(crate) mod recipes;

pub(crate) type Number = u8;
pub(crate) use parsers::Sentence;

#[derive(Clone, Debug)]
pub(crate) enum Hint {
    /// Given coordinate has given judgment
    Judgment(Coord, Judgment),
    /// Given set of coordinates has that many suspects
    Count(Set1Op, Cardinal),
    /// Given set of coordinates does not have that many suspects
    NotCount(Set1Op, Cardinal),
    /// Given set of coordinates in total have that many suspects
    CountTotal([Set1Op; 2], Cardinal),
    /// Given set of coordinates is connected
    Connected(Set1Op),
    /// The first set compares with the second set
    CompareSets([Set1Op; 2], Comparison),
    /// Among the given `sets`, `count` many have `each` suspects
    UniqueWithCount { sets: Vec1<Set1Op>, count: Cardinal },
    /// Each member of the given set has a given number of neighbors with the given judgment
    EachNeighbors(Set1Expr, Cardinal, Judgment),
    /// `count` many members of the given set has `each` neighbors with given judgment
    CountWithNeighbors {
        set: Set1Expr,
        each: Cardinal,
        count: Cardinal,
        judgment: Judgment,
    },
}

impl Hint {
    pub(crate) fn evaluate(&self, solution: &Solution) -> bool {
        match self {
            &Self::Judgment(coord, judgment) => solution[coord] == judgment,
            Self::Count(set, quantity) => quantity.matches(solution.select(set).len()),
            Self::NotCount(set, quantity) => !quantity.matches(solution.select(set).len()),
            Self::CountTotal(sets, quantity) => {
                let total = sets.iter().map(|set| solution.select(set).len()).sum();
                quantity.matches(total)
            }
            Self::Connected(set) => solution.select(set).connected(),
            Self::CompareSets(sets, comparison) => {
                let [lhs, rhs] = sets.each_ref().map(|set| solution.select(set).len());
                comparison.compare(lhs, rhs)
            }
            Self::UniqueWithCount { sets, count } => {
                sets.iter()
                    .filter(|&set| count.matches(solution.select(set).len()))
                    .count()
                    == 1
            }
            Self::CountWithNeighbors {
                set,
                each,
                count,
                judgment,
            } => {
                let counted = solution
                    .select(set)
                    .into_iter()
                    .filter(|coord| {
                        let neighbors = solution.select(&coord.neighbors().judged(*judgment)).len();
                        each.matches(neighbors)
                    })
                    .collect::<Set>()
                    .len();
                count.matches(counted)
            }
            Self::EachNeighbors(set, cardinal, judgment) => {
                solution.select(set).into_iter().all(|coord| {
                    let neighbors = solution.select(&coord.neighbors().judged(*judgment)).len();
                    cardinal.matches(neighbors)
                })
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Comparison {
    ExactDifference(Number),
    More,
}

impl Comparison {
    fn compare(self, lhs: Number, rhs: Number) -> bool {
        match self {
            Self::ExactDifference(excess) => lhs == rhs + excess,
            Self::More => lhs > rhs,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Line {
    Row(Row),
    Column(Column),
}

impl Line {
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

impl From<Line> for Set1 {
    fn from(line: Line) -> Self {
        match line {
            Line::Row(row) => row.all().into_iter1().collect1(),
            Line::Column(column) => column.all().into_iter1().collect1(),
        }
    }
}

impl From<Line> for Set {
    fn from(line: Line) -> Self {
        match line {
            Line::Row(row) => row.all().into_iter().collect(),
            Line::Column(column) => column.all().into_iter().collect(),
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
pub(crate) enum Cardinal {
    Exact(Number),
    AtLeast(Number),
    AtMost(Number),
    Parity(Parity),
}

impl Cardinal {
    pub(crate) fn matches(self, len: Number) -> bool {
        match self {
            Self::Exact(value) => len == value,
            Self::AtLeast(value) => len >= value,
            Self::AtMost(value) => len <= value,
            Self::Parity(parity) => parity.matches(len),
        }
    }
}

impl From<Parity> for Cardinal {
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
    pub(crate) fn matches(self, len: Number) -> bool {
        match self {
            Self::Even => len.is_multiple_of(2),
            Self::Odd => !len.is_multiple_of(2),
        }
    }
}

impl Not for Parity {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Even => Self::Odd,
            Self::Odd => Self::Even,
        }
    }
}
