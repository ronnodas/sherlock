use mitsein::array_vec1::ArrayVec1;
use mitsein::iter1::{IntoIterator1 as _, IteratorExt as _};
use mitsein::vec1::Vec1;

use crate::models::{Column, Coordinate, Judgment, Row};
use crate::solver::grid::coordinates::{ModifiedSet, Set};
use crate::solver::solution::Solution;

mod parsers;
pub(crate) mod recipes;

pub(crate) type Number = u8;
pub(crate) use parsers::Sentence;

#[derive(Clone, Debug)]
pub(crate) enum Hint {
    /// Given coordinate has given judgment
    Judgment(Coordinate, Judgment),
    /// Given set of coordinates has that many suspects
    Count(ModifiedSet, Cardinal),
    /// Given set of coordinates does not have that many suspects
    NotCount(ModifiedSet, Cardinal),
    /// Given set of coordinates in total have that many suspects
    CountTotal([ModifiedSet; 2], Cardinal),
    /// Given set of coordinates is connected
    Connected(ModifiedSet),
    /// The first set compares with the second set
    CompareSets([ModifiedSet; 2], Comparison),
    /// Among the given `sets`, `count` many have `each` suspects
    CountWithCount {
        sets: Vec<ModifiedSet>,
        count: Cardinal,
        each: Cardinal,
    },
    /// Each member of the given set has a given number of neighbors with the given judgment
    EachNeighbors(ModifiedSet, Cardinal, Judgment),
    /// `count` many members of the given set has `each` neighbors with given judgment
    CountWithNeighbors {
        set: ModifiedSet,
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
            Self::CountWithCount { sets, count, each } => count.matches(
                sets.iter()
                    .filter(|set| each.matches(solution.select(set).len()))
                    .count(),
            ),
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
                        let neighbors = solution
                            .select(&coord.neighbors().collect::<ModifiedSet>().judged(*judgment))
                            .len();
                        each.matches(neighbors)
                    })
                    .count();
                count.matches(counted)
            }
            Self::EachNeighbors(set, cardinal, judgment) => {
                solution.select(set).into_iter().all(|coord| {
                    let neighbors = solution
                        .select(&coord.neighbors().collect::<ModifiedSet>().judged(*judgment))
                        .len();
                    cardinal.matches(neighbors)
                })
            }
        }
    }

    fn unique_with_count(sets: Vec1<ModifiedSet>, quantity: Cardinal) -> Self {
        Self::CountWithCount {
            sets: sets.into_vec(),
            each: quantity,
            count: Cardinal::Exact(1),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum Comparison {
    Equal,
    MoreThan(Option<Number>),
}

impl Comparison {
    fn compare(self, lhs: usize, rhs: usize) -> bool {
        match self {
            Self::Equal => lhs == rhs,
            Self::MoreThan(excess) => {
                excess.map_or(lhs > rhs, |excess| lhs == rhs + usize::from(excess))
            }
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
pub(crate) enum Cardinal {
    Exact(Number),
    AtLeast(Number),
    AtMost(Number),
    Parity(Parity),
}

impl Cardinal {
    pub(crate) fn matches(self, len: usize) -> bool {
        match self {
            Self::Exact(value) => len == usize::from(value),
            Self::AtLeast(value) => len >= usize::from(value),
            Self::AtMost(value) => len <= usize::from(value),
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
    fn matches(self, len: usize) -> bool {
        match self {
            Self::Even => len.is_multiple_of(2),
            Self::Odd => !len.is_multiple_of(2),
        }
    }
}
