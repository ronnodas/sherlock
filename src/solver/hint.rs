use std::ops::{BitXor, Not};

use mitsein::array_vec1::ArrayVec1;
use mitsein::iter1::{IntoIterator1 as _, IteratorExt as _};
use mitsein::vec1::Vec1;
use strum::EnumDiscriminants;

use crate::models::{Column, Coord, Judgment, Row, Solution};
use crate::solver::board::coordinates::{Set, Set1, SetExpr1, SetOp1};

mod parsers;
pub(crate) mod recipes;

pub(crate) type Number = u8;
pub(crate) use parsers::Sentence;

#[derive(Clone, Debug)]
pub(crate) enum Hint {
    /// Given coordinate has given judgment
    Judgment(Coord, Judgment),
    /// Given set of coordinates has that many suspects
    Count(SetOp1, BoundOrNot),
    /// Given set of coordinates in total have that many suspects
    CountTotal([SetOp1; 2], Bound),
    /// Given set of coordinates is connected
    Connected(SetOp1),
    /// The first set compares with the second set
    CompareSets([SetOp1; 2], Comparison),
    /// Among the given `sets`, exactly one has count matching `bound`
    UniqueWithCount { sets: Vec1<SetOp1>, bound: Bound },
    /// Each member of the given set has a given number of neighbors with the given judgment
    EachNeighbors(SetExpr1, Bound, Judgment),
    /// `count` many members of the given set has `each` neighbors with given judgment
    CountWithNeighbors {
        set: SetExpr1,
        each: Bound,
        count: Bound,
        judgment: Judgment,
    },
}

impl Hint {
    pub(crate) fn evaluate(&self, solution: &Solution) -> bool {
        match self {
            &Self::Judgment(coord, judgment) => solution[coord] == judgment,
            Self::Count(set, bound) => bound.matches(solution.select(set).len()),
            Self::CountTotal(sets, bound) => {
                let total = sets.iter().map(|set| solution.select(set).len()).sum();
                bound.matches(total)
            }
            Self::Connected(set) => solution.select(set).connected(),
            Self::CompareSets(sets, comparison) => {
                let [lhs, rhs] = sets.each_ref().map(|set| solution.select(set).len());
                comparison.compare(lhs, rhs)
            }
            Self::UniqueWithCount { sets, bound } => {
                sets.iter()
                    .filter(|&set| bound.matches(solution.select(set).len()))
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
            Self::EachNeighbors(set, bound, judgment) => {
                solution.select(set).into_iter().all(|coord| {
                    let neighbors = solution.select(&coord.neighbors().judged(*judgment)).len();
                    bound.matches(neighbors)
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
            Self::ExactDifference(excess) => lhs == rhs.strict_add(excess),
            Self::More => lhs > rhs,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, EnumDiscriminants)]
#[strum_discriminants(name(LineKind))]
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

impl LineKind {
    fn all(self) -> ArrayVec1<Line, 5> {
        match self {
            Self::Row => Row::ALL.map(Line::Row).into(),
            Self::Column => Column::ALL.map(Line::Column).into_iter1().collect1(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Bound {
    Exact(Number),
    AtLeast(Number),
    AtMost(Number),
    Parity(Parity),
}

impl Bound {
    pub(crate) fn matches(self, len: Number) -> bool {
        match self {
            Self::Exact(value) => len == value,
            Self::AtLeast(value) => len >= value,
            Self::AtMost(value) => len <= value,
            Self::Parity(parity) => parity.matches(len),
        }
    }

    pub(crate) fn not(self) -> Option<BoundOrNot> {
        let bound = match self {
            Self::Exact(0) => BoundOrNot::AtLeast(1),
            Self::Exact(value) => BoundOrNot::NotExact(value),
            Self::AtLeast(value) => BoundOrNot::AtMost(value.checked_sub(1)?),
            Self::AtMost(value) => BoundOrNot::AtLeast(value.strict_add(1)),
            Self::Parity(parity) => BoundOrNot::Parity(!parity),
        };
        Some(bound)
    }
}

impl From<Parity> for Bound {
    fn from(v: Parity) -> Self {
        Self::Parity(v)
    }
}

impl From<Number> for Bound {
    fn from(value: Number) -> Self {
        Self::Exact(value)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Parity {
    Even,
    Odd,
}

impl Parity {
    pub(crate) fn matches(self, len: Number) -> bool {
        self == Self::of(len)
    }

    pub(crate) fn of(number: Number) -> Self {
        if number.is_multiple_of(2) {
            Self::Even
        } else {
            Self::Odd
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

impl BitXor for Parity {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        if self == rhs { Self::Even } else { Self::Odd }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum BoundOrNot {
    Exact(Number),
    AtLeast(Number),
    AtMost(Number),
    NotExact(Number),
    Parity(Parity),
}

impl BoundOrNot {
    pub(crate) fn matches(self, len: Number) -> bool {
        match self {
            Self::Exact(value) => len == value,
            Self::AtLeast(value) => len >= value,
            Self::AtMost(value) => len <= value,
            Self::NotExact(value) => len != value,
            Self::Parity(parity) => parity.matches(len),
        }
    }
}

impl From<Bound> for BoundOrNot {
    fn from(value: Bound) -> Self {
        match value {
            Bound::Exact(value) => Self::Exact(value),
            Bound::AtLeast(value) => Self::AtLeast(value),
            Bound::AtMost(value) => Self::AtMost(value),
            Bound::Parity(parity) => Self::Parity(parity),
        }
    }
}

impl From<Parity> for BoundOrNot {
    fn from(v: Parity) -> Self {
        Self::Parity(v)
    }
}
