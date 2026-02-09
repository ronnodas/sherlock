mod parsers;
pub(crate) mod recipes;

use std::collections::HashSet;

use mitsein::array_vec1::ArrayVec1;
use mitsein::iter1::IntoIterator1 as _;
use mitsein::vec1::{Vec1, vec1};

use crate::solver::Profession;
use crate::solver::hint::recipes::SetRecipe;

use super::{Column, Coordinate, Judgment, Row, Solution};

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
            &Self::Judgment(coord, judgment) => solution[coord.to_index()] == judgment,
            Self::Count(set, judgment, quantity) => {
                quantity.matches(judgment.filter(set, solution).count())
            }
            Self::Connected(set, judgment) => {
                Coordinate::connected(&judgment.filter(set, solution).collect())
            }
            Self::Equal(a, b, judgment) => {
                judgment.filter(a, solution).count() == judgment.filter(b, solution).count()
            }
            Self::Bigger {
                big,
                small,
                judgment,
            } => judgment.filter(big, solution).count() > judgment.filter(small, solution).count(),
            Self::UniqueWithCount(sets, judgment, quantity) => {
                sets.iter()
                    .filter(|set| quantity.matches(judgment.filter(set, solution).count()))
                    .count()
                    == 1
            }
            Self::Not(hint) => !hint.evaluate(solution),
        }
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) enum Unit {
    Direction(Direction, recipes::NameRecipe),
    Line(Line),
    Profession(Profession),
    ProfessionShift(Profession, Direction),
    Neighbor(recipes::NameRecipe),
    Edges,
    Quantified(Box<Self>, Quantity),
    Corners,
}

impl Unit {
    fn and(self, other: Self) -> SetRecipe {
        SetRecipe::Intersection(vec1![self, other])
    }

    fn quantify(self, quantity: Quantity) -> Self {
        Self::Quantified(Box::new(self), quantity)
    }

    fn maybe_quantify(self, quantity: Option<Quantity>) -> Self {
        if let Some(quantity) = quantity {
            self.quantify(quantity)
        } else {
            self
        }
    }
}

impl From<Line> for Unit {
    fn from(v: Line) -> Self {
        Self::Line(v)
    }
}

impl From<Row> for Unit {
    fn from(row: Row) -> Self {
        Self::Line(Line::Row(row))
    }
}

impl From<Column> for Unit {
    fn from(column: Column) -> Self {
        Self::Line(Line::Column(column))
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Direction {
    Above,
    Below,
    Left,
    Right,
}

impl Direction {
    pub(crate) const ALL: [Self; 4] = [Self::Above, Self::Below, Self::Left, Self::Right];
}

#[cfg(test)]
mod tests {
    use crate::solver::hint::recipes::HintRecipe;
    use crate::solver::hint::{Line, Unit};
    use crate::solver::{Judgment, Row};

    use super::{Direction, Parity, Quantity};

    #[test]
    fn sample_26_02_05_alice() {
        assert_eq!(
            HintRecipe::parse("Tina is one of 3 criminals in row\u{A0}4").unwrap(),
            [
                HintRecipe::Count(
                    Unit::Line(Line::Row(Row::Four)).into(),
                    Judgment::Criminal,
                    Quantity::Exact(3)
                ),
                HintRecipe::Member(
                    "Tina".into(),
                    Unit::Line(Line::Row(Row::Four)),
                    Judgment::Criminal
                ),
            ]
        );
    }

    #[test]
    fn sample_26_02_05_tina() {
        let set = Unit::Direction(Direction::Above, "Xavi".into());
        assert_eq!(
            HintRecipe::parse("Both criminals above Xavi are connected").unwrap(),
            [
                HintRecipe::Count(set.clone().into(), Judgment::Criminal, Quantity::Exact(2)),
                HintRecipe::Connected(set, Judgment::Criminal)
            ]
        );
    }

    #[test]
    fn sample_26_02_05_kyle() {
        assert_eq!(
            HintRecipe::parse("An odd number of innocents above Zara neighbor Gary").unwrap(),
            [HintRecipe::Count(
                Unit::Direction(Direction::Above, "Zara".into()).and(Unit::Neighbor("Gary".into())),
                Judgment::Innocent,
                Quantity::Parity(Parity::Odd)
            )],
        );
    }
}
