mod parsers;

use std::collections::HashSet;

use anyhow::{Result, bail};
use mitsein::array_vec1::ArrayVec1;
use mitsein::hash_set1::HashSet1;
use mitsein::iter1::IntoIterator1 as _;
use mitsein::vec1::{Vec1, vec1};

use crate::solver::{Grid, Profession};

use super::{Column, Coordinate, Judgment, Name, Row, Solution};
use parsers::Sentence;

pub(crate) type Set = HashSet<Coordinate>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Hint {
    Judgment(Coordinate, Judgment),
    Count(Set, Judgment, Quantity),
    Connected(Set, Judgment),
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
#[derive(Debug)]
pub(crate) enum HintRecipe {
    Member(Name, Unit, Judgment),
    Count(SetRecipe, Judgment, Quantity),
    Connected(Unit, Judgment),
    Bigger {
        big: Unit,
        small: Unit,
        judgment: Judgment,
    },
    UniqueWithNeighbors(Unit, Judgment, Quantity),
    UniqueLine(LineKind, Judgment, Quantity),
    Not(Box<Self>),
}

impl HintRecipe {
    pub(crate) fn parse(hint: &str) -> Result<Vec<Self>> {
        Ok(Sentence::parse(hint)?.collate())
    }

    fn not(self) -> Self {
        if let Self::Not(reverse) = self {
            *reverse
        } else {
            Self::Not(Box::new(self))
        }
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum SetRecipe {
    Unit(Unit),
    Intersection(Vec1<Unit>),
}

impl From<Unit> for SetRecipe {
    fn from(unit: Unit) -> Self {
        Self::Unit(unit)
    }
}

impl From<Line> for SetRecipe {
    fn from(line: Line) -> Self {
        Self::Unit(Unit::Line(line))
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) enum Unit {
    Direction(Direction, Name),
    Line(Line),
    Profession(Profession, Option<Quantity>),
    ProfessionShift(Profession, Direction),
    Neighbor(Name),
    Edges,
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

impl Unit {
    fn and(self, other: Self) -> SetRecipe {
        SetRecipe::Intersection(vec1![self, other])
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
    Parity(Parity),
}

impl Quantity {
    pub(crate) fn matches(self, len: usize) -> bool {
        match self {
            Self::Exact(value) => len == value.into(),
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

pub(crate) trait Recipe {
    type Output;

    fn contextualize(self, grid: &Grid) -> Result<Self::Output>;
}

impl Recipe for HintRecipe {
    type Output = Hint;

    fn contextualize(self, grid: &Grid) -> Result<Self::Output> {
        let hint = match self {
            Self::Member(name, set, judgment) => {
                let coordinate = grid.coord(&name)?;
                let set = set.contextualize(grid)?;
                if !set.contains(&coordinate) {
                    bail!("invalid hint: {name} not in {set:?}")
                }
                Hint::Judgment(coordinate, judgment)
            }
            Self::Count(set, judgment, quantity) => {
                Hint::Count(set.contextualize(grid)?, judgment, quantity)
            }
            Self::Connected(set, judgment) => Hint::Connected(set.contextualize(grid)?, judgment),
            Self::Bigger {
                big,
                small,
                judgment,
            } => Hint::Bigger {
                big: big.contextualize(grid)?,
                small: small.contextualize(grid)?,
                judgment,
            },
            Self::UniqueWithNeighbors(unit, judgment, quantity) => {
                let Ok(set) = HashSet1::try_from(unit.contextualize(grid)?) else {
                    bail!("empty unit {unit:?} cannnot have unique member")
                };
                let sets = set
                    .into_iter1()
                    .map(|coord| Coordinate::neighbors(coord).collect())
                    .collect1();
                Hint::UniqueWithCount(sets, judgment, quantity)
            }
            Self::UniqueLine(kind, judgment, quantity) => {
                let sets: Result<Vec1<HashSet<Coordinate>>> = kind
                    .all()
                    .into_iter1()
                    .map(|line| line.contextualize(grid))
                    .collect1();
                Hint::UniqueWithCount(sets?, judgment, quantity)
            }
            Self::Not(reverse) => Hint::Not(Box::new(reverse.contextualize(grid)?)),
        };
        Ok(hint)
    }
}

impl Recipe for SetRecipe {
    type Output = Set;

    fn contextualize(self, grid: &Grid) -> Result<Self::Output> {
        match self {
            Self::Unit(unit) => unit.contextualize(grid),
            Self::Intersection(sets) => sets
                .into_iter1()
                .map(|set| set.contextualize(grid))
                .reduce(|a, b| Ok(a?.intersection(&b?).copied().collect())),
        }
    }
}

impl Recipe for &Unit {
    type Output = Set;

    fn contextualize(self, grid: &Grid) -> Result<Self::Output> {
        let set = match self {
            &Unit::Line(line) => line.contextualize(grid)?,
            Unit::Direction(direction, name) => {
                let start = grid.coord(name)?;
                Coordinate::direction(start, *direction).collect()
            }
            Unit::Neighbor(name) => {
                let center = grid.coord(name)?;
                Coordinate::neighbors(center).collect()
            }
            Unit::Profession(profession, quantity) => {
                let set = grid.by_profession(profession)?.clone().into_hash_set();
                if let Some(quantity) = quantity
                    && !quantity.matches(set.len())
                {
                    bail!("{profession} does not have {quantity:?} members")
                }
                set
            }
            Unit::Edges => Coordinate::edges().collect(),
            Unit::ProfessionShift(profession, direction) => grid
                .by_profession(profession)?
                .into_iter()
                .filter_map(|coord| coord.step(*direction))
                .collect(),
        };
        Ok(set)
    }
}

impl Recipe for Line {
    type Output = Set;

    fn contextualize(self, _grid: &Grid) -> Result<Self::Output> {
        let set = match self {
            Self::Row(row) => Coordinate::row_all(row).collect(),
            Self::Column(column) => Coordinate::column_all(column).collect(),
        };
        Ok(set)
    }
}

#[cfg(test)]
mod tests {
    use crate::solver::hint::{Line, Unit};
    use crate::solver::{Judgment, Row};

    use super::{Direction, HintRecipe, Parity, Quantity};

    #[test]
    fn sample_26_02_05_alice() {
        assert_eq!(
            HintRecipe::parse("Tina is one of 3 criminals in row\u{A0}4").unwrap(),
            [
                HintRecipe::Member(
                    "Tina".to_owned(),
                    Unit::Line(Line::Row(Row::Four)),
                    Judgment::Criminal
                ),
                HintRecipe::Count(
                    Unit::Line(Line::Row(Row::Four)).into(),
                    Judgment::Criminal,
                    Quantity::Exact(3)
                )
            ]
        );
    }

    #[test]
    fn sample_26_02_05_tina() {
        let set = Unit::Direction(Direction::Above, "Xavi".to_owned());
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
                Unit::Neighbor("Zara".to_owned()).and(Unit::Neighbor("Gary".to_owned())),
                Judgment::Innocent,
                Quantity::Parity(Parity::Odd)
            )],
        );
    }
}
