use std::collections::HashSet;

use anyhow::{Result, bail};
use mitsein::hash_set1::HashSet1;
use mitsein::iter1::IntoIterator1 as _;
use mitsein::vec1::Vec1;

use crate::solver::grid::Grid;
use crate::solver::hint::parsers::Sentence;
use crate::solver::hint::{Hint, Line, LineKind, Quantity, Set, Unit};
use crate::solver::{Coordinate, Judgment, Name};

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone)]
pub(crate) enum NameRecipe {
    Me,
    Other(Name),
}

impl From<&str> for NameRecipe {
    fn from(v: &str) -> Self {
        Self::Other(v.to_owned())
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum HintRecipe {
    Member(NameRecipe, Unit, Judgment),
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
    EqualSize(Unit, Unit, Judgment),
}

impl HintRecipe {
    pub(crate) fn parse(hint: &str) -> Result<Vec<Self>> {
        Ok(Sentence::parse(&hint.replace("&nbsp;", "\u{A0}"))?.collate())
    }

    pub(crate) fn not(self) -> Self {
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

pub(crate) trait Recipe {
    type Output;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output>;
}

impl Recipe for HintRecipe {
    type Output = Hint;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        let hint = match self {
            Self::Member(name, set, judgment) => {
                let coordinate = name.contextualize(grid, speaker)?;
                let set = set.contextualize(grid, speaker)?;
                if !set.contains(&coordinate) {
                    bail!("invalid hint: {name:?} not in {set:?}")
                }
                Hint::Judgment(coordinate, judgment)
            }
            Self::Count(set, judgment, quantity) => {
                Hint::Count(set.contextualize(grid, speaker)?, judgment, quantity)
            }
            Self::Connected(set, judgment) => {
                Hint::Connected(set.contextualize(grid, speaker)?, judgment)
            }
            Self::Bigger {
                big,
                small,
                judgment,
            } => Hint::Bigger {
                big: big.contextualize(grid, speaker)?,
                small: small.contextualize(grid, speaker)?,
                judgment,
            },
            Self::UniqueWithNeighbors(unit, judgment, quantity) => {
                let Ok(set) = HashSet1::try_from(unit.contextualize(grid, speaker)?) else {
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
                    .map(|line| line.contextualize(grid, speaker))
                    .collect1();
                Hint::UniqueWithCount(sets?, judgment, quantity)
            }
            Self::EqualSize(a, b, judgment) => Hint::Equal(
                a.contextualize(grid, speaker)?,
                b.contextualize(grid, speaker)?,
                judgment,
            ),
            Self::Not(reverse) => Hint::Not(Box::new(reverse.contextualize(grid, speaker)?)),
        };
        Ok(hint)
    }
}

impl Recipe for SetRecipe {
    type Output = Set;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        match self {
            Self::Unit(unit) => unit.contextualize(grid, speaker),
            Self::Intersection(sets) => sets
                .into_iter1()
                .map(|set| set.contextualize(grid, speaker))
                .reduce(|a, b| Ok(a?.intersection(&b?).copied().collect())),
        }
    }
}

impl Recipe for &Unit {
    type Output = Set;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        let set = match self {
            &Unit::Line(line) => line.contextualize(grid, speaker)?,
            Unit::Direction(direction, name) => {
                let start = name.contextualize(grid, speaker)?;
                Coordinate::direction(start, *direction).collect()
            }
            Unit::Neighbor(name) => {
                let center = name.contextualize(grid, speaker)?;
                Coordinate::neighbors(center).collect()
            }
            Unit::Profession(profession) => grid.by_profession(profession)?.clone().into_hash_set(),
            Unit::Edges => Coordinate::edges().collect(),
            Unit::Corners => Coordinate::corners().collect(),
            Unit::ProfessionShift(profession, direction) => grid
                .by_profession(profession)?
                .into_iter()
                .filter_map(|coord| coord.step(*direction))
                .collect(),
            Unit::Quantified(inner, quantity) => {
                let set = inner.contextualize(grid, speaker)?;
                if !quantity.matches(set.len()) {
                    bail!("{inner:?} does not have {quantity:?} members")
                }
                set
            }
        };
        Ok(set)
    }
}

impl Recipe for &NameRecipe {
    type Output = Coordinate;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        let name = match self {
            NameRecipe::Me => speaker,
            NameRecipe::Other(name) => name,
        };
        grid.coord(name)
    }
}

impl Recipe for Line {
    type Output = Set;

    fn contextualize(self, _grid: &Grid, _speaker: &Name) -> Result<Self::Output> {
        let set = match self {
            Self::Row(row) => Coordinate::row_all(row).collect(),
            Self::Column(column) => Coordinate::column_all(column).collect(),
        };
        Ok(set)
    }
}
