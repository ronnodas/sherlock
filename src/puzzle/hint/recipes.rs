use std::iter::once;
use std::ops::Not as _;

use anyhow::{Result, bail};
use mitsein::hash_set1::HashSet1;
use mitsein::iter1::{self, IntoIterator1 as _};
use mitsein::vec1::Vec1;

use crate::puzzle::Name;
use crate::puzzle::grid::{Coordinate, Grid};
use crate::puzzle::hint::parsers::{SentenceKind, Unit};
use crate::puzzle::hint::{HintKind, Line, Quantity, Set, WithJudgment};

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

impl<T: Recipe> Recipe for WithJudgment<T> {
    type Output = WithJudgment<T::Output>;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        Ok(WithJudgment {
            kind: self.kind.contextualize(grid, speaker)?,
            judgment: self.judgment,
        })
    }
}

impl Recipe for SentenceKind {
    type Output = Vec<HintKind>;

    fn contextualize(self, grid: &Grid, speaker: &Name) -> Result<Self::Output> {
        let hints: Vec<HintKind> = match self {
            Self::TraitsAreNeighborsInUnit(unit, quantity) => {
                let set = unit.contextualize(grid, speaker)?;
                quantity
                    .map(|quantity| HintKind::Count(set.clone(), quantity))
                    .into_iter()
                    .chain(iter1::one(HintKind::Connected(set)))
                    .collect()
            }
            Self::HasMostTraits(unit) => {
                let small = unit.others(grid, speaker)?;
                let big = Unit::from(unit).contextualize(grid, speaker)?;
                small
                    .into_iter()
                    .map(|small| HintKind::Bigger {
                        big: big.clone(),
                        small,
                    })
                    .collect()
            }
            Self::IsOneOfNTraitsInUnit(unit, name, quantity) => {
                let set = unit.contextualize(grid, speaker)?;
                let coord = name.contextualize(grid, speaker)?;
                if !set.contains(&coord) {
                    bail!("{name:?} does not belong to {unit:?}")
                }
                vec![HintKind::Count(set, quantity), HintKind::Judgment(coord)]
            }
            Self::MoreTraitsInUnitThanUnit { big, small } => {
                vec![HintKind::Bigger {
                    big: big.contextualize(grid, speaker)?,
                    small: small.contextualize(grid, speaker)?,
                }]
            }
            Self::NumberOfTraitsInUnit(unit, quantity) => {
                let set = unit.contextualize(grid, speaker)?;
                vec![HintKind::Count(set, quantity)]
            }
            Self::OnlyOnePersonInUnitHasNTraitNeighbors(unit, quantity, name) => {
                let set = unit.contextualize(grid, speaker)?;
                let coord = name
                    .as_ref()
                    .map(|name| name.contextualize(grid, speaker))
                    .transpose()?;
                if let Some(coord) = coord {
                    if !set.contains(&coord) {
                        bail!("{name:?} does not belong to {unit:?}")
                    }
                    once(HintKind::Count(
                        Coordinate::neighbors(coord).collect(),
                        quantity,
                    ))
                    .chain(
                        set.into_iter()
                            .filter(|&other| other != coord)
                            .map(|other| {
                                HintKind::Count(Coordinate::neighbors(other).collect(), quantity)
                                    .not()
                            }),
                    )
                    .collect()
                } else {
                    let Ok(set) = HashSet1::try_from(set) else {
                        bail!("empty unit {unit:?} cannnot have unique member")
                    };
                    let sets = set
                        .into_iter1()
                        .map(|coord| Coordinate::neighbors(coord).collect())
                        .collect1();
                    vec![HintKind::UniqueWithCount(sets, quantity)]
                }
            }
            Self::OnlyOneLineHasNTraits(kind, quantity) => {
                let sets = kind.all().into_iter1().map(Set::from).collect1();
                vec![HintKind::UniqueWithCount(sets, quantity)]
            }
            Self::EachLineHasNTraits(kind, quantity) => kind
                .all()
                .into_iter()
                .map(|line| HintKind::Count(line.into(), quantity))
                .collect(),
            Self::OnlyGivenLineHasNTraits(line, quantity) => {
                let equal = HintKind::Count(line.into(), quantity);
                line.others()
                    .into_iter()
                    .map(|other| HintKind::Count(other.into(), quantity).not())
                    .chain(once(equal))
                    .collect()
            }
            Self::UnitSharesNOutOfNTraitsWithUnit {
                quantity,
                quantified,
                other,
                intersection,
            } => {
                let quantified = quantified.contextualize(grid, speaker)?;
                let other = other
                    .contextualize(grid, speaker)?
                    .into_iter()
                    .filter(|other| quantified.contains(other))
                    .collect();
                vec![
                    HintKind::Count(quantified, quantity),
                    HintKind::Count(other, intersection),
                ]
            }
            Self::UnitsShareNTraits([a, b], quantity) => {
                let intersection = a.and(b).contextualize(grid, speaker)?;
                vec![HintKind::Count(intersection, quantity)]
            }
            Self::EqualNumberOfTraitsInUnits(units) => {
                let [a, b] = units.map(|unit| unit.contextualize(grid, speaker));
                vec![HintKind::Equal([a?, b?])]
            }
            Self::MoreTraitsInUnit(unit) => {
                vec![HintKind::Majority(unit.contextualize(grid, speaker)?)]
            }
            Self::HasTrait(name) => {
                vec![HintKind::Judgment(name.contextualize(grid, speaker)?)]
            }
            Self::AtMostNTraitsInNeighborsInUnit(unit, number) => unit
                .contextualize(grid, speaker)?
                .into_iter()
                .map(|coord| {
                    HintKind::Count(
                        Coordinate::neighbors(coord).collect(),
                        Quantity::AtMost(number),
                    )
                })
                .collect(),
        };
        Ok(hints)
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
            &Unit::Line(line) => line.into(),
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
            Unit::Between(names) => {
                let [a, b] = names
                    .each_ref()
                    .map(|name| name.contextualize(grid, speaker));
                Coordinate::between([a?, b?])?
            }
            Unit::All => Coordinate::all().collect(),
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
