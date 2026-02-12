use std::iter::once;
use std::ops::Not as _;

use anyhow::{Result, bail};
use mitsein::iter1::IntoIterator1 as _;

use crate::puzzle::Name;
use crate::puzzle::grid::{Coordinate, Grid};
use crate::puzzle::hint::parsers::{SentenceKind, Unit};
use crate::puzzle::hint::{HintKind, Set, WithJudgment};

#[derive(Clone, Copy)]
pub(crate) struct Context<'ctx> {
    pub grid: &'ctx Grid,
    pub speaker: &'ctx Name,
}

impl<'ctx> Context<'ctx> {
    pub(crate) fn new(grid: &'ctx Grid, speaker: &'ctx Name) -> Self {
        Self { grid, speaker }
    }
}

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

pub(crate) trait AddContext {
    type Output;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output>;
}

impl<T: AddContext> AddContext for WithJudgment<T> {
    type Output = WithJudgment<T::Output>;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        Ok(WithJudgment {
            kind: self.kind.add_context(context)?,
            judgment: self.judgment,
        })
    }
}

impl AddContext for SentenceKind {
    type Output = Vec<HintKind>;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let hints: Vec<HintKind> = match self {
            Self::TraitsAreNeighborsInUnit(unit, quantity) => {
                unit.members_are_connected(quantity, context)?
            }
            Self::HasMostTraits(unit) => unit.has_most(context)?,
            Self::IsOneOfNTraitsInUnit(unit, name, quantity) => {
                let set = unit.add_context(context)?;
                let coord = name.add_context(context)?;
                if !set.contains(&coord) {
                    bail!("{name:?} does not belong to {unit:?}")
                }
                vec![HintKind::Count(set, quantity), HintKind::Judgment(coord)]
            }
            Self::MoreTraitsInUnitThanUnit { big, small } => {
                vec![HintKind::Bigger {
                    big: big.add_context(context)?,
                    small: small.add_context(context)?,
                }]
            }
            Self::NumberOfTraitsInUnit(unit, quantity) => {
                let set = unit.add_context(context)?;
                vec![HintKind::Count(set, quantity)]
            }
            Self::OnlyOnePersonInUnitHasNTraitNeighbors(unit, quantity, name) => {
                unit.unique_member_has_n_neighbors(quantity, name.as_ref(), context)?
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
            } => quantified.intersects_with(&other, intersection, Some(quantity), context)?,
            Self::UnitsShareNTraits([a, b], quantity) => {
                a.intersects_with(&b, quantity, None, context)?
            }
            Self::EqualNumberOfTraitsInUnits(units) => {
                let [a, b] = units.map(|unit| unit.add_context(context));
                vec![HintKind::Equal([a?, b?])]
            }
            Self::MoreTraitsInUnit(unit) => {
                vec![HintKind::Majority(unit.add_context(context)?)]
            }
            Self::HasTrait(name) => {
                vec![HintKind::Judgment(name.add_context(context)?)]
            }
            Self::AtMostNTraitsInNeighborsInUnit(unit, number) => {
                unit.members_have_at_most_neighbors(number, context)?
            }
        };
        Ok(hints)
    }
}

impl AddContext for &Unit {
    type Output = Set;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let set = match self {
            &Unit::Line(line) => line.into(),
            Unit::Direction(direction, name) => {
                let start = name.add_context(context)?;
                Coordinate::direction(start, *direction).collect()
            }
            Unit::Neighbor(name) => {
                let center = name.add_context(context)?;
                Coordinate::neighbors(center).collect()
            }
            Unit::Profession(profession) => context
                .grid
                .by_profession(profession)?
                .clone()
                .into_hash_set(),
            Unit::Edges => Coordinate::edges().collect(),
            Unit::Corners => Coordinate::corners().collect(),
            Unit::ProfessionShift(profession, direction) => context
                .grid
                .by_profession(profession)?
                .into_iter()
                .filter_map(|coord| coord.step(*direction))
                .collect(),
            Unit::Between(names) => {
                let [a, b] = names.each_ref().map(|name| name.add_context(context));
                Coordinate::between([a?, b?])?
            }
            Unit::All => Coordinate::all().collect(),
            Unit::Quantified(inner, quantity) => {
                let set = inner.add_context(context)?;
                if !quantity.matches(set.len()) {
                    bail!("{inner:?} does not have {quantity:?} members")
                }
                set
            }
        };
        Ok(set)
    }
}

impl AddContext for &NameRecipe {
    type Output = Coordinate;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let name = match self {
            NameRecipe::Me => context.speaker,
            NameRecipe::Other(name) => name,
        };
        context.grid.coord(name)
    }
}
