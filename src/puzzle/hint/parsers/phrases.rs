use std::iter::once;
use std::ops::Not as _;

use anyhow::bail;
use mitsein::hash_set1::HashSet1;
use mitsein::iter1::{IntoIterator1 as _, IteratorExt as _};
use mitsein::vec1::Vec1;

use crate::puzzle::Profession;
use crate::puzzle::grid::coordinate::{Column, Coordinate, Direction, Row};
use crate::puzzle::hint::recipes::{AddContext, Context, NameRecipe};
use crate::puzzle::hint::{Cardinal, HintKind, Line, LineKind, Number, Set};

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum SentenceKind {
    // This I think can't actually be "Me"
    HasTrait(NameRecipe),
    TraitsAreNeighborsInUnit(Unit, Option<Number>),
    HasMostTraits(UnitInSeries),
    IsOneOfNTraitsInUnit(Unit, NameRecipe, Cardinal),
    EqualNumberOfTraitsInUnits([Unit; 2]),
    MoreTraitsInUnitThanUnit {
        big: Unit,
        small: Unit,
    },
    EqualTraitsInUnit(Unit),
    MoreTraitsInUnit(Unit),
    NumberOfTraitsInUnit(Unit, Cardinal),
    OnlyOnePersonInUnitHasNTraitNeighbors(Unit, Cardinal, Option<NameRecipe>),
    EachUnitInSeriesHasNTraits(Series, Cardinal),
    OnlyOneUnitInSeriesHasNTraits(Series, Cardinal),
    OnlyGivenUnitHasNTraits(UnitInSeries, Cardinal),
    UnitSharesNOutOfNTraitsWithUnit {
        total: Number,
        quantified: Unit,
        other: Unit,
        intersection: Number,
    },
    UnitsShareNTraits([Unit; 2], Cardinal),
    AtMostNTraitsInNeighborsInUnit(Unit, Number),
    TotalNumberOfTraitsInUnits([Unit; 2], Cardinal),
}

impl AddContext for SentenceKind {
    type Output = Vec<HintKind>;

    fn add_context(self, context: Context<'_>) -> anyhow::Result<Self::Output> {
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
            Self::TotalNumberOfTraitsInUnits(units, quantity) => {
                let [a, b] = units.map(|unit| unit.add_context(context));
                vec![HintKind::CountTotal([a?, b?], quantity)]
            }
            Self::OnlyOnePersonInUnitHasNTraitNeighbors(unit, quantity, name) => {
                unit.unique_member_has_n_neighbors(quantity, name.as_ref(), context)?
            }
            Self::OnlyOneUnitInSeriesHasNTraits(series, quantity) => {
                let sets = series.all(context);
                vec![HintKind::UniqueWithCount(sets, quantity)]
            }
            Self::EachUnitInSeriesHasNTraits(kind, quantity) => kind
                .all(context)
                .into_iter()
                .map(|set| HintKind::Count(set, quantity))
                .collect(),
            Self::OnlyGivenUnitHasNTraits(unit, quantity) => {
                let others = unit.others(context)?;
                let equal = HintKind::Count(Unit::from(unit).add_context(context)?, quantity);
                others
                    .into_iter()
                    .map(|other| HintKind::Count(other, quantity).not())
                    .chain(once(equal))
                    .collect()
            }
            Self::UnitSharesNOutOfNTraitsWithUnit {
                total: quantity,
                quantified,
                other,
                intersection,
            } => quantified.intersects_with(
                &other,
                Cardinal::Exact(intersection),
                Some(quantity),
                context,
            )?,
            Self::UnitsShareNTraits([a, b], quantity) => {
                a.intersects_with(&b, quantity, None, context)?
            }
            Self::EqualNumberOfTraitsInUnits(units) => {
                let [a, b] = units.map(|unit| unit.add_context(context));
                vec![HintKind::Equal([a?, b?])]
            }
            Self::EqualTraitsInUnit(unit) => {
                let set = unit.add_context(context)?;
                //TODO use div_exact
                if !set.len().is_multiple_of(2) {
                    bail!("{unit:?} cannot be split equally")
                }
                let cardinal = Cardinal::Exact(u8::try_from(set.len() / 2).expect("at most 20"));
                vec![HintKind::Count(set, cardinal)]
            }
            Self::MoreTraitsInUnit(unit) => {
                let set = unit.add_context(context)?;
                let cardinal =
                    Cardinal::AtLeast(u8::try_from(set.len() / 2 + 1).expect("at most 20"));
                vec![HintKind::Count(set, cardinal)]
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

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) enum Unit {
    Direction(Direction, NameRecipe),
    Line(Line),
    Profession(Profession),
    ProfessionShift(Profession, Direction, Option<Number>),
    Neighbor(NameRecipe),
    Between([NameRecipe; 2]),
    Edges,
    Corners,
    All,
    Quantified(Box<Self>, Number),
}

impl Unit {
    pub(crate) fn unique_member_has_n_neighbors(
        &self,
        quantity: Cardinal,
        name: Option<&NameRecipe>,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        let set = self.add_context(context)?;
        let coord = name
            .as_ref()
            .map(|name| name.add_context(context))
            .transpose()?;
        let hints = if let Some(coord) = coord {
            if !set.contains(&coord) {
                bail!("{name:?} does not belong to {self:?}")
            }
            once(HintKind::Count(coord.neighbors().collect(), quantity))
                .chain(
                    set.into_iter()
                        .filter(|&other| other != coord)
                        .map(|other| HintKind::Count(other.neighbors().collect(), quantity).not()),
                )
                .collect()
        } else {
            let Ok(set) = HashSet1::try_from(set) else {
                bail!("empty unit {self:?} cannnot have unique member")
            };
            let sets = set
                .into_iter1()
                .map(|coord| coord.neighbors().collect())
                .collect1();
            vec![HintKind::UniqueWithCount(sets, quantity)]
        };
        Ok(hints)
    }

    pub(crate) fn intersects_with(
        &self,
        other: &Self,
        intersection: Cardinal,
        quantity: Option<Number>,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        let self_ = self.add_context(context)?;
        let other = other
            .add_context(context)?
            .into_iter()
            .filter(|other| self_.contains(other))
            .collect();
        let intersection = HintKind::Count(other, intersection);
        let hints = quantity
            .map(|quantity| HintKind::Count(self_, Cardinal::Exact(quantity)))
            .into_iter()
            .chain(once(intersection))
            .collect();
        Ok(hints)
    }

    pub(crate) fn members_have_at_most_neighbors(
        &self,
        number: u8,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        Ok(self
            .add_context(context)?
            .into_iter()
            .map(|coord| HintKind::Count(coord.neighbors().collect(), Cardinal::AtMost(number)))
            .collect())
    }

    pub(crate) fn members_are_connected(
        self,
        quantity: Option<Number>,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        let set = self.add_context(context)?;
        Ok(quantity
            .map(|quantity| HintKind::Count(set.clone(), Cardinal::Exact(quantity)))
            .into_iter()
            .chain(once(HintKind::Connected(set)))
            .collect())
    }

    pub(crate) fn quantify(self, quantity: Number) -> Self {
        Self::Quantified(Box::new(self), quantity)
    }

    #[cfg(test)]
    pub(crate) fn profession(profession: impl Into<Profession>) -> Self {
        Self::Profession(profession.into())
    }

    #[cfg(test)]
    pub(crate) fn neighbor(name: impl Into<NameRecipe>) -> Self {
        Self::Neighbor(name.into())
    }

    #[cfg(test)]
    pub(crate) fn direction(direction: Direction, name: impl Into<NameRecipe>) -> Self {
        Self::Direction(direction, name.into())
    }
}

impl AddContext for &Unit {
    type Output = Set;

    fn add_context(self, context: Context<'_>) -> anyhow::Result<Self::Output> {
        let set = match self {
            &Unit::Line(line) => line.into(),
            Unit::Direction(direction, name) => {
                let start = name.add_context(context)?;
                Coordinate::direction(start, *direction).collect()
            }
            Unit::Neighbor(name) => {
                let center = name.add_context(context)?;
                center.neighbors().collect()
            }
            Unit::Profession(profession) => context
                .grid
                .profession_as_set(profession)?
                .clone()
                .into_hash_set(),
            Unit::Edges => Coordinate::edges().collect(),
            Unit::Corners => Coordinate::corners().collect(),
            Unit::ProfessionShift(profession, direction, total) => {
                let set = context.grid.profession_as_set(profession)?;
                if total.is_some_and(|total| usize::from(total) != set.len().get()) {
                    bail!("{profession:?} does not have {total:?} members")
                }
                set.into_iter()
                    .filter_map(|coord| coord.step(*direction))
                    .collect()
            }
            Unit::Between(names) => {
                let [a, b] = names.each_ref().map(|name| name.add_context(context));
                Coordinate::between([a?, b?])?
            }
            Unit::All => Coordinate::all().into_iter().collect(),
            Unit::Quantified(inner, quantity) => {
                let set = inner.add_context(context)?;
                if usize::from(*quantity) != set.len() {
                    bail!("{inner:?} does not have {quantity:?} members")
                }
                set
            }
        };
        Ok(set)
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

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum UnitInSeries {
    Line(Line),
    Profession(Profession),
    Neighbor(NameRecipe),
}

impl UnitInSeries {
    pub(crate) fn has_most(self, context: Context<'_>) -> Result<Vec<HintKind>, anyhow::Error> {
        let small = self.others(context)?;
        let big = Unit::from(self).add_context(context)?;
        Ok(small
            .into_iter()
            .map(|small| HintKind::Bigger {
                big: big.clone(),
                small,
            })
            .collect())
    }

    pub(crate) fn others(&self, context: Context<'_>) -> anyhow::Result<Vec1<Set>> {
        match self {
            Self::Line(line) => Ok(line.others().into_iter1().map(Set::from).collect1()),
            Self::Profession(profession) => context.grid.other_professions(profession),
            Self::Neighbor(name) => {
                let coord = name.add_context(context)?;
                Ok(Coordinate::all()
                    .into_iter()
                    .filter(|&other| other != coord)
                    .map(|other| other.neighbors().collect())
                    .try_collect1()
                    .unwrap_or_else(|_empty| unreachable!()))
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn neighbor(name: impl Into<NameRecipe>) -> Self {
        Self::Neighbor(name.into())
    }

    #[cfg(test)]
    pub(crate) fn profession(profession: impl Into<Profession>) -> Self {
        Self::Profession(profession.into())
    }
}

impl From<Line> for UnitInSeries {
    fn from(v: Line) -> Self {
        Self::Line(v)
    }
}

impl From<Row> for UnitInSeries {
    fn from(row: Row) -> Self {
        Self::Line(Line::Row(row))
    }
}

impl From<Column> for UnitInSeries {
    fn from(column: Column) -> Self {
        Self::Line(Line::Column(column))
    }
}

impl From<UnitInSeries> for Unit {
    fn from(value: UnitInSeries) -> Self {
        match value {
            UnitInSeries::Line(line) => Self::Line(line),
            UnitInSeries::Profession(profession) => Self::Profession(profession),
            UnitInSeries::Neighbor(name) => Self::Neighbor(name),
        }
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone, Copy)]
pub(crate) enum Series {
    Line(LineKind),
    // Profession,
    Neighbor,
}

impl Series {
    pub(crate) fn all(self, _context: Context) -> Vec1<Set> {
        match self {
            Self::Line(line_kind) => line_kind.all().into_iter1().map(Set::from).collect1(),
            // Self::Profession => context
            //     .grid
            //     .by_profession()
            //     .values()
            //     .map(|set| set.clone().into_hash_set())
            //     .try_collect1()
            //     .expect("total len 20"),
            Self::Neighbor => Coordinate::all()
                .map(|center| center.neighbors().collect())
                .collect1(),
        }
    }
}

impl From<LineKind> for Series {
    fn from(kind: LineKind) -> Self {
        Self::Line(kind)
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone, Copy)]
pub(crate) enum Quantifier {
    Simple(Cardinal),
    // Maybe this needs to be Quantity, Quantity?
    Subset(Number, Number),
}

impl Quantifier {
    pub(crate) fn exact(self) -> Option<u8> {
        match self {
            Self::Subset(count, total) if count == total => Some(total),
            Self::Simple(Cardinal::Exact(total)) => Some(total),
            Self::Simple(Cardinal::AtLeast(_) | Cardinal::AtMost(_) | Cardinal::Parity(_))
            | Self::Subset(_, _) => None,
        }
    }
}
