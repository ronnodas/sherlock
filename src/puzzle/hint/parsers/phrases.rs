use std::ops::Not as _;

use anyhow::bail;
use mitsein::iter1::{IntoIterator1 as _, IteratorExt as _};
use mitsein::vec1::Vec1;

use crate::puzzle::grid::coordinate::{Column, Coordinate, Direction, ModifiedSet, Row, Set1};
use crate::puzzle::hint::recipes::{
    AddContext, ColumnRecipe, Context, LineRecipe, NameRecipe, RowRecipe,
};
use crate::puzzle::hint::{Cardinal, Comparison, HintKind, LineKind, Number, Set};
use crate::puzzle::{Judgment, Profession};

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
        excess: Option<Number>,
    },
    EqualTraitsInUnit(Unit),
    MoreTraitsInUnit(Unit),
    NumberOfTraitsInUnit(Unit, Cardinal),
    OnlyOnePersonInUnitHasNTraitNeighbors(Unit, Cardinal, Option<NameRecipe>),
    NPeopleInUnitHaveNTraitNeighbors {
        unit: Unit,
        quantity: Cardinal,
        neighbors: Cardinal,
    },
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
                let (set, mut hints) = unit.add_context(context)?;
                let coord = name.add_context(context)?;
                if set.as_regular().is_some_and(|set| !set.contains(coord)) {
                    bail!("{name:?} does not belong to {unit:?}")
                }
                hints.extend([HintKind::Count(set, quantity), HintKind::Judgment(coord)]);
                hints
            }
            Self::MoreTraitsInUnitThanUnit { big, small, excess } => {
                let (big, mut hints) = big.add_context(context)?;
                let (small, small_hints) = small.add_context(context)?;
                hints.extend(small_hints);
                hints.push(HintKind::CompareSets(
                    [big, small],
                    Comparison::MoreThan(excess),
                ));
                hints
            }
            Self::NumberOfTraitsInUnit(unit, quantity) => {
                let (set, mut hints) = unit.add_context(context)?;
                hints.push(HintKind::Count(set, quantity));
                hints
            }
            Self::TotalNumberOfTraitsInUnits(units, quantity) => {
                let [a, b] = units.map(|unit| unit.add_context(context));
                let (a, mut hints) = a?;
                let (b, b_hints) = b?;
                hints.extend(b_hints);
                hints.push(HintKind::CountTotal([a, b], quantity));
                hints
            }
            Self::OnlyOnePersonInUnitHasNTraitNeighbors(unit, quantity, name) => {
                unit.unique_member_has_n_neighbors(quantity, name.as_ref(), context)?
            }

            Self::OnlyOneUnitInSeriesHasNTraits(series, quantity) => {
                let sets = series.all(context);
                vec![HintKind::unique_with_count(sets, quantity)]
            }
            Self::EachUnitInSeriesHasNTraits(kind, quantity) => kind
                .all(context)
                .into_iter()
                .map(|set| HintKind::Count(set.into(), quantity))
                .collect(),
            Self::OnlyGivenUnitHasNTraits(unit, quantity) => {
                unit.only_one_with_n_traits(quantity, context)?
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
                let (a, mut hints) = a?;
                let (b, b_hints) = b?;
                hints.extend(b_hints);
                hints.push(HintKind::CompareSets([a, b], Comparison::Equal));
                hints
            }
            Self::EqualTraitsInUnit(unit) => unit.equal_traits(context)?,
            Self::MoreTraitsInUnit(unit) => {
                let (set, mut hints) = unit.add_context(context)?;
                hints.push(HintKind::CompareTraits(set, Comparison::MoreThan(None)));
                hints
            }
            Self::HasTrait(name) => {
                vec![HintKind::Judgment(name.add_context(context)?)]
            }
            Self::AtMostNTraitsInNeighborsInUnit(unit, number) => {
                unit.members_have_at_most_neighbors(number, context)?
            }
            Self::NPeopleInUnitHaveNTraitNeighbors {
                unit,
                quantity,
                neighbors,
            } => unit.n_members_have_n_neighbors(quantity, neighbors, context)?,
        };
        Ok(hints)
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) enum Unit {
    Direction(Direction, NameRecipe),
    Line(LineRecipe),
    Profession(Profession),
    Neighbor(NameRecipe),
    Between([NameRecipe; 2]),
    Edges,
    Corners,
    All,
    Shifted(Box<Self>, Direction),
    Quantified(Box<Self>, Number),
    Judged(Box<Self>, Judgment),
}

impl Unit {
    pub(crate) fn unique_member_has_n_neighbors(
        &self,
        quantity: Cardinal,
        name: Option<&NameRecipe>,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        let (set, mut hints) = self.add_context(context)?;
        let coord = name
            .as_ref()
            .map(|name| name.add_context(context))
            .transpose()?;
        if let Some(set) = set.as_regular() {
            if let Some(coord) = coord {
                if !set.contains(coord) {
                    bail!("{name:?} does not belong to {self:?}")
                }
                hints.push(HintKind::Count(coord.neighbors().collect(), quantity));
                hints.extend(
                    set.into_iter()
                        .filter(|&other| other != coord)
                        .map(|other| HintKind::Count(other.neighbors().collect(), quantity).not()),
                );
            } else {
                let Ok(set) = Set1::try_from(set) else {
                    bail!("empty unit {self:?} cannnot have unique member")
                };
                let sets = set
                    .into_iter1()
                    .map(|coord| coord.neighbors().collect())
                    .collect1();
                hints.push(HintKind::unique_with_count(sets, quantity));
            }
        } else {
            let unique = HintKind::CountWithNeighbors {
                set,
                each: quantity,
                count: Cardinal::Exact(1),
            };
            hints.push(unique);
            if let Some(coord) = coord {
                hints.push(HintKind::Count(coord.neighbors().collect(), quantity));
            }
        }
        Ok(hints)
    }

    pub(crate) fn intersects_with(
        &self,
        other: &Self,
        intersection: Cardinal,
        quantity: Option<Number>,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        let (self_, mut hints) = self.add_context(context)?;
        let (other, other_hints) = other.add_context(context)?;
        hints.extend(other_hints);
        let other = other.intersect(self_.clone());
        hints.push(HintKind::Count(other, intersection));
        if let Some(quantity) = quantity {
            hints.push(HintKind::Count(self_, Cardinal::Exact(quantity)));
        }
        Ok(hints)
    }

    pub(crate) fn members_have_at_most_neighbors(
        &self,
        number: u8,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        let (set, mut hints) = self.add_context(context)?;
        hints.push(HintKind::EachNeighbors(set, Cardinal::AtMost(number)));
        Ok(hints)
    }

    pub(crate) fn members_are_connected(
        self,
        quantity: Option<Number>,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        let (set, mut hints) = self.add_context(context)?;
        if let Some(quantity) = quantity {
            hints.push(HintKind::Count(set.clone(), Cardinal::Exact(quantity)));
        }
        hints.push(HintKind::Connected(set));
        Ok(hints)
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

    fn n_members_have_n_neighbors(
        self,
        quantity: Cardinal,
        neighbors: Cardinal,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        let (set, mut hints) = self.add_context(context)?;
        hints.push(HintKind::CountWithNeighbors {
            set,
            count: quantity,
            each: neighbors,
        });
        Ok(hints)
    }

    pub(crate) fn with_judgment(self, judgment: Judgment) -> Self {
        Self::Judged(Box::new(self), judgment)
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        Self::Shifted(Box::new(self), direction)
    }

    fn equal_traits(&self, context: Context<'_>) -> Result<Vec<HintKind>, anyhow::Error> {
        let (set, mut hints) = self.add_context(context)?;
        let extra = if let Some(set) = set.as_regular() {
            if !set.len().is_multiple_of(2) {
                bail!("{self:?} cannot be split equally")
            }
            let count = u8::try_from(set.len()).expect("at most 20") / 2;
            HintKind::Count(set.into(), Cardinal::Exact(count))
        } else {
            HintKind::CompareTraits(set, Comparison::Equal)
        };
        hints.push(extra);
        Ok(hints)
    }
}

impl AddContext for &Unit {
    type Output = (ModifiedSet, Vec<HintKind>);

    fn add_context(self, context: Context<'_>) -> anyhow::Result<Self::Output> {
        let mut hints = Vec::new();
        let set: ModifiedSet = match self {
            &Unit::Line(line) => line.add_context(context)?.into(),
            Unit::Direction(direction, name) => {
                let start = name.add_context(context)?;
                Coordinate::direction(start, *direction).collect()
            }
            Unit::Neighbor(name) => {
                let center = name.add_context(context)?;
                center.neighbors().collect()
            }
            Unit::Profession(profession) => (*context.grid.profession_as_set(profession)?).into(),
            Unit::Edges => Coordinate::edges().collect(),
            Unit::Corners => Coordinate::corners().collect(),
            Unit::Between(names) => {
                let [a, b] = names.each_ref().map(|name| name.add_context(context));
                Coordinate::between([a?, b?])?.into()
            }
            Unit::All => Coordinate::all().into_iter().collect(),
            Unit::Quantified(inner, quantity) => {
                let set;
                (set, hints) = inner.add_context(context)?;
                hints.push(HintKind::Count(set.clone(), Cardinal::Exact(*quantity)));
                set
            }
            Unit::Shifted(inner, direction) => {
                let set;
                (set, hints) = inner.add_context(context)?;
                match set {
                    ModifiedSet::Regular(set) => ModifiedSet::Regular(set.shift(*direction)),
                    set @ (ModifiedSet::Modified(..) | ModifiedSet::Intersection(..)) => {
                        ModifiedSet::Modified(Box::new(set), (*direction).into())
                    }
                }
            }
            Unit::Judged(inner, judgment) => {
                let set;
                (set, hints) = inner.add_context(context)?;
                ModifiedSet::Modified(Box::new(set), (*judgment).into())
            }
        };
        Ok((set, hints))
    }
}

impl From<LineRecipe> for Unit {
    fn from(v: LineRecipe) -> Self {
        Self::Line(v)
    }
}

impl From<RowRecipe> for Unit {
    fn from(row: RowRecipe) -> Self {
        Self::Line(LineRecipe::Row(row))
    }
}

impl From<Row> for Unit {
    fn from(row: Row) -> Self {
        Self::Line(LineRecipe::Row(RowRecipe::Explicit(row)))
    }
}

impl From<ColumnRecipe> for Unit {
    fn from(column: ColumnRecipe) -> Self {
        Self::Line(LineRecipe::Column(column))
    }
}

impl From<Column> for Unit {
    fn from(value: Column) -> Self {
        Self::Line(LineRecipe::Column(ColumnRecipe::Explicit(value)))
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum UnitInSeries {
    Line(LineRecipe),
    Profession(Profession),
    Neighbor(NameRecipe),
}

impl UnitInSeries {
    pub(crate) fn has_most(self, context: Context<'_>) -> Result<Vec<HintKind>, anyhow::Error> {
        let small = self.others(context)?;
        let (big, mut hints) = Unit::from(self).add_context(context)?;
        hints.extend(small.into_iter().map(|other| {
            HintKind::CompareSets([big.clone(), other.into()], Comparison::MoreThan(None))
        }));
        Ok(hints)
    }

    // TODO return Vec1<Set1>
    pub(crate) fn others(&self, context: Context<'_>) -> anyhow::Result<Vec1<Set>> {
        match self {
            Self::Line(line) => Ok(line
                .add_context(context)?
                .others()
                .into_iter1()
                .map(Set::from)
                .collect1()),
            Self::Profession(profession) => context
                .grid
                .other_professions(profession)
                .map(|others| others.into_iter1().map(Set::from).collect1()),
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

    fn only_one_with_n_traits(
        self,
        quantity: Cardinal,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<HintKind>> {
        let others = self.others(context)?;
        let (this, mut hints) = Unit::from(self).add_context(context)?;
        hints.push(HintKind::Count(this, quantity));
        hints.extend(
            others
                .into_iter()
                .map(|other| HintKind::Count(other.into(), quantity).not()),
        );
        Ok(hints)
    }
}

impl From<LineRecipe> for UnitInSeries {
    fn from(v: LineRecipe) -> Self {
        Self::Line(v)
    }
}

impl From<RowRecipe> for UnitInSeries {
    fn from(row: RowRecipe) -> Self {
        Self::Line(LineRecipe::Row(row))
    }
}

impl From<Row> for UnitInSeries {
    fn from(row: Row) -> Self {
        Self::Line(LineRecipe::Row(RowRecipe::Explicit(row)))
    }
}

impl From<ColumnRecipe> for UnitInSeries {
    fn from(column: ColumnRecipe) -> Self {
        Self::Line(LineRecipe::Column(column))
    }
}

impl From<Column> for UnitInSeries {
    fn from(value: Column) -> Self {
        Self::Line(LineRecipe::Column(ColumnRecipe::Explicit(value)))
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
