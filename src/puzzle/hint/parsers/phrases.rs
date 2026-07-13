use anyhow::bail;
use mitsein::iter1::{IntoIterator1 as _, IteratorExt as _};
use mitsein::vec1::Vec1;

use crate::puzzle::grid::coordinate::{Column, Coordinate, Direction, ModifiedSet, Row, Set1};
use crate::puzzle::hint::recipes::{
    AddContext, ColumnRecipe, Context, LineRecipe, NameRecipe, RowRecipe,
};
use crate::puzzle::hint::{Cardinal, Comparison, Hint, LineKind, Number, Set};
use crate::puzzle::{Judgment, Profession};

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum Sentence {
    // This I think can't actually be "Me"
    HasTrait(NameRecipe, Judgment),
    UnitIsConnected(Unit),
    BiggestInSeries(UnitInSeries, Judgment),
    IsOneOfNInUnit(Unit, NameRecipe, Cardinal, Judgment),
    EqualNumberOfTraitsInUnits([Unit; 2], Judgment),
    UnitBiggerThanUnit {
        big: Unit,
        small: Unit,
        excess: Option<Number>,
    },
    UnitEquallySplit(Unit),
    MoreTraitsInUnit(Unit, Judgment),
    UnitSize(Unit, Cardinal),
    UniqueInUnitHasNNeighbors(Unit, Cardinal, Option<NameRecipe>, Judgment),
    NInUnitHaveNNeighbors {
        unit: Unit,
        quantity: Cardinal,
        neighbors: Cardinal,
        judgment: Judgment,
    },
    EachUnitInSeriesHasSize(Series, Cardinal, Judgment),
    UniqueUnitInSeriesHasSize(Series, Cardinal, Judgment),
    OnlyGivenUnitHasNTraits(UnitInSeries, Cardinal, Judgment),
    UnitAndIntersectionSize {
        total: Number,
        quantified: Unit,
        other: Unit,
        intersection: Number,
        judgment: Judgment,
    },
    IntersectionSize([Unit; 2], Cardinal, Judgment),
    EachInUnitHasAtMostNNeighbors(Unit, Number, Judgment),
    TotalUnitsSize([Unit; 2], Cardinal, Judgment),
}

impl AddContext for Sentence {
    type Output = Vec<Hint>;

    fn add_context(self, context: Context<'_>) -> anyhow::Result<Self::Output> {
        let hints: Vec<Hint> = match self {
            Self::UnitIsConnected(unit) => unit.members_are_connected(context)?,
            Self::BiggestInSeries(unit, judgment) => unit.has_most(judgment, context)?,
            Self::IsOneOfNInUnit(unit, name, quantity, judgment) => {
                unit.one_of_n_in_unit(&name, quantity, judgment, context)?
            }
            Self::UnitBiggerThanUnit { big, small, excess } => {
                let (big, mut hints) = big.add_context(context)?;
                let (small, small_hints) = small.add_context(context)?;
                hints.extend(small_hints);
                hints.push(Hint::CompareSets(
                    [big, small],
                    Comparison::MoreThan(excess),
                ));
                hints
            }
            Self::UnitSize(unit, quantity) => {
                let (set, mut hints) = unit.add_context(context)?;
                hints.push(Hint::Count(set, quantity));
                hints
            }
            Self::TotalUnitsSize(units, quantity, judgment) => {
                let (sets, mut hints) = units.add_context(context)?;
                let sets = sets.map(|set| set.judged(judgment));
                hints.push(Hint::CountTotal(sets, quantity));
                hints
            }
            Self::UniqueInUnitHasNNeighbors(unit, quantity, name, judgment) => {
                unit.unique_member_has_n_neighbors(quantity, judgment, name.as_ref(), context)?
            }

            Self::UniqueUnitInSeriesHasSize(series, quantity, judgment) => {
                let sets = series
                    .all(context)
                    .into_iter1()
                    .map(|set| set.judged(judgment))
                    .collect1();
                vec![Hint::unique_with_count(sets, quantity)]
            }
            Self::EachUnitInSeriesHasSize(kind, quantity, judgment) => kind
                .all(context)
                .into_iter()
                .map(|set| Hint::Count(set.judged(judgment), quantity))
                .collect(),
            Self::OnlyGivenUnitHasNTraits(unit, quantity, judgment) => {
                unit.only_one_with_n_traits(quantity, judgment, context)?
            }
            Self::UnitAndIntersectionSize {
                total: quantity,
                quantified,
                other,
                intersection,
                judgment,
            } => quantified.intersects_with(
                &other,
                Cardinal::Exact(intersection),
                Some(quantity),
                judgment,
                context,
            )?,
            Self::IntersectionSize([a, b], quantity, judgment) => {
                a.intersects_with(&b, quantity, None, judgment, context)?
            }
            Self::EqualNumberOfTraitsInUnits(units, judgment) => {
                let (sets, mut hints) = units.add_context(context)?;
                let sets = sets.map(|set| set.judged(judgment));
                hints.push(Hint::CompareSets(sets, Comparison::Equal));
                hints
            }
            Self::UnitEquallySplit(unit) => unit.equal_traits(context)?,
            Self::MoreTraitsInUnit(unit, judgment) => {
                let (set, mut hints) = unit.add_context(context)?;
                hints.push(Hint::CompareSets(
                    [set.clone().judged(judgment), set.judged(!judgment)],
                    Comparison::MoreThan(None),
                ));
                hints
            }
            Self::HasTrait(name, judgment) => {
                vec![Hint::Judgment(name.add_context(context)?, judgment)]
            }
            Self::EachInUnitHasAtMostNNeighbors(unit, number, judgment) => {
                unit.members_have_at_most_neighbors(number, judgment, context)?
            }
            Self::NInUnitHaveNNeighbors {
                unit,
                quantity,
                neighbors,
                judgment,
            } => unit.n_members_have_n_neighbors(quantity, neighbors, judgment, context)?,
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
    NotNeighbor(NameRecipe),
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
        judgment: Judgment,
        name: Option<&NameRecipe>,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<Hint>> {
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
                hints.push(Hint::Count(
                    coord.neighbors().collect::<Set>().judged(judgment),
                    quantity,
                ));
                hints.extend(
                    set.into_iter()
                        .filter(|&other| other != coord)
                        .map(|other| {
                            Hint::NotCount(
                                other.neighbors().collect::<Set>().judged(judgment),
                                quantity,
                            )
                        }),
                );
            } else {
                let Ok(set) = Set1::try_from(set) else {
                    bail!("empty unit {self:?} cannnot have unique member")
                };
                let sets = set
                    .into_iter1()
                    .map(|coord| coord.neighbors().collect::<Set>().judged(judgment))
                    .collect1();
                hints.push(Hint::unique_with_count(sets, quantity));
            }
        } else {
            let unique = Hint::CountWithNeighbors {
                set,
                each: quantity,
                count: Cardinal::Exact(1),
                judgment,
            };
            hints.push(unique);
            if let Some(coord) = coord {
                hints.push(Hint::Count(
                    coord.neighbors().collect::<Set>().judged(judgment),
                    quantity,
                ));
            }
        }
        Ok(hints)
    }

    pub(crate) fn intersects_with(
        &self,
        other: &Self,
        intersection: Cardinal,
        quantity: Option<Number>,
        judgment: Judgment,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<Hint>> {
        let ([self_, other], mut hints) = [self, other].add_context(context)?;
        let other = other.intersect(self_.clone()).judged(judgment);
        let self_ = self_.judged(judgment);
        hints.push(Hint::Count(other, intersection));
        if let Some(quantity) = quantity {
            hints.push(Hint::Count(self_, Cardinal::Exact(quantity)));
        }
        Ok(hints)
    }

    pub(crate) fn members_have_at_most_neighbors(
        &self,
        number: u8,
        judgment: Judgment,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        hints.push(Hint::EachNeighbors(set, Cardinal::AtMost(number), judgment));
        Ok(hints)
    }

    pub(crate) fn members_are_connected(self, context: Context<'_>) -> anyhow::Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        hints.push(Hint::Connected(set));
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
    pub(crate) fn not_neighbor(name: impl Into<NameRecipe>) -> Self {
        Self::NotNeighbor(name.into())
    }

    #[cfg(test)]
    pub(crate) fn direction(direction: Direction, name: impl Into<NameRecipe>) -> Self {
        Self::Direction(direction, name.into())
    }

    fn n_members_have_n_neighbors(
        self,
        quantity: Cardinal,
        neighbors: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        hints.push(Hint::CountWithNeighbors {
            set,
            count: quantity,
            each: neighbors,
            judgment,
        });
        Ok(hints)
    }

    pub(crate) fn with_judgment(self, judgment: Judgment) -> Self {
        Self::Judged(Box::new(self), judgment)
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        Self::Shifted(Box::new(self), direction)
    }

    fn equal_traits(&self, context: Context<'_>) -> Result<Vec<Hint>, anyhow::Error> {
        let (set, mut hints) = self.add_context(context)?;
        let extra = if let Some(set) = set.as_regular() {
            if !set.len().is_multiple_of(2) {
                bail!("{self:?} cannot be split equally")
            }
            let count = u8::try_from(set.len()).expect("at most 20") / 2;
            Hint::Count(set.judged(Judgment::Innocent), Cardinal::Exact(count))
        } else {
            Hint::CompareSets(
                [
                    set.clone().judged(Judgment::Innocent),
                    set.judged(Judgment::Criminal),
                ],
                Comparison::Equal,
            )
        };
        hints.push(extra);
        Ok(hints)
    }

    fn one_of_n_in_unit(
        &self,
        name: &NameRecipe,
        quantity: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>, anyhow::Error> {
        let (set, mut hints) = self.add_context(context)?;
        let coord = name.add_context(context)?;
        if set.as_regular().is_some_and(|set| !set.contains(coord)) {
            bail!("{name:?} does not belong to {self:?}")
        }
        hints.extend([
            Hint::Count(set.judged(judgment), quantity),
            Hint::Judgment(coord, judgment),
        ]);
        Ok(hints)
    }
}

impl AddContext for &Unit {
    type Output = (ModifiedSet, Vec<Hint>);

    fn add_context(self, context: Context<'_>) -> anyhow::Result<Self::Output> {
        let mut hints = Vec::new();
        let set: ModifiedSet = match self {
            &Unit::Line(line) => line.add_context(context)?.into(),
            Unit::Direction(direction, name) => {
                let start = name.add_context(context)?;
                Coordinate::direction(start, *direction).collect()
            }
            Unit::Neighbor(name) => name.add_context(context)?.neighbors().collect(),
            Unit::NotNeighbor(name) => name
                .add_context(context)?
                .neighbors()
                .collect::<Set>()
                .complement()
                .into(),
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
                hints.push(Hint::Count(set.clone(), Cardinal::Exact(*quantity)));
                set
            }
            Unit::Shifted(inner, direction) => {
                let set;
                (set, hints) = inner.add_context(context)?;
                set.shift(*direction)
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

impl AddContext for &[Unit; 2] {
    type Output = ([ModifiedSet; 2], Vec<Hint>);

    fn add_context(self, context: Context<'_>) -> anyhow::Result<Self::Output> {
        self.each_ref().add_context(context)
    }
}

impl AddContext for [&Unit; 2] {
    type Output = ([ModifiedSet; 2], Vec<Hint>);

    fn add_context(self, context: Context<'_>) -> anyhow::Result<Self::Output> {
        // TODO use `try_map`
        let [a, b] = self.each_ref().map(|unit| unit.add_context(context));
        let (a, mut hints) = a?;
        let (b, more_hints) = b?;
        hints.extend(more_hints);
        Ok(([a, b], hints))
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
    pub(crate) fn has_most(
        self,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>, anyhow::Error> {
        let small = self.others(context)?;
        let (big, mut hints) = Unit::from(self).add_context(context)?;
        let big = big.judged(judgment);
        hints.extend(small.into_iter().map(|other| {
            Hint::CompareSets(
                [big.clone(), other.judged(judgment)],
                Comparison::MoreThan(None),
            )
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
        judgment: Judgment,
        context: Context<'_>,
    ) -> anyhow::Result<Vec<Hint>> {
        let others = self.others(context)?;
        let (this, mut hints) = Unit::from(self).add_context(context)?;
        hints.push(Hint::Count(this.judged(judgment), quantity));
        hints.extend(
            others
                .into_iter()
                .map(|other| Hint::NotCount(other.judged(judgment), quantity)),
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
    Profession,
    Neighbor,
}

impl Series {
    pub(crate) fn all(self, context: Context) -> Vec1<Set> {
        match self {
            Self::Line(line_kind) => line_kind.all().into_iter1().map(Set::from).collect1(),
            Self::Profession => context
                .grid
                .by_profession()
                .values1()
                .map(|&set| set.into())
                .collect1(),
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
