use anyhow::{Context as _, Result, bail};
use mitsein::iter1::IntoIterator1 as _;
use mitsein::vec1::Vec1;

use crate::models::{Column, Coord, Direction, Judgment, Profession, Row};
use crate::solver::board::coordinates::{Set, Set1, SetExpr, SetOp};
use crate::solver::hint::recipes::{
    AddContext, ColumnRecipe, Context, LineRecipe, NameRecipe, RowRecipe,
};
use crate::solver::hint::{Cardinal, CardinalOrNot, Comparison, Hint, LineKind, Number, Parity};

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum Sentence {
    // This I think can't actually be "Me"
    HasTrait(NameRecipe, Judgment),
    UnitIsConnected(UnitExpr),
    BiggestInSeries(UnitInSeries, Judgment),
    IsOneOfNInUnit(Unit, NameRecipe, Cardinal, Judgment),
    EqualNumberOfTraitsInUnits([Unit; 2], Judgment),
    UnitBiggerThanUnit {
        big: UnitExpr,
        small: UnitExpr,
        excess: Option<Number>,
    },
    UnitEquallySplit(Unit),
    MoreTraitsInUnit(Unit, Judgment),
    UnitSize(UnitExpr, Cardinal),
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
        intersection: Cardinal,
        judgment: Judgment,
    },
    // TODO replace Quantifier with cardinal here
    IntersectionSize([Unit; 2], Quantifier, Judgment),
    EachInUnitHasNNeighbors(Unit, Cardinal, Judgment),
    TotalUnitsSize([Unit; 2], Cardinal, Judgment),
    TraitsInUnitAreInUnit {
        any_all: AnyAll,
        split: Unit,
        judgment: Judgment,
        other: Unit,
    },
    IsInUnit(Unit, NameRecipe, Judgment),
}

impl AddContext for &Sentence {
    type Output = Vec<Hint>;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let hints: Vec<Hint> = match self {
            Sentence::UnitIsConnected(unit) => unit.members_are_connected(context)?,
            Sentence::BiggestInSeries(unit, judgment) => unit.has_most(*judgment, context)?,
            Sentence::IsOneOfNInUnit(unit, name, quantity, judgment) => {
                unit.contains_in_n(name, *quantity, *judgment, context)?
            }
            Sentence::UnitBiggerThanUnit { big, small, excess } => {
                big.bigger_than(small, *excess, context)?
            }
            Sentence::UnitSize(unit, quantity) => {
                let (set, mut hints) = unit.add_context(context)?;
                match set.regular() {
                    Ok(set) if quantity.matches(set.len()) => {}
                    Ok(_) => bail!("{unit:?} does not match {quantity:?}"),
                    Err(set) => hints.push(Hint::Count(set, (*quantity).into())),
                }
                hints
            }
            Sentence::TotalUnitsSize(units, quantity, judgment) => {
                Unit::total_size(units, *quantity, *judgment, context)?
            }
            Sentence::UniqueInUnitHasNNeighbors(unit, quantity, name, judgment) => {
                unit.unique_member_has_n_neighbors(*quantity, *judgment, name.as_ref(), context)?
            }

            &Sentence::UniqueUnitInSeriesHasSize(series, count, judgment) => {
                let sets = series
                    .all(context)
                    .into_iter1()
                    .map(|set| set.judged(judgment))
                    .collect1();
                vec![Hint::UniqueWithCount { sets, count }]
            }
            &Sentence::EachUnitInSeriesHasSize(kind, quantity, judgment) => kind
                .all(context)
                .into_iter()
                .map(|set| Hint::Count(set.judged(judgment), quantity.into()))
                .collect(),
            Sentence::OnlyGivenUnitHasNTraits(unit, quantity, judgment) => {
                unit.only_one_with_n_traits(*quantity, *judgment, context)?
            }
            Sentence::UnitAndIntersectionSize {
                total,
                quantified,
                other,
                intersection,
                judgment,
            } => quantified.and_intersection(*total, other, *intersection, *judgment, context)?,
            Sentence::IntersectionSize([a, b], quantity, judgment) => {
                a.intersection(b, *quantity, *judgment, context)?
            }
            Sentence::EqualNumberOfTraitsInUnits(units, judgment) => {
                let sets = units.add_context(context)?;
                match sets.map(Set::non_empty) {
                    [None, None] => Vec::new(),
                    [None, Some(set)] | [Some(set), None] => {
                        vec![Hint::Count(set.judged(*judgment), CardinalOrNot::Exact(0))]
                    }
                    [Some(a), Some(b)] => {
                        let sets = [a, b].map(|set| set.judged(*judgment));
                        vec![Hint::CompareSets(sets, Comparison::ExactDifference(0))]
                    }
                }
            }
            Sentence::UnitEquallySplit(unit) => unit.equal_traits(context)?,
            Sentence::MoreTraitsInUnit(unit, judgment) => {
                let set = unit.add_context(context)?;
                let Some(set) = set.non_empty() else {
                    bail!("{unit:?} is empty")
                };
                let hint = match [set.judged(*judgment), set.judged(!*judgment)] {
                    [big, small] => Hint::CompareSets([big, small], Comparison::More),
                };
                vec![hint]
            }
            Sentence::HasTrait(name, judgment) => {
                vec![Hint::Judgment(name.add_context(context)?, *judgment)]
            }
            Sentence::EachInUnitHasNNeighbors(unit, count, judgment) => {
                unit.members_have_at_most_neighbors(*count, *judgment, context)?
            }
            Sentence::NInUnitHaveNNeighbors {
                unit,
                quantity,
                neighbors,
                judgment,
            } => unit.n_members_have_n_neighbors(*quantity, *neighbors, *judgment, context)?,
            Sentence::TraitsInUnitAreInUnit {
                any_all,
                split,
                judgment,
                other,
            } => split.traits_are_in_unit(*any_all, *judgment, other, context)?,
            Sentence::IsInUnit(unit, name, judgment) => unit.contains(name, *judgment, context)?,
        };
        Ok(hints)
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) enum UnitExpr {
    Regular(Unit),
    Shifted(Box<Self>, Direction),
    Quantified(Box<Self>, Cardinal),
    Judged(Box<Self>, Judgment),
}

impl UnitExpr {
    pub(crate) fn with_judgment(self, judgment: Judgment) -> Self {
        Self::Judged(Box::new(self), judgment)
    }

    pub(crate) fn quantify(self, quantity: impl Into<Cardinal>) -> Self {
        if let Self::Regular(unit) = self {
            Self::Regular(unit.quantify(quantity))
        } else {
            Self::Quantified(Box::new(self), quantity.into())
        }
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        if let Self::Regular(unit) = self {
            Self::Regular(unit.shift(direction))
        } else {
            Self::Shifted(Box::new(self), direction)
        }
    }

    fn members_are_connected(&self, context: Context<'_>) -> Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        match set.regular() {
            Ok(set) if !set.connected() => bail!("{self:?} is not connected"),
            Ok(_) => {}
            Err(set) => hints.push(Hint::Connected(set)),
        }
        Ok(hints)
    }

    fn bigger_than(
        &self,
        small_unit: &Self,
        excess: Option<u8>,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let (big, mut hints) = self.add_context(context)?;
        let (small, small_hints) = small_unit.add_context(context)?;
        hints.extend(small_hints);
        let compare = excess.map_or(Comparison::More, Comparison::ExactDifference);
        match [big, small].map(SetExpr::regular) {
            [Ok(big), Ok(small)] => {
                let matches = match compare {
                    Comparison::ExactDifference(diff) => big.len() == small.len().strict_add(diff),
                    Comparison::More => big.len() > small.len(),
                };
                if !matches {
                    bail!("{self:?} is not bigger than {small_unit:?} by {compare:?}")
                }
            }
            [Ok(big), Err(small)] => {
                let count = match compare {
                    Comparison::ExactDifference(diff) => {
                        big.len().checked_sub(diff).map(Cardinal::Exact)
                    }
                    Comparison::More => big.len().checked_sub(1).map(Cardinal::AtMost),
                }
                .with_context(|| format!("{self:?} cannot be bigger by {compare:?}"))?;
                hints.push(Hint::Count(small, count.into()));
            }
            [Err(big), Ok(small)] => {
                let count = match compare {
                    Comparison::ExactDifference(diff) => {
                        Cardinal::Exact(small.len().strict_add(diff))
                    }
                    Comparison::More => Cardinal::AtLeast(small.len().strict_add(1)),
                };
                hints.push(Hint::Count(big, count.into()));
            }
            [Err(big), Err(small)] => {
                hints.push(Hint::CompareSets([big, small], compare));
            }
        }
        Ok(hints)
    }
}

impl From<Unit> for UnitExpr {
    fn from(v: Unit) -> Self {
        Self::Regular(v)
    }
}

impl AddContext for &UnitExpr {
    type Output = (SetExpr, Vec<Hint>);

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let mut hints = Vec::new();
        let set;
        let set: SetExpr = match self {
            UnitExpr::Regular(unit) => unit.add_context(context)?.into(),
            UnitExpr::Quantified(inner, quantity) => {
                (set, hints) = inner.add_context(context)?;
                match set.regular() {
                    Ok(set) if quantity.matches(set.len()) => set.into(),
                    Ok(_) => bail!("{inner:?} does not have size {quantity:?}"),
                    Err(set) => {
                        hints.push(Hint::Count(set.clone(), (*quantity).into()));
                        set.into()
                    }
                }
            }
            UnitExpr::Shifted(inner, direction) => {
                (set, hints) = inner.add_context(context)?;
                set.shift(*direction)
            }
            UnitExpr::Judged(inner, judgment) => {
                (set, hints) = inner.add_context(context)?;
                set.judged(*judgment).into()
            }
        };
        Ok((set, hints))
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
    NotName(NameRecipe),
    Edges,
    Corners,
    All,
    Shifted(Box<Self>, Direction),
    Quantified(Box<Self>, Cardinal),
}

impl Unit {
    fn unique_member_has_n_neighbors(
        &self,
        count: Cardinal,
        judgment: Judgment,
        name: Option<&NameRecipe>,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        let coord = name
            .as_ref()
            .map(|name| name.add_context(context))
            .transpose()?;
        let Some(set) = set.non_empty() else {
            bail!("empty unit {self:?} cannnot have unique member")
        };
        let hints = if let Some(coord) = coord {
            if !set.contains(coord) {
                bail!("{name:?} does not belong to {self:?}")
            }
            let mut hints = vec![Hint::Count(
                coord.neighbors().judged(judgment),
                count.into(),
            )];
            if let Some(others) = (set ^ Set1::from_one(coord)).non_empty() {
                let count = count
                    .not()
                    .with_context(|| format!("impossible to not have {count:?}"))?;
                hints.extend(
                    others
                        .into_iter()
                        .map(|other| Hint::Count(other.neighbors().judged(judgment), count)),
                );
            }
            hints
        } else {
            let sets = set
                .into_iter1()
                .map(|coord| coord.neighbors().judged(judgment))
                .collect1();
            vec![Hint::UniqueWithCount { sets, count }]
        };
        Ok(hints)
    }

    fn and_intersection(
        &self,
        total: Number,
        other: &Self,
        intersection: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let [self_, other] = [self, other].add_context(context)?;
        match self_.non_empty() {
            None => {
                if total != 0 || !intersection.matches(0) {
                    bail!("{self:?} is empty");
                }
                Ok(Vec::new())
            }
            Some(self_) => {
                let self_ = self_.judged(judgment);
                let mut hints = vec![Hint::Count(self_.clone(), CardinalOrNot::Exact(total))];
                let other = self_.intersect_set(other).judged(judgment);
                match other {
                    SetOp::Empty => {
                        if !intersection.matches(0) {
                            bail!("intersection of {self:?} and {other:?} is empty")
                        }
                    }
                    SetOp::NonEmpty(other) => {
                        hints.push(Hint::Count(other, intersection.into()));
                    }
                }
                Ok(hints)
            }
        }
    }

    fn intersection(
        &self,
        other_unit: &Self,
        intersection: Quantifier,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let [self_, other] = [self, other_unit].add_context(context)?;
        let set = other & self_;
        match set.non_empty() {
            None => {
                let forced_non_empty = match intersection {
                    Quantifier::Simple(cardinal) => !cardinal.matches(0),
                    Quantifier::Subset(_, total) => total != 0,
                };
                if forced_non_empty {
                    bail!("{self:?} intersection {other_unit:?} is empty");
                }
                Ok(Vec::new())
            }
            Some(set) => {
                let intersection = match intersection {
                    Quantifier::Simple(intersection) => intersection,
                    Quantifier::Subset(intersection, total) => {
                        if total != set.len().get() {
                            bail!(
                                "{self:?} intersection {other_unit:?} does not have size {total}"
                            );
                        }
                        intersection
                    }
                };
                Ok(vec![Hint::Count(set.judged(judgment), intersection.into())])
            }
        }
    }

    fn members_have_at_most_neighbors(
        &self,
        count: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        let hints = set
            .non_empty()
            .map_or_default(|set| vec![Hint::EachNeighbors(set.into(), count, judgment)]);
        Ok(hints)
    }

    fn total_size(
        units: &[Self; 2],
        quantity: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let sets = units.add_context(context)?;
        let hints = match sets.map(Set::non_empty) {
            [None, None] => {
                if !quantity.matches(0) {
                    bail!("{units:?} are both empty so cannot total to {quantity:?}")
                }
                Vec::new()
            }
            [None, Some(set)] | [Some(set), None] => {
                vec![Hint::Count(set.judged(judgment), quantity.into())]
            }
            [Some(a), Some(b)] => {
                let sets = [a, b].map(|set| set.judged(judgment));
                vec![Hint::CountTotal(sets, quantity)]
            }
        };
        Ok(hints)
    }

    pub(crate) fn quantify(self, quantity: impl Into<Cardinal>) -> Self {
        Self::Quantified(Box::new(self), quantity.into())
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

    #[cfg(test)]
    pub(crate) fn not_name(name: impl Into<NameRecipe>) -> Self {
        Self::NotName(name.into())
    }

    fn n_members_have_n_neighbors(
        &self,
        count: Cardinal,
        each: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        if let Some(set) = set.non_empty() {
            let hints = vec![Hint::CountWithNeighbors {
                set: set.into(),
                count,
                each,
                judgment,
            }];
            Ok(hints)
        } else if !count.matches(0) {
            bail!("{self:?} is empty so cannot have {count:?} members");
        } else {
            Ok(Vec::new())
        }
    }

    pub(crate) fn with_judgment(self, judgment: Judgment) -> UnitExpr {
        UnitExpr::from(self).with_judgment(judgment)
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        Self::Shifted(Box::new(self), direction)
    }

    fn equal_traits(&self, context: Context<'_>) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        let hints = if let Some(set) = set.non_empty() {
            if !set.len().get().is_multiple_of(2) {
                bail!("{self:?} cannot be split equally")
            }
            let count = set.len().get() / 2;
            let extra = Hint::Count(set.judged(Judgment::Innocent), CardinalOrNot::Exact(count));
            vec![extra]
        } else {
            Vec::new()
        };
        Ok(hints)
    }

    fn contains_in_n(
        &self,
        name: &NameRecipe,
        quantity: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        let coord = name.add_context(context)?;
        let Some(set) = set.non_empty() else {
            bail!("{self:?} is empty")
        };
        set.assert_contains(coord)?;
        Ok(vec![
            Hint::Judgment(coord, judgment),
            Hint::Count(set.judged(judgment), quantity.into()),
        ])
    }

    fn traits_are_in_unit(
        &self,
        any_all: AnyAll,
        judgment: Judgment,
        other: &Self,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let [self_, other] = [self, other].add_context(context)?;
        let mut hints = Vec::new();
        if let Some(self_) = self_.non_empty() {
            let split = self_.judged(judgment);
            if any_all.is_all() {
                hints.push(Hint::Count(split.clone(), CardinalOrNot::AtLeast(1)));
            }
            let hint = match split.clone().intersect_set(other) {
                SetOp::Empty => Hint::Count(split, CardinalOrNot::Exact(0)),
                SetOp::NonEmpty(intersection) => {
                    Hint::CompareSets([split, intersection], Comparison::ExactDifference(0))
                }
            };
            hints.push(hint);
        } else if any_all.is_all() {
            bail!("\"All\" should mean at least one, but {self:?} is empty")
        }
        Ok(hints)
    }

    fn contains(
        &self,
        name: &NameRecipe,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        let coord = name.add_context(context)?;
        let Some(set) = set.non_empty() else {
            bail!("{self:?} is empty")
        };
        set.assert_contains(coord)?;
        Ok(vec![Hint::Judgment(coord, judgment)])
    }
}

impl AddContext for &Unit {
    type Output = Set;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let set = match self {
            &Unit::Line(line) => line.add_context(context)?.into(),
            Unit::Direction(direction, name) => {
                let start = name.add_context(context)?;
                Coord::direction(start, *direction).collect()
            }
            Unit::Neighbor(name) => name.add_context(context)?.neighbors().into(),
            Unit::NotNeighbor(name) => {
                Set::from(name.add_context(context)?.neighbors()).complement()
            }
            Unit::Profession(profession) => (*context.profession_as_set(profession)?).into(),
            Unit::Edges => Set1::edges().into(),
            Unit::Corners => Set1::corners().into(),
            Unit::Between(names) => {
                let [a, b] = names.each_ref().map(|name| name.add_context(context));
                Set::between([a?, b?])?
            }
            Unit::All => Coord::all().into(),
            Unit::Quantified(inner, quantity) => {
                let set = inner.add_context(context)?;
                if quantity.matches(set.len()) {
                    set
                } else {
                    bail!("{inner:?} does not have size {quantity:?}")
                }
            }
            Unit::NotName(name) => {
                let coord = name.add_context(context)?;
                Set1::from_one(coord).complement()
            }
            Unit::Shifted(inner, direction) => inner.add_context(context)?.shift(*direction),
        };
        Ok(set)
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
    type Output = [Set; 2];

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        self.each_ref().add_context(context)
    }
}

impl AddContext for [&Unit; 2] {
    type Output = [Set; 2];

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        // TODO use `try_map()` https://github.com/rust-lang/rust/issues/79711
        let [a, b] = self.map(|unit| unit.add_context(context));
        Ok([a?, b?])
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
    fn has_most(&self, judgment: Judgment, context: Context<'_>) -> Result<Vec<Hint>> {
        let small = self.others(context)?;
        let big = self.add_context(context)?.judged(judgment);
        let hints = small
            .into_iter()
            .map(|other| Hint::CompareSets([big.clone(), other.judged(judgment)], Comparison::More))
            .collect();
        Ok(hints)
    }

    fn others(&self, context: Context<'_>) -> Result<Vec<Set1>> {
        let others = match self {
            Self::Line(line) => line
                .add_context(context)?
                .others()
                .into_iter()
                .map(Set1::from)
                .collect(),
            Self::Profession(profession) => context
                .by_profession
                .as_btree_map()
                .iter()
                .filter(move |&(other, _)| other != profession)
                .map(|(_, &set)| set)
                .collect(),
            Self::Neighbor(name) => {
                let coord = name.add_context(context)?;
                Coord::all()
                    .into_iter()
                    .filter(|&other| other != coord)
                    .map(Coord::neighbors)
                    .collect()
            }
        };
        Ok(others)
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
        &self,
        quantity: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let others = self.others(context)?;
        let this = self.add_context(context)?;
        let mut hints = vec![Hint::Count(this.judged(judgment), quantity.into())];
        if !others.is_empty() {
            let quantity = quantity
                .not()
                .with_context(|| format!("impossible to not have {quantity:?}"))?;
            hints.extend(
                others
                    .into_iter()
                    .map(|other| Hint::Count(other.judged(judgment), quantity)),
            );
        }
        Ok(hints)
    }
}

impl AddContext for &UnitInSeries {
    type Output = Set1;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let set = match self {
            UnitInSeries::Line(line) => line.add_context(context)?.into(),
            UnitInSeries::Profession(profession) => *context
                .by_profession
                .get(profession)
                .with_context(|| format!("{profession} not in puzzle"))?,
            UnitInSeries::Neighbor(suspect) => suspect.add_context(context)?.neighbors(),
        };
        Ok(set)
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
    fn all(self, context: Context<'_>) -> Vec1<Set1> {
        match self {
            Self::Line(line_kind) => line_kind.all().into_iter1().map(Set1::from).collect1(),
            Self::Profession => context.by_profession.values1().copied().collect1(),
            Self::Neighbor => Coord::all().into_iter1().map(Coord::neighbors).collect1(),
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
    Subset(Cardinal, Number),
}

impl Quantifier {
    pub(crate) fn to_cardinal(self) -> Option<Cardinal> {
        match self {
            Self::Subset(Cardinal::Exact(exact) | Cardinal::AtLeast(exact), total)
                if exact == total =>
            {
                Some(Cardinal::Exact(exact))
            }
            Self::Subset(Cardinal::Parity(Parity::Even), 0) => Some(Cardinal::Exact(0)),
            Self::Subset(Cardinal::Parity(Parity::Odd), 1) => Some(Cardinal::Exact(1)),
            Self::Simple(cardinal) => Some(cardinal),
            Self::Subset(_, _) => None,
        }
    }
}

impl From<Cardinal> for Quantifier {
    fn from(v: Cardinal) -> Self {
        Self::Simple(v)
    }
}

impl From<Parity> for Quantifier {
    fn from(v: Parity) -> Self {
        Self::Simple(v.into())
    }
}

impl From<Number> for Quantifier {
    fn from(v: Number) -> Self {
        Self::Simple(v.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MoreOrLess {
    More,
    Less,
}

impl MoreOrLess {
    pub(crate) fn big<T>(self, [left, right]: [T; 2]) -> T {
        match self {
            Self::More => left,
            Self::Less => right,
        }
    }

    pub(crate) fn big_small<T>(self, [left, right]: [T; 2]) -> [T; 2] {
        match self {
            Self::More => [left, right],
            Self::Less => [right, left],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AnyAll {
    Any,
    All,
}

impl AnyAll {
    /// Returns `true` if the any all is [`All`].
    ///
    /// [`All`]: AnyAll::All
    #[must_use]
    pub(crate) fn is_all(self) -> bool {
        matches!(self, Self::All)
    }
}
