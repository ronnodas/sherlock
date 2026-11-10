use anyhow::{Context as _, Result, bail};
use mitsein::iter1::IntoIterator1 as _;
use mitsein::vec1::Vec1;

use crate::models::{Column, Coord, Direction, Judged, Judgment, Name, Profession, Row};
use crate::solver::board::coordinates::{Set, Set1};
use crate::solver::hint::recipes::{
    AddContext, ColumnRecipe, Context, LineRecipe, NameRecipe, RowRecipe,
};
use crate::solver::hint::{
    Bound, BoundOrNot, Comparison, Hint, LineKind, Number, Parity, ShiftedCount,
};

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum Sentence {
    HasTrait(Name, Judgment),
    UnitIsConnected(Unit, Judgment, Option<Bound>),
    BiggestInSeries(UnitInSeries, Judgment),
    IsOneOfNInUnit(Unit, NameRecipe, Bound, Judgment),
    EqualNumberOfTraitsInUnits([Unit; 2], Judgment),
    UnitBiggerThanUnit {
        big: Judged<Unit>,
        small: Judged<Unit>,
        excess: Option<Number>,
    },
    UnitEquallySplit(Unit),
    MoreTraitsInUnit(Unit, Judgment),
    UnitSize(Unit, Judgment, Bound),
    TraitsInUnitHaveTraitInDirection {
        source: Unit,
        source_judgment: Judgment,
        direction: Direction,
        target_judgment: Judgment,
        bound: Bound,
    },
    UniqueInUnitHasNNeighbors(Unit, Bound, Option<NameRecipe>, Judgment),
    NInUnitHaveNNeighbors {
        unit: Unit,
        count: Bound,
        each: Bound,
        judgment: Judgment,
    },
    EachUnitInSeriesHasSize(Series, Bound, Judgment),
    UniqueUnitInSeriesHasSize(Series, Bound, Judgment),
    OnlyGivenUnitHasNTraits(UnitInSeries, Bound, Judgment),
    UnitAndIntersectionSize {
        total: Number,
        split: Unit,
        other: Unit,
        intersection: Bound,
        judgment: Judgment,
    },
    IntersectionSize([Unit; 2], BoundPair, Judgment),
    EachInUnitHasNNeighbors(Unit, Bound, Judgment),
    TotalUnitsSize([Unit; 2], Bound, Judgment),
    TraitsInUnitAreInUnit {
        any_all: AnyAll,
        split: Unit,
        judgment: Judgment,
        other: Unit,
    },
    IsInUnit(Unit, NameRecipe, Judgment),
    IfThen([Judged<NameRecipe>; 2]),
    OnlyTraitInSeries(NameRecipe, DisjointSeries, Judgment),
}

impl AddContext for &Sentence {
    type Output = Vec<Hint>;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let hints: Vec<Hint> = match self {
            Sentence::UnitIsConnected(unit, judgment, bound) => {
                unit.members_are_connected(*judgment, *bound, context)?
            }
            Sentence::BiggestInSeries(unit, judgment) => unit.has_most(*judgment, context)?,
            Sentence::IsOneOfNInUnit(unit, name, bound, judgment) => {
                unit.contains_in_n(name, *bound, *judgment, context)?
            }
            Sentence::UnitBiggerThanUnit { big, small, excess } => {
                big.bigger_than(small, *excess, context)?
            }
            Sentence::UnitSize(unit, judgment, bound) => {
                unit.has_size(*judgment, *bound, context)?
            }
            Sentence::TotalUnitsSize(units, bound, judgment) => {
                Unit::total_size(units, *bound, *judgment, context)?
            }
            Sentence::UniqueInUnitHasNNeighbors(unit, bound, name, judgment) => {
                unit.unique_member_has_n_neighbors(*bound, *judgment, name.as_ref(), context)?
            }

            &Sentence::UniqueUnitInSeriesHasSize(series, bound, judgment) => {
                vec![Hint::UniqueWithCount {
                    sets: series.all(context),
                    judgment,
                    bound,
                }]
            }
            &Sentence::EachUnitInSeriesHasSize(kind, bound, judgment) => kind
                .all(context)
                .into_iter()
                .map(|set| Hint::Count(set, judgment, bound.into()))
                .collect(),
            Sentence::OnlyGivenUnitHasNTraits(unit, bound, judgment) => {
                unit.only_one_with_n_traits(*bound, *judgment, context)?
            }
            Sentence::UnitAndIntersectionSize {
                total,
                split,
                other,
                intersection,
                judgment,
            } => split.and_intersection(*total, other, *intersection, *judgment, context)?,
            Sentence::IntersectionSize([a, b], bound_pair, judgment) => {
                a.intersection(b, *bound_pair, *judgment, context)?
            }
            Sentence::EqualNumberOfTraitsInUnits(units, judgment) => {
                Unit::equal_traits(units, *judgment, context)?
            }
            Sentence::UnitEquallySplit(unit) => unit.equally_split(context)?,
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
                vec![Hint::Judgment(context.coord(name)?, *judgment)]
            }
            Sentence::EachInUnitHasNNeighbors(unit, bound, judgment) => {
                unit.members_have_at_most_neighbors(*bound, *judgment, context)?
            }
            Sentence::NInUnitHaveNNeighbors {
                unit,
                count,
                each,
                judgment,
            } => unit.n_members_have_n_neighbors(*count, *each, *judgment, context)?,
            Sentence::TraitsInUnitAreInUnit {
                any_all,
                split,
                judgment,
                other,
            } => split.traits_are_in_unit(*any_all, *judgment, other, context)?,
            Sentence::IsInUnit(unit, name, judgment) => unit.contains(name, *judgment, context)?,
            Sentence::IfThen(clauses) => {
                vec![Hint::IfThen(clauses.each_ref().add_context(context)?)]
            }
            Sentence::OnlyTraitInSeries(name, series, judgment) => {
                series.only_trait(name, *judgment, context)?
            }
            Sentence::TraitsInUnitHaveTraitInDirection {
                source,
                source_judgment,
                direction,
                target_judgment,
                bound,
            } => source.shifted_count(
                *source_judgment,
                *direction,
                *target_judgment,
                *bound,
                context,
            )?,
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
    NotName(NameRecipe),
    Edges,
    Corners,
    All,
    Shifted(Box<Self>, Direction),
    Bounded(Box<Self>, Bound),
}

impl Unit {
    fn unique_member_has_n_neighbors(
        &self,
        bound: Bound,
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
            let mut hints = vec![Hint::Count(coord.neighbors(), judgment, bound.into())];
            if let Some(others) = (set ^ Set1::from_one(coord)).non_empty() {
                let bound = bound
                    .not()
                    .with_context(|| format!("impossible to not have size {bound:?}"))?;
                hints.extend(
                    others
                        .into_iter()
                        .map(|other| Hint::Count(other.neighbors(), judgment, bound)),
                );
            }
            hints
        } else {
            let sets = set.into_iter1().map(Coord::neighbors).collect1();
            vec![Hint::UniqueWithCount {
                sets,
                judgment,
                bound,
            }]
        };
        Ok(hints)
    }

    fn and_intersection(
        &self,
        total: Number,
        other: &Self,
        intersection: Bound,
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
                let mut hints = vec![Hint::Count(self_, judgment, BoundOrNot::Exact(total))];
                match (self_ & other).non_empty() {
                    None if !intersection.matches(0) => {
                        bail!("intersection of {self:?} and {other:?} is empty")
                    }
                    None => {}
                    Some(other) => hints.push(Hint::Count(other, judgment, intersection.into())),
                }
                Ok(hints)
            }
        }
    }

    fn intersection(
        &self,
        other_unit: &Self,
        bound: BoundPair,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let [self_, other] = [self, other_unit].add_context(context)?;
        let set = other & self_;
        match set.non_empty() {
            None => {
                let forced_non_empty = match bound {
                    BoundPair::Simple(bound) => !bound.matches(0),
                    BoundPair::Subset { total, .. } => total != 0,
                };
                if forced_non_empty {
                    bail!("{self:?} intersection {other_unit:?} is empty");
                }
                Ok(Vec::new())
            }
            Some(set) => {
                let intersection = match bound {
                    BoundPair::Simple(intersection) => intersection,
                    BoundPair::Subset {
                        matching: intersection,
                        total,
                    } => {
                        if total != set.len().get() {
                            bail!(
                                "{self:?} intersection {other_unit:?} does not have size {total}"
                            );
                        }
                        intersection
                    }
                };
                Ok(vec![Hint::Count(set, judgment, intersection.into())])
            }
        }
    }

    fn members_have_at_most_neighbors(
        &self,
        bound: Bound,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        let hints = set
            .non_empty()
            .map_or_default(|set| vec![Hint::EachNeighbors(set, bound, judgment)]);
        Ok(hints)
    }

    fn total_size(
        units: &[Self; 2],
        bound: Bound,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let sets = units.add_context(context)?;
        let hints = match sets.map(Set::non_empty) {
            [None, None] => {
                if !bound.matches(0) {
                    bail!("{units:?} are both empty so cannot total to {bound:?}")
                }
                Vec::new()
            }
            [None, Some(set)] | [Some(set), None] => {
                vec![Hint::Count(set, judgment, bound.into())]
            }
            [Some(a), Some(b)] => {
                let sets = [a, b].map(|set| set.judged(judgment));
                vec![Hint::CountTotal(sets, bound)]
            }
        };
        Ok(hints)
    }

    pub(crate) fn bound(self, bound: impl Into<Bound>) -> Self {
        Self::Bounded(Box::new(self), bound.into())
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
        count: Bound,
        each: Bound,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        if let Some(set) = set.non_empty() {
            let hints = vec![Hint::CountWithNeighbors {
                set,
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

    pub(crate) fn judged(self, judgment: Judgment) -> Judged<Self> {
        Judged(self, judgment)
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        Self::Shifted(Box::new(self), direction)
    }

    fn equally_split(&self, context: Context<'_>) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        let hints = if let Some(set) = set.non_empty() {
            if !set.len().get().is_multiple_of(2) {
                bail!("{self:?} cannot be split equally")
            }
            let half = set.len().get() / 2;
            let extra = Hint::Count(set, Judgment::Innocent, BoundOrNot::Exact(half));
            vec![extra]
        } else {
            Vec::new()
        };
        Ok(hints)
    }

    fn contains_in_n(
        &self,
        name: &NameRecipe,
        bound: Bound,
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
            Hint::Count(set, judgment, bound.into()),
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
            if any_all.is_all() {
                hints.push(Hint::Count(self_, judgment, BoundOrNot::AtLeast(1)));
            }
            let hint = (self_ & other).non_empty().map_or_else(
                || Hint::Count(self_, judgment, BoundOrNot::Exact(0)),
                |intersection| {
                    Hint::CompareSets(
                        [self_, intersection].map(|set| set.judged(judgment)),
                        Comparison::ExactDifference(0),
                    )
                },
            );
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

    fn equal_traits(
        units: &[Self; 2],
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let sets = units.add_context(context)?;
        let hints = match sets.map(Set::non_empty) {
            [None, None] => Vec::new(),
            [None, Some(set)] | [Some(set), None] => {
                vec![Hint::Count(set, judgment, BoundOrNot::Exact(0))]
            }
            [Some(a), Some(b)] => {
                let sets = [a, b].map(|set| set.judged(judgment));
                vec![Hint::CompareSets(sets, Comparison::ExactDifference(0))]
            }
        };
        Ok(hints)
    }

    #[cfg(test)]
    pub(crate) fn between(names: [impl Into<NameRecipe>; 2]) -> Self {
        Self::Between(names.map(Into::into))
    }

    fn members_are_connected(
        &self,
        judgment: Judgment,
        bound: Option<Bound>,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let set = self.add_context(context)?;
        let hints = match (set.non_empty(), bound) {
            (None, _) if !Set::empty().connected() => bail!("{self:?} is not connected"),
            (None, Some(bound)) if !bound.matches(0) => bail!("{self:?} is empty, not {bound:?}"),
            (None, Some(_) | None) => vec![],
            (Some(set), None) => vec![Hint::Connected(set, judgment)],
            (Some(set), Some(bound)) => vec![
                Hint::Count(set, judgment, bound.into()),
                Hint::Connected(set, judgment),
            ],
        };
        Ok(hints)
    }

    fn has_size(
        &self,
        judgment: Judgment,
        bound: Bound,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let hints = match self.add_context(context)?.non_empty() {
            None if bound.matches(0) => Vec::new(),
            None => bail!("{self:?} is empty, not {bound:?}"),
            Some(set) => vec![Hint::Count(set, judgment, bound.into())],
        };
        Ok(hints)
    }

    fn shifted_count(
        &self,
        source_judgment: Judgment,
        direction: Direction,
        target_judgment: Judgment,
        bound: Bound,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let hints = match self.add_context(context)?.non_empty() {
            Some(source) => {
                vec![Hint::ShiftedCount(ShiftedCount {
                    source,
                    source_judgment,
                    direction,
                    target_judgment,
                    bound,
                })]
            }
            None if bound.matches(0) => vec![],
            None => bail!("{self:?} is empty, not {bound:?}"),
        };
        Ok(hints)
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
            Unit::Profession(profession) => context.profession_as_set(profession)?.into(),
            Unit::Edges => Set1::edges().into(),
            Unit::Corners => Set1::corners().into(),
            Unit::Between(names) => {
                let [a, b] = names.each_ref().map(|name| name.add_context(context));
                Set::between([a?, b?])?
            }
            Unit::All => Coord::all().into(),
            Unit::Bounded(inner, bound) => {
                let set = inner.add_context(context)?;
                if bound.matches(set.len()) {
                    set
                } else {
                    bail!("{inner:?} does not have size {bound:?}")
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
            .map(|other| Hint::CompareSets([big, other.judged(judgment)], Comparison::More))
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
            Self::Profession(profession) => {
                let profession = context.normalize_profession(profession)?;
                context
                    .by_profession
                    .as_btree_map()
                    .iter()
                    .filter(move |&(other, _)| other != profession)
                    .map(|(_, &set)| set)
                    .collect()
            }
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
        bound: Bound,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let others = self.others(context)?;
        let this = self.add_context(context)?;
        let mut hints = vec![Hint::Count(this, judgment, bound.into())];
        if !others.is_empty() {
            let bound = bound
                .not()
                .with_context(|| format!("impossible to not have {bound:?}"))?;
            hints.extend(
                others
                    .into_iter()
                    .map(|other| Hint::Count(other, judgment, bound)),
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
            UnitInSeries::Profession(profession) => context.profession_as_set(profession)?,
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

impl Judged<Unit> {
    fn bigger_than(
        &self,
        small_unit: &Self,
        excess: Option<u8>,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let big = self.add_context(context)?;
        let small = small_unit.add_context(context)?;
        let compare = excess.map_or(Comparison::More, Comparison::ExactDifference);
        let hints = match [big, small].map(Judged::<Set>::non_empty) {
            [None, None] => {
                let matches = match compare {
                    Comparison::ExactDifference(diff) => diff == 0,
                    Comparison::More => false,
                };
                if !matches {
                    bail!("{self:?} is not bigger than {small_unit:?} by {compare:?}, both empty")
                }
                vec![]
            }
            [None, Some(small)] => match compare {
                Comparison::ExactDifference(0) => {
                    let Judged(small, judgment) = small;
                    vec![Hint::Count(small, judgment, BoundOrNot::Exact(0))]
                }
                Comparison::ExactDifference(_) | Comparison::More => {
                    bail!("{self:?} is empty, cannot be bigger by {compare:?}")
                }
            },
            [Some(big), None] => {
                let bound = match compare {
                    Comparison::ExactDifference(diff) => Bound::Exact(diff),
                    Comparison::More => Bound::AtLeast(1),
                };
                let Judged(big, judgment) = big;
                vec![Hint::Count(big, judgment, bound.into())]
            }
            [Some(big), Some(small)] => {
                vec![Hint::CompareSets([big, small], compare)]
            }
        };
        Ok(hints)
    }
}

impl AddContext for &Judged<Unit> {
    type Output = Judged<Set>;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let Judged(inner, judgment) = self;
        Ok(Judged(inner.add_context(context)?, *judgment))
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

impl From<DisjointSeries> for Series {
    fn from(series: DisjointSeries) -> Self {
        match series {
            DisjointSeries::Line(kind) => Self::Line(kind),
            DisjointSeries::Profession => Self::Profession,
        }
    }
}

/// Series whose sets are disjoint, in particular, given a `Coord`, determines a `UnitInSeries`
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DisjointSeries {
    Line(LineKind),
    Profession,
}

impl DisjointSeries {
    fn add_context(self, coord: Coord, context: Context<'_>) -> Set1 {
        match self {
            Self::Line(kind) => kind.of(coord).into(),
            Self::Profession => *context
                .by_profession
                .as_btree_map()
                .values()
                .find(|set| set.contains(coord))
                .expect("partition of coordinates"),
        }
    }

    fn only_trait(
        self,
        name: &NameRecipe,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let coord = name.add_context(context)?;
        let set = self.add_context(coord, context);
        Ok(vec![
            Hint::Judgment(coord, judgment),
            Hint::Count(set, judgment, BoundOrNot::Exact(1)),
        ])
    }
}

impl From<LineKind> for DisjointSeries {
    fn from(v: LineKind) -> Self {
        Self::Line(v)
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone, Copy)]
pub(crate) enum BoundPair {
    Simple(Bound),
    Subset { matching: Bound, total: Number },
}

impl BoundPair {
    pub(crate) fn to_bound(self) -> Option<Bound> {
        match self {
            Self::Subset {
                matching: Bound::Exact(exact) | Bound::AtLeast(exact),
                total,
            } if exact == total => Some(Bound::Exact(exact)),
            Self::Subset {
                matching: Bound::Parity(Parity::Even),
                total: 0,
            } => Some(Bound::Exact(0)),
            Self::Subset {
                matching: Bound::Parity(Parity::Odd),
                total: 1,
            } => Some(Bound::Exact(1)),
            Self::Simple(bound) => Some(bound),
            Self::Subset { .. } => None,
        }
    }
}

impl From<Bound> for BoundPair {
    fn from(v: Bound) -> Self {
        Self::Simple(v)
    }
}

impl From<Parity> for BoundPair {
    fn from(v: Parity) -> Self {
        Self::Simple(v.into())
    }
}

impl From<Number> for BoundPair {
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

/// "All" implies at least one, but "any" does not
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
