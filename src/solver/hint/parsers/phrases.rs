use anyhow::{Context as _, Result, bail};
use mitsein::iter1::IntoIterator1 as _;
use mitsein::vec1::Vec1;

use crate::models::{Column, Coord, Direction, Judgment, Profession, Row};
use crate::solver::board::coordinates::{ModifiedSet, Set, Set1};
use crate::solver::hint::recipes::{
    AddContext, ColumnRecipe, Context, LineRecipe, NameRecipe, RowRecipe,
};
use crate::solver::hint::{Cardinal, Comparison, Hint, LineKind, Number, Parity};

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
        intersection: Cardinal,
        judgment: Judgment,
    },
    // TODO replace with UnitSize?
    IntersectionSize([Unit; 2], Quantifier, Judgment),
    EachInUnitHasAtMostNNeighbors(Unit, Number, Judgment),
    TotalUnitsSize([Unit; 2], Cardinal, Judgment),
}

impl AddContext for Sentence {
    type Output = Vec<Hint>;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let hints: Vec<Hint> = match self {
            Self::UnitIsConnected(unit) => unit.members_are_connected(context)?,
            Self::BiggestInSeries(unit, judgment) => unit.has_most(judgment, context)?,
            Self::IsOneOfNInUnit(unit, name, quantity, judgment) => {
                unit.one_of_n_in_unit(&name, quantity, judgment, context)?
            }
            Self::UnitBiggerThanUnit { big, small, excess } => {
                big.bigger_than(&small, excess, context)?
            }
            Self::UnitSize(unit, quantity) => {
                let (set, mut hints) = unit.add_context(context)?;
                if let ModifiedSet::NonEmpty(set) = set {
                    hints.push(Hint::Count(set, quantity));
                } else if !quantity.matches(0) {
                    bail!("{set:?} is empty");
                }
                hints
            }
            Self::TotalUnitsSize(units, quantity, judgment) => {
                let (sets, mut hints) = units.add_context(context)?;
                let sets = sets.map(|set| set.judged(judgment));
                match sets {
                    [ModifiedSet::Empty, ModifiedSet::Empty] => {
                        if !quantity.matches(0) {
                            bail!("{units:?} are both empty so cannot total to {quantity:?}")
                        }
                    }
                    [ModifiedSet::Empty, ModifiedSet::NonEmpty(set)]
                    | [ModifiedSet::NonEmpty(set), ModifiedSet::Empty] => {
                        hints.push(Hint::Count(set, quantity));
                    }
                    [ModifiedSet::NonEmpty(a), ModifiedSet::NonEmpty(b)] => {
                        hints.push(Hint::CountTotal([a, b], quantity));
                    }
                }
                hints
            }
            Self::UniqueInUnitHasNNeighbors(unit, quantity, name, judgment) => {
                unit.unique_member_has_n_neighbors(quantity, judgment, name.as_ref(), context)?
            }

            Self::UniqueUnitInSeriesHasSize(series, quantity, judgment) => {
                let sets = series
                    .all(context)
                    .into_iter1()
                    .map(|set| set.judged(judgment).into())
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
                total,
                quantified,
                other,
                intersection,
                judgment,
            } => {
                quantified.this_and_intersection(total, &other, intersection, judgment, context)?
            }
            Self::IntersectionSize([a, b], quantity, judgment) => {
                a.intersection(&b, quantity, judgment, context)?
            }
            Self::EqualNumberOfTraitsInUnits(units, judgment) => {
                let (sets, mut hints) = units.add_context(context)?;
                let sets = sets.map(|set| set.judged(judgment));
                match sets {
                    [ModifiedSet::Empty, ModifiedSet::Empty] => {}
                    [ModifiedSet::Empty, ModifiedSet::NonEmpty(set)]
                    | [ModifiedSet::NonEmpty(set), ModifiedSet::Empty] => {
                        hints.push(Hint::Count(set, Cardinal::Exact(0)));
                    }
                    [ModifiedSet::NonEmpty(a), ModifiedSet::NonEmpty(b)] => {
                        hints.push(Hint::CompareSets([a, b], Comparison::ExactDifference(0)));
                    }
                }
                hints
            }
            Self::UnitEquallySplit(unit) => unit.equal_traits(context)?,
            Self::MoreTraitsInUnit(unit, judgment) => {
                let (set, mut hints) = unit.add_context(context)?;
                let hint = match [set.clone().judged(judgment), set.judged(!judgment)] {
                    [ModifiedSet::Empty, _] => bail!("no {judgment} in {unit:?}"),
                    [ModifiedSet::NonEmpty(big), ModifiedSet::Empty] => {
                        Hint::Count(big, Cardinal::AtLeast(1))
                    }
                    [ModifiedSet::NonEmpty(big), ModifiedSet::NonEmpty(small)] => {
                        Hint::CompareSets([big, small], Comparison::More)
                    }
                };
                hints.push(hint);
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
    fn unique_member_has_n_neighbors(
        &self,
        quantity: Cardinal,
        judgment: Judgment,
        name: Option<&NameRecipe>,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        let coord = name
            .as_ref()
            .map(|name| name.add_context(context))
            .transpose()?;
        let ModifiedSet::NonEmpty(set) = set else {
            bail!("empty unit {self:?} cannnot have unique member")
        };
        if let Some(set) = set.as_regular() {
            if let Some(coord) = coord {
                if !set.contains(coord) {
                    bail!("{name:?} does not belong to {self:?}")
                }
                hints.push(Hint::Count(coord.neighbors().judged(judgment), quantity));
                hints.extend(
                    set.into_iter()
                        .filter(|&other| other != coord)
                        .map(|other| Hint::NotCount(other.neighbors().judged(judgment), quantity)),
                );
            } else {
                let sets = set
                    .into_iter1()
                    .map(|coord| coord.neighbors().judged(judgment).into())
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
                hints.push(Hint::Count(coord.neighbors().judged(judgment), quantity));
            }
        }
        Ok(hints)
    }

    fn this_and_intersection(
        &self,
        total: Number,
        other: &Self,
        intersection: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let ([self_, other], mut hints) = [self, other].add_context(context)?;
        match self_.judged(judgment) {
            ModifiedSet::Empty => {
                if total != 0 || !intersection.matches(0) {
                    bail!("{self:?} is empty");
                }
            }
            ModifiedSet::NonEmpty(self_) => {
                hints.push(Hint::Count(self_.clone(), Cardinal::Exact(total)));
                let other = other.intersect1(self_).judged(judgment);
                match other {
                    ModifiedSet::Empty => {
                        if !intersection.matches(0) {
                            bail!("intersection of {self:?} and {other:?} is empty")
                        }
                    }
                    ModifiedSet::NonEmpty(other) => {
                        hints.push(Hint::Count(other, intersection));
                    }
                }
            }
        }
        Ok(hints)
    }

    fn intersection(
        &self,
        other_unit: &Self,
        intersection: Quantifier,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let ([self_, other], mut hints) = [self, other_unit].add_context(context)?;
        let set = other.intersect(self_);
        match set {
            ModifiedSet::Empty => {
                let forced_non_empty = match intersection {
                    Quantifier::Simple(cardinal) => !cardinal.matches(0),
                    Quantifier::Subset(_, total) => total != 0,
                };
                if forced_non_empty {
                    bail!("{self:?} is empty");
                }
            }
            ModifiedSet::NonEmpty(set) => {
                let intersection = match intersection {
                    Quantifier::Simple(intersection) => intersection,
                    Quantifier::Subset(intersection, total) => {
                        hints.push(Hint::Count(set.clone(), Cardinal::Exact(total)));
                        intersection
                    }
                };
                match set.judged(judgment) {
                    ModifiedSet::Empty => {
                        if !intersection.matches(0) {
                            bail!("intersection of {self:?} and {other_unit:?} is empty")
                        }
                    }
                    ModifiedSet::NonEmpty(set) => {
                        hints.push(Hint::Count(set, intersection));
                    }
                }
            }
        }

        Ok(hints)
    }

    fn members_have_at_most_neighbors(
        &self,
        number: Number,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        if let ModifiedSet::NonEmpty(set) = set {
            hints.push(Hint::EachNeighbors(set, Cardinal::AtMost(number), judgment));
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
        match [big, small] {
            [ModifiedSet::Empty, _] if !matches!(compare, Comparison::ExactDifference(0)) => {
                bail!("{self:?} is empty so cannot be > {small_unit:?}")
            }
            [ModifiedSet::Empty, ModifiedSet::Empty] => {}
            [ModifiedSet::Empty, ModifiedSet::NonEmpty(small)] => {
                hints.push(Hint::Count(small, Cardinal::Exact(0)));
            }
            [ModifiedSet::NonEmpty(big), ModifiedSet::Empty] => {
                let count = match compare {
                    Comparison::ExactDifference(exact) => Cardinal::Exact(exact),
                    Comparison::More => Cardinal::AtLeast(1),
                };
                hints.push(Hint::Count(big, count));
            }
            [ModifiedSet::NonEmpty(big), ModifiedSet::NonEmpty(small)] => {
                hints.push(Hint::CompareSets([big, small], compare));
            }
        }
        Ok(hints)
    }

    fn members_are_connected(self, context: Context<'_>) -> Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        if let ModifiedSet::NonEmpty(set) = set {
            hints.push(Hint::Connected(set));
        }
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
        count: Cardinal,
        each: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        if let ModifiedSet::NonEmpty(set) = set {
            hints.push(Hint::CountWithNeighbors {
                set,
                count,
                each,
                judgment,
            });
        } else if !count.matches(0) {
            bail!("{self:?} is empty so cannot have {count:?} members");
        }
        Ok(hints)
    }

    pub(crate) fn with_judgment(self, judgment: Judgment) -> Self {
        Self::Judged(Box::new(self), judgment)
    }

    pub(crate) fn shift(self, direction: Direction) -> Self {
        Self::Shifted(Box::new(self), direction)
    }

    fn equal_traits(&self, context: Context<'_>) -> Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        if let ModifiedSet::NonEmpty(set) = set {
            let extra = if let Some(set) = set.as_regular() {
                if !set.len().get().is_multiple_of(2) {
                    bail!("{self:?} cannot be split equally")
                }
                let count = set.len().get() / 2;
                Some(Hint::Count(
                    set.judged(Judgment::Innocent),
                    Cardinal::Exact(count),
                ))
            } else {
                match [
                    set.clone().judged(Judgment::Innocent),
                    set.judged(Judgment::Criminal),
                ] {
                    [ModifiedSet::Empty, ModifiedSet::Empty] => None,
                    [ModifiedSet::Empty, ModifiedSet::NonEmpty(set)]
                    | [ModifiedSet::NonEmpty(set), ModifiedSet::Empty] => {
                        Some(Hint::Count(set, Cardinal::Exact(0)))
                    }
                    [ModifiedSet::NonEmpty(a), ModifiedSet::NonEmpty(b)] => {
                        Some(Hint::CompareSets([a, b], Comparison::ExactDifference(0)))
                    }
                }
            };
            if let Some(extra) = extra {
                hints.push(extra);
            }
        }
        Ok(hints)
    }

    fn one_of_n_in_unit(
        &self,
        name: &NameRecipe,
        quantity: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let (set, mut hints) = self.add_context(context)?;
        let coord = name.add_context(context)?;
        let ModifiedSet::NonEmpty(set) = set else {
            bail!("{self:?} is empty")
        };
        hints.extend(set.conditions_to_contain(coord)?);
        hints.push(Hint::Judgment(coord, judgment));
        match set.judged(judgment) {
            ModifiedSet::Empty => {
                if !quantity.matches(0) {
                    bail!("{self:?} has no {judgment}")
                }
            }
            ModifiedSet::NonEmpty(set) => hints.push(Hint::Count(set, quantity)),
        }
        Ok(hints)
    }
}

impl AddContext for &Unit {
    type Output = (ModifiedSet, Vec<Hint>);

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let mut hints = Vec::new();
        let set: ModifiedSet = match self {
            &Unit::Line(line) => line.add_context(context)?.into(),
            Unit::Direction(direction, name) => {
                let start = name.add_context(context)?;
                Coord::direction(start, *direction).collect()
            }
            Unit::Neighbor(name) => name.add_context(context)?.neighbors().into(),
            Unit::NotNeighbor(name) => Set::from(name.add_context(context)?.neighbors())
                .complement()
                .into(),
            Unit::Profession(profession) => (*context.profession_as_set(profession)?).into(),
            Unit::Edges => Coord::edges().collect(),
            Unit::Corners => Coord::corners().collect(),
            Unit::Between(names) => {
                let [a, b] = names.each_ref().map(|name| name.add_context(context));
                Set::between([a?, b?])?.into()
            }
            Unit::All => Coord::all().into_iter().collect(),
            Unit::Quantified(inner, quantity) => {
                let set;
                (set, hints) = inner.add_context(context)?;
                if let ModifiedSet::NonEmpty(set) = &set {
                    hints.push(Hint::Count(set.clone(), Cardinal::Exact(*quantity)));
                } else if quantity != &0 {
                    bail!("{inner:?} is empty")
                }
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
                set.judged(*judgment)
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

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        self.each_ref().add_context(context)
    }
}

impl AddContext for [&Unit; 2] {
    type Output = ([ModifiedSet; 2], Vec<Hint>);

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        // TODO use `try_map()` https://github.com/rust-lang/rust/issues/79711
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
    fn has_most(self, judgment: Judgment, context: Context<'_>) -> Result<Vec<Hint>> {
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
        self,
        quantity: Cardinal,
        judgment: Judgment,
        context: Context<'_>,
    ) -> Result<Vec<Hint>> {
        let others = self.others(context)?;
        let this = self.add_context(context)?;
        let mut hints = vec![Hint::Count(this.judged(judgment), quantity)];
        hints.extend(
            others
                .into_iter()
                .map(|other| Hint::NotCount(other.judged(judgment), quantity)),
        );
        Ok(hints)
    }
}

impl AddContext for UnitInSeries {
    type Output = Set1;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let set = match self {
            Self::Line(line) => line.add_context(context)?.into(),
            Self::Profession(profession) => *context
                .by_profession
                .get(&profession)
                .with_context(|| format!("{profession} not in puzzle"))?,
            Self::Neighbor(suspect) => suspect.add_context(context)?.neighbors(),
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
            Self::Neighbor => Coord::all().map(Coord::neighbors).collect1(),
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
    pub(crate) fn exact(self) -> Option<Number> {
        match self {
            Self::Subset(Cardinal::Exact(count), total) if count == total => Some(total),
            Self::Simple(Cardinal::Exact(total)) => Some(total),
            Self::Simple(Cardinal::AtLeast(_) | Cardinal::AtMost(_) | Cardinal::Parity(_))
            | Self::Subset(_, _) => None,
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
