use anyhow::anyhow;
use mitsein::iter1::{IntoIterator1 as _, IteratorExt as _};
use mitsein::vec1::{Vec1, vec1};
use winnow::ascii::dec_uint;
use winnow::combinator::{alt, delimited, opt, preceded, separated_pair, terminated};
use winnow::error::{ParserError, StrContext};
use winnow::token::take_while;
use winnow::{Parser, Result};

use crate::puzzle::grid::{Column, Coordinate, Grid, Row};
use crate::puzzle::hint::recipes::{NameRecipe, Recipe as _, SetRecipe};
use crate::puzzle::hint::{
    Direction, Line, LineKind, Number, Parity, Profession, Quantity, Set, WithJudgment,
};
use crate::puzzle::{Judgment, Name};

pub(crate) type Sentence = WithJudgment<SentenceKind>;

impl Sentence {
    pub(crate) fn parse(hint: &str) -> anyhow::Result<Self> {
        Self::parse_cased(hint).or_else(|e| {
            let mut hint = hint.to_owned();
            let Some(first) = hint.get_mut(..1) else {
                return Err(e);
            };
            first.make_ascii_lowercase();
            Self::parse_cased(&hint).map_err(|_lower_case_err| e)
        })
    }

    fn parse_cased(hint: &str) -> anyhow::Result<Self> {
        Self::any.parse(hint).map_err(|e| anyhow!("{e}"))
    }

    fn any(input: &mut &str) -> Result<Self> {
        alt((
            Self::traits_are_neighbors_in_unit,
            Self::has_most_traits,
            Self::is_one_of_n_traits_in_unit,
            Self::more_traits_in_unit_than_unit,
            Self::units_share_n_traits,
            Self::number_of_traits_in_unit,
            Self::only_one_person_in_unit_has_exactly_n_trait_neighbors,
            Self::only_one_line_has_exactly_n_traits,
            Self::only_line_has_exactly_n_traits,
            Self::unit_shares_n_out_of_n_traits_with_unit,
            Self::equal_number_of_traits_in_units,
            Self::each_line_has_n_traits,
            Self::more_traits_in_unit,
            Self::has_trait,
            Self::at_most_n_traits_in_neighbors_in_unit,
        ))
        .parse_next(input)
    }

    fn traits_are_neighbors_in_unit(input: &mut &str) -> Result<Self> {
        terminated(
            spaced(alt(("All".value(None), quantity.map(Some))), judged_unit),
            " are connected",
        )
        .map(|(quantity, (judgment, unit))| Self {
            kind: SentenceKind::TraitsAreNeighborsInUnit(unit, quantity),
            judgment,
        })
        .parse_next(input)
    }

    fn has_most_traits(input: &mut &str) -> Result<Self> {
        alt((
            (
                line,
                " has more ",
                judgment_plural,
                " than any other ",
                line_kind,
            )
                .verify(|&(line, _, _, _, kind)| line.kind() == kind)
                .map(|(line, _, judgment, _, _)| Self {
                    kind: SentenceKind::HasMostTraits(line.into()),
                    judgment,
                }),
            delimited(
                "There are more ",
                separated_pair(judgment_plural, " among ", profession_plural),
                " than any other profession",
            )
            .map(|(judgment, profession)| Self {
                kind: SentenceKind::HasMostTraits(UnitInSeries::Profession(profession)),
                judgment,
            }),
            (name, " has the most ", judgment_singular, " neighbors").map(
                |(name, _, judgment, _)| Self {
                    kind: SentenceKind::HasMostTraits(UnitInSeries::Neighbor(name)),
                    judgment,
                },
            ),
        ))
        .parse_next(input)
    }

    fn is_one_of_n_traits_in_unit(input: &mut &str) -> Result<Self> {
        separated_pair(name, " is one of ", quantified_judged_unit)
            .map(|(name, (count, judgment, unit))| Self {
                kind: SentenceKind::IsOneOfNTraitsInUnit(unit, name, count),
                judgment,
            })
            .parse_next(input)
    }

    fn more_traits_in_unit_than_unit(input: &mut &str) -> Result<Self> {
        alt((
            preceded(
                "There are more ",
                separated_pair(judged_unit, " than ", maybe_judged_unit),
            )
            .verify_map(|((judgment, big), (judgment_small, small))| {
                judgment_small
                    .is_none_or(|small| small == judgment)
                    .then_some(Self {
                        kind: SentenceKind::MoreTraitsInUnitThanUnit { big, small },
                        judgment,
                    })
            }),
            (
                name,
                delimited(" has more ", judgment_singular, " neighbors than "),
                name,
            )
                .map(|(big, judgment, small)| Self {
                    kind: SentenceKind::MoreTraitsInUnitThanUnit {
                        big: Unit::Neighbor(big),
                        small: Unit::Neighbor(small),
                    },
                    judgment,
                }),
        ))
        .parse_next(input)
    }

    fn number_of_traits_in_unit(input: &mut &str) -> Result<Self> {
        alt((
            preceded(
                alt(("There is ", "There are ", "There's ")),
                quantified_judged_unit,
            ),
            separated_pair(
                name,
                " has ",
                terminated(quantified_judgment, (" ", neighbor_any)),
            )
            .map(|(name, (quantity, judgment))| (quantity, judgment, Unit::Neighbor(name))),
            separated_pair(
                quantified_profession,
                (alt((" has ", " have ")), alt(("an ", "a "))),
                (
                    judgment_singular,
                    delimited(" directly ", direction, alt((" them", " us"))),
                ),
            )
            .map(|((count, profession), (judgment, direction))| {
                let unit = Unit::ProfessionShift(profession, direction);
                (count, judgment, unit)
            }),
        ))
        .map(|(count, judgment, unit)| Self {
            kind: SentenceKind::NumberOfTraitsInUnit(unit, count),
            judgment,
        })
        .parse_next(input)
    }

    fn only_one_person_in_unit_has_exactly_n_trait_neighbors(input: &mut &str) -> Result<Self> {
        alt((
            separated_pair(
                preceded((alt(("Only one of ", "Only one ")), opt("person ")), unit),
                " has ",
                terminated(spaced(quantity, judgment_singular), (" ", neighbor_any)),
            )
            .map(|(unit, (count, judgment))| Self {
                kind: SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(unit, count, None),
                judgment,
            }),
            separated_pair(
                name,
                " is the only one ",
                separated_pair(
                    unit,
                    " with ",
                    terminated(quantified_judgment, (" ", neighbor_any)),
                ),
            )
            .map(|(name, (unit, (quantity, judgment)))| Self {
                kind: SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(
                    unit,
                    quantity,
                    Some(name),
                ),
                judgment,
            }),
        ))
        .parse_next(input)
    }

    fn only_one_line_has_exactly_n_traits(input: &mut &str) -> Result<Self> {
        separated_pair(
            preceded("Only one ", line_kind),
            " has ",
            quantified_judgment,
        )
        .map(|(kind, (count, judgment))| Self {
            kind: SentenceKind::OnlyOneLineHasNTraits(kind, count),
            judgment,
        })
        .parse_next(input)
    }

    fn only_line_has_exactly_n_traits(input: &mut &str) -> Result<Self> {
        (
            line,
            " is the only ",
            line_kind,
            " with ",
            quantified_judgment,
        )
            .verify(|&(line, _, kind, _, _)| line.kind() == kind)
            .context(StrContext::Label("a matching row/column"))
            .map(|(line, _, _, _, (count, judgment))| Self {
                kind: SentenceKind::OnlyGivenLineHasNTraits(line, count),
                judgment,
            })
            .parse_next(input)
    }

    fn unit_shares_n_out_of_n_traits_with_unit(input: &mut &str) -> Result<Self> {
        alt((
            separated_pair(
                separated_pair(
                    quantity,
                    (" of ", opt((determiner, " "))),
                    quantified_judged_unit,
                ),
                alt((" is ", " are ")),
                unit,
            )
            .map(
                |((intersection, (quantity, judgment, quantified)), other)| Self {
                    kind: SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
                        quantified,
                        other,
                        quantity,
                        intersection,
                    },
                    judgment,
                },
            ),
            separated_pair(
                separated_pair(
                    quantity,
                    " of ",
                    spaced(
                        name_possessive,
                        terminated(quantified_judgment, (" ", neighbor_any)),
                    ),
                ),
                (" also ", neighbor_any, " "),
                name,
            )
            .map(
                |((intersection, (quantified, (quantity, judgment))), other)| Self {
                    kind: SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
                        quantity,
                        quantified: Unit::Neighbor(quantified),
                        other: Unit::Neighbor(other),
                        intersection,
                    },
                    judgment,
                },
            ),
        ))
        .parse_next(input)
    }

    fn units_share_n_traits(input: &mut &str) -> Result<Self> {
        alt((
            (
                preceded(
                    opt(alt(("There are ", "There is ", "There's "))),
                    quantified_judged_unit,
                ),
                alt((
                    preceded(
                        alt((
                            " who neighbor ",
                            " neighbor ",
                            " is neighboring ",
                            " are neighboring ",
                        )),
                        name,
                    )
                    .map(Unit::Neighbor),
                    preceded(alt((" is ", " are ", " ")), unit),
                )),
            )
                .map(|((count, judgment, unit), other)| ([unit, other], judgment, count)),
            (
                pair(name),
                " have ",
                quantified_judgment,
                (" ", neighbor_any, " in common"),
            )
                .map(|(names, _, (count, judgment), _)| {
                    (names.map(Unit::Neighbor), judgment, count)
                }),
        ))
        .map(|([a, b], judgment, count)| Self {
            kind: SentenceKind::UnitsShareNTraits([a, b], count),
            judgment,
        })
        .parse_next(input)
    }

    fn equal_number_of_traits_in_units(input: &mut &str) -> Result<Self> {
        alt((
            preceded(
                "There's an equal number of ",
                spaced(judgment_plural, unit_pair),
            ),
            preceded(
                "There are as many ",
                separated_pair(
                    judged_unit,
                    terminated(" as ", opt("there are ")),
                    judged_unit,
                ),
            )
            .verify_map(|((judgment_a, a), (judgment_b, b))| {
                (judgment_a == judgment_b).then_some((judgment_a, [a, b]))
            }),
            (
                pair(name),
                " have an equal number of ",
                judgment_singular,
                " neighbors",
            )
                .map(|(names, _, judgment, _)| (judgment, names.map(Unit::Neighbor))),
        ))
        .map(|(judgment, pair)| Self {
            kind: SentenceKind::EqualNumberOfTraitsInUnits(pair),
            judgment,
        })
        .parse_next(input)
    }

    fn each_line_has_n_traits(input: &mut &str) -> Result<Self> {
        separated_pair(preceded("Each ", line_kind), " has ", quantified_judgment)
            .map(|(kind, (quantity, judgment))| Self {
                kind: SentenceKind::EachLineHasNTraits(kind, quantity),
                judgment,
            })
            .parse_next(input)
    }

    fn more_traits_in_unit(input: &mut &str) -> Result<Self> {
        preceded(
            "There are more ",
            spaced(
                separated_pair(judgment_plural, " than ", judgment_plural),
                unit,
            ),
        )
        .verify(|&((more, less), _)| more == !less)
        .map(|((judgment, _), unit)| Self {
            kind: SentenceKind::MoreTraitsInUnit(unit),
            judgment,
        })
        .parse_next(input)
    }

    fn has_trait(input: &mut &str) -> Result<Self> {
        separated_pair(name, (" is ", opt("a ")), judgment_singular)
            .map(|(name, judgment)| Self {
                kind: SentenceKind::HasTrait(name),
                judgment,
            })
            .parse_next(input)
    }

    fn at_most_n_traits_in_neighbors_in_unit(input: &mut &str) -> Result<Self> {
        separated_pair(
            preceded("No one ", unit),
            " has more than ",
            spaced(dec_uint, terminated(judgment_singular, (" ", neighbor_any))),
        )
        .map(|(unit, (number, judgment))| Self {
            kind: SentenceKind::AtMostNTraitsInNeighborsInUnit(unit, number),
            judgment,
        })
        .parse_next(input)
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug)]
pub(crate) enum SentenceKind {
    // This I think can't actually be "Me"
    HasTrait(NameRecipe),
    TraitsAreNeighborsInUnit(Unit, Option<Quantity>),
    HasMostTraits(UnitInSeries),
    IsOneOfNTraitsInUnit(Unit, NameRecipe, Quantity),
    EqualNumberOfTraitsInUnits([Unit; 2]),
    MoreTraitsInUnitThanUnit {
        big: Unit,
        small: Unit,
    },
    MoreTraitsInUnit(Unit),
    NumberOfTraitsInUnit(Unit, Quantity),
    OnlyOnePersonInUnitHasNTraitNeighbors(Unit, Quantity, Option<NameRecipe>),
    EachLineHasNTraits(LineKind, Quantity),
    OnlyOneLineHasNTraits(LineKind, Quantity),
    OnlyGivenLineHasNTraits(Line, Quantity),
    UnitSharesNOutOfNTraitsWithUnit {
        quantity: Quantity,
        quantified: Unit,
        other: Unit,
        intersection: Quantity,
    },
    UnitsShareNTraits([Unit; 2], Quantity),
    AtMostNTraitsInNeighborsInUnit(Unit, Number),
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) enum Unit {
    Direction(Direction, NameRecipe),
    Line(Line),
    Profession(Profession),
    ProfessionShift(Profession, Direction),
    Neighbor(NameRecipe),
    Between([NameRecipe; 2]),
    Edges,
    Corners,
    All,
    Quantified(Box<Self>, Quantity),
}

impl Unit {
    pub(crate) fn and(self, other: Self) -> SetRecipe {
        SetRecipe::Intersection(vec1![self, other])
    }

    fn quantify(self, quantity: Quantity) -> Self {
        Self::Quantified(Box::new(self), quantity)
    }

    fn maybe_quantify(self, quantity: Option<Quantity>) -> Self {
        if let Some(quantity) = quantity {
            self.quantify(quantity)
        } else {
            self
        }
    }

    #[cfg(test)]
    fn profession(profession: impl Into<Profession>) -> Self {
        Self::Profession(profession.into())
    }

    #[cfg(test)]
    fn neighbor(name: impl Into<NameRecipe>) -> Self {
        Self::Neighbor(name.into())
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
    pub(crate) fn others(&self, grid: &Grid, speaker: &Name) -> anyhow::Result<Vec1<Set>> {
        match self {
            Self::Line(line) => Ok(line.others().into_iter1().map(Set::from).collect1()),
            Self::Profession(profession) => grid.other_professions(profession),
            Self::Neighbor(name) => {
                let coord = name.contextualize(grid, speaker)?;
                Ok(Coordinate::all()
                    .filter(|&other| other != coord)
                    .map(|other| Coordinate::neighbors(other).collect())
                    .try_collect1()
                    .unwrap_or_else(|_empty| unreachable!()))
            }
        }
    }

    #[cfg(test)]
    fn neighbor(name: impl Into<NameRecipe>) -> Self {
        Self::Neighbor(name.into())
    }

    #[cfg(test)]
    fn profession(profession: impl Into<Profession>) -> Self {
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

fn unit_pair(input: &mut &str) -> Result<[Unit; 2]> {
    preceded("in ", line_pair)
        .map(|lines| lines.map(Unit::Line))
        .parse_next(input)
}

fn unit(input: &mut &str) -> Result<Unit> {
    alt((
        "in total".value(Unit::All),
        "on the edges".value(Unit::Edges),
        alt(("in a corner", "in the corners")).value(Unit::Corners),
        preceded(opt("in "), alt((between, line.map(Unit::Line)))),
        spaced(direction, name).map(|(direction, name)| Unit::Direction(direction, name)),
        terminated(name_possessive, (" ", neighbor_any)).map(Unit::Neighbor),
        (
            opt(delimited((determiner, " "), quantity, " ")),
            profession_any,
        )
            .map(|(quantity, profession)| Unit::Profession(profession).maybe_quantify(quantity)),
    ))
    .parse_next(input)
}

fn maybe_judged_unit(input: &mut &str) -> Result<(Option<Judgment>, Unit)> {
    qualified_unit
        .verify_map(|(count, judgment, unit)| count.is_none().then_some((judgment, unit)))
        .parse_next(input)
}

fn judged_unit(input: &mut &str) -> Result<(Judgment, Unit)> {
    qualified_unit
        .verify_map(|(count, judgment, unit)| {
            let judgment = judgment?;
            count.is_none().then_some((judgment, unit))
        })
        .parse_next(input)
}

fn quantified_judged_unit(input: &mut &str) -> Result<(Quantity, Judgment, Unit)> {
    alt((
        separated_pair(
            separated_pair(quantity, " ", judgment_plural),
            " neighboring ",
            name,
        )
        .map(|((count, judgment), name)| (count, judgment, Unit::Neighbor(name))),
        spaced(
            name_possessive,
            terminated(quantified_judgment, " neighbors"),
        )
        .map(|(name, (quantity, judgment))| (quantity, judgment, Unit::Neighbor(name))),
        qualified_unit.verify_map(|(count, judgment, unit)| Some((count?, judgment?, unit))),
    ))
    .parse_next(input)
}

fn qualified_unit(input: &mut &str) -> Result<(Option<Quantity>, Option<Judgment>, Unit)> {
    (
        opt(terminated(quantity, " ")),
        opt(terminated(judgment_any, " ")),
        unit,
    )
        .parse_next(input)
}

fn quantity(input: &mut &str) -> Result<Quantity> {
    alt((
        "both".value(Quantity::Exact(2)),
        "no".value(Quantity::Exact(0)),
        preceded(opt(alt(("exactly ", "only "))), number).map(Quantity::Exact),
        preceded("at least ", number).map(Quantity::AtLeast),
        delimited("an ", parity, " number of").map(Quantity::Parity),
    ))
    .parse_next(input)
}

fn number(input: &mut &str) -> Result<u8> {
    alt((dec_uint, "one".value(1))).parse_next(input)
}

fn parity(input: &mut &str) -> Result<Parity> {
    alt(("even".value(Parity::Even), "odd".value(Parity::Odd))).parse_next(input)
}

fn quantified_judgment(input: &mut &str) -> Result<(Quantity, Judgment)> {
    spaced(quantity, judgment_any).parse_next(input)
}

fn judgment_any(input: &mut &str) -> Result<Judgment> {
    alt((judgment_plural, judgment_singular)).parse_next(input)
}

fn judgment_plural(input: &mut &str) -> Result<Judgment> {
    alt((
        "innocents".value(Judgment::Innocent),
        "criminals".value(Judgment::Criminal),
    ))
    .parse_next(input)
}

fn judgment_singular(input: &mut &str) -> Result<Judgment> {
    alt((
        "innocent".value(Judgment::Innocent),
        "criminal".value(Judgment::Criminal),
    ))
    .parse_next(input)
}

fn name_possessive(input: &mut &str) -> Result<NameRecipe> {
    alt((
        "my".value(NameRecipe::Me),
        raw_name
            .verify_map(|s| s.strip_suffix("'s"))
            .map(|name| NameRecipe::Other(name.to_owned())),
    ))
    .parse_next(input)
}

fn name(input: &mut &str) -> Result<NameRecipe> {
    alt((
        "me".value(NameRecipe::Me),
        raw_name.map(|name| NameRecipe::Other(name.to_owned())),
    ))
    .parse_next(input)
}

fn raw_name<'input>(input: &mut &'input str) -> Result<&'input str> {
    take_while(1.., |c| c != ' ')
        .verify(|s: &str| s.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
        .parse_next(input)
}

fn quantified_profession(input: &mut &str) -> Result<(Quantity, Profession)> {
    separated_pair(
        quantity,
        alt((
            (" of ", determiner, (" ", opt((quantity, " ")))).void(),
            " ".void(),
        )),
        profession_any,
    )
    .parse_next(input)
}

fn direction(input: &mut &str) -> Result<Direction> {
    alt((
        "above".value(Direction::Above),
        "below".value(Direction::Below),
        "to the left of".value(Direction::Left),
        "to the right of".value(Direction::Right),
    ))
    .parse_next(input)
}

fn determiner<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("the", "a", "an", "us")).parse_next(input)
}

fn profession_any(input: &mut &str) -> Result<Profession> {
    alt((
        profession_plural,
        preceded(opt(alt(("an ", "a "))), profession_singular),
    ))
    .parse_next(input)
}

fn profession_singular(input: &mut &str) -> Result<Profession> {
    take_while(1.., |c| c != ' ')
        .map(str::to_owned)
        .parse_next(input)
}

fn profession_plural(input: &mut &str) -> Result<Profession> {
    take_while(2.., |c| c != ' ')
        .verify_map(|s: &str| s.strip_suffix('s'))
        .map(str::to_owned)
        .parse_next(input)
}

fn neighbor_any(input: &mut &str) -> Result<()> {
    alt(("neighbors", "neighbor")).void().parse_next(input)
}

fn between(input: &mut &str) -> Result<Unit> {
    preceded("between ", pair(name))
        .map(Unit::Between)
        .parse_next(input)
}

fn line(input: &mut &str) -> Result<Line> {
    alt((row.map(Line::Row), column.map(Line::Column))).parse_next(input)
}

fn line_kind(input: &mut &str) -> Result<LineKind> {
    alt(("row".value(LineKind::Row), "column".value(LineKind::Column))).parse_next(input)
}

fn line_pair(input: &mut &str) -> Result<[Line; 2]> {
    alt((
        line_prefixed("rows", pair(row_bare)).map(|rows| rows.map(Line::Row)),
        line_prefixed("columns", pair(column_bare)).map(|rows| rows.map(Line::Column)),
    ))
    .parse_next(input)
}

fn row(input: &mut &str) -> Result<Row> {
    line_prefixed("row", row_bare).parse_next(input)
}

fn line_prefixed<'input, T, E: ParserError<&'input str>>(
    prefix: &str,
    inner: impl Parser<&'input str, T, E>,
) -> impl Parser<&'input str, T, E> {
    preceded((prefix, alt(("\u{A0}", " ", "&nbsp;"))), inner)
}

fn row_bare(input: &mut &str) -> Result<Row> {
    alt((
        "1".value(Row::One),
        "2".value(Row::Two),
        "3".value(Row::Three),
        "4".value(Row::Four),
        "5".value(Row::Five),
    ))
    .parse_next(input)
}

fn column(input: &mut &str) -> Result<Column> {
    line_prefixed("column", column_bare).parse_next(input)
}

fn column_bare(input: &mut &str) -> Result<Column> {
    alt((
        "A".value(Column::A),
        "B".value(Column::B),
        "C".value(Column::C),
        "D".value(Column::D),
    ))
    .parse_next(input)
}

fn pair<'input, T, E: ParserError<&'input str>>(
    inner: impl Parser<&'input str, T, E> + Copy,
) -> impl Parser<&'input str, [T; 2], E> {
    separated_pair(inner, " and ", inner).map(Into::into)
}

fn spaced<'input, S, T, E: ParserError<&'input str>>(
    first: impl Parser<&'input str, S, E>,
    second: impl Parser<&'input str, T, E>,
) -> impl Parser<&'input str, (S, T), E> {
    separated_pair(first, " ", second).map(Into::into)
}

#[cfg(test)]
mod tests;
