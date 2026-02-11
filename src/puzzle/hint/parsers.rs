use std::iter::once;
use std::ops::Not as _;

use anyhow::anyhow;
use mitsein::iter1::{IntoIterator1 as _, IteratorExt as _};
use mitsein::vec1::{Vec1, vec1};
use winnow::ascii::dec_uint;
use winnow::combinator::{alt, delimited, opt, preceded, separated_pair, terminated};
use winnow::error::{ParserError, StrContext};
use winnow::token::take_while;
use winnow::{Parser, Result};

use crate::puzzle::grid::{Column, Coordinate, Grid, Row};
use crate::puzzle::hint::recipes::{
    HintRecipe as Hint, HintRecipeKind as HintKind, NameRecipe, Recipe as _, SetRecipe,
};
use crate::puzzle::hint::{
    Direction, Line, LineKind, Parity, Profession, Quantity, Set, WithJudgment,
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

    pub(crate) fn collate(self) -> Vec<Hint> {
        self.kind
            .collate()
            .into_iter()
            .map(|kind| Hint {
                kind,
                judgment: self.judgment,
            })
            .collect()
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
            Self::n_professions_have_trait_in_dir,
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

    fn n_professions_have_trait_in_dir(input: &mut &str) -> Result<Self> {
        (
            quantified_profession,
            (alt((" has ", " have ")), alt(("an ", "a "))),
            judgment_singular,
            " directly ",
            direction,
            alt((" them", " us")),
        )
            .map(|((count, profession), _, judgment, _, direction, _)| Self {
                kind: SentenceKind::NProfessionsHaveTraitInDir(profession, direction, count),
                judgment,
            })
            .parse_next(input)
    }

    fn number_of_traits_in_unit(input: &mut &str) -> Result<Self> {
        alt((
            preceded(
                alt(("There is ", "There are ", "There's ")),
                quantified_judged_unit,
            ),
            separated_pair(name, " has ", terminated(quantified_judgment, " neighbors"))
                .map(|(name, (quantity, judgment))| (quantity, judgment, Unit::Neighbor(name))),
        ))
        .map(|(count, judgment, unit)| Self {
            kind: SentenceKind::NumberOfTraitsInUnit(unit, count),
            judgment,
        })
        .parse_next(input)
    }

    fn only_one_person_in_unit_has_exactly_n_trait_neighbors(input: &mut &str) -> Result<Self> {
        separated_pair(
            preceded((alt(("Only one of ", "Only one ")), opt("person ")), unit),
            " has ",
            terminated(spaced(quantity, judgment_singular), " neighbors"),
        )
        .map(|(unit, (count, judgment))| Self {
            kind: SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(unit, count),
            judgment,
        })
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
        separated_pair(
            separated_pair(quantity, " of the ", quantified_judged_unit),
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
        )
        .parse_next(input)
    }

    fn units_share_n_traits(input: &mut &str) -> Result<Self> {
        alt((
            separated_pair(
                preceded(
                    opt(alt(("There are ", "There is "))),
                    quantified_judged_unit,
                ),
                alt((
                    " who neighbor ",
                    " neighbor ",
                    " is neighboring ",
                    " are neighboring ",
                )),
                name,
            )
            .map(|((count, judgment, unit), name)| ([unit, Unit::Neighbor(name)], judgment, count)),
            separated_pair(quantified_judged_unit, alt((" is ", " are ")), unit)
                .map(|((quantity, judgment, a), b)| ([a, b], judgment, quantity)),
            (
                pair(name),
                " have ",
                quantified_judgment,
                (" neighbor", opt("s"), " in common"),
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
    NProfessionsHaveTraitInDir(Profession, Direction, Quantity),
    NumberOfTraitsInUnit(Unit, Quantity),
    OnlyOnePersonInUnitHasNTraitNeighbors(Unit, Quantity),
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
}

impl SentenceKind {
    fn collate(self) -> Vec<HintKind> {
        match self {
            Self::TraitsAreNeighborsInUnit(unit, quantity) => quantity
                .map(|quantity| HintKind::Count(unit.clone().into(), quantity))
                .into_iter()
                .chain(once(HintKind::Connected(unit)))
                .collect(),
            Self::HasMostTraits(unit) => vec![HintKind::BiggerThanOthers(unit)],
            Self::IsOneOfNTraitsInUnit(unit, name, quantity) => {
                vec![
                    HintKind::Count(unit.clone().into(), quantity),
                    HintKind::Member(name, unit),
                ]
            }
            Self::MoreTraitsInUnitThanUnit { big, small } => vec![HintKind::Bigger { big, small }],
            Self::NProfessionsHaveTraitInDir(profession, direction, quantity) => {
                vec![HintKind::Count(
                    Unit::ProfessionShift(profession, direction).into(),
                    quantity,
                )]
            }
            Self::NumberOfTraitsInUnit(unit, quantity) => {
                vec![HintKind::Count(unit.into(), quantity)]
            }
            Self::OnlyOnePersonInUnitHasNTraitNeighbors(unit, quantity) => {
                vec![HintKind::UniqueWithNeighbors(unit, quantity)]
            }
            Self::OnlyOneLineHasNTraits(line_kind, quantity) => {
                vec![HintKind::UniqueLine(line_kind, quantity)]
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
            } => vec![
                HintKind::Count(quantified.clone().into(), quantity),
                HintKind::Count(quantified.and(other), intersection),
            ],
            Self::UnitsShareNTraits([a, b], quantity) => {
                vec![HintKind::Count(a.and(b), quantity)]
            }
            Self::EqualNumberOfTraitsInUnits([a, b]) => {
                vec![HintKind::EqualSize([a, b])]
            }
            Self::MoreTraitsInUnit(unit) => vec![HintKind::Majority(unit)],
            Self::HasTrait(name) => vec![HintKind::Is(name)],
        }
    }
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
        "in a corner".value(Unit::Corners),
        preceded(opt("in "), alt((between, line.map(Unit::Line)))),
        spaced(direction, name).map(|(direction, name)| Unit::Direction(direction, name)),
        terminated(name_possessive, alt((" neighbors", " neighbor"))).map(Unit::Neighbor),
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
        alt((terminated(" of us ", opt((quantity, " "))), " ")),
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
mod tests {
    use std::fmt;

    use winnow::Parser;
    use winnow::error::ParserError;

    use crate::puzzle::Judgment;
    use crate::puzzle::grid::{Column, Row};
    use crate::puzzle::hint::{Direction, LineKind, Parity, Quantity};

    use super::{NameRecipe as Name, Sentence, SentenceKind, Unit, UnitInSeries};

    #[test]
    fn uma_2026_02_03() {
        let input = "exactly 1 judge has an innocent directly above them";
        let kind = SentenceKind::NProfessionsHaveTraitInDir(
            "judge".into(),
            Direction::Above,
            Quantity::Exact(1),
        );
        let judgment = Judgment::Innocent;
        sentence(input, kind, judgment);
    }

    #[test]
    fn salil_2026_02_04() {
        sentence(
            "There are as many innocent builders as there are innocent guards",
            SentenceKind::EqualNumberOfTraitsInUnits(
                ["builder", "guard"]
                    .map(str::to_owned)
                    .map(Unit::Profession),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn alice_2026_02_05() {
        sentence(
            "Tina is one of 3 criminals in row\u{A0}4",
            SentenceKind::IsOneOfNTraitsInUnit(Row::Four.into(), "Tina".into(), Quantity::Exact(3)),
            Judgment::Criminal,
        );
    }

    #[test]
    fn chuck_2026_02_05() {
        sentence(
            "exactly 2 of the 4 innocents neighboring Gary are in row\u{a0}1",
            SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
                quantity: Quantity::Exact(4),
                quantified: Unit::Neighbor("Gary".into()),
                other: Row::One.into(),
                intersection: Quantity::Exact(2),
            },
            Judgment::Innocent,
        );
    }

    #[test]
    fn ethan_2026_02_05() {
        sentence(
            "an odd number of innocents on the edges neighbor Gary",
            SentenceKind::UnitsShareNTraits(
                [Unit::Edges, Unit::Neighbor("Gary".into())],
                Quantity::Parity(Parity::Odd),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn gary_2026_02_05() {
        sentence(
            "exactly 1 innocent in row\u{a0}4 is neighboring Xavi",
            SentenceKind::UnitsShareNTraits(
                [Row::Four.into(), Unit::Neighbor("Xavi".into())],
                Quantity::Exact(1),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn ike_2026_02_05() {
        sentence(
            "Xavi has exactly 3 innocent neighbors",
            SentenceKind::NumberOfTraitsInUnit(Unit::Neighbor("Xavi".into()), Quantity::Exact(3)),
            Judgment::Innocent,
        );
    }

    #[test]
    fn kyle_2026_02_05() {
        sentence(
            "an odd number of innocents above Zara neighbor Gary",
            SentenceKind::UnitsShareNTraits(
                [
                    Unit::Direction(Direction::Above, "Zara".into()),
                    Unit::Neighbor("Gary".into()),
                ],
                Quantity::Parity(Parity::Odd),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn tina_2026_02_05() {
        sentence(
            "both criminals above Xavi are connected",
            SentenceKind::TraitsAreNeighborsInUnit(
                Unit::Direction(Direction::Above, "Xavi".into()),
                Some(Quantity::Exact(2)),
            ),
            Judgment::Criminal,
        );
    }

    #[test]
    fn vera_2026_02_05() {
        sentence(
            "Each column has at least 3 innocents",
            SentenceKind::EachLineHasNTraits(LineKind::Column, Quantity::AtLeast(3)),
            Judgment::Innocent,
        );
    }

    #[test]
    fn freya_2026_02_06() {
        sentence(
            "Only one of us 2 singers has exactly 2 criminal neighbors",
            SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(
                Unit::Profession("singer".to_owned()).quantify(Quantity::Exact(2)),
                Quantity::Exact(2),
            ),
            Judgment::Criminal,
        );
    }

    #[test]
    fn helen_2026_02_06() {
        sentence(
            "Jason is one of Ellie's 4 innocent neighbors",
            SentenceKind::IsOneOfNTraitsInUnit(
                Unit::Neighbor("Ellie".into()),
                "Jason".into(),
                Quantity::Exact(4),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn jason_2026_02_06() {
        sentence(
            "Ellie and Noah have only one innocent neighbor in common",
            SentenceKind::UnitsShareNTraits(
                ["Ellie", "Noah"].map(Name::from).map(Unit::Neighbor),
                Quantity::Exact(1),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn logan_2026_02_06() {
        sentence(
            "exactly 1 farmer has a criminal directly above them",
            SentenceKind::NProfessionsHaveTraitInDir(
                "farmer".to_owned(),
                Direction::Above,
                Quantity::Exact(1),
            ),
            Judgment::Criminal,
        );
    }

    #[test]
    fn ivan_2026_02_06() {
        sentence(
            "row 5 has more innocents than any other row",
            SentenceKind::HasMostTraits(Row::Five.into()),
            Judgment::Innocent,
        );
    }

    #[test]
    fn scott_2026_02_06() {
        sentence(
            "There are exactly 2 innocents to the left of Noah",
            SentenceKind::NumberOfTraitsInUnit(
                Unit::Direction(Direction::Left, "Noah".into()),
                Quantity::Exact(2),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn vera_2026_02_06() {
        sentence(
            "There are more innocent cooks than innocent mechs",
            SentenceKind::MoreTraitsInUnitThanUnit {
                big: Unit::Profession("cook".to_owned()),
                small: Unit::Profession("mech".to_owned()),
            },
            Judgment::Innocent,
        );
    }

    #[test]
    fn gary_2026_02_07() {
        sentence(
            "only 1 of the 2 innocents in column\u{a0}C is Zara's neighbor",
            SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
                quantity: Quantity::Exact(2),
                quantified: Column::C.into(),
                other: Unit::Neighbor("Zara".into()),
                intersection: Quantity::Exact(1),
            },
            Judgment::Innocent,
        );
    }

    #[test]
    fn uma_2026_02_07() {
        sentence(
            "only 1 of the 3 innocents neighboring me is to the right of Kay",
            SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
                quantity: Quantity::Exact(3),
                quantified: Unit::Neighbor(Name::Me),
                other: Unit::Direction(Direction::Right, "Kay".into()),
                intersection: Quantity::Exact(1),
            },
            Judgment::Innocent,
        );
    }

    #[test]
    fn xena_2026_02_08() {
        sentence(
            "There are no innocents in row 1 who neighbor Donna",
            SentenceKind::UnitsShareNTraits(
                [Row::One.into(), Unit::Neighbor("Donna".into())],
                Quantity::Exact(0),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn hank_2026_02_08() {
        sentence(
            "Only one person in a corner has exactly 2 innocent neighbors",
            SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(Unit::Corners, Quantity::Exact(2)),
            Judgment::Innocent,
        );
    }

    #[test]
    fn tina_2026_02_09() {
        sentence(
            "exactly 2 innocents in column C are neighboring me",
            SentenceKind::UnitsShareNTraits(
                [Column::C.into(), Unit::Neighbor(Name::Me)],
                Quantity::Exact(2),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn kumar_2026_02_09() {
        sentence(
            "exactly 2 of the 3 innocents in row 5 are Susan's neighbors",
            SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
                quantity: Quantity::Exact(3),
                quantified: Row::Five.into(),
                other: Unit::Neighbor("Susan".into()),
                intersection: Quantity::Exact(2),
            },
            Judgment::Innocent,
        );
    }

    #[test]
    fn xavi_2026_02_09() {
        sentence(
            "There are more innocents in row 3 than row 5",
            SentenceKind::MoreTraitsInUnitThanUnit {
                big: Row::Three.into(),
                small: Row::Five.into(),
            },
            Judgment::Innocent,
        );
    }

    #[test]
    fn ollie_2026_02_09() {
        sentence(
            "There's an equal number of innocents in rows 2 and 3",
            SentenceKind::EqualNumberOfTraitsInUnits([Row::Two, Row::Three].map(Unit::from)),
            Judgment::Innocent,
        );
    }

    #[test]
    fn gabe_2026_02_09() {
        sentence(
            "There are at least 10 innocents on the edges",
            SentenceKind::NumberOfTraitsInUnit(Unit::Edges, Quantity::AtLeast(10)),
            Judgment::Innocent,
        );
    }

    #[test]
    fn gary_2026_02_10() {
        sentence(
            "Ryan is one of 2 innocents in between Betty and Vicky",
            SentenceKind::IsOneOfNTraitsInUnit(
                Unit::Between(["Betty", "Vicky"].map(Name::from)),
                "Ryan".into(),
                Quantity::Exact(2),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn lisa_2026_02_10() {
        sentence(
            "exactly 1 innocent on the edges is a farmer",
            SentenceKind::UnitsShareNTraits(
                [Unit::Edges, Unit::Profession("farmer".to_owned())],
                Quantity::Exact(1),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn will_2026_02_10() {
        sentence(
            "2 of us 3 singers have an innocent directly to the left of us",
            SentenceKind::NProfessionsHaveTraitInDir(
                "singer".into(),
                Direction::Left,
                Quantity::Exact(2),
            ),
            Judgment::Innocent,
        );
    }

    #[test]
    fn janet_2026_02_10() {
        sentence(
            "There are 9 innocents in total",
            SentenceKind::NumberOfTraitsInUnit(Unit::All, Quantity::Exact(9)),
            Judgment::Innocent,
        );
    }

    #[test]
    fn diane_0cf47() {
        sentence(
            "Xavi has more criminal neighbors than Ben",
            SentenceKind::MoreTraitsInUnitThanUnit {
                big: Unit::Neighbor("Xavi".into()),
                small: Unit::Neighbor("Ben".into()),
            },
            Judgment::Criminal,
        );
    }

    #[test]
    fn hal_0cf47() {
        sentence(
            "Emily and Tom have an equal number of criminal neighbors",
            SentenceKind::EqualNumberOfTraitsInUnits(
                ["Emily", "Tom"].map(Name::from).map(Unit::Neighbor),
            ),
            Judgment::Criminal,
        );
    }

    #[test]
    fn paul_0cf47() {
        sentence(
            "There are more criminals among guards than any other profession",
            SentenceKind::HasMostTraits(UnitInSeries::Profession("guard".into())),
            Judgment::Criminal,
        );
    }

    #[test]
    fn rob_0cf47() {
        sentence(
            "There are more criminals than innocents in a corner",
            SentenceKind::MoreTraitsInUnit(Unit::Corners),
            Judgment::Criminal,
        );
    }

    #[test]
    fn vicky_0cf47() {
        sentence(
            "Paul has the most criminal neighbors",
            SentenceKind::HasMostTraits(UnitInSeries::Neighbor("Paul".into())),
            Judgment::Criminal,
        );
    }

    #[test]
    fn noah_2026_02_11() {
        sentence(
            "Olof is a criminal",
            SentenceKind::HasTrait("Olof".into()),
            Judgment::Criminal,
        );
    }

    fn sentence(input: &str, kind: SentenceKind, judgment: Judgment) {
        parser(Sentence::any, input, &Sentence { kind, judgment });
    }

    fn parser<
        'input,
        P: Parser<&'input str, T, E>,
        T: PartialEq + fmt::Debug,
        E: ParserError<&'input str, Inner: fmt::Debug + ParserError<&'input str>>,
    >(
        mut parser: P,
        input: &'input str,
        expected: &T,
    ) {
        let output = parser.parse(input).unwrap();
        assert_eq!(&output, expected);
    }
}
