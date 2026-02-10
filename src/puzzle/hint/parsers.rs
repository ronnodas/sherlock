use std::iter::once;

use anyhow::anyhow;
use mitsein::vec1::vec1;
use winnow::ascii::dec_uint;
use winnow::combinator::{alt, delimited, opt, preceded, separated_pair, terminated};
use winnow::error::{ParserError, StrContext};
use winnow::token::take_while;
use winnow::{Parser, Result};

use crate::puzzle::Judgment;
use crate::puzzle::grid::{Column, Row};

use super::recipes::{HintRecipe as Hint, NameRecipe as Name, SetRecipe as Set};
use super::{Direction, Line, LineKind, Parity, Profession, Quantity};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub(crate) enum Sentence {
    TraitsAreNeighborsInUnit(Unit, Judgment, Option<Quantity>),
    HasMostTraits(Line, Judgment),
    IsOneOfNTraitsInUnit(Unit, Name, Judgment, Quantity),
    EqualNumberOfTraitsInUnits(Unit, Unit, Judgment),
    MoreTraitsInUnitThanUnit {
        big: Unit,
        small: Unit,
        judgment: Judgment,
    },
    NProfessionsHaveTraitInDir(Profession, Judgment, Direction, Quantity),
    NumberOfTraitsInUnit(Unit, Judgment, Quantity),
    OnlyOnePersonInUnitHasNTraitNeighbors(Unit, Judgment, Quantity),
    EachLineHasNTraits(LineKind, Judgment, Quantity),
    OnlyOneLineHasNTraits(LineKind, Judgment, Quantity),
    OnlyGivenLineHasNTraits(Line, Judgment, Quantity),
    UnitSharesNOutOfNTraitsWithUnit {
        quantity: Quantity,
        quantified: Unit,
        other: Unit,
        judgment: Judgment,
        intersection: Quantity,
    },
    UnitsShareNTraits(Unit, Unit, Judgment, Quantity),
}

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
        match self {
            Self::TraitsAreNeighborsInUnit(unit, judgment, quantity) => quantity
                .map(|quantity| Hint::Count(unit.clone().into(), judgment, quantity))
                .into_iter()
                .chain(once(Hint::Connected(unit, judgment)))
                .collect(),
            Self::HasMostTraits(line, judgment) => line
                .others()
                .into_iter()
                .map(|other| Hint::Bigger {
                    big: line.into(),
                    small: other.into(),
                    judgment,
                })
                .collect(),
            Self::IsOneOfNTraitsInUnit(unit, name, judgment, quantity) => {
                vec![
                    Hint::Count(unit.clone().into(), judgment, quantity),
                    Hint::Member(name, unit, judgment),
                ]
            }
            Self::MoreTraitsInUnitThanUnit {
                big,
                small,
                judgment,
            } => vec![Hint::Bigger {
                big,
                small,
                judgment,
            }],
            Self::NProfessionsHaveTraitInDir(profession, judgment, direction, quantity) => {
                vec![Hint::Count(
                    Unit::ProfessionShift(profession, direction).into(),
                    judgment,
                    quantity,
                )]
            }
            Self::NumberOfTraitsInUnit(unit, judgment, quantity) => {
                vec![Hint::Count(unit.into(), judgment, quantity)]
            }
            Self::OnlyOnePersonInUnitHasNTraitNeighbors(unit, judgment, quantity) => {
                vec![Hint::UniqueWithNeighbors(unit, judgment, quantity)]
            }
            Self::OnlyOneLineHasNTraits(line_kind, judgment, quantity) => {
                vec![Hint::UniqueLine(line_kind, judgment, quantity)]
            }
            Self::EachLineHasNTraits(kind, judgment, quantity) => kind
                .all()
                .into_iter()
                .map(|line| Hint::Count(line.into(), judgment, quantity))
                .collect(),
            Self::OnlyGivenLineHasNTraits(line, judgment, quantity) => {
                let equal = Hint::Count(line.into(), judgment, quantity);
                line.others()
                    .into_iter()
                    .map(|other| Hint::Count(other.into(), judgment, quantity).not())
                    .chain(once(equal))
                    .collect()
            }
            Self::UnitSharesNOutOfNTraitsWithUnit {
                quantity,
                quantified,
                other,
                judgment,
                intersection,
            } => vec![
                Hint::Count(quantified.clone().into(), judgment, quantity),
                Hint::Count(quantified.and(other), judgment, intersection),
            ],
            Self::UnitsShareNTraits(a, b, judgment, quantity) => {
                vec![Hint::Count(a.and(b), judgment, quantity)]
            }
            Self::EqualNumberOfTraitsInUnits(a, b, judgment) => {
                vec![Hint::EqualSize(a, b, judgment)]
            }
        }
    }

    fn parse_cased(hint: &str) -> anyhow::Result<Self> {
        Self::any.parse(hint).map_err(|e| anyhow!("{e}"))
    }

    fn any(input: &mut &str) -> Result<Self> {
        alt((
            Self::all_traits_are_neighbors_in_unit,
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
        ))
        .parse_next(input)
    }

    fn all_traits_are_neighbors_in_unit(input: &mut &str) -> Result<Self> {
        terminated(
            separated_pair(
                alt(("All".value(None), quantity.map(Some))),
                " ",
                judged_unit,
            ),
            " are connected",
        )
        .map(|(quantity, (judgment, unit))| {
            Self::TraitsAreNeighborsInUnit(unit, judgment, quantity)
        })
        .parse_next(input)
    }

    fn has_most_traits(input: &mut &str) -> Result<Self> {
        (
            line,
            " has more ",
            judgment_plural,
            " than any other ",
            line_kind,
        )
            .verify(|&(line, _, _, _, kind)| line.kind() == kind)
            .map(|(line, _, judgment, _, _)| Self::HasMostTraits(line, judgment))
            .parse_next(input)
    }

    fn is_one_of_n_traits_in_unit(input: &mut &str) -> Result<Self> {
        separated_pair(name, " is one of ", quantified_judged_unit)
            .map(|(name, (count, judgment, unit))| {
                Self::IsOneOfNTraitsInUnit(unit, name, judgment, count)
            })
            .parse_next(input)
    }

    fn more_traits_in_unit_than_unit(input: &mut &str) -> Result<Self> {
        preceded(
            "There are more ",
            separated_pair(judged_unit, " than ", maybe_judged_unit),
        )
        .verify_map(|((judgment, big), (judgment_small, small))| {
            judgment_small
                .is_none_or(|small| small == judgment)
                .then_some(Self::MoreTraitsInUnitThanUnit {
                    big,
                    small,
                    judgment,
                })
        })
        .parse_next(input)
    }

    fn n_professions_have_trait_in_dir(input: &mut &str) -> Result<Self> {
        (
            quantified_profession,
            alt((" has ", " have ")),
            alt(("an ", "a ")),
            judgment_singular,
            " directly ",
            direction,
            alt((" them", " us")),
        )
            .map(|((count, profession), _, _, judgment, _, direction, _)| {
                Self::NProfessionsHaveTraitInDir(profession, judgment, direction, count)
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
        .map(|(count, judgment, unit)| Self::NumberOfTraitsInUnit(unit, judgment, count))
        .parse_next(input)
    }

    fn only_one_person_in_unit_has_exactly_n_trait_neighbors(input: &mut &str) -> Result<Self> {
        separated_pair(
            preceded((alt(("Only one of ", "Only one ")), opt("person ")), unit),
            " has ",
            terminated(
                separated_pair(quantity, " ", judgment_singular),
                " neighbors",
            ),
        )
        .map(|(unit, (count, judgment))| {
            Self::OnlyOnePersonInUnitHasNTraitNeighbors(unit, judgment, count)
        })
        .parse_next(input)
    }

    fn only_one_line_has_exactly_n_traits(input: &mut &str) -> Result<Self> {
        separated_pair(
            preceded("Only one ", line_kind),
            " has ",
            quantified_judgment,
        )
        .map(|(kind, (count, judgment))| Self::OnlyOneLineHasNTraits(kind, judgment, count))
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
            .map(|(line, _, _, _, (count, judgment))| {
                Self::OnlyGivenLineHasNTraits(line, judgment, count)
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
            |((intersection, (quantity, judgment, quantified)), other)| {
                Self::UnitSharesNOutOfNTraitsWithUnit {
                    quantified,
                    other,
                    judgment,
                    quantity,
                    intersection,
                }
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
            .map(|((count, judgment, unit), name)| (unit, Unit::Neighbor(name), judgment, count)),
            separated_pair(quantified_judged_unit, alt((" is ", " are ")), unit)
                .map(|((quantity, judgment, a), b)| (a, b, judgment, quantity)),
            (
                name,
                " and ",
                name,
                " have ",
                quantified_judgment,
                " neighbor",
                opt("s"),
                " in common",
            )
                .map(|(a, _, b, _, (count, judgment), _, _, _)| {
                    (Unit::Neighbor(a), Unit::Neighbor(b), judgment, count)
                }),
        ))
        .map(|(a, b, judgment, count)| Self::UnitsShareNTraits(a, b, judgment, count))
        .parse_next(input)
    }

    fn equal_number_of_traits_in_units(input: &mut &str) -> Result<Self> {
        alt((
            preceded(
                "There's an equal number of ",
                separated_pair(judgment_plural, " ", unit_pair),
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
        ))
        .map(|(judgment, [a, b])| Self::EqualNumberOfTraitsInUnits(a, b, judgment))
        .parse_next(input)
    }

    fn each_line_has_n_traits(input: &mut &str) -> Result<Self> {
        separated_pair(preceded("Each ", line_kind), " has ", quantified_judgment)
            .map(|(kind, (quantity, judgment))| Self::EachLineHasNTraits(kind, judgment, quantity))
            .parse_next(input)
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Debug)]
pub(crate) enum Unit {
    Direction(Direction, Name),
    Line(Line),
    Profession(Profession),
    ProfessionShift(Profession, Direction),
    Neighbor(Name),
    Between(Name, Name),
    Edges,
    Corners,
    All,
    Quantified(Box<Self>, Quantity),
}

impl Unit {
    pub(crate) fn and(self, other: Self) -> Set {
        Set::Intersection(vec1![self, other])
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
        separated_pair(direction, " ", name)
            .map(|(direction, name)| Unit::Direction(direction, name)),
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
        (name_possessive, " ", quantified_judgment, " neighbors")
            .map(|(name, _, (quantity, judgment), _)| (quantity, judgment, Unit::Neighbor(name))),
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
    separated_pair(quantity, " ", judgment_any).parse_next(input)
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

fn name_possessive(input: &mut &str) -> Result<Name> {
    alt((
        "my".value(Name::Me),
        raw_name
            .verify_map(|s| s.strip_suffix("'s"))
            .map(|name| Name::Other(name.to_owned())),
    ))
    .parse_next(input)
}

fn name(input: &mut &str) -> Result<Name> {
    alt((
        "me".value(Name::Me),
        raw_name.map(|name| Name::Other(name.to_owned())),
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
    preceded("between ", separated_pair(name, " and ", name))
        .map(|(a, b)| Unit::Between(a, b))
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
        line_prefixed("rows", separated_pair(row_bare, " and ", row_bare))
            .map(|rows| <[Row; 2]>::from(rows).map(Line::Row)),
        line_prefixed("columns", separated_pair(column_bare, " and ", column_bare))
            .map(|rows| <[Column; 2]>::from(rows).map(Line::Column)),
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

#[cfg(test)]
mod tests {
    use std::fmt;

    use winnow::Parser;
    use winnow::error::ParserError;

    use crate::puzzle::Judgment;
    use crate::puzzle::grid::{Column, Row};
    use crate::puzzle::hint::{Direction, LineKind, Parity, Quantity};

    use super::{Name, Sentence, Unit};

    #[test]
    fn uma_2026_02_03() {
        test_parser(
            Sentence::any,
            "exactly 1 judge has an innocent directly above them",
            &Sentence::NProfessionsHaveTraitInDir(
                "judge".into(),
                Judgment::Innocent,
                Direction::Above,
                Quantity::Exact(1),
            ),
        );
    }

    #[test]
    fn salil_2026_02_04() {
        test_parser(
            Sentence::any,
            "There are as many innocent builders as there are innocent guards",
            &Sentence::EqualNumberOfTraitsInUnits(
                Unit::Profession("builder".into()),
                Unit::Profession("guard".into()),
                Judgment::Innocent,
            ),
        );
    }

    #[test]
    fn alice_2026_02_05() {
        test_parser(
            Sentence::any,
            "Tina is one of 3 criminals in row\u{A0}4",
            &Sentence::IsOneOfNTraitsInUnit(
                Row::Four.into(),
                "Tina".into(),
                Judgment::Criminal,
                Quantity::Exact(3),
            ),
        );
    }

    #[test]
    fn chuck_2026_02_05() {
        test_parser(
            Sentence::any,
            "exactly 2 of the 4 innocents neighboring Gary are in row\u{a0}1",
            &Sentence::UnitSharesNOutOfNTraitsWithUnit {
                quantity: Quantity::Exact(4),
                quantified: Unit::Neighbor("Gary".into()),
                other: Row::One.into(),
                judgment: Judgment::Innocent,
                intersection: Quantity::Exact(2),
            },
        );
    }

    #[test]
    fn ethan_2026_02_05() {
        test_parser(
            Sentence::any,
            "an odd number of innocents on the edges neighbor Gary",
            &Sentence::UnitsShareNTraits(
                Unit::Edges,
                Unit::Neighbor("Gary".into()),
                Judgment::Innocent,
                Quantity::Parity(Parity::Odd),
            ),
        );
    }

    #[test]
    fn gary_2026_02_05() {
        test_parser(
            Sentence::any,
            "exactly 1 innocent in row\u{a0}4 is neighboring Xavi",
            &Sentence::UnitsShareNTraits(
                Row::Four.into(),
                Unit::Neighbor("Xavi".into()),
                Judgment::Innocent,
                Quantity::Exact(1),
            ),
        );
    }

    #[test]
    fn ike_2026_02_05() {
        test_parser(
            Sentence::any,
            "Xavi has exactly 3 innocent neighbors",
            &Sentence::NumberOfTraitsInUnit(
                Unit::Neighbor("Xavi".into()),
                Judgment::Innocent,
                Quantity::Exact(3),
            ),
        );
    }

    #[test]
    fn kyle_2026_02_05() {
        test_parser(
            Sentence::any,
            "an odd number of innocents above Zara neighbor Gary",
            &Sentence::UnitsShareNTraits(
                Unit::Direction(Direction::Above, "Zara".into()),
                Unit::Neighbor("Gary".into()),
                Judgment::Innocent,
                Quantity::Parity(Parity::Odd),
            ),
        );
    }

    #[test]
    fn tina_2026_02_05() {
        test_parser(
            Sentence::any,
            "both criminals above Xavi are connected",
            &Sentence::TraitsAreNeighborsInUnit(
                Unit::Direction(Direction::Above, "Xavi".into()),
                Judgment::Criminal,
                Some(Quantity::Exact(2)),
            ),
        );
    }

    #[test]
    fn vera_2026_02_05() {
        test_parser(
            Sentence::any,
            "Each column has at least 3 innocents",
            &Sentence::EachLineHasNTraits(
                LineKind::Column,
                Judgment::Innocent,
                Quantity::AtLeast(3),
            ),
        );
    }

    #[test]
    fn freya_2026_02_06() {
        test_parser(
            Sentence::any,
            "Only one of us 2 singers has exactly 2 criminal neighbors",
            &Sentence::OnlyOnePersonInUnitHasNTraitNeighbors(
                Unit::Profession("singer".to_owned()).quantify(Quantity::Exact(2)),
                Judgment::Criminal,
                Quantity::Exact(2),
            ),
        );
    }

    #[test]
    fn helen_2026_02_06() {
        test_parser(
            Sentence::any,
            "Jason is one of Ellie's 4 innocent neighbors",
            &Sentence::IsOneOfNTraitsInUnit(
                Unit::Neighbor("Ellie".into()),
                "Jason".into(),
                Judgment::Innocent,
                Quantity::Exact(4),
            ),
        );
    }

    #[test]
    fn jason_2026_02_06() {
        test_parser(
            Sentence::any,
            "Ellie and Noah have only one innocent neighbor in common",
            &Sentence::UnitsShareNTraits(
                Unit::Neighbor("Ellie".into()),
                Unit::Neighbor("Noah".into()),
                Judgment::Innocent,
                Quantity::Exact(1),
            ),
        );
    }

    #[test]
    fn logan_2026_02_06() {
        test_parser(
            Sentence::any,
            "exactly 1 farmer has a criminal directly above them",
            &Sentence::NProfessionsHaveTraitInDir(
                "farmer".to_owned(),
                Judgment::Criminal,
                Direction::Above,
                Quantity::Exact(1),
            ),
        );
    }

    #[test]
    fn ivan_2026_02_06() {
        test_parser(
            Sentence::any,
            "row 5 has more innocents than any other row",
            &Sentence::HasMostTraits(Row::Five.into(), Judgment::Innocent),
        );
    }

    #[test]
    fn scott_2026_02_06() {
        test_parser(
            Sentence::any,
            "There are exactly 2 innocents to the left of Noah",
            &Sentence::NumberOfTraitsInUnit(
                Unit::Direction(Direction::Left, "Noah".into()),
                Judgment::Innocent,
                Quantity::Exact(2),
            ),
        );
    }

    #[test]
    fn vera_2026_02_06() {
        test_parser(
            Sentence::any,
            "There are more innocent cooks than innocent mechs",
            &Sentence::MoreTraitsInUnitThanUnit {
                big: Unit::Profession("cook".to_owned()),
                small: Unit::Profession("mech".to_owned()),
                judgment: Judgment::Innocent,
            },
        );
    }

    #[test]
    fn gary_2026_02_07() {
        test_parser(
            Sentence::any,
            "only 1 of the 2 innocents in column\u{a0}C is Zara's neighbor",
            &Sentence::UnitSharesNOutOfNTraitsWithUnit {
                quantity: Quantity::Exact(2),
                quantified: Column::C.into(),
                other: Unit::Neighbor("Zara".into()),
                judgment: Judgment::Innocent,
                intersection: Quantity::Exact(1),
            },
        );
    }

    #[test]
    fn uma_2026_02_07() {
        test_parser(
            Sentence::any,
            "only 1 of the 3 innocents neighboring me is to the right of Kay",
            &Sentence::UnitSharesNOutOfNTraitsWithUnit {
                quantity: Quantity::Exact(3),
                quantified: Unit::Neighbor(Name::Me),
                other: Unit::Direction(Direction::Right, "Kay".into()),
                judgment: Judgment::Innocent,
                intersection: Quantity::Exact(1),
            },
        );
    }

    #[test]
    fn xena_2026_02_08() {
        test_parser(
            Sentence::any,
            "There are no innocents in row 1 who neighbor Donna",
            &Sentence::UnitsShareNTraits(
                Row::One.into(),
                Unit::Neighbor("Donna".into()),
                Judgment::Innocent,
                Quantity::Exact(0),
            ),
        );
    }

    #[test]
    fn hank_2026_02_08() {
        test_parser(
            Sentence::any,
            "Only one person in a corner has exactly 2 innocent neighbors",
            &Sentence::OnlyOnePersonInUnitHasNTraitNeighbors(
                Unit::Corners,
                Judgment::Innocent,
                Quantity::Exact(2),
            ),
        );
    }

    #[test]
    fn tina_2026_02_09() {
        test_parser(
            Sentence::any,
            "exactly 2 innocents in column C are neighboring me",
            &Sentence::UnitsShareNTraits(
                Column::C.into(),
                Unit::Neighbor(Name::Me),
                Judgment::Innocent,
                Quantity::Exact(2),
            ),
        );
    }

    #[test]
    fn kumar_2026_02_09() {
        test_parser(
            Sentence::any,
            "exactly 2 of the 3 innocents in row 5 are Susan's neighbors",
            &Sentence::UnitSharesNOutOfNTraitsWithUnit {
                quantity: Quantity::Exact(3),
                quantified: Row::Five.into(),
                other: Unit::Neighbor("Susan".into()),
                judgment: Judgment::Innocent,
                intersection: Quantity::Exact(2),
            },
        );
    }

    #[test]
    fn xavi_2026_02_09() {
        test_parser(
            Sentence::any,
            "There are more innocents in row 3 than row 5",
            &Sentence::MoreTraitsInUnitThanUnit {
                big: Row::Three.into(),
                small: Row::Five.into(),
                judgment: Judgment::Innocent,
            },
        );
    }

    #[test]
    fn ollie_2026_02_09() {
        test_parser(
            Sentence::any,
            "There's an equal number of innocents in rows 2 and 3",
            &Sentence::EqualNumberOfTraitsInUnits(
                Row::Two.into(),
                Row::Three.into(),
                Judgment::Innocent,
            ),
        );
    }

    #[test]
    fn gabe_2026_02_09() {
        test_parser(
            Sentence::any,
            "There are at least 10 innocents on the edges",
            &Sentence::NumberOfTraitsInUnit(Unit::Edges, Judgment::Innocent, Quantity::AtLeast(10)),
        );
    }

    #[test]
    fn gary_2026_02_10() {
        test_parser(
            Sentence::any,
            "Ryan is one of 2 innocents in between Betty and Vicky",
            &Sentence::IsOneOfNTraitsInUnit(
                Unit::Between("Betty".into(), "Vicky".into()),
                "Ryan".into(),
                Judgment::Innocent,
                Quantity::Exact(2),
            ),
        );
    }

    #[test]
    fn lisa_2026_02_10() {
        test_parser(
            Sentence::any,
            "exactly 1 innocent on the edges is a farmer",
            &Sentence::UnitsShareNTraits(
                Unit::Edges,
                Unit::Profession("farmer".to_owned()),
                Judgment::Innocent,
                Quantity::Exact(1),
            ),
        );
    }

    #[test]
    fn will_2026_02_10() {
        test_parser(
            Sentence::any,
            "2 of us 3 singers have an innocent directly to the left of us",
            &Sentence::NProfessionsHaveTraitInDir(
                "singer".into(),
                Judgment::Innocent,
                Direction::Left,
                Quantity::Exact(2),
            ),
        );
    }

    #[test]
    fn janet_2026_02_10() {
        test_parser(
            Sentence::any,
            "There are 9 innocents in total",
            &Sentence::NumberOfTraitsInUnit(Unit::All, Judgment::Innocent, Quantity::Exact(9)),
        );
    }

    fn test_parser<
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
