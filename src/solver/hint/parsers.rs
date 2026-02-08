use std::iter::once;

use anyhow::anyhow;
use winnow::ascii::dec_uint;
use winnow::combinator::{alt, delimited, opt, preceded, separated_pair, terminated};
use winnow::error::StrContext;
use winnow::token::{take_until, take_while};
use winnow::{Parser as _, Result};

use crate::solver::hint::{
    Direction, HintRecipe as Hint, Line, LineKind, Parity, Profession, Quantity, Unit,
};
use crate::solver::{Column, Judgment, Name, Row};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub(crate) enum Sentence {
    AllTraitsAreNeighborsInUnit(Unit, Judgment),
    HasMostTraits(Line, Judgment),
    IsOneOfNTraitsInUnit(Unit, Name, Judgment, Quantity),
    MoreTraitsInUnitThanUnit {
        big: Unit,
        small: Unit,
        judgment: Judgment,
    },
    NProfessionsHaveTraitInDir(Profession, Judgment, Direction, Quantity),
    NumberOfTraitsInUnit(Unit, Judgment, Quantity),
    OnlyOnePersonInUnitHasExactlyNTraitNeighbors(Unit, Judgment, Quantity),
    OnlyOneLineHasExactlyNTraits(LineKind, Judgment, Quantity),
    OnlyLineHasExactlyNTraits(Line, Judgment, Quantity),
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
    //TODO take Cow?
    pub(crate) fn parse(hint: &str) -> anyhow::Result<Self> {
        Self::parse_cased(hint).or_else(|e| {
            let mut hint = hint.to_owned();
            let Some(first) = hint.get_mut(..1) else {
                return Err(e);
            };
            first.make_ascii_lowercase();
            Self::parse_cased(&hint)
        })
    }

    pub(crate) fn collate(self) -> Vec<Hint> {
        match self {
            Self::AllTraitsAreNeighborsInUnit(unit, judgment) => {
                vec![Hint::Connected(unit, judgment)]
            }
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
            Self::OnlyOnePersonInUnitHasExactlyNTraitNeighbors(unit, judgment, quantity) => {
                vec![Hint::UniqueWithNeighbors(unit, judgment, quantity)]
            }
            Self::OnlyOneLineHasExactlyNTraits(line_kind, judgment, quantity) => {
                vec![Hint::UniqueLine(line_kind, judgment, quantity)]
            }
            Self::OnlyLineHasExactlyNTraits(line, judgment, quantity) => {
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
            Self::number_of_traits_in_unit,
            Self::only_one_person_in_unit_has_exactly_n_trait_neighbors,
            Self::only_one_line_has_exactly_n_traits,
            Self::only_line_has_exactly_n_traits,
            Self::unit_shares_n_out_of_n_traits_with_unit,
            Self::units_share_n_traits,
        ))
        .parse_next(input)
    }

    fn all_traits_are_neighbors_in_unit(input: &mut &str) -> Result<Self> {
        // "All criminals in row 2 are connected"
        delimited("All ", judged_unit, " are connected")
            .map(|(judgment, unit)| Self::AllTraitsAreNeighborsInUnit(unit, judgment))
            .parse_next(input)
    }

    fn has_most_traits(input: &mut &str) -> Result<Self> {
        // "Row 5 has more innocents than any other row"
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
        // "There are more innocent #PROFS than innocent #PROFS"
        preceded(
            "There are more ",
            separated_pair(judged_unit, " than ", judged_unit),
        )
        .verify_map(|((judgment_big, big), (judgement_small, small))| {
            (judgment_big == judgement_small).then_some(Self::MoreTraitsInUnitThanUnit {
                big,
                small,
                judgment: judgment_big,
            })
        })
        .parse_next(input)
    }

    fn n_professions_have_trait_in_dir(input: &mut &str) -> Result<Self> {
        // "Exactly 1 #PROF has a criminal directly above them"
        (
            quantified_profession,
            " has a ",
            judgment_singular,
            " directly ",
            direction,
            " them",
        )
            .map(|((count, profession), _, judgment, _, direction, _)| {
                Self::NProfessionsHaveTraitInDir(profession, judgment, direction, count)
            })
            .parse_next(input)
    }

    fn number_of_traits_in_unit(input: &mut &str) -> Result<Self> {
        // #NAME has exactly 2 innocent neighbors
        // There's an odd number of innocents neighboring #NAME
        // There's an odd number of innocents on the edges
        // There's an odd number of criminals neighboring #NAME
        // There's an odd number of criminals in row #R
        // There are exactly 2 innocents #BETWEEN
        // There is only one innocent #BETWEEN
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
        // Only one #PROF has exactly 4 criminal neighbors
        // Only one of us 2 #PROFS has exactly 2 criminal neighbors
        // Only one person in a corner has exactly 2 innocent neighbors
        separated_pair(
            preceded(alt(("Only one of ", "Only one ")), unit),
            " has ",
            terminated(
                separated_pair(quantity, " ", judgment_singular),
                " neighbors",
            ),
        )
        .map(|(unit, (count, judgment))| {
            Self::OnlyOnePersonInUnitHasExactlyNTraitNeighbors(unit, judgment, count)
        })
        .parse_next(input)
    }

    fn only_one_line_has_exactly_n_traits(input: &mut &str) -> Result<Self> {
        //"Only one column has exactly 2 innocents"
        separated_pair(
            preceded("Only one ", line_kind),
            " has ",
            quantified_judgment,
        )
        .map(|(kind, (count, judgment))| Self::OnlyOneLineHasExactlyNTraits(kind, judgment, count))
        .parse_next(input)
    }

    fn only_line_has_exactly_n_traits(input: &mut &str) -> Result<Self> {
        // Row #R is the only row with exactly 2 innocents
        // Row #R is the only row with exactly 2 criminals
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
                Self::OnlyLineHasExactlyNTraits(line, judgment, count)
            })
            .parse_next(input)
    }

    fn unit_shares_n_out_of_n_traits_with_unit(input: &mut &str) -> Result<Self> {
        // Exactly 2 of the #N innocents neighboring #NAME are #BETWEEN
        // Only 1 of the 2 innocents in column #C is #NAMES neighbor
        // Only 1 of the #N innocents neighboring #NAME is #BETWEEN
        // Only 1 of the #N innocents neighboring #NAME is #NAMES neighbor
        separated_pair(
            separated_pair(
                preceded(opt("Only "), quantity),
                " of the ",
                quantified_judged_unit,
            ),
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
        // #NAME and #NAME have only one innocent neighbor in common
        // An odd number of innocents #BETWEEN neighbor #NAME
        // There are no innocents #BETWEEN who neighbor #NAME
        alt((
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
            separated_pair(
                quantified_judged_unit,
                alt((" neighbor ", " is neighboring ")),
                name,
            )
            .map(|((count, judgment, unit), name)| (unit, Unit::Neighbor(name), judgment, count)),
        ))
        .map(|(a, b, judgment, count)| Self::UnitsShareNTraits(a, b, judgment, count))
        .parse_next(input)
    }
}

fn unit(input: &mut &str) -> Result<Unit> {
    // in Row 2
    alt((
        preceded("in ", line).map(Unit::Line),
        separated_pair(direction, " ", name)
            .map(|(direction, name)| Unit::Direction(direction, name)),
        (
            opt(delimited((determiner, " "), quantity, " ")),
            profession_any,
        )
            .map(|(count, profession)| Unit::Profession(profession, count)),
    ))
    .parse_next(input)
}

fn judged_unit(input: &mut &str) -> Result<(Judgment, Unit)> {
    // criminals in row 2
    // innocent #PROFS
    qualified_unit
        .verify_map(|(count, judgment, unit)| {
            if count.is_none() {
                Some((judgment?, unit))
            } else {
                None
            }
        })
        .parse_next(input)
}

fn quantified_judged_unit(input: &mut &str) -> Result<(Quantity, Judgment, Unit)> {
    // #NAMES #N innocent neighbors
    alt((
        separated_pair(
            separated_pair(quantity, " ", judgment_plural),
            " neighboring ",
            name,
        )
        .map(|((count, judgment), name)| (count, judgment, Unit::Neighbor(name))),
        (name_possessive, " ", quantified_judgment, " neighbors")
            .map(|(name, _, (quantity, judgment), _)| (quantity, judgment, Unit::Neighbor(name))),
        terminated(quantified_judgment, " on the edges")
            .map(|(quantity, judgment)| (quantity, judgment, Unit::Edges)),
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
        .map(|(quantity, judgment, unit)| (quantity, judgment, unit))
        .parse_next(input)
}

fn line(input: &mut &str) -> Result<Line> {
    // Row 5
    alt((row.map(Line::Row), column.map(Line::Column))).parse_next(input)
}

fn line_kind(input: &mut &str) -> Result<LineKind> {
    // row
    alt(("row".value(LineKind::Row), "column".value(LineKind::Column))).parse_next(input)
}

fn quantity(input: &mut &str) -> Result<Quantity> {
    alt((
        preceded(
            opt(alt(("exactly ", "only "))),
            alt((dec_uint, "one".value(1))).map(Quantity::Exact),
        ),
        delimited("an ", parity, " number of").map(Quantity::Parity),
    ))
    .parse_next(input)
}

fn parity(input: &mut &str) -> Result<Parity> {
    alt(("even".value(Parity::Even), "odd".value(Parity::Odd))).parse_next(input)
}

fn quantified_judgment(input: &mut &str) -> Result<(Quantity, Judgment)> {
    // exactly 2 innocent
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
    name.verify_map(|s| s.strip_suffix("'s").map(str::to_owned))
        .parse_next(input)
}

fn name(input: &mut &str) -> Result<Name> {
    take_while(1.., |c| c != ' ')
        .verify(|s: &str| s.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
        .map(str::to_owned)
        .parse_next(input)
}

fn quantified_profession(input: &mut &str) -> Result<(Quantity, Profession)> {
    separated_pair(quantity, " ", profession_any).parse_next(input)
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
    alt((profession_plural, profession_singular)).parse_next(input)
}

fn profession_singular(input: &mut &str) -> Result<Profession> {
    take_until(1.., ' ').map(str::to_owned).parse_next(input)
}

fn profession_plural(input: &mut &str) -> Result<Profession> {
    take_while(2.., |c| c != ' ')
        .verify_map(|s: &str| s.strip_suffix('s'))
        .map(str::to_owned)
        .parse_next(input)
}

fn row(input: &mut &str) -> Result<Row> {
    preceded(
        alt(("row\u{A0}", "row ")),
        alt((
            "1".value(Row::One),
            "2".value(Row::Two),
            "3".value(Row::Three),
            "4".value(Row::Four),
            "5".value(Row::Five),
        )),
    )
    .parse_next(input)
}

fn column(input: &mut &str) -> Result<Column> {
    preceded(
        alt(("column\u{A0}", "column ")),
        alt((
            "A".value(Column::A),
            "B".value(Column::B),
            "C".value(Column::C),
            "D".value(Column::D),
        )),
    )
    .parse_next(input)
}

#[cfg(test)]
mod tests {
    use std::fmt;

    use winnow::Parser;
    use winnow::error::ParserError;

    use crate::solver::hint::parsers::Sentence;
    use crate::solver::hint::{Direction, Parity, Quantity, Unit};
    use crate::solver::{Judgment, Row};

    #[test]
    fn alice_2026_02_05() {
        test_parser(
            Sentence::any,
            "Tina is one of 3 criminals in row\u{A0}4",
            &Sentence::IsOneOfNTraitsInUnit(
                Row::Four.into(),
                "Tina".to_owned(),
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
                quantified: Unit::Neighbor("Gary".to_owned()),
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
                Unit::Neighbor("Gary".to_owned()),
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
                Unit::Neighbor("Xavi".to_owned()),
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
                Unit::Neighbor("Xavi".to_owned()),
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
                Unit::Neighbor("Gary".to_owned()),
                Judgment::Innocent,
                Quantity::Parity(Parity::Odd),
            ),
        );
    }

    #[ignore = "unknown category"]
    #[test]
    fn tina_2026_02_05() {
        test_parser(
            Sentence::any,
            "both criminals above Xavi are connected",
            &Sentence::AllTraitsAreNeighborsInUnit(
                Unit::Direction(Direction::Above, "Xavi".to_owned()),
                Judgment::Criminal,
            ),
        );
    }

    #[test]
    fn freya_2026_02_06() {
        test_parser(
            Sentence::any,
            "Only one of us 2 singers has exactly 2 criminal neighbors",
            &Sentence::OnlyOnePersonInUnitHasExactlyNTraitNeighbors(
                Unit::Profession("singer".to_owned(), Some(Quantity::Exact(2))),
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
                Unit::Neighbor("Ellie".to_owned()),
                "Jason".to_owned(),
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
                Unit::Neighbor("Ellie".to_owned()),
                Unit::Neighbor("Noah".to_owned()),
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
                Unit::Direction(Direction::Left, "Noah".to_owned()),
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
                big: Unit::Profession("cook".to_owned(), None),
                small: Unit::Profession("mech".to_owned(), None),
                judgment: Judgment::Innocent,
            },
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
