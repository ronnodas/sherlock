use std::iter::once;

use anyhow::anyhow;
use winnow::combinator::{alt, delimited, opt, preceded, separated_pair, terminated};
use winnow::error::StrContext;
use winnow::{Parser as _, Result};

use crate::solver::hint::{
    Direction, HintRecipe as Hint, Line, LineKind, Profession, Quantity, Unit,
};
use crate::solver::{Judgment, Name};

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
        // "#NAME is one of #NAMES #N innocent neighbors"
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
            "them",
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
            preceded(
                alt(("Only one ", delimited("Only one of ", determiner, " "))),
                unit,
            ),
            " has exactly ",
            terminated((count, judgment_singular), "neighbors"),
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
                preceded(opt("Only "), count),
                " of the ",
                quantified_judged_unit,
            ),
            alt(("is", "are")),
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
                count,
                " ",
                judgment_singular,
                " neighbor",
                opt("s"),
                " in common",
            )
                .map(|(a, _, b, _, count, _, judgment, _, _, _)| {
                    (Unit::Neighbor(a), Unit::Neighbor(b), judgment, count)
                }),
            separated_pair(quantified_judged_unit, " neighbor ", name).map(
                |((count, judgment, unit), name)| (unit, Unit::Neighbor(name), judgment, count),
            ),
        ))
        .map(|(a, b, judgment, count)| Self::UnitsShareNTraits(a, b, judgment, count))
        .parse_next(input)
    }
}

fn unit(input: &mut &str) -> Result<Unit> {
    // in Row 2
    todo!()
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
    qualified_unit
        .verify_map(|(count, judgment, unit)| Some((count?, judgment?, unit)))
        .parse_next(input)
}

fn qualified_unit(input: &mut &str) -> Result<(Option<Quantity>, Option<Judgment>, Unit)> {
    todo!()
}

fn line(input: &mut &str) -> Result<Line> {
    // Row 5
    todo!()
}

fn line_kind(input: &mut &str) -> Result<LineKind> {
    // row
    alt(("row".value(LineKind::Row), "column".value(LineKind::Column))).parse_next(input)
}

fn count(input: &mut &str) -> Result<Quantity> {
    todo!()
}

fn quantified_judgment(input: &mut &str) -> Result<(Quantity, Judgment)> {
    // exactly 2 innocent
    todo!()
}

fn judgment_plural(input: &mut &str) -> Result<Judgment> {
    // innocents
    todo!()
}

fn judgment_singular(input: &mut &str) -> Result<Judgment> {
    // criminal
    todo!()
}

fn name(input: &mut &str) -> Result<Name> {
    todo!()
}

fn quantified_profession(input: &mut &str) -> Result<(Quantity, Profession)> {
    // Exactly 1 #PROF
    todo!()
}

fn direction(input: &mut &str) -> Result<Direction> {
    // above
    todo!()
}

fn determiner<'input>(input: &mut &'input str) -> Result<&'input str> {
    todo!()
}
