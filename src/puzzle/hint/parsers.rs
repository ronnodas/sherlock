mod phrases;

use std::iter::once;

use anyhow::anyhow;
use itertools::Itertools as _;
use winnow::ascii::dec_uint;
use winnow::combinator::{
    alt, delimited, dispatch, empty, eof, fail, opt, preceded, separated_pair, terminated,
};
use winnow::error::{ParserError, StrContext};
use winnow::token::{any, rest, take_while};
use winnow::{Parser, Result};

use crate::puzzle::Judgment;
use crate::puzzle::grid::{Column, Row};
use crate::puzzle::hint::recipes::NameRecipe;
use crate::puzzle::hint::{Direction, Line, LineKind, Parity, Profession, Quantity, WithJudgment};

pub(crate) type Sentence = WithJudgment<SentenceKind>;
pub(crate) use phrases::{SentenceKind, Series, Unit, UnitInSeries};

impl Sentence {
    pub(crate) fn parse(hint: &str) -> anyhow::Result<Self> {
        let mut hint = hint
            .split(' ')
            .filter(|word| !word.is_empty())
            .collect_vec();
        Self::parse_cased(&hint).or_else(move |e| {
            if let Some(first) = hint.first_mut() {
                let mut word = (*first).to_owned();
                if let Some(first_char) = word.get_mut(..1) {
                    first_char.make_ascii_lowercase();
                    let hint = once(&*word).chain(hint.into_iter().skip(1)).collect_vec();

                    if let Ok(parsed) = Self::parse_cased(&hint) {
                        return Ok(parsed);
                    }
                }
            }
            Err(e)
        })
    }

    fn parse_cased(hint: &[&str]) -> anyhow::Result<Self> {
        Self::any.parse(hint).map_err(|e| anyhow!("{e:?}"))
    }

    fn any(input: &mut &[&str]) -> Result<Self> {
        alt((
            Self::traits_are_neighbors_in_unit,
            Self::has_most_traits,
            Self::is_one_of_n_traits_in_unit,
            Self::more_traits_in_unit_than_unit,
            Self::units_share_n_traits,
            Self::each_unit_in_series_has_n_traits,
            Self::number_of_traits_in_unit,
            Self::only_one_person_in_unit_has_exactly_n_trait_neighbors,
            Self::only_one_unit_in_series_has_exactly_n_traits,
            Self::only_given_unit_has_exactly_n_traits,
            Self::unit_shares_n_out_of_n_traits_with_unit,
            Self::equal_number_of_traits_in_units,
            Self::more_traits_in_unit,
            Self::has_trait,
            Self::at_most_n_traits_in_neighbors_in_unit,
        ))
        .parse_next(input)
    }

    fn traits_are_neighbors_in_unit(input: &mut &[&str]) -> Result<Self> {
        terminated(
            (
                alt((word("All").value(None), quantity.map(Some))),
                judged_unit,
            ),
            words(["are", "connected"]),
        )
        .map(|(quantity, (judgment, unit))| Self {
            kind: SentenceKind::TraitsAreNeighborsInUnit(unit, quantity),
            judgment,
        })
        .parse_next(input)
    }

    fn has_most_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                separated_pair(line, words(["has", "more"]), word(judgment_plural)),
                words(["than", "any", "other"]),
                word(line_kind),
            )
            .verify(|&((line, _), kind)| line.kind() == kind)
            .map(|((line, judgment), _)| Self {
                kind: SentenceKind::HasMostTraits(line.into()),
                judgment,
            }),
            delimited(
                words(["There", "are", "more"]),
                separated_pair(
                    word(judgment_plural),
                    word("among"),
                    word(profession_plural),
                ),
                words(["than", "any", "other", "profession"]),
            )
            .map(|(judgment, profession)| Self {
                kind: SentenceKind::HasMostTraits(UnitInSeries::Profession(profession)),
                judgment,
            }),
            (
                word(name),
                words(["has", "the", "most"]),
                word(judgment_singular),
                word("neighbors"),
            )
                .map(|(name, _, judgment, _)| Self {
                    kind: SentenceKind::HasMostTraits(UnitInSeries::Neighbor(name)),
                    judgment,
                }),
        ))
        .parse_next(input)
    }

    fn is_one_of_n_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        separated_pair(
            word(name),
            words(["is", "one", "of"]),
            quantified_judged_unit,
        )
        .map(|(name, (count, judgment, unit))| Self {
            kind: SentenceKind::IsOneOfNTraitsInUnit(unit, name, count),
            judgment,
        })
        .parse_next(input)
    }

    fn more_traits_in_unit_than_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            preceded(
                words(["There", "are", "more"]),
                separated_pair(judged_unit, word("than"), maybe_judged_unit),
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
                word(name),
                delimited(
                    words((has_have, "more")),
                    word(judgment_singular),
                    words(["neighbors", "than"]),
                ),
                word(name),
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

    fn number_of_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            preceded(there_is, quantified_judged_unit),
            separated_pair(word(name), word(has_have), quantified_judged_neighbors)
                .map(|(name, (quantity, judgment))| (quantity, judgment, Unit::Neighbor(name))),
            separated_pair(
                alt((
                    separated_pair(
                        quantity,
                        (words(["out", "of"]), word(determiner)),
                        quantified_profession,
                    )
                    .map(|(quantity, (total, profession))| (quantity, Some(total), profession)),
                    subset_of_profession,
                )),
                (word(has_have), word(alt(("an", "a")))),
                (
                    word(judgment_singular),
                    delimited(word("directly"), direction, word(alt(("them", "us")))),
                ),
            )
            .map(|((count, total, profession), (judgment, direction))| {
                let unit = Unit::ProfessionShift(profession, direction, total);
                (count, judgment, unit)
            }),
        ))
        .map(|(count, judgment, unit)| Self {
            kind: SentenceKind::NumberOfTraitsInUnit(unit, count),
            judgment,
        })
        .parse_next(input)
    }

    fn only_one_person_in_unit_has_exactly_n_trait_neighbors(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                preceded(
                    (words(["Only", "one"]), opt(word(alt(("of", "person"))))),
                    unit,
                ),
                word("has"),
                quantified_judged_neighbors,
            )
            .map(|(unit, (count, judgment))| Self {
                kind: SentenceKind::OnlyOnePersonInUnitHasNTraitNeighbors(unit, count, None),
                judgment,
            }),
            separated_pair(
                word(name),
                words(["is", "the", "only", "one"]),
                separated_pair(unit, word("with"), quantified_judged_neighbors),
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

    fn only_one_unit_in_series_has_exactly_n_traits(input: &mut &[&str]) -> Result<Self> {
        separated_pair(
            preceded(words(["Only", "one"]), word(line_kind)),
            word("has"),
            quantified_judgment,
        )
        .map(|(kind, (count, judgment))| Self {
            kind: SentenceKind::OnlyOneUnitInSeriesHasNTraits(kind.into(), count),
            judgment,
        })
        .parse_next(input)
    }

    fn only_given_unit_has_exactly_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                line,
                words(["is", "the", "only"]),
                separated_pair(word(line_kind), word("with"), quantified_judgment),
            )
            .verify(|&(line, (kind, _))| line.kind() == kind)
            .context(StrContext::Label("a matching row/column"))
            .map(|(line, (_, (quantity, judgment)))| (line.into(), quantity, judgment)),
            separated_pair(
                word(name),
                words(["is", "the", "only", "one", "with"]),
                quantified_judged_neighbors,
            )
            .map(|(name, (quantity, judgment))| (UnitInSeries::Neighbor(name), quantity, judgment)),
        ))
        .map(|(unit, count, judgment)| Self {
            kind: SentenceKind::OnlyGivenUnitHasNTraits(unit, count),
            judgment,
        })
        .parse_next(input)
    }

    fn unit_shares_n_out_of_n_traits_with_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                separated_pair(
                    quantity,
                    (word("of"), opt(word(determiner))),
                    quantified_judged_unit,
                ),
                word(alt(("is", "are"))),
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
                separated_pair(quantity, word("of"), possessive_quantified_judged_neighbors),
                (word("also"), word(neighbor_any)),
                word(name),
            )
            .map(
                |((intersection, (quantified, quantity, judgment)), other)| Self {
                    kind: SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
                        quantity,
                        quantified: Unit::Neighbor(quantified),
                        other: Unit::Neighbor(other),
                        intersection,
                    },
                    judgment,
                },
            ),
            separated_pair(
                preceded(words(["The", "only"]), judged_unit),
                word("is"),
                unit,
            )
            .map(|((judgment, quantified), other)| Self {
                kind: SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
                    quantity: Quantity::Exact(1),
                    quantified,
                    other,
                    intersection: Quantity::Exact(1),
                },
                judgment,
            }),
            separated_pair(
                separated_pair(word(name_possessive), word("only"), word(judgment_singular)),
                words(["neighbor", "is"]),
                terminated(word(name_possessive), word("neighbor")),
            )
            .map(|((quantified, judgment), other)| Self {
                kind: SentenceKind::UnitSharesNOutOfNTraitsWithUnit {
                    quantity: Quantity::Exact(1),
                    quantified: Unit::Neighbor(quantified),
                    other: Unit::Neighbor(other),
                    intersection: Quantity::Exact(1),
                },
                judgment,
            }),
        ))
        .parse_next(input)
    }

    fn units_share_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            (
                preceded(opt(there_is), quantified_judged_unit),
                alt((
                    preceded(neighboring_verb, word(name)).map(Unit::Neighbor),
                    preceded(opt(alt((word("is"), word("are")))), unit),
                )),
            )
                .map(|((count, judgment, unit), other)| ([unit, other], judgment, count)),
            (
                pair(name),
                word("have"),
                quantified_judgment,
                (word(neighbor_any), words(["in", "common"])),
            )
                .map(|(names, _, (count, judgment), _)| {
                    (names.map(Unit::Neighbor), judgment, count)
                }),
            separated_pair(
                separated_pair(
                    quantity,
                    word("of"),
                    separated_pair(word(name_possessive), word("neighbors"), unit),
                ),
                alt((word("is"), word("are"))),
                word(judgment_singular),
            )
            .map(|((quantity, (name, unit)), judgment)| {
                ([Unit::Neighbor(name), unit], judgment, quantity)
            }),
            separated_pair(
                word(name),
                word("has"),
                separated_pair(quantified_judgment, word(neighbor_any), unit),
            )
            .map(|(name, ((quantity, judgment), unit))| {
                ([Unit::Neighbor(name), unit], judgment, quantity)
            }),
        ))
        .map(|([a, b], judgment, quantity)| Self {
            kind: SentenceKind::UnitsShareNTraits([a, b], quantity),
            judgment,
        })
        .parse_next(input)
    }

    fn equal_number_of_traits_in_units(input: &mut &[&str]) -> Result<Self> {
        alt((
            preceded(
                words(["There's", "an", "equal", "number", "of"]),
                (word(judgment_plural), unit_pair),
            ),
            preceded(
                words(["There", "are", "as", "many"]),
                separated_pair(
                    judged_unit,
                    (word("as"), opt(words(["there", "are"]))),
                    judged_unit,
                ),
            )
            .verify_map(|((judgment_a, a), (judgment_b, b))| {
                (judgment_a == judgment_b).then_some((judgment_a, [a, b]))
            }),
            (
                pair(name),
                words(["have", "an", "equal", "number", "of"]),
                word(judgment_singular),
                word("neighbors"),
            )
                .map(|(names, _, judgment, _)| (judgment, names.map(Unit::Neighbor))),
        ))
        .map(|(judgment, pair)| Self {
            kind: SentenceKind::EqualNumberOfTraitsInUnits(pair),
            judgment,
        })
        .parse_next(input)
    }

    fn each_unit_in_series_has_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                preceded(word("Each"), word(line_kind)).map(Series::from),
                word("has"),
                quantified_judgment,
            ),
            separated_pair(
                word("Everyone").value(Series::Neighbor),
                word("has"),
                quantified_judged_neighbors,
            ),
        ))
        .map(|(series, (quantity, judgment))| Self {
            kind: SentenceKind::EachUnitInSeriesHasNTraits(series, quantity),
            judgment,
        })
        .parse_next(input)
    }

    fn more_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        preceded(
            words(["There", "are", "more"]),
            (
                separated_pair(word(judgment_plural), word("than"), word(judgment_plural)),
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

    fn has_trait(input: &mut &[&str]) -> Result<Self> {
        separated_pair(
            word(name),
            (word("is"), opt(word("a"))),
            word(judgment_singular),
        )
        .map(|(name, judgment)| Self {
            kind: SentenceKind::HasTrait(name),
            judgment,
        })
        .parse_next(input)
    }

    fn at_most_n_traits_in_neighbors_in_unit(input: &mut &[&str]) -> Result<Self> {
        separated_pair(
            preceded(words(["No", "one"]), unit),
            words(["has", "more", "than"]),
            (
                word(dec_uint),
                terminated(word(judgment_singular), word(neighbor_any)),
            ),
        )
        .map(|(unit, (number, judgment))| Self {
            kind: SentenceKind::AtMostNTraitsInNeighborsInUnit(unit, number),
            judgment,
        })
        .parse_next(input)
    }
}

fn unit_pair(input: &mut &[&str]) -> Result<[Unit; 2]> {
    preceded(word("in"), line_pair)
        .map(|lines| lines.map(Unit::Line))
        .parse_next(input)
}

fn unit(input: &mut &[&str]) -> Result<Unit> {
    alt((
        words(["in", "total"]).value(Unit::All),
        words(["on", "the", "edges"]).value(Unit::Edges),
        alt((
            words(["in", "a", "corner"]),
            words(["in", "the", "corners"]),
        ))
        .value(Unit::Corners),
        preceded(opt(word("in")), alt((between, line.map(Unit::Line)))),
        (direction, word(name)).map(|(direction, name)| Unit::Direction(direction, name)),
        terminated(word(name_possessive), word(neighbor_any)).map(Unit::Neighbor),
        (opt(preceded(word(determiner), quantity)), profession_any)
            .map(|(quantity, profession)| Unit::Profession(profession).maybe_quantify(quantity)),
    ))
    .parse_next(input)
}

fn maybe_judged_unit(input: &mut &[&str]) -> Result<(Option<Judgment>, Unit)> {
    qualified_unit
        .verify_map(|(count, judgment, unit)| count.is_none().then_some((judgment, unit)))
        .parse_next(input)
}

fn judged_unit(input: &mut &[&str]) -> Result<(Judgment, Unit)> {
    qualified_unit
        .verify_map(|(count, judgment, unit)| {
            let judgment = judgment?;
            count.is_none().then_some((judgment, unit))
        })
        .parse_next(input)
}

fn quantified_judged_unit(input: &mut &[&str]) -> Result<(Quantity, Judgment, Unit)> {
    alt((
        separated_pair(
            (quantity, word(judgment_plural)),
            word("neighboring"),
            word(name),
        )
        .map(|((quantity, judgment), name)| (quantity, judgment, Unit::Neighbor(name))),
        possessive_quantified_judged_neighbors
            .map(|(name, quantity, judgment)| (quantity, judgment, Unit::Neighbor(name))),
        qualified_unit.verify_map(|(count, judgment, unit)| Some((count?, judgment?, unit))),
    ))
    .parse_next(input)
}

fn qualified_unit(input: &mut &[&str]) -> Result<(Option<Quantity>, Option<Judgment>, Unit)> {
    (opt(quantity), opt(word(judgment_any)), unit).parse_next(input)
}

fn quantity(input: &mut &[&str]) -> Result<Quantity> {
    alt((
        word("both").value(Quantity::Exact(2)),
        word("no").value(Quantity::Exact(0)),
        terminated(number, words(["or", "more"])).map(Quantity::AtLeast),
        preceded(opt(alt((word("exactly"), word("only")))), number).map(Quantity::Exact),
        preceded(words(["at", "least"]), number).map(Quantity::AtLeast),
        delimited(word("an"), parity, words(["number", "of"])).map(Quantity::Parity),
    ))
    .parse_next(input)
}

fn number(input: &mut &[&str]) -> Result<u8> {
    alt((word(dec_uint), word("one").value(1), word("two").value(2))).parse_next(input)
}

fn parity(input: &mut &[&str]) -> Result<Parity> {
    alt((
        word("even").value(Parity::Even),
        word("odd").value(Parity::Odd),
    ))
    .parse_next(input)
}

fn quantified_judgment(input: &mut &[&str]) -> Result<(Quantity, Judgment)> {
    (quantity, word(judgment_any)).parse_next(input)
}

fn quantified_judged_neighbors(input: &mut &[&str]) -> Result<(Quantity, Judgment)> {
    terminated(quantified_judgment, word(neighbor_any)).parse_next(input)
}

fn possessive_quantified_judged_neighbors(
    input: &mut &[&str],
) -> Result<(NameRecipe, Quantity, Judgment)> {
    (word(name_possessive), quantified_judged_neighbors)
        .map(|(name, (quantity, judgment))| (name, quantity, judgment))
        .parse_next(input)
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
            .verify_map(|s| {
                s.strip_suffix("'s")
                    .or_else(|| s.strip_suffix("'").filter(|name| name.ends_with('s')))
            })
            .map(|name| NameRecipe::Other(name.to_owned())),
    ))
    .parse_next(input)
}

fn name(input: &mut &str) -> Result<NameRecipe> {
    alt((
        take_while(1.., |c| c != ' ')
            .verify(|name: &str| name == "I" || name == "me")
            .value(NameRecipe::Me),
        raw_name.map(|name| NameRecipe::Other(name.to_owned())),
    ))
    .parse_next(input)
}

fn raw_name<'input>(input: &mut &'input str) -> Result<&'input str> {
    take_while(1.., |c| c != ' ')
        .verify(|s: &str| s.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
        .parse_next(input)
}

fn quantified_profession(input: &mut &[&str]) -> Result<(Quantity, Profession)> {
    separated_pair(quantity, opt(words(["of", "the"])), profession_any).parse_next(input)
}

fn subset_of_profession(input: &mut &[&str]) -> Result<(Quantity, Option<Quantity>, Profession)> {
    (
        quantity,
        opt(preceded(words(("of", determiner)), opt(quantity))).map(Option::flatten),
        profession_any,
    )
        .parse_next(input)
}

fn direction(input: &mut &[&str]) -> Result<Direction> {
    alt((
        word("above").value(Direction::Above),
        word("below").value(Direction::Below),
        delimited(
            words(["to", "the"]),
            alt((
                word("left").value(Direction::Left),
                word("right").value(Direction::Right),
            )),
            word("of"),
        ),
    ))
    .parse_next(input)
}

fn determiner<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("the", "a", "an", "us")).parse_next(input)
}

fn profession_any(input: &mut &[&str]) -> Result<Profession> {
    alt((
        word(profession_plural),
        preceded(opt(alt((word("an"), word("a")))), word(profession_singular)),
    ))
    .parse_next(input)
}

fn profession_singular(input: &mut &str) -> Result<Profession> {
    rest.map(str::to_owned).parse_next(input)
}

fn profession_plural(input: &mut &str) -> Result<Profession> {
    rest.verify_map(|s: &str| s.strip_suffix('s'))
        .map(str::to_owned)
        .parse_next(input)
}

fn neighbor_any(input: &mut &str) -> Result<()> {
    alt(("neighbors", "neighbor")).void().parse_next(input)
}

fn there_is<'input, 'inner: 'input>(
    input: &mut &'input [&'inner str],
) -> Result<&'input [&'inner str]> {
    alt((
        words(["There", "are"]).take(),
        words(["There", "is"]).take(),
        word("There's").take(),
    ))
    .parse_next(input)
}

fn has_have<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("has", "have")).parse_next(input)
}

fn neighboring_verb<'input, 'inner: 'input>(
    input: &mut &'input [&'inner str],
) -> Result<&'input [&'inner str]> {
    alt((
        words(["who", "neighbor"]).take(),
        word("neighbor").take(),
        words(["is", "neighboring"]).take(),
        words(["are", "neighboring"]).take(),
    ))
    .parse_next(input)
}

fn between(input: &mut &[&str]) -> Result<Unit> {
    preceded(word("between"), pair(name))
        .map(Unit::Between)
        .parse_next(input)
}

fn line(input: &mut &[&str]) -> Result<Line> {
    alt((row.map(Line::Row), column.map(Line::Column))).parse_next(input)
}

fn line_kind(input: &mut &str) -> Result<LineKind> {
    alt(("row".value(LineKind::Row), "column".value(LineKind::Column))).parse_next(input)
}

fn line_pair(input: &mut &[&str]) -> Result<[Line; 2]> {
    alt((
        separated_pair(line_prefixed("rows", row_bare), word("and"), word(row_bare))
            .map(|rows| <[Row; 2]>::from(rows).map(Line::Row)),
        separated_pair(
            line_prefixed("columns", column_bare),
            word("and"),
            word(column_bare),
        )
        .map(|rows| <[Column; 2]>::from(rows).map(Line::Column)),
    ))
    .parse_next(input)
}

fn row(input: &mut &[&str]) -> Result<Row> {
    line_prefixed("row", row_bare).parse_next(input)
}

fn line_prefixed<'input, 'inner, T, E>(
    prefix: &'static str,
    inner: impl Parser<&'inner str, T, E>,
) -> impl Parser<&'input [&'inner str], T, E>
where
    'inner: 'input,
    E: ParserError<&'input [&'inner str]> + ParserError<&'inner str>,
{
    alt((
        preceded(word(prefix), any),
        any.verify_map(move |s: &str| {
            let rest = s.strip_prefix(prefix)?;
            rest.strip_prefix("\u{A0}")
                .or_else(|| rest.strip_prefix("&nbsp;"))
        }),
    ))
    .and_then(inner)
}

fn row_bare(input: &mut &str) -> Result<Row> {
    dispatch!(any;
        '1' => empty.value(Row::One),
        '2' => empty.value(Row::Two),
        '3' => empty.value(Row::Three),
        '4' => empty.value(Row::Four),
        '5' => empty.value(Row::Five),
        _ => fail,
    )
    .parse_next(input)
}

fn column(input: &mut &[&str]) -> Result<Column> {
    line_prefixed("column", column_bare).parse_next(input)
}

fn column_bare(input: &mut &str) -> Result<Column> {
    dispatch!(any;
        'A' => empty.value(Column::A),
        'B' => empty.value(Column::B),
        'C' => empty.value(Column::C),
        'D' => empty.value(Column::D),
        _ => fail,
    )
    .parse_next(input)
}

fn pair<
    'input,
    'inner: 'input,
    T,
    E: ParserError<&'input [&'inner str]> + ParserError<&'inner str>,
>(
    inner: impl Parser<&'inner str, T, E> + Copy,
) -> impl Parser<&'input [&'inner str], [T; 2], E> {
    separated_pair(word(inner), word("and"), word(inner)).map(Into::into)
}

fn word<
    'input,
    'inner: 'input,
    O,
    E: ParserError<&'input [&'inner str]> + ParserError<&'inner str>,
>(
    inner: impl Parser<&'inner str, O, E>,
) -> impl Parser<&'input [&'inner str], O, E> {
    any.and_then(terminated(inner, eof))
}

fn words<'input, 'inner, O, E, W>(inner: W) -> impl Parser<&'input [&'inner str], O, E>
where
    'inner: 'input,
    E: ParserError<&'input [&'inner str]> + ParserError<&'inner str>,
    W: Words<'inner, O, E>,
{
    inner.map_word()
}

trait Words<'inner, O, E> {
    fn map_word<'input>(self) -> impl Parser<&'input [&'inner str], O, E>
    where
        'inner: 'input,
        E: ParserError<&'input [&'inner str]> + ParserError<&'inner str>;
}

macro_rules! words_impl {
    ($(($p:ident, $o:ident)),*; $($a: ident),*) => {
impl<'inner, $($o),*, E, $($p: Parser<&'inner str, $o, E>),*>
    Words<'inner, ($($o),*,), E> for ($($p),*,)
{
    fn map_word<'input>(self) -> impl Parser<&'input [&'inner str], ($($o),*,), E>
    where
        'inner: 'input,
        E: ParserError<&'input [&'inner str]> + ParserError<&'inner str>,
    {
        let ($($a),*,) = self;
        ($(word($a)),*,)
    }
}

    };
    ($N:literal; $($a: ident),*) => {
        impl<'inner, O, E, P: Parser<&'inner str, O, E>> Words<'inner, [O; $N], E> for [P; $N] {
            fn map_word<'input>(self) -> impl Parser<&'input [&'inner str], [O; $N], E>
            where
                'inner: 'input,
                E: ParserError<&'input [&'inner str]> + ParserError<&'inner str>,
            {
                let ($($a),*,) = self.map(word).into();
                ($($a),*,).map(<[O; $N]>::from)
            }
        }
    };
}

words_impl!(2; a,b);
words_impl!(3; a, b, c);
words_impl!(4; a, b, c, d);
words_impl!(5; a, b, c, d, e);

words_impl!((P0, O0), (P1, O1); a, b);
words_impl!((P0, O0), (P1, O1), (P2, O2); a, b, c);
words_impl!((P0, O0), (P1, O1), (P2, O2), (P3, O3); a, b, c, d);

#[cfg(test)]
mod tests;
