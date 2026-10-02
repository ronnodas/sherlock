use std::iter::once;

use anyhow::anyhow;
use itertools::Itertools as _;
use winnow::ascii::dec_uint;
use winnow::combinator::{
    alt, delimited, dispatch, empty, eof, fail, opt, preceded, separated_pair, terminated,
};
use winnow::error::{ParserError, StrContext};
use winnow::token::{any, rest};
use winnow::{Parser, Result};

use crate::models::{Column, Direction, Judgment, Profession, Row};
use crate::solver::hint::parsers::phrases::{MoreOrLess, Quantifier};
use crate::solver::hint::recipes::{ColumnRecipe, LineRecipe, NameRecipe, RowRecipe};
use crate::solver::hint::{Cardinal, LineKind, Number, Parity};

mod phrases;

pub(crate) use phrases::{Sentence, Series, Unit, UnitInSeries};

impl Sentence {
    pub(crate) fn parse(hint: &str) -> anyhow::Result<Self> {
        let words = hint
            .split_ascii_whitespace()
            .filter(|word| !word.is_empty())
            .collect_vec();
        Self::parse_cased(&words)
            .or_else(move |e| {
                if let Some(&word) = words.first() {
                    let mut word = word.to_owned();
                    if let Some(first_char) = word.get_mut(..1) {
                        first_char.make_ascii_lowercase();
                        let words = once(&*word).chain(words.into_iter().skip(1)).collect_vec();
                        return Self::parse_cased(&words);
                    }
                }
                Err(e)
            })
            .map_err(|_err| anyhow!("\"{hint}\""))
    }

    fn parse_cased(hint: &[&str]) -> anyhow::Result<Self> {
        Self::any.parse(hint).map_err(|e| anyhow!("{e:?}"))
    }

    fn any(input: &mut &[&str]) -> Result<Self> {
        alt((
            alt((
                terminated(Self::traits_are_neighbors_in_unit, eof),
                terminated(Self::has_most_traits, eof),
                terminated(Self::is_one_of_n_traits_in_unit, eof),
                terminated(Self::more_traits_in_unit_than_unit, eof),
                terminated(Self::units_share_n_traits, eof),
                terminated(Self::each_unit_in_series_has_n_traits, eof),
                terminated(Self::unit_shares_quantified_traits_with_unit, eof),
                terminated(Self::unit_size, eof),
                terminated(
                    Self::only_one_person_in_unit_has_cardinal_trait_neighbors,
                    eof,
                ),
            )),
            alt((
                terminated(Self::n_people_in_unit_have_cardinal_trait_neighbors, eof),
                terminated(Self::only_one_unit_in_series_has_exactly_n_traits, eof),
                terminated(Self::only_given_unit_has_exactly_n_traits, eof),
                terminated(Self::equal_number_of_traits_in_units, eof),
                terminated(Self::more_traits_in_unit, eof),
                terminated(Self::equal_traits_in_unit, eof),
                terminated(Self::has_trait, eof),
                terminated(Self::at_most_n_traits_in_neighbors_in_unit, eof),
                terminated(Self::total_number_of_traits_in_units, eof),
            )),
        ))
        .parse_next(input)
    }

    fn traits_are_neighbors_in_unit(input: &mut &[&str]) -> Result<Self> {
        terminated(
            (
                alt((word("All").value(None), quantifier.map(Some))),
                judged_unit,
            ),
            words(("are", "connected")),
        )
        .verify_map(|(quantity, (judgment, unit))| {
            let unit = unit.with_judgment(judgment);
            let unit = match quantity {
                Some(quantity) => unit.quantify(quantity.exact()?),
                None => unit,
            };
            Some(Self::UnitIsConnected(unit))
        })
        .parse_next(input)
    }

    fn has_most_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                separated_pair(line, words(("has", "more")), word(judgment_plural)),
                words(("than", "any", "other")),
                word(line_kind),
            )
            .verify(|&((line, _), kind)| line.kind() == kind)
            .map(|((line, judgment), _)| Self::BiggestInSeries(line.into(), judgment)),
            delimited(
                words(("There", "are", "more")),
                separated_pair(
                    word(judgment_plural),
                    word("among"),
                    word(profession_plural),
                ),
                words(("than", "any", "other", "profession")),
            )
            .map(|(judgment, profession)| {
                Self::BiggestInSeries(UnitInSeries::Profession(profession), judgment)
            }),
            separated_pair(
                name_has,
                words(("the", "most")),
                terminated(word(judgment_singular), word("neighbors")),
            )
            .map(|(name, judgment)| Self::BiggestInSeries(UnitInSeries::Neighbor(name), judgment)),
        ))
        .parse_next(input)
    }

    fn is_one_of_n_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                alt((
                    terminated(word(name_subject), word(be_verb_singular)),
                    word("I'm").value(NameRecipe::Me),
                )),
                words(("one", "of")),
                cardinal_judged_unit,
            )
            .map(|(name, (count, judgment, unit))| {
                Self::IsOneOfNInUnit(unit, name, count, judgment)
            }),
            separated_pair(
                word(name_subject),
                words(("is", "the", "only")),
                judged_unit,
            )
            .map(|(name, (judgment, unit))| {
                Self::IsOneOfNInUnit(unit, name, Cardinal::Exact(1), judgment)
            }),
        ))
        .parse_next(input)
    }

    fn more_traits_in_unit_than_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            (
                preceded(words(("There", "are")), word(more_or_less)),
                separated_pair(judged_unit, word("than"), maybe_judged_unit),
            )
                .map(|(cmp, ((judgment, big), (judgment_small, small)))| {
                    let left = big.with_judgment(judgment);
                    let right = small.with_judgment(judgment_small.unwrap_or(judgment));
                    let units = cmp.big_small([left, right]);
                    (units, None)
                }),
            (
                name_has,
                opt(word(number)),
                word(more_or_less),
                terminated(word(judgment_singular), words((neighbor_any, "than"))),
                word(name_object),
            )
                .map(|(left, excess, cmp, judgment, right)| {
                    let units = cmp
                        .big_small([left, right])
                        .map(|name| Unit::Neighbor(name).with_judgment(judgment));
                    (units, excess)
                }),
            (
                name_has,
                word(more_or_less),
                word(judgment_plural),
                direction,
                delimited(word("than"), direction, word(pronoun_object)),
            )
                .map(|(name, cmp, judgment, left, right)| {
                    let units = cmp.big_small([left, right]).map(|direction| {
                        Unit::Direction(direction, name.clone()).with_judgment(judgment)
                    });
                    (units, None)
                }),
        ))
        .map(|([big, small], excess)| Self::UnitBiggerThanUnit { big, small, excess })
        .parse_next(input)
    }

    fn unit_size(input: &mut &[&str]) -> Result<Self> {
        alt((
            preceded(there_is, cardinal_judged_unit),
            (name_has, cardinal, judged_neighbors)
                .map(|(name, quantity, judgment)| (quantity, judgment, Unit::Neighbor(name))),
            separated_pair(
                quantified_unit,
                word(has_have),
                (
                    a_judgment,
                    terminated(directly_direction, word(alt(("them", "us")))),
                ),
            )
            .map(|((quantifier, unit), (judgment, direction))| {
                let (cardinal, unit) = match quantifier {
                    Quantifier::Simple(cardinal) => (cardinal, unit),
                    Quantifier::Subset(count, total) => (count, unit.quantify(total)),
                };
                let unit = unit.shift(direction);
                (cardinal, judgment, unit)
            }),
            (quantified_unit, is_judgment_any).map(
                |((quantifier, unit), judgment)| match quantifier {
                    Quantifier::Simple(cardinal) => (cardinal, judgment, unit),
                    Quantifier::Subset(count, total) => (count, judgment, unit.quantify(total)),
                },
            ),
            alt((
                separated_pair(
                    preceded(word("Everyone"), unit),
                    word("is"),
                    word(judgment_singular),
                ),
                delimited(
                    word("Every"),
                    separated_pair(
                        word(profession_singular),
                        word("has"),
                        (a_judgment, directly_direction),
                    ),
                    word("them"),
                )
                .map(|(profession, (judgment, direction))| {
                    (Unit::Profession(profession).shift(direction), judgment)
                }),
            ))
            .map(|(unit, judgment)| (Cardinal::Exact(0), !judgment, unit)),
        ))
        .map(|(count, judgment, unit)| Self::UnitSize(unit.with_judgment(judgment), count))
        .parse_next(input)
    }

    fn only_one_person_in_unit_has_cardinal_trait_neighbors(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                alt((
                    preceded((words(("Only", "one")), opt(word("person"))), unit),
                    quantified_profession.verify_map(|(quantifier, profession)| {
                        if let Quantifier::Subset(Cardinal::Exact(1), total) = quantifier {
                            Some(Unit::Profession(profession).quantify(total))
                        } else {
                            None
                        }
                    }),
                )),
                word("has"),
                (cardinal, judged_neighbors),
            )
            .map(|(unit, (count, judgment))| {
                Self::UniqueInUnitHasNNeighbors(unit, count, None, judgment)
            }),
            separated_pair(
                word(name_subject),
                words((be_verb_singular, "the", "only", alt(("one", "person")))),
                separated_pair(unit, word("with"), (cardinal, judged_neighbors)),
            )
            .map(|(name, (unit, (quantity, judgment)))| {
                Self::UniqueInUnitHasNNeighbors(unit, quantity, Some(name), judgment)
            }),
        ))
        .parse_next(input)
    }

    fn n_people_in_unit_have_cardinal_trait_neighbors(input: &mut &[&str]) -> Result<Self> {
        separated_pair(
            quantified_profession,
            word(has_have),
            (cardinal, judged_neighbors),
        )
        .map(|((count, profession), (neighbors, judgment))| {
            let unit = Unit::Profession(profession);
            let (quantity, unit) = match count {
                Quantifier::Simple(cardinal) => (cardinal, unit),
                Quantifier::Subset(count, total) => (count, unit.quantify(total)),
            };
            Self::NInUnitHaveNNeighbors {
                unit,
                quantity,
                neighbors,
                judgment,
            }
        })
        .parse_next(input)
    }

    fn only_one_unit_in_series_has_exactly_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                preceded(words(("Only", "one")), word(line_kind)),
                word("has"),
                cardinal_judgment,
            )
            .map(|(kind, (count, judgment))| (kind.into(), count, judgment)),
            delimited(
                words(("Only", "one", "person", "has")),
                cardinal_judgment,
                word(neighbor_any),
            )
            .map(|(quantity, judgment)| (Series::Neighbor, quantity, judgment)),
            preceded(
                words(("There", "is", "only", "one", "profession", "with")),
                cardinal_judgment,
            )
            .map(|(quantity, judgment)| (Series::Profession, quantity, judgment)),
        ))
        .map(|(series, count, judgment)| Self::UniqueUnitInSeriesHasSize(series, count, judgment))
        .parse_next(input)
    }

    fn only_given_unit_has_exactly_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                line,
                words(("is", "the", "only")),
                separated_pair(word(line_kind), word("with"), cardinal_judgment),
            )
            .verify(|&(line, (kind, _))| line.kind() == kind)
            .context(StrContext::Label("a matching row/column"))
            .map(|(line, (_, (quantity, judgment)))| (line.into(), quantity, judgment)),
            separated_pair(
                word(name_subject),
                words((be_verb_singular, "the", "only", "one", "with")),
                (cardinal, judged_neighbors),
            )
            .map(|(name, (quantity, judgment))| (UnitInSeries::Neighbor(name), quantity, judgment)),
            separated_pair(
                word(profession_singular),
                words(("is", "the", "only", "profession", "with")),
                cardinal_judgment,
            )
            .map(|(profession, (quantity, judgment))| {
                (UnitInSeries::Profession(profession), quantity, judgment)
            }),
        ))
        .map(|(unit, count, judgment)| Self::OnlyGivenUnitHasNTraits(unit, count, judgment))
        .parse_next(input)
    }

    fn unit_shares_quantified_traits_with_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            terminated(
                (
                    preceded(opt(there_is), quantified_judged_unit),
                    alt((
                        preceded(neighboring_verb, word(name_object)).map(Unit::Neighbor),
                        preceded(opt(word(be_verb_third_person)), unit),
                    )),
                ),
                eof,
            )
            .map(|((quantifier, judgment, unit), other)| (quantifier, unit, other, judgment)),
            terminated(
                separated_pair(
                    separated_pair(word(name_possessive), word("only"), word(judgment_singular)),
                    words(("neighbor", "is")),
                    alt((
                        terminated(direction, word(pronoun_object)).map(Err),
                        unit.map(Ok),
                    )),
                ),
                eof,
            )
            .map(|((name, judgment), other)| {
                let other =
                    other.unwrap_or_else(|direction| Unit::Direction(direction, name.clone()));
                let quantifier = Quantifier::Subset(Cardinal::Exact(1), 1);
                (quantifier, Unit::Neighbor(name), other, judgment)
            }),
            terminated(
                separated_pair(
                    word(name_subject),
                    word("shares"),
                    separated_pair(
                        (quantifier, judged_neighbors),
                        word("with"),
                        word(name_object),
                    ),
                ),
                eof,
            )
            .map(|(quantified, ((quantifier, judgment), other))| {
                (
                    quantifier,
                    Unit::Neighbor(quantified),
                    Unit::Neighbor(other),
                    judgment,
                )
            }),
            terminated(
                separated_pair(
                    quantified_judged_unit,
                    word(neighbor_any),
                    word(name_object),
                ),
                eof,
            )
            .map(|((quantifier, judgment, unit), name)| {
                (quantifier, unit, Unit::Neighbor(name), judgment)
            }),
            terminated(
                separated_pair(quantified_judged_unit, not_neighbor_any, word(name_object)),
                eof,
            )
            .map(|((quantifier, judgment, unit), name)| {
                (quantifier, unit, Unit::NotNeighbor(name), judgment)
            }),
        ))
        .map(
            |(quantifier, quantified, other, judgment)| match quantifier {
                quantifier @ Quantifier::Simple(_) => {
                    Self::IntersectionSize([quantified, other], quantifier, judgment)
                }
                Quantifier::Subset(intersection, total) => Self::UnitAndIntersectionSize {
                    total,
                    quantified,
                    other,
                    intersection,
                    judgment,
                },
            },
        )
        .parse_next(input)
    }

    fn units_share_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            terminated(
                separated_pair(pair(name_subject, "and"), word("have"), cardinal_judgment),
                words((neighbor_any, "in", "common")),
            )
            .map(|(names, (count, judgment))| {
                (
                    names.map(Unit::Neighbor),
                    judgment,
                    Quantifier::Simple(count),
                )
            }),
            separated_pair(
                (
                    quantifier,
                    separated_pair(word(name_possessive), word("neighbors"), unit),
                ),
                word(be_verb_third_person),
                word(judgment_singular),
            )
            .map(|((quantity, (name, unit)), judgment)| {
                ([Unit::Neighbor(name), unit], judgment, quantity)
            }),
            (
                name_has,
                separated_pair(cardinal_judgment, word(neighbor_any), unit),
            )
                .map(|(name, ((quantity, judgment), unit))| {
                    (
                        [Unit::Neighbor(name), unit],
                        judgment,
                        Quantifier::Simple(quantity),
                    )
                }),
            separated_pair(
                pair(name_subject, "and"),
                word("share"),
                (cardinal, judged_neighbors),
            )
            .map(|(names, (quantity, judgment))| {
                (
                    names.map(Unit::Neighbor),
                    judgment,
                    Quantifier::Simple(quantity),
                )
            }),
            (
                separated_pair(quantified_unit, neighboring_verb, word(name_object)),
                is_judgment_any,
            )
                .map(|(((quantifier, a), b), judgment)| {
                    ([a, Unit::Neighbor(b)], judgment, quantifier)
                }),
        ))
        .map(|(units, judgment, cardinal)| Self::IntersectionSize(units, cardinal, judgment))
        .parse_next(input)
    }

    fn equal_number_of_traits_in_units(input: &mut &[&str]) -> Result<Self> {
        alt((
            preceded(
                words(("There's", "an", "equal", "number", "of")),
                (word(judgment_plural), unit_pair),
            ),
            preceded(
                words(("There", "are", "as", "many")),
                separated_pair(
                    judged_unit,
                    (word("as"), opt(words(("there", "are")))),
                    judged_unit,
                ),
            )
            .verify_map(|((judgment_a, a), (judgment_b, b))| {
                (judgment_a == judgment_b).then_some((judgment_a, [a, b]))
            }),
            separated_pair(
                pair(name_subject, "and"),
                words(("have", "an", "equal", "number", "of")),
                terminated(word(judgment_singular), word("neighbors")),
            )
            .map(|(names, judgment)| (judgment, names.map(Unit::Neighbor))),
        ))
        .map(|(judgment, pair)| Self::EqualNumberOfTraitsInUnits(pair, judgment))
        .parse_next(input)
    }

    fn each_unit_in_series_has_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(
                preceded(word("Each"), word(series)),
                word("has"),
                cardinal_judgment,
            )
            .map(|(series, (quantity, judgment))| (series, quantity, judgment)),
            separated_pair(
                preceded(words(("There", be_verb_third_person)), cardinal_judgment),
                words((alt(("in", "among")), "each")),
                word(series),
            )
            .map(|((quantity, judgment), series)| (series, quantity, judgment)),
            preceded(words(("Everyone", "has")), (cardinal, judged_neighbors))
                .map(|(quantity, judgment)| (Series::Neighbor, quantity, judgment)),
        ))
        .map(|(series, quantity, judgment)| {
            Self::EachUnitInSeriesHasSize(series, quantity, judgment)
        })
        .parse_next(input)
    }

    fn more_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            preceded(
                words(("There", "are")),
                (
                    word(more_or_less),
                    alt((
                        pair(judgment_plural, "than"),
                        pair(judgment_singular, "than"),
                    )),
                    unit,
                ),
            ),
            (
                name_has,
                word(more_or_less),
                word(judgment_singular),
                delimited(word("than"), word(judgment_singular), word("neighbors")),
            )
                .map(|(name, cmp, left, right)| (cmp, [left, right], Unit::Neighbor(name))),
        ))
        .verify(|&(_, [more, less], _)| more == !less)
        .map(|(cmp, judgments, unit)| Self::MoreTraitsInUnit(unit, cmp.big(judgments)))
        .parse_next(input)
    }

    fn equal_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            preceded(
                words(("There", "are", "as", "many")),
                (pair(judgment_plural, "as"), unit),
            ),
            preceded(
                words(("There's", "an", "equal", "number", "of")),
                alt((
                    (
                        pair(judgment_adjective, "and"),
                        word(profession_plural).map(Unit::Profession),
                    ),
                    (pair(judgment_plural, "and"), unit),
                )),
            ),
            (
                name_has,
                delimited(
                    words(("an", "equal", "number", "of")),
                    pair(judgment_adjective, "and"),
                    word("neighbors"),
                ),
            )
                .map(|(name, judgments)| (judgments, Unit::Neighbor(name))),
        ))
        .verify(|&([a, b], _)| a == !b)
        .map(|(_, unit)| Self::UnitEquallySplit(unit))
        .parse_next(input)
    }

    fn has_trait(input: &mut &[&str]) -> Result<Self> {
        separated_pair(word(raw_name), word("is"), judgment_predicate_singular)
            .map(|(name, judgment)| Self::HasTrait(NameRecipe::Explicit(name.into()), judgment))
            .parse_next(input)
    }

    fn at_most_n_traits_in_neighbors_in_unit(input: &mut &[&str]) -> Result<Self> {
        separated_pair(
            preceded(words(("No", "one")), unit),
            words(("has", "more", "than")),
            (
                word(number),
                terminated(word(judgment_singular), word(neighbor_any)),
            ),
        )
        .map(|(unit, (number, judgment))| {
            Self::EachInUnitHasAtMostNNeighbors(unit, number, judgment)
        })
        .parse_next(input)
    }

    fn total_number_of_traits_in_units(input: &mut &[&str]) -> Result<Self> {
        alt((
            preceded(
                words(("There", "are", "a", "total", "of")),
                (
                    cardinal_judgment,
                    alt((
                        preceded(word("in"), line_pair.map(|lines| lines.map(Unit::Line))),
                        pair(profession_plural, "and").map(|profs| profs.map(Unit::Profession)),
                    )),
                ),
            ),
            terminated(
                separated_pair(
                    pair(name_subject, "and").map(|names| names.map(Unit::Neighbor)),
                    word("have"),
                    (cardinal, judged_neighbors),
                ),
                words(("in", "total")),
            )
            .map(|(units, cardinal_judgment)| (cardinal_judgment, units)),
        ))
        .map(|((quantity, judgment), units)| Self::TotalUnitsSize(units, quantity, judgment))
        .parse_next(input)
    }
}

fn unit_pair(input: &mut &[&str]) -> Result<[Unit; 2]> {
    alt((
        preceded(word("in"), line_pair).map(|lines| lines.map(Unit::Line)),
        separated_pair(unit, word("and"), unit).map(<[Unit; 2]>::from),
    ))
    .parse_next(input)
}

fn unit(input: &mut &[&str]) -> Result<Unit> {
    preceded(
        alt((word("person").void(), word("persons").void(), empty)),
        alt((
            words(("in", "total")).value(Unit::All),
            words(("on", "the", "edges")).value(Unit::Edges),
            (
                word("in"),
                alt((words(("a", "corner")), words(("the", "corners")))),
            )
                .value(Unit::Corners),
            (alt((between, preceded(opt(word("in")), line.map(Unit::Line))))),
            (direction, word(name_object))
                .map(|(direction, name)| Unit::Direction(direction, name)),
            separated_pair(
                directly_direction,
                word(indefinite_article),
                word(profession_singular),
            )
            .map(|(direction, profession)| Unit::Profession(profession).shift(direction)),
            alt((
                preceded(neighboring_verb, word(name_object)),
                delimited(
                    opt(words(("one", "of"))),
                    word(name_possessive),
                    word(neighbor_any),
                ),
            ))
            .map(Unit::Neighbor),
            preceded(word("not"), word(name_object)).map(Unit::NotName),
            profession_any.map(Unit::Profession),
        )),
    )
    .parse_next(input)
}

fn maybe_judged_unit(input: &mut &[&str]) -> Result<(Option<Judgment>, Unit)> {
    preceded(opt(word(determiner)), (opt(word(judgment_any)), unit)).parse_next(input)
}

fn judged_unit(input: &mut &[&str]) -> Result<(Judgment, Unit)> {
    alt((
        preceded(opt(word("us")), (word(judgment_any), unit)),
        (
            word(name_possessive),
            terminated(word(judgment_any), word(neighbor_any)),
        )
            .map(|(name, judgment)| (judgment, Unit::Neighbor(name))),
    ))
    .parse_next(input)
}

fn quantified_judged_unit(input: &mut &[&str]) -> Result<(Quantifier, Judgment, Unit)> {
    alt((
        quantified_possessive_judged_neighbors
            .map(|(name, quantity, judgment)| (quantity, judgment, Unit::Neighbor(name))),
        (
            quantifier,
            preceded(opt((word("of"), opt(word(determiner)))), word(judgment_any)),
            unit,
        ),
    ))
    .parse_next(input)
}

fn cardinal_judged_unit(input: &mut &[&str]) -> Result<(Cardinal, Judgment, Unit)> {
    alt((
        (cardinal, word(judgment_any), unit),
        (word(name_possessive), cardinal, judged_neighbors)
            .map(|(name, quantity, judgment)| (quantity, judgment, Unit::Neighbor(name))),
    ))
    .parse_next(input)
}

// TODO include the optional determiners at the end of `quantifier` and `cardinal` and remove ones
// made redundant
fn quantifier(input: &mut &[&str]) -> Result<Quantifier> {
    alt((
        word("both").value(Quantifier::Subset(Cardinal::Exact(2), 2)),
        (
            word("neither"),
            opt((
                word("of"),
                opt(words((alt((determiner, pronoun_possessive)), "2"))),
            )),
        )
            .value(Quantifier::Subset(Cardinal::Exact(0), 2)),
        separated_pair(
            cardinal,
            (
                opt(words(("out", "of"))),
                alt((
                    word(determiner).void(),
                    word(pronoun_possessive).void(),
                    empty,
                )),
            ),
            word(number),
        )
        .map(|(a, b)| Quantifier::Subset(a, b)),
        terminated(cardinal, opt(word(determiner))).map(Quantifier::Simple),
        words(("the", "only")).value(Quantifier::Subset(Cardinal::Exact(1), 1)),
    ))
    .parse_next(input)
}

fn cardinal(input: &mut &[&str]) -> Result<Cardinal> {
    alt((
        word("no").value(Cardinal::Exact(0)),
        terminated(word(number), words(("or", "more"))).map(Cardinal::AtLeast),
        terminated(number_phrase, opt(word("of"))).map(Cardinal::Exact),
        preceded(words(("at", "least")), word(number)).map(Cardinal::AtLeast),
        delimited(word("an"), parity, words(("number", "of"))).map(Cardinal::Parity),
    ))
    .parse_next(input)
}

fn number_phrase(input: &mut &[&str]) -> Result<Number> {
    preceded(opt(word(alt(("exactly", "only")))), word(number)).parse_next(input)
}

fn number(input: &mut &str) -> Result<Number> {
    alt((dec_uint, "none".value(0), "one".value(1), "two".value(2))).parse_next(input)
}

fn parity(input: &mut &[&str]) -> Result<Parity> {
    alt((
        word("even").value(Parity::Even),
        word("odd").value(Parity::Odd),
    ))
    .parse_next(input)
}

fn cardinal_judgment(input: &mut &[&str]) -> Result<(Cardinal, Judgment)> {
    (cardinal, word(judgment_any)).parse_next(input)
}

fn judged_neighbors(input: &mut &[&str]) -> Result<Judgment> {
    terminated(word(judgment_adjective), word(neighbor_any)).parse_next(input)
}

fn quantified_possessive_judged_neighbors(
    input: &mut &[&str],
) -> Result<(NameRecipe, Quantifier, Judgment)> {
    (
        cardinal,
        terminated(
            (word(name_possessive), opt(word(number)), word(judgment_any)),
            word(neighbor_any),
        ),
    )
        .map(|(number, (name, total, judgment))| {
            let quantifier = total.map_or(Quantifier::Simple(number), |total| {
                Quantifier::Subset(number, total)
            });
            (name, quantifier, judgment)
        })
        .parse_next(input)
}

fn a_judgment(input: &mut &[&str]) -> Result<Judgment> {
    alt((
        words(("an", "innocent")).value(Judgment::Innocent),
        words(("a", "criminal")).value(Judgment::Criminal),
    ))
    .parse_next(input)
}

fn is_judgment_any(input: &mut &[&str]) -> Result<Judgment> {
    alt((
        preceded(word("is"), judgment_predicate_singular),
        preceded(word("are"), word(judgment_adjective)),
    ))
    .parse_next(input)
}

fn judgment_predicate_singular(input: &mut &[&str]) -> Result<Judgment> {
    alt((
        word("innocent").value(Judgment::Innocent),
        words(("a", "criminal")).value(Judgment::Criminal),
    ))
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

use judgment_adjective as judgment_singular;

fn judgment_adjective(input: &mut &str) -> Result<Judgment> {
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
            .map(|name| NameRecipe::Explicit(name.into())),
    ))
    .parse_next(input)
}

fn name_has(input: &mut &[&str]) -> Result<NameRecipe> {
    alt((
        words(("I", "have")).value(NameRecipe::Me),
        terminated(word(raw_name), word("has")).map(|name| NameRecipe::Explicit(name.into())),
    ))
    .parse_next(input)
}

fn name_subject(input: &mut &str) -> Result<NameRecipe> {
    raw_name
        .map(|name| {
            if name == "I" {
                NameRecipe::Me
            } else {
                NameRecipe::Explicit(name.into())
            }
        })
        .parse_next(input)
}

fn name_object(input: &mut &str) -> Result<NameRecipe> {
    alt((
        raw_name.map(|name| {
            if name == "Me" {
                NameRecipe::Me
            } else {
                NameRecipe::Explicit(name.into())
            }
        }),
        "me".value(NameRecipe::Me),
    ))
    .parse_next(input)
}

fn raw_name<'input>(input: &mut &'input str) -> Result<&'input str> {
    rest.verify(|s: &str| s.chars().next().is_some_and(char::is_uppercase))
        .parse_next(input)
}

fn pronoun_object<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("him", "her", "them", "me")).parse_next(input)
}

fn quantified_profession(input: &mut &[&str]) -> Result<(Quantifier, Profession)> {
    separated_pair(quantifier, opt(word(determiner)), profession_any).parse_next(input)
}

fn quantified_unit(input: &mut &[&str]) -> Result<(Quantifier, Unit)> {
    alt((
        separated_pair(quantifier, opt(word(determiner)), maybe_judged_unit),
        separated_pair(
            cardinal,
            opt(word(determiner)),
            (number_phrase, maybe_judged_unit),
        )
        .map(|(cardinal, (total, unit))| (Quantifier::Subset(cardinal, total), unit)),
    ))
    .map(|(quantifier, (judgment, unit))| {
        let unit = if let Some(judgment) = judgment {
            unit.with_judgment(judgment)
        } else {
            unit
        };
        (quantifier, unit)
    })
    .parse_next(input)
}

fn direction(input: &mut &[&str]) -> Result<Direction> {
    alt((
        word("above").value(Direction::Above),
        word("below").value(Direction::Below),
        delimited(
            words(("to", "the")),
            alt((
                word("left").value(Direction::Left),
                word("right").value(Direction::Right),
            )),
            opt(word("of")),
        ),
    ))
    .parse_next(input)
}

fn directly_direction(input: &mut &[&str]) -> Result<Direction> {
    preceded(word("directly"), direction).parse_next(input)
}

fn indefinite_article<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("a", "an")).parse_next(input)
}

fn pronoun_possessive<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("her", "his")).parse_next(input)
}

fn determiner<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("the", "us")).parse_next(input)
}

fn profession_any(input: &mut &[&str]) -> Result<Profession> {
    alt((
        word(profession_plural),
        preceded(opt(word(indefinite_article)), word(profession_singular)),
    ))
    .parse_next(input)
}

// TODO combine the verify and map into a single constructor
fn profession_singular(input: &mut &str) -> Result<Profession> {
    rest.verify_map(Profession::from_singular).parse_next(input)
}

fn profession_plural(input: &mut &str) -> Result<Profession> {
    rest.verify_map(Profession::from_plural).parse_next(input)
}

fn neighbor_any(input: &mut &str) -> Result<()> {
    alt(("neighbors", "neighbor")).void().parse_next(input)
}

fn not_neighbor_any(input: &mut &[&str]) -> Result<()> {
    words((alt(("doesn't", "don't")), "neighbor"))
        .void()
        .parse_next(input)
}

fn there_is<'input, 'inner: 'input>(
    input: &mut &'input [&'inner str],
) -> Result<&'input [&'inner str]> {
    alt((
        words(("There", be_verb_third_person)).take(),
        word("There's").take(),
    ))
    .parse_next(input)
}

fn has_have<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("has", "have")).parse_next(input)
}

// TODO split, "who neighbor" and "neighboring" are qualifiers, while "also neighbor" is a verb
// phrase
fn neighboring_verb<'input, 'inner: 'input>(
    input: &mut &'input [&'inner str],
) -> Result<&'input [&'inner str]> {
    alt((
        (opt(word(alt(("who", "also")))), word("neighbor")).take(),
        (opt(word(be_verb_third_person)), word("neighboring")).take(),
    ))
    .parse_next(input)
}

fn be_verb_third_person<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("is", "are")).parse_next(input)
}

fn be_verb_singular<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("is", "am")).parse_next(input)
}

fn between(input: &mut &[&str]) -> Result<Unit> {
    preceded(words(("in", "between")), pair(name_object, "and"))
        .map(Unit::Between)
        .parse_next(input)
}

fn more_or_less(input: &mut &str) -> Result<MoreOrLess> {
    alt((
        "more".value(MoreOrLess::More),
        "less".value(MoreOrLess::Less),
        "fewer".value(MoreOrLess::Less),
    ))
    .parse_next(input)
}

fn series(input: &mut &str) -> Result<Series> {
    alt((
        line_kind.map(Series::from),
        "profession".value(Series::Profession),
    ))
    .parse_next(input)
}

fn line(input: &mut &[&str]) -> Result<LineRecipe> {
    alt((row.map(LineRecipe::Row), column.map(LineRecipe::Column))).parse_next(input)
}

fn line_kind(input: &mut &str) -> Result<LineKind> {
    alt(("row".value(LineKind::Row), "column".value(LineKind::Column))).parse_next(input)
}

fn line_pair(input: &mut &[&str]) -> Result<[LineRecipe; 2]> {
    alt((
        separated_pair(line_prefixed("rows", row_bare), word("and"), word(row_bare)).map(|rows| {
            <[Row; 2]>::from(rows)
                .map(RowRecipe::Explicit)
                .map(LineRecipe::Row)
        }),
        separated_pair(
            line_prefixed("columns", column_bare),
            word("and"),
            word(column_bare),
        )
        .map(|cols| {
            <[Column; 2]>::from(cols)
                .map(ColumnRecipe::Explicit)
                .map(LineRecipe::Column)
        }),
    ))
    .parse_next(input)
}

fn row(input: &mut &[&str]) -> Result<RowRecipe> {
    alt((
        line_prefixed("row", row_bare).map(RowRecipe::Explicit),
        words(("my", "row")).value(RowRecipe::Me),
    ))
    .parse_next(input)
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

fn column(input: &mut &[&str]) -> Result<ColumnRecipe> {
    alt((
        line_prefixed("column", column_bare).map(ColumnRecipe::Explicit),
        words(("my", "column")).value(ColumnRecipe::Me),
    ))
    .parse_next(input)
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
    sep: &'static str,
) -> impl Parser<&'input [&'inner str], [T; 2], E> {
    separated_pair(word(inner), word(sep), word(inner)).map(Into::into)
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
}

words_impl!((P0, O0), (P1, O1); a, b);
words_impl!((P0, O0), (P1, O1), (P2, O2); a, b, c);
words_impl!((P0, O0), (P1, O1), (P2, O2), (P3, O3); a, b, c, d);
words_impl!((P0, O0), (P1, O1), (P2, O2), (P3, O3), (P4, O4); a, b, c, d, e);
words_impl!((P0, O0), (P1, O1), (P2, O2), (P3, O3), (P4, O4), (P5, O5); a, b, c, d, e, f);

#[cfg(test)]
mod tests;
