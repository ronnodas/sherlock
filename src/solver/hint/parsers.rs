use std::iter::once;

use anyhow::anyhow;
use winnow::ascii::dec_uint;
use winnow::combinator::{
    alt, delimited, dispatch, empty, eof, fail, opt, preceded, separated_pair, seq, terminated,
};
use winnow::error::{ParserError, StrContext};
use winnow::token::{any, rest};
use winnow::{Parser, Result};

use crate::models::{Column, Direction, Judgment, Profession, Row};
use crate::solver::hint::parsers::phrases::{AnyAll, MoreOrLess, Quantifier, UnitExpr};
use crate::solver::hint::recipes::{ColumnRecipe, LineRecipe, NameRecipe, RowRecipe};
use crate::solver::hint::{Cardinal, LineKind, Number, Parity};

mod phrases;

pub(crate) use phrases::{Sentence, Series, Unit, UnitInSeries};

impl Sentence {
    pub(crate) fn parse(hint: &str) -> anyhow::Result<Self> {
        let words: Vec<_> = hint
            .split_ascii_whitespace()
            .filter(|word| !word.is_empty())
            .collect();
        Self::parse_cased(&words)
            .or_else(move |e| {
                if let Some(&word) = words.first() {
                    let mut word = word.to_owned();
                    if let Some(first_char) = word.get_mut(..1) {
                        first_char.make_ascii_lowercase();
                        let words: Vec<_> = once(&*word).chain(words.into_iter().skip(1)).collect();
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
            alt((
                terminated(Self::all_traits_in_unit_are_in_unit, eof),
                terminated(Self::is_one_of_traits_in_unit, eof),
            )),
        ))
        .parse_next(input)
    }

    fn traits_are_neighbors_in_unit(input: &mut &[&str]) -> Result<Self> {
        seq!(
            alt((
                word("All").value(Some(Quantifier::Simple(Cardinal::AtLeast(1)))),
                quantifier.map(Some),
                empty.value(None),
            )),
            judged_unit,
            _: words(("are", "connected")),
        )
        .verify_map(|(quantity, (judgment, unit))| {
            let unit = unit.with_judgment(judgment);
            let unit = if let Some(quantity) = quantity {
                unit.quantify(quantity.to_cardinal()?)
            } else {
                unit
            };
            Some(Self::UnitIsConnected(unit))
        })
        .parse_next(input)
    }

    fn has_most_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                line,
                _: words(("has", "more")),
                word(judgment_plural),
                _: words(("than", "any", "other")),
                word(line_kind),
            )
            .verify(|&(line, _, kind)| line.kind() == kind)
            .map(|(line, judgment, _)| (line.into(), judgment)),
            seq!(
                _: words(("There", "are", "more")),
                word(judgment_plural),
                _: word("among"),
                word(profession_plural),
                _: words(("than", "any", "other", "profession")),
            )
            .map(|(judgment, profession)| (UnitInSeries::Profession(profession), judgment)),
            seq!(
                name_has.map(UnitInSeries::Neighbor),
                _: words(("the", "most")),
                word(judgment_singular),
                _: word("neighbors")
            ),
            separated_pair(
                word(profession_singular).map(UnitInSeries::Profession),
                words(("is", "the", "profession", "with", "the", "most")),
                word(judgment_plural),
            ),
        ))
        .map(|(unit, judgment)| Self::BiggestInSeries(unit, judgment))
        .parse_next(input)
    }

    fn is_one_of_n_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            separated_pair(name_is, words(("one", "of")), cardinal_judged_unit).map(
                |(name, (count, judgment, unit))| Self::IsOneOfNInUnit(unit, name, count, judgment),
            ),
            separated_pair(name_is, words(("the", "only")), judged_unit).map(
                |(name, (judgment, unit))| {
                    Self::IsOneOfNInUnit(unit, name, Cardinal::Exact(1), judgment)
                },
            ),
        ))
        .parse_next(input)
    }

    fn is_one_of_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        separated_pair(name_is, words(("one", "of", determiner)), judged_unit)
            .map(|(name, (judgment, unit))| Self::IsInUnit(unit, name, judgment))
            .parse_next(input)
    }

    fn more_traits_in_unit_than_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: words(("There", "are")),
                word(more_or_less),
                judged_unit,
                _: word("than"),
                maybe_judged_unit
            )
            .map(|(cmp, (judgment, left), (judgment_right, right))| {
                let left = left.with_judgment(judgment);
                let right = right.with_judgment(judgment_right.unwrap_or(judgment));
                (cmp, [left, right], None)
            }),
            seq!(
                name_has,
                opt(word(number)),
                word(more_or_less),
                word(judgment_adjective),
                _: words((neighbor_any, "than")),
                word(name_object),
            )
            .map(|(left, excess, cmp, judgment, right)| {
                let units = [left, right].map(|name| Unit::Neighbor(name).with_judgment(judgment));
                (cmp, units, excess)
            }),
            seq!(
                name_has,
                word(more_or_less),
                word(judgment_plural),
                direction,
                _: word("than"),
                direction,
                _: word(pronoun_object_singular),
            )
            .map(|(name, cmp, judgment, left, right)| {
                let units = [left, right].map(|direction| {
                    Unit::Direction(direction, name.clone()).with_judgment(judgment)
                });
                (cmp, units, None)
            }),
        ))
        .map(|(cmp, units, excess)| {
            let [big, small] = cmp.big_small(units);
            Self::UnitBiggerThanUnit { big, small, excess }
        })
        .parse_next(input)
    }

    fn unit_size(input: &mut &[&str]) -> Result<Self> {
        alt((
            preceded(there_is, cardinal_judged_unit)
                .map(|(cardinal, judgment, unit)| (cardinal, judgment, unit.into())),
            seq!(
                name_has,
                cardinal,
                word(judgment_adjective),
                _: word(neighbor_any)
            )
            .map(|(name, cardinal, judgment)| (cardinal, judgment, Unit::Neighbor(name).into())),
            seq!(
                quantified_unit_expr,
                _: word(has_have),
                a_judgment,
                directly_direction,
                _: word(pronoun_object_plural)
            )
            .map(|((quantifier, unit), judgment, direction)| {
                let (cardinal, unit) = match quantifier {
                    Quantifier::Simple(cardinal) => (cardinal, unit),
                    Quantifier::Subset(count, total) => (count, unit.quantify(total)),
                };
                (cardinal, judgment, unit.shift(direction))
            }),
            (quantified_unit, is_judgment_any).map(
                |((quantifier, unit), judgment)| match quantifier {
                    Quantifier::Simple(cardinal) => (cardinal, judgment, unit.into()),
                    Quantifier::Subset(count, total) => {
                        (count, judgment, unit.quantify(total).into())
                    }
                },
            ),
            alt((
                seq!(
                    _: word("Everyone"),
                    unit,
                    _: word("is"),
                    word(judgment_singular),
                ),
                seq!(
                    _: word("Every"),
                    word(profession_singular),
                    _: word("has"),
                    a_judgment,
                    directly_direction,
                    _: word("them"),
                )
                .map(|(profession, judgment, direction)| {
                    (Unit::Profession(profession).shift(direction), judgment)
                }),
            ))
            .map(|(unit, judgment)| (Cardinal::Exact(0), !judgment, unit.into())),
            seq!(
                _: words(("Not", "everyone")),
                unit,
                _: word("is"),
                judgment_predicate_singular,
            )
            .map(|(unit, judgment)| (Cardinal::AtLeast(1), !judgment, unit.into())),
        ))
        .map(|(cardinal, judgment, unit)| Self::UnitSize(unit.with_judgment(judgment), cardinal))
        .parse_next(input)
    }

    fn only_one_person_in_unit_has_cardinal_trait_neighbors(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                alt((
                    preceded(words(("Only", "one")), unit),
                    quantified_profession.verify_map(|(quantifier, profession)| {
                        if let Quantifier::Subset(Cardinal::Exact(1), total) = quantifier {
                            Some(Unit::Profession(profession).quantify(total))
                        } else {
                            None
                        }
                    }),
                )),
                _: word("has"),
                cardinal,
                word(judgment_adjective),
                _: word(neighbor_any)
            )
            .map(|(unit, count, judgment)| (None, unit, count, judgment)),
            seq!(
                name_is.map(Some),
                _: words(("the", "only", alt(("one", "person")))),
                unit,
                _: word("with"),
                cardinal,
                word(judgment_adjective),
                _: word(neighbor_any)
            ),
        ))
        .map(|(name, unit, quantity, judgment)| {
            Self::UniqueInUnitHasNNeighbors(unit, quantity, name, judgment)
        })
        .parse_next(input)
    }

    fn n_people_in_unit_have_cardinal_trait_neighbors(input: &mut &[&str]) -> Result<Self> {
        seq!(
            quantified_profession,
            _: word(has_have),
            cardinal,
            word(judgment_adjective),
            _: word(neighbor_any),
        )
        .map(|((count, profession), neighbors, judgment)| {
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
            seq!(
                _: words(("Only", "one")),
                word(line_kind).map(Series::Line),
                _: word("has"),
                cardinal,
                word(judgment_any)
            ),
            seq!(
                _: words(("Only", "one", "person", "has")),
                cardinal,
                word(judgment_any),
                _: word(neighbor_any),
            )
            .map(|(quantity, judgment)| (Series::Neighbor, quantity, judgment)),
            seq!(
                _: words(("There", "is", "only", "one", "profession", "with")),
                cardinal,
                word(judgment_any),
            )
            .map(|(quantity, judgment)| (Series::Profession, quantity, judgment)),
        ))
        .map(|(series, count, judgment)| Self::UniqueUnitInSeriesHasSize(series, count, judgment))
        .parse_next(input)
    }

    fn only_given_unit_has_exactly_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                separated_pair(line, words(("is", "the", "only")), word(line_kind))
                    .verify(|&(line, kind)| line.kind() == kind)
                    .context(StrContext::Label("a matching row/column"))
                    .map(|(line, _)| UnitInSeries::Line(line)),
                _: word("with"),
                cardinal,
                word(judgment_any),
            ),
            seq!(
                name_is.map(UnitInSeries::Neighbor),
                _: words(("the", "only", "one", "with")),
                cardinal,
                word(judgment_adjective),
                _: word(neighbor_any),
            ),
            seq!(
                word(profession_singular).map(UnitInSeries::Profession),
                _: words(("is", "the", "only", "profession", "with")),
                cardinal,
                word(judgment_any),
            ),
        ))
        .map(|(unit, count, judgment)| Self::OnlyGivenUnitHasNTraits(unit, count, judgment))
        .parse_next(input)
    }

    fn unit_shares_quantified_traits_with_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: there_is,
                cardinal_judged_unit,
                alt((
                    preceded(neighboring_qualifier, word(name_object)).map(Unit::Neighbor),
                    unit,
                )),
                _: eof,
            )
            .map(|((quantifier, judgment, unit), other)| {
                (quantifier.into(), unit, other, judgment)
            }),
            seq!(
                quantified_judged_unit,
                alt((
                    preceded(neighboring_verb, word(name_object)).map(Unit::Neighbor),
                    preceded(word(be_verb_third_person), unit),
                )),
                _: eof,
            )
            .map(|((quantifier, judgment, unit), other)| (quantifier, unit, other, judgment)),
            seq!(
                word(name_possessive),
                _: word("only"),
                word(judgment_singular),
                _: words(("neighbor", "is")),
                alt((
                    terminated(direction, word(pronoun_object_singular)).map(Err),
                    unit.map(Ok),
                )),
                _: eof,
            )
            .map(|(name, judgment, other)| {
                let other =
                    other.unwrap_or_else(|direction| Unit::Direction(direction, name.clone()));
                let quantifier = Quantifier::Subset(Cardinal::Exact(1), 1);
                (quantifier, Unit::Neighbor(name), other, judgment)
            }),
            seq!(
                word(name_subject),
                _: word("shares"),
                quantifier,
                word(judgment_adjective),
                _: words((neighbor_any, "with")),
                word(name_object),
                _: eof,
            )
            .map(|(quantified, quantifier, judgment, other)| {
                (
                    quantifier,
                    Unit::Neighbor(quantified),
                    Unit::Neighbor(other),
                    judgment,
                )
            }),
            seq!(
                quantified_judged_unit,
                _: word(neighbor_any),
                word(name_object),
                _: eof,
            )
            .map(|((quantifier, judgment, unit), name)| {
                (quantifier, unit, Unit::Neighbor(name), judgment)
            }),
            seq!(
                quantified_judged_unit,
                _: not_neighbor_any,
                word(name_object),
                _: eof,
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
            alt((
                seq!(
                    pair(name_subject, "and"),
                    _: word("have"),
                    cardinal,
                    word(judgment_any),
                    _: alt((
                        words((neighbor_any, "in", "common")).void(),
                        words(("common", neighbor_any)).void(),
                    )),
                ),
                seq!(
                    pair(name_subject, "and"),
                    _: word("share"),
                    cardinal,
                    word(judgment_adjective),
                _: word(neighbor_any),
                ),
                seq!(
                    _: there_is,
                    cardinal,
                    word(judgment_any),
                    _: (neighboring_qualifier, word("both")),
                    pair(name_object, "and"),
                )
                .map(|(cardinal, judgment, names)| (names, cardinal, judgment)),
            ))
            .map(|(names, count, judgment)| (names.map(Unit::Neighbor), judgment, count.into())),
            seq!(
                quantifier,
                word(name_possessive),
                _: word("neighbors"),
                unit,
                is_judgment_any,
            )
            .map(|(quantity, name, unit, judgment)| {
                ([Unit::Neighbor(name), unit], judgment, quantity)
            }),
            seq!(
                name_has,
                cardinal,
                word(judgment_any),
                _: word(neighbor_any),
                unit,
            )
            .map(|(name, quantity, judgment, unit)| {
                ([Unit::Neighbor(name), unit], judgment, quantity.into())
            }),
            seq!(
                quantified_unit,
                _: neighboring_qualifier,
                word(name_object),
                is_judgment_any,
            )
            .map(|((quantifier, a), b, judgment)| ([a, Unit::Neighbor(b)], judgment, quantifier)),
            seq!(
                _: there_is,
                cardinal,
                word(judgment_any),
                _: words(("on", "the", "edges", "of")),
                line,
            )
            .map(|(count, judgment, line)| ([Unit::Edges, line.into()], judgment, count.into())),
        ))
        .map(|(units, judgment, cardinal)| Self::IntersectionSize(units, cardinal, judgment))
        .parse_next(input)
    }

    fn equal_number_of_traits_in_units(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: words(("There's", "an", "equal", "number", "of")),
                word(judgment_plural),
                unit_pair,
            ),
            seq!(
                _: words(("There", "are", "as", "many")),
                judged_unit,
                _: word("as"),
                _: opt(words(("there", "are"))),
                judged_unit,
            )
            .verify_map(|((judgment_a, a), (judgment_b, b))| {
                (judgment_a == judgment_b).then_some((judgment_a, [a, b]))
            }),
            seq!(
                pair(name_subject, "and"),
                _: words(("have", "an", "equal", "number", "of")),
                word(judgment_singular),
                _: word("neighbors"),
            )
            .map(|(names, judgment)| (judgment, names.map(Unit::Neighbor))),
        ))
        .map(|(judgment, pair)| Self::EqualNumberOfTraitsInUnits(pair, judgment))
        .parse_next(input)
    }

    fn each_unit_in_series_has_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: word("Each"),
                word(series),
                _: word("has"),
                cardinal,
                word(judgment_any),
            ),
            seq!(
                _: words(("There", be_verb_third_person)),
                cardinal,
                word(judgment_any),
                _: words((alt(("in", "among")), "each")),
                word(series),
            )
            .map(|(quantity, judgment, series)| (series, quantity, judgment)),
            seq!(
                _: words(("Everyone", "has")),
                cardinal,
                word(judgment_adjective),
                _: word(neighbor_any)
            )
            .map(|(quantity, judgment)| (Series::Neighbor, quantity, judgment)),
            seq!(
                _: words(("There", "is", "no")),
                word(series),
                _: words(("with", "only")),
                word(judgment_plural),
            )
            .map(|(series, judgment)| (series, Cardinal::AtLeast(1), !judgment)),
        ))
        .map(|(series, quantity, judgment)| {
            Self::EachUnitInSeriesHasSize(series, quantity, judgment)
        })
        .parse_next(input)
    }

    fn more_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: words(("There", "are")),
                word(more_or_less),
                alt((
                    pair(judgment_plural, "than"),
                    pair(judgment_adjective, "than"),
                )),
                unit,
            ),
            seq!(
                name_has,
                word(more_or_less),
                word(judgment_singular),
                _: word("than"),
                word(judgment_singular),
                _: word("neighbors"),
            )
            .map(|(name, cmp, left, right)| (cmp, [left, right], Unit::Neighbor(name))),
        ))
        .verify(|&(_, [more, less], _)| more == !less)
        .map(|(cmp, judgments, unit)| Self::MoreTraitsInUnit(unit, cmp.big(judgments)))
        .parse_next(input)
    }

    fn equal_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: words(("There", "are", "as", "many")),
                pair(judgment_plural, "as"),
                unit,
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
            seq!(
                name_has,
                _: words(("an", "equal", "number", "of")),
                pair(judgment_adjective, "and"),
                _: word("neighbors"),
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
        alt((
            seq!(
                _: words(("No", "one")),
                unit,
                _: words(("has", "more", "than")),
                word(number).map(Cardinal::AtMost),
                word(judgment_singular),
                _: word(neighbor_any)
            ),
            seq!(
                _: word("Everyone"),
                unit,
                _: word("has"),
                cardinal,
                word(judgment_adjective),
                _: word(neighbor_any),
            ),
        ))
        .map(|(unit, count, judgment)| Self::EachInUnitHasNNeighbors(unit, count, judgment))
        .parse_next(input)
    }

    fn total_number_of_traits_in_units(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: words(("There", "are", "a", "total", "of")),
                cardinal,
                word(judgment_any),
                alt((
                    preceded(word("in"), line_pair.map(|lines| lines.map(Unit::Line))),
                    pair(profession_plural, "and").map(|profs| profs.map(Unit::Profession)),
                )),
            )
            .map(|(cardinal, judgment, units)| (units, cardinal, judgment)),
            seq!(
                pair(name_subject, "and").map(|names| names.map(Unit::Neighbor)),
                _: word("have"),
                cardinal,
                word(judgment_adjective),
                _: words((neighbor_any, "in", "total")),
            ),
        ))
        .map(|(units, cardinal, judgment)| Self::TotalUnitsSize(units, cardinal, judgment))
        .parse_next(input)
    }

    fn all_traits_in_unit_are_in_unit(input: &mut &[&str]) -> Result<Self> {
        seq!(
            word(any_all),
            judged_unit,
            _: word(be_verb_third_person),
            unit,
        )
        .map(
            |(any_all, (judgment, split), other)| Self::TraitsInUnitAreInUnit {
                any_all,
                split,
                judgment,
                other,
            },
        )
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

// TODO split off a unit_predicate
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
            alt((between, preceded(opt(word("in")), line.map(Unit::Line)))),
            (direction, word(name_object))
                .map(|(direction, name)| Unit::Direction(direction, name)),
            separated_pair(
                directly_direction,
                word(indefinite_article),
                word(profession_singular),
            )
            .map(|(direction, profession)| Unit::Profession(profession).shift(direction)),
            alt((
                preceded(neighboring_qualifier, word(name_object)),
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
    seq!(
        _: opt(word(determiner)),
        opt(word(judgment_any)),
        unit
    )
    .parse_next(input)
}

fn judged_unit(input: &mut &[&str]) -> Result<(Judgment, Unit)> {
    alt((
        seq!(
            _: opt(word("us")),
            word(judgment_any),
            unit
        ),
        seq!(
            word(name_possessive),
            word(judgment_adjective),
            _: word(neighbor_any)
        )
        .map(|(name, judgment)| (judgment, Unit::Neighbor(name))),
    ))
    .parse_next(input)
}

fn quantified_judged_unit(input: &mut &[&str]) -> Result<(Quantifier, Judgment, Unit)> {
    alt((
        seq!(
            cardinal,
            word(name_possessive),
            opt(word(number)),
            word(judgment_adjective),
            _: word(neighbor_any),
        )
        .map(|(number, name, total, judgment)| {
            let quantifier = total.map_or(Quantifier::Simple(number), |total| {
                Quantifier::Subset(number, total)
            });
            (quantifier, judgment, Unit::Neighbor(name))
        }),
        seq!(
            quantifier,
            _: opt((word("of"), opt(word(determiner)))),
            word(judgment_any),
            unit,
        ),
    ))
    .parse_next(input)
}

fn cardinal_judged_unit(input: &mut &[&str]) -> Result<(Cardinal, Judgment, Unit)> {
    alt((
        (cardinal, word(judgment_any), unit),
        seq!(
            word(name_possessive),
            cardinal,
            word(judgment_adjective),
            _: word(neighbor_any)
        )
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
        word("multiple").value(Cardinal::AtLeast(2)),
    ))
    .parse_next(input)
}

fn number_phrase(input: &mut &[&str]) -> Result<Number> {
    preceded(opt(word(alt(("exactly", "only")))), word(number)).parse_next(input)
}

fn number(input: &mut &str) -> Result<Number> {
    alt((
        dec_uint,
        "none".value(0),
        "one".value(1),
        "two".value(2),
        "zero".value(0),
    ))
    .parse_next(input)
}

fn parity(input: &mut &[&str]) -> Result<Parity> {
    alt((
        word("even").value(Parity::Even),
        word("odd").value(Parity::Odd),
    ))
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
        terminated(word(raw_name), word("has")).map(NameRecipe::from),
    ))
    .parse_next(input)
}

fn name_is(input: &mut &[&str]) -> Result<NameRecipe> {
    alt((
        word("I'm").value(NameRecipe::Me),
        words(("I", "am")).value(NameRecipe::Me),
        terminated(word(raw_name), word("is")).map(NameRecipe::from),
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

fn pronoun_object_singular<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("him", "her", "them", "me")).parse_next(input)
}

fn pronoun_object_plural<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("them", "us")).parse_next(input)
}

fn quantified_profession(input: &mut &[&str]) -> Result<(Quantifier, Profession)> {
    separated_pair(quantifier, opt(word(determiner)), profession_any).parse_next(input)
}

fn quantified_unit_expr(input: &mut &[&str]) -> Result<(Quantifier, UnitExpr)> {
    alt((
        separated_pair(quantifier, opt(word(determiner)), maybe_judged_unit),
        seq!(
            cardinal,
            _: opt(word(determiner)),
            number_phrase,
            maybe_judged_unit,
        )
        .map(|(cardinal, total, unit)| (Quantifier::Subset(cardinal, total), unit)),
    ))
    .map(|(quantifier, (judgment, unit))| {
        let unit = if let Some(judgment) = judgment {
            unit.with_judgment(judgment)
        } else {
            unit.into()
        };
        (quantifier, unit)
    })
    .parse_next(input)
}

fn quantified_unit(input: &mut &[&str]) -> Result<(Quantifier, Unit)> {
    alt((
        separated_pair(quantifier, opt(word(determiner)), unit),
        seq!(cardinal, _: opt(word(determiner)), number_phrase, unit)
            .map(|(cardinal, total, unit)| (Quantifier::Subset(cardinal, total), unit)),
    ))
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

fn neighboring_verb<'input, 'inner: 'input>(
    input: &mut &'input [&'inner str],
) -> Result<&'input [&'inner str]> {
    alt((
        (opt(word("also")), word("neighbor")).take(),
        (word(be_verb_third_person), word("neighboring")).take(),
    ))
    .parse_next(input)
}

fn neighboring_qualifier<'input, 'inner: 'input>(
    input: &mut &'input [&'inner str],
) -> Result<&'input [&'inner str]> {
    alt((
        words(("who", "neighbor")).take(),
        word("neighboring").take(),
    ))
    .parse_next(input)
}

fn be_verb_third_person<'input>(input: &mut &'input str) -> Result<&'input str> {
    alt(("is", "are")).parse_next(input)
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

fn any_all(input: &mut &str) -> Result<AnyAll> {
    alt(("all".value(AnyAll::All), "any".value(AnyAll::Any))).parse_next(input)
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
