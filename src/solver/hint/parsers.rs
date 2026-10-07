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
use crate::solver::hint::parsers::phrases::{AnyAll, BoundPair, MoreOrLess, UnitExpr};
use crate::solver::hint::recipes::{ColumnRecipe, LineRecipe, NameRecipe, RowRecipe};
use crate::solver::hint::{Bound, LineKind, Number, Parity};

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
                terminated(Self::unit_shares_n_out_of_n_traits_with_unit, eof),
                terminated(Self::unit_size, eof),
                terminated(Self::only_one_person_in_unit_has_n_trait_neighbors, eof),
            )),
            alt((
                terminated(Self::n_people_in_unit_have_n_trait_neighbors, eof),
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
                word("All").value(Some(BoundPair::Simple(Bound::AtLeast(1)))),
                bound_pair.map(Some),
                empty.value(None),
            )),
            judged_unit,
            _: words(("are", "connected")),
        )
        .verify_map(|(bound_pair, (judgment, unit))| {
            let unit = unit.with_judgment(judgment);
            let unit = if let Some(bound_pair) = bound_pair {
                unit.bound(bound_pair.to_bound()?)
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
            separated_pair(name_is, words(("one", "of")), bounded_judged_unit).map(
                |(name, (bound, judgment, unit))| Self::IsOneOfNInUnit(unit, name, bound, judgment),
            ),
            separated_pair(name_is, words(("the", "only")), judged_unit).map(
                |(name, (judgment, unit))| {
                    Self::IsOneOfNInUnit(unit, name, Bound::Exact(1), judgment)
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
                word(judgment_plural),
                _: word("in"),
                line,
                _: word("than"),
                line,
            )
            .map(|(cmp, judgment, left, right)| {
                let units = [left, right].map(|line| Unit::from(line).with_judgment(judgment));
                (cmp, units, None)
            }),
            seq!(
                _: words(("There", "are")),
                word(more_or_less),
                judged_unit,
                _: (word("than"), opt(word(determiner))),
                opt(word(judgment_any)),
                unit
            )
            .map(|(cmp, (judgment, left), judgment_right, right)| {
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
            preceded(there_is, bounded_judged_unit)
                .map(|(bound, judgment, unit)| (bound, judgment, unit.into())),
            seq!(
                name_has,
                bound,
                word(judgment_adjective),
                _: word(neighbor_any)
            )
            .map(|(name, bound, judgment)| (bound, judgment, Unit::Neighbor(name).into())),
            seq!(
                pair_bounded_unit_expr,
                _: word(has_have),
                a_judgment,
                directly_direction,
                _: word(pronoun_object_plural)
            )
            .map(|((bound, unit), judgment, direction)| {
                let (bound, unit) = match bound {
                    BoundPair::Simple(bound) => (bound, unit),
                    BoundPair::Subset {
                        matching: bound,
                        total,
                    } => (bound, unit.bound(total)),
                };
                (bound, judgment, unit.shift(direction))
            }),
            (pair_bounded_profession, is_judgment_any).map(|((bound, profession), judgment)| {
                let unit = Unit::Profession(profession);
                match bound {
                    BoundPair::Simple(bound) => (bound, judgment, unit.into()),
                    BoundPair::Subset {
                        matching: bound,
                        total,
                    } => (bound, judgment, unit.bound(total).into()),
                }
            }),
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
            .map(|(unit, judgment)| (Bound::Exact(0), !judgment, unit.into())),
            seq!(
                _: words(("Not", "everyone")),
                unit,
                _: word("is"),
                judgment_predicate_singular,
            )
            .map(|(unit, judgment)| (Bound::AtLeast(1), !judgment, unit.into())),
        ))
        .map(|(bound, judgment, unit)| Self::UnitSize(unit.with_judgment(judgment), bound))
        .parse_next(input)
    }

    fn only_one_person_in_unit_has_n_trait_neighbors(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                alt((
                    preceded(words(("Only", "one", "person")), unit),
                    pair_bounded_profession.verify_map(|(bound, profession)| {
                        if let BoundPair::Subset { matching: Bound::Exact(1), total } = bound {
                            Some(Unit::Profession(profession).bound(total))
                        } else {
                            None
                        }
                    }),
                )),
                _: word("has"),
                bound,
                word(judgment_adjective),
                _: word(neighbor_any)
            )
            .map(|(unit, bound, judgment)| (None, unit, bound, judgment)),
            seq!(
                name_is.map(Some),
                _: words(("the", "only", alt(("one", "person")))),
                unit,
                _: word("with"),
                bound,
                word(judgment_adjective),
                _: word(neighbor_any)
            ),
        ))
        .map(|(name, unit, bound, judgment)| {
            Self::UniqueInUnitHasNNeighbors(unit, bound, name, judgment)
        })
        .parse_next(input)
    }

    fn n_people_in_unit_have_n_trait_neighbors(input: &mut &[&str]) -> Result<Self> {
        seq!(
            pair_bounded_profession,
            _: word(has_have),
            bound,
            word(judgment_adjective),
            _: word(neighbor_any),
        )
        .map(|((count, profession), neighbors, judgment)| {
            let unit = Unit::Profession(profession);
            let (count, unit) = match count {
                BoundPair::Simple(bound) => (bound, unit),
                BoundPair::Subset {
                    matching: bound,
                    total,
                } => (bound, unit.bound(total)),
            };
            Self::NInUnitHaveNNeighbors {
                unit,
                count,
                each: neighbors,
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
                bound,
                word(judgment_any)
            ),
            seq!(
                _: words(("Only", "one", "person", "has")),
                bound,
                word(judgment_any),
                _: word(neighbor_any),
            )
            .map(|(bound, judgment)| (Series::Neighbor, bound, judgment)),
            seq!(
                _: words(("There", "is", "only", "one", "profession", "with")),
                bound,
                word(judgment_any),
            )
            .map(|(bound, judgment)| (Series::Profession, bound, judgment)),
        ))
        .map(|(series, bound, judgment)| Self::UniqueUnitInSeriesHasSize(series, bound, judgment))
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
                bound,
                word(judgment_any),
            ),
            seq!(
                name_is.map(UnitInSeries::Neighbor),
                _: words(("the", "only", "one", "with")),
                bound,
                word(judgment_adjective),
                _: word(neighbor_any),
            ),
            seq!(
                word(profession_singular).map(UnitInSeries::Profession),
                _: words(("is", "the", "only", "profession", "with")),
                bound,
                word(judgment_any),
            ),
        ))
        .map(|(unit, bound, judgment)| Self::OnlyGivenUnitHasNTraits(unit, bound, judgment))
        .parse_next(input)
    }

    fn unit_shares_n_out_of_n_traits_with_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: there_is,
                bounded_judged_unit,
                alt((
                    preceded(neighboring_predicate, word(name_object)).map(Unit::Neighbor),
                    unit,
                )),
                _: eof,
            )
            .map(|((bound, judgment, unit), other)| (bound.into(), unit, other, judgment)),
            seq!(
                pair_bounded_judged_unit,
                alt((
                    preceded(neighboring_verb, word(name_object)).map(Unit::Neighbor),
                    preceded(word(be_verb_third_person), unit),
                )),
                _: eof,
            )
            .map(|((bound, judgment, unit), other)| (bound, unit, other, judgment)),
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
                let bound = BoundPair::Subset {
                    matching: Bound::Exact(1),
                    total: 1,
                };
                (bound, Unit::Neighbor(name), other, judgment)
            }),
            seq!(
                word(name_subject),
                _: word("shares"),
                bound_pair,
                word(judgment_adjective),
                _: words((neighbor_any, "with")),
                word(name_object),
                _: eof,
            )
            .map(|(name, bound, judgment, other)| {
                (bound, Unit::Neighbor(name), Unit::Neighbor(other), judgment)
            }),
            seq!(
                pair_bounded_judged_unit,
                _: word(neighbor_any),
                word(name_object),
                _: eof,
            )
            .map(|((bound, judgment, unit), name)| (bound, unit, Unit::Neighbor(name), judgment)),
            seq!(
                pair_bounded_judged_unit,
                _: not_neighbor_any,
                word(name_object),
                _: eof,
            )
            .map(|((bound, judgment, unit), name)| {
                (bound, unit, Unit::NotNeighbor(name), judgment)
            }),
        ))
        .map(|(bound_pair, split, other, judgment)| match bound_pair {
            bound @ BoundPair::Simple(_) => Self::IntersectionSize([split, other], bound, judgment),
            BoundPair::Subset {
                matching: intersection,
                total,
            } => Self::UnitAndIntersectionSize {
                total,
                split,
                other,
                intersection,
                judgment,
            },
        })
        .parse_next(input)
    }

    fn units_share_n_traits(input: &mut &[&str]) -> Result<Self> {
        alt((
            alt((
                seq!(
                    pair(name_subject, "and"),
                    _: word("have"),
                    bound,
                    word(judgment_any),
                    _: alt((
                        words((neighbor_any, "in", "common")).void(),
                        words(("common", neighbor_any)).void(),
                    )),
                ),
                seq!(
                    pair(name_subject, "and"),
                    _: word("share"),
                    bound,
                    word(judgment_adjective),
                _: word(neighbor_any),
                ),
                seq!(
                    _: there_is,
                    bound,
                    word(judgment_any),
                    _: (neighboring_predicate, word("both")),
                    pair(name_object, "and"),
                )
                .map(|(bound, judgment, names)| (names, bound, judgment)),
            ))
            .map(|(names, bound, judgment)| (names.map(Unit::Neighbor), judgment, bound.into())),
            seq!(
                bound_pair,
                word(name_possessive),
                _: word("neighbors"),
                unit,
                is_judgment_any,
            )
            .map(|(bound_pair, name, unit, judgment)| {
                ([Unit::Neighbor(name), unit], judgment, bound_pair)
            }),
            seq!(
                name_has,
                bound,
                word(judgment_adjective),
                _: word(neighbor_any),
                unit,
            )
            .map(|(name, bound, judgment, unit)| {
                ([Unit::Neighbor(name), unit], judgment, bound.into())
            }),
            seq!(
                pair_bounded_profession,
                _: neighboring_predicate,
                word(name_object),
                is_judgment_any,
            )
            .map(|((bound, profession), name, judgment)| {
                let units = [Unit::Profession(profession), Unit::Neighbor(name)];
                (units, judgment, bound)
            }),
            seq!(
                _: there_is,
                bound,
                word(judgment_any),
                _: words(("on", "the", "edges", "of")),
                line,
            )
            .map(|(bound, judgment, line)| ([Unit::Edges, line.into()], judgment, bound.into())),
        ))
        .map(|(units, judgment, bound_pair)| Self::IntersectionSize(units, bound_pair, judgment))
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
                _: (word("as"), opt(words(("there", "are"))), opt(word("us"))),
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
                bound,
                word(judgment_any),
            ),
            seq!(
                _: words(("There", be_verb_third_person)),
                bound,
                word(judgment_any),
                _: words((alt(("in", "among")), "each")),
                word(series),
            )
            .map(|(bound, judgment, series)| (series, bound, judgment)),
            seq!(
                _: words(("Everyone", "has")),
                bound,
                word(judgment_adjective),
                _: word(neighbor_any)
            )
            .map(|(bound, judgment)| (Series::Neighbor, bound, judgment)),
            seq!(
                _: words(("There", "is", "no")),
                word(series),
                _: words(("with", "only")),
                word(judgment_plural),
            )
            .map(|(series, judgment)| (series, Bound::AtLeast(1), !judgment)),
        ))
        .map(|(series, bound, judgment)| Self::EachUnitInSeriesHasSize(series, bound, judgment))
        .parse_next(input)
    }

    fn more_traits_in_unit(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: words(("There", "are")),
                word(more_or_less),
                alt((
                    (pair(judgment_plural, "than"), unit),
                    (pair(judgment_adjective, "than"), word(profession_plural).map(Unit::Profession)),
                )),
            )
            .map(|(cmp, (judgments, unit))| (cmp, judgments, unit)),
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
        .verify(|&(_, [left, right], _)| left == !right)
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
                word(number).map(Bound::AtMost),
                word(judgment_singular),
                _: word(neighbor_any)
            ),
            seq!(
                _: word("Everyone"),
                unit,
                _: word("has"),
                bound,
                word(judgment_adjective),
                _: word(neighbor_any),
            ),
        ))
        .map(|(unit, bound, judgment)| Self::EachInUnitHasNNeighbors(unit, bound, judgment))
        .parse_next(input)
    }

    fn total_number_of_traits_in_units(input: &mut &[&str]) -> Result<Self> {
        alt((
            seq!(
                _: words(("There", "are", "a", "total", "of")),
                bound,
                word(judgment_any),
                alt((
                    preceded(word("in"), line_pair.map(|lines| lines.map(Unit::Line))),
                    pair(profession_plural, "and").map(|profs| profs.map(Unit::Profession)),
                )),
            )
            .map(|(bound, judgment, units)| (units, bound, judgment)),
            seq!(
                pair(name_subject, "and").map(|names| names.map(Unit::Neighbor)),
                _: word("have"),
                bound,
                word(judgment_adjective),
                _: words((neighbor_any, "in", "total")),
            ),
        ))
        .map(|(units, bound, judgment)| Self::TotalUnitsSize(units, bound, judgment))
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

fn unit(input: &mut &[&str]) -> Result<Unit> {
    alt((
        alt((
            words(("in", "total")).value(Unit::All),
            words(("on", "the", "edges")).value(Unit::Edges),
            words(("in", "a", "corner")).value(Unit::Corners),
            words(("in", "the", "corners")).value(Unit::Corners),
            preceded(words(("in", "between")), pair(name_object, "and")).map(Unit::Between),
            preceded(word("in"), line.map(Unit::Line)),
            (direction, word(name_object))
                .map(|(direction, name)| Unit::Direction(direction, name)),
            separated_pair(
                directly_direction,
                word(indefinite_article),
                word(profession_singular),
            )
            .map(|(direction, profession)| Unit::Profession(profession).shift(direction)),
            terminated(word(name_possessive), word(neighbor_any)).map(Unit::Neighbor),
        )),
        delimited(
            words(("one", "of")),
            word(name_possessive),
            word("neighbors"),
        )
        .map(Unit::Neighbor),
        preceded(neighboring_predicate, word(name_object)).map(Unit::Neighbor),
        preceded(word("not"), word(name_object)).map(Unit::NotName),
        profession_any.map(Unit::Profession),
    ))
    .parse_next(input)
}

fn maybe_judged_unit(input: &mut &[&str]) -> Result<(Option<Judgment>, Unit)> {
    alt((
        (word(judgment_any).map(Some), unit),
        preceded(
            word(alt(("persons", "person"))),
            alt((
                words(("on", "the", "edges")).value(Unit::Edges),
                words(("in", "a", "corner")).value(Unit::Corners),
                preceded(word("in"), line.map(Unit::Line)),
            )),
        )
        .map(|unit| (None, unit)),
        profession_any.map(|profession| (None, Unit::Profession(profession))),
    ))
    .parse_next(input)
}

fn judged_unit(input: &mut &[&str]) -> Result<(Judgment, Unit)> {
    alt((
        (word(judgment_any), unit),
        seq!(
            word(name_possessive),
            word(judgment_adjective),
            _: word(neighbor_any)
        )
        .map(|(name, judgment)| (judgment, Unit::Neighbor(name))),
    ))
    .parse_next(input)
}

fn pair_bounded_judged_unit(input: &mut &[&str]) -> Result<(BoundPair, Judgment, Unit)> {
    alt((
        seq!(
            bound,
            word(name_possessive),
            opt(word(number)),
            word(judgment_adjective),
            _: word(neighbor_any),
        )
        .map(|(number, name, total, judgment)| {
            let bound = total.map_or(BoundPair::Simple(number), |total| BoundPair::Subset {
                matching: number,
                total,
            });
            (bound, judgment, Unit::Neighbor(name))
        }),
        seq!(
            bound_pair,
            _: opt((word("of"), opt(word(determiner)))),
            word(judgment_any),
            unit,
        ),
    ))
    .parse_next(input)
}

fn bounded_judged_unit(input: &mut &[&str]) -> Result<(Bound, Judgment, Unit)> {
    alt((
        (bound, word(judgment_any), unit),
        seq!(
            word(name_possessive),
            bound,
            word(judgment_adjective),
            _: word(neighbor_any)
        )
        .map(|(name, bound, judgment)| (bound, judgment, Unit::Neighbor(name))),
    ))
    .parse_next(input)
}

// TODO include the optional determiners at the end of `bound_pair` and `bound` and remove ones
// made redundant
fn bound_pair(input: &mut &[&str]) -> Result<BoundPair> {
    alt((
        word("both").value(BoundPair::Subset {
            matching: Bound::Exact(2),
            total: 2,
        }),
        (
            word("neither"),
            opt((
                word("of"),
                opt(words((alt((determiner, pronoun_possessive)), "2"))),
            )),
        )
            .value(BoundPair::Subset {
                matching: Bound::Exact(0),
                total: 2,
            }),
        separated_pair(
            bound,
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
        .map(|(a, b)| BoundPair::Subset {
            matching: a,
            total: b,
        }),
        terminated(bound, opt(word(determiner))).map(BoundPair::Simple),
        words(("the", "only")).value(BoundPair::Subset {
            matching: Bound::Exact(1),
            total: 1,
        }),
    ))
    .parse_next(input)
}

fn bound(input: &mut &[&str]) -> Result<Bound> {
    alt((
        word("no").value(Bound::Exact(0)),
        terminated(word(number), words(("or", "more"))).map(Bound::AtLeast),
        terminated(number_phrase, opt(word("of"))).map(Bound::Exact),
        preceded(words(("at", "least")), word(number)).map(Bound::AtLeast),
        delimited(word("an"), parity, words(("number", "of"))).map(Bound::Parity),
        word("multiple").value(Bound::AtLeast(2)),
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

fn pair_bounded_profession(input: &mut &[&str]) -> Result<(BoundPair, Profession)> {
    separated_pair(bound_pair, opt(word(determiner)), profession_any).parse_next(input)
}

fn pair_bounded_unit_expr(input: &mut &[&str]) -> Result<(BoundPair, UnitExpr)> {
    separated_pair(bound_pair, opt(word(determiner)), maybe_judged_unit)
        .map(|(bound, (judgment, unit))| {
            let unit = if let Some(judgment) = judgment {
                unit.with_judgment(judgment)
            } else {
                unit.into()
            };
            (bound, unit)
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

fn neighboring_predicate<'input, 'inner: 'input>(
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
