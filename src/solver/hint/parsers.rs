use std::iter::once;

use anyhow::anyhow;
use winnow::ascii::dec_uint;
use winnow::combinator::{
    alt, delimited, dispatch, empty, eof, fail, opt, preceded, separated_pair, seq, terminated,
};
use winnow::error::{ParserError, StrContext};
use winnow::token::{any, rest};
use winnow::{ModalResult, Parser};

use crate::models::{Column, Direction, Judged, Judgment, Profession, Row};
use crate::solver::hint::parsers::phrases::{
    AnyAll, BoundPair, DisjointSeries, MoreOrLess, Series,
};
use crate::solver::hint::recipes::{ColumnRecipe, LineRecipe, NameRecipe, RowRecipe};
use crate::solver::hint::{Bound, LineKind, Number, Parity};

mod phrases;

pub(crate) use phrases::{Sentence, Unit, UnitInSeries};

const PUNCTUATION: [char; 3] = ['.', '!', ':'];

impl Sentence {
    pub(crate) fn parse(hint: &str) -> anyhow::Result<Self> {
        let hint = hint.replace(' ', " ");
        Self::parse_entire(&hint)
            .or_else(|err| {
                hint.split(PUNCTUATION)
                    .find_map(|segment| Self::parse_entire(segment).ok())
                    .ok_or(err)
            })
            .map_err(|_err| anyhow!("parsing {hint:?}"))
    }

    fn parse_entire(hint: &str) -> anyhow::Result<Self> {
        let words: Vec<_> = hint
            .split_ascii_whitespace()
            .filter(|word| !word.is_empty())
            .collect();
        Self::parse_exact(&words).or_else(|err| {
            if let Some((&first_word, rest)) = words.split_first()
                && let first_word = first_word.to_owned()
                && let mut chars = first_word.chars()
                && let Some(first_char) = chars.next()
                && !first_char.is_lowercase()
            {
                let first_word: String = first_char.to_lowercase().chain(chars).collect();
                let words: Vec<_> = once(&*first_word).chain(rest.iter().copied()).collect();
                Self::parse_exact(&words).map_err(|_err| err)
            } else {
                Err(err)
            }
        })
    }

    fn parse_exact(hint: &[&str]) -> anyhow::Result<Self> {
        Self::any.parse(hint).map_err(|e| anyhow!("{e:?}"))
    }

    fn any(input: &mut &[&str]) -> ModalResult<Self> {
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
            terminated(Self::all_traits_in_unit_are_in_unit, eof),
            terminated(Self::is_one_of_traits_in_unit, eof),
            terminated(Self::if_then, eof),
            terminated(Self::only_trait_in_series, eof),
            terminated(Self::traits_in_unit_have_trait_in_direction, eof),
        ))
        .parse_next(input)
    }

    fn traits_are_neighbors_in_unit(input: &mut &[&str]) -> ModalResult<Self> {
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
            let bound = if let Some(bound_pair) = bound_pair {
                Some(bound_pair.to_bound()?)
            } else {
                None
            };
            Some(Self::UnitIsConnected(unit, judgment, bound))
        })
        .parse_next(input)
    }

    fn has_most_traits(input: &mut &[&str]) -> ModalResult<Self> {
        alt((
            alt((
                seq!(
                    line,
                    _: words(("has", "more")),
                    word(judgment_plural),
                    _: words(("than", "any", "other")),
                    word(line_kind),
                )
                .map(|(line, judgment, kind)| (judgment, line, kind)),
                seq!(
                    _: words(("There", "are", "more")),
                    word(judgment_plural),
                    _: word("in"),
                    line,
                    _: words(("than", "in", "any", "other")),
                    word(line_kind)
                ),
            ))
            .verify(|&(_, line, kind)| line.kind() == kind)
            .map(|(judgment, line, _)| (line.into(), judgment)),
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

    fn is_one_of_n_traits_in_unit(input: &mut &[&str]) -> ModalResult<Self> {
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

    fn is_one_of_traits_in_unit(input: &mut &[&str]) -> ModalResult<Self> {
        separated_pair(name_is, words(("one", "of", determiner)), judged_unit)
            .map(|(name, (judgment, unit))| Self::IsInUnit(unit, name, judgment))
            .parse_next(input)
    }

    fn more_traits_in_unit_than_unit(input: &mut &[&str]) -> ModalResult<Self> {
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
                let units = [left, right].map(|line| Unit::from(line).judged(judgment));
                (cmp, units, None)
            }),
            seq!(
                _: words(("There", "are")),
                word(more_or_less),
                word(judgment_adjective),
                word(profession_plural),
                _: (word("than"), opt(word("us"))),
                word(judgment_adjective),
                word(profession_plural)
            )
            .map(|(cmp, judgment_left, left, judgment_right, right)| {
                let left = Unit::Profession(left).judged(judgment_left);
                let right = Unit::Profession(right).judged(judgment_right);
                (cmp, [left, right], None)
            }),
            seq!(
                _: words(("There", "are")),
                word(more_or_less),
                judged_unit,
                _: word("than"),
                unit_prepositional
            )
            .map(|(cmp, (judgment, left), right)| {
                (cmp, [left, right].map(|unit| unit.judged(judgment)), None)
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
                let units = [left, right].map(|name| Unit::Neighbor(name).judged(judgment));
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
                let units = [left, right]
                    .map(|direction| Unit::Direction(direction, name.clone()).judged(judgment));
                (cmp, units, None)
            }),
        ))
        .map(|(cmp, units, excess)| {
            let [big, small] = cmp.big_small(units);
            Self::UnitBiggerThanUnit { big, small, excess }
        })
        .parse_next(input)
    }

    fn unit_size(input: &mut &[&str]) -> ModalResult<Self> {
        alt((
            preceded(there_is, bounded_judged_unit)
                .map(|(bound, judgment, unit)| (bound, judgment, unit)),
            seq!(
                name_has,
                bound,
                word(judgment_adjective),
                _: word(neighbor_any)
            )
            .map(|(name, bound, judgment)| (bound, judgment, Unit::Neighbor(name))),
            seq!(
                pair_bounded_unit,
                _: word(has_have),
                a_judgment,
                directly_direction,
                _: opt(word(pronoun_object))
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
                    BoundPair::Simple(bound) => (bound, judgment, unit),
                    BoundPair::Subset {
                        matching: bound,
                        total,
                    } => (bound, judgment, unit.bound(total)),
                }
            }),
            alt((
                seq!(
                    _: word("Everyone"),
                    unit_prepositional,
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
                seq!(
                    _: word("All"),
                    word(profession_plural).map(Unit::Profession),
                    _: word("are"),
                    word(judgment_adjective)
                ),
            ))
            .map(|(unit, judgment)| (Bound::Exact(0), !judgment, unit)),
            seq!(
                _: words(("Not", "everyone")),
                unit_prepositional,
                _: word("is"),
                judgment_predicate_singular,
            )
            .map(|(unit, judgment)| (Bound::AtLeast(1), !judgment, unit)),
            seq!(line, _: word("has"), bound, word(judgment_any))
                .map(|(line, bound, judgment)| (bound, judgment, line.into())),
        ))
        .map(|(bound, judgment, unit)| Self::UnitSize(unit, judgment, bound))
        .parse_next(input)
    }

    fn only_one_person_in_unit_has_n_trait_neighbors(input: &mut &[&str]) -> ModalResult<Self> {
        alt((
            seq!(
                alt((
                    preceded(words(("Only", "one", "person")), unit_prepositional),
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
                unit_prepositional,
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

    fn n_people_in_unit_have_n_trait_neighbors(input: &mut &[&str]) -> ModalResult<Self> {
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

    fn only_one_unit_in_series_has_exactly_n_traits(input: &mut &[&str]) -> ModalResult<Self> {
        alt((
            seq!(
                _: (opt(word("only")), word("one")),
                word(line_kind).map(Series::Line),
                _: word("has"),
                bound,
                word(judgment_any)
            ),
            seq!(
                _: words(("Only", "one", "person", "has")),
                bound,
                word(judgment_adjective),
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

    fn only_given_unit_has_exactly_n_traits(input: &mut &[&str]) -> ModalResult<Self> {
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

    fn unit_shares_n_out_of_n_traits_with_unit(input: &mut &[&str]) -> ModalResult<Self> {
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

    fn units_share_n_traits(input: &mut &[&str]) -> ModalResult<Self> {
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
                unit_prepositional,
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
                unit_prepositional,
            )
            .map(|(name, bound, judgment, unit)| {
                ([Unit::Neighbor(name), unit], judgment, bound.into())
            }),
            seq!(
                name_doesnt_have,
                _: word("any"),
                word(judgment_adjective),
                _: word("neighbors"),
                unit_prepositional,
            )
            .map(|(name, judgment, unit)| ([Unit::Neighbor(name), unit], judgment, 0.into())),
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
            alt((
                seq!(
                    _: word("All"),
                    word(profession_plural),
                    unit_prepositional,
                    _: word("are"),
                    word(judgment_adjective)
                ),
                seq!(
                    _: word("Each"),
                    word(profession_singular),
                    unit_prepositional,
                    _: word("is"),
                    judgment_predicate_singular
                ),
            ))
            .map(|(profession, unit, judgment)| {
                ([Unit::Profession(profession), unit], !judgment, 0.into())
            }),
        ))
        .map(|(units, judgment, bound_pair)| Self::IntersectionSize(units, bound_pair, judgment))
        .parse_next(input)
    }

    fn equal_number_of_traits_in_units(input: &mut &[&str]) -> ModalResult<Self> {
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

    fn each_unit_in_series_has_n_traits(input: &mut &[&str]) -> ModalResult<Self> {
        alt((
            seq!(
                _: word("Each"),
                word(disjoint_series).map(Series::from),
                _: word("has"),
                bound,
                word(judgment_any),
            ),
            seq!(
                _: words(("There", be_verb_third_person)),
                bound,
                word(judgment_any),
                _: words((alt(("in", "among")), "each")),
                word(disjoint_series).map(Series::from),
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
                word(disjoint_series).map(Series::from),
                _: words(("with", "only")),
                word(judgment_plural),
            )
            .map(|(series, judgment)| (series, Bound::AtLeast(1), !judgment)),
        ))
        .map(|(series, bound, judgment)| Self::EachUnitInSeriesHasSize(series, bound, judgment))
        .parse_next(input)
    }

    fn more_traits_in_unit(input: &mut &[&str]) -> ModalResult<Self> {
        alt((
            seq!(
                _: words(("There", "are")),
                word(more_or_less),
                alt((
                    (pair(judgment_plural, "than"), unit_prepositional),
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

    fn equal_traits_in_unit(input: &mut &[&str]) -> ModalResult<Self> {
        alt((
            seq!(
                _: words(("There", "are", "as", "many")),
                pair(judgment_plural, "as"),
                unit_prepositional,
            ),
            preceded(
                words(("There's", "an", "equal", "number", "of")),
                alt((
                    (
                        pair(judgment_adjective, "and"),
                        word(profession_plural).map(Unit::Profession),
                    ),
                    (pair(judgment_plural, "and"), unit_prepositional),
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

    fn has_trait(input: &mut &[&str]) -> ModalResult<Self> {
        separated_pair(word(raw_name), word("is"), judgment_predicate_singular)
            .map(|(name, judgment)| Self::HasTrait(name.into(), judgment))
            .parse_next(input)
    }

    fn at_most_n_traits_in_neighbors_in_unit(input: &mut &[&str]) -> ModalResult<Self> {
        alt((
            seq!(
                _: words(("No", "one")),
                unit_prepositional,
                _: word("has"),
                alt((
                    seq!(_: words(("more", "than")), word(number).map(Bound::AtMost), word(judgment_adjective)),
                    a_judgment.map(|judgment| (Bound::Exact(0), judgment)),
                )),
                _: word(neighbor_any)
            ),
            seq!(
                _: word("Everyone"),
                unit_prepositional,
                _: word("has"),
                (bound, word(judgment_adjective)),
                _: word(neighbor_any),
            ),
        ))
        .map(|(unit, (bound, judgment))| Self::EachInUnitHasNNeighbors(unit, bound, judgment))
        .parse_next(input)
    }

    fn total_number_of_traits_in_units(input: &mut &[&str]) -> ModalResult<Self> {
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

    fn all_traits_in_unit_are_in_unit(input: &mut &[&str]) -> ModalResult<Self> {
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

    fn if_then(input: &mut &[&str]) -> ModalResult<Self> {
        alt((
            seq!(
                _: word("If"),
                name_is,
                judgment_predicate_comma,
                _: words(("then", "so")),
                is_name
            )
            .map(|(name_p, judgment, name_c)| [(name_p, judgment), (name_c, judgment)]),
            seq!(
                _: word("If"),
                (name_is, judgment_predicate_comma),
                _: word("then"),
                (name_is, judgment_predicate_singular)
            )
            .map(<[_; 2]>::from),
            seq!(
                (name_is, judgment_predicate_singular),
                _: word("unless"),
                name_is,
                judgment_predicate_singular,
            )
            .map(|(conclusion, name_p, not_judgment_p)| [(name_p, !not_judgment_p), conclusion]),
        ))
        .map(|clauses| Self::IfThen(clauses.map(|(name, judgment)| Judged(name, judgment))))
        .parse_next(input)
    }

    fn only_trait_in_series(input: &mut &[&str]) -> ModalResult<Self> {
        seq!(
            name_is,
            _: words(("the", "only")),
            word(judgment_singular),
            _: words(("in", pronoun_possessive_singular)),
            word(disjoint_series)
        )
        .map(|(name, judgment, series)| Self::OnlyTraitInSeries(name, series, judgment))
        .parse_next(input)
    }

    fn traits_in_unit_have_trait_in_direction(input: &mut &[&str]) -> ModalResult<Self> {
        seq!(
            bounded_judged_unit,
            _: word(has_have),
            a_judgment,
            directly_direction,
            _: opt(word(pronoun_object)),
        )
        .map(
            |((bound, source_judgment, source), target_judgment, direction)| {
                Self::TraitsInUnitHaveTraitInDirection {
                    source,
                    source_judgment,
                    direction,
                    target_judgment,
                    bound,
                }
            },
        )
        .parse_next(input)
    }
}

fn unit_pair(input: &mut &[&str]) -> ModalResult<[Unit; 2]> {
    alt((
        preceded(word("in"), line_pair).map(|lines| lines.map(Unit::Line)),
        separated_pair(unit_prepositional, word("and"), unit_prepositional).map(<[Unit; 2]>::from),
    ))
    .parse_next(input)
}

fn unit(input: &mut &[&str]) -> ModalResult<Unit> {
    alt((
        unit_prepositional,
        terminated(word(name_possessive), word(neighbor_any)).map(Unit::Neighbor),
        delimited(
            words(("one", "of")),
            word(name_possessive),
            word("neighbors"),
        )
        .map(Unit::Neighbor),
        preceded(word("not"), word(name_object)).map(Unit::NotName),
        profession_any.map(Unit::Profession),
    ))
    .parse_next(input)
}

fn unit_prepositional(input: &mut &[&str]) -> ModalResult<Unit> {
    alt((
        words(("in", "total")).value(Unit::All),
        words(("on", "the", "edges")).value(Unit::Edges),
        words(("in", "a", "corner")).value(Unit::Corners),
        words(("in", "the", "corners")).value(Unit::Corners),
        preceded(words(("in", "between")), pair(name_object, "and")).map(Unit::Between),
        preceded(word("in"), line.map(Unit::Line)),
        (direction, word(name_object)).map(|(direction, name)| Unit::Direction(direction, name)),
        separated_pair(
            directly_direction,
            word(indefinite_article),
            word(profession_singular),
        )
        .map(|(direction, profession)| Unit::Profession(profession).shift(direction)),
        preceded(neighboring_predicate, word(name_object)).map(Unit::Neighbor),
    ))
    .parse_next(input)
}

fn unit_subject(input: &mut &[&str]) -> ModalResult<Unit> {
    alt((
        preceded(
            word(alt(("persons", "person"))),
            alt((
                words(("on", "the", "edges")).value(Unit::Edges),
                words(("in", "a", "corner")).value(Unit::Corners),
                preceded(word("in"), line.map(Unit::Line)),
                (direction, word(name_object))
                    .map(|(direction, name)| Unit::Direction(direction, name)),
            )),
        ),
        profession_any.map(Unit::Profession),
    ))
    .parse_next(input)
}

fn judged_unit(input: &mut &[&str]) -> ModalResult<(Judgment, Unit)> {
    alt((
        (word(judgment_any), unit_prepositional),
        (word(judgment_any), profession_any.map(Unit::Profession)),
        seq!(
            word(name_possessive),
            word(judgment_adjective),
            _: word(neighbor_any)
        )
        .map(|(name, judgment)| (judgment, Unit::Neighbor(name))),
    ))
    .parse_next(input)
}

fn pair_bounded_judged_unit(input: &mut &[&str]) -> ModalResult<(BoundPair, Judgment, Unit)> {
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
            alt((unit_prepositional, profession_any.map(Unit::Profession))),
        ),
        seq!(
            _: words(("neither", "of")),
            word(name_possessive),
            _: opt(word("2")),
            word(judgment_adjective),
            _: word("neighbors")
        )
        .map(|(name, judgment)| {
            (
                BoundPair::Subset {
                    matching: Bound::Exact(0),
                    total: 2,
                },
                judgment,
                Unit::Neighbor(name),
            )
        }),
    ))
    .parse_next(input)
}

fn bounded_judged_unit(input: &mut &[&str]) -> ModalResult<(Bound, Judgment, Unit)> {
    alt((
        (bound, word(judgment_any), unit_prepositional),
        (
            bound,
            word(judgment_any),
            profession_any.map(Unit::Profession),
        ),
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
fn bound_pair(input: &mut &[&str]) -> ModalResult<BoundPair> {
    alt((
        word("both").value(BoundPair::Subset {
            matching: Bound::Exact(2),
            total: 2,
        }),
        (
            word("neither"),
            opt((
                word("of"),
                opt(words((alt((determiner, pronoun_possessive_singular)), "2"))),
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
                    word(pronoun_possessive_singular).void(),
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

fn bound(input: &mut &[&str]) -> ModalResult<Bound> {
    alt((
        word("no").value(Bound::Exact(0)),
        terminated(word(number), words(("or", "more"))).map(Bound::AtLeast),
        delimited(
            opt(word(alt(("exactly", "only")))),
            word(number).map(Bound::Exact),
            opt(word("of")),
        ),
        preceded(words(("at", "least")), word(number)).map(Bound::AtLeast),
        delimited(word("an"), word(parity), words(("number", "of"))).map(Bound::Parity),
        word("multiple").value(Bound::AtLeast(2)),
        preceded(words(("more", "than")), word(number))
            .map(|number| Bound::AtLeast(number.strict_add(1))),
        preceded(words(("fewer", "than")), word(number))
            .verify_map(|number| number.checked_sub(1).map(Bound::AtMost)),
    ))
    .parse_next(input)
}

fn number(input: &mut &str) -> ModalResult<Number> {
    alt((
        dec_uint,
        "none".value(0),
        "one".value(1),
        "two".value(2),
        "zero".value(0),
    ))
    .parse_next(input)
}

fn parity(input: &mut &str) -> ModalResult<Parity> {
    alt(("even".value(Parity::Even), "odd".value(Parity::Odd))).parse_next(input)
}

fn a_judgment(input: &mut &[&str]) -> ModalResult<Judgment> {
    alt((
        words(("an", "innocent")).value(Judgment::Innocent),
        words(("a", "criminal")).value(Judgment::Criminal),
    ))
    .parse_next(input)
}

fn is_judgment_any(input: &mut &[&str]) -> ModalResult<Judgment> {
    alt((
        preceded(word("is"), judgment_predicate_singular),
        preceded(word("are"), word(judgment_adjective)),
    ))
    .parse_next(input)
}

fn judgment_predicate_singular(input: &mut &[&str]) -> ModalResult<Judgment> {
    alt((
        word("innocent").value(Judgment::Innocent),
        words(("a", "criminal")).value(Judgment::Criminal),
        words(("not", "innocent")).value(Judgment::Criminal),
        words(("not", "a", "criminal")).value(Judgment::Innocent),
    ))
    .parse_next(input)
}

fn judgment_predicate_comma(input: &mut &[&str]) -> ModalResult<Judgment> {
    alt((
        word_or_comma_terminated("innocent").value(Judgment::Innocent),
        (word("a"), word_or_comma_terminated("criminal")).value(Judgment::Criminal),
        (word("not"), word_or_comma_terminated("innocent")).value(Judgment::Criminal),
        (words(("not", "a")), word_or_comma_terminated("criminal")).value(Judgment::Innocent),
    ))
    .parse_next(input)
}

fn judgment_any(input: &mut &str) -> ModalResult<Judgment> {
    alt((judgment_plural, judgment_singular)).parse_next(input)
}

fn judgment_plural(input: &mut &str) -> ModalResult<Judgment> {
    alt((
        "innocents".value(Judgment::Innocent),
        "criminals".value(Judgment::Criminal),
    ))
    .parse_next(input)
}

use judgment_adjective as judgment_singular;

fn judgment_adjective(input: &mut &str) -> ModalResult<Judgment> {
    alt((
        "innocent".value(Judgment::Innocent),
        "criminal".value(Judgment::Criminal),
    ))
    .parse_next(input)
}

fn name_possessive(input: &mut &str) -> ModalResult<NameRecipe> {
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

fn name_has(input: &mut &[&str]) -> ModalResult<NameRecipe> {
    alt((
        words(("I", "have")).value(NameRecipe::Me),
        terminated(word(raw_name), word("has")).map(NameRecipe::from),
    ))
    .parse_next(input)
}

fn name_doesnt_have(input: &mut &[&str]) -> ModalResult<NameRecipe> {
    alt((
        words(("I", "don't", "have")).value(NameRecipe::Me),
        terminated(word(raw_name), words(("doesn't", "have"))).map(NameRecipe::from),
    ))
    .parse_next(input)
}

fn name_is(input: &mut &[&str]) -> ModalResult<NameRecipe> {
    alt((
        word("I'm").value(NameRecipe::Me),
        words(("I", "am")).value(NameRecipe::Me),
        terminated(word(raw_name), word("is")).map(NameRecipe::from),
    ))
    .parse_next(input)
}

fn is_name(input: &mut &[&str]) -> ModalResult<NameRecipe> {
    alt((
        words(("am", "I")).value(NameRecipe::Me),
        preceded(word("is"), word(raw_name)).map(NameRecipe::from),
    ))
    .parse_next(input)
}

fn name_subject(input: &mut &str) -> ModalResult<NameRecipe> {
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

fn name_object(input: &mut &str) -> ModalResult<NameRecipe> {
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

fn raw_name<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    rest.verify(|s: &str| s.chars().next().is_some_and(char::is_uppercase))
        .parse_next(input)
}

fn pronoun_object<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    alt(("him", "her", "them", "me", "us")).parse_next(input)
}

fn pronoun_object_singular<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    alt(("him", "her", "them", "me")).parse_next(input)
}

fn pair_bounded_profession(input: &mut &[&str]) -> ModalResult<(BoundPair, Profession)> {
    separated_pair(bound_pair, opt(word(determiner)), profession_any).parse_next(input)
}

fn pair_bounded_unit(input: &mut &[&str]) -> ModalResult<(BoundPair, Unit)> {
    separated_pair(bound_pair, opt(word(determiner)), unit_subject).parse_next(input)
}

fn direction(input: &mut &[&str]) -> ModalResult<Direction> {
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

fn directly_direction(input: &mut &[&str]) -> ModalResult<Direction> {
    preceded(word("directly"), direction).parse_next(input)
}

fn indefinite_article<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    alt(("a", "an")).parse_next(input)
}

fn pronoun_possessive_singular<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    alt(("her", "his", "my", "their")).parse_next(input)
}

fn determiner<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    alt(("the", "us")).parse_next(input)
}

fn profession_any(input: &mut &[&str]) -> ModalResult<Profession> {
    alt((
        word(profession_plural),
        preceded(opt(word(indefinite_article)), word(profession_singular)),
    ))
    .parse_next(input)
}

fn profession_singular(input: &mut &str) -> ModalResult<Profession> {
    rest.verify_map(Profession::from_singular).parse_next(input)
}

fn profession_plural(input: &mut &str) -> ModalResult<Profession> {
    rest.verify_map(Profession::from_plural).parse_next(input)
}

fn neighbor_any(input: &mut &str) -> ModalResult<()> {
    alt(("neighbors", "neighbor")).void().parse_next(input)
}

fn not_neighbor_any(input: &mut &[&str]) -> ModalResult<()> {
    words((alt(("doesn't", "don't")), "neighbor"))
        .void()
        .parse_next(input)
}

fn there_is<'input, 'inner: 'input>(
    input: &mut &'input [&'inner str],
) -> ModalResult<&'input [&'inner str]> {
    alt((
        words(("There", be_verb_third_person)).take(),
        word("There's").take(),
    ))
    .parse_next(input)
}

fn has_have<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    alt(("has", "have")).parse_next(input)
}

fn neighboring_verb<'input, 'inner: 'input>(
    input: &mut &'input [&'inner str],
) -> ModalResult<&'input [&'inner str]> {
    alt((
        (opt(word("also")), word("neighbor")).take(),
        (word(be_verb_third_person), word("neighboring")).take(),
    ))
    .parse_next(input)
}

fn neighboring_predicate<'input, 'inner: 'input>(
    input: &mut &'input [&'inner str],
) -> ModalResult<&'input [&'inner str]> {
    alt((
        words(("who", "neighbors")).take(),
        words(("who", "neighbor")).take(),
        word("neighboring").take(),
    ))
    .parse_next(input)
}

fn be_verb_third_person<'input>(input: &mut &'input str) -> ModalResult<&'input str> {
    alt(("is", "are")).parse_next(input)
}

fn more_or_less(input: &mut &str) -> ModalResult<MoreOrLess> {
    alt((
        "more".value(MoreOrLess::More),
        "less".value(MoreOrLess::Less),
        "fewer".value(MoreOrLess::Less),
    ))
    .parse_next(input)
}

fn any_all(input: &mut &str) -> ModalResult<AnyAll> {
    alt(("all".value(AnyAll::All), "any".value(AnyAll::Any))).parse_next(input)
}

fn disjoint_series(input: &mut &str) -> ModalResult<DisjointSeries> {
    alt((
        line_kind.map(DisjointSeries::from),
        "profession".value(DisjointSeries::Profession),
    ))
    .parse_next(input)
}

fn line(input: &mut &[&str]) -> ModalResult<LineRecipe> {
    alt((row.map(LineRecipe::Row), column.map(LineRecipe::Column))).parse_next(input)
}

fn line_kind(input: &mut &str) -> ModalResult<LineKind> {
    alt(("row".value(LineKind::Row), "column".value(LineKind::Column))).parse_next(input)
}

fn line_pair(input: &mut &[&str]) -> ModalResult<[LineRecipe; 2]> {
    alt((
        preceded(word("rows"), pair(row_bare, "and"))
            .map(|rows| rows.map(RowRecipe::Explicit).map(LineRecipe::Row)),
        preceded(word("columns"), pair(column_bare, "and"))
            .map(|cols| cols.map(ColumnRecipe::Explicit).map(LineRecipe::Column)),
    ))
    .parse_next(input)
}

fn row(input: &mut &[&str]) -> ModalResult<RowRecipe> {
    alt((
        preceded(word("row"), word(row_bare)).map(RowRecipe::Explicit),
        words(("my", "row")).value(RowRecipe::Me),
    ))
    .parse_next(input)
}

fn row_bare(input: &mut &str) -> ModalResult<Row> {
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

fn column(input: &mut &[&str]) -> ModalResult<ColumnRecipe> {
    alt((
        preceded(word("column"), word(column_bare)).map(ColumnRecipe::Explicit),
        words(("my", "column")).value(ColumnRecipe::Me),
    ))
    .parse_next(input)
}

fn column_bare(input: &mut &str) -> ModalResult<Column> {
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

fn word_or_comma_terminated<
    'input,
    'inner: 'input,
    O,
    E: ParserError<&'input [&'inner str]> + ParserError<&'inner str>,
>(
    inner: impl Parser<&'inner str, O, E>,
) -> impl Parser<&'input [&'inner str], O, E> {
    any.and_then(terminated(inner, (opt(','), eof)))
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
