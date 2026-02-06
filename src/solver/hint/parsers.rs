use anyhow::anyhow;
use nom::branch::alt;
use nom::bytes::complete::{tag, take_while};
use nom::character::complete::u8;
use nom::character::{multispace0, satisfy};
use nom::combinator::{map, recognize};
use nom::error::Error;
use nom::sequence::pair;
use nom::{IResult, Parser as _};

use crate::solver::{Column, Judgment, Name, Row};

use super::{Hint, Quantity, Set};

type SResult<'str, O> = IResult<&'str str, O>;

#[derive(PartialEq, Eq, Debug)]
pub(super) enum Sentence {
    IsOneOf(Name, Quantity, Set),
}

impl Sentence {
    pub(super) fn parse(hint: &str) -> anyhow::Result<Self> {
        let (rest, sentence) = sentence
            .parse_complete(hint)
            .map_err(nom::Err::<Error<&str>>::to_owned)?;
        if rest.is_empty() {
            Ok(sentence)
        } else {
            Err(anyhow!(r#"cannot parse "{hint}""#))
        }
    }

    pub(super) fn collate(self) -> Vec<Hint> {
        match self {
            Self::IsOneOf(name, quantity, set) => {
                vec![Hint::Member(name, set.clone()), Hint::Count(set, quantity)]
            }
        }
    }
}

pub(super) fn sentence(input: &str) -> SResult<'_, Sentence> {
    alt((is_one_of,)).parse(input)
}

fn is_one_of(input: &str) -> SResult<'_, Sentence> {
    map(
        (name, tag(" is one of "), quantity, multispace0(), set),
        |(name, _, quantity, _, set)| Sentence::IsOneOf(name, quantity, set),
    )
    .parse(input)
}

fn name(input: &str) -> SResult<'_, String> {
    map(
        recognize(pair(
            satisfy(|c| c.is_ascii_uppercase()),
            take_while(|c: char| c.is_ascii() && !c.is_ascii_whitespace()),
        )),
        str::to_owned,
    )
    .parse(input)
}

fn quantity(input: &str) -> SResult<'_, Quantity> {
    alt((map(u8, |count: u8| Quantity::Exact(count.into())),)).parse(input)
}

fn set(input: &str) -> SResult<'_, Set> {
    alt((
        map(
            (judgment_plural, tag(" in "), line),
            |(judgment, _, line)| Set::Judgment(judgment).and(line.into()),
        ),
        map(judgment_plural, Set::Judgment),
        map(row, Set::Row),
    ))
    .parse(input)
}

fn judgment_plural(input: &str) -> SResult<'_, Judgment> {
    alt((
        map(tag("innocents"), |_| Judgment::Innocent),
        map(tag("criminals"), |_| Judgment::Criminal),
    ))
    .parse(input)
}

enum Line {
    Row(Row),
    Column(Column),
}

impl From<Line> for Set {
    fn from(value: Line) -> Self {
        match value {
            Line::Row(row) => Self::Row(row),
            Line::Column(column) => Self::Column(column),
        }
    }
}

fn line(input: &str) -> SResult<'_, Line> {
    alt((map(row, Line::Row), map(column, Line::Column))).parse(input)
}

fn row(input: &str) -> SResult<'_, Row> {
    map(
        (
            tag("row\u{A0}"),
            alt((
                map(tag("1"), |_| Row::One),
                map(tag("2"), |_| Row::Two),
                map(tag("3"), |_| Row::Three),
                map(tag("4"), |_| Row::Four),
                map(tag("5"), |_| Row::Five),
            )),
        ),
        |(_, row)| row,
    )
    .parse(input)
}

fn column(input: &str) -> SResult<'_, Column> {
    map(
        (
            tag("column\u{A0}"),
            alt((
                map(tag("A"), |_| Column::A),
                map(tag("B"), |_| Column::B),
                map(tag("C"), |_| Column::C),
                map(tag("D"), |_| Column::D),
            )),
        ),
        |(_, column)| column,
    )
    .parse(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sample_26_02_05_alice() {
        let (remainder, sentence) = sentence("Tina is one of 3 criminals in row\u{A0}4").unwrap();
        assert_eq!(remainder, "");
        assert_eq!(
            sentence,
            Sentence::IsOneOf(
                "Tina".to_owned(),
                Quantity::Exact(3),
                Set::Judgment(Judgment::Criminal).and(Set::Row(Row::Four))
            )
        );
    }
}
