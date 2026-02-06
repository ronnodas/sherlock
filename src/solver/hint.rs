mod parsers;

use anyhow::Result;

use super::{Column, Judgment, Name, Row};
use parsers::Sentence;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Hint {
    Member(Name, Set),
    Count(Set, Quantity),
}

impl Hint {
    pub(crate) fn parse(hint: &str) -> Result<Vec<Self>> {
        Ok(Sentence::parse(hint)?.collate())
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Set {
    Judgment(Judgment),
    Row(Row),
    Column(Column),
    And(Vec<Self>),
}

impl Set {
    fn and(mut self, mut other: Self) -> Self {
        if let Self::And(vec) = &mut self {
            vec.push(other);
            self
        } else if let Self::And(vec) = &mut other {
            vec.push(self);
            other
        } else {
            Self::And(vec![self, other])
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Quantity {
    Exact(usize),
}

impl Quantity {
    pub(crate) fn matches(&self, len: usize) -> bool {
        match self {
            &Self::Exact(value) => len == value,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Hint, Quantity, Set};
    use crate::solver::{Judgment, Row};

    #[test]
    fn sample_26_02_05_alice() {
        assert_eq!(
            Hint::parse("Tina is one of 3 criminals in row\u{A0}4").unwrap(),
            [
                Hint::Member(
                    "Tina".to_owned(),
                    Set::Judgment(Judgment::Criminal).and(Set::Row(Row::Four))
                ),
                Hint::Count(
                    Set::Judgment(Judgment::Criminal).and(Set::Row(Row::Four)),
                    Quantity::Exact(3)
                )
            ]
        );
    }
}
