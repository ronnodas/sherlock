mod parsers;

use std::collections::HashSet;

use anyhow::Result;
use itertools::Itertools as _;
use mitsein::vec1::{Vec1, vec1};

use super::{Column, Coordinate, Judgment, Name, Row, Solution};
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
    And(Vec1<Self>),
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
            Self::And(vec1![self, other])
        }
    }

    pub(crate) fn contains(&self, coord: Coordinate, solution: &Solution) -> bool {
        match self {
            &Self::Judgment(judgment) => solution[coord.to_index()] == judgment,
            &Self::Row(row) => coord.row == row,
            &Self::Column(column) => coord.col == column,
            Self::And(sets) => sets.iter().all(|set| set.contains(coord, solution)),
        }
    }

    pub(crate) fn all_members(&self, solution: &[Judgment; 20]) -> HashSet<Coordinate> {
        match self {
            Self::Judgment(target) => solution
                .iter()
                .positions(|judgment| judgment == target)
                .map(Coordinate::from_index)
                .collect(),
            &Self::Row(row) => Coordinate::row_all(row).collect(),
            &Self::Column(column) => Coordinate::column_all(column).collect(),
            Self::And(sets) => sets
                .iter1()
                .map(|set| set.all_members(solution))
                .reduce(|a, b| a.intersection(&b).copied().collect()),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Quantity {
    Exact(usize),
}

impl Quantity {
    pub(crate) const fn matches(&self, len: usize) -> bool {
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
