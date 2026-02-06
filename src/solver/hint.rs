mod parsers;

use std::collections::HashSet;

use anyhow::Result;
use itertools::Itertools as _;
use mitsein::vec1::{Vec1, vec1};

use super::{Column, Coordinate, Judgment, Name, Row, Solution};
use parsers::Sentence;

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Hint {
    Member(Coordinate, Set),
    Count(Set, Quantity),
    Connected(Set),
}

impl Hint {
    pub(crate) fn evaluate(&self, solution: &Solution) -> bool {
        match self {
            Self::Member(card, set) => set.contains(*card, solution),
            Self::Count(set, quantity) => quantity.matches(set.all_members(solution).len()),
            Self::Connected(set) => Coordinate::connected(&set.all_members(solution)),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum HintRecipe {
    Member(Name, SetRecipe),
    Count(SetRecipe, Quantity),
    Connected(SetRecipe),
}

impl HintRecipe {
    pub(crate) fn parse(hint: &str) -> Result<Vec<Self>> {
        Ok(Sentence::parse(hint)?.collate())
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum Set {
    Judgment(Judgment),
    Coord(HashSet<Coordinate>),
    And(Vec1<Self>),
}

impl Set {
    pub(crate) fn contains(&self, coord: Coordinate, solution: &Solution) -> bool {
        match self {
            &Self::Judgment(judgment) => solution[coord.to_index()] == judgment,
            Self::Coord(coordinates) => coordinates.contains(&coord),
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
            Self::Coord(set) => set.clone(),
            Self::And(sets) => sets
                .iter1()
                .map(|set| set.all_members(solution))
                .reduce(|a, b| a.intersection(&b).copied().collect()),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) enum SetRecipe {
    Judgment(Judgment),
    Row(Row),
    Column(Column),
    Direction(Name, Direction),
    And(Vec1<Self>),
}

impl SetRecipe {
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Direction {
    Above,
    Below,
    Left,
    Right,
}

#[cfg(test)]
mod tests {
    use super::{Direction, HintRecipe, Quantity, SetRecipe};
    use crate::solver::{Judgment, Row};

    #[test]
    fn sample_26_02_05_alice() {
        assert_eq!(
            HintRecipe::parse("Tina is one of 3 criminals in row\u{A0}4").unwrap(),
            [
                HintRecipe::Member(
                    "Tina".to_owned(),
                    SetRecipe::Judgment(Judgment::Criminal).and(SetRecipe::Row(Row::Four))
                ),
                HintRecipe::Count(
                    SetRecipe::Judgment(Judgment::Criminal).and(SetRecipe::Row(Row::Four)),
                    Quantity::Exact(3)
                )
            ]
        );
    }

    #[test]
    fn sample_26_02_05_tina() {
        assert_eq!(
            HintRecipe::parse("Both criminals above Xavi are connected").unwrap(),
            [
                HintRecipe::Count(
                    SetRecipe::Direction("Xavi".to_owned(), Direction::Above),
                    Quantity::Exact(2)
                ),
                HintRecipe::Connected(SetRecipe::Direction("Xavi".to_owned(), Direction::Above))
            ]
        );
    }
}
