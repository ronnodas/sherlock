mod card;
mod hint;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::iter::{repeat, successors};
use std::ops::{Index, IndexMut};

use anyhow::{Result, anyhow, bail};
use inquire::Editor;
use itertools::Itertools as _;
use mitsein::NonEmpty;
use mitsein::hash_set1::HashSet1;
use select::document::Document;
use select::predicate::{Attr, Predicate as _};

use crate::html::{Class, ClassName, Div, NodeExt as _};
use crate::solver::hint::{Direction, HintRecipe, Recipe as _, Set};

use card::Card;
use hint::Hint;

type Name = String;
type Profession = String;

#[derive(Clone, Debug)]
pub(crate) struct Puzzle {
    grid: Grid,
    hints: Vec<Hint>,
    solutions: Vec<Solution>,
}

type Solution = [Judgment; 20];

impl Puzzle {
    pub(crate) fn prompt() -> Result<Self> {
        // TODO allow manual entry
        loop {
            let html = Editor::new("Enter html:").prompt()?;
            match Self::parse(&html) {
                Ok(puzzle) => return Ok(puzzle),
                Err(e) => eprintln!("{e}"),
            }
        }
    }

    pub(crate) fn parse(html: &str) -> Result<Self> {
        let grid = Grid::parse(html)?;
        Self::new(grid)
    }

    fn new(grid: Grid) -> Result<Self> {
        let hints: Vec<(Name, HintRecipe)> = grid
            .iter()
            .filter_map(|card| Some((card.name().clone(), card.hint()?)))
            .map(|(name, hint)| HintRecipe::parse(hint).map(|hints| repeat(name).zip(hints)))
            .flatten_ok()
            .try_collect()?;

        let mut puzzle = Self {
            grid,
            hints: Vec::new(),
            solutions: Vec::new(),
        };

        hints
            .into_iter()
            .try_for_each(|(speaker, hint)| puzzle.add_parsed_hint(hint, &speaker))?;

        Ok(puzzle)
    }

    fn validate(&self, solution: &Solution) -> bool {
        self.hints.iter().all(|hint| hint.evaluate(solution))
    }

    pub(crate) fn solved(&self) -> bool {
        self.grid.solved()
    }

    pub(crate) fn infer(&mut self) -> Result<Vec<(Name, Judgment)>> {
        let (first, rest): (Solution, &[Solution]) = loop {
            // TODO move to initialization (and then make the type Vec1?)
            if let Some((&first, rest)) = self.solutions.split_first() {
                break (first, rest);
            }
            let old = self.grid.fixed();
            let fixed_values = old
                .iter()
                .enumerate()
                .filter_map(|(index, &judgment)| Some((index, judgment?)));
            self.solutions = SolutionIterator::new(fixed_values)
                .filter(|solution| self.validate(solution))
                .collect();
            if self.solutions.is_empty() {
                bail!("no solutions!")
            }
        };

        let mut fixed = first.map(Some);
        for solution in rest {
            for i in 0..20 {
                let fixed = &mut fixed[i];
                if let Some(val) = *fixed
                    && val != solution[i]
                {
                    *fixed = None;
                }
            }
        }
        Ok(fixed
            .into_iter()
            .enumerate()
            .filter_map(|(index, fixed)| {
                let fixed = fixed?;
                let name = self.grid.set_new(index, fixed)?.name().to_owned();
                Some((name, fixed))
            })
            .collect())
    }

    pub(crate) fn add_hint(&mut self, hint: &str, speaker: &Name) -> Result<()> {
        HintRecipe::parse(hint)?
            .into_iter()
            .try_for_each(|hint| self.add_parsed_hint(hint, speaker))
    }

    fn add_parsed_hint(&mut self, hint: HintRecipe, speaker: &Name) -> Result<()> {
        let hint = hint.contextualize(&self.grid, speaker)?;
        self.solutions.retain(|solution| hint.evaluate(solution));
        self.hints.push(hint);
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct Grid {
    cards: [Card; 20],
    coordinates: HashMap<Name, Coordinate>,
    by_profession: HashMap<Profession, NonEmpty<Set>>,
}

impl Grid {
    fn parse(html: &str) -> Result<Self> {
        let html = Document::from(html);
        let Ok(cards) = html
            .find(Div.and(Class(ClassName::CardGrid)).and(Attr("id", "grid")))
            .exactly_one()
        else {
            bail!("expecting unique element");
        };
        let cards: [Card; 20] = cards
            .expect_children::<20>()?
            .iter()
            .map(|card| Card::parse(card))
            .collect::<Result<Vec<Card>>>()?
            .try_into()
            .unwrap_or_else(|_| unreachable!());
        let coordinates = cards
            .iter()
            .enumerate()
            .map(|(index, card)| (card.name().to_owned(), Coordinate::from_index(index)))
            .collect();
        let by_profession = cards
            .iter()
            .enumerate()
            .map(|(index, card)| (card.profession().to_owned(), Coordinate::from_index(index)))
            .into_grouping_map()
            .aggregate(|set: Option<NonEmpty<Set>>, _, item| {
                let set = set.map_or_else(
                    || HashSet1::from_one(item),
                    |mut set| {
                        _ = set.insert(item);
                        set
                    },
                );
                Some(set)
            });
        Ok(Self {
            cards,
            coordinates,
            by_profession,
        })
    }

    fn iter(&self) -> impl Iterator<Item = &Card> {
        self.cards.iter()
    }

    fn coord(&self, name: &Name) -> Result<Coordinate> {
        self.coordinates
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("{name} not in grid"))
    }

    fn solved(&self) -> bool {
        self.cards.iter().all(Card::is_judged)
    }

    fn fixed(&self) -> [Option<Judgment>; 20] {
        self.cards.each_ref().map(Card::status)
    }

    fn set_new(&mut self, index: usize, judgment: Judgment) -> Option<&Card> {
        self.cards[index].set(judgment)
    }

    fn by_profession(&self, profession: &Profession) -> Result<&NonEmpty<Set>> {
        self.by_profession
            .get(profession)
            .ok_or_else(|| anyhow!("{profession} not in grid"))
    }
}

impl Index<Coordinate> for Grid {
    type Output = Card;

    fn index(&self, index: Coordinate) -> &Card {
        &self.cards[index.to_index()]
    }
}

impl IndexMut<Coordinate> for Grid {
    fn index_mut(&mut self, index: Coordinate) -> &mut Card {
        &mut self.cards[index.to_index()]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Judgment {
    Innocent,
    Criminal,
}

impl Judgment {
    const fn class(self) -> ClassName {
        match self {
            Self::Innocent => ClassName::Innocent,
            Self::Criminal => ClassName::Criminal,
        }
    }

    fn filter(self, set: &Set, solution: &Solution) -> impl Iterator<Item = Coordinate> {
        set.iter()
            .filter(move |coord| solution[coord.to_index()] == self)
            .copied()
    }
}

impl fmt::Display for Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Innocent => write!(f, "Innocent"),
            Self::Criminal => write!(f, "Criminal"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(crate) struct Coordinate {
    row: Row,
    col: Column,
}

impl Coordinate {
    fn from_index(index: usize) -> Self {
        Self {
            row: Row::from_index(index / 4),
            col: Column::from_index(index % 4),
        }
    }

    const fn to_index(self) -> usize {
        4 * self.row.to_index() + self.col.to_index()
    }

    fn row_all(row: Row) -> impl Iterator<Item = Self> {
        Column::ALL.into_iter().map(move |col| Self { row, col })
    }

    fn column_all(col: Column) -> impl Iterator<Item = Self> {
        Row::ALL.into_iter().map(move |row| Self { row, col })
    }

    fn connected(set: &HashSet<Self>) -> bool {
        if set.len() == 1 {
            return true;
        }
        let Some(&start) = set.iter().next() else {
            return true;
        };
        let mut seen = HashSet::new();
        let mut frontier = vec![start];
        while let Some(node) = frontier.pop() {
            if !seen.insert(node) {
                continue;
            }
            frontier.extend(
                Direction::ALL
                    .into_iter()
                    .filter_map(|dir| node.step(dir))
                    .filter(|node| set.contains(node)),
            );
        }
        seen.len() == set.len()
    }

    fn step(self, direction: Direction) -> Option<Self> {
        let coord = match direction {
            Direction::Above => Self {
                row: self.row.prev()?,
                col: self.col,
            },
            Direction::Below => Self {
                row: self.row.next()?,
                col: self.col,
            },
            Direction::Left => Self {
                row: self.row,
                col: self.col.prev()?,
            },
            Direction::Right => Self {
                row: self.row,
                col: self.col.next()?,
            },
        };
        Some(coord)
    }

    fn direction(start: Self, direction: Direction) -> impl Iterator<Item = Self> {
        successors(start.step(direction), move |coord| coord.step(direction))
    }

    fn neighbors(center: Self) -> impl Iterator<Item = Self> {
        use Direction::{Above, Below, Left, Right};
        [center.step(Above), center.step(Below)]
            .into_iter()
            .flatten()
            .flat_map(|vert| [Some(vert), vert.step(Right), vert.step(Left)])
            .chain([center.step(Left), center.step(Right)])
            .flatten()
    }

    fn edges() -> impl Iterator<Item = Self> {
        [Column::A, Column::D]
            .into_iter()
            .cartesian_product(Row::ALL)
            .chain(
                [Column::B, Column::C]
                    .into_iter()
                    .cartesian_product([Row::One, Row::Five]),
            )
            .map(|(col, row)| Self { row, col })
    }

    fn corners() -> impl Iterator<Item = Self> {
        [Row::One, Row::Five]
            .into_iter()
            .cartesian_product([Column::A, Column::B])
            .map(|(row, col)| Self { row, col })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum Row {
    One,
    Two,
    Three,
    Four,
    Five,
}

impl Row {
    const ALL: [Self; 5] = [Self::One, Self::Two, Self::Three, Self::Four, Self::Five];
    fn from_index(index: usize) -> Self {
        match index {
            0 => Self::One,
            1 => Self::Two,
            2 => Self::Three,
            3 => Self::Four,
            4 => Self::Five,
            5.. => unreachable!(),
        }
    }

    const fn to_index(self) -> usize {
        match self {
            Self::One => 0,
            Self::Two => 1,
            Self::Three => 2,
            Self::Four => 3,
            Self::Five => 4,
        }
    }

    const fn prev(self) -> Option<Self> {
        match self {
            Self::One => None,
            Self::Two => Some(Self::One),
            Self::Three => Some(Self::Two),
            Self::Four => Some(Self::Three),
            Self::Five => Some(Self::Four),
        }
    }

    const fn next(self) -> Option<Self> {
        match self {
            Self::One => Some(Self::Two),
            Self::Two => Some(Self::Three),
            Self::Three => Some(Self::Four),
            Self::Four => Some(Self::Five),
            Self::Five => None,
        }
    }

    fn others(&self) -> impl Iterator<Item = Self> {
        Self::ALL.into_iter().filter(move |other| other != self)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
enum Column {
    A,
    B,
    C,
    D,
}

impl Column {
    const ALL: [Self; 4] = [Self::A, Self::B, Self::C, Self::D];

    fn from_index(index: usize) -> Self {
        match index {
            0 => Self::A,
            1 => Self::B,
            2 => Self::C,
            3 => Self::D,
            4.. => unreachable!(),
        }
    }

    const fn to_index(self) -> usize {
        match self {
            Self::A => 0,
            Self::B => 1,
            Self::C => 2,
            Self::D => 3,
        }
    }

    const fn prev(self) -> Option<Self> {
        match self {
            Self::A => None,
            Self::B => Some(Self::A),
            Self::C => Some(Self::B),
            Self::D => Some(Self::C),
        }
    }

    const fn next(self) -> Option<Self> {
        match self {
            Self::A => Some(Self::B),
            Self::B => Some(Self::C),
            Self::C => Some(Self::D),
            Self::D => None,
        }
    }

    fn others(&self) -> impl Iterator<Item = Self> {
        Self::ALL.into_iter().filter(move |other| other != self)
    }
}

struct SolutionIterator {
    counter: u32,
    template: [Judgment; 20],
    free_indices: Vec<usize>,
}

impl SolutionIterator {
    fn new(fixed_values: impl IntoIterator<Item = (usize, Judgment)>) -> Self {
        let mut template = [Judgment::Innocent; 20];
        let mut fixed_mask = [false; 20];

        for (idx, val) in fixed_values {
            template[idx] = val;
            fixed_mask[idx] = true;
        }

        let free_indices: Vec<usize> = (0..20).filter(|i| !fixed_mask[*i]).collect();

        Self {
            counter: 0,
            template,
            free_indices,
        }
    }

    const fn max_counter(&self) -> u32 {
        1_u32 << self.free_indices.len()
    }
}

impl Iterator for SolutionIterator {
    type Item = Solution;

    fn next(&mut self) -> Option<Self::Item> {
        if self.counter >= self.max_counter() {
            return None;
        }

        let mut current = self.template;

        for (bit_pos, &target_idx) in self.free_indices.iter().enumerate() {
            // Check if the nth bit of the counter is set
            if (self.counter >> bit_pos) & 1 == 1 {
                current[target_idx] = Judgment::Criminal;
            } else {
                current[target_idx] = Judgment::Innocent;
            }
        }

        self.counter += 1;
        Some(current)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.max_counter() - self.counter)
            .try_into()
            .map_or((usize::MAX, None), |remaining| (remaining, Some(remaining)))
    }
}
