mod card;
mod hint;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::iter::successors;
use std::ops::{Index, IndexMut};

use anyhow::{Result, anyhow, bail};
use inquire::Editor;
use itertools::Itertools as _;
use mitsein::iter1::IntoIterator1 as _;
use select::document::Document;
use select::predicate::{Attr, Predicate as _};

use crate::html::{Class, ClassName, Div, NodeExt as _};
use crate::solver::hint::{Direction, HintRecipe, Set, SetRecipe};

use card::Card;
use hint::Hint;

type Name = String;

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
                Ok(grid) => return Ok(grid),
                Err(e) => eprintln!("{e}"),
            }
        }
    }

    pub(crate) fn parse(html: &str) -> Result<Self> {
        let grid = Grid::parse(html)?;
        Self::new(grid)
    }

    fn new(grid: Grid) -> Result<Self> {
        let hints: Vec<HintRecipe> = grid
            .iter()
            .filter_map(|card| card.hint())
            .map(HintRecipe::parse)
            .flatten_ok()
            .try_collect()?;

        let mut puzzle = Self {
            grid,
            hints: Vec::new(),
            solutions: Vec::new(),
        };

        hints
            .into_iter()
            .try_for_each(|hint| puzzle.add_parsed_hint(hint))?;

        Ok(puzzle)
    }

    fn validate(&self, solution: &Solution) -> bool {
        self.hints.iter().all(|hint| hint.evaluate(solution))
    }

    fn format(self) -> String {
        unimplemented!()
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

    pub(crate) fn add_hint(&mut self, hint: &str) -> Result<()> {
        HintRecipe::parse(hint)?
            .into_iter()
            .try_for_each(|hint| self.add_parsed_hint(hint))
    }

    fn add_parsed_hint(&mut self, hint: HintRecipe) -> Result<()> {
        let hint = hint.contextualize(&self.grid)?;
        self.solutions.retain(|solution| hint.evaluate(solution));
        self.hints.push(hint);
        Ok(())
    }
}

#[derive(Clone, Debug)]
struct Grid {
    cards: [Card; 20],
    coordinates: HashMap<Name, Coordinate>,
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
        Ok(Self { cards, coordinates })
    }

    fn iter(&self) -> impl Iterator<Item = &Card> {
        self.cards.iter()
    }

    fn coord(&self, name: &Name) -> Option<Coordinate> {
        self.coordinates.get(name).copied()
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
struct Coordinate {
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
        todo!()
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

trait Recipe {
    type Output;

    fn contextualize(self, grid: &Grid) -> Result<Self::Output>;
}

impl Recipe for SetRecipe {
    type Output = Set;

    fn contextualize(self, grid: &Grid) -> Result<Self::Output> {
        let set = match self {
            Self::Judgment(judgment) => Set::Judgment(judgment),
            Self::Row(row) => Set::Coord(Coordinate::row_all(row).collect()),
            Self::Column(column) => Set::Coord(Coordinate::column_all(column).collect()),
            Self::Direction(name, direction) => {
                let start = grid.coord(&name).ok_or_else(|| not_in_grid(&name))?;
                Set::Coord(Coordinate::direction(start, direction).collect())
            }
            Self::And(recipes) => {
                let sets = recipes
                    .into_iter1()
                    .map(|recipe| recipe.contextualize(grid))
                    .coalesce(|a, b| match (a, b) {
                        (e @ Err(_), _) | (_, e @ Err(_)) => Ok(e),
                        (Ok(Set::Coord(a)), Ok(Set::Coord(b))) => {
                            Ok(Ok(Set::Coord(a.union(&b).copied().collect())))
                        }
                        (Ok(b @ Set::Coord(_)), Ok(a)) | (Ok(a), Ok(b)) => Err((Ok(a), Ok(b))),
                    })
                    .collect1::<Result<_>>()?;
                Set::And(sets)
            }
        };
        Ok(set)
    }
}

impl Recipe for HintRecipe {
    type Output = Hint;

    fn contextualize(self, grid: &Grid) -> Result<Self::Output> {
        let hint = match self {
            Self::Member(name, set) => {
                let coordinate = grid.coord(&name).ok_or_else(|| not_in_grid(&name))?;
                Hint::Member(coordinate, set.contextualize(grid)?)
            }
            Self::Count(set, quantity) => Hint::Count(set.contextualize(grid)?, quantity),
            Self::Connected(set) => Hint::Connected(set.contextualize(grid)?),
        };
        Ok(hint)
    }
}

fn not_in_grid(name: &Name) -> anyhow::Error {
    anyhow!("{name} not in grid")
}
