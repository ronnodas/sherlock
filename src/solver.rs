mod card;
mod hint;

use std::collections::{HashMap, HashSet};
use std::fmt;
use std::iter::zip;
use std::ops::{Index, IndexMut};

use anyhow::{Result, anyhow, bail};
use inquire::Editor;
use itertools::Itertools as _;
use select::document::Document;
use select::predicate::{Attr, Predicate as _};

use crate::html::{Class, ClassName, Div, NodeExt as _};

use card::Card;
use hint::{Hint, Set};

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
        let hints = grid
            .iter()
            .filter_map(|card| card.hint())
            .map(Hint::parse)
            .flatten_ok()
            .map(|hint| grid.verify_context(hint?))
            .try_collect()?;

        Ok(Self {
            grid,
            hints,
            solutions: Vec::new(),
        })
    }

    fn validate(&self, solution: &Solution) -> bool {
        self.hints.iter().all(|hint| self.evaluate(hint, solution))
    }

    fn format(self) -> String {
        unimplemented!()
    }

    fn evaluate(&self, hint: &Hint, solution: &Solution) -> bool {
        match hint {
            Hint::Member(name, set) => self.is_in(self.grid.coord(name), set),
            Hint::Count(set, quantity) => quantity.matches(self.all_members(set).len()),
        }
    }

    fn is_in(&self, coord: Coordinate, set: &Set) -> bool {
        unimplemented!()
    }

    fn all_members(&self, set: &Set) -> HashSet<Coordinate> {
        unimplemented!()
    }

    pub(crate) fn solved(&self) -> bool {
        self.grid.solved()
    }

    pub(crate) fn infer(&mut self) -> Vec<(Name, Judgment)> {
        let (first, rest): (Solution, &[Solution]) = loop {
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
        fixed
            .into_iter()
            .enumerate()
            .filter_map(|(index, fixed)| {
                let fixed = fixed?;
                let name = self.grid.set_new(index, fixed)?.name().to_owned();
                Some((name, fixed))
            })
            .collect()
    }

    pub(crate) fn add_hint(&self, hint: String) -> Result<()> {
        unimplemented!()
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

    fn verify_context(&self, hint: Hint) -> Result<Hint> {
        let issue = match &hint {
            Hint::Member(name, _) => {
                (!self.coordinates.contains_key(name)).then_some(name.to_owned())
            }
            Hint::Count(..) => None,
        };
        issue.map_or(Ok(hint), |name| Err(anyhow!("{name} is unknown")))
    }

    fn coord(&self, name: &str) -> Coordinate {
        self.coordinates[name]
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

#[derive(Clone, Copy, Debug)]
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
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Row {
    One,
    Two,
    Three,
    Four,
    Five,
}

impl Row {
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
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Column {
    A,
    B,
    C,
    D,
}

impl Column {
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

    fn max_counter(&self) -> u32 {
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
