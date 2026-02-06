mod card;
mod hint;

use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use anyhow::{Result, bail};
use inquire::Editor;
use itertools::Itertools as _;
use select::document::Document;
use select::predicate::{Attr, Predicate as _};

use crate::html::{Class, ClassName, Div, NodeExt as _};

use card::Card;
use hint::Hint;

type Name = String;

#[derive(Clone)]
pub(crate) struct Puzzle {
    grid: Grid,
    hints: Vec<Hint>,
}

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
            .try_collect()?;

        Ok(Self { grid, hints })
    }

    fn format(self) -> String {
        todo!()
    }
}

#[derive(Clone)]
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

#[derive(Clone, Copy)]
enum Judgment {
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

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
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
