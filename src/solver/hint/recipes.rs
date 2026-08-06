use std::collections::HashMap;

use anyhow::{Result, anyhow};
use mitsein::btree_map1::BTreeMap1;
use mitsein::iter1::IteratorExt as _;
use mitsein::vec1::Vec1;

use crate::models::{Column, Coord, Name, Profession, Row};
use crate::solver::board::Board;
use crate::solver::board::coordinates::Set1;
use crate::solver::hint::{Line, LineKind};

pub(crate) type NameRecipe = MeOrExplicit<Name>;
pub(crate) type RowRecipe = MeOrExplicit<Row>;
pub(crate) type ColumnRecipe = MeOrExplicit<Column>;

pub(crate) trait AddContext {
    type Output;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output>;
}

#[derive(Clone, Copy)]
pub(crate) struct Context<'ctx> {
    pub coordinates: &'ctx HashMap<Name, Coord>,
    pub by_profession: &'ctx BTreeMap1<Profession, Set1>,
    pub speaker: Coord,
}

impl<'ctx> Context<'ctx> {
    pub(crate) fn new<C>(board: &'ctx Board<C>, speaker: Coord) -> Self {
        Self {
            coordinates: board.coordinates(),
            by_profession: board.by_profession(),
            speaker,
        }
    }

    fn coord(&self, name: &str) -> Result<Coord> {
        self.coordinates
            .get(name)
            .copied()
            .ok_or_else(|| anyhow!("{name} not in puzzle"))
    }

    pub(crate) fn profession_as_set(&self, profession: &Profession) -> Result<&Set1> {
        self.by_profession
            .get(profession)
            .ok_or_else(|| anyhow!("{profession} not in puzzle"))
    }

    pub(crate) fn other_professions(&self, profession: &str) -> Result<Vec1<Set1>> {
        self.by_profession
            .as_btree_map()
            .iter()
            .filter(move |&(other, _)| other != profession)
            .map(|(_, &set)| set)
            .try_collect1()
            .map_err(|_empty| anyhow!("only {profession}s in puzzle"))
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone, Copy)]
pub(crate) enum MeOrExplicit<T> {
    Me,
    Explicit(T),
}

impl From<&str> for NameRecipe {
    fn from(v: &str) -> Self {
        Self::Explicit(v.to_owned())
    }
}

impl AddContext for &NameRecipe {
    type Output = Coord;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        match self {
            NameRecipe::Me => Ok(context.speaker),
            NameRecipe::Explicit(name) => context.coord(name),
        }
    }
}

impl AddContext for RowRecipe {
    type Output = Row;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let row = match self {
            Self::Me => context.speaker.row,
            Self::Explicit(row) => row,
        };
        Ok(row)
    }
}

impl AddContext for ColumnRecipe {
    type Output = Column;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let col = match self {
            Self::Me => context.speaker.col,
            Self::Explicit(col) => col,
        };
        Ok(col)
    }
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Clone, Copy, Debug)]
pub(crate) enum LineRecipe {
    Row(RowRecipe),
    Column(ColumnRecipe),
}

impl LineRecipe {
    pub(crate) fn kind(self) -> LineKind {
        match self {
            Self::Row(_) => LineKind::Row,
            Self::Column(_) => LineKind::Column,
        }
    }
}

impl AddContext for LineRecipe {
    type Output = Line;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        match self {
            Self::Row(row) => row.add_context(context).map(Line::Row),
            Self::Column(column) => column.add_context(context).map(Line::Column),
        }
    }
}
