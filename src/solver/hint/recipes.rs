use anyhow::Result;

use crate::models::{Column, Coordinate, Name, Row};
use crate::solver::grid::Grid;
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
    pub grid: &'ctx Grid,
    pub speaker: &'ctx Name,
}

impl<'ctx> Context<'ctx> {
    pub(crate) fn new(grid: &'ctx Grid, speaker: &'ctx Name) -> Self {
        Self { grid, speaker }
    }

    fn speaker_coord(&self) -> Result<Coordinate> {
        self.grid.coord(self.speaker)
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
    type Output = Coordinate;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let name = match self {
            NameRecipe::Me => context.speaker,
            NameRecipe::Explicit(name) => name,
        };
        context.grid.coord(name)
    }
}

impl AddContext for RowRecipe {
    type Output = Row;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let row = match self {
            Self::Me => context.speaker_coord()?.row,
            Self::Explicit(row) => row,
        };
        Ok(row)
    }
}

impl AddContext for ColumnRecipe {
    type Output = Column;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let col = match self {
            Self::Me => context.speaker_coord()?.col,
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
