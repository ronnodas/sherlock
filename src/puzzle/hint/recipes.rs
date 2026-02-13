use anyhow::Result;

use crate::puzzle::Name;
use crate::puzzle::grid::{Coordinate, Grid};

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
}

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone)]
pub(crate) enum NameRecipe {
    Me,
    Other(Name),
}

impl From<&str> for NameRecipe {
    fn from(v: &str) -> Self {
        Self::Other(v.to_owned())
    }
}

impl AddContext for &NameRecipe {
    type Output = Coordinate;

    fn add_context(self, context: Context<'_>) -> Result<Self::Output> {
        let name = match self {
            NameRecipe::Me => context.speaker,
            NameRecipe::Other(name) => name,
        };
        context.grid.coord(name)
    }
}
