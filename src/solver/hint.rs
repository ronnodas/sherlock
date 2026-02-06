use anyhow::Result;

use super::Name;

#[derive(Clone)]
pub(crate) enum Hint {
    Member(Name, Set),
    Count(Set, Quantity),
}

impl Hint {
    pub(crate) fn parse(hint: &str) -> Result<Vec<Self>> {
        todo!()
    }
}

#[derive(Clone)]
pub(crate) enum Set {}

#[derive(Clone)]
pub(crate) enum Quantity {}
