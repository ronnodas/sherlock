use std::fmt;

use anyhow::{Result, anyhow};
use itertools::Itertools as _;
use select::node::Node;
use select::predicate::{self, Name, Predicate};

use crate::puzzle::Judgment;

pub(crate) trait NodeExt<'html>: Sized + fmt::Debug {
    fn is(&self, predicate: impl Predicate) -> bool;
    fn children(&self) -> impl Iterator<Item = Node<'html>>;
    fn attrs(&self) -> impl Iterator<Item = (&'html str, &'html str)>;
    fn name(&self) -> Option<&'html str>;

    fn expect(self, predicate: impl Predicate + fmt::Debug + Copy) -> Result<Self> {
        if self.is(predicate) {
            Ok(self)
        } else {
            Err(anyhow!(
                "expecting {predicate:?}, found name: {:?}, attrs: {:?}",
                self.name(),
                self.attrs().collect_vec()
            ))
        }
    }

    fn expect_children<const N: usize>(
        &self,
        predicate: impl Predicate + Copy,
    ) -> Result<[Node<'html>; N]> {
        let children = self
            .children()
            .filter(|child| child.name().is_some() && child.is(predicate))
            .collect_vec();
        children.try_into().map_err(|children: Vec<Node<'_>>| {
            anyhow!("expecting {N} children, found {}", children.len())
        })
    }

    fn unique_child(&self, predicate: impl Predicate + Copy) -> Result<Node<'html>> {
        let [child] = self.expect_children(predicate)?;
        Ok(child)
    }
}

impl<'html> NodeExt<'html> for Node<'html> {
    fn is(&self, predicate: impl Predicate) -> bool {
        self.is(predicate)
    }

    fn children(&self) -> impl Iterator<Item = Node<'html>> {
        self.children()
    }

    fn attrs(&self) -> impl Iterator<Item = (&'html str, &'html str)> {
        self.attrs()
    }

    fn name(&self) -> Option<&'html str> {
        self.name()
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Div;

impl Predicate for Div {
    fn matches(&self, node: &Node) -> bool {
        Name("div").matches(node)
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Paragraph;

impl Predicate for Paragraph {
    fn matches(&self, node: &Node) -> bool {
        Name("p").matches(node)
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct H3;

impl Predicate for H3 {
    fn matches(&self, node: &Node) -> bool {
        Name("h3").matches(node)
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Class(pub ClassName);

impl Predicate for Class {
    fn matches(&self, node: &Node) -> bool {
        predicate::Class(self.0.as_str()).matches(node)
    }
}

#[derive(Clone, Copy, Debug)]
pub(crate) enum ClassName {
    Card,
    CardBack,
    CardFront,
    CardGrid,
    Criminal,
    Flipped,
    HasHint,
    Hint,
    Innocent,
    Name,
    Profession,
}

impl ClassName {
    fn as_str(self) -> &'static str {
        match self {
            Self::Card => "card",
            Self::CardBack => "card-back",
            Self::CardFront => "card-front",
            Self::CardGrid => "card-grid",
            Self::Criminal => "criminal",
            Self::Flipped => "flipped",
            Self::HasHint => "has-hint",
            Self::Hint => "hint",
            Self::Innocent => "innocent",
            Self::Name => "name",
            Self::Profession => "profession",
        }
    }
}

impl From<Judgment> for ClassName {
    fn from(judgment: Judgment) -> Self {
        match judgment {
            Judgment::Innocent => Self::Innocent,
            Judgment::Criminal => Self::Criminal,
        }
    }
}
