use std::fmt;

use anyhow::{Context as _, Ok, Result, anyhow, bail};
use itertools::Itertools as _;
use select::node::Node;
use select::predicate::{self, Name, Predicate};

use crate::models::{CardBack, CardFront, Judgment, MaybeHint};

pub(crate) fn parse_card(node: &Node<'_>) -> Result<(CardFront, Option<CardBack>, bool)> {
    let node = node
        .expect(Div)?
        .unique_child(Div.and(Class(ClassName::Card)))?;
    let status = if node.is(Class(ClassName::Flipped)) {
        if node.is(Class(ClassName::Innocent)) {
            Some(Judgment::Innocent)
        } else if node.is(Class(ClassName::Criminal)) {
            Some(Judgment::Criminal)
        } else {
            bail!("expecting either `.innocent` or `.criminal`")
        }
    } else {
        None
    };
    // TODO validate coord
    let [card] = if status.is_some() {
        node.expect_children(Div.and(Class(ClassName::CardBack)))
    } else {
        node.expect_children(Div.and(Class(ClassName::CardFront)))
    }
    .context("inside a `.card`")?;
    let name = parse_name(card)?;
    let profession = parse_profession(card)?;
    let back = status
        .map(|judgment| parse_back(card, judgment))
        .transpose()?;
    let has_hint = node.is(Class(ClassName::HasHint));
    let front = CardFront { name, profession };
    Ok((front, back, has_hint))
}

fn parse_back(card: Node<'_>, judgment: Judgment) -> Result<CardBack> {
    let card = card
        .expect(Class(judgment.into()))
        .context("`.card-back` should be consistent with `.card`")?;

    let hint = MaybeHint::Logical(parse_hint(card)?);
    Ok(CardBack::with_hint(judgment, hint))
}

fn parse_hint(card: Node<'_>) -> Result<String> {
    let hint = card
        .unique_child(Paragraph.and(Class(ClassName::Hint)))
        .with_context(|| format!("`.card-back` should have a unique `p .hint`: {card:?}"))?;
    Ok(hint.text().trim().to_owned())
}

fn parse_profession(card: Node<'_>) -> Result<String> {
    let profession = card
        .unique_child(Paragraph.and(Class(ClassName::Profession)))
        .with_context(|| {
            format!("`.card-{{back,front}}` should have a unique `p .profession`: {card:?}")
        })?;
    Ok(profession.text().trim().to_owned())
}

fn parse_name(card: Node<'_>) -> Result<String> {
    let [name] = card
        .expect_children(Div.and(Class(ClassName::Name)))
        .context("`.card-{back,front}` should have a unique `div .name`")?;
    let name = name
        .unique_child(H3.and(Class(ClassName::Name)))
        .context("`div .name` should have a unique `h3 .name`")?
        .text();
    // emulating `text-transform: capitalize`
    Ok(name
        .trim()
        .chars()
        .with_position()
        .map(|(position, c)| {
            if position.is_first {
                c.to_ascii_uppercase()
            } else {
                c
            }
        })
        .collect())
}

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
struct Paragraph;

impl Predicate for Paragraph {
    fn matches(&self, node: &Node) -> bool {
        Name("p").matches(node)
    }
}

#[derive(Clone, Copy, Debug)]
struct H3;

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
