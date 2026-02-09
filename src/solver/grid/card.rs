use anyhow::{Ok, Result, bail};
use itertools::Itertools as _;
use select::node::Node;
use select::predicate::Predicate as _;

use crate::solver::{Judgment, Name, Profession};

use super::html::{Class, ClassName, Div, H3, NodeExt as _, Paragraph};

#[derive(Clone, Debug)]
pub(crate) struct Card {
    name: Name,
    profession: Profession,
    hint: Option<String>,
    status: Option<Judgment>,
}

impl Card {
    pub(crate) fn parse(node: &Node<'_>) -> Result<Self> {
        let node = node
            .expect(Div.and(Class(ClassName::CardContainer)))?
            .unique_child()?
            .expect(Div.and(Class(ClassName::Card)))?;
        let status = if node.is(Class(ClassName::Flipped)) {
            if node.is(Class(ClassName::Innocent)) {
                Some(Judgment::Innocent)
            } else if node.is(Class(ClassName::Criminal)) {
                Some(Judgment::Criminal)
            } else {
                bail!("expecting either `.innocent` or `.criminal`")
            }
        } else if node.is(Class(ClassName::Unflipped)) {
            None
        } else {
            bail!("expecting either `.flipped` or `.unflipped`");
        };
        let has_hint = node.is(Class(ClassName::HasHint));
        // TODO validate coord
        let [_coord, card, _inspect, _aria] = node.expect_children()?;
        status.map_or_else(
            || Self::parse_unflipped(card),
            |status| Self::parse_flipped(card, status, has_hint),
        )
    }

    fn parse_flipped(card: Node<'_>, status: Judgment, has_hint: bool) -> Result<Self> {
        let card = card.expect(
            Div.and(Class(ClassName::CardBack))
                .and(Class(status.into())),
        )?;
        let [name, profession, hint] =
            card.expect_children()
                .map(|[_face, name, profession, _aria, hint]| [name, profession, hint])
                .or_else(|_| {
                    card.expect_children().map(
                        |[_correct, _face, name, profession, _aria, hint]| [name, profession, hint],
                    )
                })?;
        let name = parse_name(name)?;
        let profession = parse_profession(profession)?;
        let hint = if has_hint {
            Some(parse_hint(hint)?)
        } else {
            None
        };
        Ok(Self {
            name,
            profession,
            hint,
            status: Some(status),
        })
    }

    fn parse_unflipped(card: Node<'_>) -> Result<Self> {
        let [_face, name, profession] = card
            .expect(Div.and(Class(ClassName::CardFront)))?
            .expect_children()?;
        let name = parse_name(name)?;
        let profession = parse_profession(profession)?;
        Ok(Self {
            name,
            profession,
            hint: None,
            status: None,
        })
    }

    pub(crate) fn hint(&self) -> Option<&str> {
        self.hint.as_deref()
    }

    pub(crate) const fn name(&self) -> &Name {
        &self.name
    }

    pub(crate) const fn profession(&self) -> &Profession {
        &self.profession
    }

    pub(crate) const fn is_judged(&self) -> bool {
        self.status.is_some()
    }

    pub(crate) const fn status(&self) -> Option<Judgment> {
        self.status
    }

    pub(crate) fn set(&mut self, judgment: Judgment) -> Option<&Self> {
        (self.status != Some(judgment)).then(|| {
            self.status = Some(judgment);
            &*self
        })
    }
}

fn parse_hint(hint: Node<'_>) -> Result<String> {
    hint.expect(Paragraph.and(Class(ClassName::Hint)))?
        .expect_text()
        .map(str::to_owned)
}

fn parse_profession(profession: Node<'_>) -> Result<String> {
    profession
        .expect(Paragraph.and(Class(ClassName::Profession)))?
        .expect_text()
        .map(str::to_owned)
}

fn parse_name(name: Node<'_>) -> Result<String> {
    name.expect(Div.and(Class(ClassName::Name)))?
        .unique_child()?
        .expect(H3.and(Class(ClassName::Name)))?
        .expect_text()
        .map(|name| {
            name.chars()
                .with_position()
                .map(|(position, c)| match position {
                    itertools::Position::First | itertools::Position::Only => {
                        c.to_ascii_uppercase()
                    }
                    itertools::Position::Middle | itertools::Position::Last => c,
                })
                .collect()
        })
}
