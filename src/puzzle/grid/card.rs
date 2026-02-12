use anyhow::{Context as _, Ok, Result, bail};
use itertools::Itertools as _;
use select::node::Node;
use select::predicate::Predicate as _;
use serde::{Deserialize, Serialize};

use crate::puzzle::{Judgment, Name, Profession};

use super::html::{Class, ClassName, Div, H3, NodeExt as _, Paragraph};

// TODO change types so that hint can only exist if status is Some
#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct Card {
    name: Name,
    profession: Profession,
    #[serde(skip_serializing_if = "HintText::is_unknown")]
    hint: HintText,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    status: Option<Judgment>,
}

impl Card {
    pub(crate) fn parse(node: &Node<'_>) -> Result<(Self, bool)> {
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
        status
            .map_or_else(
                || Self::parse_unflipped(card),
                |status| Self::parse_flipped(card, status),
            )
            .map(|card| (card, node.is(Class(ClassName::HasHint))))
    }

    fn parse_flipped(card: Node<'_>, status: Judgment) -> Result<Self> {
        let card = card
            .expect(Class(status.into()))
            .context("`.card-back` should be consistent with `.card`")?;

        let name = parse_name(card)?;
        let profession = parse_profession(card)?;
        let hint = HintText::Known(parse_hint(card)?);
        Ok(Self {
            name,
            profession,
            hint,
            status: Some(status),
        })
    }

    fn parse_unflipped(card: Node<'_>) -> Result<Self> {
        let name = parse_name(card)?;
        let profession = parse_profession(card)?;
        Ok(Self {
            name,
            profession,
            hint: HintText::Unknown,
            status: None,
        })
    }

    pub(crate) fn hint(&self) -> Option<&str> {
        self.hint.as_known()
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

    pub(crate) fn set_hint(&mut self, hint: String) {
        self.hint = HintText::Known(hint);
    }

    pub(crate) fn mark_as_flavor(&mut self) {
        self.hint = HintText::Flavor;
    }

    pub(crate) const fn hint_pending(&self) -> bool {
        self.status.is_some() && self.hint.is_unknown()
    }
}

#[derive(Clone, Debug, Deserialize, Default)]
#[serde(from = "Option<String>")]
enum HintText {
    #[default]
    Unknown,
    Flavor,
    Known(String),
}

impl HintText {
    #[must_use]
    fn as_known(&self) -> Option<&str> {
        if let Self::Known(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the hint text is [`Unknown`].
    ///
    /// [`Unknown`]: HintText::Unknown
    #[must_use]
    const fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }
}

impl From<Option<String>> for HintText {
    fn from(value: Option<String>) -> Self {
        match value {
            Some(string) if string == "Flavor" => Self::Flavor,
            Some(string) => Self::Known(string),
            None => Self::Unknown,
        }
    }
}

impl Serialize for HintText {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let option = match self {
            Self::Unknown => None,
            Self::Flavor => Some("Flavor"),
            Self::Known(hint) => Some(hint.as_str()),
        };
        option.serialize(serializer)
    }
}

fn parse_hint(card: Node<'_>) -> Result<String> {
    card.unique_child(Paragraph.and(Class(ClassName::Hint)))
        .context("`.card-back` should have a unique `p .hint`")?
        .expect_text()
        .map(str::to_owned)
}

fn parse_profession(card: Node<'_>) -> Result<String> {
    card.unique_child(Paragraph.and(Class(ClassName::Profession)))
        .context("`.card-{back,front}` should have a unique `p .profession`")?
        .expect_text()
        .map(str::to_owned)
}

fn parse_name(card: Node<'_>) -> Result<String> {
    let [name] = card
        .expect_children(Div.and(Class(ClassName::Name)))
        .context("`.card-{back,front}` should have a unique `div .name`")?;
    let name = name
        .unique_child(H3.and(Class(ClassName::Name)))
        .context("`div .name` should have a unique `h3 .name`")?
        .expect_text()?;
    // emulating `text-transform: capitalize`
    Ok(name
        .chars()
        .with_position()
        .map(|(position, c)| match position {
            itertools::Position::First | itertools::Position::Only => c.to_ascii_uppercase(),
            itertools::Position::Middle | itertools::Position::Last => c,
        })
        .collect())
}
