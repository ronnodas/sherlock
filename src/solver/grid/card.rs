use anyhow::{Context as _, Ok, Result, bail};
use itertools::Itertools as _;
use select::node::Node;
use select::predicate::Predicate as _;
use serde::{Deserialize, Serialize};

use crate::solver::grid::coordinate::Coordinate;
use crate::solver::grid::html::{Class, ClassName, Div, H3, NodeExt as _, Paragraph};
use crate::solver::{Judgment, Name, Profession, Suspect};

#[derive(Clone, Debug)]
pub(crate) struct Card {
    name: Name,
    profession: Profession,
    back: Option<CardBack>,
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
        let name = parse_name(card)?;
        let profession = parse_profession(card)?;
        let back = status
            .map(|judgment| Self::parse_back(card, judgment))
            .transpose()?;
        let has_hint = node.is(Class(ClassName::HasHint));
        let card = Self {
            name,
            profession,
            back,
        };
        Ok((card, has_hint))
    }

    fn parse_back(card: Node<'_>, judgment: Judgment) -> Result<CardBack> {
        let card = card
            .expect(Class(judgment.into()))
            .context("`.card-back` should be consistent with `.card`")?;

        let hint = HintText::Logical(parse_hint(card)?);
        Ok(CardBack { judgment, hint })
    }

    pub(crate) fn logical_hint(&self) -> Option<&str> {
        self.back.as_ref()?.hint().as_logical()
    }

    pub(crate) fn name(&self) -> &Name {
        &self.name
    }

    pub(crate) fn profession(&self) -> &Profession {
        &self.profession
    }

    pub(crate) fn flipped(&self) -> bool {
        self.back.is_some()
    }

    pub(crate) fn back_mut(&mut self) -> Option<&mut CardBack> {
        self.back.as_mut()
    }

    pub(crate) fn judgment(&self) -> Option<Judgment> {
        self.back.as_ref().map(|back| back.judgment)
    }

    pub(crate) fn reveal(&mut self, judgment: Judgment) -> Option<&Self> {
        (self.back.is_none()).then(|| {
            self.back = Some(CardBack {
                judgment,
                hint: HintText::Unknown,
            });
            &*self
        })
    }

    pub(crate) fn hint_pending(&self) -> Option<Judgment> {
        self.back.as_ref()?.hint_pending()
    }

    pub(crate) fn back(&self) -> Option<&CardBack> {
        self.back.as_ref()
    }

    pub(crate) fn into_parts(self) -> (Name, Profession, Option<CardBack>) {
        (self.name, self.profession, self.back)
    }

    pub(crate) fn new(name: String, profession: String, back: Option<CardBack>) -> Self {
        Self {
            name,
            profession,
            back,
        }
    }

    pub(crate) fn to_suspect(&self, coord: Coordinate) -> Option<Suspect> {
        self.hint_pending().map(|judgment| Suspect {
            name: self.name.clone(),
            judgment,
            coord,
        })
    }

    pub(crate) fn emoji(&self) -> char {
        match self.judgment() {
            Some(Judgment::Innocent) => '🟩',
            Some(Judgment::Criminal) => '🟥',
            None => '⬛',
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct CardBack {
    judgment: Judgment,
    #[serde(skip_serializing_if = "HintText::is_unknown", default)]
    hint: HintText,
}

impl CardBack {
    pub(crate) fn mark_as_flavor(&mut self) {
        self.hint = HintText::Flavor;
    }

    pub(crate) fn set_hint(&mut self, hint: String) {
        self.hint = HintText::Logical(hint);
    }

    pub(crate) fn new(judgment: Judgment, hint: HintText) -> Self {
        Self { judgment, hint }
    }

    pub(crate) fn judgment(&self) -> Judgment {
        self.judgment
    }

    pub(crate) fn hint(&self) -> &HintText {
        &self.hint
    }

    pub(crate) fn set_judgment(&mut self, judgment: Judgment) {
        self.judgment = judgment;
    }

    fn hint_pending(&self) -> Option<Judgment> {
        self.hint.is_unknown().then_some(self.judgment)
    }
}

#[derive(Clone, Debug, Deserialize, Default)]
#[serde(from = "Option<String>")]
pub(crate) enum HintText {
    #[default]
    Unknown,
    Flavor,
    Logical(String),
}

impl HintText {
    #[must_use]
    pub(crate) fn as_logical(&self) -> Option<&str> {
        if let Self::Logical(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the hint text is [`Unknown`].
    ///
    /// [`Unknown`]: HintText::Unknown
    #[must_use]
    pub(crate) fn is_unknown(&self) -> bool {
        matches!(self, Self::Unknown)
    }

    #[must_use]
    pub(crate) fn is_flavor(&self) -> bool {
        matches!(self, Self::Flavor)
    }
}

impl From<Option<String>> for HintText {
    fn from(value: Option<String>) -> Self {
        match value {
            Some(string) if string == "Flavor" => Self::Flavor,
            Some(string) => Self::Logical(string),
            None => Self::Unknown,
        }
    }
}

impl Serialize for HintText {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let option = match self {
            Self::Unknown => None,
            Self::Flavor => Some("Flavor"),
            Self::Logical(hint) => Some(hint.as_str()),
        };
        option.serialize(serializer)
    }
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
