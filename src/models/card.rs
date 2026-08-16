use std::fmt;
use std::ops::Not;
use std::rc::Rc;

use colored::Color;
use serde::{Deserialize, Serialize};

use crate::models::HintText;

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(transparent)]
pub(crate) struct Name(Rc<str>);

impl Name {
    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for Name {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<&str> for Name {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

// TODO Maybe should be `Rc<Str1>`, and always construct through `from_singular()`?
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[serde(transparent)]
pub(crate) struct Profession(Rc<str>);

impl Profession {
    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }

    pub(crate) fn from_singular(value: &str) -> Option<Self> {
        value
            .chars()
            .next()
            .is_some_and(char::is_lowercase)
            .then(|| Self(value.into()))
    }

    pub(crate) fn from_plural(value: &str) -> Option<Self> {
        Self::from_singular(value.strip_suffix('s')?)
    }
}

impl fmt::Display for Profession {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for Profession {
    fn from(value: String) -> Self {
        Self(value.into())
    }
}

impl From<&str> for Profession {
    fn from(value: &str) -> Self {
        Self(value.into())
    }
}

#[derive(Clone, Debug)]
pub(crate) struct CardFront {
    pub name: Name,
    pub profession: Profession,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub(crate) enum Judgment {
    Innocent,
    Criminal,
}

impl Judgment {
    pub(crate) fn color(self) -> Color {
        match self {
            Self::Innocent => Color::Green,
            Self::Criminal => Color::Red,
        }
    }

    pub(crate) fn emoji(self) -> char {
        match self {
            Self::Innocent => '🟩',
            Self::Criminal => '🟥',
        }
    }
}

impl Not for Judgment {
    type Output = Self;

    fn not(self) -> Self {
        match self {
            Self::Innocent => Self::Criminal,
            Self::Criminal => Self::Innocent,
        }
    }
}

impl fmt::Display for Judgment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Innocent => write!(f, "Innocent"),
            Self::Criminal => write!(f, "Criminal"),
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct CardBack {
    judgment: Judgment,
    #[serde(skip_serializing_if = "MaybeHint::is_unknown", default)]
    hint: MaybeHint,
}

impl CardBack {
    pub(crate) fn new(judgment: Judgment) -> Self {
        Self {
            judgment,
            hint: MaybeHint::Unknown,
        }
    }

    pub(crate) fn mark_as_flavor(&mut self) {
        self.hint = MaybeHint::Flavor;
    }

    pub(crate) fn set_hint(&mut self, hint: String) {
        self.hint = MaybeHint::Logical(hint);
    }

    pub(crate) fn with_hint(judgment: Judgment, hint: MaybeHint) -> Self {
        Self { judgment, hint }
    }

    pub(crate) fn judgment(&self) -> Judgment {
        self.judgment
    }

    pub(crate) fn hint(&self) -> &MaybeHint {
        &self.hint
    }

    pub(crate) fn set_judgment(&mut self, judgment: Judgment) {
        self.judgment = judgment;
    }

    pub(crate) fn hint_pending(&self) -> Option<Judgment> {
        self.hint.is_unknown().then_some(self.judgment)
    }

    pub(crate) fn logical_hint(&self) -> Option<&str> {
        self.hint.as_logical()
    }
}

#[derive(Clone, Debug, Deserialize, Default)]
#[serde(from = "Option<String>")]
pub(crate) enum MaybeHint {
    #[default]
    Unknown,
    Flavor,
    Logical(String),
}

impl MaybeHint {
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

    #[must_use]
    pub(crate) fn is_logical(&self) -> bool {
        matches!(self, Self::Logical(_))
    }

    pub(crate) fn known(&self) -> Option<HintText> {
        match self {
            Self::Unknown => None,
            Self::Flavor => Some(HintText::Flavor),
            Self::Logical(hint) => Some(HintText::Logical(hint.clone())),
        }
    }
}

impl From<Option<String>> for MaybeHint {
    fn from(value: Option<String>) -> Self {
        match value {
            Some(string) if string == "Flavor" => Self::Flavor,
            Some(string) => Self::Logical(string),
            None => Self::Unknown,
        }
    }
}

impl From<HintText> for MaybeHint {
    fn from(value: HintText) -> Self {
        match value {
            HintText::Flavor => Self::Flavor,
            HintText::Logical(string) => Self::Logical(string),
        }
    }
}

impl Serialize for MaybeHint {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let option = match self {
            Self::Unknown => None,
            Self::Flavor => Some("Flavor"),
            Self::Logical(hint) => Some(hint.as_str()),
        };
        option.serialize(serializer)
    }
}
