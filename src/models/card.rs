use std::fmt;
use std::ops::Not;

use colored::Color;
use serde::{Deserialize, Serialize};

use crate::models::{Coord, HintText, JudgedCard};
use crate::solver::Suspect;

// TODO force non-empty
pub(crate) type Name = String;
pub(crate) type Profession = String;

#[derive(Clone, Debug)]
pub(crate) struct SolveCard {
    name: Name,
    profession: Profession,
    back: Option<CardBack>,
}

impl SolveCard {
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
                hint: MaybeHint::Unknown,
            });
            &*self
        })
    }

    pub(crate) fn hint_pending(&self, coord: Coord) -> Option<Suspect> {
        self.back
            .as_ref()?
            .hint_pending()
            .map(|judgment| Suspect::new(coord, self.name().clone(), judgment))
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

    pub(crate) fn emoji(&self) -> char {
        match self.judgment() {
            Some(Judgment::Innocent) => '🟩',
            Some(Judgment::Criminal) => '🟥',
            None => '⬛',
        }
    }

    pub(crate) fn judged(self) -> Option<JudgedCard> {
        Some(JudgedCard::new(self.name, self.profession, self.back?))
    }
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
    pub(crate) fn mark_as_flavor(&mut self) {
        self.hint = MaybeHint::Flavor;
    }

    pub(crate) fn set_hint(&mut self, hint: String) {
        self.hint = MaybeHint::Logical(hint);
    }

    pub(crate) fn new(judgment: Judgment, hint: MaybeHint) -> Self {
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

    fn hint_pending(&self) -> Option<Judgment> {
        self.hint.is_unknown().then_some(self.judgment)
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
