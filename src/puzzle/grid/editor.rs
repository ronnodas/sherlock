use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::ops::{Index, IndexMut};

use std::fmt;

use anyhow::{Result, anyhow};
use colored::Colorize as _;
use inquire::{Autocomplete, Select, Text};
use itertools::Itertools as _;

use crate::puzzle::grid::card::{Card, CardBack, HintText};
use crate::puzzle::grid::{Coordinate, Format, Grid};
use crate::puzzle::{Judgment, Name, Profession};

pub(crate) struct GridEditor {
    cards: [CardEdit; 20],
    professions: BTreeSet<Profession>,
}

impl GridEditor {
    pub(crate) fn new() -> Self {
        Self {
            cards: <[CardEdit; 20]>::default(),
            professions: BTreeSet::new(),
        }
    }

    pub(crate) fn is_complete(&self) -> bool {
        self.cards
            .iter()
            .all(|card| matches!(card, CardEdit::Draft(..)))
            && self.cards.iter().any(|card| card.known_hint().is_some())
    }

    pub(crate) fn build(self) -> Result<Grid> {
        let cards: [Card; 20] = self
            .cards
            .into_iter()
            .filter_map(CardEdit::finalize)
            .collect_vec()
            .try_into()
            .map_err(|cards: Vec<_>| {
                anyhow!("grid is incomplete: only {}/20 cards defined", cards.len())
            })?;

        Ok(Grid::new(cards, Format::Sep2025, None))
    }

    pub(crate) fn interact(mut self) -> Result<Option<Grid>> {
        loop {
            self.print_grid();
            let mut options = vec![EditorOption::SelectCell, EditorOption::Quit];
            if self.is_complete() {
                options.insert(1, EditorOption::Play);
            }

            let selected = Select::new("Manual mode:", options).prompt_skippable()?;
            match selected {
                Some(EditorOption::SelectCell) => self.select_cell_and_edit()?,
                Some(EditorOption::Play) => return self.build().map(Some),
                Some(EditorOption::Quit) | None => return Ok(None),
            }
        }
    }

    fn render_grid(&self) -> String {
        use std::fmt::Write as _;
        let mut out = String::new();
        writeln!(out, "\n=== Grid Editor ===").expect("writing to a string should not fail");
        let coords = Coordinate::all().into_iter().collect_vec();
        let width = 35;
        for chunk in coords.chunks(4) {
            let cells = chunk
                .iter()
                .map(|&coord| self[coord].render(coord, width))
                .collect_vec();

            let max_lines = cells.iter().map(Vec::len).max().unwrap_or(0);
            for i in 0..max_lines {
                for cell in &cells {
                    let line = cell.get(i).map_or("", String::as_str);
                    write!(out, "{line:<width$}").expect("writing to a string should not fail");
                }
                out.push('\n');
            }
            out.push('\n');
        }
        out
    }

    fn print_grid(&self) {
        print!("{}", self.render_grid());
    }

    fn select_cell_and_edit(&mut self) -> Result<()> {
        let cells = Coordinate::all()
            .into_iter()
            .zip(&self.cards)
            .map(|(coord, edit)| CellOption { coord, edit })
            .sorted()
            .collect_vec();

        if let Some(cell) = Select::new("Select cell to edit:", cells).prompt_skippable()? {
            let mut coord = cell.coord;
            loop {
                let professions = ProfessionAutocomplete::new(&self.professions);
                let card = &mut self.cards[coord.to_index()];
                let update = card.edit(coord, professions)?;
                let profession = card.profession().cloned();
                self.professions.extend(profession);
                match update {
                    Some(scroll) => coord = scroll,
                    None => break,
                }
            }
        }
        Ok(())
    }
}

impl From<Grid> for GridEditor {
    fn from(grid: Grid) -> Self {
        let professions = grid.by_profession().keys().cloned().collect();
        Self {
            cards: grid.into_cards().map(CardEdit::from),
            professions,
        }
    }
}

impl Index<Coordinate> for GridEditor {
    type Output = CardEdit;

    fn index(&self, index: Coordinate) -> &CardEdit {
        &self.cards[index.to_index()]
    }
}

impl IndexMut<Coordinate> for GridEditor {
    fn index_mut(&mut self, index: Coordinate) -> &mut CardEdit {
        &mut self.cards[index.to_index()]
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) enum CardEdit {
    #[default]
    Empty,
    Draft(CardFront, Option<CardBack>),
}

impl CardEdit {
    pub(crate) fn judgment(&self) -> Option<Judgment> {
        match self {
            Self::Draft(_, Some(back)) => Some(back.judgment()),
            Self::Empty | Self::Draft(_, None) => None,
        }
    }

    pub(crate) fn known_hint(&self) -> Option<&str> {
        match self {
            Self::Draft(_, Some(back)) => back.hint().as_known(),
            Self::Empty | Self::Draft(_, None) => None,
        }
    }

    pub(crate) fn finalize(self) -> Option<Card> {
        match self {
            Self::Empty => None,
            Self::Draft(front, back) => Some(Card::new(front.name, front.profession, back)),
        }
    }

    fn edit(
        &mut self,
        coord: Coordinate,
        professions: ProfessionAutocomplete<'_>,
    ) -> Result<Option<Coordinate>> {
        let Self::Draft(front, back) = self else {
            let name = Text::new("Name:").prompt()?;
            let profession = Text::new("Profession:")
                .with_autocomplete(professions)
                .prompt()?;
            *self = Self::Draft(CardFront { name, profession }, None);
            return self.edit(coord, professions);
        };

        if back.is_none() {
            match front.edit_unflipped(coord, professions)? {
                UnflippedUpdate::None => {}
                UnflippedUpdate::Back(new_back) => *back = Some(new_back),
                UnflippedUpdate::Scroll(scroll) => return Ok(Some(scroll)),
            }
        }

        if let Some(b) = back {
            match front.edit_flipped(b, coord, professions)? {
                FlippedUpdate::None => {}
                FlippedUpdate::Unflip => *back = None,
                FlippedUpdate::Scroll(scroll) => return Ok(Some(scroll)),
            }
        }
        Ok(None)
    }

    fn render(&self, coord: Coordinate, width: usize) -> Vec<String> {
        match self {
            Self::Empty => vec![
                format!("{:<width$}", format!("[{coord}]"))
                    .dimmed()
                    .to_string(),
                format!("{:<width$}", "____").dimmed().to_string(),
                format!("{:<width$}", ""),
                format!("{:<width$}", ""),
            ],
            Self::Draft(front, _) => {
                let mut lines = vec![
                    format!("{:<width$}", format!("[{coord}]")),
                    format!("{:<width$}", front.name),
                    format!("{:<width$}", format!("({})", front.profession)),
                ];
                if let Some(j) = self.judgment() {
                    lines.push(
                        format!("{:<width$}", format!(" {j} "))
                            .on_color(j.color())
                            .white()
                            .bold()
                            .to_string(),
                    );
                }
                lines
            }
        }
    }

    fn profession(&self) -> Option<&Profession> {
        match self {
            Self::Empty => None,
            Self::Draft(front, _) => Some(&front.profession),
        }
    }
}

impl From<Card> for CardEdit {
    fn from(card: Card) -> Self {
        let (name, profession, back) = card.into_parts();
        Self::Draft(CardFront { name, profession }, back)
    }
}

#[derive(Clone, Debug)]
pub(crate) struct CardFront {
    name: Name,
    profession: Profession,
}

impl CardFront {
    fn edit_unflipped(
        &mut self,
        coord: Coordinate,
        professions: ProfessionAutocomplete<'_>,
    ) -> Result<UnflippedUpdate> {
        let options = UnflippedAction::options(self, coord);

        let Some(action) = Select::new("Edit cell:", options).prompt_skippable()? else {
            return Ok(UnflippedUpdate::None);
        };
        let update = match action {
            UnflippedAction::Common(common) => {
                let update = common.prompt(professions)?;
                self.handle(update);
                UnflippedUpdate::None
            }
            UnflippedAction::SetInnocent => {
                UnflippedUpdate::Back(CardBack::new(Judgment::Innocent, HintText::Unknown))
            }
            UnflippedAction::SetCriminal => {
                UnflippedUpdate::Back(CardBack::new(Judgment::Criminal, HintText::Unknown))
            }
            UnflippedAction::Scroll(scroll, _) => UnflippedUpdate::Scroll(scroll),
        };
        Ok(update)
    }

    fn edit_flipped(
        &mut self,
        back: &mut CardBack,
        coord: Coordinate,
        professions: ProfessionAutocomplete<'_>,
    ) -> Result<FlippedUpdate> {
        let options = FlippedAction::options(self, back, coord);

        if let Some(action) = Select::new("Edit cell:", options).prompt_skippable()? {
            match action {
                FlippedAction::Common(common) => {
                    let update = common.prompt(professions)?;
                    self.handle(update);
                }
                FlippedAction::ToggleJudgment(current) => {
                    back.set_judgment(!current);
                }
                FlippedAction::EditHint(current) => {
                    if let Some(hint) = Text::new("Hint:")
                        .with_initial_value(current.as_known().unwrap_or(""))
                        .prompt_skippable()?
                    {
                        back.set_hint(hint);
                    }
                }
                FlippedAction::MarkAsFlavor => {
                    back.mark_as_flavor();
                }
                FlippedAction::Unflip => return Ok(FlippedUpdate::Unflip),
                FlippedAction::Scroll(scroll, _) => return Ok(FlippedUpdate::Scroll(scroll)),
            }
        }
        Ok(FlippedUpdate::None)
    }

    fn handle(&mut self, update: CommonUpdate) {
        match update {
            CommonUpdate::Name(name) => self.name = name,
            CommonUpdate::Profession(profession) => self.profession = profession,
        }
    }
}

#[derive(Clone, Copy)]
enum EditorOption {
    SelectCell,
    Play,
    Quit,
}

impl fmt::Display for EditorOption {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SelectCell => write!(f, "Select cell"),
            Self::Play => write!(f, "Play"),
            Self::Quit => write!(f, "Quit"),
        }
    }
}

#[derive(Clone)]
enum CommonAction<'edit> {
    EditName(&'edit Name),
    EditProfession(&'edit Profession),
}

impl CommonAction<'_> {
    fn prompt(self, professions: ProfessionAutocomplete<'_>) -> Result<CommonUpdate> {
        let update = match self {
            Self::EditName(name) => {
                CommonUpdate::Name(Text::new("Name:").with_initial_value(name).prompt()?)
            }
            Self::EditProfession(profession) => CommonUpdate::Profession(
                Text::new("Profession:")
                    .with_initial_value(profession)
                    .with_autocomplete(professions)
                    .prompt()?,
            ),
        };
        Ok(update)
    }
}

impl fmt::Display for CommonAction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EditName(name) => write!(f, "{name}"),
            Self::EditProfession(profession) => write!(f, "{profession}"),
        }
    }
}

enum CommonUpdate {
    Name(Name),
    Profession(Profession),
}

#[derive(Clone)]
enum UnflippedAction<'edit> {
    Common(CommonAction<'edit>),
    SetInnocent,
    SetCriminal,
    Scroll(Coordinate, ScrollDirection),
}

impl<'edit> UnflippedAction<'edit> {
    fn options(front: &'edit CardFront, coord: Coordinate) -> Vec<Self> {
        let mut options = vec![
            UnflippedAction::Common(CommonAction::EditName(&front.name)),
            UnflippedAction::Common(CommonAction::EditProfession(&front.profession)),
            UnflippedAction::SetInnocent,
            UnflippedAction::SetCriminal,
        ];
        options.extend(
            ScrollDirection::ALL
                .into_iter()
                .filter_map(|dir| Some(Self::Scroll(dir.shift(coord)?, dir))),
        );
        options
    }
}

impl fmt::Display for UnflippedAction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Common(c) => write!(f, "{c}"),
            Self::SetInnocent => write!(f, "Mark as innocent"),
            Self::SetCriminal => write!(f, "Mark as criminal"),
            Self::Scroll(coord, dir) => write!(f, "{dir} {coord}"),
        }
    }
}

#[derive(Clone)]
enum FlippedAction<'edit> {
    Common(CommonAction<'edit>),
    ToggleJudgment(Judgment),
    EditHint(&'edit HintText),
    MarkAsFlavor,
    Unflip,
    Scroll(Coordinate, ScrollDirection),
}

impl<'edit> FlippedAction<'edit> {
    fn options(arg: &'edit CardFront, back: &'edit CardBack, coord: Coordinate) -> Vec<Self> {
        let mut options = vec![
            FlippedAction::Common(CommonAction::EditName(&arg.name)),
            FlippedAction::Common(CommonAction::EditProfession(&arg.profession)),
            FlippedAction::ToggleJudgment(back.judgment()),
            FlippedAction::EditHint(back.hint()),
        ];

        if !back.hint().is_flavor() {
            options.push(FlippedAction::MarkAsFlavor);
        }
        options.push(FlippedAction::Unflip);

        options.extend(
            ScrollDirection::ALL
                .into_iter()
                .filter_map(|dir| Some(FlippedAction::Scroll(dir.shift(coord)?, dir))),
        );
        options
    }
}

impl fmt::Display for FlippedAction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Common(c) => write!(f, "{c}"),
            Self::ToggleJudgment(j) => {
                write!(f, "{}", j.to_string().color(j.color()))
            }
            Self::EditHint(hint) => match hint {
                HintText::Known(s) => write!(f, "Hint: {s}"),
                HintText::Flavor => write!(f, "Hint: <flavor>"),
                HintText::Unknown => write!(f, "Add hint"),
            },
            Self::MarkAsFlavor => write!(f, "Mark hint as flavor text"),
            Self::Unflip => write!(f, "Unflip"),
            Self::Scroll(coord, dir) => write!(f, "{dir} {coord}"),
        }
    }
}

#[derive(Clone)]
struct CellOption<'edit> {
    coord: Coordinate,
    edit: &'edit CardEdit,
}

impl PartialEq for CellOption<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.coord == other.coord
            && matches!(self.edit, CardEdit::Empty) == matches!(other.edit, CardEdit::Empty)
    }
}

impl Eq for CellOption<'_> {}

impl Ord for CellOption<'_> {
    fn cmp(&self, other: &Self) -> Ordering {
        matches!(other.edit, CardEdit::Empty)
            .cmp(&matches!(self.edit, CardEdit::Empty))
            .then_with(|| self.coord.cmp(&other.coord))
    }
}

impl PartialOrd for CellOption<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for CellOption<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.edit {
            CardEdit::Empty => write!(f, "{}", self.coord),
            CardEdit::Draft(front, back) => {
                let label = format!("{} ({})", front.name, self.coord);
                if let Some(back) = back {
                    write!(f, "{}", label.color(back.judgment().color()))
                } else {
                    write!(f, "{label}")
                }
            }
        }
    }
}

enum UnflippedUpdate {
    None,
    Back(CardBack),
    Scroll(Coordinate),
}

enum FlippedUpdate {
    None,
    Unflip,
    Scroll(Coordinate),
}

#[derive(Clone, Copy)]
enum ScrollDirection {
    Next,
    Prev,
}

impl ScrollDirection {
    const ALL: [Self; 2] = [Self::Next, Self::Prev];

    fn shift(self, coord: Coordinate) -> Option<Coordinate> {
        match self {
            Self::Next => coord.next(),
            Self::Prev => coord.prev(),
        }
    }
}

impl fmt::Display for ScrollDirection {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Next => write!(f, ">"),
            Self::Prev => write!(f, "<"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct ProfessionAutocomplete<'edit> {
    professions: &'edit BTreeSet<Profession>,
}

impl<'edit> ProfessionAutocomplete<'edit> {
    fn new(professions: &'edit BTreeSet<Profession>) -> Self {
        Self { professions }
    }
}

impl Autocomplete for ProfessionAutocomplete<'_> {
    fn get_suggestions(&mut self, input: &str) -> Result<Vec<String>, inquire::CustomUserError> {
        Ok(self
            .professions
            .iter()
            .filter(|p| p.starts_with(&input.to_lowercase()))
            .map(Profession::to_owned)
            .collect())
    }

    fn get_completion(
        &mut self,
        _input: &str,
        highlighted_suggestion: Option<String>,
    ) -> Result<Option<String>, inquire::CustomUserError> {
        Ok(highlighted_suggestion)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn render_grid_empty() {
        let editor = GridEditor::new();
        let rendered = editor.render_grid();
        assert!(rendered.contains("=== Grid Editor ==="));
        assert!(rendered.contains("[A1]"));
        assert!(rendered.contains("____"));
        assert!(rendered.contains("[D5]"));
    }

    #[test]
    fn render_grid_populated() {
        let mut editor = GridEditor::new();
        let coord = Coordinate::from_index(0); // A1
        editor[coord] = CardEdit::Draft(
            CardFront {
                name: "Alice".to_owned(),
                profession: "Artist".to_owned(),
            },
            None,
        );
        let rendered = editor.render_grid();
        assert!(rendered.contains("Alice"));
        assert!(rendered.contains("(Artist)"));
    }
}
