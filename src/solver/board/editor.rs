use std::array::from_fn;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt;
use std::ops::{Index, IndexMut};

use anyhow::{Result, anyhow};
use colored::Colorize as _;
use inquire::{Autocomplete, Select, Text};
use itertools::Itertools as _;
use tabled::Table;
use tabled::settings::Alignment;
use tabled::settings::formatting::AlignmentStrategy;
use tabled::settings::{Color as TabledColor, Style, object::Cell};

use crate::models::{
    Card, CardBack, Column, Coordinate, Judgment, MaybeHint, Name, Profession, Row,
};
use crate::solver::board::{Format, Board};

pub(crate) struct BoardEditor {
    cards: [CardEdit; 20],
    professions: BTreeSet<Profession>,
}

impl BoardEditor {
    pub(crate) fn new() -> Self {
        Self {
            cards: <[CardEdit; 20]>::default(),
            professions: BTreeSet::new(),
        }
    }

    fn is_complete(&self) -> bool {
        self.cards
            .iter()
            .all(|card| matches!(card, CardEdit::Draft(..)))
            && self.cards.iter().any(|card| card.logical_hint().is_some())
    }

    fn build(self) -> Result<Board> {
        let cards: [Card; 20] = self
            .cards
            .into_iter()
            .filter_map(CardEdit::finalize)
            .collect_vec()
            .try_into()
            .map_err(|cards: Vec<_>| {
                anyhow!("grid is incomplete: only {}/20 cards defined", cards.len())
            })?;

        Ok(Board::new(cards, Format::Sep2025, None))
    }

    pub(crate) fn interact(mut self) -> Result<Option<Board>> {
        loop {
            self.print_board();
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

    fn render_board(&self) -> Table {
        let (rows, _) = self.cards.as_chunks();
        let mut table = Table::nohead(rows.iter().zip(Row::ALL).map(
            |(cards, row): (&[CardEdit; 4], Row)| -> [IndexedCard<'_>; 4] {
                from_fn(|col| IndexedCard::new(row, Column::from_index(col), &cards[col]))
            },
        ));
        _ = table
            .with(Style::modern_rounded())
            .with(AlignmentStrategy::PerLine)
            .with(Alignment::center());

        for coordinate in Coordinate::all() {
            if let Some(judgment) = self.cards[coordinate.to_index()].judgment() {
                let color = match judgment {
                    Judgment::Innocent => TabledColor::FG_GREEN,
                    Judgment::Criminal => TabledColor::FG_RED,
                };
                _ = table.modify(
                    Cell::new(coordinate.row.to_index(), coordinate.col.to_index()),
                    color,
                );
            }
        }
        table
    }

    fn print_board(&self) {
        let table = self.render_board();
        println!("{table}");
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
                let update = card.edit(professions)?;
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

impl From<Board> for BoardEditor {
    fn from(board: Board) -> Self {
        let professions = board
            .by_profession()
            .as_btree_map()
            .keys()
            .cloned()
            .collect();
        Self {
            cards: board.cards.map(CardEdit::from),
            professions,
        }
    }
}

impl Index<Coordinate> for BoardEditor {
    type Output = CardEdit;

    fn index(&self, index: Coordinate) -> &CardEdit {
        &self.cards[index.to_index()]
    }
}

impl IndexMut<Coordinate> for BoardEditor {
    fn index_mut(&mut self, index: Coordinate) -> &mut CardEdit {
        &mut self.cards[index.to_index()]
    }
}

struct IndexedCard<'edit> {
    coord: Coordinate,
    card: &'edit CardEdit,
}

impl<'edit> IndexedCard<'edit> {
    fn new(row: Row, col: Column, card: &'edit CardEdit) -> Self {
        let coord = Coordinate { row, col };
        Self { coord, card }
    }
}

impl fmt::Display for IndexedCard<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.card {
            CardEdit::Empty => write!(f, "{}", self.coord),
            CardEdit::Draft(front, _) => write!(f, "{}\n{}", self.coord, front),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) enum CardEdit {
    #[default]
    Empty,
    Draft(CardFront, Option<CardBack>),
}

impl CardEdit {
    fn logical_hint(&self) -> Option<&str> {
        match self {
            Self::Draft(_, Some(back)) => back.hint().as_logical(),
            Self::Empty | Self::Draft(_, None) => None,
        }
    }

    fn finalize(self) -> Option<Card> {
        match self {
            Self::Empty => None,
            Self::Draft(front, back) => Some(Card::new(front.name, front.profession, back)),
        }
    }

    fn edit(&mut self, professions: ProfessionAutocomplete<'_>) -> Result<Option<Coordinate>> {
        let Self::Draft(front, back) = self else {
            let name = Text::new("Name:").prompt()?;
            let profession = Text::new("Profession:")
                .with_autocomplete(professions)
                .prompt()?;
            *self = Self::Draft(CardFront { name, profession }, None);
            return self.edit(professions);
        };

        if back.is_none()
            && let Some(new_back) = front.edit_unflipped(professions)?
        {
            *back = Some(new_back);
        }

        if let Some(b) = back {
            match front.edit_flipped(b, professions)? {
                FlippedUpdate::None => {}
                FlippedUpdate::Unflip => *back = None,
            }
        }
        Ok(None)
    }

    fn profession(&self) -> Option<&Profession> {
        match self {
            Self::Empty => None,
            Self::Draft(front, _) => Some(&front.profession),
        }
    }

    fn judgment(&self) -> Option<Judgment> {
        Some(self.back()?.judgment())
    }

    fn back(&self) -> Option<&CardBack> {
        match self {
            Self::Empty => None,
            Self::Draft(_, back) => back.as_ref(),
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
        professions: ProfessionAutocomplete<'_>,
    ) -> Result<Option<CardBack>> {
        let options = UnflippedAction::options(self);

        let Some(action) = Select::new("Edit cell:", options).prompt_skippable()? else {
            return Ok(None);
        };
        let update = match action {
            UnflippedAction::Common(common) => {
                let update = common.prompt(professions)?;
                self.handle(update);
                None
            }
            UnflippedAction::SetInnocent => {
                Some(CardBack::new(Judgment::Innocent, MaybeHint::Unknown))
            }
            UnflippedAction::SetCriminal => {
                Some(CardBack::new(Judgment::Criminal, MaybeHint::Unknown))
            }
        };
        Ok(update)
    }

    fn edit_flipped(
        &mut self,
        back: &mut CardBack,
        professions: ProfessionAutocomplete<'_>,
    ) -> Result<FlippedUpdate> {
        let options = FlippedAction::options(self, back);

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
                        .with_initial_value(current.as_logical().unwrap_or(""))
                        .prompt_skippable()?
                    {
                        back.set_hint(hint);
                    }
                }
                FlippedAction::MarkAsFlavor => {
                    back.mark_as_flavor();
                }
                FlippedAction::Unflip => return Ok(FlippedUpdate::Unflip),
            }
        }
        Ok(FlippedUpdate::None)
    }

    fn handle(&mut self, update: CommonUpdate) {
        match update {
            CommonUpdate::Name(name) => self.name = name,
            CommonUpdate::Profession(profession) => self.profession = profession,
            CommonUpdate::None => {}
        }
    }
}

impl fmt::Display for CardFront {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n{}", self.name, self.profession)
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
    Done,
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
            Self::Done => CommonUpdate::None,
        };
        Ok(update)
    }
}

impl fmt::Display for CommonAction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EditName(name) => write!(f, "{name}"),
            Self::EditProfession(profession) => write!(f, "{profession}"),
            Self::Done => write!(f, "Done"),
        }
    }
}

enum CommonUpdate {
    Name(Name),
    Profession(Profession),
    None,
}

#[derive(Clone)]
enum UnflippedAction<'edit> {
    Common(CommonAction<'edit>),
    SetInnocent,
    SetCriminal,
}

impl<'edit> UnflippedAction<'edit> {
    fn options(front: &'edit CardFront) -> Vec<Self> {
        vec![
            UnflippedAction::Common(CommonAction::Done),
            UnflippedAction::SetInnocent,
            UnflippedAction::SetCriminal,
            UnflippedAction::Common(CommonAction::EditName(&front.name)),
            UnflippedAction::Common(CommonAction::EditProfession(&front.profession)),
        ]
    }
}

impl fmt::Display for UnflippedAction<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Common(c) => write!(f, "{c}"),
            Self::SetInnocent => write!(f, "Mark as innocent"),
            Self::SetCriminal => write!(f, "Mark as criminal"),
        }
    }
}

#[derive(Clone)]
enum FlippedAction<'edit> {
    Common(CommonAction<'edit>),
    ToggleJudgment(Judgment),
    EditHint(&'edit MaybeHint),
    MarkAsFlavor,
    Unflip,
}

impl<'edit> FlippedAction<'edit> {
    fn options(arg: &'edit CardFront, back: &'edit CardBack) -> Vec<Self> {
        let mut options = vec![
            FlippedAction::Common(CommonAction::Done),
            FlippedAction::Common(CommonAction::EditName(&arg.name)),
            FlippedAction::Common(CommonAction::EditProfession(&arg.profession)),
            FlippedAction::ToggleJudgment(back.judgment()),
        ];
        let hint = back.hint();
        let hint_index = if hint.is_unknown() { 1 } else { 4 };
        if !hint.is_flavor() {
            options.insert(hint_index, FlippedAction::MarkAsFlavor);
        }
        options.insert(hint_index, FlippedAction::EditHint(hint));

        options.push(FlippedAction::Unflip);

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
                MaybeHint::Logical(s) => write!(f, "Hint: {s}"),
                MaybeHint::Flavor => write!(f, "Hint: <flavor>"),
                MaybeHint::Unknown => write!(f, "Add hint"),
            },
            Self::MarkAsFlavor => write!(f, "Mark hint as flavor text"),
            Self::Unflip => write!(f, "Unflip"),
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

enum FlippedUpdate {
    None,
    Unflip,
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
