use std::ops::ControlFlow;
use std::{fmt, mem};

use ansi_to_tui::IntoText as _;
use anyhow::Result;
use colored::{ColoredString, Colorize as _};
use linearize::StaticCopyMap;
use ratatui::crossterm::event::{self, Event, KeyCode, KeyModifiers};
use ratatui::layout::{Constraint, Layout};
use ratatui::prelude::{Frame, Rect};
use ratatui::style::Stylize as _;
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, BorderType, List, Paragraph, Wrap};
use tabled::Table;
use tabled::settings::formatting::AlignmentStrategy;
use tabled::settings::{Alignment, Color as TabledColor, Style as TabledStyle};

use crate::grid::Grid;
use crate::models::{CardFront, Coord, Direction, HintText, Judgment, Name, Puzzle, Row};
use crate::player::tag_spot::TagSpot;

type Tags = StaticCopyMap<TagSpot, Option<Tag>>;

pub(crate) struct Game<'puzz> {
    puzzle: &'puzz Puzzle,
}

impl<'puzz> Game<'puzz> {
    fn new(puzzle: &'puzz Puzzle) -> Self {
        Self { puzzle }
    }

    fn start(&self) -> (Coord, Judgment, &'puzz str) {
        let start = self.puzzle.start;
        let card = &self.puzzle.cards[start];
        (start, card.judgment, self.puzzle.starting_hint())
    }

    fn fronts(&self) -> Grid<&'puzz CardFront> {
        self.puzzle.cards.each_ref().map(|card| &card.front)
    }

    fn mark(&self, coord: Coord, judgment: Judgment) -> Option<&'puzz HintText> {
        let card = &self.puzzle.cards[coord];
        // TODO better validation, like the actual game
        (card.judgment == judgment).then_some(&card.hint)
    }
}

pub(crate) struct App<'puzz> {
    game: Game<'puzz>,
    fronts: Grid<&'puzz CardFront>,
    state: Grid<State<'puzz>>,
    current: Coord,
    mistakes: usize,
    redraw: bool,
}

impl<'puzz> App<'puzz> {
    pub(crate) fn new(puzzle: &'puzz Puzzle) -> Self {
        let game = Game::new(puzzle);
        let (start, judgment, hint) = game.start();
        let state = Grid::from_fn(|coord| {
            if coord == start {
                State::start(judgment, hint)
            } else {
                State::default()
            }
        });
        let current = puzzle.start;
        let fronts = game.fronts();
        Self {
            game,
            fronts,
            state,
            current,
            mistakes: 0,
            redraw: true,
        }
    }

    pub(crate) fn play(&mut self) -> Result<()> {
        ratatui::run(|terminal| -> Result<()> {
            loop {
                if self.redraw {
                    let mut result = Ok(());
                    _ = terminal.draw(|frame| result = self.frame(frame))?;
                    result?;
                    self.redraw = false;
                }

                if let Some(message) = Message::read()? {
                    match self.update(message) {
                        ControlFlow::Continue(redraw) => self.redraw = redraw,
                        ControlFlow::Break(()) => return Ok(()),
                    }
                }
            }
        })
    }

    fn frame(&self, frame: &mut Frame<'_>) -> Result<()> {
        let grid = self.grid();

        // TODO split based on aspect ratio
        let [grid_width, grid_height] = [grid.total_width(), grid.total_height()]
            .map(|dim| dim.try_into().expect("terminal width < u16::MAX"));

        let [grid_area, hints_area] =
            Layout::horizontal([Constraint::Length(grid_width), Constraint::Fill(1)])
                .spacing(1_u16)
                .areas(frame.area());

        let [grid_area, current_hint_area] =
            Layout::vertical([Constraint::Length(grid_height), Constraint::Fill(1)])
                .spacing(1_u16)
                .areas(grid_area);
        frame.render_widget(Paragraph::new(grid.to_string().into_text()?), grid_area);

        self.render_current_hint(frame, current_hint_area);

        self.render_hints(frame, hints_area);

        Ok(())
    }

    fn render_current_hint(&self, frame: &mut Frame<'_>, area: Rect) {
        let name = &self.fronts[self.current].name;
        let hint = match self.state[self.current].hint {
            HintState::Hidden => None,
            HintState::Flavor(_) => Some(Line::from(
                [
                    Span::from(name.as_str()),
                    Span::from(": "),
                    Span::from("flavor text").italic(),
                ]
                .as_slice(),
            )),
            HintState::Revealed(_, hint) => Some(Line::from(HintDisplay::show(name, hint))),
            HintState::Discarded(_, hint) => Some(Line::from(HintDisplay::discard(name, hint))),
        };
        if let Some(hint) = hint {
            frame.render_widget(Paragraph::new(hint).wrap(Wrap { trim: true }), area);
        }
    }

    fn render_hints(&self, frame: &mut Frame<'_>, area: Rect) {
        let hints = self
            .state
            .iter()
            .filter_map(|(coord, state)| {
                let name = &self.fronts[coord].name;
                match state.hint {
                    HintState::Hidden | HintState::Flavor(_) | HintState::Discarded(..) => None,
                    HintState::Revealed(_, hint) => Some(HintDisplay::show(name, hint)),
                }
            })
            .map(Line::from);
        frame.render_widget(
            List::new(hints).block(
                Block::bordered()
                    .title("Hints")
                    .border_type(BorderType::Rounded),
            ),
            area,
        );
    }

    fn grid(&self) -> Table {
        let mut table = Table::nohead(Row::ALL.map(|row| {
            row.all().map(|coord| {
                DisplayCard::new(
                    coord,
                    self.fronts[coord],
                    coord == self.current,
                    self.state[coord].tags,
                )
            })
        }));
        _ = table
            .with(TabledStyle::modern_rounded())
            .with(AlignmentStrategy::PerLine)
            .with(Alignment::center());

        for coord in Coord::all() {
            if let Some(judgment) = self.state[coord].hint.judgment() {
                let color = match judgment {
                    Judgment::Innocent => TabledColor::FG_GREEN,
                    Judgment::Criminal => TabledColor::FG_RED,
                };
                _ = table.modify(coord.as_tuple(), color);
            }
        }
        table
    }

    fn update(&mut self, message: Message) -> ControlFlow<(), bool> {
        match message {
            Message::Mark(judgment) => {
                let state = &mut self.state[self.current];
                if !state.hint.is_hidden() {
                    return ControlFlow::Continue(false);
                }
                match self.game.mark(self.current, judgment) {
                    Some(hint) => {
                        state.tags = Tags::default();
                        state.hint = match hint {
                            HintText::Flavor => HintState::Flavor(judgment),
                            HintText::Logical(hint) => HintState::Revealed(judgment, hint),
                        }
                    }
                    None => self.mistakes += 1,
                }
            }
            Message::Move(direction) => match self.current.step(direction) {
                Some(coord) => self.current = coord,
                None => return ControlFlow::Continue(false),
            },
            Message::Discard => self.state[self.current].hint.discard(),
            Message::Tag(spot, tag) => {
                Tag::set_or_clear(&mut self.state[self.current].tags[spot], tag);
            }
            Message::ClearTag(spot) => self.state[self.current].tags[spot] = None,
            Message::Quit => return ControlFlow::Break(()),
        }
        ControlFlow::Continue(true)
    }
}

#[derive(Default)]
struct State<'puzz> {
    hint: HintState<'puzz>,
    tags: Tags,
}

impl<'puzz> State<'puzz> {
    fn start(judgment: Judgment, hint: &'puzz str) -> Self {
        Self {
            hint: HintState::Revealed(judgment, hint),
            ..Self::default()
        }
    }
}

#[derive(Default, Clone, Copy)]
enum HintState<'puzz> {
    #[default]
    Hidden,
    Revealed(Judgment, &'puzz str),
    Discarded(Judgment, &'puzz str),
    Flavor(Judgment),
}

impl HintState<'_> {
    fn judgment(&self) -> Option<Judgment> {
        match self {
            Self::Hidden => None,
            &Self::Revealed(judgment, _)
            | &Self::Discarded(judgment, _)
            | &Self::Flavor(judgment) => Some(judgment),
        }
    }

    /// Returns `true` if the hint state is [`Hidden`].
    ///
    /// [`Hidden`]: HintState::Hidden
    #[must_use]
    fn is_hidden(&self) -> bool {
        matches!(self, Self::Hidden)
    }

    fn discard(&mut self) {
        *self = match mem::take(self) {
            state @ (HintState::Hidden | HintState::Flavor(_)) => state,
            HintState::Revealed(judgment, hint) => Self::Discarded(judgment, hint),
            HintState::Discarded(judgment, hint) => Self::Revealed(judgment, hint),
        }
    }
}

struct DisplayCard<'puzz> {
    coord: Coord,
    front: &'puzz CardFront,
    selected: bool,
    tags: Tags,
}

impl<'puzz> DisplayCard<'puzz> {
    fn new(coord: Coord, front: &'puzz CardFront, selected: bool, tags: Tags) -> Self {
        Self {
            coord,
            front,
            selected,
            tags,
        }
    }
}

impl fmt::Display for DisplayCard<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.selected {
            writeln!(f, "> {} <", self.coord)?;
        } else {
            writeln!(f, "{}", self.coord)?;
        }
        let [left, right] =
            [TagSpot::Left, TagSpot::Right].map(|spot| Tag::colored(self.tags[spot]));
        writeln!(f, "{left}  {right}")?;
        write!(f, "{}\n{}", self.front.name, self.front.profession)
    }
}

struct HintDisplay<'puzz> {
    name: &'puzz Name,
    hint: &'puzz str,
    style: HintDisplayStyle,
}

impl<'puzz> HintDisplay<'puzz> {
    fn show(name: &'puzz Name, hint: &'puzz str) -> Self {
        Self {
            name,
            hint,
            style: HintDisplayStyle::Shown,
        }
    }

    fn discard(name: &'puzz Name, hint: &'puzz str) -> Self {
        Self {
            name,
            hint,
            style: HintDisplayStyle::Discarded,
        }
    }
}

impl From<HintDisplay<'_>> for Line<'static> {
    fn from(hint: HintDisplay<'_>) -> Self {
        let text = format!("{}: {}", hint.name, hint.hint);
        match hint.style {
            HintDisplayStyle::Shown => Line::from(text),
            HintDisplayStyle::Discarded => Line::from(text).crossed_out(),
        }
    }
}

enum HintDisplayStyle {
    Shown,
    Discarded,
}

#[derive(Clone, Copy)]
enum Message {
    Mark(Judgment),
    Move(Direction),
    Tag(TagSpot, Tag),
    ClearTag(TagSpot),
    Discard,
    Quit,
}

impl Message {
    fn read() -> Result<Option<Self>> {
        Ok(Self::from_event(&event::read()?))
    }

    fn from_event(event: &Event) -> Option<Self> {
        let event = event.as_key_press_event()?;
        let message = match event.code {
            KeyCode::Left => Self::Move(Direction::Left),
            KeyCode::Right => Self::Move(Direction::Right),
            KeyCode::Up => Self::Move(Direction::Above),
            KeyCode::Down => Self::Move(Direction::Below),
            KeyCode::Char(c) => {
                // TODO come up with more layout-agnostic controls
                match c {
                    'i' => Self::Mark(Judgment::Innocent),
                    'c' if event.modifiers.contains(KeyModifiers::CONTROL) => Self::Quit,
                    'c' => Self::Mark(Judgment::Criminal),
                    'x' => Self::Discard,
                    'q' => Self::Quit,
                    '0' => Self::ClearTag(TagSpot::Left),
                    ')' => Self::ClearTag(TagSpot::Right),
                    '1' => Self::Tag(TagSpot::Left, Tag::Yellow),
                    '2' => Self::Tag(TagSpot::Left, Tag::Red),
                    '3' => Self::Tag(TagSpot::Left, Tag::Green),
                    '4' => Self::Tag(TagSpot::Left, Tag::Orange),
                    '5' => Self::Tag(TagSpot::Left, Tag::Purple),
                    '6' => Self::Tag(TagSpot::Left, Tag::Cyan),
                    '!' => Self::Tag(TagSpot::Right, Tag::Yellow),
                    '@' => Self::Tag(TagSpot::Right, Tag::Red),
                    '#' => Self::Tag(TagSpot::Right, Tag::Green),
                    '$' => Self::Tag(TagSpot::Right, Tag::Orange),
                    '%' => Self::Tag(TagSpot::Right, Tag::Purple),
                    '^' => Self::Tag(TagSpot::Right, Tag::Cyan),

                    _ => return None,
                }
            }
            KeyCode::Esc => Self::Quit,
            KeyCode::Backspace
            | KeyCode::Enter
            | KeyCode::Home
            | KeyCode::End
            | KeyCode::PageUp
            | KeyCode::PageDown
            | KeyCode::Tab
            | KeyCode::BackTab
            | KeyCode::Delete
            | KeyCode::Insert
            | KeyCode::F(_)
            | KeyCode::Null
            | KeyCode::CapsLock
            | KeyCode::ScrollLock
            | KeyCode::NumLock
            | KeyCode::PrintScreen
            | KeyCode::Pause
            | KeyCode::Menu
            | KeyCode::KeypadBegin
            | KeyCode::Media(_)
            | KeyCode::Modifier(_) => return None,
        };
        Some(message)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tag {
    Yellow,
    Red,
    Green,
    Orange,
    Purple,
    Cyan,
}

impl Tag {
    fn color(self) -> colored::Color {
        match self {
            Self::Yellow => colored::Color::Yellow,
            Self::Red => colored::Color::Red,
            Self::Green => colored::Color::Green,
            Self::Orange => colored::Color::Blue,
            Self::Purple => colored::Color::Magenta,
            Self::Cyan => colored::Color::Cyan,
        }
    }

    fn set_or_clear(this: &mut Option<Self>, tag: Self) {
        *this = (*this != Some(tag)).then_some(tag);
    }

    fn colored(this: Option<Self>) -> ColoredString {
        this.map_or_else(|| " ".normal(), |tag| " ".on_color(tag.color()))
    }
}

impl fmt::Display for Tag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", " ".on_color(self.color()))
    }
}

mod tag_spot {
    #![expect(unsafe_code, reason = "derive macro")]

    use linearize::Linearize;
    #[derive(Linearize, Clone, Copy)]
    pub(crate) enum TagSpot {
        Left,
        Right,
    }
}
