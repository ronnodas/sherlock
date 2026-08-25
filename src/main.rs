use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::{fmt, fs};

use anyhow::{Context as _, Result, bail};
use bpaf::{Bpaf, Parser as _};
use inquire::{Confirm, CustomType, Editor, Select, Text};
use jiff::Timestamp;
use jiff::tz::TimeZone;
use mitsein::EmptyError;
use mitsein::str1::Str1;
use mitsein::string1::String1;

use crate::editor::BoardEditor;
use crate::models::Puzzle;
use crate::player::App;
use crate::solver::ParsedBoard;

mod editor;
mod grid;
mod models;
mod player;
mod solver;

const API_KEY_FILE: &str = "browserless_api_key";
const SAVE_DIR: &str = "saved/";
const ARCHIVE_DIR: &str = "archive/";

fn main() -> Result<()> {
    let args = args().run();

    let result = match args {
        Args::Menu => main_menu()?,
        Args::Html { path } => MainMenu::Solve(read_from_file(path, FileType::Html)?),
        Args::Load { path } => MainMenu::Solve(read_from_file(path, FileType::Ron)?),
        Args::Today => MainMenu::Solve(fetch_today()?),
        Args::Archive { id_or_url } => MainMenu::Solve(archive(
            id_or_url.try_into().context("id or url cannot be empty")?,
        )?),
    };
    match result {
        MainMenu::Solve(board) => board.solve_brute_force()?,
        MainMenu::Play(puzzle) => App::new(&puzzle).play()?,
    }

    Ok(())
}

fn main_menu() -> Result<MainMenu> {
    loop {
        let mode = Select::new(
            "Which puzzle do you want to solve?",
            InputMode::ALL.to_vec(),
        )
        .prompt()?;
        let result = match mode {
            InputMode::Today => MainMenu::Solve(fetch_today()?),
            InputMode::Fetch => {
                let archive_id = CustomType::<NonEmptyText>::new("Enter puzzle archive id or url")
                    .with_placeholder("s/a0b1c2d3e4f5")
                    .prompt()?
                    .into();
                MainMenu::Solve(archive(archive_id)?)
            }
            InputMode::Load => {
                let path = Text::new("Enter path to ron:")
                    .with_initial_value(SAVE_DIR)
                    .prompt()?;
                let path = PathBuf::from(path);
                let path = if path.extension().is_none() {
                    path.with_added_extension("ron")
                } else {
                    path
                };
                MainMenu::Solve(read_from_file(path, FileType::Ron)?)
            }

            InputMode::Html => {
                let path = Text::new("Enter path to html:").prompt()?;
                MainMenu::Solve(read_from_file(path, FileType::Html)?)
            }
            InputMode::Paste => {
                let html = Editor::new("Enter HTML in your editor:").prompt()?;
                MainMenu::Solve(ParsedBoard::from_html(&html, None)?)
            }
            InputMode::Manual => {
                if let Some(board) = manual_mode()? {
                    MainMenu::Solve(board)
                } else {
                    continue;
                }
            }
            InputMode::Replay => MainMenu::Play(load_puzzle()?),
        };
        return Ok(result);
    }
}

fn load_puzzle() -> Result<Puzzle> {
    let path = Text::new("Enter path to ron:")
        .with_initial_value(ARCHIVE_DIR)
        .prompt()?;
    let path = PathBuf::from(path);
    let path = if path.extension().is_none() {
        path.with_added_extension("ron")
    } else {
        path
    };

    let contents = fs::read_to_string(path)?;
    let puzzle = ron::from_str(&contents)?;
    Ok(puzzle)
}

enum MainMenu {
    Solve(ParsedBoard),
    Play(Puzzle),
}

fn archive(id_or_url: String1) -> Result<ParsedBoard> {
    let (url, id) = if let Some(url) = id_or_url.strip_prefix("https://") {
        if let Some(id) = extract_id(url) {
            (Cow::Borrowed(id_or_url.as_str()), Cow::Borrowed(id))
        } else {
            bail!("did not recognize url")
        }
    } else if let Some(id) = extract_id(&id_or_url) {
        (
            Cow::Owned(format!("https://{id_or_url}")),
            Cow::Borrowed(id),
        )
    } else if let Some(id) = id_or_url.strip_prefix("s/")
        && let Ok(id) = Str1::try_from_str(id)
    {
        (
            Cow::Owned(archive_url(id.as_str(), true)),
            Cow::Borrowed(id),
        )
    } else {
        let url_with_s = archive_url(&id_or_url, true);
        return fetch_from_url(&url_with_s, None).or_else(|_e| {
            let url_without_s = archive_url(&id_or_url, false);
            fetch_from_url(&url_without_s, Some(id_or_url))
        });
    };

    fetch_from_url(url.as_ref(), Some(id.into_owned()))
}

fn archive_url(input: &str, with_s: bool) -> String {
    if with_s {
        format!("https://cluesbysam.com/s/archive/{input}/")
    } else {
        format!("https://cluesbysam.com/archive/{input}/")
    }
}

fn extract_id(url: &str) -> Option<&Str1> {
    // TODO use trim_suffix('/') https://github.com/rust-lang/rust/issues/142312
    let url = url.trim().trim_end_matches('/');
    let prefixes = [
        "cluesbysam.com/s/archive/",
        "cluesbysam.com/archive/",
        "cluesbysam.com/s/play/?puzzleId=",
    ];
    prefixes
        .into_iter()
        .find_map(|prefix| url.strip_prefix(prefix))
        .and_then(|id| id.try_into().ok())
}

fn fetch_today() -> Result<ParsedBoard> {
    fetch_from_url("https://cluesbysam.com/", Some(date_string()))
}

fn date_string() -> String1 {
    Timestamp::now()
        .to_zoned(TimeZone::get("America/New_York").expect("valid identifier"))
        .date()
        .to_string()
        .try_into()
        .expect("YYYY-MM-DD")
}

fn fetch_from_url(target_url: &str, title: Option<String1>) -> Result<ParsedBoard> {
    let api_key = read_api_key()?;
    let json = format!(r#"{{"url": "{target_url}"}}"#);
    let html = ureq::post(format!(
        "https://production-sfo.browserless.io/content?token={api_key}"
    ))
    .content_type("application/json")
    .send(&json)?
    .body_mut()
    .read_to_string()?;
    ParsedBoard::from_html(&html, title)
}

fn read_api_key() -> Result<String> {
    let api_key = if let Ok(api_key) = fs::read_to_string(API_KEY_FILE) {
        api_key.trim().to_owned()
    } else {
        let key = Text::new("Enter an API token from [browserless.io]:")
            .prompt()?
            .trim()
            .to_owned();
        let save = Confirm::new("Save key to disk (current directory)?")
            .with_default(true)
            .prompt()?;
        if save {
            fs::write(API_KEY_FILE, &key)?;
        }
        key
    };
    Ok(api_key)
}

fn read_from_file(path: impl AsRef<Path>, file_type: FileType) -> Result<ParsedBoard> {
    let path = path.as_ref();
    let contents = fs::read_to_string(path)?;
    let title = path
        .file_stem()
        .and_then(|name| Str1::try_from_str(name.to_str()?).ok())
        .map(Str1::to_owned);
    let parsed = match file_type {
        FileType::Ron => ParsedBoard::load(&contents, title)?,
        FileType::Html => ParsedBoard::from_html(&contents, title)?,
    };
    Ok(parsed)
}

#[derive(Debug, Clone, Bpaf)]
#[bpaf(options)]
#[bpaf(fallback(Args::Menu))]
enum Args {
    /// Show the main menu (default)
    #[bpaf(command)]
    Menu,

    /// Load a save from the specified path
    #[bpaf(command("load"), short('l'))]
    Load {
        #[bpaf(positional("PATH"))]
        path: PathBuf,
    },

    /// Parse an HTML file as a puzzle
    #[bpaf(command("html"), short('h'))]
    Html {
        #[bpaf(positional("PATH"))]
        path: PathBuf,
    },

    /// Load today's puzzle
    #[bpaf(command("today"), short('t'))]
    Today,

    /// Load a puzzle from the online archive
    #[bpaf(command("archive"), short('a'))]
    Archive {
        #[bpaf(positional("ID or URL"))]
        id_or_url: String,
    },
}

#[derive(Clone, Copy)]
enum FileType {
    Html,
    Ron,
}

#[derive(Clone, Copy)]
enum InputMode {
    Today,
    Html,
    Load,
    Fetch,
    Paste,
    Manual,
    Replay,
}

impl InputMode {
    const ALL: [Self; 7] = [
        Self::Today,
        Self::Load,
        Self::Fetch,
        Self::Html,
        Self::Paste,
        Self::Manual,
        Self::Replay,
    ];
}

impl fmt::Display for InputMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Today => write!(f, "download today's daily puzzle"),
            Self::Load => write!(f, "load a previous save"),
            Self::Fetch => write!(f, "download puzzle from archive"),
            Self::Html => write!(f, "read html from file"),
            Self::Paste => write!(f, "paste html"),
            Self::Manual => write!(f, "manually enter puzzle"),
            Self::Replay => write!(f, "replay a previous puzzle"),
        }
    }
}

fn manual_mode() -> Result<Option<ParsedBoard>> {
    BoardEditor::new()
        .interact()?
        .map(|board| ParsedBoard::new(board, None))
        .transpose()
}

#[derive(Clone, Debug)]
struct NonEmptyText(String1);

impl FromStr for NonEmptyText {
    type Err = EmptyError<String>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.to_owned().try_into().map(Self)
    }
}

impl fmt::Display for NonEmptyText {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<NonEmptyText> for String1 {
    fn from(value: NonEmptyText) -> Self {
        value.0
    }
}

#[cfg(test)]
mod tests {
    use mitsein::str1::str1;

    use super::extract_id;

    #[test]
    fn recognizes_archive_urls() {
        assert_eq!(
            extract_id("cluesbysam.com/archive/abc123/"),
            Some(str1!("abc123"))
        );
        assert_eq!(
            extract_id("cluesbysam.com/s/archive/abc123/"),
            Some(str1!("abc123"))
        );
    }

    #[test]
    fn recognizes_play_page_urls() {
        assert_eq!(
            extract_id("cluesbysam.com/s/play/?puzzleId=abc123"),
            Some(str1!("abc123"))
        );
    }
}
