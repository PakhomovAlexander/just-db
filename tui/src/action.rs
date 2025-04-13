use crossterm::event::KeyEvent;
use db::{optimizer::types::Tuple, parser::errors::ParseError};
use serde::{Deserialize, Serialize};
use strum::Display;

#[derive(Debug, Clone, PartialEq, Eq, Display, Serialize, Deserialize)]
pub enum Action {
    Tick,
    Render,
    Resize(u16, u16),
    Suspend,
    Resume,
    Quit,
    ClearScreen,
    Help,

    D,

    ToEditingMode,
    ToNormalMode,
    ClearEditor,

    Typed(KeyEvent),
    TextUpdated(String),

    QueryResultReceived(Vec<Tuple>),
    ExecuteQueryRequested,
    ExecuteQuery(String),

    UpdateStatusBar(String),
    Error(ParseError),
}
