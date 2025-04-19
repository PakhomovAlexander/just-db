use miette::Diagnostic;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::lexer::LexError;

#[derive(Diagnostic, Debug, Error, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[error("Parse error")]
#[diagnostic(code("parse-error"))]
pub struct ParseError {
    #[source_code]
    pub src: String,

    #[label("here")]
    pub snip: (usize, usize),

    #[help]
    pub message: String,

    #[source]
    pub source_err: Option<LexError>,
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> Self {
        Self {
            src: err.src.clone(),
            snip: (err.snip.0 - 1, err.snip.1),
            message: "Unexpected character".to_string(),
            source_err: Some(err),
        }
    }
}
