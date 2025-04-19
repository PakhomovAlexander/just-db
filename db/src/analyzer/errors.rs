use miette::Diagnostic;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::parser::errors::ParseError;

#[derive(Diagnostic, Debug, Error, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[error("Analyze error")]
#[diagnostic(code("analyze-error"))]
pub struct AnalyzeError {
    #[source_code]
    pub src: String,

    #[label("here")]
    pub snip: (usize, usize),

    #[help]
    pub message: String,

    #[source]
    pub source_err: Option<ParseError>,
}

impl From<ParseError> for AnalyzeError {
    fn from(err: ParseError) -> Self {
        Self {
            src: err.src.clone(),
            snip: err.snip,
            message: err.message.to_string(),
            source_err: Some(err),
        }
    }
}
