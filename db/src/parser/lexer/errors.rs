use miette::Diagnostic;
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Diagnostic, Debug, Error, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[error("Lexical error")]
#[diagnostic(code("lex-error"))]
pub struct LexError {
    #[source_code]
    pub src: String,

    #[label("Unexpected character")]
    pub snip: (usize, usize),
}
