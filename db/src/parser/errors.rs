use miette::Diagnostic;
use thiserror::Error;

#[derive(Diagnostic, Debug, Error, Clone, PartialEq)]
#[error("oops")]
#[diagnostic(code("1"))]
pub struct ParseError {
    #[source_code]
    pub src: String,

    #[label("This is bad")]
    pub snip: (usize, usize),

    #[help("LOL")]
    pub message: String,
}
