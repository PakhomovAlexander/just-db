#[derive(Debug, PartialEq, Clone)]
pub enum LexError {
    InvalidCharacter(char, usize),
}
