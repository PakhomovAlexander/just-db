#[derive(Debug, PartialEq, Clone)]
pub enum LexError {
    InvalidCharacter(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    // Keywords
    Select,
    From,
    Where,
    Insert,
    Into,
    Values,
    Update,
    Set,
    Delete,
    Create,
    Table,
    Primary,
    Key,
    Foreign,
    References,
    Drop,
    Alter,
    Add,
    Column,
    Constraint,
    Index,
    Join,
    Inner,
    Left,
    Right,
    Full,
    Outer,
    On,
    Group,
    By,
    Order,
    Asc,
    Desc,
    Union,
    All,
    Distinct,
    Limit,
    Offset,
    Having,
    As,
    And,
    Or,
    Not,
    Null,
    Is,
    In,
    Between,
    Like,
    Exists,
    Any,
    Case,
    When,
    Then,
    Else,
    End,
    Default,

    // Data Types
    Int,
    Integer,
    SmallInt,
    TinyInt,
    BigInt,
    Float,
    Real,
    Double,
    Decimal,
    Numeric,
    VarChar,
    Char,
    Text,
    Date,
    Time,
    Timestamp,
    Boolean,

    // Symbols and Operators
    Asterisk,
    Comma,
    Semicolon,
    OpenParen,
    CloseParen,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,
    Plus,
    Minus,
    Slash,
    Percent,
    Concat,
    SingleQuote,
    DoubleQuote,

    // Identifiers and Literals
    Identifier(&'a str),
    StringLiteral(String),
    NumericLiteral(String),

    // Comments
    SingleLineComment(String),
    MultiLineComment(String),

    // Whitespace
    Whitespace,

    // EOF
    EOF,
}

struct Lexer<'a> {
    input: &'a str,
    input_iterator: std::str::Chars<'a>,
    current_position: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let c = self.skip_whitespace()?;

            match c {
                '*' => return Some(self.single(Token::Asterisk)),
                ',' => return Some(self.single(Token::Comma)),
                '=' => return Some(self.single(Token::Equals)),
                '+' => return Some(self.single(Token::Plus)),
                '-' => return Some(self.single(Token::Minus)),
                '%' => return Some(self.single(Token::Percent)),
                '|' => return Some(self.single(Token::Concat)),
                // may be part of longer token
                '<' => return Some(self.may_be_longer(Token::LessThan)),
                '>' => return Some(self.may_be_longer(Token::GreaterThan)),
                '/' => return Some(self.may_be_longer(Token::Slash)),
                '\'' => return Some(self.may_be_longer(Token::SingleQuote)),
                '"' => return Some(self.literal_started()),
                '(' => return Some(Ok(Token::OpenParen)),
                ')' => return Some(Ok(Token::CloseParen)),
                ';' => return Some(Ok(Token::Semicolon)),
                c => {
                    if c.is_alphabetic() {
                        return Some(self.word_started());
                    } else {
                        return Some(Err(LexError::InvalidCharacter(c)));
                    }
                }
            };
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            input_iterator: input.chars(),
            current_position: 0,
        }
    }

    fn single(&mut self, token: Token<'a>) -> Result<Token<'a>, LexError> {
        match self.input_iterator.next() {
            Some(c) => {
                self.current_position += 1;
                if c.is_whitespace() {
                    return Ok(token);
                } else {
                    return Err(LexError::InvalidCharacter(c));
                }
            }
            None => return Ok(token),
        }
    }

    // select *
    pub fn skip_whitespace(&mut self) -> Option<char> {
        while let Some(c) = self.input_iterator.next() {
            self.current_position += 1;
            if !c.is_whitespace() {
                return Some(c);
            }
        }
        return None;
    }

    fn may_be_longer(&mut self, first: Token<'a>) -> Result<Token<'a>, LexError> {
        let second = self.input_iterator.next();
        self.current_position += 1;

        match first {
            Token::LessThan => match second {
                Some('=') => return Ok(Token::LessThanOrEquals),
                Some('>') => return Ok(Token::NotEquals),
                Some(' ') => return Ok(Token::LessThan),
                Some(c) => return Err(LexError::InvalidCharacter(c)),
                None => return Ok(Token::LessThan),
            },
            Token::GreaterThan => match second {
                Some('=') => return Ok(Token::GreaterThanOrEquals),
                Some(' ') => return Ok(Token::GreaterThan),
                Some(c) => return Err(LexError::InvalidCharacter(c)),
                None => return Ok(Token::GreaterThan),
            },
            Token::Slash => match second {
                Some(' ') => return Ok(Token::Slash),
                Some(c) => return Err(LexError::InvalidCharacter(c)),
                None => return Ok(Token::Slash),
            },
            _ => return Err(LexError::InvalidCharacter('|')),
        }
    }

    fn literal_started(&self) -> Result<Token<'a>, LexError> {
        todo!()
    }

    // default
    fn word_started(&mut self) -> Result<Token<'a>, LexError> {
        let started_position = self.current_position - 1;
        loop {
            let c = self.input_iterator.next();
            self.current_position += 1;
            if c.is_some() && c.unwrap().is_alphabetic() {
                continue;
            } else {
                break;
            }
        }

        let word = &self.input[started_position..self.current_position - 1];

        let lower_case_word = word.to_lowercase();

        if lower_case_word == "select" {
            return Ok(Token::Select);
        } else if lower_case_word == "from" {
            return Ok(Token::From);
        } else if lower_case_word == "where" {
            return Ok(Token::Where);
        } else if lower_case_word == "insert" {
            return Ok(Token::Insert);
        } else if lower_case_word == "into" {
            return Ok(Token::Into);
        } else if lower_case_word == "values" {
            return Ok(Token::Values);
        } else if lower_case_word == "update" {
            return Ok(Token::Update);
        } else if lower_case_word == "set" {
            return Ok(Token::Set);
        } else if lower_case_word == "delete" {
            return Ok(Token::Delete);
        } else if lower_case_word == "create" {
            return Ok(Token::Create);
        } else if lower_case_word == "table" {
            return Ok(Token::Table);
        } else if lower_case_word == "primary" {
            return Ok(Token::Primary);
        } else if lower_case_word == "key" {
            return Ok(Token::Key);
        } else if lower_case_word == "foreign" {
            return Ok(Token::Foreign);
        } else if lower_case_word == "references" {
            return Ok(Token::References);
        } else if lower_case_word == "drop" {
            return Ok(Token::Drop);
        } else if lower_case_word == "alter" {
            return Ok(Token::Alter);
        } else if lower_case_word == "add" {
            return Ok(Token::Add);
        } else if lower_case_word == "column" {
            return Ok(Token::Column);
        } else if lower_case_word == "constraint" {
            return Ok(Token::Constraint);
        } else if lower_case_word == "index" {
            return Ok(Token::Index);
        } else if lower_case_word == "join" {
            return Ok(Token::Join);
        } else if lower_case_word == "inner" {
            return Ok(Token::Inner);
        } else if lower_case_word == "left" {
            return Ok(Token::Left);
        } else if lower_case_word == "right" {
            return Ok(Token::Right);
        } else if lower_case_word == "full" {
            return Ok(Token::Full);
        } else if lower_case_word == "outer" {
            return Ok(Token::Outer);
        } else if lower_case_word == "on" {
            return Ok(Token::On);
        } else if lower_case_word == "group" {
            return Ok(Token::Group);
        } else if lower_case_word == "by" {
            return Ok(Token::By);
        } else if lower_case_word == "order" {
            return Ok(Token::Order);
        } else if lower_case_word == "asc" {
            return Ok(Token::Asc);
        } else if lower_case_word == "desc" {
            return Ok(Token::Desc);
        } else if lower_case_word == "union" {
            return Ok(Token::Union);
        } else if lower_case_word == "all" {
            return Ok(Token::All);
        } else if lower_case_word == "distinct" {
            return Ok(Token::Distinct);
        } else if lower_case_word == "limit" {
            return Ok(Token::Limit);
        } else if lower_case_word == "offset" {
            return Ok(Token::Offset);
        } else if lower_case_word == "having" {
            return Ok(Token::Having);
        } else if lower_case_word == "as" {
            return Ok(Token::As);
        } else if lower_case_word == "and" {
            return Ok(Token::And);
        } else if lower_case_word == "or" {
            return Ok(Token::Or);
        } else if lower_case_word == "not" {
            return Ok(Token::Not);
        } else if lower_case_word == "null" {
            return Ok(Token::Null);
        } else if lower_case_word == "is" {
            return Ok(Token::Is);
        } else if lower_case_word == "in" {
            return Ok(Token::In);
        } else if lower_case_word == "between" {
            return Ok(Token::Between);
        } else if lower_case_word == "like" {
            return Ok(Token::Like);
        } else if lower_case_word == "exists" {
            return Ok(Token::Exists);
        } else if lower_case_word == "any" {
            return Ok(Token::Any);
        } else if lower_case_word == "case" {
            return Ok(Token::Case);
        } else if lower_case_word == "when" {
            return Ok(Token::When);
        } else if lower_case_word == "then" {
            return Ok(Token::Then);
        } else if lower_case_word == "else" {
            return Ok(Token::Else);
        } else if lower_case_word == "end" {
            return Ok(Token::End);
        } else if lower_case_word == "default" {
            return Ok(Token::Default);
        } else {
            return Ok(Token::Identifier(word));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::lexer::{LexError, Lexer, Token};

    #[test]
    fn lex_empty_input() {
        let input = "";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![];

        assert_eq!(actual, expected);
    }

    #[test]
    fn lex_whitespace() {
        let input = "   ";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![];

        assert_eq!(actual, expected);
    }

    #[test]
    fn lex_error() {
        let input = "**";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![Err(LexError::InvalidCharacter('*'))];

        assert_eq!(actual, expected);
    }

    #[test]
    fn lex_single_chars() {
        let input = "* , ; ( ) = < >   + - / % |";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::Asterisk),
            Ok(Token::Comma),
            Ok(Token::Semicolon),
            Ok(Token::OpenParen),
            Ok(Token::CloseParen),
            Ok(Token::Equals),
            Ok(Token::LessThan),
            Ok(Token::GreaterThan),
            Ok(Token::Plus),
            Ok(Token::Minus),
            Ok(Token::Slash),
            Ok(Token::Percent),
            Ok(Token::Concat),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn lex_longer_tokens() {
        let input = "<= >= <>";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::LessThanOrEquals),
            Ok(Token::GreaterThanOrEquals),
            Ok(Token::NotEquals),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn lex_keywords() {
        let input = "select from where insert into values update set delete create table primary key foreign references drop alter add column constraint index join inner left right full outer on group by order asc desc union all distinct limit offset having as and or not null is in between like exists any case when then else end default";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::Select),
            Ok(Token::From),
            Ok(Token::Where),
            Ok(Token::Insert),
            Ok(Token::Into),
            Ok(Token::Values),
            Ok(Token::Update),
            Ok(Token::Set),
            Ok(Token::Delete),
            Ok(Token::Create),
            Ok(Token::Table),
            Ok(Token::Primary),
            Ok(Token::Key),
            Ok(Token::Foreign),
            Ok(Token::References),
            Ok(Token::Drop),
            Ok(Token::Alter),
            Ok(Token::Add),
            Ok(Token::Column),
            Ok(Token::Constraint),
            Ok(Token::Index),
            Ok(Token::Join),
            Ok(Token::Inner),
            Ok(Token::Left),
            Ok(Token::Right),
            Ok(Token::Full),
            Ok(Token::Outer),
            Ok(Token::On),
            Ok(Token::Group),
            Ok(Token::By),
            Ok(Token::Order),
            Ok(Token::Asc),
            Ok(Token::Desc),
            Ok(Token::Union),
            Ok(Token::All),
            Ok(Token::Distinct),
            Ok(Token::Limit),
            Ok(Token::Offset),
            Ok(Token::Having),
            Ok(Token::As),
            Ok(Token::And),
            Ok(Token::Or),
            Ok(Token::Not),
            Ok(Token::Null),
            Ok(Token::Is),
            Ok(Token::In),
            Ok(Token::Between),
            Ok(Token::Like),
            Ok(Token::Exists),
            Ok(Token::Any),
            Ok(Token::Case),
            Ok(Token::When),
            Ok(Token::Then),
            Ok(Token::Else),
            Ok(Token::End),
            Ok(Token::Default),
        ];

        assert_eq!(actual, expected);
    }
}
