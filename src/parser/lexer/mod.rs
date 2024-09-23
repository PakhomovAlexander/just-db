mod tokens;

use tokens::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum LexError {
    InvalidCharacter(char),
}

pub struct Lexer<'a> {
    input: &'a str,
    input_iterator: std::str::Chars<'a>,
    current_position: usize,
    is_finished: bool,
    cache: Option<char>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.move_and_skip_whitespace()?;

        match c {
            '*' => return Some(self.single(Token::Asterisk)),
            ',' => return Some(self.single(Token::Comma)),
            '=' => return Some(self.single(Token::Equals)),
            '+' => return Some(self.single(Token::Plus)),
            '%' => return Some(self.single(Token::Percent)),
            '|' => return Some(self.single(Token::Concat)),
            // may be part of longer token
            '<' => return Some(self.may_be_longer(Token::LessThan)),
            '-' => return Some(self.may_be_longer(Token::Minus)),
            '>' => return Some(self.may_be_longer(Token::GreaterThan)),
            '/' => return Some(self.may_be_longer(Token::Slash)),
            '\'' => return Some(self.may_be_longer(Token::SingleQuote)),
            '"' => return Some(self.may_be_longer(Token::DoubleQuote)),
            '(' => return Some(Ok(Token::OpenParen)),
            ')' => return Some(Ok(Token::CloseParen)),
            ';' => return Some(Ok(Token::Semicolon)),
            c => {
                if c.is_alphabetic() {
                    return Some(self.word_started());
                } else if c.is_numeric() {
                    return Some(self.numeric_started(false));
                } else {
                    return Some(Err(LexError::InvalidCharacter(c)));
                }
            }
        };
    }
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            input_iterator: input.chars(),
            current_position: 0,
            is_finished: false,
            cache: None,
        }
    }

    fn get_next_and_increment(&mut self) -> Option<char> {
        let c = self.input_iterator.next();
        if c.is_some() {
            self.current_position += 1;
        } else {
            self.is_finished = true;
        }

        return c;
    }

    fn get_last_token_end(&self) -> usize {
        if self.is_finished {
            return self.current_position;
        }
        self.current_position - 1
    }

    fn single(&mut self, token: Token<'a>) -> Result<Token<'a>, LexError> {
        match self.get_next_and_increment() {
            Some(c) => {
                if c.is_whitespace() {
                    return Ok(token);
                } else {
                    return Err(LexError::InvalidCharacter(c));
                }
            }
            None => return Ok(token),
        }
    }

    pub fn move_and_skip_whitespace(&mut self) -> Option<char> {
        if self.cache.is_some() {
            let c = self.cache;
            self.cache = None;
            return c;
        }

        while let Some(c) = self.get_next_and_increment() {
            if !c.is_whitespace() {
                return Some(c);
            }
        }
        return None;
    }

    fn may_be_longer(&mut self, first: Token<'a>) -> Result<Token<'a>, LexError> {
        match first {
            Token::SingleQuote | Token::DoubleQuote => return self.quote_started(first),
            _ => (),
        }

        let second = self.get_next_and_increment();

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
                Some('*') => return self.multi_line_comment_started(),
                Some(c) => return Err(LexError::InvalidCharacter(c)),
                None => return Ok(Token::Slash),
            },
            Token::Minus => match second {
                Some(' ') => return Ok(Token::Minus),
                Some('-') => return self.single_line_comment_started(),
                Some(c) => {
                    if c.is_numeric() {
                        return self.numeric_started(true);
                    }

                    return Err(LexError::InvalidCharacter(c));
                }
                None => return Ok(Token::Minus),
            },
            _ => return Err(LexError::InvalidCharacter(second.unwrap())),
        }
    }

    fn word_started(&mut self) -> Result<Token<'a>, LexError> {
        let started_position = self.get_last_token_end();

        let is_word = |c: Option<char>| -> bool {
            match c {
                Some(n) => n.is_alphabetic() || n.is_numeric() || n == '_',
                _ => false,
            }
        };

        loop {
            let c = self.get_next_and_increment();
            if is_word(c) {
                continue;
            } else {
                if c == Some('.') {
                    return self.identifier_dot_started(started_position);
                }
                if c.is_none() {
                    break;
                }

                // here c might be ')' or ',' or '('
                if [')', ',', ';'].contains(&c.unwrap()) {
                    self.cache(c);
                }

                break;
            }
        }

        let word = &self.input[started_position..self.get_last_token_end()];

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
        } else if lower_case_word == "true" {
            return Ok(Token::BooleanLiteral(true));
        } else if lower_case_word == "false" {
            return Ok(Token::BooleanLiteral(false));
        } else if lower_case_word == "int" {
            return Ok(Token::Int);
        } else if lower_case_word == "integer" {
            return Ok(Token::Integer);
        } else if lower_case_word == "smallint" {
            return Ok(Token::SmallInt);
        } else if lower_case_word == "tinyint" {
            return Ok(Token::TinyInt);
        } else if lower_case_word == "bigint" {
            return Ok(Token::BigInt);
        } else if lower_case_word == "float" {
            return Ok(Token::Float);
        } else if lower_case_word == "real" {
            return Ok(Token::Real);
        } else if lower_case_word == "double" {
            return Ok(Token::Double);
        } else if lower_case_word == "decimal" {
            return Ok(Token::Decimal);
        } else if lower_case_word == "numeric" {
            return Ok(Token::Numeric);
        } else if lower_case_word == "varchar" {
            return Ok(Token::VarChar);
        } else if lower_case_word == "char" {
            return Ok(Token::Char);
        } else if lower_case_word == "text" {
            return Ok(Token::Text);
        } else if lower_case_word == "date" {
            return Ok(Token::Date);
        } else if lower_case_word == "time" {
            return Ok(Token::Time);
        } else if lower_case_word == "timestamp" {
            return Ok(Token::Timestamp);
        } else if lower_case_word == "datetime" {
            return Ok(Token::DateTime);
        } else if lower_case_word == "boolean" {
            return Ok(Token::Boolean);
        } else {
            return Ok(Token::identifier(word));
        }
    }

    fn quote_started(&mut self, quote: Token<'a>) -> Result<Token<'a>, LexError> {
        let same_quote = |c: Option<char>| -> bool {
            match c {
                Some('\'') => quote == Token::SingleQuote,
                Some('"') => quote == Token::DoubleQuote,
                _ => false,
            }
        };

        let started_position = self.current_position;
        loop {
            let c = self.get_next_and_increment();
            if !same_quote(c) {
                continue;
            } else {
                break;
            }
        }

        let literal = &self.input[started_position..self.get_last_token_end()];

        return Ok(Token::StringLiteral(literal.to_string()));
    }

    fn numeric_started(&mut self, has_sign: bool) -> Result<Token<'a>, LexError> {
        let is_numeric = |c: Option<char>| -> bool {
            match c {
                Some(n) => n.is_numeric(),
                _ => false,
            }
        };

        let started_position = self.get_last_token_end();
        let mut seen_dot = false;
        loop {
            let c = self.get_next_and_increment();
            if is_numeric(c) {
                continue;
            } else if c == Some('.') {
                if seen_dot {
                    return Err(LexError::InvalidCharacter('.'));
                }
                seen_dot = true;
                continue;
            } else {
                break;
            }
        }

        let literal = &self.input[started_position..self.get_last_token_end()];

        return Ok(Token::NumericLiteral(if has_sign {
            format!("-{}", literal)
        } else {
            literal.to_string()
        }));
    }

    fn single_line_comment_started(&mut self) -> Result<Token<'a>, LexError> {
        let started_position = self.current_position;
        loop {
            let c = self.get_next_and_increment();
            if c == Some('\n') || c == None {
                break;
            }
        }

        let comment = &self.input[started_position..self.get_last_token_end()];

        return Ok(Token::SingleLineComment(comment.to_string()));
    }

    fn multi_line_comment_started(&mut self) -> Result<Token<'a>, LexError> {
        let started_position = self.current_position;
        loop {
            let c = self.get_next_and_increment();
            if c == Some('*') {
                let next = self.get_next_and_increment();
                if next == Some('/') {
                    break;
                }
            }
        }

        let comment = &self.input[started_position..self.current_position - 2];

        return Ok(Token::MultiLineComment(comment.to_string()));
    }

    fn identifier_dot_started(&mut self, started_position: usize) -> Result<Token<'a>, LexError> {
        let first_dot_position = self.current_position;
        let mut second_dot_position = 0;
        let mut third_dot_position = 0;

        let mut is_word = |c: Option<char>, cp: usize| -> bool {
            match c {
                Some('.') => {
                    if second_dot_position == 0 {
                        second_dot_position = cp;
                        return true;
                    }
                    if third_dot_position == 0 {
                        third_dot_position = cp;
                    }
                    return true;
                }
                Some(n) => n.is_alphabetic() || n.is_numeric() || n == '_',
                _ => false,
            }
        };

        loop {
            let c = self.get_next_and_increment();
            if is_word(c, self.current_position) {
                continue;
            } else {
                break;
            }
        }

        if third_dot_position > 0 {
            return Err(LexError::InvalidCharacter('.'));
        }

        let first_name = &self.input[started_position..first_dot_position - 1];
        let second_name = if second_dot_position > 0 {
            &self.input[first_dot_position..second_dot_position - 1]
        } else {
            &self.input[first_dot_position..self.get_last_token_end()]
        };

        let third_name = if second_dot_position > 0 {
            Some(&self.input[second_dot_position..self.get_last_token_end()])
        } else {
            None
        };

        return Ok(Token::Identifier {
            first_name,
            second_name: Some(second_name),
            third_name,
        });
    }

    fn cache(&mut self, c: Option<char>) {
        if self.cache.is_some() {
            panic!("Cache is already full");
        }

        self.cache = c;
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::lexer::{LexError, Lexer, Token};
    use pretty_assertions::assert_eq;
    use rstest::rstest;

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

    #[test]
    fn string_literals() {
        let input = "'hello' 'world'";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::StringLiteral("hello".to_string())),
            Ok(Token::StringLiteral("world".to_string())),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn string_literal_with_escape() {
        let input = "'hello \"world\"'";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![Ok(Token::StringLiteral("hello \"world\"".to_string()))];

        assert_eq!(actual, expected);
    }

    #[test]
    fn numeric_literals() {
        let input = "123 456.789 -123 -456.789";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::NumericLiteral("123".to_string())),
            Ok(Token::NumericLiteral("456.789".to_string())),
            Ok(Token::NumericLiteral("-123".to_string())),
            Ok(Token::NumericLiteral("-456.789".to_string())),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn boolean_literals() {
        let input = "true false";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::BooleanLiteral(true)),
            Ok(Token::BooleanLiteral(false)),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn date_literals() {
        let input = "DATE '2021-01-01'";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::Date),
            Ok(Token::StringLiteral("2021-01-01".to_string())),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn time_literals() {
        let input = "TIME '12:34:56'";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::Time),
            Ok(Token::StringLiteral("12:34:56".to_string())),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn datetime_literals() {
        let input = "DATETIME '2021-01-01 12:34:56'";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::DateTime),
            Ok(Token::StringLiteral("2021-01-01 12:34:56".to_string())),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn single_line_comment() {
        let input = "-- this is a comment";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![Ok(Token::SingleLineComment(
            " this is a comment".to_string(),
        ))];

        assert_eq!(actual, expected);
    }

    #[test]
    fn single_line_comment_with_other_tokens() {
        let input = "select * -- this is a comment\n from table1";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::Select),
            Ok(Token::Asterisk),
            Ok(Token::SingleLineComment(" this is a comment".to_string())),
            Ok(Token::From),
            Ok(Token::identifier("table1")),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn multi_line_comment() {
        let input = "/* this is a comment */";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![Ok(Token::MultiLineComment(
            " this is a comment ".to_string(),
        ))];

        assert_eq!(actual, expected);
    }

    #[test]
    fn really_multi_line_comment() {
        let input = "/* this is \n a comment */";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![Ok(Token::MultiLineComment(
            " this is \n a comment ".to_string(),
        ))];

        assert_eq!(actual, expected);
    }

    #[test]
    fn multi_line_comment_with_other_tokens() {
        let input = "select * /* this is a comment */ from table1";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::Select),
            Ok(Token::Asterisk),
            Ok(Token::MultiLineComment(" this is a comment ".to_string())),
            Ok(Token::From),
            Ok(Token::identifier("table1")),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn identifiers() {
        let input = "table1 column1 PUBLIC.table2 my_col_3 PUBLIC_4.table_5 public.t6able.column_7";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::identifier("table1")),
            Ok(Token::identifier("column1")),
            Ok(Token::Identifier {
                first_name: "PUBLIC",
                second_name: Some("table2"),
                third_name: None,
            }),
            Ok(Token::identifier("my_col_3")),
            Ok(Token::Identifier {
                first_name: "PUBLIC_4",
                second_name: Some("table_5"),
                third_name: None,
            }),
            Ok(Token::Identifier {
                first_name: "public",
                second_name: Some("t6able"),
                third_name: Some("column_7"),
            }),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn close_paren() {
        let input = "false) and";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::BooleanLiteral(false)),
            Ok(Token::CloseParen),
            Ok(Token::And),
        ];

        assert_eq!(actual, expected);
    }

    #[rstest]
    fn lex_all(
        #[values(
            "\
            select * 
            from table1 \
            where column1 = 123 and column2 = 'hello' \
                or column3 = 456.789 and column4 = -123 \
                or (column5 = -456.789 or column7 = false) and column6 = true \
                and column8 = DATE '2021-01-01' and 1 + 1 = 2 \
                -- this is a comment \n
            /* this is a multi-line 
               comment */; \
            ",
            "\
            SELECT * 
            FROM table1 \
            WHERE column1 = 123 AND column2 = 'hello' \
                OR column3 = 456.789 AND column4 = -123 \
                OR (column5 = -456.789 OR column7 = FALSE) AND column6 = TRUE \
                AND column8 = DATE '2021-01-01' AND 1 + 1 = 2 \
                -- this is a comment \n
            /* this is a multi-line 
               comment */; \
            ",
            "\
            Select * 
            From table1 \
            wherE column1 = 123 aNd column2 = 'hello' \
                OR column3 = 456.789 AND column4 = -123 \
                OR (column5 = -456.789 OR column7 = False) AND column6 = True \
                AND column8 = date '2021-01-01' and 1 + 1 = 2 \
                -- this is a comment \n
            /* this is a multi-line 
               comment */; \
            "
        )]
        input: &str,
    ) {
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::Select),
            Ok(Token::Asterisk),
            Ok(Token::From),
            Ok(Token::identifier("table1")),
            Ok(Token::Where),
            Ok(Token::identifier("column1")),
            Ok(Token::Equals),
            Ok(Token::NumericLiteral("123".to_string())),
            Ok(Token::And),
            Ok(Token::identifier("column2")),
            Ok(Token::Equals),
            Ok(Token::StringLiteral("hello".to_string())),
            Ok(Token::Or),
            Ok(Token::identifier("column3")),
            Ok(Token::Equals),
            Ok(Token::NumericLiteral("456.789".to_string())),
            Ok(Token::And),
            Ok(Token::identifier("column4")),
            Ok(Token::Equals),
            Ok(Token::NumericLiteral("-123".to_string())),
            Ok(Token::Or),
            Ok(Token::OpenParen),
            Ok(Token::identifier("column5")),
            Ok(Token::Equals),
            Ok(Token::NumericLiteral("-456.789".to_string())),
            Ok(Token::Or),
            Ok(Token::identifier("column7")),
            Ok(Token::Equals),
            Ok(Token::BooleanLiteral(false)),
            Ok(Token::CloseParen),
            Ok(Token::And),
            Ok(Token::identifier("column6")),
            Ok(Token::Equals),
            Ok(Token::BooleanLiteral(true)),
            Ok(Token::And),
            Ok(Token::identifier("column8")),
            Ok(Token::Equals),
            Ok(Token::Date),
            Ok(Token::StringLiteral("2021-01-01".to_string())),
            Ok(Token::And),
            Ok(Token::NumericLiteral("1".to_string())),
            Ok(Token::Plus),
            Ok(Token::NumericLiteral("1".to_string())),
            Ok(Token::Equals),
            Ok(Token::NumericLiteral("2".to_string())),
            Ok(Token::SingleLineComment(" this is a comment ".to_string())),
            Ok(Token::MultiLineComment(
                " this is a multi-line \n               comment ".to_string(),
            )),
            Ok(Token::Semicolon),
        ];

        assert_eq!(actual, expected);
    }
}
