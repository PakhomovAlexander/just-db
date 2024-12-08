pub mod tokens;

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
    peeked: Option<Result<Token<'a>, LexError>>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peeked.is_some() {
            let peeked = self.peeked.clone();
            self.peeked = None;
            return peeked;
        }

        let c = self.move_and_skip_whitespace()?;

        match c {
            '*' => self.single(Token::Asterisk),
            ',' => self.single(Token::Comma),
            '=' => self.single(Token::Equals),
            '+' => self.single(Token::Plus),
            '%' => self.single(Token::Percent),
            '|' => self.single(Token::Concat),

            '<' => self.may_be_longer(Token::LessThan),
            '-' => self.may_be_longer(Token::Minus),
            '>' => self.may_be_longer(Token::GreaterThan),
            '/' => self.may_be_longer(Token::Slash),
            '\'' => self.may_be_longer(Token::SingleQuote),
            '"' => self.may_be_longer(Token::DoubleQuote),

            '(' => Some(Ok(Token::OpenParen)),
            ')' => Some(Ok(Token::CloseParen)),
            ';' => Some(Ok(Token::Semicolon)),

            c if c.is_alphabetic() => self.word_started(),
            c if c.is_numeric() => self.numeric_started(false),

            _ => Some(Err(LexError::InvalidCharacter(c))),
        }
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
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<Result<Token<'a>, LexError>> {
        if self.peeked.is_some() {
            return self.peeked.clone();
        }

        let token = self.next();
        self.peeked = token.clone();

        token
    }

    fn get_next_and_increment(&mut self) -> Option<char> {
        let c = self.input_iterator.next();
        if c.is_some() {
            self.current_position += 1;
        } else {
            self.is_finished = true;
        }

        c
    }

    fn get_last_token_end(&self) -> usize {
        if self.is_finished {
            return self.current_position;
        }
        self.current_position - 1
    }

    fn single(&mut self, token: Token<'a>) -> Option<Result<Token<'a>, LexError>> {
        match self.get_next_and_increment() {
            Some(c) if c.is_whitespace() => Some(Ok(token)),
            Some(c) => Some(Err(LexError::InvalidCharacter(c))),
            None => Some(Ok(token)),
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

        None
    }

    fn may_be_longer(&mut self, first: Token<'a>) -> Option<Result<Token<'a>, LexError>> {
        if first == Token::SingleQuote || first == Token::DoubleQuote {
            return self.quote_started(first);
        }

        let second = self.get_next_and_increment();

        match first {
            Token::LessThan => match second {
                Some('=') => Some(Ok(Token::LessThanOrEquals)),
                Some('>') => Some(Ok(Token::NotEquals)),
                Some(' ') => Some(Ok(Token::LessThan)),

                Some(c) => Some(Err(LexError::InvalidCharacter(c))),

                None => Some(Ok(Token::LessThan)),
            },
            Token::GreaterThan => match second {
                Some('=') => Some(Ok(Token::GreaterThanOrEquals)),
                Some(' ') => Some(Ok(Token::GreaterThan)),

                Some(c) => Some(Err(LexError::InvalidCharacter(c))),

                None => Some(Ok(Token::GreaterThan)),
            },
            Token::Slash => match second {
                Some(' ') => Some(Ok(Token::Slash)),
                Some('*') => self.multi_line_comment_started(),

                Some(c) => Some(Err(LexError::InvalidCharacter(c))),

                None => Some(Ok(Token::Slash)),
            },
            Token::Minus => match second {
                Some(' ') => Some(Ok(Token::Minus)),
                Some('-') => self.single_line_comment_started(),

                Some(c) if c.is_numeric() => self.numeric_started(true),
                Some(c) => Some(Err(LexError::InvalidCharacter(c))),

                None => Some(Ok(Token::Minus)),
            },
            _ => Some(Err(LexError::InvalidCharacter(second.unwrap()))),
        }
    }

    fn word_started(&mut self) -> Option<Result<Token<'a>, LexError>> {
        let started_position = self.get_last_token_end();

        let is_word = |c: Option<char>| -> bool {
            if c.is_none() {
                return false;
            }

            let c = c.unwrap();

            c.is_alphabetic() || c.is_numeric() || c == '_'
        };

        loop {
            let c = self.get_next_and_increment();

            if is_word(c) {
                continue;
            }

            if c == Some('.') {
                return self.identifier_dot_started(started_position);
            }

            if c.is_none() {
                break;
            }

            if [')', ',', ';'].contains(&c.unwrap()) {
                self.cache(c);
            }

            break;
        }

        let word = &self.input[started_position..self.get_last_token_end()];

        let binding = word.to_lowercase();
        let lower_case_word = binding.as_str();

        match lower_case_word {
            "select" => Some(Ok(Token::Select)),
            "from" => Some(Ok(Token::From)),
            "where" => Some(Ok(Token::Where)),
            "insert" => Some(Ok(Token::Insert)),
            "into" => Some(Ok(Token::Into)),
            "values" => Some(Ok(Token::Values)),
            "update" => Some(Ok(Token::Update)),
            "set" => Some(Ok(Token::Set)),
            "delete" => Some(Ok(Token::Delete)),
            "create" => Some(Ok(Token::Create)),
            "table" => Some(Ok(Token::Table)),
            "primary" => Some(Ok(Token::Primary)),
            "key" => Some(Ok(Token::Key)),
            "foreign" => Some(Ok(Token::Foreign)),
            "references" => Some(Ok(Token::References)),
            "drop" => Some(Ok(Token::Drop)),
            "alter" => Some(Ok(Token::Alter)),
            "add" => Some(Ok(Token::Add)),
            "column" => Some(Ok(Token::Column)),
            "constraint" => Some(Ok(Token::Constraint)),
            "index" => Some(Ok(Token::Index)),
            "join" => Some(Ok(Token::Join)),
            "inner" => Some(Ok(Token::Inner)),
            "left" => Some(Ok(Token::Left)),
            "right" => Some(Ok(Token::Right)),
            "full" => Some(Ok(Token::Full)),
            "outer" => Some(Ok(Token::Outer)),
            "on" => Some(Ok(Token::On)),
            "group" => Some(Ok(Token::Group)),
            "by" => Some(Ok(Token::By)),
            "order" => Some(Ok(Token::Order)),
            "asc" => Some(Ok(Token::Asc)),
            "desc" => Some(Ok(Token::Desc)),
            "union" => Some(Ok(Token::Union)),
            "all" => Some(Ok(Token::All)),
            "distinct" => Some(Ok(Token::Distinct)),
            "limit" => Some(Ok(Token::Limit)),
            "offset" => Some(Ok(Token::Offset)),
            "having" => Some(Ok(Token::Having)),
            "as" => Some(Ok(Token::As)),
            "and" => Some(Ok(Token::And)),
            "or" => Some(Ok(Token::Or)),
            "not" => Some(Ok(Token::Not)),
            "null" => Some(Ok(Token::Null)),
            "is" => Some(Ok(Token::Is)),
            "in" => Some(Ok(Token::In)),
            "between" => Some(Ok(Token::Between)),
            "like" => Some(Ok(Token::Like)),
            "exists" => Some(Ok(Token::Exists)),
            "any" => Some(Ok(Token::Any)),
            "case" => Some(Ok(Token::Case)),
            "when" => Some(Ok(Token::When)),
            "then" => Some(Ok(Token::Then)),
            "else" => Some(Ok(Token::Else)),
            "end" => Some(Ok(Token::End)),
            "default" => Some(Ok(Token::Default)),
            "true" => Some(Ok(Token::BooleanLiteral(true))),
            "false" => Some(Ok(Token::BooleanLiteral(false))),
            "int" => Some(Ok(Token::Int)),
            "integer" => Some(Ok(Token::Integer)),
            "smallint" => Some(Ok(Token::SmallInt)),
            "tinyint" => Some(Ok(Token::TinyInt)),
            "bigint" => Some(Ok(Token::BigInt)),
            "float" => Some(Ok(Token::Float)),
            "real" => Some(Ok(Token::Real)),
            "double" => Some(Ok(Token::Double)),
            "decimal" => Some(Ok(Token::Decimal)),
            "numeric" => Some(Ok(Token::Numeric)),
            "varchar" => Some(Ok(Token::VarChar)),
            "char" => Some(Ok(Token::Char)),
            "text" => Some(Ok(Token::Text)),
            "date" => Some(Ok(Token::Date)),
            "time" => Some(Ok(Token::Time)),
            "timestamp" => Some(Ok(Token::Timestamp)),
            "datetime" => Some(Ok(Token::DateTime)),
            "boolean" => Some(Ok(Token::Boolean)),
            _ => Some(Ok(Token::identifier(word))),
        }
    }

    fn quote_started(&mut self, quote: Token<'a>) -> Option<Result<Token<'a>, LexError>> {
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

        return Some(Ok(Token::StringLiteral(literal.to_string())));
    }

    fn numeric_started(&mut self, has_sign: bool) -> Option<Result<Token<'a>, LexError>> {
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
                    return Some(Err(LexError::InvalidCharacter('.')));
                }
                seen_dot = true;
                continue;
            } else if c.is_none() {
                break;
            } else if [')', ',', ';'].contains(&c.unwrap()) {
                self.cache(c);
                break;
            } else {
                break;
            }
        }

        let literal = &self.input[started_position..self.get_last_token_end()];

        let string_representation = if has_sign {
            format!("-{}", literal)
        } else {
            literal.to_string()
        };

        return Some(Ok(Token::NumericLiteral(string_representation)));
    }

    fn single_line_comment_started(&mut self) -> Option<Result<Token<'a>, LexError>> {
        let started_position = self.current_position;
        loop {
            let c = self.get_next_and_increment();
            if c == Some('\n') || c.is_none() {
                break;
            }
        }

        let comment = &self.input[started_position..self.get_last_token_end()];

        return Some(Ok(Token::SingleLineComment(comment.to_string())));
    }

    fn multi_line_comment_started(&mut self) -> Option<Result<Token<'a>, LexError>> {
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

        return Some(Ok(Token::MultiLineComment(comment.to_string())));
    }

    fn identifier_dot_started(
        &mut self,
        started_position: usize,
    ) -> Option<Result<Token<'a>, LexError>> {
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
                    true
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
            return Some(Err(LexError::InvalidCharacter('.')));
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

        Some(Ok(Token::Identifier {
            first_name,
            second_name: Some(second_name),
            third_name,
        }))
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
    fn simple_expr() {
        let input = "1 + 2";
        let lexer = Lexer::new(input);

        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::NumericLiteral("1".to_string())),
            Ok(Token::Plus),
            Ok(Token::NumericLiteral("2".to_string())),
        ];

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

    #[test]
    fn open_paren() {
        let input = "(column5 = -456.789 or column7 = false)";
        let lexer = Lexer::new(input);

        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::OpenParen),
            Ok(Token::identifier("column5")),
            Ok(Token::Equals),
            Ok(Token::NumericLiteral("-456.789".to_string())),
            Ok(Token::Or),
            Ok(Token::identifier("column7")),
            Ok(Token::Equals),
            Ok(Token::BooleanLiteral(false)),
            Ok(Token::CloseParen),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn paren_22() {
        let input = "(1 + 222) * 3";

        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::OpenParen),
            Ok(Token::NumericLiteral("1".to_string())),
            Ok(Token::Plus),
            Ok(Token::NumericLiteral("222".to_string())),
            Ok(Token::CloseParen),
            Ok(Token::Asterisk),
            Ok(Token::NumericLiteral("3".to_string())),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn paren_2() {
        let input = "(1 + 2) * 3";

        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::OpenParen),
            Ok(Token::NumericLiteral("1".to_string())),
            Ok(Token::Plus),
            Ok(Token::NumericLiteral("2".to_string())),
            Ok(Token::CloseParen),
            Ok(Token::Asterisk),
            Ok(Token::NumericLiteral("3".to_string())),
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

    #[test]
    fn peek() {
        let input = "1 + 2";
        let mut lexer = Lexer::new(input);

        let actual = lexer.peek();
        let expected = Some(Ok(Token::NumericLiteral("1".to_string())));
        assert_eq!(actual, expected);

        let actual = lexer.peek();
        let expected = Some(Ok(Token::NumericLiteral("1".to_string())));
        assert_eq!(actual, expected);

        let actual = lexer.next();
        let expected = Some(Ok(Token::NumericLiteral("1".to_string())));
        assert_eq!(actual, expected);

        let actual = lexer.peek();
        let expected = Some(Ok(Token::Plus));
        assert_eq!(actual, expected);

        let actual = lexer.next();
        let expected = Some(Ok(Token::Plus));
        assert_eq!(actual, expected);

        let actual = lexer.next();
        let expected = Some(Ok(Token::NumericLiteral("2".to_string())));
        assert_eq!(actual, expected);

        let actual = lexer.next();
        let expected = None;
        assert_eq!(actual, expected);
    }

    #[test]
    fn next_only() {
        let input = "1 + 2";
        let mut lexer = Lexer::new(input);

        let actual = lexer.next();
        let expected = Some(Ok(Token::NumericLiteral("1".to_string())));
        assert_eq!(actual, expected);

        let actual = lexer.next();
        let expected = Some(Ok(Token::Plus));
        assert_eq!(actual, expected);

        let actual = lexer.next();
        let expected = Some(Ok(Token::NumericLiteral("2".to_string())));
        assert_eq!(actual, expected);

        let actual = lexer.next();
        let expected = None;
        assert_eq!(actual, expected);
    }

    #[test]
    fn create_table() {
        let input = "CREATE TABLE table1 (column1 INT, column2 VARCHAR(255))";
        let lexer = Lexer::new(input);
        let actual: Vec<Result<Token, LexError>> = lexer.collect();

        let expected = vec![
            Ok(Token::Create),
            Ok(Token::Table),
            Ok(Token::identifier("table1")),
            Ok(Token::OpenParen),
            Ok(Token::identifier("column1")),
            Ok(Token::Int),
            Ok(Token::Comma),
            Ok(Token::identifier("column2")),
            Ok(Token::VarChar),
            Ok(Token::OpenParen),
            Ok(Token::NumericLiteral("255".to_string())),
            Ok(Token::CloseParen),
            Ok(Token::CloseParen),
        ];

        assert_eq!(actual, expected);
    }
}
