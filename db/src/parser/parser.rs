use crate::parser::errors::ParseError;
use crate::parser::lexer::Lexer;
use crate::parser::lexer::Token;
use crate::types::ColType;

use super::lexer::LexError;
use super::lexer::PositionedToken;
use super::tree::{Literal, Node, Op};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser { lexer }
    }

    pub fn parse(&mut self) -> Result<Node, ParseError> {
        self.parse_bp(0)
    }

    fn parse_bp(&mut self, min_bp: u8) -> Result<Node, ParseError> {
        let pos_token = self.lexer.next();

        let token = self.try_extract(&pos_token);
        if token.is_none() {
            return self.parse_err("Unexpected end of input");
        }

        let mut lhs = match token.unwrap() {
            Token::NumericLiteral(i) => Ok(Node::Leaf(Literal::numeric(i))),
            Token::StringLiteral(s) => Ok(Node::Leaf(Literal::string(s))),
            Token::Identifier {
                first_name,
                second_name: None,
                third_name: None,
            } => Ok(Node::Leaf(Literal::Identifier {
                first_name: first_name.to_string(),
                second_name: None,
                third_name: None,
            })),
            Token::Not => {
                let ((), r_bp) = Self::prefix_operator_bp(&Op::Not);
                let rhs = self.parse_bp(r_bp);
                Ok(Node::Prefix(Op::Not, vec![rhs]))
            }
            Token::OpenParen => {
                let lhs = self.parse_bp(0);

                let p_token = self.lexer.next();
                let token = self.try_extract(&p_token).unwrap();

                match token {
                    Token::CloseParen => lhs,
                    s => return self.unexpected_token_err(s),
                }
            }
            Token::Select => self.parse_select(min_bp),
            Token::Create => self.parse_create(min_bp),
            Token::Drop => self.parse_drop(min_bp),
            Token::Insert => self.parse_insert(min_bp),
            s => return self.unexpected_token_err(s),
        };

        loop {
            let p_token = self.lexer.peek();
            let token = self.try_extract(&p_token);
            if token.is_none() {
                break;
            }
            let token = token.unwrap();

            let op = match token {
                Token::And => Op::And,
                Token::Or => Op::Or,
                Token::Plus => Op::Plus,
                Token::Minus => Op::Minus,
                Token::Asterisk => Op::Multiply,
                Token::Slash => Op::Divide,
                Token::Equals => Op::Equals,
                Token::NotEquals => Op::NotEquals,
                Token::LessThan => Op::LessThan,
                Token::GreaterThan => Op::GreaterThan,
                Token::LessThanOrEquals => Op::LessThanOrEquals,
                Token::GreaterThanOrEquals => Op::GreaterThanOrEquals,
                Token::CloseParen => Op::CloseParen,
                Token::Comma => Op::Comma,
                _ => break,
            };

            // postfix bp
            if let Some((_l_bp, ())) = Self::postfix_operator_bp(&op) {
                // operate with postfix
                continue;
            }

            if let Some((l_bp, r_bp)) = Self::infix_operator_bp(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();

                let rhs = self.parse_bp(r_bp);

                lhs = Ok(Node::Infix(op, vec![lhs, rhs]));

                continue;
            }

            break;
        }

        lhs
    }

    fn parse_drop(&mut self, min_bp: u8) -> Result<Node, ParseError> {
        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token).unwrap();
        match token {
            Token::Table => self.parse_drop_table(min_bp),
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_drop_table(&mut self, _min_bp: u8) -> Result<Node, ParseError> {
        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token).unwrap();

        let lhs = match token {
            Token::Identifier {
                first_name,
                second_name: None,
                third_name: None,
            } => Literal::identifier(first_name),
            s => return self.unexpected_token_err(s),
        };

        Ok(Node::Prefix(Op::DropTable, vec![Ok(Node::Leaf(lhs))]))
    }

    fn parse_create(&mut self, min_bp: u8) -> Result<Node, ParseError> {
        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token).unwrap();

        match token {
            Token::Table => self.parse_create_table(min_bp),
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_create_table(&mut self, _min_bp: u8) -> Result<Node, ParseError> {
        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token).unwrap();

        let lhs = match token {
            Token::Identifier {
                first_name,
                second_name: None,
                third_name: None,
            } => Literal::identifier(first_name),
            s => return self.unexpected_token_err(s),
        };

        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token).unwrap();

        match token {
            Token::OpenParen => {
                let mut columns = vec![];

                loop {
                    let p_token = self.lexer.next();
                    let token = self.try_extract(&p_token).unwrap();

                    let column_name = match token {
                        Token::Identifier {
                            first_name,
                            second_name: None,
                            third_name: None,
                        } => Literal::identifier(first_name),
                        Token::CloseParen => break,
                        s => return self.unexpected_token_err(s),
                    };

                    let p_token = self.lexer.next();
                    let token = self.try_extract(&p_token).unwrap();

                    match token {
                        Token::Int => {
                            columns.push(Ok(Node::Infix(
                                Op::ColumnDefinition,
                                vec![
                                    Ok(Node::Leaf(column_name)),
                                    Ok(Node::LeafType(ColType::Int)),
                                ],
                            )));
                        }
                        s => return self.unexpected_token_err(s),
                    }

                    let p_token = self.lexer.next();
                    let token = self.try_extract(&p_token).unwrap();

                    match token {
                        Token::Comma => continue,
                        Token::CloseParen => break,
                        s => return self.unexpected_token_err(s),
                    }
                }

                if columns.len() == 1 {
                    Ok(Node::Prefix(
                        Op::CreateTable,
                        vec![Ok(Node::Leaf(lhs)), columns.pop().unwrap()],
                    ))
                } else {
                    Ok(Node::Prefix(
                        Op::CreateTable,
                        vec![Ok(Node::Leaf(lhs)), Ok(Node::Infix(Op::Comma, columns))],
                    ))
                }
            }
            s => return self.unexpected_token_err(s),
        }
    }

    fn unexpected_token_err(&mut self, token: Token) -> Result<Node, ParseError> {
        self.parse_err(&format!("Unexpected token: {:?}", token))
    }

    fn unexpected_operator_err(&mut self, op: Op) -> Result<Node, ParseError> {
        self.parse_err(&format!("Unexpected operator: {:?}", op))
    }

    fn parse_err(&mut self, msg: &str) -> Result<Node, ParseError> {
        Err(ParseError {
            src: self.lexer.input.to_string(),
            message: msg.to_string(),
            snip: (0, 0),
        })
    }

    fn parse_select(&mut self, min_bp: u8) -> Result<Node, ParseError> {
        let rhs = self.parse_bp(0);

        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token);

        match token {
            Some(Token::From) => Ok(Node::Prefix(Op::Select, vec![rhs, self.parse_from(min_bp)])),
            None => Ok(Node::Prefix(Op::Select, vec![rhs])),
            Some(s) => self.unexpected_token_err(s),
        }
    }

    fn parse_from(&mut self, min_bp: u8) -> Result<Node, ParseError> {
        let rhs = self.parse_bp(0);

        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token);

        match token {
            Some(Token::Where) => Ok(Node::Prefix(Op::From, vec![rhs, self.parse_where(min_bp)])),
            None => Ok(Node::Prefix(Op::From, vec![rhs])),
            Some(s) => self.unexpected_token_err(s),
        }
    }

    fn parse_where(&mut self, _min_bp: u8) -> Result<Node, ParseError> {
        let rhs = self.parse_bp(0);

        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token);

        match token {
            None => Ok(Node::Prefix(Op::Where, vec![rhs])),
            Some(s) => self.unexpected_token_err(s),
        }
    }

    fn parse_insert(&mut self, min_bp: u8) -> Result<Node, ParseError> {
        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token).unwrap();

        match token {
            Token::Into => self.parse_insert_into(min_bp),
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_insert_into(&mut self, _min_bp: u8) -> Result<Node, ParseError> {
        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token).unwrap();

        let lhs = match token {
            Token::Identifier {
                first_name,
                second_name: None,
                third_name: None,
            } => Literal::identifier(first_name),
            s => panic!("Unexpected token: {:?}", s),
        };

        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token).unwrap();
        match token {
            Token::OpenParen => {
                let mut columns = vec![];

                loop {
                    let p_token = self.lexer.next();
                    let token = self.try_extract(&p_token).unwrap();

                    let column_name = match token {
                        Token::Identifier {
                            first_name,
                            second_name: None,
                            third_name: None,
                        } => Literal::identifier(first_name),
                        Token::Comma => continue,
                        Token::CloseParen => break,
                        s => panic!("Unexpected token: {:?}", s),
                    };

                    columns.push(Ok(Node::Leaf(column_name)));
                }

                let p_token = self.lexer.next();
                let token = self.try_extract(&p_token).unwrap();
                let values = match token {
                    Token::Values => self.parse_values(),
                    s => panic!("Unexpected token: {:?}", s),
                };

                Ok(Node::Prefix(
                    Op::InsertInto,
                    vec![
                        Ok(Node::Leaf(lhs)),
                        Ok(Node::Prefix(Op::ColumnList, columns)),
                        Ok(Node::Prefix(Op::Values, values)),
                    ],
                ))
            }
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_values(&mut self) -> Vec<Result<Node, ParseError>> {
        let p_token = self.lexer.next();
        let token = self.try_extract(&p_token).unwrap();

        match token {
            Token::OpenParen => {
                let mut values = vec![];

                loop {
                    let p_token = self.lexer.next();
                    let token = self.try_extract(&p_token).unwrap();
                    let value = match token {
                        Token::NumericLiteral(i) => Ok(Node::Leaf(Literal::numeric(i))),
                        Token::CloseParen => break,
                        Token::Comma => continue,
                        s => self.unexpected_token_err(s),
                    };

                    values.push(value);
                }

                values
            }
            s => vec![self.unexpected_token_err(s)],
        }
    }

    fn prefix_operator_bp(op: &Op) -> ((), u8) {
        match op {
            Op::Not => ((), 7),
            _ => panic!("Unexpected prefix operator: {:?}", op),
        }
    }

    fn postfix_operator_bp(_op: &Op) -> Option<(u8, ())> {
        None
    }

    fn infix_operator_bp(op: &Op) -> Option<(u8, u8)> {
        match op {
            Op::Or => Some((1, 2)),
            Op::And => Some((3, 4)),

            Op::Equals => Some((4, 5)),
            Op::NotEquals => Some((4, 5)),
            Op::LessThan => Some((4, 5)),
            Op::GreaterThan => Some((4, 5)),
            Op::LessThanOrEquals => Some((4, 5)),
            Op::GreaterThanOrEquals => Some((4, 5)),

            Op::Comma => Some((4, 5)),
            Op::Plus => Some((6, 7)),
            Op::Minus => Some((6, 7)),

            Op::Multiply => Some((8, 9)),
            Op::Divide => Some((8, 9)),

            Op::CloseParen => None,

            _ => panic!("Unexpected infix operator: {:?}", op),
        }
    }

    fn try_extract(
        &self,
        p_token: &Option<Result<PositionedToken<'a>, LexError>>,
    ) -> Option<Token<'a>> {
        match p_token {
            Some(Ok(t)) => Some(t.token.clone()), // FIXME: clone
            _ => {
                dbg!(p_token);
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        parser::{
            errors::ParseError,
            tree::{Literal, Node, Op},
            Lexer,
        },
        types::ColType,
    };

    use super::Parser;

    fn id(identifier: &str) -> Literal {
        Literal::identifier(identifier)
    }

    fn num(i: i32) -> Literal {
        Literal::Numeric(i)
    }

    fn string(i: &str) -> Literal {
        Literal::String(i.to_string())
    }

    fn leaf(literal: Literal) -> Result<Node, ParseError> {
        Ok(Node::Leaf(literal))
    }

    fn leaf_type(typ: ColType) -> Result<Node, ParseError> {
        Ok(Node::LeafType(typ))
    }

    fn infix(
        op: Op,
        lhs: Result<Node, ParseError>,
        rhs: Result<Node, ParseError>,
    ) -> Result<Node, ParseError> {
        Ok(Node::Infix(op, vec![lhs, rhs]))
    }

    fn infix_vec(op: Op, nodes: Vec<Result<Node, ParseError>>) -> Result<Node, ParseError> {
        Ok(Node::Infix(op, nodes))
    }

    fn prefix_vec(op: Op, nodes: Vec<Result<Node, ParseError>>) -> Result<Node, ParseError> {
        Ok(Node::Prefix(op, nodes))
    }

    fn prefix(op: Op, lhs: Result<Node, ParseError>) -> Result<Node, ParseError> {
        Ok(Node::Prefix(op, vec![lhs]))
    }

    fn prefix_chain(
        op: Op,
        lhs: Result<Node, ParseError>,
        chain_node: Result<Node, ParseError>,
    ) -> Result<Node, ParseError> {
        Ok(Node::Prefix(op, vec![lhs, chain_node]))
    }

    fn postfix(op: Op, rhs: Node) -> Result<Node, ParseError> {
        Ok(Node::Postfix(op, vec![Ok(rhs)]))
    }

    fn parse(input: &str) -> Result<Node, ParseError> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse()
    }

    #[test]
    fn mininmal_expression_parser() {
        assert_eq!(parse("1"), leaf(num(1)));
    }

    #[test]
    #[ignore] // TODO: implement error handling
    fn unexpected_token() {
        let input = "1 2";

        let err = parse("(1 +");
    }

    #[test]
    fn mininmal_expression_parser_str() {
        assert_eq!(parse("'I am a string!'"), leaf(string("I am a string!")));
    }

    #[test]
    fn expression_parser() {
        assert_eq!(parse("1 + 2"), infix(Op::Plus, leaf(num(1)), leaf(num(2))));
    }

    #[test]
    fn expression_parser_with_precedence() {
        assert_eq!(
            parse("1 + 2 * 3"),
            infix(
                Op::Plus,
                leaf(num(1)),
                infix(Op::Multiply, leaf(num(2)), leaf(num(3)))
            )
        );
    }

    #[test]
    fn parse_reverse() {
        assert_eq!(
            parse("2 * 3 + 4"),
            infix(
                Op::Plus,
                infix(Op::Multiply, leaf(num(2)), leaf(num(3))),
                leaf(num(4))
            )
        );
    }

    #[test]
    fn parse_more() {
        assert_eq!(
            parse("1 + 2 * 3 * 4 + 5"),
            infix(
                Op::Plus,
                infix(
                    Op::Plus,
                    leaf(num(1)),
                    infix(
                        Op::Multiply,
                        infix(Op::Multiply, leaf(num(2)), leaf(num(3))),
                        leaf(num(4))
                    )
                ),
                leaf(num(5))
            )
        );
    }

    #[test]
    fn simple_where_expr() {
        assert_eq!(
            parse("1 = 1"),
            infix(Op::Equals, leaf(num(1)), leaf(num(1)))
        );
    }

    #[test]
    fn where_expr() {
        assert_eq!(
            parse("1 = 1 and 2 >= 3 * 1 or 4 < 5 + 6"),
            infix(
                Op::Or,
                infix(
                    Op::And,
                    infix(Op::Equals, leaf(num(1)), leaf(num(1))),
                    infix(
                        Op::GreaterThanOrEquals,
                        leaf(num(2)),
                        infix(Op::Multiply, leaf(num(3)), leaf(num(1)))
                    )
                ),
                infix(
                    Op::LessThan,
                    leaf(num(4)),
                    infix(Op::Plus, leaf(num(5)), leaf(num(6)))
                )
            )
        );
    }

    #[test]
    fn prefix_operator_not() {
        assert_eq!(
            parse("not 1 = 1"),
            infix(Op::Equals, prefix(Op::Not, leaf(num(1))), leaf(num(1)))
        );
    }

    #[test]
    fn where_expr_with_parentheses() {
        assert_eq!(
            parse("(1 + 2) * 3"),
            infix(
                Op::Multiply,
                infix(Op::Plus, leaf(num(1)), leaf(num(2))),
                leaf(num(3))
            )
        );
    }

    #[test]
    #[ignore]
    // TODO: implement function call
    fn postfix_function_call() {
        let input = "func(1, 2)";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse();
    }

    #[test]
    fn where_expr_with_parentheses_2() {
        assert_eq!(
            parse("(1 = 1 and 2 >= 3) * 1 or 4 < 5 + 6"),
            infix(
                Op::Or,
                infix(
                    Op::Multiply,
                    infix(
                        Op::And,
                        infix(Op::Equals, leaf(num(1)), leaf(num(1))),
                        infix(Op::GreaterThanOrEquals, leaf(num(2)), leaf(num(3)))
                    ),
                    leaf(num(1))
                ),
                infix(
                    Op::LessThan,
                    leaf(num(4)),
                    infix(Op::Plus, leaf(num(5)), leaf(num(6)))
                )
            )
        );
    }

    #[test]
    fn select_query_without_from() {
        assert_eq!(parse("select 1"), prefix(Op::Select, leaf(num(1))));
    }

    #[test]
    fn select_query_with_from() {
        assert_eq!(
            parse("select 1 from table1"),
            prefix_chain(
                Op::Select,
                leaf(num(1)),
                prefix(Op::From, leaf(id("table1")))
            )
        );
    }

    #[test]
    fn select_query_many_columns() {
        assert_eq!(
            parse("select col1, col2 from table1"),
            prefix_chain(
                Op::Select,
                infix(Op::Comma, leaf(id("col1")), leaf(id("col2"))),
                prefix(Op::From, leaf(id("table1")))
            )
        );
    }

    #[test]
    fn select_query_many_commas() {
        assert_eq!(
            parse("select col1, col2, 1 + 1 from table1, table2"),
            prefix_chain(
                Op::Select,
                infix(
                    Op::Comma,
                    infix(Op::Comma, leaf(id("col1")), leaf(id("col2"))),
                    infix(Op::Plus, leaf(num(1)), leaf(num(1)))
                ),
                prefix(
                    Op::From,
                    infix(Op::Comma, leaf(id("table1")), leaf(id("table2")))
                )
            )
        );
    }

    #[test]
    fn select_query_with_where() {
        assert_eq!(
            parse("select col1 from table1 where col1 = 1"),
            prefix_chain(
                Op::Select,
                leaf(id("col1")),
                prefix_chain(
                    Op::From,
                    leaf(id("table1")),
                    prefix(
                        Op::Where,
                        infix(Op::Equals, leaf(id("col1")), leaf(num(1)),)
                    )
                )
            )
        );
    }

    #[test]
    fn select_query_with_where_and_and() {
        let input = r"
        select col1, col2, col3 
        from table1, table2 
        where (col1 = 1 or col2 = 2) and col3 >= 3";

        assert_eq!(
            parse(input),
            prefix_chain(
                Op::Select,
                infix(
                    Op::Comma,
                    infix(Op::Comma, leaf(id("col1")), leaf(id("col2"))),
                    leaf(id("col3"))
                ),
                prefix_chain(
                    Op::From,
                    infix(Op::Comma, leaf(id("table1")), leaf(id("table2"))),
                    prefix(
                        Op::Where,
                        infix(
                            Op::And,
                            infix(
                                Op::Or,
                                infix(Op::Equals, leaf(id("col1")), leaf(num(1))),
                                infix(Op::Equals, leaf(id("col2")), leaf(num(2)))
                            ),
                            infix(Op::GreaterThanOrEquals, leaf(id("col3")), leaf(num(3)))
                        )
                    )
                )
            )
        );
    }

    #[test]
    fn create_table_with_single_column() {
        let input = r"
        create table table1 (
            col1 int
        )";

        assert_eq!(
            parse(input),
            prefix_chain(
                Op::CreateTable,
                leaf(id("table1")),
                infix(
                    Op::ColumnDefinition,
                    leaf(id("col1")),
                    leaf_type(ColType::Int)
                )
            )
        );
    }

    #[test]
    fn create_table_two_columns() {
        let input = r"
        create table table1 (
            col1 int,
            col2 int
        )";

        assert_eq!(
            parse(input),
            prefix_chain(
                Op::CreateTable,
                leaf(id("table1")),
                infix(
                    Op::Comma,
                    infix(
                        Op::ColumnDefinition,
                        leaf(id("col1")),
                        leaf_type(ColType::Int)
                    ),
                    infix(
                        Op::ColumnDefinition,
                        leaf(id("col2")),
                        leaf_type(ColType::Int)
                    )
                )
            )
        );
    }

    #[test]
    fn create_table_three_columns() {
        let input = r"
        create table table1 (
            col1 int,
            col2 int,
            col3 int
        )";

        assert_eq!(
            parse(input),
            prefix_chain(
                Op::CreateTable,
                leaf(id("table1")),
                infix_vec(
                    Op::Comma,
                    vec![
                        infix(
                            Op::ColumnDefinition,
                            leaf(id("col1")),
                            leaf_type(ColType::Int)
                        ),
                        infix(
                            Op::ColumnDefinition,
                            leaf(id("col2")),
                            leaf_type(ColType::Int)
                        ),
                        infix(
                            Op::ColumnDefinition,
                            leaf(id("col3")),
                            leaf_type(ColType::Int)
                        )
                    ]
                )
            )
        );
    }

    #[test]
    fn drop_table() {
        assert_eq!(
            parse("drop table table1"),
            prefix(Op::DropTable, leaf(id("table1")),)
        );
    }

    #[test]
    fn insert_into_single_column() {
        let input = r"
        insert into table1 (col1) values (1)";

        assert_eq!(
            parse(input),
            prefix_vec(
                Op::InsertInto,
                vec![
                    leaf(id("table1")),
                    prefix_vec(Op::ColumnList, vec![leaf(id("col1"))]),
                    prefix_vec(Op::Values, vec![leaf(num(1))])
                ]
            )
        );
    }

    #[test]
    fn insert_into() {
        let input = r"
        insert into table1 (col1, col2) values (1, 2)";

        assert_eq!(
            parse(input),
            prefix_vec(
                Op::InsertInto,
                vec![
                    leaf(id("table1")),
                    prefix_vec(Op::ColumnList, vec![leaf(id("col1")), leaf(id("col2"))]),
                    prefix_vec(Op::Values, vec![leaf(num(1)), leaf(num(2))])
                ]
            )
        );
    }
}
