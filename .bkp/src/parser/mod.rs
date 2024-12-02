#![allow(dead_code)]

pub mod lexer;

use lexer::tokens::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Numeric(i32),
    String(String),
    Identifier {
        first_name: String,
        second_name: Option<String>,
        third_name: Option<String>,
    },
    Float(f32),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type {
    Int,
    String,
    Bool,
}

impl Literal {
    fn numeric(i: String) -> Literal {
        Literal::Numeric(i.parse().unwrap())
    }

    fn string(s: String) -> Literal {
        Literal::String(s)
    }

    fn identifier(identifier: &str) -> Literal {
        let parts: Vec<&str> = identifier.split('.').collect();

        match parts.len() {
            1 => Literal::Identifier {
                first_name: parts[0].to_string(),
                second_name: None,
                third_name: None,
            },
            2 => Literal::Identifier {
                first_name: parts[0].to_string(),
                second_name: Some(parts[1].to_string()),
                third_name: None,
            },
            3 => Literal::Identifier {
                first_name: parts[0].to_string(),
                second_name: Some(parts[1].to_string()),
                third_name: Some(parts[2].to_string()),
            },
            _ => panic!("Invalid identifier: {}", identifier),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Leaf(Literal),
    LeafType(Type),

    Infix(Op, Vec<Node>),
    Prefix(Op, Vec<Node>),
    Postfix(Op, Vec<Node>),
}

impl Node {
    pub fn op(&self) -> Option<Op> {
        match self {
            Node::Infix(op, _) => Some(*op),
            Node::Prefix(op, _) => Some(*op),
            Node::Postfix(op, _) => Some(*op),
            _ => None,
        }
    }

    pub fn children(&self) -> Vec<Node> {
        match self {
            Node::Infix(_, children) => children.to_vec(),
            Node::Prefix(_, children) => children.to_vec(),
            Node::Postfix(_, children) => children.to_vec(),
            _ => vec![],
        }
    }

    pub fn literal(&self) -> Option<Literal> {
        match self {
            Node::Leaf(literal) => Some(literal.clone()),
            _ => None,
        }
    }
}

pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Op {
    And,
    Or,

    Plus,
    Minus,
    Multiply,
    Divide,

    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessThanOrEquals,
    GreaterThanOrEquals,
    Not,
    CloseParen,
    Comma,

    Select,
    From,
    Where,

    CreateTable,
    DropTable,
    InsertInto,

    ColumnDefinition,
    ColumnList,
    Values,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: lexer::Lexer<'a>) -> Self {
        Parser { lexer }
    }

    pub fn parse(&mut self) -> Node {
        self.parse_bp(0)
    }

    fn parse_bp(&mut self, min_bp: u8) -> Node {
        let mut lhs = match self.lexer.next() {
            Some(Ok(Token::NumericLiteral(i))) => Node::Leaf(Literal::numeric(i)),
            Some(Ok(Token::StringLiteral(s))) => Node::Leaf(Literal::string(s)),
            Some(Ok(Token::Identifier {
                first_name,
                second_name: None,
                third_name: None,
            })) => Node::Leaf(Literal::Identifier {
                first_name: first_name.to_string(),
                second_name: None,
                third_name: None,
            }),
            Some(Ok(Token::Not)) => {
                let ((), r_bp) = Self::prefix_operator_bp(&Op::Not);
                let rhs = self.parse_bp(r_bp);
                Node::Prefix(Op::Not, vec![rhs])
            }
            Some(Ok(Token::OpenParen)) => {
                let lhs = self.parse_bp(0);

                match self.lexer.next() {
                    Some(Ok(Token::CloseParen)) => lhs,
                    s => panic!("Unexpected token: {:?}", s),
                }
            }
            Some(Ok(Token::Select)) => self.parse_select(min_bp),
            Some(Ok(Token::Create)) => self.parse_create(min_bp),
            Some(Ok(Token::Drop)) => self.parse_drop(min_bp),
            Some(Ok(Token::Insert)) => self.parse_insert(min_bp),
            s => panic!("Unexpected token: {:?}", s),
        };

        loop {
            let op = match self.lexer.peek() {
                Some(Ok(Token::And)) => Op::And,
                Some(Ok(Token::Or)) => Op::Or,
                Some(Ok(Token::Plus)) => Op::Plus,
                Some(Ok(Token::Minus)) => Op::Minus,
                Some(Ok(Token::Asterisk)) => Op::Multiply,
                Some(Ok(Token::Slash)) => Op::Divide,
                Some(Ok(Token::Equals)) => Op::Equals,
                Some(Ok(Token::NotEquals)) => Op::NotEquals,
                Some(Ok(Token::LessThan)) => Op::LessThan,
                Some(Ok(Token::GreaterThan)) => Op::GreaterThan,
                Some(Ok(Token::LessThanOrEquals)) => Op::LessThanOrEquals,
                Some(Ok(Token::GreaterThanOrEquals)) => Op::GreaterThanOrEquals,
                Some(Ok(Token::CloseParen)) => Op::CloseParen,
                Some(Ok(Token::Comma)) => Op::Comma,
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

                lhs = Node::Infix(op, vec![lhs, rhs]);

                continue;
            }

            break;
        }

        lhs
    }

    fn parse_drop(&mut self, min_bp: u8) -> Node {
        match self.lexer.next() {
            Some(Ok(Token::Table)) => self.parse_drop_table(min_bp),
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_drop_table(&mut self, _min_bp: u8) -> Node {
        let lhs = match self.lexer.next() {
            Some(Ok(Token::Identifier {
                first_name,
                second_name: None,
                third_name: None,
            })) => Literal::identifier(first_name),
            s => panic!("Unexpected token: {:?}", s),
        };

        Node::Prefix(Op::DropTable, vec![Node::Leaf(lhs)])
    }

    fn parse_create(&mut self, min_bp: u8) -> Node {
        match self.lexer.next() {
            Some(Ok(Token::Table)) => self.parse_create_table(min_bp),
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_create_table(&mut self, _min_bp: u8) -> Node {
        let lhs = match self.lexer.next() {
            Some(Ok(Token::Identifier {
                first_name,
                second_name: None,
                third_name: None,
            })) => Literal::identifier(first_name),
            s => panic!("Unexpected token: {:?}", s),
        };

        match self.lexer.next() {
            Some(Ok(Token::OpenParen)) => {
                let mut columns = vec![];

                loop {
                    let column_name = match self.lexer.next() {
                        Some(Ok(Token::Identifier {
                            first_name,
                            second_name: None,
                            third_name: None,
                        })) => Literal::identifier(first_name),
                        Some(Ok(Token::CloseParen)) => break,
                        s => panic!("Unexpected token: {:?}", s),
                    };

                    match self.lexer.next() {
                        Some(Ok(Token::Int)) => {
                            columns.push(Node::Infix(
                                Op::ColumnDefinition,
                                vec![Node::Leaf(column_name), Node::LeafType(Type::Int)],
                            ));
                        }
                        s => panic!("Unexpected token: {:?}", s),
                    }

                    match self.lexer.next() {
                        Some(Ok(Token::Comma)) => continue,
                        Some(Ok(Token::CloseParen)) => break,
                        s => panic!("Unexpected token: {:?}", s),
                    }
                }

                if columns.len() == 1 {
                    Node::Prefix(
                        Op::CreateTable,
                        vec![Node::Leaf(lhs), columns.pop().unwrap()],
                    )
                } else {
                    Node::Prefix(
                        Op::CreateTable,
                        vec![Node::Leaf(lhs), Node::Infix(Op::Comma, columns)],
                    )
                }
            }
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_select(&mut self, min_bp: u8) -> Node {
        let rhs = self.parse_bp(0);

        match self.lexer.next() {
            Some(Ok(Token::From)) => Node::Prefix(Op::Select, vec![rhs, self.parse_from(min_bp)]),
            None => Node::Prefix(Op::Select, vec![rhs]),
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_from(&mut self, min_bp: u8) -> Node {
        let rhs = self.parse_bp(0);

        match self.lexer.next() {
            Some(Ok(Token::Where)) => Node::Prefix(Op::From, vec![rhs, self.parse_where(min_bp)]),
            None => Node::Prefix(Op::From, vec![rhs]),
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_where(&mut self, _min_bp: u8) -> Node {
        let rhs = self.parse_bp(0);

        match self.lexer.next() {
            None => Node::Prefix(Op::Where, vec![rhs]),
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_insert(&mut self, min_bp: u8) -> Node {
        match self.lexer.next() {
            Some(Ok(Token::Into)) => self.parse_insert_into(min_bp),
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_insert_into(&mut self, _min_bp: u8) -> Node {
        let lhs = match self.lexer.next() {
            Some(Ok(Token::Identifier {
                first_name,
                second_name: None,
                third_name: None,
            })) => Literal::identifier(first_name),
            s => panic!("Unexpected token: {:?}", s),
        };

        match self.lexer.next() {
            Some(Ok(Token::OpenParen)) => {
                let mut columns = vec![];

                loop {
                    let column_name = match self.lexer.next() {
                        Some(Ok(Token::Identifier {
                            first_name,
                            second_name: None,
                            third_name: None,
                        })) => Literal::identifier(first_name),
                        Some(Ok(Token::Comma)) => continue,
                        Some(Ok(Token::CloseParen)) => break,
                        s => panic!("Unexpected token: {:?}", s),
                    };

                    columns.push(Node::Leaf(column_name));
                }

                let values = match self.lexer.next() {
                    Some(Ok(Token::Values)) => self.parse_values(),
                    s => panic!("Unexpected token: {:?}", s),
                };

                Node::Prefix(
                    Op::InsertInto,
                    vec![
                        Node::Leaf(lhs),
                        Node::Prefix(Op::ColumnList, columns),
                        Node::Prefix(Op::Values, values),
                    ],
                )
            }
            s => panic!("Unexpected token: {:?}", s),
        }
    }

    fn parse_values(&mut self) -> Vec<Node> {
        match self.lexer.next() {
            Some(Ok(Token::OpenParen)) => {
                let mut values = vec![];

                loop {
                    let value = match self.lexer.next() {
                        Some(Ok(Token::NumericLiteral(i))) => Node::Leaf(Literal::numeric(i)),
                        Some(Ok(Token::CloseParen)) => break,
                        Some(Ok(Token::Comma)) => continue,
                        s => panic!("Unexpected token: {:?}", s),
                    };

                    values.push(value);
                }

                values
            }
            s => panic!("Unexpected token: {:?}", s),
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
}

#[cfg(test)]
mod tests {
    use crate::parser::{Literal, Node, Op, Parser};

    use super::{lexer::Lexer, Type};
    use pretty_assertions::assert_eq;

    fn id(identifier: &str) -> Literal {
        Literal::identifier(identifier)
    }

    fn num(i: i32) -> Literal {
        Literal::Numeric(i)
    }

    fn string(i: &str) -> Literal {
        Literal::String(i.to_string())
    }

    fn leaf(literal: Literal) -> Node {
        Node::Leaf(literal)
    }

    fn leaf_type(typ: Type) -> Node {
        Node::LeafType(typ)
    }

    fn infix(op: Op, lhs: Node, rhs: Node) -> Node {
        Node::Infix(op, vec![lhs, rhs])
    }

    fn infix_vec(op: Op, nodes: Vec<Node>) -> Node {
        Node::Infix(op, nodes)
    }

    fn prefix_vec(op: Op, nodes: Vec<Node>) -> Node {
        Node::Prefix(op, nodes)
    }

    fn prefix(op: Op, lhs: Node) -> Node {
        Node::Prefix(op, vec![lhs])
    }

    fn prefix_chain(op: Op, lhs: Node, chain_node: Node) -> Node {
        Node::Prefix(op, vec![lhs, chain_node])
    }

    fn postfix(op: Op, rhs: Node) -> Node {
        Node::Postfix(op, vec![rhs])
    }

    fn parse(input: &str) -> Node {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        parser.parse()
    }

    #[test]
    fn mininmal_expression_parser() {
        assert_eq!(parse("1"), leaf(num(1)));
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
            dbg!(parse(input)),
            prefix_chain(
                Op::CreateTable,
                leaf(id("table1")),
                infix(Op::ColumnDefinition, leaf(id("col1")), leaf_type(Type::Int))
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
                    infix(Op::ColumnDefinition, leaf(id("col1")), leaf_type(Type::Int)),
                    infix(Op::ColumnDefinition, leaf(id("col2")), leaf_type(Type::Int))
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
                        infix(Op::ColumnDefinition, leaf(id("col1")), leaf_type(Type::Int)),
                        infix(Op::ColumnDefinition, leaf(id("col2")), leaf_type(Type::Int)),
                        infix(Op::ColumnDefinition, leaf(id("col3")), leaf_type(Type::Int))
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
