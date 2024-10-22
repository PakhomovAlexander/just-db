#![allow(dead_code)]

pub mod lexer;

use lexer::tokens::Token;

///
/// TREE STRUCTURE
///
/// *Queries*:
///  select col1, col2 from table1;
///  insert into table1 (col1, col2) values (1, 'valStr');
///  insert into table1 (col1) values (1);
#[derive(Debug, PartialEq)]
struct Queries {
    queries: Vec<Query>,
}

#[derive(Debug, PartialEq)]
enum Query {
    // select ...
    Select(SelectQuery),
    // insert ...
    Insert(InsertQuery),
    // delete ...
    Delete(DeleteQuery),
    // update ...
    Update(UpdateQuery),
    // create table ...
    CreateTable(CreateTableQuery),
    // drop table ...
    DropTable(DropTableQuery),
    // alter table ... AlterTable(AlterTableQuery),
}

/// *SelectQuery*:
///  select <select_statement>
///  from <from_statement>
///  where <where_statement>
///  order by <order_by_statement>
///  group by <group_by_statement>
///  having <having_statement>
///  limit <limit_statement>
#[derive(Debug, PartialEq)]
struct SelectQuery {
    select_statement: SelectStatement,
    from_statement: FromStatement,
    where_statement: Option<WhereStatement>,
    order_by_statement: Option<OrderByStatement>,
    group_by_statement: Option<GroupByStatement>,
    having_statement: Option<HavingStatement>,
    limit_statement: Option<LimitStatement>,
}

/// *InsertQuery*:
///  insert into <table_name> (<columns>) values (<values>)
#[derive(Debug, PartialEq)]
struct InsertQuery {
    table_name: String,
    columns: Vec<String>,
    values: Vec<String>,
}

/// *DeleteQuery*:
///  delete from <table_name> where <where_statement>
#[derive(Debug, PartialEq)]
struct DeleteQuery {
    table_name: String,
    where_statement: Option<WhereStatement>,
}

/// *UpdateQuery*:
///  update <table_name> set <set_statement> where <where_statement>
#[derive(Debug, PartialEq)]
struct UpdateQuery {
    table_name: String,
    set_statement: SetStatement,
    where_statement: Option<WhereStatement>,
}

/// *CreateTableQuery*:
///  create table <table_name> (<columns_definitions>) <constraints>
#[derive(Debug, PartialEq)]
struct CreateTableQuery {
    table_name: String,
    // TODO: Design this
    columns_definitions: Vec<String>,
    // TODO: Design this
    constraints: Vec<String>,
}

/// *DropTableQuery*:
///  drop table <table_name>
#[derive(Debug, PartialEq)]
struct DropTableQuery {
    table_name: String,
}

/// *AlterTableQuery*:
///  alter table <table_name> <action>
#[derive(Debug, PartialEq)]
struct AlterTableQuery {
    table_name: String,
    // TODO: Design this
    action: String,
}

#[derive(Debug, PartialEq)]
struct SelectStatement {
    columns: Vec<ColumnStatement>,
    distinct: bool,
}

#[derive(Debug, PartialEq)]
struct FromStatement {
    tables: Vec<TableStatement>,
    joins: Vec<JoinStatement>,
}

#[derive(Debug, PartialEq)]
struct TableStatement {
    table_name: String,
    alias: Option<String>,
}

#[derive(Debug, PartialEq)]
struct JoinStatement {
    table: TableStatement,
    // TODO: Design this
    join_type: String,
    on: Vec<Condition>,
}

#[derive(Debug, PartialEq)]
struct Condition {
    left: ColumnStatement,
    operator: Operator,
    right: ColumnStatement,
}

#[derive(Debug, PartialEq)]
enum Operator {
    Equal,
    NotEqual,
}

#[derive(Debug, PartialEq)]
struct WhereStatement {
    conditions: Vec<Condition>,
}

#[derive(Debug, PartialEq)]
struct OrderByStatement {
    columns: Vec<ColumnStatement>,
    order: Order,
}

#[derive(Debug, PartialEq)]
enum Order {
    Asc,
    Desc,
}

#[derive(Debug, PartialEq)]
struct LimitStatement {
    limit: i32,
}

#[derive(Debug, PartialEq)]
struct GroupByStatement {
    columns: Vec<ColumnStatement>,
}

#[derive(Debug, PartialEq)]
struct HavingStatement {
    conditions: Vec<Condition>,
}

#[derive(Debug, PartialEq)]
struct SetStatement {
    // TODO: Design this
    columns: Vec<String>,
}

#[derive(Debug, PartialEq)]
enum ColumnStatement {
    Identifier(ColumnIdentifier),
    Literal(Literal),
    Function(Function),
}

#[derive(Debug, PartialEq)]
struct ColumnIdentifier {
    table_name: Option<String>,
    column_name: String,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Numeric(i32),
    String(String),
    Identifier {
        first_name: String,
        second_name: Option<String>,
        third_name: Option<String>,
    },
    Float(f32),
    Boolean(bool),
}

impl Literal {
    fn numeric(i: String) -> Literal {
        Literal::Numeric(i.parse().unwrap())
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

#[derive(Debug, PartialEq)]
pub struct ColumnList {
    columns: Vec<ColumnStatement>,
}

#[derive(Debug, PartialEq)]
pub struct TableList {
    tables: Vec<TableStatement>,
}

#[derive(Debug, PartialEq)]
pub struct Where {
    conditions: Vec<Condition>,
}

#[derive(Debug, PartialEq)]
struct Function {
    name: String,
    arguments: Vec<ColumnStatement>,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Leaf(Literal),

    Infix(Op, Vec<Node>),
    Prefix(Op, Vec<Node>),
    Postfix(Op, Vec<Node>),
}

pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
}

#[derive(Debug, PartialEq)]
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
            if let Some((l_bp, ())) = Self::postfix_operator_bp(&op) {
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

    fn prefix_operator_bp(op: &Op) -> ((), u8) {
        match op {
            Op::Not => ((), 7),
            _ => panic!("Unexpected prefix operator: {:?}", op),
        }
    }

    fn postfix_operator_bp(op: &Op) -> Option<(u8, ())> {
        match op {
            _ => None,
        }
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

    use super::lexer::Lexer;
    use pretty_assertions::assert_eq;

    fn id(identifier: &str) -> Literal {
        Literal::identifier(identifier)
    }

    fn num(i: i32) -> Literal {
        Literal::Numeric(i)
    }

    fn leaf(literal: Literal) -> Node {
        Node::Leaf(literal)
    }

    fn infix(op: Op, lhs: Node, rhs: Node) -> Node {
        Node::Infix(op, vec![lhs, rhs])
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
}
