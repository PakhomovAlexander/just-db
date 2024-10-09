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
    Float(f32),
    Boolean(bool),
}

impl Literal {
    fn numeric(i: String) -> Literal {
        Literal::Numeric(i.parse().unwrap())
    }
}

#[derive(Debug, PartialEq)]
struct Function {
    name: String,
    arguments: Vec<ColumnStatement>,
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Leaf(Literal),
    Inner(Op, Box<Node>, Box<Node>),
    Prefix(Op, Box<Node>),
    Postfix(Op, Box<Node>),
}

pub struct Parser<'a> {
    lexer: lexer::Lexer<'a>,
}

impl Node {
    fn prefix(op: Op, node: Node) -> Node {
        Node::Prefix(op, Box::new(node))
    }

    fn postfix(op: Op, node: Node) -> Node {
        Node::Postfix(op, Box::new(node))
    }

    fn inner(op: Op, left: Node, right: Node) -> Node {
        Node::Inner(op, Box::new(left), Box::new(right))
    }

    fn leaf(literal: Literal) -> Node {
        Node::Leaf(literal)
    }
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
}

impl<'a> Parser<'a> {
    pub fn new(lexer: lexer::Lexer<'a>) -> Self {
        Parser { lexer }
    }

    pub fn parse(&mut self) -> Node {
        dbg!(self.parse_bp(0))
    }

    // (1 + 2) * 3
    fn parse_bp(&mut self, min_bp: u8) -> Node {
        dbg!("REQ");
        dbg!(min_bp, self.lexer.peek());
        let mut lhs = match self.lexer.next() {
            Some(Ok(Token::NumericLiteral(i))) => Node::leaf(Literal::numeric(i)),
            Some(Ok(Token::Not)) => {
                let ((), r_bp) = Self::prefix_operator_bp(&Op::Not);
                let rhs = self.parse_bp(r_bp);
                Node::prefix(Op::Not, rhs)
            }
            Some(Ok(Token::OpenParen)) => {
                dbg!("OpenParen found");
                let lhs = self.parse_bp(0);

                match self.lexer.next() {
                    Some(Ok(Token::CloseParen)) => {
                        dbg!("CloseParen found");
                        lhs
                    }
                    s => panic!("Unexpected token: {:?}", s),
                }
            }
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
                _ => break,
            };

            dbg!(&op);

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

                lhs = Node::inner(op, lhs, rhs);

                continue;
            }

            break;
        }

        lhs
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

    #[test]
    fn mininmal_expression_parser() {
        let input = "1";
        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(parse_tree, Node::leaf(Literal::Numeric(1)));
    }

    #[test]
    fn expression_parser() {
        let input = "1 + 2";
        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(
            parse_tree,
            Node::inner(
                Op::Plus,
                Node::leaf(Literal::Numeric(1)),
                Node::leaf(Literal::Numeric(2))
            )
        );
    }

    #[test]
    fn expression_parser_with_precedence() {
        let input = "1 + 2 * 3";
        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(
            parse_tree,
            Node::inner(
                Op::Plus,
                Node::leaf(Literal::Numeric(1)),
                Node::inner(
                    Op::Multiply,
                    Node::leaf(Literal::Numeric(2)),
                    Node::leaf(Literal::Numeric(3))
                )
            )
        );
    }

    #[test]
    fn parse_reverse() {
        let input = "2 * 3 + 4";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(
            parse_tree,
            Node::inner(
                Op::Plus,
                Node::inner(
                    Op::Multiply,
                    Node::leaf(Literal::Numeric(2)),
                    Node::leaf(Literal::Numeric(3))
                ),
                Node::leaf(Literal::Numeric(4))
            )
        );
    }

    #[test]
    fn parse_more() {
        let input = "1 + 2 * 3 * 4 + 5";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(
            parse_tree,
            Node::inner(
                Op::Plus,
                Node::inner(
                    Op::Plus,
                    Node::leaf(Literal::Numeric(1)),
                    Node::inner(
                        Op::Multiply,
                        Node::inner(
                            Op::Multiply,
                            Node::leaf(Literal::Numeric(2)),
                            Node::leaf(Literal::Numeric(3))
                        ),
                        Node::leaf(Literal::Numeric(4))
                    )
                ),
                Node::leaf(Literal::Numeric(5))
            )
        );
    }

    #[test]
    fn simple_where_expr() {
        let input = "1 = 1";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(
            parse_tree,
            Node::inner(
                Op::Equals,
                Node::leaf(Literal::Numeric(1)),
                Node::leaf(Literal::Numeric(1))
            )
        );
    }

    #[test]
    fn where_expr() {
        let input = "1 = 1 and 2 >= 3 * 1 or 4 < 5 + 6";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(
            parse_tree,
            Node::inner(
                Op::Or,
                Node::inner(
                    Op::And,
                    Node::inner(
                        Op::Equals,
                        Node::leaf(Literal::Numeric(1)),
                        Node::leaf(Literal::Numeric(1))
                    ),
                    Node::inner(
                        Op::GreaterThanOrEquals,
                        Node::leaf(Literal::Numeric(2)),
                        Node::inner(
                            Op::Multiply,
                            Node::leaf(Literal::Numeric(3)),
                            Node::leaf(Literal::Numeric(1))
                        )
                    )
                ),
                Node::inner(
                    Op::LessThan,
                    Node::leaf(Literal::Numeric(4)),
                    Node::inner(
                        Op::Plus,
                        Node::leaf(Literal::Numeric(5)),
                        Node::leaf(Literal::Numeric(6))
                    )
                )
            )
        );
    }

    #[test]
    fn prefix_operator_not() {
        let input = "not 1 = 1";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(
            parse_tree,
            Node::inner(
                Op::Equals,
                Node::prefix(Op::Not, Node::leaf(Literal::Numeric(1))),
                Node::leaf(Literal::Numeric(1))
            )
        );
    }

    #[test]
    fn where_expr_with_parentheses() {
        let input = "(1 + 2) * 3";

        let lexer = Lexer::new(input);

        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(
            parse_tree,
            Node::inner(
                Op::Multiply,
                Node::inner(
                    Op::Plus,
                    Node::leaf(Literal::Numeric(1)),
                    Node::leaf(Literal::Numeric(2))
                ),
                Node::leaf(Literal::Numeric(3))
            )
        );
    }

    #[test]
    #[ignore]
    fn postfix_function_call() {
        let input = "func(1, 2)";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let _ = parser.parse();
    }

    #[test]
    fn where_expr_with_parentheses_2() {
        let input = "(1 = 1 and 2 >= 3) * 1 or 4 < 5 + 6";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let parse_tree = parser.parse();

        assert_eq!(
            parse_tree,
            Node::inner(
                Op::Or,
                Node::inner(
                    Op::Multiply,
                    Node::inner(
                        Op::And,
                        Node::inner(
                            Op::Equals,
                            Node::leaf(Literal::Numeric(1)),
                            Node::leaf(Literal::Numeric(1))
                        ),
                        Node::inner(
                            Op::GreaterThanOrEquals,
                            Node::leaf(Literal::Numeric(2)),
                            Node::leaf(Literal::Numeric(3))
                        )
                    ),
                    Node::leaf(Literal::Numeric(1))
                ),
                Node::inner(
                    Op::LessThan,
                    Node::leaf(Literal::Numeric(4)),
                    Node::inner(
                        Op::Plus,
                        Node::leaf(Literal::Numeric(5)),
                        Node::leaf(Literal::Numeric(6))
                    )
                )
            )
        );
    }
}
