use std::fmt::Display;

use crate::types::ColType;

use super::errors::ParseError;

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

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Numeric(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "{}", s),
            Literal::Identifier {
                first_name,
                second_name,
                third_name,
            } => {
                if let Some(second_name) = second_name {
                    if let Some(third_name) = third_name {
                        write!(f, "{}.{}.{}", first_name, second_name, third_name)
                    } else {
                        write!(f, "{}.{}", first_name, second_name)
                    }
                } else {
                    write!(f, "{}", first_name)
                }
            }
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl Literal {
    pub fn numeric(i: String) -> Literal {
        Literal::Numeric(i.parse().unwrap())
    }

    pub fn string(s: String) -> Literal {
        Literal::String(s)
    }

    pub fn identifier(identifier: &str) -> Literal {
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
    LeafType(ColType),

    Infix(Op, Vec<Result<Node, ParseError>>),
    Prefix(Op, Vec<Result<Node, ParseError>>),
    Postfix(Op, Vec<Result<Node, ParseError>>),
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

    pub fn children(&self) -> Vec<Result<Node, ParseError>> {
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

    pub fn ttype(&self) -> Option<ColType> {
        match self {
            Node::LeafType(typ) => Some(*typ),
            _ => None,
        }
    }
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

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::And => write!(f, "AND"),
            Op::Or => write!(f, "OR"),
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Multiply => write!(f, "*"),
            Op::Divide => write!(f, "/"),
            Op::Equals => write!(f, "="),
            Op::NotEquals => write!(f, "<>"),
            Op::LessThan => write!(f, "<"),
            Op::GreaterThan => write!(f, ">"),
            Op::LessThanOrEquals => write!(f, "<="),
            Op::GreaterThanOrEquals => write!(f, ">="),
            Op::Not => write!(f, "NOT"),
            Op::CloseParen => write!(f, ")"),
            Op::Comma => write!(f, ","),
            Op::Select => write!(f, "SELECT"),
            Op::From => write!(f, "FROM"),
            Op::Where => write!(f, "WHERE"),
            Op::CreateTable => write!(f, "CREATE TABLE"),
            Op::DropTable => write!(f, "DROP TABLE"),
            Op::InsertInto => write!(f, "INSERT INTO"),
            Op::ColumnDefinition => write!(f, "COLUMN DEFINITION"),
            Op::ColumnList => write!(f, "COLUMN LIST"),
            Op::Values => write!(f, "VALUES"),
        }
    }
}
