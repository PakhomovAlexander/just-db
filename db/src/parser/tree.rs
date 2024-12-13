use crate::types::ColType;

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
