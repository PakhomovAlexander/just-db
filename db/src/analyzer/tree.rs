use std::fmt::Display;

use crate::types::ColType;

//#[allow(clippy::unused)]
#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Projec {
        columns: Vec<Column>,
    },
    Filter {
        node: Box<LogicalNode>,
    },
    Read {
        table: Table,
    },
    Join(),
    Group(),
    Sort(),
    Limit(),
    Distinct(),

    Add {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Sub {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Mul {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Div {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Eq {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Neq {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Lt {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Lte {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Gt {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },

    Gte {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    And {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Or {
        left: Box<LogicalNode>,
        right: Box<LogicalNode>,
    },
    Not,
    In,
    Like,
    IsNull,
    IsNotNull,
    Exists,
    NotExists,
    Between,
    Case,
    Cast,

    Const(Constant),
    Col(Column),

    CreateTable {
        table_name: String,
        columns: Vec<ColumnDefinition>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct Column {
    pub column_name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ColumnDefinition {
    pub column_name: String,
    pub column_type: ColType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Num(i32),
    Str(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Table {
    pub table_name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LogicalNode {
    pub op: Operator,
    pub children: Vec<LogicalNode>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LogicalPlan {
    pub root: LogicalNode,
}

impl Display for LogicalPlan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = f.write_str("NOT IMPLEMENTED YET");
        Ok(())
    }
}
