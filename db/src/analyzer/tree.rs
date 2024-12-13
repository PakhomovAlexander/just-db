use std::fmt::Display;

use crate::types::ColType;

//#[allow(clippy::unused)]
#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Projec(ProjectInfo),
    Filter(FilterInfo),
    Read(ReadInfo),
    Join(JoinInfo),
    Group(GroupInfo),
    Sort(SortInfo),
    Limit(LimitInfo),
    Distinct(DistinctInfo),

    Add(InfixOpInfo),
    Sub(InfixOpInfo),
    Mul(InfixOpInfo),
    Div(InfixOpInfo),
    Eq(InfixOpInfo),
    Neq(InfixOpInfo),
    Lt(InfixOpInfo),
    Lte(InfixOpInfo),
    Gt(InfixOpInfo),
    Gte(InfixOpInfo),
    And(InfixOpInfo),
    Or(InfixOpInfo),
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

    CreateTable(CreateTableInfo),
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProjectInfo {
    pub columns: Vec<Column>,
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
pub struct FilterInfo {
    pub node: Box<LogicalNode>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixOpInfo {
    pub left: Box<LogicalNode>,
    pub right: Box<LogicalNode>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReadInfo {
    pub table: Table,
}

#[derive(Debug, PartialEq, Clone)]
pub struct JoinInfo {}

#[derive(Debug, PartialEq, Clone)]
pub struct GroupInfo {}

#[derive(Debug, PartialEq, Clone)]
pub struct SortInfo {}

#[derive(Debug, PartialEq, Clone)]
pub struct LimitInfo {}

#[derive(Debug, PartialEq, Clone)]
pub struct DistinctInfo {}

#[derive(Debug, PartialEq, Clone)]
pub struct CreateTableInfo {
    pub table_name: String,
    pub columns: Vec<ColumnDefinition>,
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
