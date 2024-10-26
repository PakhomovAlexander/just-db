use core::panic;

use crate::parser::{Literal, Node, Op};

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
pub struct Table {
    pub table_name: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FilterInfo {}

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
pub struct LogicalNode {
    pub op: Operator,
    pub children: Vec<LogicalNode>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct LogicalPlan {
    pub root: LogicalNode,
}

pub struct Analyzer {}

impl Analyzer {
    pub fn new() -> Self {
        Analyzer {}
    }

    pub fn analyze(&self, node: &Node) -> LogicalPlan {
        LogicalPlan {
            root: self.walk(node)[0].clone(),
        }
    }

    fn walk(&self, node: &Node) -> Vec<LogicalNode> {
        match node.op() {
            Some(Op::Select) => {
                let children = node.children();
                let from_node = children[children.len() - 1].clone();
                let columns_node = children[0].clone();

                let mut walker = ColumnWalker::new();
                let columns = walker.walk(columns_node);

                vec![LogicalNode {
                    op: Operator::Projec(ProjectInfo { columns }),
                    children: self.walk(&from_node),
                }]
            }
            Some(Op::From) => {
                let tables_node = node.children()[0].clone();
                let mut walker = TableWalker::new();
                let tables = walker.walk(tables_node);

                let mut nodes = vec![];

                for table in tables {
                    let node = LogicalNode {
                        op: Operator::Read(ReadInfo { table }),
                        children: vec![],
                    };

                    nodes.push(node);
                }

                nodes
            }
            None => {
                panic!("unexpected node: {:?}", node);
            }
            n => {
                panic!("unexpected node: {:?}", n);
            }
        }
    }
}

struct ColumnWalker {
    columns: Vec<Column>,
}

impl ColumnWalker {
    fn new() -> Self {
        ColumnWalker { columns: vec![] }
    }

    fn walk(&mut self, node: Node) -> Vec<Column> {
        match node.op() {
            Some(Op::Comma) => {
                let children = node.children();
                // TODO: do not clone here
                self.walk(children[0].clone());
                self.walk(children[1].clone());
                self.columns.clone()
            }
            None => {
                let column_name = match node.literal().unwrap() {
                    Literal::Identifier {
                        first_name,
                        second_name: _,
                        third_name: _,
                    } => first_name,
                    _ => panic!("unexpected node: {:?}", node),
                };
                self.columns.push(Column { column_name });

                self.columns.clone()
            }
            n => {
                panic!("unexpected node: {:?}", n);
            }
        }
    }
}

struct TableWalker {
    tables: Vec<Table>,
}

impl TableWalker {
    fn new() -> Self {
        TableWalker { tables: vec![] }
    }

    fn walk(&mut self, node: Node) -> Vec<Table> {
        match node.op() {
            Some(Op::Comma) => {
                let children = node.children();
                // TODO: do not clone here
                self.walk(children[0].clone());
                self.walk(children[1].clone());

                self.tables.clone()
            }
            None => {
                let table_name = match node.literal().unwrap() {
                    Literal::Identifier {
                        first_name,
                        second_name: _,
                        third_name: _,
                    } => first_name,
                    _ => panic!("unexpected node: {:?}", node),
                };
                self.tables.push(Table { table_name });

                self.tables.clone()
            }
            n => {
                panic!("unexpected node: {:?}", n);
            }
        }
    }
}

mod tests {
    use crate::{
        analyzer::*,
        parser::{lexer::Lexer, Parser},
    };

    fn column(column_name: &str) -> Column {
        Column {
            column_name: column_name.to_string(),
        }
    }

    fn table(table_name: &str) -> Table {
        Table {
            table_name: table_name.to_string(),
        }
    }

    fn project(columns: Vec<Column>) -> Operator {
        Operator::Projec(ProjectInfo { columns })
    }

    fn read(table: Table) -> Operator {
        Operator::Read(ReadInfo { table })
    }

    fn node(op: Operator, children: Vec<LogicalNode>) -> LogicalNode {
        LogicalNode { op, children }
    }

    fn leaf(op: Operator) -> LogicalNode {
        LogicalNode {
            op,
            children: vec![],
        }
    }

    fn plan(root: LogicalNode) -> LogicalPlan {
        LogicalPlan { root }
    }

    fn analyze(input: &str) -> LogicalPlan {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let analyzer = Analyzer::new();

        analyzer.analyze(&parser.parse())
    }

    #[test]
    fn simple_test() {
        assert_eq!(
            analyze("SELECT col1 FROM table1"),
            plan(node(
                project(vec![column("col1")]),
                vec![leaf(read(table("table1")))]
            ))
        );
    }

    #[test]
    fn select_many_columns() {
        assert_eq!(
            analyze("SELECT col1, col2, col3 FROM table1"),
            plan(node(
                project(vec![column("col1"), column("col2"), column("col3")]),
                vec![leaf(read(table("table1")))]
            ))
        );
    }

    #[test]
    fn select_from_many_tables() {
        assert_eq!(
            analyze("SELECT col1 FROM table1, table2"),
            plan(node(
                project(vec![column("col1")]),
                vec![leaf(read(table("table1"))), leaf(read(table("table2")))]
            ))
        );
    }
}
