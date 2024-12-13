use core::panic;

use crate::parser::tree::{Literal, Node, Op};

use super::tree::*;

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
                let children = node.children();

                let tables_node = children[0].clone();
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

                let where_node = children[children.len() - 1].clone();
                if where_node.op() == Some(Op::Where) {
                    let where_l_node = self.walk(&where_node);
                    let n = LogicalNode {
                        op: Operator::Filter(FilterInfo {
                            node: Box::new(where_l_node[0].clone()),
                        }),
                        children: nodes,
                    };
                    vec![n]
                } else {
                    nodes
                }
            }
            Some(Op::Where) => {
                let children = node.children();

                let node = children[0].clone();
                let mut walker = WhereWalker::new();

                vec![walker.walk(node)]
            }
            Some(Op::CreateTable) => {
                let children = node.children();
                let mut walker = CreateTableWalker::new();

                vec![walker.walk(&node)]
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

struct WhereWalker {}

impl WhereWalker {
    fn new() -> Self {
        WhereWalker {}
    }

    fn walk(&mut self, node: Node) -> LogicalNode {
        match node.op() {
            Some(Op::Equals) => {
                let children = node.children();
                // TODO: do not clone here
                let left = self.walk(children[0].clone());
                let right = self.walk(children[1].clone());

                LogicalNode {
                    op: Operator::Eq(InfixOpInfo {
                        left: Box::new(left),
                        right: Box::new(right),
                    }),
                    children: vec![],
                }
            }
            None => match node.literal().unwrap() {
                Literal::Identifier {
                    first_name,
                    second_name: _,
                    third_name: _,
                } => LogicalNode {
                    op: Operator::Col(Column {
                        column_name: first_name,
                    }),
                    children: vec![],
                },
                Literal::Numeric(n) => LogicalNode {
                    op: Operator::Const(Constant::Num(n)),
                    children: vec![],
                },
                Literal::String(s) => LogicalNode {
                    op: Operator::Const(Constant::Str(s.to_string())),
                    children: vec![],
                },
                n => panic!("unexpected node: {:?}", n),
            },
            n => panic!("unexpected node: {:?}", n),
        }
    }
}

struct CreateTableWalker {}

impl CreateTableWalker {
    fn new() -> Self {
        CreateTableWalker {}
    }

    fn walk(&mut self, node: &Node) -> LogicalNode {
        let children = node.children();
        let table_name = match children[0].literal().unwrap() {
            Literal::Identifier {
                first_name,
                second_name: _,
                third_name: _,
            } => first_name,
            _ => panic!("unexpected node: {:?}", node),
        };
        let columns = &children[1];

        return LogicalNode {
            op: Operator::CreateTable(CreateTableInfo {
                table_name: table_name.to_string(),
                columns: self.walk_column_definition(columns),
            }),
            children: vec![],
        };
    }

    fn walk_column_definition(&mut self, node: &Node) -> Vec<ColumnDefinition> {
        match node.op() {
            Some(Op::ColumnDefinition) => {
                let children = node.children();
                let column_name = match children[0].literal().unwrap() {
                    Literal::Identifier {
                        first_name,
                        second_name: _,
                        third_name: _,
                    } => first_name,
                    _ => panic!("unexpected node: {:?}", node),
                };

                let column_type = children[1].ttype().unwrap();

                vec![ColumnDefinition {
                    column_name,
                    column_type,
                }]
            }
            Some(Op::Comma) => {
                let columns_nodes = node.children();
                let mut columns = vec![];
                for column_node in columns_nodes {
                    columns.append(&mut self.walk_column_definition(&column_node));
                }

                columns
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
#[cfg(test)]
mod tests {
    use crate::{
        analyzer::*,
        parser::{Lexer, Parser},
        types::ColType,
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

    fn col_def(column_name: &str, column_type: ColType) -> ColumnDefinition {
        ColumnDefinition {
            column_name: column_name.to_string(),
            column_type,
        }
    }

    fn create_table(table_name: &str, columns: Vec<ColumnDefinition>) -> Operator {
        Operator::CreateTable(CreateTableInfo {
            table_name: table_name.to_string(),
            columns,
        })
    }

    fn read(table: Table) -> Operator {
        Operator::Read(ReadInfo { table })
    }

    fn filter(node: LogicalNode) -> Operator {
        Operator::Filter(FilterInfo {
            node: Box::new(node),
        })
    }

    fn eq(left: LogicalNode, right: LogicalNode) -> Operator {
        Operator::Eq(InfixOpInfo {
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn col(column_name: &str) -> Operator {
        Operator::Col(column(column_name))
    }

    fn cons_int(i: i32) -> Operator {
        Operator::Const(Constant::Num(i))
    }

    fn cons_str(s: &str) -> Operator {
        Operator::Const(Constant::Str(s.to_string()))
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

    #[test]
    fn select_from_where() {
        assert_eq!(
            analyze("SELECT col1 FROM table1 WHERE col2 = 1"),
            plan(node(
                project(vec![column("col1")]),
                vec![node(
                    filter(leaf(eq(leaf(col("col2")), leaf(cons_int(1))))),
                    vec![leaf(read(table("table1")))]
                ),]
            ))
        );
    }

    #[test]
    fn select_from_where_with_string() {
        assert_eq!(
            analyze("SELECT col1 FROM table1 WHERE col2 = 'I am a string!'"),
            plan(node(
                project(vec![column("col1")]),
                vec![node(
                    filter(leaf(eq(
                        leaf(col("col2")),
                        leaf(cons_str("I am a string!"))
                    ))),
                    vec![leaf(read(table("table1")))]
                ),]
            ))
        );
    }

    #[test]
    fn create_table_test() {
        assert_eq!(
            analyze("CREATE TABLE table1 (col1 INT, col2 INT, col3 INT)"),
            plan(node(
                create_table(
                    "table1",
                    vec![
                        col_def("col1", ColType::Int),
                        col_def("col2", ColType::Int),
                        col_def("col3", ColType::Int)
                    ]
                ),
                vec![]
            ))
        );
    }
}
