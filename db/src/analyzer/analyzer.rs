use core::panic;
use std::vec;

use crate::parser::{
    errors::ParseError,
    tree::{Literal, Node, Op},
};

use super::tree::*;

pub struct Analyzer {
    seen_tables: Vec<String>,
}

impl Analyzer {
    pub fn new() -> Self {
        Analyzer {
            seen_tables: vec![],
        }
    }

    pub fn analyze(mut self, node: Node) -> LogicalPlan {
        match self.walk(node) {
            Ok(nodes) => LogicalPlan {
                root: nodes[0].clone(),
                seen_tables: self.seen_tables,
            },
            Err(e) => panic!("error: {:?}", e), //FIXME: no panic
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn walk(&mut self, node: Node) -> Result<Vec<LogicalNode>, ParseError> {
        match node.op() {
            Some(Op::Select) => {
                let children = node.children();
                let from_node = children[children.len() - 1].clone();
                let columns_node = children[0].clone();

                let mut walker = ColumnWalker::new();
                let columns = walker.walk(&columns_node?)?;

                Ok(vec![LogicalNode {
                    op: Operator::Project { columns },
                    children: self.walk(from_node?)?,
                }])
            }
            Some(Op::From) => {
                let children = node.children();

                let tables_node = children[0].clone();
                let mut walker = TableWalker::new();
                let tables = walker.walk(&tables_node?)?;

                self.seen_tables
                    .append(&mut tables.iter().map(|t| t.table_name.clone()).collect());

                let mut nodes = vec![];

                for table in tables {
                    let node = LogicalNode {
                        op: Operator::Read { table },
                        children: vec![],
                    };

                    nodes.push(node);
                }

                let where_node = children[children.len() - 1].clone()?;
                if where_node.op() == Some(Op::Where) {
                    let where_l_node = self.walk(where_node)?;
                    let n = LogicalNode {
                        op: Operator::Filter {
                            node: Box::new(where_l_node[0].clone()),
                        },
                        children: nodes,
                    };
                    Ok(vec![n])
                } else {
                    Ok(nodes)
                }
            }
            Some(Op::Where) => {
                let children = node.children();

                let node = children[0].clone()?;
                let mut walker = WhereWalker::new();

                Ok(vec![walker.walk(&node)?])
            }
            Some(Op::CreateTable) => {
                let mut walker = CreateTableWalker::new();
                let l_node = walker.walk(&node);

                self.seen_tables.append(&mut walker.seen_tables);

                Ok(vec![l_node?])
            }
            Some(Op::InsertInto) => {
                let mut walker = InsertIntoWalker::new();
                let l_node = walker.walk(&node);

                self.seen_tables.append(&mut walker.seen_tables);

                Ok(vec![l_node?])
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

    fn walk(&mut self, node: &Node) -> Result<Vec<Column>, ParseError> {
        match node.op() {
            Some(Op::Comma) => {
                let children = node.children();
                // TODO: what is going on here?
                self.walk(&children[0].clone()?);
                self.walk(&children[1].clone()?);
                Ok(self.columns.clone())
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

                Ok(self.columns.clone())
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

    fn walk(&mut self, node: &Node) -> Result<Vec<Table>, ParseError> {
        match node.op() {
            Some(Op::Comma) => {
                let children = node.children();
                self.walk(&children[0].clone()?);
                self.walk(&children[1].clone()?);

                Ok(self.tables.clone())
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

                Ok(self.tables.clone())
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

    fn walk(&mut self, node: &Node) -> Result<LogicalNode, ParseError> {
        match node.op() {
            Some(Op::Equals) => {
                let children = node.children();
                let left = self.walk(&children[0].clone()?)?;
                let right = self.walk(&children[1].clone()?)?;

                Ok(LogicalNode {
                    op: Operator::Eq {
                        left: Box::new(left),
                        right: Box::new(right),
                    },
                    children: vec![],
                })
            }
            None => match node.literal().unwrap() {
                // TODO: toooo verbosse
                Literal::Identifier {
                    first_name,
                    second_name: _,
                    third_name: _,
                } => Ok(LogicalNode {
                    op: Operator::Col(Column {
                        column_name: first_name,
                    }),
                    children: vec![],
                }),
                Literal::Numeric(n) => Ok(LogicalNode {
                    op: Operator::Const(Constant::Num(n)),
                    children: vec![],
                }),
                Literal::String(s) => Ok(LogicalNode {
                    op: Operator::Const(Constant::Str(s.to_string())),
                    children: vec![],
                }),
                n => panic!("unexpected node: {:?}", n),
            },
            n => panic!("unexpected node: {:?}", n),
        }
    }
}

struct CreateTableWalker {
    seen_tables: Vec<String>,
}

impl CreateTableWalker {
    fn new() -> Self {
        CreateTableWalker {
            seen_tables: vec![],
        }
    }

    fn walk(&mut self, node: &Node) -> Result<LogicalNode, ParseError> {
        let children = node.children();
        let table_name = match children[0].clone()?.literal().unwrap() {
            Literal::Identifier {
                first_name,
                second_name: _,
                third_name: _,
            } => first_name,
            _ => panic!("unexpected node: {:?}", node),
        };
        let columns = &children[1].clone()?;
        self.seen_tables.push(table_name.to_string());

        Ok(LogicalNode {
            op: Operator::CreateTable {
                table_name: table_name.to_string(),
                columns: self.walk_column_definition(columns)?,
            },
            children: vec![],
        })
    }

    fn walk_column_definition(&mut self, node: &Node) -> Result<Vec<ColumnDefinition>, ParseError> {
        match node.op() {
            Some(Op::ColumnDefinition) => {
                let children = node.children();
                let column_name = match children[0].clone()?.literal().unwrap() {
                    Literal::Identifier {
                        first_name,
                        second_name: _,
                        third_name: _,
                    } => first_name,
                    _ => panic!("unexpected node: {:?}", node),
                };

                let column_type = children[1].clone()?.ttype().unwrap();

                Ok(vec![ColumnDefinition {
                    column_name,
                    column_type,
                }])
            }
            Some(Op::Comma) => {
                let columns_nodes = node.children();
                let mut columns = vec![];
                for column_node in columns_nodes {
                    columns.append(&mut self.walk_column_definition(&column_node?)?);
                }

                Ok(columns)
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

struct InsertIntoWalker {
    seen_tables: Vec<String>,
}

impl InsertIntoWalker {
    fn new() -> Self {
        InsertIntoWalker {
            seen_tables: vec![],
        }
    }

    fn walk(&mut self, node: &Node) -> Result<LogicalNode, ParseError> {
        let children = node.children();
        let table_name = match children[0].clone()?.literal().unwrap() {
            Literal::Identifier {
                first_name,
                second_name: _,
                third_name: _,
            } => first_name,
            _ => panic!("unexpected node: {:?}", node),
        };
        let columns = &children[1].clone()?;
        let values = &children[2].clone()?;

        self.seen_tables.push(table_name.to_string());

        Ok(LogicalNode {
            op: Operator::InsertInto {
                table_name: table_name.to_string(),
                columns: self.walk_columns(columns)?,
                values: self.walk_values(values)?,
            },
            children: vec![],
        })
    }

    fn walk_columns(&mut self, node: &Node) -> Result<Vec<Column>, ParseError> {
        match node.op() {
            Some(Op::ColumnList) => {
                let mut columns = vec![];
                for c in node.children() {
                    match c.clone()?.op() {
                        Some(Op::Comma) => {}
                        None => {
                            let column_name = Self::get_str_literal(&c?);
                            columns.push(Column { column_name });
                        }
                        n => {
                            panic!("unexpected node: {:?}", n);
                        }
                    }
                }

                Ok(columns)
            }
            None => {
                panic!("unexpected node: {:?}", node);
            }
            n => {
                panic!("unexpected node: {:?}", n);
            }
        }
    }

    fn walk_values(&mut self, node: &Node) -> Result<Vec<Constant>, ParseError> {
        match node.op() {
            Some(Op::Values) => {
                let mut values = vec![];
                for c in node.children() {
                    match c.clone()?.op() {
                        Some(Op::Comma) => {}
                        None => {
                            values.push(Self::get_constant(&c?));
                        }
                        n => {
                            panic!("unexpected node: {:?}", n);
                        }
                    }
                }

                Ok(values)
            }
            None => {
                panic!("unexpected node: {:?}", node);
            }
            n => {
                panic!("unexpected node: {:?}", n);
            }
        }
    }

    fn get_str_literal(node: &Node) -> String {
        match node.literal().unwrap() {
            Literal::Identifier {
                first_name,
                second_name: _,
                third_name: _,
            } => first_name,
            _ => panic!("unexpected node: {:?}", node),
        }
    }

    fn get_constant(node: &Node) -> Constant {
        match node.literal().unwrap() {
            Literal::Numeric(n) => Constant::Num(n),
            _ => panic!("unexpected node: {:?}", node),
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
    use pretty_assertions::assert_eq;

    fn column(column_name: &str) -> Column {
        Column {
            column_name: column_name.to_string(),
        }
    }

    fn constant(n: i32) -> Constant {
        Constant::Num(n)
    }

    fn table(table_name: &str) -> Table {
        Table {
            table_name: table_name.to_string(),
        }
    }

    fn project(columns: Vec<Column>) -> Operator {
        Operator::Project { columns }
    }

    fn col_def(column_name: &str, column_type: ColType) -> ColumnDefinition {
        ColumnDefinition {
            column_name: column_name.to_string(),
            column_type,
        }
    }

    fn create_table(table_name: &str, columns: Vec<ColumnDefinition>) -> Operator {
        Operator::CreateTable {
            table_name: table_name.to_string(),
            columns,
        }
    }

    fn insert_into(table_name: &str, columns: Vec<Column>, values: Vec<Constant>) -> Operator {
        Operator::InsertInto {
            table_name: table_name.to_string(),
            columns,
            values,
        }
    }

    fn read(table: Table) -> Operator {
        Operator::Read { table }
    }

    fn filter(node: LogicalNode) -> Operator {
        Operator::Filter {
            node: Box::new(node),
        }
    }

    fn eq(left: LogicalNode, right: LogicalNode) -> Operator {
        Operator::Eq {
            left: Box::new(left),
            right: Box::new(right),
        }
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

    fn plan(root: LogicalNode, seen_tables: Vec<String>) -> LogicalPlan {
        LogicalPlan { root, seen_tables }
    }

    fn analyze(input: &str) -> LogicalPlan {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let analyzer = Analyzer::new();

        analyzer.analyze(parser.parse().unwrap())
    }

    #[test]
    fn simple_test() {
        assert_eq!(
            analyze("SELECT col1 FROM table1"),
            plan(
                node(
                    project(vec![column("col1")]),
                    vec![leaf(read(table("table1")))]
                ),
                vec!["table1".to_string()]
            )
        );
    }

    #[test]
    fn select_many_columns() {
        assert_eq!(
            analyze("SELECT col1, col2, col3 FROM table1"),
            plan(
                node(
                    project(vec![column("col1"), column("col2"), column("col3")]),
                    vec![leaf(read(table("table1")))]
                ),
                vec!["table1".to_string()]
            )
        );
    }

    #[test]
    fn select_from_many_tables() {
        assert_eq!(
            analyze("SELECT col1 FROM table1, table2"),
            plan(
                node(
                    project(vec![column("col1")]),
                    vec![leaf(read(table("table1"))), leaf(read(table("table2")))]
                ),
                vec!["table1".to_string(), "table2".to_string(),]
            )
        );
    }

    #[test]
    fn select_from_where() {
        assert_eq!(
            analyze("SELECT col1 FROM table1 WHERE col2 = 1"),
            plan(
                node(
                    project(vec![column("col1")]),
                    vec![node(
                        filter(leaf(eq(leaf(col("col2")), leaf(cons_int(1))))),
                        vec![leaf(read(table("table1")))]
                    ),]
                ),
                vec!["table1".to_string()]
            )
        );
    }

    #[test]
    fn select_from_where_with_string() {
        assert_eq!(
            analyze("SELECT col1 FROM table1 WHERE col2 = 'I am a string!'"),
            plan(
                node(
                    project(vec![column("col1")]),
                    vec![node(
                        filter(leaf(eq(
                            leaf(col("col2")),
                            leaf(cons_str("I am a string!"))
                        ))),
                        vec![leaf(read(table("table1")))]
                    ),]
                ),
                vec!["table1".to_string()]
            )
        );
    }

    #[test]
    fn create_table_test() {
        assert_eq!(
            analyze("CREATE TABLE table1 (col1 INT, col2 INT, col3 INT)"),
            plan(
                node(
                    create_table(
                        "table1",
                        vec![
                            col_def("col1", ColType::Int),
                            col_def("col2", ColType::Int),
                            col_def("col3", ColType::Int)
                        ]
                    ),
                    vec![]
                ),
                vec!["table1".to_string()]
            )
        );
    }

    #[test]
    fn insert_into_table() {
        assert_eq!(
            analyze("INSERT INTO table1 (col1, col2, col3) values (1, 22, 333)"),
            plan(
                node(
                    insert_into(
                        "table1",
                        vec![column("col1"), column("col2"), column("col3")],
                        vec![constant(1), constant(22), constant(333)]
                    ),
                    vec![]
                ),
                vec!["table1".to_string()]
            )
        );
    }
}
