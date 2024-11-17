#![allow(dead_code)]

use core::panic;
use std::collections::HashMap;

use crate::{
    analyzer::{LogicalNode, LogicalPlan, Operator},
    catalog::{Catalog, ColumnSchema, TableId},
};

struct Optimizer {
    catalog: Catalog,
}

impl Optimizer {
    pub fn optimize(&self, l_plan: LogicalPlan) -> PhysicalPlan {
        let root_node = l_plan.root;

        let mut builder = PhysicalPlanBuilder {
            catalog: &self.catalog,
        };

        let root_op = builder.walk(&root_node);
        assert!(root_op.len() == 1);

        PhysicalPlan {
            root: root_op[0].clone(),
        }
    }

    pub fn new(catalog: Catalog) -> Optimizer {
        Optimizer { catalog }
    }
}

struct PhysicalPlanBuilder<'a> {
    catalog: &'a Catalog,
}

impl PhysicalPlanBuilder<'_> {
    fn walk(&mut self, node: &LogicalNode) -> Vec<Op> {
        match &node.op {
            Operator::Projec(info) => {
                let mut cols = Vec::new();
                let table_id = TableId::public("table1");
                let table_schema = self.catalog.get_table(&table_id).unwrap();

                for c in &info.columns {
                    cols.push(Column::new(
                        &table_schema.get_column(&c.column_name).unwrap(),
                    ));
                }

                let c = &node.children;

                let child_ops = if !c.is_empty() {
                    self.walk(&c[0])
                } else {
                    Vec::new()
                };

                vec![Op::Project(ProjectInfo { cols }, child_ops)]
            }
            Operator::Read(info) => {
                vec![Op::FullScan(
                    FullScanInfo {
                        name: info.table.table_name.clone(),
                    },
                    Vec::new(),
                )]
            }
            Operator::Filter(_info) => {
                vec![Op::Filter(FilterInfo {}, self.walk(&node.children[0]))]
            }
            _ => {
                panic!("Unsopported node")
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Op {
    Project(ProjectInfo, Vec<Op>),
    Filter(FilterInfo, Vec<Op>),
    FullScan(FullScanInfo, Vec<Op>),
}

/// Vulcano pipiline model
impl Op {
    fn open(&mut self) {}

    fn next(&mut self) -> Option<Tuple> {
        None
    }

    fn close(&mut self) {}
}

#[derive(Debug, PartialEq)]
struct Tuple {
    data: HashMap<String, Val>,
}

#[derive(Debug, PartialEq)]
enum Val {
    Int(i32),
    String(String),
    Bool(bool),
    Null,
}

#[derive(Debug, PartialEq)]
struct PhysicalPlan {
    root: Op,
}

#[derive(Debug, PartialEq, Clone)]
struct FullScanInfo {
    name: String,
}

#[derive(Debug, PartialEq, Clone)]
struct ProjectInfo {
    cols: Vec<Column>,
}

#[derive(Debug, PartialEq, Clone)]
struct Column {
    col_schema: ColumnSchema,
}

impl Column {
    fn new(col_schema: &ColumnSchema) -> Column {
        Column {
            col_schema: col_schema.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct FilterInfo {}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::{
        analyzer::{Analyzer, LogicalPlan},
        catalog::{Catalog, DataType, TableSchemaBuilder},
        optimizer::{Column, FullScanInfo, Op, ProjectInfo},
        parser::{lexer::Lexer, Parser},
    };

    use super::{Optimizer, PhysicalPlan};

    fn analyze(input: &str) -> LogicalPlan {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let analyzer = Analyzer::new();

        analyzer.analyze(&parser.parse())
    }

    #[test]
    fn simple_test() {
        let l_plan = analyze("SELECT col1 FROM table1");

        let mut catalog = Catalog::mem();

        let ts = TableSchemaBuilder::public()
            .table("table1")
            .col("col1", DataType::Int)
            .build();
        let _ = catalog.register_table(&ts);
        let cs = ts.get_column("col1").unwrap();

        let optimizer = Optimizer::new(catalog);

        let p_plan = optimizer.optimize(l_plan);

        assert_eq!(
            p_plan,
            PhysicalPlan {
                root: Op::Project(
                    ProjectInfo {
                        cols: vec![Column::new(&cs)]
                    },
                    vec![Op::FullScan(
                        FullScanInfo {
                            name: "table1".to_string()
                        },
                        vec![]
                    )]
                )
            }
        );
    }
}
