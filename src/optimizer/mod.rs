use core::panic;
use std::collections::HashMap;

use crate::analyzer::{LogicalNode, LogicalPlan, Operator};

trait Schema {
    fn get_col_type(&self, table: String, col: String) -> Type;
}

struct MemSchema {
    // table_name -> col_name -> type
    map: HashMap<String, HashMap<String, Type>>,
}

impl Schema for MemSchema {
    fn get_col_type(&self, table: String, col: String) -> Type {
        match self.map.get(&table) {
            Some(inner) => match inner.get(&col) {
                Some(t) => t.clone(),
                None => {
                    panic!("No such column {} in table {}", col, table)
                }
            },
            None => {
                panic!("No such table {}", table)
            }
        }
    }
}

struct Optimizer {
    s: MemSchema,
}

impl Optimizer {
    pub fn optimize(&self, l_plan: LogicalPlan) -> PhysicalPlan {
        let root_node = l_plan.root;

        let mut builder = PhysicalPlanBuilder {
            schema: &self.s,

            project: None,
            scan: None,
            filter: None,
        };

        builder.walk(&root_node);

        builder.build()
    }

    fn new(schema: MemSchema) -> Self {
        Self { s: schema }
    }
}

struct PhysicalPlanBuilder<'a> {
    schema: &'a MemSchema,

    project: Option<Project>,
    scan: Option<TableScan>,
    filter: Option<Filter>,
}

impl PhysicalPlanBuilder<'_> {
    fn walk(&mut self, node: &LogicalNode) {
        match &node.op {
            Operator::Projec(info) => {
                let mut cols = Vec::new();
                for c in &info.columns {
                    cols.push(Column {
                        name: c.column_name.clone(),
                        t: self
                            .schema
                            // TODO: resolve names
                            .get_col_type("table1".to_string(), c.column_name.clone()),
                    });
                }

                self.project = Some(Project { cols });
            }
            Operator::Read(info) => {
                self.scan = Some(TableScan {
                    name: info.table.table_name.clone(),
                });
            }
            Operator::Filter(_info) => {
                self.filter = Some(Filter {});
            }
            _ => {
                panic!("Unsopported node")
            }
        };

        let c = &node.children;

        if !c.is_empty() {
            self.walk(&c[0]);
        }
    }

    fn build(self) -> PhysicalPlan {
        PhysicalPlan {
            project: self.project.unwrap(),
            filter: self.filter,
            scan: self.scan.unwrap(),
        }
    }
}

#[derive(Debug, PartialEq)]
struct PhysicalPlan {
    project: Project,
    scan: TableScan,
    filter: Option<Filter>,
}

#[derive(Debug, PartialEq)]
struct TableScan {
    name: String,
}

#[derive(Debug, PartialEq)]
struct Project {
    cols: Vec<Column>,
}

#[derive(Debug, PartialEq)]
struct Column {
    t: Type,
    name: String,
}

#[derive(Debug, PartialEq, Clone)]
enum Type {
    String,
    Int,
}

#[derive(Debug, PartialEq)]
struct Filter {}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, vec};

    use crate::{
        analyzer::{Analyzer, LogicalPlan},
        optimizer::{self, Column, MemSchema, Project, Schema, TableScan, Type},
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

        let schema = MemSchema {
            map: HashMap::from([(
                "table1".to_string(),
                HashMap::from([("col1".to_string(), Type::Int)]),
            )]),
        };

        let optimizer = Optimizer::new(schema);

        let p_plan = optimizer.optimize(l_plan);

        assert_eq!(
            p_plan,
            PhysicalPlan {
                project: Project {
                    cols: vec![Column {
                        t: Type::Int,
                        name: "col1".to_string()
                    }]
                },
                filter: None,
                scan: TableScan {
                    name: "table1".to_string()
                }
            }
        );
    }
}
