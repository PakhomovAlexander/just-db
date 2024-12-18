use core::panic;
use std::{collections::HashMap, rc::Rc};

use serde::{Deserialize, Serialize};
use strum::Display;

use crate::{
    analyzer::{LogicalNode, LogicalPlan, Operator},
    catalog::{types::TableId, Catalog},
    optimizer::types::{Column, FullScanState, PhysicalPlan},
};

use super::types::{Op, StorageEngine};

pub struct Optimizer {
    catalog: Rc<Catalog>,
}

impl Optimizer {
    pub fn optimize(&self, l_plan: LogicalPlan) -> PhysicalPlan {
        let root_node = l_plan.root;

        let mut builder = PhysicalPlanBuilder {
            catalog: Rc::clone(&self.catalog),
        };

        let root_op = builder.walk(&root_node);
        assert!(root_op.len() == 1);

        PhysicalPlan {
            root: root_op[0].clone(),
        }
    }

    pub fn new(catalog: Rc<Catalog>) -> Self {
        Optimizer { catalog }
    }
}

struct PhysicalPlanBuilder {
    catalog: Rc<Catalog>,
}

impl PhysicalPlanBuilder {
    fn walk(&mut self, node: &LogicalNode) -> Vec<Op> {
        match &node.op {
            Operator::Project { columns } => {
                let mut cols = Vec::new();
                // FIXME: table is hardcoded!
                let table_id = TableId::public("table1");
                let table_schema = self.catalog.get_table(&table_id).unwrap();

                for c in columns {
                    cols.push(Column::new(
                        &table_schema.get_column(&c.column_name).unwrap(),
                    ));
                }

                let c = &node.children;

                let children = if !c.is_empty() {
                    self.walk(&c[0])
                } else {
                    Vec::new()
                };

                vec![Op::Project { cols, children }]
            }
            Operator::Read { table } => {
                vec![Op::FullScan {
                    name: table.table_name.clone(),
                    state: FullScanState {
                        curr_pos: 0,
                        iterator: None,
                    },
                    children: Vec::new(),
                }]
            }
            Operator::Filter { .. } => {
                vec![Op::Filter {
                    children: self.walk(&node.children[0]),
                }]
            }
            _ => {
                panic!("Unsopported node")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{rc::Rc, vec};

    use crate::{
        analyzer::{Analyzer, LogicalPlan},
        catalog::{types::DataType, Catalog, TableSchemaBuilder},
        optimizer::{
            types::{Column, Op, PhysicalPlan, StorageEngine, Tuple, Val},
            Optimizer,
        },
        parser::{Lexer, Parser},
    };

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

        let optimizer = Optimizer::new(Rc::new(catalog));

        let p_plan = optimizer.optimize(l_plan);

        assert_eq!(
            p_plan,
            PhysicalPlan {
                root: Op::project(vec![Column::new(&cs)], vec![Op::full_scan("table1")])
            }
        );
    }

    #[test]
    fn execute_pipeline() {
        let l_plan = analyze("SELECT col1 FROM table1");

        let mut catalog = Catalog::mem();
        let mut storage = StorageEngine::mem();

        let ts = TableSchemaBuilder::public()
            .table("table1")
            .col("col1", DataType::Int)
            .build();
        let _ = catalog.register_table(&ts);

        storage.insert(
            "table1",
            vec![
                Tuple::new(vec![("col1", Val::Int(1))]),
                Tuple::new(vec![("col1", Val::Int(2))]),
                Tuple::new(vec![("col1", Val::Int(3))]),
                Tuple::new(vec![("col1", Val::Int(4))]),
            ],
        );

        let catalog_rc = Rc::new(catalog);
        let storage_rc = Rc::new(storage);

        let optimizer = Optimizer::new(Rc::clone(&catalog_rc));

        let mut p_plan = optimizer.optimize(l_plan);

        let tuples = p_plan.execute_all(Rc::clone(&storage_rc));

        assert_eq!(tuples.len(), 4);
    }
}
