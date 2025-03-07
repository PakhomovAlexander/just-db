use core::panic;
use std::{cell::RefCell, rc::Rc};

use crate::{
    analyzer::{Constant, LogicalNode, LogicalPlan, Operator},
    catalog::{types::TableId, Catalog, TableSchemaBuilder},
    optimizer::types::{Column, FullScanState, PhysicalPlan, Tuple, Val},
};

use super::types::Op;

pub struct Optimizer {
    catalog: Rc<RefCell<Catalog>>,
}

impl Optimizer {
    pub fn optimize(&self, l_plan: LogicalPlan) -> PhysicalPlan {
        let root_node = l_plan.root;

        let mut builder = PhysicalPlanBuilder {
            seen_tables: std::collections::HashSet::new(),
            catalog: Rc::clone(&self.catalog),
        };

        let root_op = builder.walk(&root_node, &l_plan.seen_tables);
        assert!(root_op.len() == 1);

        PhysicalPlan {
            root: root_op[0].clone(),
        }
    }

    pub fn new(catalog: Rc<RefCell<Catalog>>) -> Self {
        Optimizer { catalog }
    }
}

struct PhysicalPlanBuilder {
    seen_tables: std::collections::HashSet<String>,
    catalog: Rc<RefCell<Catalog>>,
}

impl PhysicalPlanBuilder {
    fn walk(&mut self, node: &LogicalNode, seen_tables: &Vec<String>) -> Vec<Op> {
        match &node.op {
            Operator::Project { columns } => {
                let mut cols = Vec::new();
                let table_id = TableId::public(seen_tables[0].as_str());
                let table_schema = self.catalog.as_ref().borrow().get_table(&table_id).unwrap();

                for c in columns {
                    cols.push(Column::new(
                        &table_schema.get_column(&c.column_name).unwrap(),
                    ));
                }

                let c = &node.children;

                let children = if !c.is_empty() {
                    self.walk(&c[0], seen_tables)
                } else {
                    Vec::new()
                };

                vec![Op::Project { cols, children }]
            }
            Operator::Read { table } => {
                self.seen_tables.insert(table.table_name.clone());

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
                    children: self.walk(&node.children[0], seen_tables),
                }]
            }
            Operator::CreateTable {
                table_name,
                columns,
            } => {
                let mut ts = TableSchemaBuilder::public();
                ts.table(table_name);
                for c in columns {
                    ts.col(&c.column_name, c.column_type);
                }

                vec![Op::create_table(ts.build())]
            }
            Operator::InsertInto {
                table_name,
                columns,
                values,
            } => {
                let mut tuples = Vec::new();
                let mut tuple_data = Vec::new();
                for (i, c) in columns.iter().enumerate() {
                    let v = &values[i];
                    match v {
                        Constant::Num(n) => tuple_data.push((c.column_name.as_str(), Val::Int(*n))),
                        Constant::Str(s) => panic!("String not supported yet"),
                    }
                }
                tuples.push(Tuple::new(tuple_data));

                vec![Op::insert_into(table_name, tuples)]
            }
            _ => {
                panic!("Unsopported node {:?}", node)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::{cell::RefCell, rc::Rc, vec};

    use crate::{
        analyzer::{Analyzer, LogicalPlan},
        catalog::{Catalog, TableSchemaBuilder},
        optimizer::{
            types::{Column, Op, PhysicalPlan, StorageEngine, Tuple, Val},
            Optimizer,
        },
        parser::{Lexer, Parser},
        types::ColType,
    };

    fn analyze(input: &str) -> LogicalPlan {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let analyzer = Analyzer::new();

        analyzer.analyze(parser.parse().unwrap())
    }

    #[test]
    fn simple_test() {
        let l_plan = analyze("SELECT col1 FROM table1");

        let mut catalog = Catalog::mem();

        let ts = TableSchemaBuilder::public()
            .table("table1")
            .col("col1", ColType::Int)
            .build();
        let _ = catalog.register_table(&ts);
        let cs = ts.get_column("col1").unwrap();

        let optimizer = Optimizer::new(Rc::new(RefCell::new(catalog)));

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
            .col("col1", ColType::Int)
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

        let catalog_rc = Rc::new(RefCell::new(catalog));
        let storage_rc = Rc::new(RefCell::new(storage));

        let optimizer = Optimizer::new(Rc::clone(&catalog_rc));

        let mut p_plan = optimizer.optimize(l_plan);

        let tuples = p_plan.execute_all(Rc::clone(&storage_rc), Rc::clone(&catalog_rc));

        assert_eq!(tuples.len(), 4);
    }

    #[test]
    fn create_table() {
        let l_plan = analyze("CREATE TABLE table1 (col1 INT, col2 INT, col3 INT)");

        let catalog_rc = Rc::new(RefCell::new(Catalog::mem()));
        let storage_rc = Rc::new(RefCell::new(StorageEngine::mem()));

        let optimizer = Optimizer::new(Rc::clone(&catalog_rc));

        let mut p_plan = optimizer.optimize(l_plan);

        assert_eq!(
            p_plan,
            PhysicalPlan {
                root: Op::create_table(
                    TableSchemaBuilder::public()
                        .table("table1")
                        .col("col1", ColType::Int)
                        .col("col2", ColType::Int)
                        .col("col3", ColType::Int)
                        .build()
                )
            }
        );

        let restult = p_plan.execute_all(storage_rc, Rc::clone(&catalog_rc));
        assert_eq!(restult.len(), 0);
    }

    #[test]
    fn create_single_column() {
        let l_plan = analyze("CREATE TABLE table2 (col1 INT)");

        let catalog_rc = Rc::new(RefCell::new(Catalog::mem()));
        let storage_rc = Rc::new(RefCell::new(StorageEngine::mem()));

        let optimizer = Optimizer::new(Rc::clone(&catalog_rc));

        let mut p_plan = optimizer.optimize(l_plan);

        assert_eq!(
            p_plan,
            PhysicalPlan {
                root: Op::create_table(
                    TableSchemaBuilder::public()
                        .table("table2")
                        .col("col1", ColType::Int)
                        .build()
                )
            }
        );

        let restult = p_plan.execute_all(storage_rc, Rc::clone(&catalog_rc));
        assert_eq!(restult.len(), 0);
    }

    #[test]
    fn insert_into() {
        let l_plan = analyze("INSERT INTO table1 (col1, col2, col3) VALUES (1, 2, 3)");

        let ts = TableSchemaBuilder::public()
            .table("table1")
            .col("col1", ColType::Int)
            .col("col2", ColType::Int)
            .col("col3", ColType::Int)
            .build();

        let catalog = Catalog::mem();
        let catalog_rc = Rc::new(RefCell::new(catalog));

        let _ = catalog_rc.borrow_mut().register_table(&ts);

        let optimizer = Optimizer::new(catalog_rc);

        let p_plan = optimizer.optimize(l_plan);

        assert_eq!(
            p_plan,
            PhysicalPlan {
                root: Op::insert_into(
                    "table1",
                    vec![Tuple::new(vec![
                        ("col1", Val::Int(1)),
                        ("col2", Val::Int(2)),
                        ("col3", Val::Int(3))
                    ])]
                )
            }
        );
    }
}
