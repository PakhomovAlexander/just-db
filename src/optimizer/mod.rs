#![allow(dead_code)]

use core::panic;
use std::{
    array::IntoIter, borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc, sync::Arc,
};

use crate::{
    analyzer::{LogicalNode, LogicalPlan, Operator},
    catalog::{Catalog, ColumnSchema, TableId},
};

struct Optimizer {
    catalog: Catalog,
    storage: StorageEngine,
}

impl Optimizer {
    pub fn optimize(&self, l_plan: LogicalPlan) -> PhysicalPlan {
        let root_node = l_plan.root;

        let mut builder = PhysicalPlanBuilder {
            catalog: self.catalog.clone(),
            storage: self.storage.clone(),
        };

        let root_op = builder.walk(&root_node);
        assert!(root_op.len() == 1);

        PhysicalPlan {
            root: root_op[0].clone(),
        }
    }

    pub fn new(catalog: Catalog, storage: StorageEngine) -> Optimizer {
        Optimizer { catalog, storage }
    }
}

struct PhysicalPlanBuilder {
    catalog: Catalog,
    storage: StorageEngine,
}

impl PhysicalPlanBuilder {
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
                        engine: self.storage.clone(),
                        state: FullScanState {
                            curr_pos: 0,
                            iterator: None,
                        },
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
    fn full_scan(table_name: &str, engine: StorageEngine) -> Op {
        Op::FullScan(
            FullScanInfo {
                name: table_name.to_string(),
                engine,
                state: FullScanState {
                    curr_pos: 0,
                    iterator: None,
                },
            },
            vec![],
        )
    }

    fn project(cols: Vec<Column>, children: Vec<Op>) -> Op {
        Op::Project(ProjectInfo { cols }, children)
    }

    fn filter(_info: FilterInfo, children: Vec<Op>) -> Op {
        Op::Filter(FilterInfo {}, children)
    }

    fn open(&mut self) {
        match self {
            Op::FullScan(info, _) => {
                let tuples = info.engine.scan(&info.name);
                let iter = FullScanIterator {
                    curr_pos: 0,
                    tuples,
                };

                info.state.iterator = Some(iter);
            }
            Op::Project(_, children) => {
                for c in children {
                    c.open();
                }
            }
            Op::Filter(_, children) => {
                for c in children {
                    c.open();
                }
            }
        }
    }

    fn next(&mut self) -> Option<Tuple> {
        match self {
            Op::FullScan(info, _) => {
                let iter = info.state.iterator.as_mut().unwrap();
                if iter.curr_pos < iter.tuples.len() {
                    let t = iter.tuples[iter.curr_pos].clone();
                    iter.curr_pos += 1;
                    Some(t)
                } else {
                    None
                }
            }
            Op::Project(_, children_ops) => {
                // TODO: Implement projection
                children_ops[0].next()
            }
            Op::Filter(_, children_ops) => {
                let t = children_ops[0].next().unwrap();
                Some(t)
            }
        }
    }

    fn close(&mut self) {
        match self {
            Op::FullScan(info, _) => {
                info.state.iterator = None;
            }
            Op::Project(_, children) => {
                for c in children {
                    c.close();
                }
            }
            Op::Filter(_, children) => {
                for c in children {
                    c.close();
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct FullScanIterator {
    curr_pos: usize,
    tuples: Vec<Tuple>,
}

#[derive(Debug, PartialEq, Clone)]
struct Tuple {
    data: HashMap<String, Val>,
}

impl Tuple {
    fn new(data: Vec<(&str, Val)>) -> Tuple {
        let mut map = HashMap::new();
        for (k, v) in data {
            map.insert(k.to_string(), v);
        }
        Tuple { data: map }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum Val {
    Int(i32),
    String(String),
    Bool(bool),
    Null,
}

#[derive(Debug, PartialEq, Clone)]
enum StorageEngine {
    Memory(MemoryStorageEngine),
}

impl StorageEngine {
    fn mem() -> StorageEngine {
        StorageEngine::Memory(MemoryStorageEngine {
            tables: HashMap::new(),
        })
    }

    fn scan(&self, table_name: &str) -> Vec<Tuple> {
        match self {
            StorageEngine::Memory(engine) => engine.tables.get(table_name).unwrap().clone(),
        }
    }

    fn insert(&mut self, table_name: &str, tuples: Vec<Tuple>) {
        match self {
            StorageEngine::Memory(engine) => {
                engine.tables.insert(table_name.to_string(), tuples);
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
struct MemoryStorageEngine {
    tables: HashMap<String, Vec<Tuple>>,
}

#[derive(Debug, PartialEq)]
struct PhysicalPlan {
    root: Op,
}

impl PhysicalPlan {
    fn execute_all(&mut self) -> Vec<Tuple> {
        let mut tuples = Vec::new();
        self.root.open();
        while let Some(t) = self.root.next() {
            tuples.push(t);
        }
        self.root.close();
        tuples
    }
}

#[derive(Debug, PartialEq, Clone)]
struct FullScanInfo {
    name: String,
    state: FullScanState,
    engine: StorageEngine,
}

#[derive(Debug, PartialEq, Clone)]
struct FullScanState {
    curr_pos: usize,
    iterator: Option<FullScanIterator>,
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
    use std::{
        borrow::{Borrow, Cow},
        cell::RefCell,
        rc::Rc,
        sync::Arc,
        vec,
    };

    use crate::{
        analyzer::{Analyzer, LogicalPlan},
        catalog::{Catalog, DataType, TableSchemaBuilder},
        optimizer::{Column, Op, StorageEngine, Tuple, Val},
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
        let mut storage = StorageEngine::mem();

        let ts = TableSchemaBuilder::public()
            .table("table1")
            .col("col1", DataType::Int)
            .build();
        let _ = catalog.register_table(&ts);
        let cs = ts.get_column("col1").unwrap();

        let optimizer = Optimizer::new(catalog.clone(), storage.clone());

        let p_plan = optimizer.optimize(l_plan);

        assert_eq!(
            p_plan,
            PhysicalPlan {
                root: Op::project(
                    vec![Column::new(&cs)],
                    vec![Op::full_scan("table1", storage)]
                )
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

        let optimizer = Optimizer::new(catalog, storage);

        let mut p_plan = optimizer.optimize(l_plan);

        let tuples = p_plan.execute_all();

        assert_eq!(tuples.len(), 4);
    }
}
