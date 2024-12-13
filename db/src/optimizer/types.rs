use std::{collections::HashMap, rc::Rc};

use serde::{Deserialize, Serialize};
use strum::Display;

use crate::catalog::types::ColumnSchema;

#[derive(Debug, PartialEq)]
pub struct PhysicalPlan {
    pub root: Op,
}

impl PhysicalPlan {
    pub fn execute_all(&mut self) -> Vec<Tuple> {
        let mut tuples = Vec::new();
        self.root.open();
        while let Some(t) = self.root.next() {
            tuples.push(t);
        }
        self.root.close();
        tuples
    }
}

#[derive(Debug, PartialEq)] // TODO: remove PartialEq
pub enum StorageEngine {
    Memory(MemoryStorageEngine),
}

impl StorageEngine {
    pub fn mem() -> StorageEngine {
        StorageEngine::Memory(MemoryStorageEngine {
            tables: HashMap::new(),
        })
    }

    pub fn scan(&self, table_name: &str) -> Vec<Tuple> {
        match self {
            StorageEngine::Memory(engine) => engine.tables.get(table_name).unwrap().clone(),
        }
    }

    pub fn insert(&mut self, table_name: &str, tuples: Vec<Tuple>) {
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

#[derive(Debug, PartialEq, Clone)]
pub struct FullScanInfo {
    pub name: String,
    pub state: FullScanState,
    pub engine: Rc<StorageEngine>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FullScanState {
    pub curr_pos: usize,
    pub iterator: Option<FullScanIterator>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ProjectInfo {
    pub cols: Vec<Column>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Column {
    pub col_schema: ColumnSchema,
}

impl Column {
    pub fn new(col_schema: &ColumnSchema) -> Column {
        Column {
            col_schema: col_schema.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FilterInfo {}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    Project(ProjectInfo, Vec<Op>),
    Filter(FilterInfo, Vec<Op>),
    FullScan(FullScanInfo, Vec<Op>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FullScanIterator {
    pub curr_pos: usize,
    pub tuples: Vec<Tuple>,
}

/// Vulcano pipiline model
impl Op {
    pub fn full_scan(table_name: &str, engine: Rc<StorageEngine>) -> Op {
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

    pub fn project(cols: Vec<Column>, children: Vec<Op>) -> Op {
        Op::Project(ProjectInfo { cols }, children)
    }

    pub fn filter(_info: FilterInfo, children: Vec<Op>) -> Op {
        Op::Filter(FilterInfo {}, children)
    }

    pub fn open(&mut self) {
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

#[derive(Debug, PartialEq, Clone, Eq, Serialize, Deserialize)]
pub struct Tuple {
    data: HashMap<String, Val>,
}

impl Tuple {
    pub fn new(data: Vec<(&str, Val)>) -> Tuple {
        let mut map = HashMap::new();
        for (k, v) in data {
            map.insert(k.to_string(), v);
        }
        Tuple { data: map }
    }

    pub fn get(&self, key: &str) -> &Val {
        self.data.get(key).unwrap()
    }

    pub fn keys(&self) -> Vec<String> {
        self.data.keys().cloned().collect()
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Display, Serialize, Deserialize)]
pub enum Val {
    Int(i32),
    String(String),
    Bool(bool),
    Null,
}
