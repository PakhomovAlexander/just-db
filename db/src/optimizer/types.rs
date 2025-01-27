use std::{cell::RefCell, collections::HashMap, rc::Rc};

use serde::{Deserialize, Serialize};
use strum::Display;

use crate::catalog::{
    types::{ColumnSchema, TableSchema},
    Catalog,
};

#[derive(Debug, PartialEq)]
pub struct PhysicalPlan {
    pub root: Op,
}

impl PhysicalPlan {
    pub fn execute_all(
        &mut self,
        engine: Rc<RefCell<StorageEngine>>,
        catalog: Rc<RefCell<Catalog>>,
    ) -> Vec<Tuple> {
        let mut tuples = Vec::new();
        self.root.open(Rc::clone(&engine), Rc::clone(&catalog));
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
pub enum Op {
    Project {
        cols: Vec<Column>,
        children: Vec<Op>,
    },
    Filter {
        children: Vec<Op>,
    },
    FullScan {
        name: String,
        state: FullScanState,
        children: Vec<Op>,
    },

    CreateTable {
        ts: TableSchema,
    },
    InsertInto {
        table_name: String,
        tuples: Vec<Tuple>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub struct FullScanIterator {
    pub curr_pos: usize,
    pub tuples: Vec<Tuple>,
}

/// Vulcano pipiline model
impl Op {
    pub fn full_scan(table_name: &str) -> Op {
        Op::FullScan {
            name: table_name.to_string(),
            state: FullScanState {
                curr_pos: 0,
                iterator: None,
            },
            children: vec![],
        }
    }

    pub fn project(cols: Vec<Column>, children: Vec<Op>) -> Op {
        Op::Project { cols, children }
    }

    pub fn filter(children: Vec<Op>) -> Op {
        Op::Filter { children }
    }

    pub fn create_table(ts: TableSchema) -> Op {
        Op::CreateTable { ts }
    }

    pub fn insert_into(table_name: &str, tuples: Vec<Tuple>) -> Op {
        Op::InsertInto {
            table_name: table_name.to_string(),
            tuples,
        }
    }

    pub fn open(&mut self, engine: Rc<RefCell<StorageEngine>>, catalog: Rc<RefCell<Catalog>>) {
        match self {
            Op::FullScan { name, state, .. } => {
                let tuples = engine.borrow_mut().scan(name);
                let iter = FullScanIterator {
                    curr_pos: 0,
                    tuples,
                };

                state.iterator = Some(iter);
            }
            Op::Project { children, .. } => {
                for c in children {
                    c.open(Rc::clone(&engine), Rc::clone(&catalog));
                }
            }
            Op::Filter { children } => {
                for c in children {
                    c.open(Rc::clone(&engine), Rc::clone(&catalog));
                }
            }
            Op::CreateTable { ts } => {
                let _ = catalog.as_ref().borrow_mut().register_table(ts);
            }
            Op::InsertInto { table_name, tuples } => {
                engine.borrow_mut().insert(table_name, tuples.clone()); //FIXME: clone
            }
        }
    }

    fn next(&mut self) -> Option<Tuple> {
        match self {
            Op::FullScan { state, .. } => {
                let iter = state.iterator.as_mut().unwrap();
                if iter.curr_pos < iter.tuples.len() {
                    let t = iter.tuples[iter.curr_pos].clone();
                    iter.curr_pos += 1;
                    Some(t)
                } else {
                    None
                }
            }
            Op::Project { children, .. } => {
                // TODO: Implement projection
                children[0].next()
            }
            Op::Filter { children } => {
                let t = children[0].next().unwrap();
                Some(t)
            }
            Op::CreateTable { .. } => None,
            Op::InsertInto { .. } => None,
        }
    }

    fn close(&mut self) {
        match self {
            Op::FullScan { state, .. } => {
                state.iterator = None;
            }
            Op::Project { children, .. } => {
                for c in children {
                    c.close();
                }
            }
            Op::Filter { children } => {
                for c in children {
                    c.close();
                }
            }
            Op::CreateTable { .. } => {}
            Op::InsertInto { .. } => {}
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
