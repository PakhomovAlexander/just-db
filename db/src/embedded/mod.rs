use std::rc::Rc;

use crate::{
    analyzer::Analyzer,
    catalog::{types::DataType, Catalog, TableSchemaBuilder},
    optimizer::{types::StorageEngine, types::Tuple, types::Val, Optimizer},
    parser::{Lexer, Parser},
};

pub struct Db {
    catalog_rc: Rc<Catalog>,
    storage_rc: Rc<StorageEngine>,
    analyzer: Analyzer,
    optimizer: Optimizer,
}

impl Default for Db {
    fn default() -> Self {
        Self::new()
    }
}

impl Db {
    pub fn new() -> Self {
        let mut catalog = Catalog::mem();
        let mut storage = StorageEngine::mem();

        let ts = TableSchemaBuilder::public()
            .table("table1")
            .col("name", DataType::Int)
            .col("address", DataType::Int)
            .col("email", DataType::Int)
            .build();
        let _ = catalog.register_table(&ts);

        storage.insert(
            "table1",
            vec![
                Tuple::new(vec![
                    ("name", Val::Int(1)),
                    ("address", Val::Int(2)),
                    ("email", Val::Int(10)),
                ]),
                Tuple::new(vec![
                    ("name", Val::Int(111)),
                    ("address", Val::Int(2222)),
                    ("email", Val::Int(10000)),
                ]),
                Tuple::new(vec![
                    ("name", Val::Int(13211)),
                    ("address", Val::Int(2)),
                    ("email", Val::Int(10)),
                ]),
                Tuple::new(vec![
                    ("name", Val::Int(1)),
                    ("address", Val::Int(2)),
                    ("email", Val::Int(10)),
                ]),
            ],
        );

        let catalog_rc = Rc::new(catalog);
        let storage_rc = Rc::new(storage);

        let optimizer = Optimizer::new(Rc::clone(&catalog_rc), Rc::clone(&storage_rc));

        Self {
            catalog_rc,
            storage_rc,
            analyzer: Analyzer::new(),
            optimizer,
        }
    }

    pub fn run_query(&self, query: &str) -> Vec<Tuple> {
        let lexer = Lexer::new(query);
        let mut parser = Parser::new(lexer);
        let analyzer = Analyzer::new();

        let l_plan = analyzer.analyze(&parser.parse());

        let mut p_plan = self.optimizer.optimize(l_plan);

        p_plan.execute_all()
    }
}
