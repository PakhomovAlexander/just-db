use std::{cell::RefCell, rc::Rc};

use crate::{
    analyzer::Analyzer,
    catalog::Catalog,
    optimizer::{
        types::{StorageEngine, Tuple},
        Optimizer,
    },
    parser::{Lexer, Parser},
};

pub struct Db {
    catalog_rc: Rc<RefCell<Catalog>>,
    storage_rc: Rc<RefCell<StorageEngine>>,
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
        let catalog = Catalog::mem();
        let storage = StorageEngine::mem();

        let catalog_rc = Rc::new(RefCell::new(catalog));
        let storage_rc = Rc::new(RefCell::new(storage));

        let optimizer = Optimizer::new(Rc::clone(&catalog_rc));

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

        // TODO: do it better
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(e) => {
                eprintln!("Error parsing query: {}", e);
                return vec![];
            }
        };

        let l_plan = analyzer.analyze(ast);

        let mut p_plan = self.optimizer.optimize(l_plan);

        p_plan.execute_all(Rc::clone(&self.storage_rc), Rc::clone(&self.catalog_rc))
    }
}
