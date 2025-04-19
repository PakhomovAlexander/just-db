use std::{cell::RefCell, rc::Rc};

use crate::{
    analyzer::{AnalyzeError, Analyzer},
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

    pub fn run_query(&self, query: &str) -> Result<Vec<Tuple>, AnalyzeError> {
        let lexer = Lexer::new(query);
        let mut parser = Parser::new(lexer);
        let analyzer = Analyzer::new();

        let l_plan = analyzer.analyze(parser.parse()?)?;

        let mut p_plan = self.optimizer.optimize(l_plan);

        Ok(p_plan.execute_all(Rc::clone(&self.storage_rc), Rc::clone(&self.catalog_rc)))
    }
}
