use core::panic;

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

            project: None,
            scan: None,
            filter: None,
        };

        builder.walk(&root_node);

        builder.build()
    }

    pub fn new(catalog: Catalog) -> Optimizer {
        Optimizer { catalog }
    }
}

struct PhysicalPlanBuilder<'a> {
    catalog: &'a Catalog,

    project: Option<Project>,
    scan: Option<TableScan>,
    filter: Option<Filter>,
}

impl PhysicalPlanBuilder<'_> {
    fn walk(&mut self, node: &LogicalNode) {
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
    col_schema: ColumnSchema,
}

impl Column {
    fn new(col_schema: &ColumnSchema) -> Column {
        Column {
            col_schema: col_schema.clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Filter {}

#[cfg(test)]
mod tests {
    use std::vec;

    use crate::{
        analyzer::{Analyzer, LogicalPlan},
        catalog::{Catalog, DataType, TableSchemaBuilder},
        optimizer::{Column, Project, TableScan},
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
                project: Project {
                    cols: vec![Column { col_schema: cs }]
                },
                filter: None,
                scan: TableScan {
                    name: "table1".to_string()
                }
            }
        );
    }
}
