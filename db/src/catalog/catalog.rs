use std::collections::HashMap;

use crate::types::ColType;

use super::types::{ColumnId, ColumnSchema, TableId, TableSchema};

pub struct Catalog {
    store: MemoryCatalogStore,
}

impl Catalog {
    pub fn mem() -> Catalog {
        Catalog {
            store: MemoryCatalogStore::new(),
        }
    }

    pub fn register_table(&mut self, table: &TableSchema) -> Result<(), ()> {
        self.store.put(table)?;
        Ok(())
    }

    pub fn get_table(&self, table_id: &TableId) -> Option<TableSchema> {
        self.store.get(table_id)
    }

    pub fn drop_table(&mut self, table_id: &TableId) -> Result<(), ()> {
        self.store.delete(table_id)?;
        Ok(())
    }
}

trait CatalogStore {
    fn get(&self, id: &TableId) -> Option<TableSchema>;

    fn put(&mut self, table: &TableSchema) -> Result<(), ()>;

    fn delete(&mut self, id: &TableId) -> Result<(), ()>;
}

#[derive(Clone)]
struct MemoryCatalogStore {
    tables: HashMap<TableId, TableSchema>,
}

impl MemoryCatalogStore {
    fn new() -> MemoryCatalogStore {
        MemoryCatalogStore {
            tables: HashMap::new(),
        }
    }
}

impl CatalogStore for MemoryCatalogStore {
    fn get(&self, id: &TableId) -> Option<TableSchema> {
        self.tables.get(id).cloned()
    }

    fn put(&mut self, table: &TableSchema) -> Result<(), ()> {
        if self.tables.contains_key(&table.id) {
            return Err(());
        }

        self.tables.insert(table.id.clone(), table.clone());
        Ok(())
    }

    fn delete(&mut self, id: &TableId) -> Result<(), ()> {
        match self.tables.remove(id) {
            None => Err(()),
            Some(_) => Ok(()),
        }
    }
}

pub struct TableSchemaBuilder {
    schema_name: String,
    id: Option<TableId>,
    columns: Vec<ColumnSchema>,
}

impl TableSchemaBuilder {
    pub fn public() -> TableSchemaBuilder {
        TableSchemaBuilder {
            schema_name: "public".to_string(),
            id: None,
            columns: Vec::new(),
        }
    }

    pub fn table(&mut self, table_name: &str) -> &mut Self {
        self.id = Some(TableId::new(&self.schema_name, table_name));
        self
    }

    pub fn col(&mut self, col_name: &str, data_type: ColType) -> &mut Self {
        self.columns.push(ColumnSchema::new(
            &ColumnId::new(self.id.as_ref().unwrap(), col_name),
            data_type,
        ));

        self
    }

    pub fn build(&self) -> TableSchema {
        TableSchema::new(self.id.as_ref().unwrap(), self.columns.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn samle_schema() -> TableSchema {
        TableSchema::new(
            &TableId::public("table1"),
            vec![
                ColumnSchema::new(
                    &ColumnId::new(&TableId::public("table1"), "col1"),
                    ColType::Int,
                ),
                ColumnSchema::new(
                    &ColumnId::new(&TableId::public("table1"), "col2"),
                    ColType::Text,
                ),
            ],
        )
    }

    #[test]
    fn register_table() {
        let mut catalog = Catalog::mem();

        let table = samle_schema();

        let register_result = catalog.register_table(&table);
        assert_eq!(register_result, Ok(()));

        assert_eq!(catalog.get_table(&table.id), Some(table));
    }

    #[test]
    fn drop_table() {
        let mut catalog = Catalog::mem();

        let table = samle_schema();
        let register_result = catalog.register_table(&table);
        assert_eq!(register_result, Ok(()));

        let drop_result = catalog.drop_table(&table.id);
        assert_eq!(drop_result, Ok(()));

        assert_eq!(catalog.get_table(&table.id), None);
    }

    #[test]
    fn register_twice() {
        let mut catalog = Catalog::mem();

        let table = samle_schema();
        let register_result = catalog.register_table(&table);
        assert_eq!(register_result, Ok(()));

        let register_result = catalog.register_table(&table);
        assert_eq!(register_result, Err(()));
    }

    #[test]
    fn drop_twice() {
        let mut catalog = Catalog::mem();

        let table = samle_schema();
        let register_result = catalog.register_table(&table);
        assert_eq!(register_result, Ok(()));

        let drop_result = catalog.drop_table(&table.id);
        assert_eq!(drop_result, Ok(()));

        let drop_result = catalog.drop_table(&table.id);
        assert_eq!(drop_result, Err(()));
    }

    #[test]
    fn table_id() {
        let table_id = TableId::new("schema1", "table1");
        assert_eq!(table_id.get_table_name(), "table1");
        assert_eq!(table_id.get_schema_name(), "schema1");
        assert_eq!(table_id.to_fqn(), "schema1.table1");

        let table_id = TableId::public("table1");
        assert_eq!(table_id.get_table_name(), "table1");
        assert_eq!(table_id.get_schema_name(), "public");
        assert_eq!(table_id.to_fqn(), "public.table1");

        let table_id = TableId::from_fqn("schema1.table1");
        assert_eq!(table_id.get_table_name(), "table1");
        assert_eq!(table_id.get_schema_name(), "schema1");
    }

    #[test]
    fn column_id() {
        let table_id = TableId::public("table1");
        let column_id = ColumnId::new(&table_id, "col1");
        assert_eq!(column_id.get_column_name(), "col1");
        assert_eq!(column_id.get_table_id(), table_id);
        assert_eq!(column_id.to_fqn(), "public.table1.col1");

        let column_id = ColumnId::from_fqn("public.table1.col1");
        assert_eq!(column_id.get_column_name(), "col1");
        assert_eq!(column_id.get_table_id(), table_id);
    }

    #[test]
    fn builder() {
        let table = TableSchemaBuilder::public()
            .table("table1")
            .col("col1", ColType::Int)
            .col("col2", ColType::Text)
            .build();

        assert_eq!(
            table,
            TableSchema::new(
                &TableId::public("table1"),
                vec![
                    ColumnSchema::new(
                        &ColumnId::new(&TableId::public("table1"), "col1"),
                        ColType::Int,
                    ),
                    ColumnSchema::new(
                        &ColumnId::new(&TableId::public("table1"), "col2"),
                        ColType::Text,
                    ),
                ],
            )
        );
    }
}
