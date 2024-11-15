use std::collections::HashMap;

pub struct Catalog {
    store: Box<dyn CatalogStore>,
}

impl Catalog {
    pub fn mem() -> Catalog {
        Catalog {
            store: Box::new(MemoryCatalogStore::new()),
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

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub struct TableId {
    schema_name: String,
    table_name: String,
}

impl TableId {
    fn new(schema_name: &str, table_name: &str) -> TableId {
        TableId {
            schema_name: schema_name.to_string(),
            table_name: table_name.to_string(),
        }
    }

    pub fn public(table_name: &str) -> TableId {
        TableId::new("public", table_name)
    }

    fn to_fqn(&self) -> String {
        format!("{}.{}", self.schema_name, self.table_name)
    }

    fn from_fqn(fqn: &str) -> TableId {
        let parts: Vec<&str> = fqn.split('.').collect();
        TableId::new(parts[0], parts[1])
    }

    fn get_table_name(&self) -> String {
        self.table_name.clone()
    }

    fn get_schema_name(&self) -> String {
        self.schema_name.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ColumnId {
    table_id: TableId,
    column_name: String,
}

impl ColumnId {
    pub fn new(table_id: &TableId, column_name: &str) -> ColumnId {
        ColumnId {
            table_id: table_id.clone(),
            column_name: column_name.to_string(),
        }
    }

    pub fn to_fqn(&self) -> String {
        format!("{}.{}", self.table_id.to_fqn(), self.column_name)
    }

    pub fn from_fqn(fqn: &str) -> ColumnId {
        let parts: Vec<&str> = fqn.split('.').collect();
        ColumnId::new(&TableId::new(parts[0], parts[1]), parts[2])
    }

    pub fn get_column_name(&self) -> String {
        self.column_name.clone()
    }

    pub fn get_table_id(&self) -> TableId {
        self.table_id.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TableSchema {
    pub id: TableId,
    pub columns: Vec<ColumnSchema>,
}

impl TableSchema {
    pub fn new(id: &TableId, columns: Vec<ColumnSchema>) -> TableSchema {
        TableSchema {
            id: id.clone(),
            columns,
        }
    }

    pub fn get_column(&self, column_name: &str) -> Option<ColumnSchema> {
        // TODO: Use a hashmap to store columns
        let column_id = ColumnId::new(&self.id, column_name);
        self.columns.iter().find(|c| c.id == column_id).cloned()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ColumnSchema {
    pub id: ColumnId,
    pub data_type: DataType,
}

impl ColumnSchema {
    fn new(id: &ColumnId, data_type: DataType) -> ColumnSchema {
        ColumnSchema {
            id: id.clone(),
            data_type,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
    Int,
    String,
}

trait CatalogStore {
    fn get(&self, id: &TableId) -> Option<TableSchema>;

    fn put(&mut self, table: &TableSchema) -> Result<(), ()>;

    fn delete(&mut self, id: &TableId) -> Result<(), ()>;
}

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

    pub fn col(&mut self, col_name: &str, data_type: DataType) -> &mut Self {
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
                    DataType::Int,
                ),
                ColumnSchema::new(
                    &ColumnId::new(&TableId::public("table1"), "col2"),
                    DataType::String,
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
            .col("col1", DataType::Int)
            .col("col2", DataType::String)
            .build();

        assert_eq!(
            table,
            TableSchema::new(
                &TableId::public("table1"),
                vec![
                    ColumnSchema::new(
                        &ColumnId::new(&TableId::public("table1"), "col1"),
                        DataType::Int,
                    ),
                    ColumnSchema::new(
                        &ColumnId::new(&TableId::public("table1"), "col2"),
                        DataType::String,
                    ),
                ],
            )
        );
    }
}
