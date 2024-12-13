#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub struct TableId {
    schema_name: String,
    table_name: String,
}

impl TableId {
    pub fn new(schema_name: &str, table_name: &str) -> TableId {
        TableId {
            schema_name: schema_name.to_string(),
            table_name: table_name.to_string(),
        }
    }

    pub fn public(table_name: &str) -> TableId {
        TableId::new("public", table_name)
    }

    pub fn to_fqn(&self) -> String {
        format!("{}.{}", self.schema_name, self.table_name)
    }

    pub fn from_fqn(fqn: &str) -> TableId {
        let parts: Vec<&str> = fqn.split('.').collect();
        TableId::new(parts[0], parts[1])
    }

    pub fn get_table_name(&self) -> String {
        self.table_name.clone()
    }

    pub fn get_schema_name(&self) -> String {
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
    pub fn new(id: &ColumnId, data_type: DataType) -> ColumnSchema {
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
