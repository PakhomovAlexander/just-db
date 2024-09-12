use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, i32, space1},
    multi::separated_list1,
    IResult,
};

///
/// TREE STRUCTURE
///
/// *Queries*:
///  select col1, col2 from table1;
///  insert into table1 (col1, col2) values (1, 'valStr');
///  insert into table1 (col1) values (1);
#[derive(Debug, PartialEq)]
struct Queries {
    queries: Vec<Query>,
}

#[derive(Debug, PartialEq)]
enum Query {
    // select ...
    Select(SelectQuery),
    // insert ...
    Insert(InsertQuery),
    // delete ...
    Delete(DeleteQuery),
    // update ...
    Update(UpdateQuery),
    // create table ...
    CreateTable(CreateTableQuery),
    // drop table ...
    DropTable(DropTableQuery),
    // alter table ... AlterTable(AlterTableQuery),
}

/// *SelectQuery*:
///  select <select_statement>
///  from <from_statement>
///  where <where_statement>
///  order by <order_by_statement>
///  group by <group_by_statement>
///  having <having_statement>
///  limit <limit_statement>
#[derive(Debug, PartialEq)]
struct SelectQuery {
    select_statement: SelectStatement,
    from_statement: FromStatement,
    where_statement: Option<WhereStatement>,
    order_by_statement: Option<OrderByStatement>,
    group_by_statement: Option<GroupByStatement>,
    having_statement: Option<HavingStatement>,
    limit_statement: Option<LimitStatement>,
}

/// *InsertQuery*:
///  insert into <table_name> (<columns>) values (<values>)
#[derive(Debug, PartialEq)]
struct InsertQuery {
    table_name: String,
    columns: Vec<String>,
    values: Vec<String>,
}

/// *DeleteQuery*:
///  delete from <table_name> where <where_statement>
#[derive(Debug, PartialEq)]
struct DeleteQuery {
    table_name: String,
    where_statement: Option<WhereStatement>,
}

/// *UpdateQuery*:
///  update <table_name> set <set_statement> where <where_statement>
#[derive(Debug, PartialEq)]
struct UpdateQuery {
    table_name: String,
    set_statement: SetStatement,
    where_statement: Option<WhereStatement>,
}

/// *CreateTableQuery*:
///  create table <table_name> (<columns_definitions>) <constraints>
#[derive(Debug, PartialEq)]
struct CreateTableQuery {
    table_name: String,
    // TODO: Design this
    columns_definitions: Vec<String>,
    // TODO: Design this
    constraints: Vec<String>,
}

/// *DropTableQuery*:
///  drop table <table_name>
#[derive(Debug, PartialEq)]
struct DropTableQuery {
    table_name: String,
}

/// *AlterTableQuery*:
///  alter table <table_name> <action>
#[derive(Debug, PartialEq)]
struct AlterTableQuery {
    table_name: String,
    // TODO: Design this
    action: String,
}

#[derive(Debug, PartialEq)]
struct SelectStatement {
    columns: Vec<ColumnStatement>,
    distinct: bool,
}

#[derive(Debug, PartialEq)]
struct FromStatement {
    tables: Vec<TableStatement>,
    joins: Vec<JoinStatement>,
}

#[derive(Debug, PartialEq)]
struct TableStatement {
    table_name: String,
    alias: Option<String>,
}

#[derive(Debug, PartialEq)]
struct JoinStatement {
    table: TableStatement,
    // TODO: Design this
    join_type: String,
    on: Vec<Condition>,
}

#[derive(Debug, PartialEq)]
struct Condition {
    left: ColumnStatement,
    operator: Operator,
    right: ColumnStatement,
}

#[derive(Debug, PartialEq)]
enum Operator {
    Equal,
    NotEqual,
}

#[derive(Debug, PartialEq)]
struct WhereStatement {
    conditions: Vec<Condition>,
}

#[derive(Debug, PartialEq)]
struct OrderByStatement {
    columns: Vec<ColumnStatement>,
    order: Order,
}

#[derive(Debug, PartialEq)]
enum Order {
    Asc,
    Desc,
}

#[derive(Debug, PartialEq)]
struct LimitStatement {
    limit: i32,
}

#[derive(Debug, PartialEq)]
struct GroupByStatement {
    columns: Vec<ColumnStatement>,
}

#[derive(Debug, PartialEq)]
struct HavingStatement {
    conditions: Vec<Condition>,
}

#[derive(Debug, PartialEq)]
struct SetStatement {
    // TODO: Design this
    columns: Vec<String>,
}

#[derive(Debug, PartialEq)]
enum ColumnStatement {
    ColumnStatement(ColumnIdentifier),
    ColumnStatementLiteral(Literal),
    ColumnStatementFunction(Function),
}

#[derive(Debug, PartialEq)]
struct ColumnIdentifier {
    table_name: Option<String>,
    column_name: String,
}

#[derive(Debug, PartialEq)]
enum Literal {
    Integer(i32),
    String(String),
    Float(f32),
    Boolean(bool),
}

#[derive(Debug, PartialEq)]
struct Function {
    name: String,
    arguments: Vec<ColumnStatement>,
}

/// PARSERS

impl Queries {
    fn parse(input: &str) -> IResult<&str, Queries> {
        let (input, queries) = separated_list1(tag(";"), Query::parse)(input)?;

        Ok((input, Queries { queries }))
    }
}

impl Query {
    fn parse(input: &str) -> IResult<&str, Query> {
        let select_query_attempt = SelectQuery::parse(input);
        if select_query_attempt.is_ok() {
            return select_query_attempt
                .map(|(input, select_query)| (input, Query::Select(select_query)));
        }

        let insert_query_attempt = InsertQuery::parse(input);
        if insert_query_attempt.is_ok() {
            return insert_query_attempt
                .map(|(input, insert_query)| (input, Query::Insert(insert_query)));
        }

        panic!("Query not recognized");
    }
}

impl SelectQuery {
    fn parse(input: &str) -> IResult<&str, SelectQuery> {
        let (input, _) = tag("select")(input)?;
        let (input, _) = space1(input)?;
        let (input, select_statement) = SelectStatement::parse(input)?;
        let (input, _) = space1(input)?;
        let (input, _) = tag("from")(input)?;
        let (input, _) = space1(input)?;
        let (input, from_statement) = FromStatement::parse(input)?;
        let (input, _) = tag(";")(input)?;

        // select_statement: SelectStatement,
        // from_statement: FromStatement,
        // where_statement: Option<WhereStatement>,
        // order_by_statement: Option<OrderByStatement>,
        // group_by_statement: Option<GroupByStatement>,
        // having_statement: Option<HavingStatement>,
        // limit_statement: Option<LimitStatement>,
        // TODO:
        Ok((
            input,
            SelectQuery {
                select_statement,
                from_statement,
                where_statement: None,
                order_by_statement: None,
                group_by_statement: None,
                having_statement: None,
                limit_statement: None,
            },
        ))
    }
}

impl SelectStatement {
    fn parse(input: &str) -> IResult<&str, SelectStatement> {
        let (input, columns) = separated_list1(tag(", "), ColumnStatement::parse)(input)?;

        Ok((
            input,
            SelectStatement {
                columns,
                // TODO: Implement distinct
                distinct: false,
            },
        ))
    }
}

impl ColumnStatement {
    fn parse(input: &str) -> IResult<&str, ColumnStatement> {
        // alt((ColumnIdentifier::parse, Literal::parse, Function::parse))(input)
        let res = ColumnIdentifier::parse(input);
        if res.is_ok() {
            return res.map(|(input, column_identifier)| {
                (input, ColumnStatement::ColumnStatement(column_identifier))
            });
        }

        let res = Literal::parse(input);
        if res.is_ok() {
            return res
                .map(|(input, literal)| (input, ColumnStatement::ColumnStatementLiteral(literal)));
        }

        let res = Function::parse(input);
        if res.is_ok() {
            return res.map(|(input, function)| {
                (input, ColumnStatement::ColumnStatementFunction(function))
            });
        }

        panic!("ColumnStatement not recognized");
    }
}

impl ColumnIdentifier {
    fn parse(input: &str) -> IResult<&str, ColumnIdentifier> {
        let (input, column_name) = alphanumeric1(input)?;

        Ok((
            input,
            ColumnIdentifier {
                // TODO: Implement table_name.col_name
                table_name: None,
                column_name: column_name.to_string(),
            },
        ))
    }
}

impl Literal {
    fn parse(input: &str) -> IResult<&str, Literal> {
        let res = i32(input);
        if res.is_ok() {
            return res.map(|(input, i_value)| (input, Literal::Integer(i_value)));
        }

        let (input, _) = tag("'")(input)?;
        let (input, s) = alpha1(input)?;
        let (input, _) = tag("'")(input)?;

        Ok((input, Literal::String(s.to_string())))
    }
}

impl Function {
    fn parse(input: &str) -> IResult<&str, Function> {
        let (input, name) = alpha1(input)?;
        let (input, _) = tag("(")(input)?;
        let (input, arguments) = separated_list1(tag(", "), ColumnStatement::parse)(input)?;
        let (input, _) = tag(")")(input)?;

        Ok((
            input,
            Function {
                name: name.to_string(),
                arguments,
            },
        ))
    }
}

impl FromStatement {
    fn parse(input: &str) -> IResult<&str, FromStatement> {
        let (input, table) = TableStatement::parse(input)?;
        // let (input, _) = space1(input)?;
        // let (input, joins) = separated_list1(tag(" "), JoinStatement::parse)(input)?;

        Ok((
            input,
            FromStatement {
                tables: vec![table],
                // TODO: Implement joins
                joins: vec![],
            },
        ))
    }
}

impl TableStatement {
    fn parse(input: &str) -> IResult<&str, TableStatement> {
        let (input, table_name) = alphanumeric1(input)?;

        Ok((
            input,
            TableStatement {
                table_name: table_name.to_string(),
                // TODO: Implement alias
                alias: None,
            },
        ))
    }
}

impl InsertQuery {
    fn parse(input: &str) -> IResult<&str, InsertQuery> {
        let (input, _) = tag("insert")(input)?;
        let (input, _) = space1(input)?;
        let (input, _) = tag("into")(input)?;
        let (input, _) = space1(input)?;
        let (input, table_name) = alphanumeric1(input)?;
        let (input, _) = space1(input)?;
        let (input, _) = tag("(")(input)?;
        let (input, columns) = Self::parse_column_list(input)?;
        let (input, _) = tag(")")(input)?;
        let (input, _) = space1(input)?;
        let (input, _) = tag("values")(input)?;
        let (input, _) = space1(input)?;
        let (input, _) = tag("(")(input)?;
        let (input, values) = Self::parse_column_list(input)?;
        let (input, _) = tag(")")(input)?;
        let (input, _) = tag(";")(input)?;

        // TODO:
        Ok((
            input,
            InsertQuery {
                table_name: table_name.to_string(),
                columns: columns.iter().map(|s| s.to_string()).collect(),
                values: values.iter().map(|s| s.to_string()).collect(),
            },
        ))
    }

    fn parse_column_list(input: &str) -> IResult<&str, Vec<&str>> {
        separated_list1(tag(", "), alphanumeric1)(input)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn select_works() {
        let (remainder, queries) = Queries::parse("select col1, col2 from table1;").unwrap();

        assert_eq!(remainder, "");
        assert_eq!(
            queries,
            Queries {
                queries: vec![Query::Select(SelectQuery {
                    select_statement: SelectStatement {
                        columns: vec![
                            ColumnStatement::ColumnStatement(ColumnIdentifier {
                                table_name: None,
                                column_name: "col1".to_string()
                            }),
                            ColumnStatement::ColumnStatement(ColumnIdentifier {
                                table_name: None,
                                column_name: "col2".to_string()
                            })
                        ],
                        distinct: false
                    },
                    from_statement: FromStatement {
                        tables: vec![TableStatement {
                            table_name: "table1".to_string(),
                            alias: None
                        }],
                        joins: vec![]
                    },
                    where_statement: None,
                    order_by_statement: None,
                    group_by_statement: None,
                    having_statement: None,
                    limit_statement: None
                })]
            }
        );
    }
}

#[test]
fn insert_works() {
    let (remainder, insert_query) =
        Queries::parse("insert into table1 (col1, col2) values (1, valStr);").unwrap();

    assert_eq!(remainder, "");
    assert_eq!(
        insert_query,
        Queries {
            queries: vec![Query::Insert(InsertQuery {
                table_name: "table1".to_string(),
                columns: vec!["col1".to_string(), "col2".to_string()],
                values: vec!["1".to_string(), "valStr".to_string()]
            })]
        }
    );
}

#[test]
fn select_single_column() {
    let (remainder, query) = Queries::parse("select col1 from table1;").unwrap();

    assert_eq!(remainder, "");
    assert_eq!(
        query,
        Queries {
            queries: vec![Query::Select(SelectQuery {
                select_statement: SelectStatement {
                    columns: vec![ColumnStatement::ColumnStatement(ColumnIdentifier {
                        table_name: None,
                        column_name: "col1".to_string()
                    })],
                    distinct: false
                },
                from_statement: FromStatement {
                    tables: vec![TableStatement {
                        table_name: "table1".to_string(),
                        alias: None
                    }],
                    joins: vec![]
                },
                where_statement: None,
                order_by_statement: None,
                group_by_statement: None,
                having_statement: None,
                limit_statement: None
            })]
        }
    );
}

#[test]
fn insert_single_column() {
    let (remainder, insert_query) =
        Queries::parse("insert into table1 (col1) values (1);").unwrap();

    assert_eq!(remainder, "");
    assert_eq!(
        insert_query,
        Queries {
            queries: vec![Query::Insert(InsertQuery {
                table_name: "table1".to_string(),
                columns: vec!["col1".to_string()],
                values: vec!["1".to_string()]
            })]
        }
    );
}
