use db::embedded::Db;

#[test]
fn create_insert_select() {
    let db = Db::new();

    let create_table_result = db.run_query("CREATE TABLE table1 (col1 INT, col2 INT, col3 INT)");
    assert_eq!(create_table_result.len(), 0);

    let insert_result = db.run_query("INSERT INTO table1 (col1, col2, col3) VALUES (11, 22, 33)");
    assert_eq!(insert_result.len(), 0);

    let select_result = db.run_query("SELECT col1, col2, col3 FROM table1");
    assert_eq!(select_result.len(), 1);

    let insert_result =
        db.run_query("INSERT INTO table1 (col1, col2, col3) VALUES (110, 220, 330)");
    assert_eq!(insert_result.len(), 0);

    let select_result = db.run_query("SELECT col1, col2, col3 FROM table1");
    assert_eq!(select_result.len(), 2);

    let insert_result =
        db.run_query("INSERT INTO table1 (col1, col2, col3) VALUES (1100, 2200, 3300)");
    assert_eq!(insert_result.len(), 0);

    let select_result = db.run_query("SELECT col1, col2, col3 FROM table1");
    assert_eq!(select_result.len(), 3);
}
