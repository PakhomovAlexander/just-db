





Today we are going to:
- [ ] write simple CLI for the database
```bash
> just-db
just-db> create table t1 (col1 int, col2 int)
ok
just-db> select * from t1
| col1 | col2 |
|------|------|
|      |      |
just-db> insert into t1 values (1, 2)
ok
just-db> select * from t1
| col1 | col2 |
|------|------|
| 1    | 2    |


```

--
- [ ] design storage API
- [ ] rewrite logical plan to have full qualified names for tables
   - [x] implement Catalog API
- [ ] try to write THE SIMPLEST optimizer in the world
   - [ ] FilterRow 
     - [ ] expression eval/analyze // WHERE 1 = col1 + col2
     - [ ] Tuple API

- [x] Rc, RefCell, Cell, Box
- [x] execute first plan ?*
  - [x] design a pipeline model / volcano model
  - [x] transform physical plan to pipeline 








