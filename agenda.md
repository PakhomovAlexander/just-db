





Today we are going to:
- [ ] write TUI for the database 
  - [x] cargo workspace
  - [x] draw real table 
  - [x] draw editor <- 
  - [ ] use real tables in TUI

```bash
> just-db



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








