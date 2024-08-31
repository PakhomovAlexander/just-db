








# High Level Design
This is a high level design of a simple database system. WIP.

## Network

### Considerations
- Use custom protocol that will be designed later.
- Security is completely out of scope.

## SQL

### Parser
- Takes a string
- Checks for syntax errors?
- Returns a parse tree

- maybe nom? 

### Analyzer
- Takes a parse tree
- Checks for semantic errors (for exmaple, no such table)
- Returns a query plan (logical plan)

### Optimizer
- Takes a query plan
- Optimizes the query plan
- Returns an optimized query plan (physical plan)


## Execution
- Takes a physical plan
- Executes the query
- Returns the result (in what format?)

## Storage
- Declares methods:
  - `get_by_key(key: bytes) -> bytes` 
  - `put(key: bytes, value: bytes) -> None`
  - `delete(key: bytes) -> None`
  - `scan(start_key: bytes, end_key: bytes) -> Iterator[Tuple[bytes, bytes]]`

## Transactions
- guarantee ACID properties -> WAL


## Configuration management
- Config file?

## Catalog
- Metadata about tables, columns, indexes, etc.
