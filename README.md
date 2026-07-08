# CSV-HAS-SQL

CSV-HAS-SQL is a small command-line tool for working with CSV files using SQL-like commands. It lets you create tables, add or update rows, change columns, filter data, and combine tables without needing a database server.

## Features

- Create a new CSV table from a list of column names
- Insert rows into an existing table
- Update or delete rows with a WHERE condition
- Add, drop, or rename columns
- Select specific columns from a table
- Combine tables with UNION, INTERSECTION, or DIFFERENCE

## Quick start

From the project root, build and run the application:

```bash
cabal build
cabal run csv-has-sql
```

For help run:
```bash
cabal run csv-has-sql -- --help
```

The program supports two modes:

- Interactive mode: run it without arguments and enter commands line by line
- One-shot mode: type noninteractive and use `-c` to run a single command

Example:

```bash
cabal run csv-has-sql -- noninteractive -c "CREATE students (name, id);"
```

This creates a file named `students` with desired columns in the current working directory.

## Basic usage

All commands must end with a semicolon `;`.

### Create a table

```sql
CREATE table_name (column1, column2, column3);
```

Example:

```sql
CREATE students (name, id);
```

### Insert data

```sql
INSERT INTO table_name (column1, column2) VALUES (value1, value2);
```

Example:

```sql
INSERT INTO students (name, id) VALUES (Alice, 1), (Bob, 2);
```

### Update rows

```sql
UPDATE table_name SET (column1 = value1, column2 = value2) WHERE condition;
```

Example:

```sql
UPDATE students SET (name = Anna) WHERE id = 1;
```

### Delete rows

```sql
DELETE FROM table_name WHERE condition;
```

Example:

```sql
DELETE FROM students WHERE id = 2;
```

### Alter columns

```sql
ALTER table_name ADD column_name;
ALTER table_name DROP COLUMN column_name;
ALTER table_name RENAME COLUMN old_name TO new_name;
```

Example:

```sql
ALTER students RENAME COLUMN id TO student_id;
```

### Select columns

```sql
SELECT (column1, column2) FROM table_name;
```


```sql
SELECT (name) FROM students;
```

### Set operations

```sql
UNION table1, table2 INTO table3;
INTERSECTION table1, table2 INTO table3;
DIFFERENCE table1, table2 INTO table3;
```

```sql
UNION students, students2 INTO students3;
```

## WHERE conditions

The WHERE clause filters rows based on a condition.

Supported operators:

- `=`
- `>`
- `<`
- `>=`
- `<=`
- `<>`
- `IN`

Example:

```sql
UPDATE students SET (name = "Anakin") WHERE name = Anna;
```

Numeric comparisons are applied only when the values can be interpreted as integers.

## Notes
- For an example run, change directory to examples and run examples.sh with the executable file path as a sole argument.
- Tables are stored as CSV files on disk.
- The tool reads and writes files using the given table name as the file path.
- Commands are evaluated immediately when entered.
- For a more detailed reference, see [docs/USER_GUIDE.md](docs/USER_GUIDE.md).
- For developer-oriented documentation, see [docs/PROGRAMMER_GUIDE.md](docs/PROGRAMMER_GUIDE.md).

