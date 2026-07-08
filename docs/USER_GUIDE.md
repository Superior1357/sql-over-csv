# User Guide

This guide explains how to use CSV-HAS-SQL from the command line.

## 1. Running the program

You can start the tool in two ways:

- Interactive mode: run the program without arguments
- One-shot mode: type interactive and use the `-c` option to run one command

Example:

```bash
cabal run csv-has-sql
```

```bash
cabal run csv-has-sql -- noninteractive -c "CREATE students (name, id);"
```

## 2. General rules

- Every command must end with a semicolon `;`
- Table names are also file names, so the program will read and write files with that name
- The application works with CSV files stored in the current working directory
- If a command is invalid, the program prints an error message

## 3. Command reference

### CREATE

Creates a new CSV file with the provided column names.

```sql
CREATE table_name (column1, column2, column3);
```

Example:

```sql
CREATE students (name, id);
```

### INSERT INTO

Adds one or more rows to an existing table.

```sql
INSERT INTO table_name (column1, column2) VALUES (value1, value2);
```

Example:

```sql
INSERT INTO students (name, id) VALUES (Alice, 1), (Bob, 2);
```

### UPDATE

Updates values in rows that satisfy a WHERE condition.

```sql
UPDATE table_name SET (column1 = value1, column2 = value2) WHERE condition;
```

Example:

```sql
UPDATE students SET (name = Anna) WHERE id = 1;
```

### DELETE FROM

Deletes rows matching a WHERE condition.

```sql
DELETE FROM table_name WHERE condition;
```

Example:

```sql
DELETE FROM students WHERE id = 2;
```

### ALTER

Changes the table structure.

#### ADD

```sql
ALTER table_name ADD column_name;
```

#### DROP

```sql
ALTER table_name DROP COLUMN column_name;
```

#### RENAME

```sql
ALTER table_name RENAME COLUMN old_name TO new_name;
```

### SELECT

Returns selected columns from a table.

```sql
SELECT (column1, column2) FROM table_name;
```

### UNION

Combines two tables with the same structure.

```sql
UNION table1, table2 INTO table3;
```

### INTERSECTION

Keeps only rows that appear in both tables.

```sql
INTERSECTION table1, table2 INTO table3;
```

### DIFFERENCE

Keeps rows from the first table that do not appear in the second one.

```sql
DIFFERENCE table1, table2 INTO table3;
```

## 4. WHERE conditions

The WHERE clause is used to filter rows.

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
SELECT (name) FROM students WHERE id >= 2;
```

## 5. Example workflow

```bash
cabal run csv-has-sql -- noninteractive -c "CREATE students (name, id);"
cabal run csv-has-sql -- noninteractive -c "INSERT INTO students (name, id) VALUES (Alice, 1), (Bob, 2);"
cabal run csv-has-sql -- noninteractive -c "SELECT (name) FROM students;"
```

## 6. Tips

- Keep values consistent with the column types you intend to compare
- If you want to test commands interactively, start the program without arguments and type commands one by one