# CSV-HAS-SQL
An easy-to-use and efficient Haskell library providing SQL-like queries over CSV files.
## Features
- create a CSV file from given specification
- edit records with a given key
- add/remove columns
- set operations (intersection, union, difference)
## Usage
When run without arguments, the program reads the standard input. 
Commands are evaluated immediately.
### Options
    -c      Run a command directly without launching the interactive terminal.
    -h      Show this help and exit.
### Commands
#### `CREATE`
Create a csv file with the given name, header and store it in the working directory.

```
CREATE table_name (
    column1_name, 
    column2_name,
    ...,
    columnN_name
);
```
##### Example
###### Input
    CREATE name_ids (name, id);
###### Output
> ./name_ids.csv:
```
name,id

```

#### `INSERT INTO`
Add new records with specified values to a specified table.
```
INSERT INTO table_name (column1 column2, column3, ...)
VALUES (value11, value12, value13, ...),
       (value21, value22, value23, ...);
```

#### `UPDATE`
Update or modify one or more records in the table.
```
UPDATE table_name
SET column1 = value1, column2 = value2, ...
WHERE condition;
```

#### `WHERE`
The WHERE clause is used to extract only those records that fulfill a specific condition.
##### Supported operators
|  Operator  | Description |
|    ---     |     ---     |
|     =      |    Equal   |
|     >      | Greater than |
|     <      | Less than |
|     >=     | Greater than or equal |
|     <=     | Less than or equal |
|     <>     | Not equal. |
|     BETWEEN| Between a certain range.
|     IN     | One of the specified values.

#### `DELETE FROM`
Delete existing records in the table.
```
DELETE FROM table_name WHERE condition;
```
#### ALTER
Add, modify or delete columns.
##### ADD
Add a column with the given name.
```
ALTER table_name ADD column_name;
```
##### DROP
Delete a column with the given name.
```
ALTER table_name DROP COLUMN column_name;
```
##### RENAME
Rename a column.
```
ALTER table_name RENAME COLUMN old_column_name TO new_column_name;
```
#### SELECT
Select data from a table.
```
SELECT column1, column2, ...
FROM table_name;
```
#### UNION
Make a union of 2 tables of the same type.
```
UNION table1_name table2_name INTO table3_name;
```
#### INTERSECTION
Find the intersection of records of 2 tables of the same type.
```
INTERSECTION table1_name table2_name INTO table3_name;
```
#### DIFFERENCE
Find the difference of records of 2 tables
of the same type.
```
DIFFERENCE table1_name table2_name INTO table3_name;
```

