# Programmer Guide

This document describes the internal structure of CSV-HAS-SQL for developers who want to understand, maintain, or extend the project.

## 1. Project overview

The application is a small command-line tool written in Haskell. It reads SQL-like commands, applies them to CSV files, and writes the result back to disk.

The main responsibilities are:

- parse user input into structured command data
- validate and execute commands against in-memory table representations
- read and write CSV files from the filesystem
- report errors through a small exception hierarchy

## 2. Architecture

The project is split into three logical layers:

### 2.1 Presentation layer

The executable entry point is in [app/Main.hs](../app/Main.hs).

It provides:

- interactive command input
- one-shot command execution with `-c`
- error handling for application exceptions

### 2.2 Command processing layer

The core logic lives in:

- [src-internal/Parsers.hs](../src-internal/Parsers.hs) for parsing commands
- [src-internal/Commands.hs](../src-internal/Commands.hs) for executing commands
- [src-internal/DataTypes.hs](../src-internal/DataTypes.hs) for shared data structures

### 2.3 I/O and exception layer

The file handling and error translation code is in:

- [src/LibControl.hs](../src/LibControl.hs)
- [src/LibExceptions.hs](../src/LibExceptions.hs)
- [src-internal/CommandExceptions.hs](../src-internal/CommandExceptions.hs)

## 3. Core data structures

The project uses a few small data types to model tables and commands.

### 3.1 Records and tables

In [src-internal/DataTypes.hs](../src-internal/DataTypes.hs), tables are represented as a vector of records.

Key types:

- `Record vs` — a single row or header row
- `Table rs` — a table containing rows and a header
- `Command` - a placeholder for parsed data
- `IOCommandData` - data for commands that require an input table
- `OutputCommandData` - data for commands that don't require an input table
- `WhereCondition c v` — a parsed filter condition
- `AlterData c` — ALTER subcommands
- `SetOperation` — `Union`, `Intersection`, or `Difference`

### 3.2 Command model

The parser produces values of type `Command d d2`, where:

- `OneTableCmd` represents operations on one CSV file
- `TwoTableCmd` represents set operations over two CSV files

The command payload is wrapped in `CommandData` and then converted into internal command types for execution.

## 4. Parsing flow

The parser in [src-internal/Parsers.hs](../src-internal/Parsers.hs) converts a string like:

```sql
INSERT INTO students (name, id) VALUES (Alice, 1);
```

into a structured value that the execution layer can process.

### Main parsing functions

- `commandParser` — parses top-level commands
- `whereParser` — parses WHERE clauses
- `parseWord` — parses identifiers and quoted strings
- `parseList` — parses comma-separated lists
- `parseDictionary` — parses `column = value` assignments

The parser uses Megaparsec and is designed to be strict about the expected syntax. If parsing fails, the program raises a `ParseException`.

## 5. Execution flow

The execution path is centered around [src/LibControl.hs](../src/LibControl.hs).

### 5.1 High-level execution sequence

1. The input string is parsed.
2. The parsed command is converted into internal command data.
3. The relevant CSV file is opened from disk.
4. The command is applied to the loaded table.
5. The resulting table is written back to the same path.

### 5.2 Important functions

- `runCommand` — entry point for executing a single textual command
- `openTable` — loads a CSV file into memory
- `saveTable` — writes an in-memory table back to CSV
- `applyCommand` — applies one-table commands
- `applyTwoTableCommand` — applies UNION / INTERSECTION / DIFFERENCE

## 6. Command implementation details

The command engine in [src-internal/Commands.hs](../src-internal/Commands.hs) implements the operations.

### Supported operations

- `create` — builds a new table with the supplied columns
- `insert` — appends rows to the table
- `update` — changes values for rows matching a condition
- `delete` — removes rows matching a condition
- `select` — projects chosen columns
- `alterAdd`, `alterDrop`, `alterRename` — modify the schema
- `applySetOperationCommand` — handles set-based operations

### Validation behavior

The engine validates table structure before many operations:

- table must not be empty
- header must not be empty
- column names must be unique
- every row must have the same length as the header

If validation fails, the application raises `InvalidTableFormatException`.

## 7. Error handling

The project uses a layered exception model.

### 7.1 Exception types

- [src-internal/CommandExceptions.hs](../src-internal/CommandExceptions.hs) defines command-level errors such as:
  - `ColumnNotFoundException`
  - `ColumnNameDuplicatedException`
  - `UnableToInterpretException`
  - `InvalidArgCountException`
  - `HeadersDifferException`
  - `InvalidTableFormatException`

- [src/LibExceptions.hs](../src/LibExceptions.hs) wraps these in application-level exceptions:
  - `IOTableException`
  - `ParseException`
  - `CmdException`

### 7.2 Error propagation

The executable entry point in [app/Main.hs](../app/Main.hs) catches application exceptions and prints a readable message through `translateException`.

## 8. Testing

The test suite in [test/Main.hs](../test/Main.hs) covers:

- parser correctness
- command execution behavior
- WHERE-condition evaluation
- invalid table shape detection

The tests are written with Hspec and can be executed using:

```bash
cabal test
```

## 9. Extension points

If you want to extend the application, the natural places to modify are:

- add a new parser rule in [src-internal/Parsers.hs](../src-internal/Parsers.hs)
- add a new command implementation in [src-internal/Commands.hs](../src-internal/Commands.hs)
- extend the command data model in [src-internal/DataTypes.hs](../src-internal/DataTypes.hs)
- update the CLI entry point in [app/Main.hs](../app/Main.hs) if the interface changes
- fix the problem where an input table is deleted when an exception is thrown (requires program refactor)

## 10. Build and run

From the project root:

```bash
cabal build
cabal run csv-has-sql
```

For tests:

```bash
cabal test
```
