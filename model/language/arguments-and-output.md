# Arguments and output

Argument shapes, parsing, stdin, `@parse`, `@render`, output rendering. Prefix: CLI.

## Arguments

### CLI-1 An argument is read by its wire type
Intent: proposed
Code: deviates (../runtime/issues/nexus-cli.md)

A number, a `Bool` or a `Str` is its argv token, verbatim. Any other type is
inline JSON or a path to a file whose format (JSON, MessagePack, a morloc
packet) is detected from its bytes, not its name. A file at an optional text
type whose content is the token `null` is `Null`. The shape follows the wire
form, not the type's name: a type that packs to `[(a, b)]` reads as one.

### CLI-2 Argument shape is one atom per docstring field
Intent: proposed
Code: deviates (issues/programs.md)

`@source inline|file`, `@form list|table|bytes|bytes-only|packet` and `@check.path`
each take one value, and the `@list.` forms do the same per element. No field
takes an OR chain, and `auto` cannot be written. A combination the wire type
does not allow is a build error at the docstring line that wrote it.

### CLI-3 At most one argument per command reads standard input
Intent: proposed
Code: deviates (issues/programs.md)

A second `-` in one run is a run-time error. An `@stdin` argument is a `Str`,
the last positional, and at most one per command; anything else is a build
error.

### CLI-4 What `@default` is written in
Intent: open
Code: unaudited

The directive reference says `@default <json>`, but an unrolled `data`
argument takes a bare constructor (`@default dot`). Either (a) a default is
always the JSON of the value; or (b) a default is written as the argv token
the argument would accept.

## Tables

### CLI-13 `@form table` reads a headed table
Intent: ruled 2026-10-15
Tests: spec-cli-13-1, spec-cli-13-2
Code: conforms 2026-10-15

`@form table` applies to a `Table`, with or without declared columns, and to
a list of records whose fields are cells (CLI-17): `Str`, `Bool`, `Int`, the
sized numbers, `Real`, an enum, or an optional of one. An enum is not a
`Table` column. Any other type, tuples and `Map` included, is a build error,
and so is `@form table` on an option or on a field of an unrolled record:
an option's `@default` is JSON. The argument is a path or `-`, never an
inline value, and holds delimited text with a header row, Arrow IPC,
Parquet, or a morloc packet of the declared type. Input whose first byte
after a UTF-8 byte-order mark and ASCII space is `[` or `{` is JSON, and an
error. With `@source`, `@check.*`, `@list.*`, `@many` or `@parse` it is a
build error. The `@table.` directive names are reserved: any of them
is a build error.

### CLI-14 Header names bind columns by name
Intent: ruled 2026-10-15
Tests: spec-cli-14-1
Code: conforms 2026-10-15

For a list of records the header names exactly the record's fields, in any
order, and each column binds to the field it names. For a `Table` the
header contains every declared column; any other column is kept as `?Str`.
Names compare exactly. A missing, unexpected, duplicated, empty or
NUL-holding name is an error. Arrow IPC and Parquet columns bind by name
too, and their names follow the same rules. Into a list of records a
columnar value converts only between integer widths when it fits, from
`F32` to `Real`, and from a dictionary to its values; into a `Table` it
converts by the table's own rules, and an undeclared column keeps its type.

### CLI-15 The header decides the delimiter
Intent: ruled 2026-10-15
Tests: spec-cli-15-1
Code: conforms 2026-10-15

A leading UTF-8 byte-order mark is dropped; UTF-16 is an error. The first
record is split on TAB, then on comma, and the first split whose names meet
CLI-14 decides the delimiter of the whole input. A table with no declared
columns takes the first split with more than one name, and TAB otherwise.
A file's name is never consulted. This holds for every delimited table
read, with or without `@form table`. Under `@form table` a `Table`'s header
is checked before the command runs, from the first MiB of the file; a
longer header is an error.

### CLI-16 An empty cell is null
Intent: ruled 2026-10-15
Tests: spec-cli-16-1
Code: conforms 2026-10-15

An unquoted empty cell is null: absent for an optional field, an error for
any other. A quoted empty cell, `""`, is the empty `Str`, and an error for
any other type.

### CLI-17 How a cell spells a value
Intent: ruled 2026-10-15
Tests: spec-cli-17-1
Code: conforms 2026-10-15

`Bool` is `true` or `false` in any case. An integer is `-?[0-9]+`, in
range for its type, and an unsigned type takes no `-`; an `Int` in a list of
records has no bound, an `Int` column of a `Table` is 64 bits. A real is
`-?D+(.D+)?([eE][+-]?D+)?` or `nan`, `inf`, `-inf`; it reads as the nearest
value of its type, and a finite spelling that overflows is an error. An enum
is a constructor name. A `Str` is the cell's text verbatim, and holds no NUL.
Quoting changes no value except that `""` is not null. Nothing else is
accepted: no `+`, no surrounding space, no `NA`.

### CLI-18 Quoting and records
Intent: proposed
Tests: spec-cli-18-1
Code: conforms 2026-10-15

A cell that opens with `"` is quoted: it runs to a `"` not doubled, may hold
delimiters, line feeds and doubled quotes, and must end at a delimiter or
record end. A `"` later in an unquoted cell is a literal character. A record
ends at LF or CRLF; a CR elsewhere outside quotes is an error. Every line
ending ends a record and the last needs none, so a blank line is a record of
one empty cell; in a one-column table a trailing blank line is a null row
(ruled 2026-10-15). A record with a different number of cells from the header is
an error.

### CLI-19 A table result writes as headed text
Intent: ruled 2026-10-15
Tests: spec-cli-19-1
Code: conforms 2026-10-15

`-f csv` and `-f tsv` write a `Table` or a list of records as a header row
of names and one line per record, each ended by LF. Null is an empty cell,
the empty `Str` is `""`, and a cell or name holding TAB, comma, `"`, CR or
LF, or opening with `[` or `{`, is quoted with doubled quotes, whichever the
delimiter. `Bool` is `true`
or `false`; a real is the shortest text that reads back to it, or `nan`,
`inf`, `-inf`. What a command writes this way, an argument of the same type
reads back as an equal value under `@form table`, for a list of records and
for a `Table` whose undeclared columns are `Str`.

### CLI-20 A table read error names where it is
Intent: proposed
Code: deviates model/runtime/issues/failure-classification.md

A rejected table names the line (or row, for a columnar file), the column,
the cell and the type it is not, and is reported before the command runs.
A `Table` is read by the pool that holds it, so only its header is checked
before the command runs; a cell error reaches the caller through the pool's
failure (the deviation).

## Output

### CLI-5 A command writes its result to stdout in the `-f` form
Intent: proposed
Code: deviates (../runtime/issues/nexus-cli.md)

A `-f` form the result type cannot produce is an error, never an
approximation. In JSON, a top-level `()` or `Null` prints nothing unless
`--keep-null` is given; binary forms always write the value, and a `null`
inside a value is always written.

## Output actions

### CLI-6 An output action is a flag that applies a handler to the output
Intent: proposed
Code: conforms 2026-10-21

`--' @with -s/--name=f` and `--' @render -s/--name=f` each add a flag
`-s/--name[=PATH]` to the command. `f :: A -> B` with `A` unifying with what
the command writes to stdout. Given a `PATH`, the action writes its output
there instead of to stdout, and several actions given paths may run in one
run. `@with` keeps `B` typed, so it is written in the `-f` form (CLI-5).

### CLI-7 Which output reaches standard output
Intent: proposed
Code: conforms 2026-10-21

In order, the first that applies:

1. `--no-stdout`: nothing.
2. An action flag given without a path. Two such flags in one run are an
   error.
3. The `@default` action, unless `-f` was given or that action was given a
   path.
4. The command's own value, in the `-f` form; without `-f`, a value whose
   type carries `@mime` is its bytes (IFACE-19).

At most one action per command is `@default`; a second is a build error.

### CLI-8 A `@render` action writes its handler's bytes verbatim
Intent: proposed
Code: deviates model/runtime/issues/cli-directives.md (return type unchecked)

The handler returns `Str`, `[Str]`, `Vector U8` or `[Vector U8]`, and its
bytes are written without quoting or escaping. Any other return type is a
build error at the directive.

### CLI-9 `-f` on a run whose stdout is a `@render` action
Intent: open
Code: unaudited

The code ignores `-f` when an action flag picks a `@render` action, which
CLI-5 forbids. Either (a) `@render` actions are exempt from `-f`; or (b)
`-f` together with a `@render` action flag is an error.

## Help

### CLI-10 An absent optional help field is left empty
Intent: ruled 2026-10-10
Tests: spec-cli-10-1
Code: conforms 2026-10-21

When a value help would show was not declared (a description, for one), its
slot is printed empty. Help never fills it with a different field, such as
a media type or a type name.

### CLI-11 The Return block of a command with output actions
Intent: ruled 2026-10-10
Tests: spec-cli-11-1
Code: conforms 2026-10-10

A command without actions prints `Return: T` and then its return
description lines. A command with actions prints `Return:` and then:

- `type:` what the command writes when no action writes stdout (CLI-7 case
  4); for a streaming command, the batch element type.
- `desc:` the return description, when there is one.
- `actions:` one row per action in declaration order: the flag, the type of
  its output (its media type when it has one), `(raw bytes)` for a
  `@render` action, and `default` for the `@default` action.

### CLI-12 `--json-help` carries every epilogue verbatim
Intent: ruled 2026-10-10
Tests: spec-cli-12-1
Code: deviates (issues/programs.md)

`program.epilogues` and each command's `epilogues` list that docstring's
`@epilogue` blocks in order, one string per block, its lines joined by
newlines and nothing trimmed. No blocks is `[]`.
