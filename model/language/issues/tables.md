# Tables

Holes against model/language/tables.md (TAB).

- **A table cannot be streamed.** [read] Gap in tables.md and streams.md:
  no item says how a program reads a table larger than memory, or starts
  on its first rows before its last arrive. A `Table` or list-of-records
  argument is loaded whole before the command runs (CLI-13), and a stream
  (STRM-1) has no form that reads a delimited, Arrow IPC or Parquet table
  as a sequence of row batches, so a table's peak memory is at least its
  whole value (performance/tables.md TBL-2). Open design: what type a
  streamed table has (an `IStream` of `Table` batches, of records, or
  both), how its schema is fixed and checked at open (STRM-1), and how a
  table result is written out batch by batch.
- **Is a table's column order part of its type?** [reproduced,
  2026-10-21] Gap in TAB-1 and TEQ-4. `Table n A` (record `A {x, y}`)
  against `Table n {y = Int, x = Int}` is rejected "Rec field order
  differs", the same rule as for records, which travel positionally. Arrow
  columns are named, so a reordered table may be sound to convert; no item
  says whether to accept it (reordering columns by name) or reject it.
