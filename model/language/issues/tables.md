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
