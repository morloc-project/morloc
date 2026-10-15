# Tables (TBL)

The cost of reading a table argument: a headed or headerless delimited file
read into a `Table`, a list of records or a list of tuples (CLI-13,
`@form list`). Every item is held against a standard: a plain Rust program,
written by hand with no morloc, that reads the same file into the same
shape and nothing more (`benchmarks/table-read/standard.rs`). It reads line
by line, splits on commas, parses each cell with the standard library, and
builds either columns (one buffer and offsets per string column, one vector
per other column) or a `Vec` of structs. It does no quoting and no header
check.

The benchmark table has six columns, `name` and `chrom` (`Str`), `start`
and `end` (`I64`), `score` (`Real`) and `keep` (`Bool`), about 45 bytes per
row as CSV. The consumer is a C++ pool that counts the rows, so the cost
includes building the native value and nothing else.

Series names are `<form>:<type>`: `table` is `@form table`, `list` is
`@form list`, `default` is a `Table` with no `@form`. `@form list` on a
`Table` and `@form table` on a list of tuples are build errors, so they
have no series.

### TBL-1 Reading a table costs at most twice the standard per row
Intent: ruled 2026-10-15
Bench: table-read
Model: t(n) = c + b*n for n rows; ratio = b / b_std, with b_std the standard reading the same file into columns (`Table`) or structs (lists)
Measured: 2026-10-15-1223-dc9bc32a+dirty, load 4.5, 1M rows (48 MB of CSV). b in ns per row; ratio over the standard:

| series | b | ratio | whole run |
|---|---|---|---|
| std (columns) | 169 | | 0.17 s |
| std (structs) | 278 | | 0.28 s |
| table:Table | 468 | 2.77 | 0.49 s |
| default:Table | 460 | 2.72 | 0.48 s |
| table:Records | 2216 | 7.98 | 2.24 s |
| list:Records | 2658 | 9.58 | 2.67 s |
| list:Tuples | 1634 | 5.89 | 1.65 s |

Limit: table:Table.ratio <= 2 x, default:Table.ratio <= 2 x, table:Records.ratio <= 2 x, list:Records.ratio <= 2 x

The slope is held, not the whole run: c includes nexus and pool start
(START). The bench also records `whole_ratio`, the whole run at the largest
size over the standard's. Lists of tuples are measured (`list:Tuples`) but
not held to the limit; no ruling covers them.

Every series breaks the limit (runtime/issues/encode-throughput.md). The
same 1M records read from a morloc packet take 0.26 s, so the pool's own
conversion is near the standard and the rest is the nexus turning CSV
into JSON text and loading that. A `Table` read from a packet takes 0.16
s; its CSV parse costs about 300 ns per row against the standard's 169.

### TBL-2 Reading a table holds at most twice the loaded value
Intent: ruled 2026-10-15
Bench: table-read
Model: peak_ratio = (P(n) - P(0)) / S(n): P the peak memory of the run, summed over its processes; S the size of the loaded value as a morloc packet; n the largest size
Measured: 2026-10-15-1223-dc9bc32a+dirty, load 4.5, 1M rows; S = 46.2 MiB (`Table`), 75.9 MiB (lists). Memory added over a header-only run:

| series | summed | ratio | largest process | ratio |
|---|---|---|---|---|
| std (columns) | 48 MiB | 1.04 | | |
| std (structs) | 139 MiB | 1.84 | | |
| table:Table | 154 MiB | 3.33 | 151 MiB | 3.26 |
| default:Table | 154 MiB | 3.33 | 151 MiB | 3.26 |
| table:Records | 813 MiB | 10.7 | 637 MiB | 8.39 |
| list:Records | 990 MiB | 13.0 | 776 MiB | 10.2 |
| list:Tuples | 828 MiB | 10.9 | 607 MiB | 7.99 |

Limit: table:Table.peak_ratio <= 2 x, default:Table.peak_ratio <= 2 x, table:Records.peak_ratio <= 2 x, list:Records.peak_ratio <= 2 x

P is the sum over the nexus and its pools of each process's VmHWM. That
bounds the concurrent peak from above, and counts shared memory touched by
two processes twice; `peak_max_ratio`, the largest single process, bounds
it from below. Ideally the ratio is well below 2; no goal is set. The
standard's own ratio is recorded as `std.table.peak_ratio` and
`std.records.peak_ratio`: a `Vec` that grows by doubling and a heap string
per cell already cost it more than the packet size.

Every series breaks the limit. A `Table` read from a morloc packet still
peaks at about 140 MiB over a header-only run, so most of its excess is in
how the loaded table is held, not in parsing. A list's excess is in the
nexus (637 of 813 MiB), which holds the input, its JSON text, a copy of
that and the JSON loader's working state at once (read, not profiled).

The limit rules out holding the input text, an intermediate form and the
value at once. A reader that meets it parses the input in bounded chunks
and appends each chunk to the value as it goes: column by column into a
`Table`, or a chunk of parsed columns at a time into the rows of a list.
Streaming a table through a program, rather than loading it whole, is a
language gap (language/issues/tables.md).
