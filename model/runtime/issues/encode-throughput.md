# Stream and value encoding throughput

Benchmark suite and the full results table: lib/biolib/runtime-bench
(README.md; `bash run.sh`, `history.tsv` per labelled run). Reads, writes,
compression levels and `@flush` for byte arrays, small strings and FASTQ-like
record pairs in Rust, C++ and Python, against `dd` and `cat`.

Found 2026-10-13 while benchmarking lib/biolib (bio, biotrim, bioseqqc).
Critical: these make a morloc stream file slower to write and read than
re-parsing the FASTQ text it was converted from, which defeats the design
of converting inputs to morloc binary once and streaming over that. All
numbers on 0.109.0 (compiler ac7b581e), 12 cores,
measured with lib/biolib/bioprojects/tools/measure.py (wall seconds, peak
RSS of the largest process). Everything below is reproduced.

## Analyses

Each section below has a reviewed analysis with causes, solutions and
tests in model/performance/issues/ (LIST.md there): stream-copies.md
(sections 1 and 2), stream-compression-pipeline.md (section 3; also a
per-stream memory bound that lets N open compressed streams exhaust
memory).

## 1. Where decode and encode time goes, by value shape

Measured before Rust's vectors were copied in bulk (fixed 2026-10-14:
Rust now writes at C++ parity and reads 600 MiB of `Vector U8` in 0.40 s
against C++'s 0.27 s); the Rust columns are stale, the C++ ones stand.

Same 1M read pairs (2 x 150 bases, 624 MiB of plain FASTQ), each batch
converted to a shape and written at level 0 with `@write`, then read back
with `loadSource` + `foldSource` in the same pool (bio-rust / bio-cpp,
stream-rust; lib/biolib). Encode times include parsing the FASTQ, which is
the first row. Seconds, wall, 12 cores, one run each:

| shape per batch | bytes | Rust enc | Rust dec | C++ enc | C++ dec |
|---|---|---|---|---|---|
| parse FASTQ only | - | 1.78 | - | 1.45 | - |
| `[(FastqEntry, FastqEntry)]` records | 992 MB | 5.54 | 4.63 | 1.71 | 1.96 |
| `[Str]` headers only (2M strings) | 80 MB | 0.89 | 0.28 | 1.08 | 0.37 |
| `[Vector U8]` bases only (2M vectors) | 464 MB | 3.13 | 1.96 | 1.31 | 0.56 |
| `[Vector U8]` one 600 MB-per-file blob | 600 MB | 5.15 | 3.41 | 1.45 | 0.57 |

What this says:

- Small strings are cheap: 2M headers decode in 0.28 s (Rust) and 0.37 s
  (C++). They are not why the records are slow.
- In C++, decoding the records (1.96 s) costs more than parsing the FASTQ
  text (1.45 s). Decode copies every field into a fresh `std::vector` /
  `std::string`: 6 allocations and copies per pair, the same work a parser
  does. So a morloc stream file cannot be faster
  to read than its text unless decoding stops copying: borrowed views of
  primitive arrays and strings into the mapped file or the shared-memory
  block (a `&[u8]` / `std::span` in the native value), with ownership only
  where the host function asks for it.
- The extra cost of nesting (a pair of records of a record of two vectors)
  over its fields is about 0.5 s per 1M pairs in C++ (1.96 s against ~1.5 s
  for the fields), on the order of 0.1 us per nested value.
- Encode overlaps the producer: in C++ the blob encode adds almost nothing
  to the parse, consistent with write-behind running in parallel (section
  3). Section 2's 4x was measured with nothing to overlap.

Recommended order: (1) borrowed views for primitive arrays and strings on
decode, in Rust and C++; (2) measure again, then the per-value walk cost.

## 2. `@write` costs more than a plain file write in every language

Same program, C++ pool, constant data: 600 MiB written at level 0 in 1.59 s
against 0.39 s for `dd`. With pseudo-random read-like data and the time to
build the batches subtracted (lib/biolib/runtime-bench, 2026-10-13): C++
0.77 s, Python 1.02 s, Rust 0.73 s (2026-10-14), against 0.61 s for
`dd conv=fdatasync` (morloc's `@close` syncs; plain `dd` 0.44 s). Not yet located; candidates are a copy into shared memory
before the file write, page faults on a fresh buffer per batch, or
per-sub-packet bookkeeping. Each 10 MiB batch is its own `@write`.

## 3. `@flush` after each `@write` makes zstd writes 2.4x slower

Since 2026-10-14 a sub-packet compresses as several frames in parallel
(ZIP-9): 600 MiB written at level 3 with `@flush` per 10 MiB batch takes
4.3 s in C++ (was 11.3 s), against 2.4 s without `@flush`. The table
below predates that.

lib/biolib (formerly biobase) saved streams with
`writeBatch level o xs = do { Ok _ <- @write level o xs; @flush o; () }`.
Ingesting 1M FASTQ pairs (Rust reader) into a stream file:

| variant | seconds | peak RSS MiB | bytes |
|---|---|---|---|
| zstd 3, `@flush` per batch | 18.32 | 21 | 308 MB |
| zstd 3, no `@flush` | 7.70 | 403 | 306 MB |
| level 0, `@flush` per batch | 7.96 | 21 | 992 MB |
| level 0, no `@flush` | 7.17 | 107 | 992 MB |

Without `@flush`, consecutive `@write`s are coalesced into larger
sub-packets: the same 1M pairs read back as 62 batches instead of 613
(about 16k pairs, 10 MiB each), so a reader sees neither the writer's batch
boundaries nor an unbounded batch. Nothing in model/ says which is intended.
So `@flush` serializes compression with the caller (write-behind, see
streams.md, presumably overlaps it otherwise), and without it the writer
buffers ~400 MiB. Open: what `@flush` is for in a file that is only read
after `@close`; whether write-behind should bound its buffer (403 MiB peak
for 1 MiB batches is a lot of memory for a streaming writer).

## Workarounds in lib/biolib (remove when fixed)

- lib/biolib/stream `writeBatch` no longer calls `@flush`.
- None for 1 and 2: read pairs are still stored as morloc stream files, and
  the benchmarks in lib/biolib/{bio,biotrim,bioseqqc}/bench record the cost
  (bench/history.tsv) so a fix shows up as a measured change.
