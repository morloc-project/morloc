# Array data alignment

FMT-6 (formats.md) says numeric array data is 64-byte aligned, and
`Schema::array_data_alignment` (morloc-runtime-types schema.rs) returns 64
for primitive numeric elements. Deferred by the user 2026-10-12.

- [measured] Not delivered on every path. The C++, Python, R and
  rustmorloc writers align the absolute address (cppmorloc.hpp, pymorloc.c,
  rmorloc.c, rustmorloc lib.rs: `ALIGN_UP` of the cursor), so values they
  write are 64-aligned. The Rust flatten (voidstar.rs, the flatten walk)
  aligns offsets within its buffer, which lands at a 16-byte block start,
  so lists built from JSON land at 0/8/32/40 mod 64. Elements appended to a
  stream buffer (stream.rs `append_one_element`, write_behind.rs
  compaction) step by 8 bytes, so lists read back from a stream are only
  8-aligned (test
  numeric_lists_read_from_a_stream_keep_their_natural_alignment).
- [read] Nothing relies on it: no generated pool or binding assumes more
  than natural alignment, and numpy checks only natural alignment
  (pymorloc.c `PyArray_ISALIGNED`).
- [reproduced, 2026-10-17, bd61418a] Natural alignment is not met either,
  and the C++ binding relies on it. Run with
  `cpp:flags=-fsanitize=address,undefined;cpp:flags=-fno-sanitize-recover=all`,
  23 of the 347 C++-only goldens stop at a misaligned read in
  cppmorloc.hpp. The reads are a `const Array*` header (lines 732, 1595)
  or an `int64_t` (765, 775) at addresses such as `...c7` and `...f5`, so
  not even 8-aligned. Examples: string-interpolation, typeclasses-1,
  cpp-copy-count, futhark-basic, parse-directive.

  Undefined behaviour in C++: it works on x86, but a strict-alignment
  target may trap, and the optimizer may assume alignment. Not traced:
  whether the field offsets of a tuple or record are packed without
  padding, or the buffer base itself is misaligned. Fix: read wire fields
  with `memcpy` (or `std::bit_cast`), or align the layout.
- [read] What it costs today: each writer's sizing pass reserves 63 bytes
  per numeric array and pads up to 63, which dominates for many small
  arrays (`[[Int8]]` rows of a few bytes).
- [measured] What it buys (/work/tests/runtime/micro/align.c, gcc -O3
  -march=native, AMD Ryzen 5 7535U, AVX2, 2026-10-12): a load-only sum is
  the same at every offset. An axpy (load and store) with data in cache is
  30-43% slower at 4-32 KiB when data starts at 8 or 16 mod 64, and 17%
  slower at 512 KiB; data starting at 32 mod 64 is as fast as 0. Memory-bound
  (32 MiB) shows no difference. So on this machine 32 bytes suffice; 64
  matters for AVX-512 and for whole cache lines.
- Still untested: a 16-byte requirement (an Arrow decimal buffer inside a
  table element written to a stream), where the stream's 8-byte steps could
  misalign.
- To decide:
  1. Guarantee or preference. As a preference, FMT-6 states natural
     alignment as the contract and 64 as a writer's choice; writers could
     then stop reserving the padding.
  2. As a guarantee, for which arrays. Aligning helps large arrays and
     wastes space on many small ones. One rule: 64 when the data is at
     least some size (1 KiB keeps padding under 6%), natural below; every
     sizing pass knows count times width, so all writers can apply it.
  3. Then the work: the Rust flatten aligns absolute addresses (or a
     64-aligned base), stream buffers and the file layout align element
     data (mapped files are page-aligned, so file offsets carry it), copies
     preserve it, and a test per path (JSON, CLI argument, stream, IFile,
     each language's writer).
