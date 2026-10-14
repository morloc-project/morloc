# Stream reads and writes copy each batch several times

Status: analysed 2026-10-13, reviewed by the devil's advocate, not fixed.
Parent: runtime/issues/encode-throughput.md sections 1b and 2. Source:
lib/biolib/runtime-bench README sections 2 and 6.

## Evidence

C++ pool, 600 MiB of `[Vector U8]` as 60 batches of one 10 MiB vector,
level 0, program start (~0.1-0.15 s) included:

| | morloc | baseline |
|---|---|---|
| write (net of `make`) | 0.77 s | `dd bs=10M` 0.44 s |
| read | 0.26 s | `cat` 0.07 s |

1M `(Read, Read)` pairs read in 0.81 s, ~0.5 s more than the same bytes as
one array. In lib/biolib rnaseq, decoding FASTQ pairs from a stream file
(1.96 s) costs more than parsing the FASTQ text (1.45 s).

Each row is one run; start-up varies by 0.1-0.15 s, as large as some of
the effects below. Paths: `R` = data/rust/morloc-runtime/src, `C` =
data/lang/cpp/cppmorloc.hpp, `M` = data/morloc/morloc.h; generated pool = runtime-bench/bench-cpp-build/
pools/cpp/pool.cpp. Line numbers at HEAD afc848b5 (the stream code is
unchanged since the benchmarked a2e21e97).

## Read path, `@next` [read]

1. The pool's `_mlc_next` calls `mlc_next`, `from_voidstar`, then
   `shfree_cpp` on the block. The batch is then moved, not copied.
2. `next_file_subpacket` (R/stream.rs:4086-4142) reads the stream through
   a read-only `MAP_PRIVATE` mmap of the file.
   `materialize_subpacket_at_offset` (R/stream.rs:5772-5848) returns a
   pointer into the mapping for an uncompressed, 8-aligned sub-packet,
   copies an unaligned one into shared memory, and decompresses a
   compressed one into shared memory.
3. But `materialize_and_finalise_subpacket` (R/stream.rs:4183-4221) turns
   even the in-mapping pointer into a shared-memory copy:
   `read_binary_with_hint` (R/voidstar.rs:580-589) `shmalloc`s the whole
   sub-packet, copies it, and walks the schema to relocate relative
   pointers. The "zero-copy" comment at R/stream.rs:5745 holds only for
   the IFile index path.
4. `drop_read_pages` (R/stream.rs:4148-4177) remaps the range behind the
   cursor to drop page-table entries.
5. C++ `from_voidstar` (C:1134-1145) decodes: `assign` per vector (one
   malloc and memcpy), a `std::string` per string.
6. `shfree` scrubs the whole block to zero (R/shm.rs:1856-1896, SHM-6).

Per 10 MiB batch: copy mapping -> shared memory, copy shared memory ->
`std::vector` (a fresh allocation each batch, whose page faults are
unmeasured), zero the block; plus mapping fault-around faults and one
`MAP_FIXED` remap per batch, which `cat` does not pay. `cat` copies once
into a buffer that stays in cache. Three passes over 600 MiB at 10-15
GB/s (+-50% for a 16 MiB-L3 laptop) is roughly the measured 0.26 s less
start-up. [inferred]

Per pair: 6 mallocs (2 headers, longer than the 15-byte SSO, and 4
vectors) and their frees, 6 small copies, 2 schema walks (relocate and
decode), plus the bulk landing copy and scrub. ~12 allocator calls and 2
walks per pair fit the ~0.5 us per pair. [inferred]

## Write path, `@write` [read]

In the pool:
1. `_mlc_write`: size walk, `shmalloc` a 10 MiB block, `to_voidstar`
   (bulk memcpy for the vector, C:1323-1330): **copy 1**. After
   `mlc_write`, `shfree` scrubs it: **memset 2**.
2. `shared_write_subpacket` (R/stream.rs:3891-4012) makes a fresh
   `scratch: Vec<u8>` per `@write` (:3959). `append_one_element`
   (:3701-3878) flattens: size walk, `buf.resize(total, 0)`
   (R/voidstar.rs:1179), **memset 1**; copy into scratch, **copy 2**; copy
   scratch into the slot's write buffer (:3836-3850), **copy 3**.
3. The write buffer itself is reused through spares (`take_spare`,
   custody.rs:854-864) and shared-memory volumes are pre-faulted
   (`MADV_POPULATE_WRITE`, R/shm.rs:89-125), so no fresh pages per batch.

In the nexus custodian (SLOT-10):
4. `compact_sealed_buffer` (custody.rs:595-754, write_behind.rs:204-240):
   the index starts with room for 1024 entries (R/stream.rs:100), one is
   used, so the 10 MiB data region is moved left by ~16 KiB: **copy 4**,
   a memmove.
5. `emit` (custody.rs:756-790): `pwritev` of header and payload (**copy
   5**, in the kernel, which `dd` also pays), `pwrite` header, `pwrite` a
   temporary footer after every sub-packet.

What is on the critical path: at level 0 the custodian's queue depth is 1
(custody.rs:13, 213-216) and it works on batch k-1 while the pool makes
batch k (~15 ms of `rb_fill`), so copy 4, copy 5 and the page-cache write
are off the critical path except for the last batch. The pool's path per
batch is copy 1, memset 1, copy 2, copy 3, memset 2: ~3 GB over the run,
~0.2-0.3 s. `scratch` is a fresh 10 MiB allocation per `@write`; whether
it faults per call is unmeasured. The rest of the 0.77 s is unattributed;
the leading candidate is the `fdatasync` at `@close` (custody.rs:829):
600 MiB is under the dirty-background threshold, so it is written
synchronously at close, while the `dd` baseline does not sync. Not
comparable with `dd` until both sync or neither does. [inferred]

For pairs the write path walks each value about six times: C++ size and
write walks, Rust size walk, flatten, shift (`append_one_element`), and the
custodian's compaction shift. (A stdout stream adds one more copy in
`send_stdio`, custody.rs:796-799.)

## Solutions

No type or API change, each measurable alone:

- **S0. Measure first.** Run once with `WORKDIR` on tmpfs and once with
  `dd conv=fdatasync` (or time `@close` separately); print `ru_minflt`
  (measure.py already calls `getrusage`); report the median of 5 runs.
- **W1. Flatten straight into the write buffer.** The largest
  critical-path win with no lock or ownership change: flatten already
  runs under the slot lock (`R/stream.rs:3922` on), so SLOT-12 is
  unchanged. `FlattenWalk` keeps the root slot separate from the cursor;
  starting it at the buffer's index offset and data offset produces
  buffer-relative pointers directly, removing memset 1, a copy, the
  scratch allocation and the per-element shift walk (which matters for
  pairs). Two hazards: `FlattenWalk` relies on a zero-filled buffer for
  alignment gaps (R/voidstar.rs:1132, 1179) and spare buffers are not
  scrubbed (`give_spare`, custody.rs:858), so the walk must zero its gaps
  or files carry stale bytes; and the size bound assumes the cursor starts
  at the slot width, so starting at the data offset must use a worst-case
  bound or keep the data offset congruent mod 64. (A thread-local
  `scratch` alone saves only the allocation: `resize(total, 0)` still
  zero-fills.)
- **R1. Decode inside the claim.** `mlc_next_with(handle, decode, ctx)`
  runs the C++ decoder inside `with_process_local_slot`, after the claim
  and before `drop_read_pages`, with no slot lock held (dropped at
  `R/stream.rs:4137`). The claimed `ProcessLocalSlot` keeps its mapping
  for the call; a pointer that escaped the call would dangle, since
  mappings are unmapped on idle overflow (`R/stream.rs:1062-1069`), by the
  SLOT-9 sweep and at close (SLOT-23). C++ decodes in payload space
  (`morloc_payload_space`, M:1298-1352; as `mlc_read_inline_packet`,
  C:1147-1157): payload relptrs are payload-relative and the resolver
  masks the offset bits. The trampoline catches every C++ exception and
  rethrows after Rust returns. Table schemas, unaligned and compressed
  payloads keep the landing copy. No new ownership rule; fork, sweep and
  multi-reader behaviour unchanged. Removes the landing copy, relocation
  walk and scrub. Python cannot use it (numpy views need a shared-memory
  owner).
- **W2. Move the index, not the data.** Instead of memmoving the 10 MiB
  data region left by the unused index (`wasted`), move the 16 + n*w
  header and index right by `wasted` and emit one slice from `buf +
  wasted`: the same relptr shift, no iovec, no format change, works for
  the compressor, stdio and channels; stays 8-aligned. Off the critical
  path at level 0 here: a CPU and bandwidth win, and a wall-time win for
  channels, which compact in the pool under the lock (`R/stream.rs:3559`).
- **W3. Encode in place (model first).** The pool reserves space in the
  slot's buffer and C++ encodes into it. C++ must then write
  buffer-relative pointers (it writes shared-memory relptrs via
  `abs2rel_cpp` today) and path-form stream handles (`PortablePayload`
  checks this only in debug builds, `R/stream.rs:3365`); handle encoding
  (a registry acquire) moves under the slot lock (lock order to model);
  reserve must not advance counters before commit, and a C++ exception
  must not unwind through Rust; oversize, staged and channel paths keep
  staging. Changes SLOT-12's text.
- **R2.** Decompress into a reused, bounded private buffer and decode in
  payload space (with R1's callback), skipping the shared-memory block
  and its scrub. A thread-local buffer needs a STATE-1 registry row.

Rejected: scrubbing large blocks with `MADV_REMOVE`/hole punching instead
of a memset. Re-allocating freed tmpfs pages on a full `/dev/shm` raises
SIGBUS on the next write, which pre-faulting exists to prevent
(R/shm.rs:21-27, 80-139), and the kernel's zero-fill on refault is itself
a pass. Lazy zeroing would be an SHM-6 ruling, not an optimization.

Borrowed views (type-visible, after the above):

- **A. C++ owner-holding views.** `mlc::bytes` / `mlc::view<T>` = pointer,
  length, `shared_ptr` owner; read-only, `std::span` access, explicit
  conversion to `std::vector`. The read walk creates the owner on the
  first view (as Python's `shm_view_ctx` does) and hands it the block
  instead of freeing it. Users opt in through the existing type and
  record mappings (`type Cpp => ... = "mlc::bytes"`); no inference.
  Backed by shared memory (keeps the landing copy, no file hazards) or by
  the mapping (no copies, but a retained view pins the mapping: SIGBUS on
  external truncation, the in-place rewrite fallback of #146 must count
  views as readers, and pages behind the cursor re-fault). One retained
  150-byte view pins a whole sub-packet, as with numpy views today.
  Precedents: Python numpy views (pymorloc.c:16-92), `mlc::ArrowTable`.
  Adopting the landing block must first take it out of the eval arena, or
  the arena frees it under the view (SHM-7); owners register with
  `morloc_view_held` (FORK-14/15); a thread-local owner context needs a
  STATE-1 row; conversion to `std::vector` explicit, to `std::span` for
  the length of a call fine; a "bytes pinned by views" counter in
  `shm_stats` makes amplification visible. SHM-3/4/6/8/10/13.
- **B. Rust lifetime-checked borrows.** `@next` returns a batch guard;
  decoded types borrow from it (`&'a [u8]`, `&'a str`). rustc proves the
  view does not escape, so no refcount and no in-process lifetime error;
  an external truncation of a mapping-backed file still raises SIGBUS, as
  does #146's in-place rewrite once the stream is closed. Codegen must emit lifetime-correct
  signatures. Builds on Rust's bulk vector copy.
- **C. Generated accessors** (FlatBuffers style): zero walks and
  allocations, most invasive; still needs A's owner to retain.

Not acceptable: a non-owning `std::span` justified by "the parameter is
`const&` and never escapes". The compiler cannot see what sourced C++ does
with a reference; it would be an unchecked lifetime claim.

Order: S0, W1, R1, W2, W3 (model first), R2; then A for C++ records and
B for Rust. Expected for bytes [inferred]: W1 saves ~2 passes (~0.08-0.12
s of 0.77), W1+W3 ~4 (~0.16-0.25 s); whether writes approach `dd` depends
on the sync share, unmeasured. Reads ~0.26 -> ~0.15-0.18 s, floored by
start-up, one pass and the mapping faults.

## Tests

- Count passes rather than time them where possible: minor-fault counts
  and a copy counter like the existing `cpp-copy-count` golden; each fix
  measured alone with a runtime-bench `BENCH_LABEL`.
- Goldens for R1: a decode exception leaves the claim released; a file
  truncated during a read fails no worse than today (today's landing
  memcpy has the same SIGBUS exposure); several readers of one stream
  still see every sub-packet once.
- W1: files written into a reused spare buffer are byte-identical to
  files written into a fresh one (no stale gap bytes).

## Open

- Retained views and memory amplification: acceptable, or compact a view
  that outlives its batch (undetectable in C++)?
- Mapping-backed views stored past the call: allowed given SIGBUS on
  truncation, or always shared-memory backed?
- Should `view -> std::vector` be implicit (hides copies) or explicit?
- Should `@close` `fdatasync` at all? A durability choice for the user.

## Review

Devil's advocate, 2026-10-13: the copy list holds; the attribution of wall
time did not. Revisions taken: the custodian's memmove and file write
overlap the next batch at level 0, so they are off the critical path, and
the unattributed remainder points at `fdatasync` at close; W1 first, with
its two hazards (stale gap bytes, sizing); W2 as "move the index"; R1 as a
scoped decode instead of an escaping lease (the lease was mis-specified:
mappings belong to per-process slots and are unmapped on overflow, sweep
and close); W3's risks; W4 rejected; B's safety claim narrowed; mapping
faults and allocator faults added; a measurement step first.
