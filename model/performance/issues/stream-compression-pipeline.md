# Compressed streams: `@flush` runs compression serially, memory is bounded per stream, reads decode on one thread

Status: analysed 2026-10-13, reviewed by the devil's advocate, not fixed.
Parent: runtime/issues/encode-throughput.md section 3. Source:
lib/biolib/runtime-bench README sections 3, 4 and 7.

## Evidence

600 MiB of `[Vector U8]` as 60 `@write`s of one 10 MiB vector (from
history.tsv, 2026-10-13, a2e21e97):

| pool | L0 + `@flush` | L3 | L3 + `@flush` | (L3+flush) - (L0+flush) |
|---|---|---|---|---|
| C++ | 2.11 s | 3.49 s | 11.72 s | 9.61 s |
| Rust | 5.90 s | 5.82 s | 15.49 s | 9.59 s |
| Python | 4.21 s | 4.51 s | 14.01 s | 9.80 s |

Peak RSS, largest single process (measure.py reports max VmHWM, which
includes shared-memory pages the process touched; not a sum): L3 without
flush C++ 350 MiB (baseline 110), Python 441 (baseline 200), Rust 188
(baseline 108). L1/L3 reads cost ~0.55 s more than L0 in C++.

The `bytes` shape is random ACGT (`rb_fill(..., quals=false)`, seed = the
batch size), so all 60 batches are identical; the README's "bases and
qualities" holds only for `pairs`. Harmless for ratio (frames are
independent, the zstd-6 window is 2 MiB).

Paths: `R` = data/rust/morloc-runtime/src, `T` =
data/rust/morloc-runtime-types/src.

## Corrections to the README

- Morloc level 3 is zstd 6 (`T/compression.rs:564`, ZIP-8). The flushed
  delta is 9.6 s / 60 = 160 ms per 10 MiB, ~65 MB/s, the same in all three
  languages, so the cost is in the nexus, not the pool. DESIGN.md
  (/work/plans/stream-compression, 158-172) measured zstd 6 on one thread
  at 76 MB/s on read data, equal to `zstd -T1`. The README's "240 MB/s, one
  thread" is a multi-threaded rate.
- `@close` calls `fdatasync` (`R/custody.rs:829`); `@flush` does not. The
  `dd` baseline has no `conv=fdatasync`. 600 MiB of dirty pages is under
  the background-writeback threshold, so close writes almost all of it:
  plausibly 0.2-0.6 s of every L0 `write` row [inferred], perhaps most of
  section 6's gap to `dd`. Unknown whether `work/` is on a disk or tmpfs.

## How a compressed file stream writes [read]

- Since 24609226 a written stream is served by a custodian thread in the
  nexus (SLOT-10..22): the pool's `@write` (`shared_write_subpacket`,
  `R/stream.rs:3891`) fills the stream's shared-memory buffer (16 MiB
  blocks); the nexus compresses and writes. A buffer becomes a sub-packet
  when the next element does not fit (`R/stream.rs:3794-3830`), at
  `@flush`, or at close; here each sub-packet is one 10 MiB batch.
- The custodian (`R/custody.rs:595-651`) hands buffers to the
  process-wide compression pool (`R/write_behind.rs`) and keeps at most
  `depth` jobs outstanding: `MORLOC_WRITE_BEHIND_DEPTH`, default 7,
  clamped to 1..7 (`custody.rs:228-230`), 1 at level 0. Commits are in
  order: wait for the oldest, `pwritev` payload, `pwrite` header,
  `pwrite` temporary footer (`custody.rs:756-790, 874`).
- A sub-packet of at most 16 MiB is one zstd frame, compressed by one
  thread (`FrameCompressor::compress`, `T/compression.rs:168-185`).
- `@flush` (`R/stream.rs:4014-4035`) queues the buffer and a flush entry
  and waits for `commit_all`: every outstanding compression done and
  written to the page cache. After it returns, the flushed elements
  survive the run dying (SLOT-13) and a reader of the open file sees them
  (SLOT-15). Nothing is created per flush.

## Causes

1. **A flushed L3 write runs at the speed of one zstd-6 thread.** [read,
   measured] With a flush after every batch, one batch is outstanding at a
   time: copy, compress (~160 ms), write, return. That is `zstd -6 -T1`
   speed: a missing optimization, not a pathology.
2. **Without flush, the cores bind, not the depth.** [read + inferred]
   The producer makes a batch every ~29 ms; keeping up needs ~5.5
   compression threads and depth allows 7, but the achieved rate is ~56
   ms per batch, ~2.9 effective threads, matching DESIGN.md's "flat from 3
   threads up" on this power-limited 6-core laptop. Depth 7 binds only on
   hosts with more than ~7 usable cores. (The 3-thread figure was
   measured on the earlier pool-side write-behind; re-measure on the
   custodian.)
3. **Memory growth: not located.** Python's +241 MiB equals C++'s +240 on
   a larger baseline, so in Python at least the largest process is
   probably the pool, holding the 16 MiB shared-memory blocks it zeroed
   (`shcalloc`) and filled, about depth + 1 of them. A nexus-side
   candidate also fits C++: each job's output `Vec` zero-filled to
   `compress_bound` and kept at capacity (`T/compression.rs:132`), zstd
   contexts, allocator arenas. Needs RssShmem/RssAnon per process.
4. **Coalescing is intended.** [read] Consecutive `@write`s fill one
   buffer (SLOT-20; STRM-2: "Compression level, `@flush` and buffer size
   change sub-packet boundaries, never the elements"). 1M pairs in 613
   writes of ~1.6 MiB read back as 62 sub-packets.
5. **Reads decode each sub-packet on the calling thread.** [read]
   `@next` (`R/stream.rs:4087`) decompresses synchronously
   (`parallel_decompress_frames`, `T/compression.rs:712`); one frame per
   sub-packet means one thread; no decode-ahead. ~1.1 GB/s accounts for
   the 0.55 s [inferred; read-path copies, stream-copies.md, may be part].

## Defects found on the way

- **The memory bound is per stream (gap in SLOT-12).** N open compressed
  streams may hold N x 7 x (a 16 MiB block + up to 16 MiB of output). A
  writer splitting reads into 96 barcode files at L3 is compression-bound,
  so every stream fills to depth: ~10 GiB of shared memory and ~10 GiB of
  output on a 14 GiB host: an OOM or `shmalloc` failure instead of
  backpressure. [inferred from read]
- **Oversize blocks.** Any block whose payload exceeds 16 MiB (one
  element over 16 MiB, or a staged stdout `@write` over 16 MiB) goes
  through `commit_framed` (`custody.rs:707-723`): it drains the stream,
  compresses on the custodian thread with a fresh `MultiFrameEncoder` that
  spawns and joins `frame_workers()` threads per block, allows `n_workers
  x 4 x 16 MiB` in flight (`T/compression.rs:270-330, 437-490`; ~1 GiB at
  16 workers) and holds the whole compressed output in one `Vec` before
  writing. In the pool an oversize element is copied four times
  (`R/stream.rs:3754-3791`: source voidstar, `scratch`,
  `oversize_payload`, the queued block): ~8 GiB for a 2 GiB chromosome.
- **zstd contexts are never freed.** Each compression thread keeps one
  `FrameCompressor` per level for the life of the process (a daemon
  nexus never frees them). At levels 7-9 (zstd 19-22, long mode) a
  context for a 16 MiB source may be large [speculative; check
  `ZSTD_estimateCCtxSize`].
- Output `Vec` zero-filled to `compress_bound` per job and kept at
  capacity (`T/compression.rs:132, 162-185`; `custody.rs:519, 620`).
- Under-spawn: two submits before a woken worker retakes the lock both
  see `idle == 1` (`write_behind.rs:134, 156-158`); the second job waits
  one job. [read, minor]
- `MORLOC_WRITE_BEHIND_DEPTH` above 7 is clamped silently
  (`custody.rs:229`).
- write_behind.rs is a spec-references-only file, but its module doc
  asserts thread ownership ("Service threads touch nothing but the job
  they run"), and the `DEFAULT_DEPTH` doc ("at most one 16 MiB frame") is
  contradicted by oversize blocks.
- The docs say `@flush` forces elements "to disk"; it reaches the page
  cache.

## Solutions

In order: measure, then the cheap and safe, then what needs a ruling.

- **S0. Measure first.** Per-process VmHWM, RssShmem and RssAnon in
  measure.py; `dd conv=fdatasync`, `zstd -6 -T1` and `zstd -6 -T6` rows.
  Decides whether S1 matters and which process grows.
- **S1. Depth from the machine.** Depth changes only timing and memory,
  never output bytes (sub-packet cuts are fixed by the buffer size). Set
  the default to min(7, physical cores / 2), or 3-4; SLOT-12's text holds.
  Re-measure on the custodian.
- **S2. A process-wide block budget.** Cap (the ring has 8 slots, so the
  count can only be capped, not replaced) outstanding work across all
  streams of a process, counting shared-memory block sizes, and admitting
  one batch when the stream has none outstanding (a budget below one
  buffer would deadlock). Closes the N-streams OOM. A shared budget makes
  one stream's producer wait on another stream's writer, so SLOT-12's
  "no wait cycle passes through a slot" must be re-derived (a stdio sink
  blocked on an external reader is a way in): model first, in
  tla/StreamQueue.
- **S3. Exact-size compression output.** Compress into spare capacity
  (zstd's `WriteBuf` over `Vec`), reserving `compress_bound` but not
  zero-filling it; shrink or reuse. ~1-2 ms per 160 ms job; the resident
  saving depends on the allocator. Nexus only.
- **S4. Frames of a fixed size per level inside a sub-packet (needs a new
  ruling).** Each frame its own job; reads then decode in parallel
  (`parallel_decompress_frames` and the frame index already handle many
  frames). Measured ratio cost (DESIGN.md): zstd 6, 4 MiB frames -0.16%,
  1 MiB -0.7%; zstd 9, 4 MiB -0.46%, 1 MiB -1.25%. The critical path is
  the largest frame: 4 MiB frames on a 10 MiB batch (4+4+2) make a
  flushed batch at most 2.5x faster (~11.7 -> ~6 s); 2 MiB approaches the
  ~3x the hardware allows (~5.3 s). The frame size must be a fixed
  function of the level, never of core count or queue state, so output
  stays byte-identical across hosts: ~2 MiB at morloc levels 1-6, 16 MiB
  at 7-9 (morloc 4-6 = zstd 9-15, window 4 MiB; 7-9 long mode, window
  2^27, already clipped to the 16 MiB source). Reverses the 2026-09-29
  ruling (16 MiB frames, no splitting); what that ruling did not weigh is
  the flushed path and per-sub-packet decode. Changes
  model/performance/compression.md (F, ZIP-6, ZIP-4's note). With several
  readers (SLOT-23), per-`@next` scoped decode threads oversubscribe the
  cores (minor).
- **S5. Decode-ahead without claiming.** Decode the next sub-packets
  speculatively into the per-handle cache (SLOT-23, bounded by
  `MORLOC_IFILE_CACHE_BYTES`), keyed by offset, and serve from it when
  the reader claims them. It must not claim ahead: claiming moves the
  shared cursor, and sub-packets claimed by a reader that stops are lost
  to the others. Helps when the consumer works or sub-packets are small.
  Model first.

Rejected:

- zstd's own workers on one frame (`ZSTD_c_nbWorkers`): DESIGN.md option
  B, already rejected. Choosing it "when the stream is serial" is decided
  by a race (flush entry pushed before or after the custodian pops the
  buffer), and nbWorkers 0 and >= 1 write different bytes, so file bytes
  would depend on timing; always-on means 7 contexts each with a zstd
  thread pool. Reads stay single-threaded. S4 dominates it.
- `@flush` as a boundary only: removes the guarantee `@flush` exists for.
  After it returns, flushed data survives a crash (SLOT-13) and a reader
  of the open file sees it (SLOT-15). Errors would surface in another
  `@try`. "Pipeline the flush with the next batch", "wait only for its
  own enqueue" and "compress consecutive flushed batches in parallel" are
  this under other names. If users want boundaries without the
  checkpoint (lib/biolib's motive is unclear), a separate opt-in form
  fits the 2026-10-06 ruling on opt-in fast modes; never a change to
  `@flush`.

Minimal change, biggest win: S0, then S1 (memory, no ruling), then S2
(model first), then put S4 to the user with the measured ratio cost.

## Review

Devil's advocate, 2026-10-13: cause 1 confirmed and better supported (the
same 9.6 s delta in every language; DESIGN.md's 76 MB/s). Revisions taken:
cause 2 is the cores, not the depth; cause 3 not located (Python's growth
equals C++'s on a larger baseline); history.tsv baselines replace the
README's; measured ratio costs for S4 and its largest-frame critical path;
a byte budget can only cap, must count blocks and admit one batch, and is
needed process-wide; nbWorkers was already rejected and would make bytes
timing-dependent; a non-waiting `@flush` loses the crash checkpoint; S5
must not claim ahead. Added defects: the per-stream bound times N streams,
oversize copies and held output, contexts kept forever.
