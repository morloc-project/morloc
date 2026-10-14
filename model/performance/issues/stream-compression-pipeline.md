# Compressed streams: memory bounded per stream, oversize elements copied, no decode-ahead

Status: open. Parent: runtime/issues/encode-throughput.md section 3.
Source: lib/biolib/runtime-bench.

Fixed 2026-10-14: a compressed sub-packet is cut into frames whose size
is fixed by the level (2 MiB at levels 1-6, 16 MiB at 7-9; ZIP-9), each
its own write-behind job, so one flushed batch compresses on several
threads and a sub-packet decompresses on several. The write-behind depth
defaults to 4 (2 with fewer compression threads), compression threads free their
zstd contexts when idle, and an oversize block goes through the same jobs
(SLOT-24). Measured on 600
MiB of `[Vector U8]` (history.tsv labels `stage1a-2` -> `stage3a-frames`):

| | before | after |
|---|---|---|
| L3 write with `@flush` per batch, C++ / Rust / Python | 11.32 / 10.99 / 13.58 s | 4.32 / 4.12 / 6.32 s |
| L3 write, no flush, C++ (peak RSS) | 3.43 s (351 MiB) | 2.40 s (211 MiB) |
| L1 / L3 read, C++ | 0.74 / 0.80 s | 0.36 / 0.36 s |
| L3 file size | 177.3 MiB | 177.4 MiB |

Paths: `R` = data/rust/morloc-runtime/src, `T` =
data/rust/morloc-runtime-types/src.

## Open

- **The memory bound is per stream (gap in SLOT-12).** N open compressed
  streams may hold N x depth x (a 16 MiB block + its compressed output). A
  writer splitting reads into 96 barcode files at L3 is compression-bound,
  so every stream fills to depth: several GiB of shared memory on a 14 GiB
  host, an OOM or `shmalloc` failure instead of backpressure. [inferred
  from read]
- **An oversize element is copied four times in the pool.** An element
  larger than the 16 MiB write buffer goes through `R/stream.rs`'s
  oversize path: source voidstar, `scratch`, `oversize_payload`, the queued
  block, ~8 GiB for a 2 GiB chromosome. See stream-copies.md W1, which
  removes the `scratch` copy for every write.
- **Reads decode on the calling thread, one sub-packet at a time.** Frames
  of one sub-packet now decode in parallel; the next sub-packet is not
  decoded until `@next` asks for it.
- **Unmeasured: which process grows.** lib/biolib/runtime-bench's
  measure.py reports the largest process's VmHWM, which counts shared
  pages it touched; per-process RssShmem and RssAnon would locate the
  remaining L3 growth (211 MiB against 110 at L0).

## Solutions

- **S2. A process-wide block budget.** Cap outstanding work across all
  streams of a process (the ring has 8 slots, so the count can only be
  capped, not replaced), counting shared-memory block sizes, and admit
  one batch when the stream has none outstanding (a budget below one
  buffer would deadlock). Closes the N-streams OOM. A shared budget makes
  one stream's producer wait on another stream's writer, so SLOT-12's
  "no wait cycle passes through a slot" must be re-derived (a stdio sink
  blocked on an external reader is a way in): model first, in
  tla/StreamQueue.
- **S5. Decode-ahead without claiming.** Decode the next sub-packets
  speculatively into the per-handle cache (SLOT-23, bounded by
  `MORLOC_IFILE_CACHE_BYTES`), keyed by offset, and serve from it when
  the reader claims them. It must not claim ahead: claiming moves the
  shared cursor, and sub-packets claimed by a reader that stops are lost
  to the others. Model first.

Rejected:

- zstd's own workers on one frame (`ZSTD_c_nbWorkers`): nbWorkers 0 and
  >= 1 write different bytes, so choosing it when the stream is serial
  (a race) would make file bytes depend on timing; reads would stay
  single-threaded. Fixed frames per level dominate it.
- `@flush` as a boundary only: after it returns, flushed data survives a
  crash (SLOT-13) and a reader of the open file sees it (SLOT-15).
  Pipelining the flush with the next batch removes that guarantee. A
  separate opt-in form would fit the 2026-10-06 ruling on opt-in fast
  modes; never a change to `@flush`.
- Exact-size compression output (no zero-fill of `compress_bound`): with
  2 MiB frames the fill is ~0.1 ms against ~30 ms of compression.
