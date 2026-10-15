# Compressed streams: which process grows

Status: open. Parent: runtime/issues/encode-throughput.md section 3.
Source: lib/biolib/runtime-bench.

Fixed 2026-10-15 (SLOT-32): the streams one process writes share 1 GiB
of queued batches; each stream's depth is lowered to its share, never
below one, and set again as streams start and end, and a stream keeps no
more spare buffers than its depth. 24 streams of 1 MiB batches at level 3
peaked at 24.0 MiB of shared memory held, 36.0 MiB without the bound. A
budget shared through a wait word was rejected: it left spares uncounted
and made producers wait on other streams' writers.

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

- **Unmeasured: which process grows.** lib/biolib/runtime-bench's
  measure.py reports the largest process's VmHWM, which counts shared
  pages it touched; per-process RssShmem and RssAnon would locate the
  remaining L3 growth (211 MiB against 110 at L0).
