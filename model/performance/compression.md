# Compression (ZIP)

Stream files written with `@write level` and read back.

## How compression runs

- Levels 0-9 map to zstd 0, 1, 3, 6, 9, 12, 15, 19, 21, 22, with long-range
  mode (window 2^27) from 7 (`CompressionLevel::zstd_level`, `use_long`;
  morloc-runtime-types compression.rs).
- A payload is cut into independent zstd frames of at most F raw bytes.
  A stream sub-packet uses F = 2 MiB at levels 1-6 and 16 MiB at 7-9
  (`subpacket_frame_size`, ZIP-9); a saved value or packet uses 16 MiB
  (`FRAME_CHUNK_SIZE`). A stream's write buffer flushes at 16 MiB
  (`WRITE_BUFFER_BYTES_DEFAULT`), and each `@flush` ends a sub-packet, so
  a frame never spans a flush.
- Each frame of a stream sub-packet is its own write-behind job, so one
  sub-packet compresses on several threads; frames decompress on frame
  workers,
  both capped by `frame_workers()`: `MORLOC_FRAME_WORKERS` (1-16) if set,
  else `available_parallelism()` capped at 16. On Linux the latter follows
  the CPU affinity mask, so `taskset` also limits it. At most
  `MORLOC_WRITE_BEHIND_DEPTH` batches are outstanding per stream (default
  `frame_workers()` clamped to 2..4; above 7, the most a stream holds, it
  is reduced to 7 with a warning). At level 3, 600 MiB in 10 MiB batches
  without `@flush` (C++, 12 threads): depth 2 3.2 s at 114 MiB peak, 4 2.5
  s at 191 MiB, 7 2.5 s at 199-351 MiB.

Consequences the models below must carry: no frame shares context with
another, so ratio depends on frame and batch size; parallelism is at most
the number of frames in flight; and long-range matching cannot reach past
a 16 MiB frame.

## Corpora

Generated in C++ (`benchmarks/compression/corpus.hpp`) from a hash of each
element's index, so a corpus is the same bytes however it is batched. All
fixed-width; `Int` is never used where performance matters (ENC-2).

| name | type | content |
|---|---|---|
| u64-uniform | U64 | uniform 64-bit: incompressible, measures pure overhead |
| u64-small | U64 | below 2^20: counts and IDs, high bytes zero |
| u64-sorted | U64 | increasing, deltas 0-15: coordinates |
| u64-zero | U64 | all zero: the ratio ceiling |
| real-walk | Real | random walk plus noise: a measurement series |
| str-dna | Str | 150-base reads over ACGT |
| str-fastq | Str | FASTQ lines: header, bases, `+`, qualities |

Ratios hold for these corpora only; a corpus is added when a real workload
does not resemble any of them.

## Experiments

m is raw bytes (the size of the level-0 file), F = 16 MiB (the write
buffer, and the frame size at levels 7-9; levels 1-6 cut 2 MiB frames), w the worker
count, k a number of frames or batches. Times subtract the same
experiment's level-0 time, which carries generation, encoding and the file
write, so a level's parameter is the cost of compressing alone.

- E1, levels: every corpus at every level, m = 2F, w = 1. Gives the
  single-thread rate and ratio per level, and the `zstd` CLI on the
  level-0 file at the same zstd level (`-T1`, one frame, with and without
  `--long=27`) as the floor.
- E2, workers: str-fastq and u64-small at levels 1, 3, 6, m = 8F,
  `MORLOC_FRAME_WORKERS` w in 1, 2, 3, 4, 6, 8, 12, write and read. Then
  `taskset -c 0-(k-1)` with no override, against w = k.
- E3, frame edges: str-fastq level 3 at m/F in 1/16, 1/4, 1/2, 1, 1+1/16,
  2, 2+1/16, 4, 4+1/16, 8, at w = 1 and at the default w.
- E4, batches: str-fastq level 3, m = 4F, `@write` + `@flush` per batch of
  64 KiB, 256 KiB, 1, 4 and 16 MiB, against one `@write`.
- Reads are timed beside each write (E1, E2).

The reference host has 6 cores and 12 threads; scaling past 6 is
hyperthreading and is reported, not modelled.

### ZIP-1 Compressing costs a rate per raw byte per thread, by level and corpus
Intent: proposed
Bench: compression
Model: t_z(m) - t_0(m) = b_z * m at w = 1 (E1); `<corpus>.z<l>` = b_z in ms/MB
Measured: pending
Limit: str-fastq.z3 <= 30 ms/MB, u64-small.z3 <= 30 ms/MB, str-fastq.z1 <= 10 ms/MB

### ZIP-2 Decompressing costs a rate per raw byte per thread, nearly independent of level
Intent: proposed
Bench: compression
Model: r_z(m) - r_0(m) = d_z * m at w = 1 (E1)
Measured: pending
Limit: str-fastq.z3 <= 10 ms/MB, str-fastq.z9 <= 15 ms/MB

### ZIP-3 Compressed size is a ratio of raw size, by level and corpus
Intent: proposed
Bench: compression
Model: size_z = rho_z * m (E1, m = 2F)
Measured: pending
Limit: str-fastq.z3 <= 0.45 x, u64-small.z3 <= 0.6 x, u64-zero.z1 <= 0.01 x

### ZIP-4 morloc compresses at zstd's own rate and ratio
Intent: proposed
Bench: compression
Model: `.time` = b_z / b_zstd and `.ratio` = rho_z / rho_zstd, against `zstd -T1` on the same raw bytes in one frame; `.long` = zstd's ratio with `--long=27` over without
Measured: pending
Limit: str-fastq.z3.time <= 1.3 x, str-fastq.z3.ratio <= 1.03 x, u64-small.z3.time <= 1.3 x

`.ratio` above 1 is the cost of cutting frames (2 MiB at levels 1-6).
Measured before ZIP-9 with zstd 6 on read data, as compressed size over
one frame's: 1.0016 at 4 MiB frames, 1.007 at 1 MiB; 2 MiB lies between.
On lib/biolib/runtime-bench's 600 MiB of bases the level-3 file grew from
177.3 to 177.4 MiB. `.long` near 1 for levels
7-9 would mean long-range mode buys nothing inside a 16 MiB frame. Open,
once measured: whether levels 7-9 keep long-range mode.

### ZIP-5 Time falls as 1/w up to the frames in flight and the physical cores
Intent: proposed
Bench: compression
Model: t(w) = a + B / min(w, k, P); k = ceil(m / F) frames, P physical cores; fitted on w <= 6 (E2); a is generation and everything serial
Measured: pending
Limit: str-fastq.z3.write.speedup6 >= 3 x, str-fastq.z3.read.speedup6 >= 2 x, str-fastq.taskset4 <= 1.15 x

`taskset<k>` is the time under `taskset` to k cores over the time with
`MORLOC_FRAME_WORKERS=k`; near 1 means the default worker count follows
the affinity mask.

### ZIP-6 Each frame costs a fixed number of bytes; wall time follows the largest frame
Intent: proposed
Bench: compression
Model: size(m) = r*m + h*ceil(m/F) (E3, w = 1); wall t(m) = c + b*m at w = 1 and t(m) = c + b*min(m, F) when every frame has a worker
Measured: pending
Limit: fastq.z3.h <= 64 KiB

A size just past a multiple of F adds a small last frame: a few bytes of
ratio, and at full width almost no time, since it runs beside the others.

### ZIP-7 Each flushed batch costs bytes and time
Intent: proposed
Bench: compression
Model: size(k) = s + h*k and t(k) = c + d*k for k flushed batches over the same m (E4); `batch<n>k` = file over the one-`@write` file
Measured: pending
Limit: fastq.z3.batch1024k <= 1.1 x

runtime/issues/encode-throughput.md section 3 reports `@flush` per batch
making a level-3 ingest 2.4x slower; d is that cost per batch.

### ZIP-8 Level 3 is the default; level 9 is for archives
Intent: ruled 2026-10-13
Bench: compression
Model: none; a choice of level, not a cost
Measured: not applicable

Level 3 (zstd 6) compresses better than gzip's default and is the level to
reach for. Levels 7-9 (zstd 19-22 with long-range mode) are for data
written once and kept cold; their ZIP-1 rate is orders of magnitude below
level 3's.

### ZIP-9 A stream sub-packet is cut into frames of a size fixed by its level
Intent: ruled 2026-10-14
Bench: compression
Model: F = 2 MiB at levels 1-6, 16 MiB at 7-9; frames per sub-packet = ceil(payload / F)
Measured: 2026-10-14, lib/biolib/runtime-bench, 600 MiB as 60 x 10 MiB at level 3 with `@flush` per batch, C++: 11.32 -> 4.32 s; level-3 read 0.80 -> 0.36 s

The frame size depends on the level alone, never on cores or queue state,
so a stream's bytes are the same on every host. Small frames let a
flushed batch compress, and a sub-packet decompress, on several threads;
levels 7-9 keep 16 MiB frames, whose long-range window is their point.

Why: reverses the 2026-09-29 ruling of one 16 MiB frame per sub-packet,
which left a flushed write at the speed of one zstd thread.
