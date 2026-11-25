# Raw stdout copies each batch up to three times

Child of runtime/issues/encode-throughput.md. The file write path is in
stream-copies.md; this file covers stdout. Fix plan:
/work/plans/output-path-perf/PLAN.md, step 2.

## Evidence

mosm `filter` (gencode v50, 1.5 GB out, `-t 1`): copies of a batch after
the handler returns its `Str`:

1. `_mlc_write_copy` (data/lang/cpp/pool.cpp:1071) stages it with
   `to_voidstar` into SHM;
2. `append_one_element` (stream.rs:4284) into the slot write buffer;
3. `compact_sealed_buffer` (write_behind.rs:318), no memmove in practice.

Item 3 runs in the nexus, which then writes header, payload and padding
as one iovec (custody.rs:951). A list batch to an unstaged stdout is
encoded in place (SLOT-36) and skips items 1-2. A stdout batch is
compressed only for packet output (SLOT-33).

## Measurements

2026-11-09 at 7760abbdc, mosm `filter -t 1`, 1.48 GB raw out, temporary
timers: the pool's sink is the critical path of the out stage (1.25 s).
Of the sink's 1.14 s: the `toFastaEntries` render 0.26 s, `_mlc_write`
(stage and append) 0.59 s, frees and `@try` about 0.12 s. Scrub on free
about 0.19 s, split across both processes. These timings predate the
nexus writing stdout parts without a join; remeasure before acting.
