# A whole-list render gathers its batches through a temp file

Child of runtime/issues/encode-throughput.md. Fix plan:
/work/plans/output-path-perf/PLAN.md, step 4.

A non-stream `@render` of a `@collect` (mosm `stat`, `cstat -s`) is
rewritten at desugar time (Desugar.hs `wrapWholeCollect`). The rewrite
writes every batch to a temp-file OStream, closes it, `@load`s the whole
list into SHM, then decodes it. This costs 0.6-0.7 s after the pass at low
load, and 1.3-1.8 s at high load (mosm BENCHMARK-FINDINGS.md, section 5).
The rewrite runs before placement, so it cannot know whether the producer
and the handler share a pool. How the time divides between the temp-file
round trip and the decode is pending (plan step 0.5).

- **A gather whose action writes to a path costs 130 ms more when empty.**
  [measured 2026-11-13, bench gather, load 1.0] `gatherCpp
  --no-stdout --sum=/dev/null` with no batches takes 214 ms against 97 ms
  with `--sum` to stdout (control 83 ms); its slope is 1.83 against 1.72
  s/GiB. The to-path route does not reach the pool's @load (no IFile open
  or mlc_load in the C++ pool); cause not traced.
