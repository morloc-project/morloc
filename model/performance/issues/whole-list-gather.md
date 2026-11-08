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
