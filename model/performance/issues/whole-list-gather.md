# A whole-list render gathers its batches through a temp file

Child of runtime/issues/encode-throughput.md. Fix plan:
/work/plans/output-path-plan.md, steps 1 and 4.

A non-stream `@render` of a `@collect` (mosm `stat`, `cstat -s`) is
rewritten at desugar time (Desugar.hs `wrapWholeCollect`). The rewrite
writes every batch to a temp-file OStream, closes it, `@load`s the whole
list into SHM, then decodes it. This costs 0.6-0.7 s after the pass at low
load, and 1.3-1.8 s at high load (mosm BENCHMARK-FINDINGS.md, section 5).
The rewrite runs before placement, so it cannot know whether the producer
and the handler share a pool. How the time divides between the temp-file
round trip and the decode is pending (plan step 0.5).

- **An action written to a path runs two more nexus processes.**
  [measured 2026-11-13, bench gather, load 1.0; remeasured 2026-11-14]
  An action given a path runs through orchestrate.rs `run`, which spawns a
  stage nexus (the command streams to a stage file through @stdout) and a
  replay nexus (reads it and calls the action's replay entry). Empty
  `gatherCpp --no-stdout --sum=/dev/null` took 214 ms against 97 ms to
  stdout while every command started every pool; since commands start
  only the pools they reach, 58-73 ms against 35-40 ms (load 5). Running
  the action's entry in-process with stdout on the file is not equivalent
  for a command with pools: prints from the parent's pools would land in
  the file (multi-output-value `noisy` expects them on stdout), and the
  replay child's environment differs (MORLOC_QUIET, no summary). Only a
  pure command could take that route. No spec item governs the route
  (CLI-6, CLI-7 state only what reaches each destination).
- **Possible enhancement, not a defect: a Python gather pays the cyclic
  GC.** [measured 2026-11-13, microbench] Building the gathered ints
  costs 2.85 s/GiB with GC off and 4.35 with it on: full collections walk
  the growing heap. Pausing GC around a native gather would save about
  1.5 s/GiB, but GC state is process-global (other threads, user cycles,
  a user's own gc.disable). Not planned (user, 2026-11-14).
