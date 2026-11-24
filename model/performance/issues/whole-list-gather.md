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

Measured 2026-11-14 (bench gather, results ...-0438 baseline vs ...-0802,
load 0.6-2): gather extra over the producer alone, C++ 1.85 -> 1.60 s/GiB,
Python 9.97 -> 8.18 s/GiB, after commands started only the pools they reach,
fork-pool heap headroom and the Python in-place load (Python whole load
7.17 -> 5.51 s/GiB). The split is now about: C++ write 0.9, load 0.76 (half
of it faulting in fresh vectors); Python write 2.3-3.9, load 5.5 (object
creation 2.85, cyclic GC 1.5). The write series is I/O-bound and noisy
(medians equal across runs, CPU unchanged). What remains is the encode,
file and decode a native in-memory gather would remove.

- **Any fork switches a live native gather to its side file, silently.**
  [read, stage 6] SIDE-9: every fork that runs the atfork handlers while a
  gather entry is NATIVE switches it, including a fork unrelated to the
  gather (another call in a daemon, a library that forks a helper). From
  then on every put of that gather is encoded to the side file and decoded
  at take, the file route's cost, with nothing in the output saying so.
  Open question: report the switch under `--debug`, or switch only entries
  whose sink the forking thread can reach.
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
- **The gather's temp file is removed before the reply, on the critical
  path.** [measured 2026-11-13: 126 ms for a 640 MiB file; read
  2026-11-14] The desugared `@close path` and the end-of-dispatch sweep
  (intrinsics.rs end_dispatch, run by pool_dispatch_packet before the
  reply is sent) both remove it synchronously. Deferring the removal past
  the reply helps a daemon only: a one-shot run removes its run directory
  at exit, so the cost stays in its wall time. Unlinking the open file
  early does not help (the space is freed at the last close).

- **Possible enhancement, not a defect: a fold put looks up its entry in
  the fork-side registry under its lock.** [read 2026-11-14] An
  always-native owner fold route could skip the lookup; measured cost ~50
  ns per batch, against the cell path's two deep copies.
