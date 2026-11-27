# Test gaps on risky paths

Paths whose behaviour is checked only by a model, a mutation run or
timing (from the 2026-11-12 audit of the output-path work).

- **No fork-pool test reads a not-run notice with a request in flight.**
  NET-7/NET-9: every fork_pool.rs revoke test revokes an idle caller, so
  its next send fails with EPIPE and the retry passes that way;
  a_retiring_worker_revokes_its_held_connection would pass under the
  retire_silently variant too. The notice itself is read only by the
  synthetic callee test in ipc_ffi.rs and by chance in golden
  pool-kept-connections. macOS order of notice and reset is unconfirmed.
- **Python's would-block retry branches are reached by timing.** SLOT-37:
  pymorloc.c's WOULD_BLOCK branches and the re-size after a wait are hit
  by write-behind depth 1 in golden write-in-place-wait-py (confirmed by
  a mutation run), not deterministically; a re-size after an object
  changed during the wait is covered only by EncodeInPlace.tla.
- **A Rust batch failing after begin is untested.** SLOT-36: a place
  error (allocation, a closed stdout pipe) is handled by WriteTxn's Drop
  after the runtime already aborted; nothing checks it from Rust. A packer
  of a generated record's fixed-width or inline-integer field also runs
  after begin, in the write (it is not run while sizing), so a packer that
  panics mid-batch reaches the same path, also untested.
- **SHM-18's commit-failure path runs only by hand.** The populate-fails
  fallback (shm.rs commit_large -> remove_pages) is tested only by an
  #[ignore]d test; no fault injection exists for it.
- **A close-time write error failing the stream is untested.** SLOT-15:
  the check added when the close-time sync was dropped for temporary
  streams has no test (needs a write-back failure, e.g. a full or
  failing filesystem).
- **The registry scan misses a C static declared with an inline struct.**
  [reproduced 2026-11-14] source_rules.rs `scan_c` reads one line at a
  time and takes the last identifier before `=`, `;` or `[`; a declaration
  `static struct {` ... `} name = ...;` yields no name, so
  `every_process_wide_value_is_registered` passed with an unregistered
  process-wide table in pymorloc.c (now declared through a typedef and
  registered). Same for `static union {` and any declaration whose name
  falls on a later line. Gap in the registry rule of model/runtime/state.md.
- **SLOT-32's daemon claim is untested.** [read, 2026-11-22] SLOT-32 says
  a later stream, in a daemon another client's call, never receives an
  earlier stream's bytes; a_later_stream_on_a_slot_holds_no_byte_of_an_earlier_one
  checks it for streams of one process on one slot, not for two clients'
  calls through one daemon.
- **The Rust binding writes no leaf through the runtime.** [read,
  2026-11-22] SLOT-36/SLOT-37: rustmorloc still writes a `String` or a
  `Vec` of numbers by its walk over a zeroed room, not with
  `mlc_write_place_bytes`; correct, but it keeps the zero and copy passes
  the leaf path removes, and no Rust leaf test exists.
- **Large node rooms are zeroed with cached stores.** [read, 2026-11-22]
  The non-temporal zero for node rooms of 8 MiB or more (Z2 in
  /work/plans/mosm-perf/01-output-path.md) is not done; it waits for a bench
  of large node elements that shows a gain.
- **No registry class fits a C++ test hook compiled into every pool.**
  [read, 2026-11-22] STATE-1: `par_failure_probe` in
  binders/cpp/cppmorloc.cpp is a plain (non-atomic) function pointer
  that only test code (golden par-pool) sets, but it is compiled into
  every C++ pool. `test-only` requires the value to be compiled only into
  tests, and `counter` describes an atomic; the row says `counter` for
  want of a better class. Gap in the class list of model/runtime/state.md.
- **WorkerPool's nest scenario never grows past one helper.** WPOOL-1,
  WPOOL-6: WorkerPool.cfg (nest) has Spans = 2 and Cap = 2, so WS = 2 and
  MaxH = Grow = Min(2, 1) = 1, although its comment says two callers, two
  helpers. Read, and checked with a temporary invariant `count <= 1` on
  nest (47,465 states, no violation). Only fair, nestfail and cap have
  two or more active helpers, so no config checks two helpers on two
  callers' nested jobs.
- **WorkerPool defines a scenario no config runs.** `wake3` (three
  helpers, Cap 3) is defined and described in the header; no .cfg uses
  it. Read.
- **A forked child's new helper can reuse a name an old job's `hon`
  holds.** WorkerPool.tla, FORK-8: harmless while the child never waits
  on its parent's jobs; if it ever does, WaitsOnlyOnLiveThreads could
  pass falsely. Read.
- **RouterForward's StartIf hardcodes `next > 3`.** RouterForward.tla
  uses the literal rather than `Cardinality(Gens)`, so changing Gens
  would silently change what the model checks. Read.
- **ForkSide's failed gather take leaves the entry live.** SIDE-1: in
  ForkSide.tla's TakeBegin, the failed branch in gather mode (FAILED or
  putFailed) does not move the state word to TAKEN, drain the native
  list or close an open side, so owner puts after it still append and
  return ok, and a child can keep writing a side nobody reads. The
  runtime does follow SIDE-1 (fork_side.rs `end` moves every state to
  TAKEN before `finish` checks the failure), so the model checks
  behaviour the code cannot have and never checks a failed take's
  TAKEN state; TakeHoldsEveryReturnedPut applies only to a take that
  succeeded. Fold mode does seal in that branch. Read.
- **ForkSide's AFailedStepFailsTheFold also covers gather.**
  ForkSide_failed_put_unmarked.bug (gather) is caught by it; the name
  says fold only. Read.
