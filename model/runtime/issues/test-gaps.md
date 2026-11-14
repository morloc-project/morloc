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
  after the runtime already aborted; nothing checks it from Rust.
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
