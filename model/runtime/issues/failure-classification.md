# Failure classification and waits

Runtime library errors are a bare message with no class; each binding
decides at the call site whether a failure is infrastructure
(`*_TRY_INFRA`, `PROPAGATE_INFRA_ERROR`: the worker ends) or the call's own
failure (a fail packet). Several sites are misclassified.

- **Peer death exits 70.** [reproduced] Violates PANIC intro ("a peer that
  died ... never raised as a panic"; 70 only for panics). A pool whose
  callee dies aborts with "morloc internal error" and exit 70: C++
  `cpp_local_dispatch` -> `MLC_INTERNAL_ABORT` (data/lang/cpp/pool.cpp);
  Python `PyMorlocInternalError` from `pybinding__foreign_call`. Repro:
  Python `die(x): os._exit(9)`, C++ `cdbl`, `safedie x = @try (cdbl (die
  x))`; and C++ `ccrash` calling `std::abort()`, `g x = @try (pinc (ccrash
  (pinc x)))`. CLI exits 1. Open: FAIL-6 (catchable or not). Code already
  answers "not catchable"; the status and message are wrong either way. A
  fix needs the error class asked about in the next entry: a binding sees
  a bare message and cannot tell a dead callee from an internal error.
- **User data decode is classified infrastructure.** [reproduced for tables]
  Violates PANIC-6 / FAIL-4: decoding an argument
  (`get_morloc_data_packet_value`) is wrapped INFRA in pymorloc.c:2769,
  rmorloc.c:3318 and :3518, pool.cpp:391, so a bad CSV cell (`zz` in a
  declared Int column) kills the pool instead of failing the call; CLI-20
  deviates on this. Probably
  also a wrong-length tensor (unconfirmed). Open: should the C ABI report an
  error class with every message?
- **A reply wait checks the listener, not the worker.** [read] NET-5
  applies to a reply wait through the connected socket's peer, which is
  the process that listened. In a Python or R fork pool that is the
  coordinator or listener, not the worker serving the call, so a worker
  that dies while a process it forked holds the connection open still
  hangs its caller. The kernel offers no way to name the accepting
  process; the callee would have to say who it is.
- **Malformed argument exits 1, unknown command 2.** Gap in LIFE-3: is a
  malformed value a usage error? (`./prog cmd notanint` -> 1.)
- **Top-level `Err` keyed by arm name.** Gap: morloc-nexus/src/dispatch.rs
  `die_on_top_level_err` fails the run for any variant arm named `Err`, not
  only `Try`. Open: should only `Try`'s `Err` fail the run?
- **Sourced C++ code catches morloc's infrastructure failures; the run
  then exits 0.** [reproduced 3/3, 2026-10-21] Violates PANIC-5,
  PANIC-13 and DAEMON-14. `MorlocInfraError` and `MorlocPipeClosed`
  (data/lang/cpp/pool.cpp:89, :648) derive from `std::runtime_error`.
  `safeApply f x = try { return f(x); } catch (const std::exception&)
  { return -1; }` (C++), `die x = os._exit(9)` (py), `run x = safeApply
  die x`: `run 3` reports the Python pool ended, prints `-1`, exits 0. The
  nexus also does not turn a recorded pool death into a failed exit when
  the call returned. Exiting at the site, as R does, closes it (not
  deriving from std::exception still leaves `catch (...)`). Repro:
  /work/plans/model-audit-2026-10-21/repro/I2/p5/.
- **A panic during a user panic's unwinding in a Rust pool aborts with
  SIGABRT.** [reproduced, 2026-10-21] Violates PANIC-1 (exits 70). A
  source `twice` whose local's `Drop` panics after `panic!("first")`
  prints "thread caused non-unwinding panic. aborting." and "Pool process
  crashed with signal 6"; neither message is reported. Cause:
  morloc-runtime-types/src/panic.rs:156-160 returns UnwindQuietly for a
  nested panic in SCOPE_HOST. Repro: /work/plans/model-audit-2026-10-21/repro/I2/p1/.
- **Requests queued at daemon shutdown are closed unanswered.** [read,
  2026-10-21] Violates DAEMON-6 (refusals answered 503). daemon_run pops
  every queued job and `close`s it (daemon_ffi.rs:3666-3671); workers stop
  popping once SHUTDOWN_REQUESTED is set (:3738). Only a popped request
  gets "Daemon shutting down." (:2062).
- **Rules with no answer.** [read, 2026-10-21]
  - NET-5 bounds a reply wait to an unnamed callee at 30 s; the code waits
    forever (ipc_ffi.rs:713-720), as DAEMON-6 (no call deadline) requires.
    Candidates: the wait ends only at end of file; or the callee names
    itself.
  - A 503 refusal's class: NET-3's busy refusal is `{"error":"too many
    connections"}` with no `status`/`class`; shutdown reuses `recovering`
    (meant for restarting a crashed pool). deploy-serving.asc:143-152 lists
    neither, nor 401/403.
  - PANIC-4 says a thread finding a poisoned lock never uses the data;
    fv.rs:69/113/160 (STATE-4), voidstar.rs:838, compression.rs:915,
    shm.rs:1978, stream.rs:4316/4424 take it by design. State the exception
    or route them through `poisoned_lock()`.
  - Which user catches may hold morloc failures: a Rust `catch_unwind`, a
    Python bare `except:`, a C++ `catch (...)` all can. Say the guarantee
    covers morloc's catches only, or end the pool at the site.
  - [speculative] The panic hook ignores `can_unwind`: a panic in an
    `extern "C"` callback inside a runtime catch scope (e.g. Arrow
    `get_next` inside `decode`, arrow_ffi.rs:143) aborts unless wrapped in
    `outside_scope`; no source rule checks the wrapping.
