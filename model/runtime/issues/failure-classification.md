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
- **A closed pipe can still be held by a catch-all in user code.**
  [read, 2026-10-21] PANIC-13: no catch holds a closed pipe. C++
  `MorlocPipeClosed` is no longer a `std::exception`, but a sourced
  `catch (...)` still holds it, and a Rust `catch_unwind` holds the
  `MorlocPipeClosed` payload. Closing it needs a sticky thread-local flag
  set at the throw sites, checked after each sourced call returns and at
  each dispatch's end (reset only at the outermost dispatch), not an exit,
  since the pool survives a closed pipe.
- **Other C++ runtime failures still throw `std::runtime_error`.** [read,
  2026-10-21] A relptr that does not resolve with a reason
  (cppmorloc.hpp:306, :319), `parse_schema_cpp`, `shmalloc_cpp` and
  `frame_init`'s `bad_alloc` throw catchable exceptions; whether running
  out of shared memory is recoverable needs a ruling (PANIC-5, PANIC-13).
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
- **A run exits 0 after a pool died during its call.** [reproduced,
  2026-10-21] Violates DAEMON-14 (a one-shot run exits as failed). When a
  catch in user code holds the failure a dead pool caused -- a Python
  `except BaseException:` or bare `except:` around a callback, a Rust
  `catch_unwind` -- the call returns a value, the nexus logs the pool's
  death and exits 0. (C++ no longer: its infrastructure failures end the
  C++ pool at the site.) Failing the run once the nexus reaps a pool during
  the call would close it for every language at once.
- **Untested C++ failure sites.** [read, 2026-10-21] No test covers a
  failed `shfree_cpp`, or an unresolved result, release or Arrow pointer,
  ending the C++ pool: reaching them needs corrupted shared memory, and
  the runtime has no fault-injection hook.
- **A C++ `@read` failure does not name its input.** [reproduced,
  2026-10-23] Violates MORLOC-CONVENTIONS G4 (an error names the value).
  `@read "abc" :: Try Str I64` in a C++ pool gives `@read: serialization
  error: JSON parse error: expected value at line 1 column 1`; before
  79788394 it was `@read: parse failed on 'abc'`. pool.cpp:644 passes the
  runtime's reason through without the string. Seen in
  projects/mosm `make test` (`cut abc`, `cut 1:x`), red since then.
