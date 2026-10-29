# Failure classification and waits

Runtime library errors are a bare message with no class; each binding
decides at the call site whether a failure is infrastructure
(`*_TRY_INFRA`, `PROPAGATE_INFRA_ERROR`: the worker ends) or the call's own
failure (a fail packet). An argument decoded from a file or an inline
packet is an input check (PANIC-13); one another pool built and passed by
reference is not (PANIC-14).

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
- **Other C++ runtime failures still throw `std::runtime_error`.** [read,
  2026-10-21] A relptr that does not resolve with a reason
  (cppmorloc.hpp:306, :319), `parse_schema_cpp`, `shmalloc_cpp` and
  `frame_init`'s `bad_alloc` throw catchable exceptions. A pool that runs
  out of shared memory now ends (SHM-17); a `bad_alloc` from the C++ heap
  is still catchable.
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
    Python bare `except:`, a C++ `catch (...)` all can. A dead peer and a
    closed pipe now fail the dispatch whatever its user code caught
    (LIFE-5); other failures still need the guarantee stated.
  - [speculative] The panic hook ignores `can_unwind`: a panic in an
    `extern "C"` callback inside a runtime catch scope (e.g. Arrow
    `get_next` inside `decode`, arrow_ffi.rs:143) aborts unless wrapped in
    `outside_scope`; no source rule checks the wrapping.
- **Untested C++ failure sites.** [read, 2026-10-21] No test covers a
  failed `shfree_cpp`, or an unresolved result, release or Arrow pointer,
  ending the C++ pool: reaching them needs corrupted shared memory, and
  the runtime has no fault-injection hook.
- **A dead-peer reply from another thread's dispatch is generic.** [read,
  2026-10-29] LIFE-5 counts peer deaths per process but keeps the message
  per thread, so a dispatch whose call died on a worker thread (a parallel
  map, a stream producer), or a concurrent dispatch in the same pool,
  replies "a pool this call depended on ended during the call" without
  naming the pool or the cause. Keeping the last message process-wide
  needs a fork-safe slot (FORK-14).
- **SHM-17 is tested only at the exit decision.** [read, 2026-10-29] The
  unit tests call the exhaustion handler directly; nothing runs a pool out
  of space to check that the allocator reaches it and that the run and
  daemon then end every pool. A full tmpfs spills to file-backed volumes,
  so a real test needs a full disk or a fault-injection hook.
- **A reference into a skipped volume reads as lost shared memory.**
  [read, 2026-10-29] The allocator counts a volume slot it had to skip
  (SHM-12: a name left by a creator that died), so a corrupt reference into
  that slot is tagged lost shared memory (exit 71) where PANIC-14 calls it
  a defect (70). Every pool ends either way; only the report is wrong.
- **PANIC-14 in the C++, Python and R pools has no end-to-end test.**
  [read, 2026-10-29] Their by-reference flag is checked only by reading the
  code: a test needs a by-reference value with a corrupt inner reference,
  and the runtime has no fault-injection hook.
- **PANIC-14 does not yet cover every value a pool did not build.**
  [read, 2026-10-29] A failed structural check stays a catchable error, not
  the end of the pool, in: a table passed by reference (the Arrow
  validation in the Rust and Python pools); the variant-tag check that
  generated C++ and Rust read code makes (C++ throws, Rust panics as a user
  call failure); and stream frames and `@read` or `@load` results in the
  C++, Python and R pools, which only the Rust pool reads as runtime-built.
- **`@unpack` refusing by-reference bytes has no test.** [read,
  2026-10-29] `@unpack` is generated only for `@parse` commands, so a test
  needs a parser that emits a crafted by-reference header. The R check
  reads the header's source byte by position (byte 14, value 2) rather
  than from morloc.h.
