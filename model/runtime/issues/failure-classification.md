# Failure classification and waits

Runtime library errors are a bare message with no class; each binding
decides at the call site whether a failure is infrastructure
(`*_TRY_INFRA`, `PROPAGATE_INFRA_ERROR`: the worker ends) or the call's own
failure (a fail packet). An argument decoded from a file or an inline
packet is an input check (PANIC-13); one another pool built and passed by
reference is not (PANIC-14).

- **A resource failure while materializing an input argument is catchable.**
  [read, 2026-10-29] The bindings classify an argument decode by the
  packet's source, so a failure of the runtime itself while reading a file
  or inline packet (shared memory exhausted, an allocation) fails only the
  call, where PANIC-5 would end the pool; whether running out of shared
  memory is recoverable is the open question below. Telling the two apart
  needs the failure's class from libmorloc with its message, an ABI change
  to make with the peer-death status.

- **Peer death exits 70.** [reproduced] Violates FAIL-6 and the PANIC
  intro (70 only for panics). A pool whose callee dies aborts with "morloc
  internal error" and exit 70 (C++, R and Rust `foreign_call` treat a failed
  send or receive as infrastructure; Python raises `MorlocInternalError`).
  Repro: Python `die(x): os._exit(9)`, C++ `cdbl`, `safedie x = @try (cdbl
  (die x))`. The CLI exits 1. An exception class cannot make the failure
  uncatchable, since every language's catch-all holds any class (see the
  closed-pipe entry below). Proposed, awaiting an OK because it adds a
  packet status and so an ABI version: `send_and_receive_over_socket`
  classifies "connection lost before a whole reply" (connect failed, a send
  on a fresh connection failed, a read I/O error) as peer death and bumps a
  process-wide counter; `send_reply_to_foreign_server` replaces the reply
  of a dispatch that saw the counter move with a new
  `PACKET_STATUS_PEER_DIED`, which callers raise again as they do
  PIPE_CLOSED and the nexus maps to exit 1. The binding raises only to
  unwind and no longer aborts with 70. This also closes the exit-0 entry
  below for every pool kind, including fork workers the nexus never reaps.
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
