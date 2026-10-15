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
