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
  answers "not catchable"; the status and message are wrong either way.
- **User data decode is classified infrastructure.** [reproduced for tables]
  Violates PANIC-6 / FAIL-4: decoding an argument
  (`get_morloc_data_packet_value`) is wrapped INFRA in pymorloc.c:2769,
  rmorloc.c:3318 and :3518, pool.cpp:391, so a bad CSV cell (`zz` in a
  declared Int column) kills the pool instead of failing the call. Probably
  also a wrong-length tensor (unconfirmed). Open: should the C ABI report an
  error class with every message?
- **Readiness wait ~11 min.** [read] Gap in DAEMON-6 / LIFE-2 (no total
  bound). `wait_for_daemon` (morloc-nexus/src/process.rs:1447) sleeps between
  17 pings with an uncapped doubling from 10 ms: 655 s for a pool that lives
  but never binds its socket.
- **120 s send deadline.** [read] Contradicts the "no deadline on pool
  calls" rule in DAEMON-6: `send_all` gives up after 120 s without progress
  (morloc-runtime/src/ipc_ffi.rs:78, 90-139), e.g. a SIGSTOPped receiver
  and a packet larger than the socket buffer.
- **No liveness check on a reply wait.** [read] Gap next to NET-5 (covers
  only reading a request): the caller waits for end of file only, so a dead
  callee whose socket is held open by a grandchild hangs it.
- **Malformed argument exits 1, unknown command 2.** Gap in LIFE-3: is a
  malformed value a usage error? (`./prog cmd notanint` -> 1.)
- **Top-level `Err` keyed by arm name.** Gap: morloc-nexus/src/dispatch.rs
  `die_on_top_level_err` fails the run for any variant arm named `Err`, not
  only `Try`.
