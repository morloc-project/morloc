# Pools, IPC and the nexus

Details and evidence: /work/plans/runtime-audit-2026-10-10/C-processes.md.

- [speculative] A connect to a pool that died while idle may retry for up
  to 30 s before failing.
- [measured] Python and R pools queue concurrent daemon requests behind a
  worker that is computing (by design of fork-mode busy counting); open
  question whether they should start a worker per concurrent job. Eight
  concurrent daemon requests making 20 Python calls each take 1.0 ms per
  request against 2.4 ms for one client (C++-only requests scale 5.2x):
  /work/tests/runtime/daemon.
- [measured] The daemon answers every HTTP request with `Connection: close`,
  so each request pays a TCP connect, accept and close. The soak client in
  test-suite/daemon-tests says its requests ride persistent connections and
  exercise the keep-alive path; there is none.
- [measured] A crossing costs 33 us (Python -> C++, reused connection)
  and 51 us (C++ -> Python, connection per call) here, against a 13.6 us
  socket round trip: /work/tests/runtime/crossing. Calls into Python and R
  still connect per call, and Python's own per-call work is ~19 us.
- [read] FORK-3 gap on non-Linux: accept and pipe set close-on-exec in a
  second step (runtime-types fd.rs ~80-83, rmorloc.c mlc_accept_cloexec and
  mlc_pipe_cloexec), so a fork-and-exec in another thread can inherit the
  descriptor. Live on macOS today. Open: macOS has no atomic form; a fork
  gate held across the two steps would block forks behind a blocking accept.
- [speculative] On macOS a connect to a full listen backlog (128) fails
  with ECONNREFUSED instead of blocking; `connect_to` then retries for at
  most 30 s (ipc_ffi.rs), a deadline on a pool call (DAEMON-6). Matters
  for fork-mode pools, whose backlog only idle workers drain.
- Open question (NET-8, NET-9): kept connections into Python fork pools.
  A C++ -> Python call costs 51 us against 26 us for the bare
  connect-per-call loop and 13.6 us for a persistent one
  (/work/tests/runtime/crossing). C++ and Rust callees keep a caller's
  connection; a Python worker closes after every reply. Keeping it fails
  without a dispatcher: a worker W holding caller T1's connection accepts
  another call, which calls back into C++; if the callback runs on T1, T1
  sends on its kept connection to W, which is blocked on C++, and the
  forked replacement cannot take that request. Closing the kept
  connection before accepting races a send T1 makes in between, after
  which T1 cannot tell whether its request was delivered, so it cannot
  retry. Options: a worker serves requests on its kept connection while
  it waits on another pool (reentrant dispatch), or callers keep
  connections only to pools whose workers are threads. Needs a ruling.

