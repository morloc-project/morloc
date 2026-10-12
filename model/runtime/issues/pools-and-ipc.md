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
- [read] FORK-3 gap on non-Linux: accept and pipe set close-on-exec in a
  second step (runtime-types fd.rs ~80-83, rmorloc.c mlc_accept_nonblocking and
  mlc_pipe_cloexec), so a fork-and-exec in another thread can inherit the
  descriptor. Live on macOS today. Open: macOS has no atomic form; a fork
  gate held across the two steps would block forks behind a blocking accept.
- [speculative] On macOS a connect to a full listen backlog (128) fails
  with ECONNREFUSED instead of blocking; `connect_to` then retries for at
  most 30 s (ipc_ffi.rs), a deadline on a pool call (DAEMON-6). Matters
  for fork-mode pools, whose backlog only idle workers drain.
- **The front end's forward to a daemon times out at 60 s.** [read]
  Violates DAEMON-6 and IFACE-12. data/rust/morloc-runtime/src/
  router_ffi.rs:772-789 sets `SO_RCVTIMEO`/`SO_SNDTIMEO` of 60 s on the
  forward socket; a call longer than that fails "Failed to read response
  length" while the child keeps computing.
