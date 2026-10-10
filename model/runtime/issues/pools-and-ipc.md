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
- [measured] Each crossing costs 76-86 us here, ~23 us of it bookkeeping:
  the callee reads `/proc/<caller>/stat` for the caller's start time on
  every accepted connection (~11 us), and caller and callee make ~8
  `clock_gettime` calls between them (real syscalls on this HPET host,
  ~1.5 us each): /work/tests/runtime/crossing.
- Spec text behind the code: topology.md lists a stream sweeper thread in
  pools and a lifeline thread in R; neither exists.
- [read] FORK-3 gap on non-Linux: accept and pipe set close-on-exec in a
  second step (runtime-types fd.rs ~80-83, rmorloc.c mlc_accept_cloexec and
  mlc_pipe_cloexec), so a fork-and-exec in another thread can inherit the
  descriptor. Live on macOS today. Open: macOS has no atomic form; a fork
  gate held across the two steps would block forks behind a blocking accept.
- [speculative] On macOS a connect to a full listen backlog (128) fails
  with ECONNREFUSED instead of blocking; `connect_to` then retries for at
  most 30 s (ipc_ffi.rs), a deadline on a pool call (DAEMON-6). Matters
  for fork-mode pools, whose backlog only idle workers drain.
