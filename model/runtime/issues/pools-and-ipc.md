# Pools, IPC and the nexus

Details and evidence: /work/plans/runtime-audit-2026-10-10/C-processes.md.

- **The per-pid stream sweep races daemon recovery.** [read] A dead pool's
  per-pid sweep is queued (nexus process.rs ~190) just before recovery
  stops every stream writer with a FAILED status (~279), so a crashed
  pool's streams end PAUSED or FAILED depending on timing (SLOT-13 says
  FAILED). Recovery and shutdown already finish every stream.
- **Pool readiness is seen late.** [read] The nexus pings a starting pool
  with a doubling sleep from 10 ms (process.rs ~1511-1518): a pool ready at
  160 ms is seen at ~310 ms, one ready at 700 ms at ~1270 ms; an R ping
  whose read times out abandons its connection. Every CLI run starting
  Python or R pays it.
- [read] R accepts connections and makes pipes without setting
  close-on-exec atomically (rmorloc.c), so a fork in another thread could
  inherit them (FORK-2).
- [read] The C++ pool builds peer socket paths in a 128-byte buffer and
  truncates longer ones silently (pool.cpp ~1101).
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
- [speculative] `read_packet` takes the packet size from the first `recv`
  even when it returned fewer than the 32 header bytes (ipc_ffi.rs
  ~648-682); the rest of the header is calloc zeros, so the size is wrong.
