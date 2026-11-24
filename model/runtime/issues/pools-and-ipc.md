# Pools, IPC and the nexus

Details and evidence: /work/plans/runtime-audit-2026-10-10/C-processes.md.

- **A revoke whose notice cannot be sent closes silently.** [read,
  2026-11-20] NET-7, NET-9. revoke (morloc-runtime/src/fork_pool.rs ~532)
  ignores the result of sending the not-run notice. A send cannot wait
  (send_all waits while the caller lives) and EPIPE means the caller is
  gone, but ENOBUFS or ENOMEM under memory pressure close the connection
  with no notice, so a request the caller already sent on it fails as the
  pool's death (the `revoke_without_notice` case of tla:KeptConn).
- [speculative] A connect to a pool that died while idle may retry for up
  to 30 s before failing.
- [measured] Python and R pools queue concurrent daemon requests behind a
  worker that is computing (by design of fork-mode busy counting); open
  question whether they should start a worker per concurrent job. Eight
  concurrent daemon requests making 20 Python calls each take 1.0 ms per
  request against 2.4 ms for one client (C++-only requests scale 5.2x):
  /work/tests/runtime/daemon. Two concurrent 2 s `time.sleep` calls to one
  Python pool take 4.0 s over the daemon, MCP over HTTP or the front end,
  where the same in C++ takes 2.0 s (NET-9; 2026-10-13).
- [measured] The daemon answers every HTTP request with `Connection: close`,
  so each request pays a TCP connect, accept and close. The soak client in
  test-suite/daemon-tests says its requests ride persistent connections and
  exercise the keep-alive path; there is none.
- **On macOS a descriptor is not close-on-exec from birth.** [read]
  Against FORK-3 and NET-7. On non-Linux hosts morloc-runtime-types fd.rs
  `pipe` (:14), `socket` (:38), `socketpair` (:51), `accept` (:75) and
  `accept_nonblocking` (:90) set `FD_CLOEXEC` in a second step. A process
  spawned by another thread in between (Rust's `Command` uses
  `posix_spawn`, which runs no fork handlers) inherits the descriptor; for
  a caller's connection the fork count `connect_to` checks does not see
  it, so the connection may be kept, and its C++ or Rust callee then waits
  on it until that process exits. Open: macOS has no atomic form; a fork
  gate held across the two steps would block forks behind a blocking
  accept.
- [speculative] On macOS a connect to a full listen backlog (128) fails
  with ECONNREFUSED instead of blocking; `connect_to` then retries for at
  most 30 s (ipc_ffi.rs), a deadline on a pool call (DAEMON-6). Matters
  for fork-mode pools, whose backlog only idle workers drain.
- **A caller's connect and send block without a liveness check.** [read]
  Against NET-5. The sockets a pool caller (ipc_ffi.rs `connect_to`,
  `send_request`) and the front end's forward (router_ffi.rs
  `connect_to_daemon`) use are blocking, so `sendmsg` to a reader that died
  while a copy of its descriptor lives on, or `connect` to a full backlog,
  blocks in the kernel and never reaches `send_parts`' once-a-second
  `Peer::alive` check. Only the reply read is liveness-checked.

- **A connection made across a fork leaves the child a socket nobody
  closes.** [read 2026-10-15, from review] NET-7. When a fork lands between
  making a caller's socket and registering it, `connect_to` drops its copy
  and makes another (302ef7f1), but the child keeps its copy of the dropped
  socket: never connected, so no callee waits on it, but a descriptor the
  child holds for its lifetime. A long-lived child forked often while
  callers connect accumulates them. The retry has no bound; it ends as soon
  as a fork does not land in the few microseconds between the two steps.

- **A one-pool C++ program takes about 55 ms on an empty input; the same
  work native takes 3 ms.** [reproduced, compiler 4443cc47] biolib
  `stream/bench` `ingestFa` on a one-entry FASTA. Under `strace -f -tt`
  (128 ms traced), the parts that are not work:
  - the nexus lists all of `/tmp` at startup (40 `getdents64` calls over
    12,222 entries here, about 20 ms traced); the cost grows with
    `/tmp`, which holds many leaked `morloc-daemon.*`, `morloc-mcp.*`
    entries;
  - connecting to the pool polls with sleeps of 1, 2, 4 and 8 ms
    (15 ms) while the pool links libstdc++ and binds;
  - at exit the main thread joins the `morloc-custody` thread, which
    after its `fdatasync` sleeps 20 ms in `clock_nanosleep`; the join
    waits out the sleep (about 17 ms);
  - `fdatasync` of the output stream file (6 ms);
  - the `/bin/sh` wrapper forks `dirname` before exec'ing the nexus.
  `-h`, which starts no pool, takes 25-48 ms.

- Host state is not shared across a fork pool's workers. [reproduced
  2026-10-26, cause read] The spec says nothing on whether state a sourced
  function keeps in its process (an R environment, a Python module
  global) persists from one call to the next. A Python or R pool forks a
  worker when a call arrives while its workers are busy (a re-entrant call:
  R calls Python, which calls R; NET-8), and a later call may reach either
  worker, so state made in one after the fork is missing in the other.
  The stdlib stream-r suite fails this way: its sorters live in an R
  environment (`.morloc_sorters`, stream-r R/stream.R), and after the
  earlier test groups make re-entrant calls, `pushSorter h2` reaches a
  worker without sorter 2 ("no open sorter 2", group "Sorting", called from
  Python's `mapE_`). The realizer now places `mapE_` beside its R
  callback (COST-4), so the suite passes, but any placement that makes R
  re-entrant reaches the same failure. Needs a ruling: per-pool affinity for stateful
  sources, a declared statefulness that keeps them on one worker, or host
  state ruled out.
- **A cross-language round trip makes 7 syscalls where 4 would do.**
  [measured with strace, 2026-11-13] Per cpp->py call: 2 poll, 3
  recvfrom, 2 sendmsg; the extra poll is the reply wait
  (ipc_ffi.rs:783-787) and the extra recvfrom is the MSG_PEEK on the held
  connection in the callee (fork_pool.rs:513; pool_ffi.rs:424 reads
  directly and treats a closed peer as Closed). Removing the peek and the
  caller's reply poll gives 5, not 4: the callee's poll multiplexes the
  listener, the held connection and the lifeline. Est. 0.5-1 us per call;
  touches NET-5/NET-9, model first.
  py->cpp I64 is 23.0 us = 1.9 us Python marshalling + 21.2 us
  foreign_call, against the 13.6 us floor (XL-1); copies and schemas are
  not where the rest goes.
- **Generated callers rebuild the socket path on every call.** [measured,
  2026-11-13] Python calls os.path.join per call (0.28 us); C++
  foreign_call_v builds a std::string per call.
- **Every R pool leaves an `Rtmp*` directory in /tmp.** [reproduced
  2026-11-14] One run of any program with an R pool adds one empty
  `/tmp/RtmpXXXXXX` (R's per-session temp dir): the pool ends without R's
  own exit cleanup removing it. A day of test runs left 8763 of them. Gap in
  model/runtime/lifecycle.md: no item says what a pool leaves behind when
  it ends. Fix direction: point R's TMPDIR at the run's temp root, which
  the run removes, or remove `tempdir()` in the pool's exit path.
- **Every run's startup scans all of /tmp.** [measured 2026-11-14]
  process.rs `cleanup_stale_shm` reads every /tmp entry to find stale
  `morloc.XXXXXX` run dirs. With about 9800 entries (mostly the leaked
  `Rtmp*` dirs above), the fixed cost of every program rose by 6.6 ms
  (C++-only 15.7 -> 22.3 ms; crossing bench `.c` terms, results
  2026-11-14-0438 vs -0802). Fix direction: keep run dirs under one
  morloc-owned parent so the scan reads only them.
- **macOS: a C++ thread's call into a live R pool fails as a dead peer.**
  [reproduced on macOS CI only, 2026-11-19; cause speculative] Golden
  fold-native, case threadsR: four C++ producer threads fold through R;
  one call fails with "the r pool ended during the call (the request could
  not be sent)", uncaught on the producer thread, and the C++ pool aborts
  (SIGABRT), so `r n=9080` is missing. Linux passes. The message comes from
  ipc_ffi.rs send_and_receive_over_socket when send_parts returns
  SendFail::Other, or PeerClosed on a fresh connection; the message now
  ends with the send's reason and OS error. Hypothesis: XNU returns
  EPROTOTYPE (not EPIPE) when a send races the peer closing, so a reused
  connection the R pool revoked (NET-7) is not retried. Next step: read
  the reason from the next macOS CI failure. Separately, an exception
  escaping a user-spawned C++ thread ends the pool by std::terminate; the
  test's producer does not catch, so this is the expected crash shape.
- **perf: a small parallel map nested inside a running one costs 6-10x
  a serial inner map.** WPOOL: with every posted job already woken, an
  inner post takes the pool lock, notifies the timer and wakes helpers at
  once. Measured (outer x inner trivial elements, us per outer call):
  12 x 64 is 65 against 6 serial-inner; 64 x 64 is 237 against 36. With
  5 us elements the pool matches or beats serial. Holding the delay for
  inner posts gains only 10-15%; the cost is the lock and notify per post,
  not profiled. A fix needs a protocol step (an inline path for a post
  made from inside a span) and a model change.
