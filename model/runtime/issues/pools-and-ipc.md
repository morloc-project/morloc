# Pools, IPC and the nexus

Details and evidence: /work/plans/runtime-audit-2026-10-10/C-processes.md.

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
- [read] FORK-3 gap on non-Linux: accept and pipe set close-on-exec in a
  second step (runtime-types fd.rs ~80-83, rmorloc.c mlc_accept_nonblocking and
  mlc_pipe_cloexec), so a fork-and-exec in another thread can inherit the
  descriptor. Live on macOS today. Open: macOS has no atomic form; a fork
  gate held across the two steps would block forks behind a blocking accept.
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

- **On macOS a socket is not close-on-exec from birth.** [read 2026-10-15]
  Against FORK-3 and NET-7. `morloc_runtime_types::fd::socket` sets
  `FD_CLOEXEC` after `socket()` on non-Linux hosts. A process spawned by
  another thread in between (Rust's `Command` uses `posix_spawn`, which
  runs no fork handlers) inherits a caller's connection; the fork count
  `connect_to` checks does not see it, so the connection may be kept, and
  its C++ or Rust callee then waits on it until that process exits.

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
