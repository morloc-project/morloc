# Python pool

- **A manifold's signal handlers never interrupt it in thread mode.** [read]
  In Python thread mode (the macOS default) manifolds run on worker
  threads, but CPython runs signal handlers only on the main thread, which
  sits in the pool's poll loop (data/lang/py/pool.py:1010-1026). A
  manifold's `signal.signal` call raises ValueError off the main thread,
  and a `signal.alarm` timeout fires in the poll loop, is logged and
  swallowed, and never interrupts the manifold. Spec gap: nothing states
  what user signal handlers may do in a pool. Fork mode does not have the
  problem; removing thread mode (/work/plans/shm-leases-pool-runtime/STEP0.md)
  would fix it.
- [read] Fork mode's busy counters are `multiprocessing` RawValues backed
  by a pymp-* temp directory that only an orderly exit removes
  (pool.py:1072-1075, 1116, 1129); a SIGKILL or daemon recovery leaks one.
  Matters on macOS if fork mode becomes the default there.
- **Idle fork workers can block where they never retire or stop.**
  [read, medium confidence] All idle workers poll one blocking socketpair
  for jobs (pool.py ~1121, ~702); a job wakes them all and the losers block
  in `_recv_fd` (~640, ~707), where they neither retire, see shutdown, nor
  reclaim leases until another job arrives. Fix: make that socket
  non-blocking and treat EAGAIN as "lost the race".
- [read] Thread mode starts and stops a thread per call when calls are more
  than ~100 ms apart (idle retirement after ~100 ms, not the 5 s
  topology.md states).
- [read] FORK-3: fds received in `_recv_fd` (pool.py ~642) lack
  close-on-exec (no `MSG_CMSG_CLOEXEC`).
- [read] An EMFILE in `_recv_fd` raises RuntimeError, which escapes the
  `except (EOFError, OSError)` at pool.py ~712; the worker exits without
  its retire token and SHM-8 ends the whole pool, dropping the call.
- [read] Fork-mode workers never get the raised descriptor limit: only the
  listener process calls `raise_nofile_limit` (ipc_ffi.rs ~382).
- [read] FORK-12 gap: a user module that forks while it is imported (after
  the worker forks, before its loop) leaves a child that polls the job
  socket and serves calls, uncounted by the coordinator.
