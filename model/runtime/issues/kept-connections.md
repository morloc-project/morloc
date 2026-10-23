# Kept caller connections into fork pools

NET-7 lets a callee keep a caller's connection; C++ and Rust pools do, and
a Python or R fork-pool worker closes after every reply (NET-9). Deferred
by the user 2026-10-12.

- [measured] What keeping would save (/work/tests/runtime/crossing,
  2026-10-12): C++ -> Python 36.5 us per call, connecting per call; Python
  -> C++ 24.9 us on a kept connection; R ~49 us; a bare socket round trip
  13.6 us. About 10-12 us per call into Python or R.
- [read] Why it is not done: a fork-pool worker is one single-threaded
  process, and a kept connection ties a caller thread to that one worker.
  - Deadlock: worker W holds the kept connection of C++ thread T, takes a
    call that calls back into C++, and the callback reaches a C++ thread
    whose kept connection also leads to W (T itself, re-entering, or any
    thread W served before). It sends to W, which is blocked in its own
    call to C++; the replacement worker the pool forks (NET-8) cannot take
    a request queued on W's private connection. Per-call connections avoid
    this because every request goes to the shared listen socket, where any
    idle or new worker takes it.
  - Head-of-line blocking: with no cycle, a call on a kept connection
    still waits for its worker while other workers are idle.
  - Closing the kept connection before W accepts another call races a
    send T makes in between: T cannot tell whether its request was
    delivered, so it cannot retry.
- Options:
  1. Reentrant dispatch: while a worker waits on another pool, it also
     serves requests arriving on its kept connections, running a nested
     request on the waiting stack. Removes the deadlock, not the
     head-of-line wait; changes busy counting (fork_pool.rs). Model first:
     extend tla ForkPool and CrossingConn.
  2. No affinity, no connect: one shared datagram socket per pool that
     every idle worker receives from, replies sent to the caller's own
     bound address. Packets are small (values over the inline threshold
     travel in shared memory). Loses the connection's end-of-file, so a
     caller waiting for a reply needs another way to learn the worker died
     (DAEMON-6). Model first.
  3. Keep connections only to thread pools (today).
- To decide by benchmark: the crossing loop above and
  pool-nested-callbacks-churn, for whichever option is modelled.
