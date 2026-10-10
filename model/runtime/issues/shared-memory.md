# Shared memory and the stream registry

Changes here touch locks and ownership: model first.

- [speculative race] shm.rs:970-983 `retire_names` clears ownership even
  when `COMMON_BASENAME.try_lock()` fails: leaks every volume.
- [speculative race] shm.rs:1442-1453 `rel2abs` fast path holds nothing on
  the mapping while `shclose_locked` (:1095-1102) unmaps. DAEMON-1 should
  make it unreachable in the daemon; check other `reset_all` callers.
- Spec text behind the code: SHM-2 omits the process-local `ALLOC_MUTEX`;
  SLOT-8 says the generation changes on close (it changes at open and
  release, stream.rs:2604, 1793); topology.md says only the nexus unlinks
  (code: the fork generation that made the primary volume, shm.rs:659-670);
  topology.md's temp-file route does not apply to tables, which always use
  shared memory.
- **C++ trackers on helper threads never flush.** [read] Per-thread
  trackers in the C++ pool (data/lang/cpp/pool.cpp, `thread_local`
  ShmTracker) are flushed at dispatch end or thread exit; a persistent
  thread that is not a dispatch worker (OpenMP, TBB, a user pool) never
  flushes, so a long-lived daemon leaks the blocks it allocates there.
  Spec gap against SHM-8 (who releases a helper thread's references).
- **Every free can stall behind another process.** [read] `shfree`,
  incref and `abs2rel` walk the volumes under process-local `VOLUMES` /
  `ALLOC_MUTEX`, which an allocation holds while waiting on another
  process's volume lock or creating a volume (FORK-10's allocator half), so
  one slow peer stalls every free in the process, and a fork waits too.
  Details: /work/plans/runtime-audit-2026-10-10/B-shm-binders.md.
- [read] Each process grows its own volumes from its own size hint, so N
  pools under pressure can create N times the memory needed.
- [read] When FORK-15 lease staging fails, a forked child's view
  references stay counted for the rest of the run (logged, not leaked
  beyond the run).
- [speculative] Python thread mode: a NumPy view's `__del__` running on
  another thread can release through that thread's tracker, dropping
  another holder's reference early when two threads hold the same block.
