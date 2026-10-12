# Shared memory and the stream registry

Changes here touch locks and ownership: model first.

- Spec text behind the code: SLOT-8 says the generation changes on close (it changes at open and
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
- [speculative] The robust error-checking mutex in `RecoverableLock`
  (which the volume lock is built on) compares the owner's thread id with the caller's; two
  processes in different pid namespaces sharing a volume could match and
  get EDEADLK instead of waiting.
- [read] When FORK-15 lease staging fails, a forked child's view
  references stay counted for the rest of the run (logged, not leaked
  beyond the run). A fix would have the child release them as it exits,
  which changes the fork protocol (FORK-15): model first.
- Not a bug, recorded so it is not re-reported: arrow_shm.rs
  `release_arena` reads a freed arena if a consumer calls a root's release
  twice; each root's release is cleared after its first call, and the
  Arrow C data interface forbids calling it again.
- Open (eval arena vs call sweep): a stream opened through a stream field
  (intrinsics.rs `record_slot_if_active`) is also recorded in the eval
  arena, which closes it at a failed `@catch` body's rollback and at the
  arena's end; other opens rely on the call sweep alone. Should every open
  be recorded, or none?
