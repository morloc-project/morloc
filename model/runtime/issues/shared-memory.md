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

- **Freeing in an eval arena scans the arena.** [measured 2026-10-15]
  Gap: no item bounds the cost of a free. `eval_arena::forget_if_active`
  (morloc-runtime/src/eval_arena.rs) finds the freed block with a linear
  search of every block recorded in the scope, so a scope that records n
  blocks and frees them one at a time costs O(n^2): a JSON load of a
  100,000-deep list, whose consolidation frees its pieces, took 13.7 s at
  80,000 levels against 0.88 s at 20,000 inside an arena (16x for 4x the
  depth), and is linear outside one. Reproduced on a user path 2026-10-15:
  a pure command `idOpt :: [?Real] -> [?Real]` (scopes are open around the
  nexus's pure commands and the daemon's calls, whose JSON arguments load
  inside them) took 0.15, 0.32, 1.03 and 5.86 s for 40k, 80k, 160k and 320k
  elements, half null, against 0.09-0.14 s for `[Real]`: each present
  optional, nested array or variant payload is a separate block that
  `voidstar::consolidate` frees one by one, oldest first. A free of a block
  not in the arena scans it all and misses, and `rollback_to` partitions the
  whole arena and then frees the drained blocks against the rest, O(m*k) per
  `@catch` fallback. An index from pointer to position would make a free
  O(1).

- **The block census counts only volumes this process has mapped.**
  [reproduced 2026-10-15] Gap in SHM-12 (a process uses volumes others
  made, mapping them when it needs them). `shm::live_block_stats` walks the
  volumes in this process's table, so blocks in a volume another process
  made, or one this process has not mapped since its arena was reopened,
  are not counted until an allocation maps it. The census test saw 32,770
  blocks, then 6,105,936 after two allocations mapped the rest. The leak
  behind that size is fixed (d9b97d00); the census can still undercount,
  which matters to anything reading it as the process's holdings.

- **An allocation that misses its cursor walks the volume.** [read; measured
  2026-10-15] Gap: no item bounds allocation cost. `shm.rs`
  `find_free_block_in_volume` / `scan_volume` is next-fit over block headers
  in shared memory: one cursor per volume, no free list; a free never moves
  the cursor, adjacent free blocks merge only during a scan, and a split
  whose remainder cannot hold a header resets the cursor to the volume's
  start. A miss reads every header from there (a cache line, often a page,
  each), and `find_in_published` repeats that over every published volume
  when the hinted one misses. With 160,000 small live blocks and holes, 4 KiB
  allocations averaged about 7 us against 33-190 ns otherwise. Free lists by
  size with coalescing on free would bound it; the allocator is shared across
  processes, so model first (tla/AllocWait).

- **A rollback drops an entry recorded before its checkpoint.** [read
  2026-10-15] Violates SHM-7 (a checkpoint names the entries recorded after
  it; freeing one drops one entry). `eval_arena::rollback_to` takes the
  entries recorded after the checkpoint out of the arena and frees each with
  `shm::shfree`, which also calls `forget_if_active`. When the same block
  has an entry from before the checkpoint (a stream cache hit before a
  `@catch` and again inside it, stream.rs `cache_get_or_materialize`; or an
  adopted reply sharing a block with an argument, daemon_ffi.rs
  `adopt_rptr_result`), that earlier entry is removed too: one reference is
  released for two entries dropped, and the reference the earlier entry
  stood for is never released (a leak until the process ends). The drained
  entries should be released with `shm::release`, which leaves the arena
  alone. tla:ArenaCheckpoint does not model the forget inside the release.
