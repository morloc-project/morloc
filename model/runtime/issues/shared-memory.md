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

- **An inline argument loads as a graph of blocks.** [read 2026-10-20] Gap
  in SHM-7/SHM-8 (a value is one block, freed by one pointer;
  voidstar.rs `consolidate` doc). cli.rs `parse_cli_data_argument_classified`
  preallocates the root for inline JSON and json.rs returns it without
  consolidating, so its parts are owned only by the caller's eval arena.
  On the daemon's remote path the pool increfs only the root of an RPTR
  argument; safe only while no receiver keeps the argument past the
  request. Outside an arena the parts, and on a failed load the root, are
  never freed; callers outside an arena not yet enumerated.
  A Python pool reading such an argument makes numpy arrays that view the
  inner data block in place (pymorloc.c `PyArray_SimpleNewFromData` ~1955)
  while the view's owner references only the block being read, the root
  (pymorloc.c `shm_view_owner`); a pool that keeps the array past the
  request (module global, lru_cache) reads freed or reused memory once the
  daemon's arena ends. [read, not reproduced: a Python pool caching its
  numpy argument across two daemon calls with inline arguments]

- **A pure command cannot `@load` a table.** [read 2026-10-20] A
  table's value is a whole Arrow block, not a slot of its schema's width,
  so the eval refuses to place it in a slot ("a table cannot be copied
  into a slot", eval_ffi.rs `take_value`, voidstar.rs `deep_copy`). A
  `@load` of an `.arrow` or `.parquet` file typed as a table in a command
  with no pool fails. No golden covers it. Gap: what a table value is
  inside the eval.

- **An eval scope keeps every intermediate until it ends.** [read
  2026-10-20] Gap: no item bounds a scope's memory. Bindings alias by
  pointer and results bit-copy relptrs, so the arena (SHM-14) is the only
  releaser, and the eval keeps each block a read or load returns
  (eval_ffi.rs `take_value`). A pure command or daemon call that loops over
  `@next` holds every batch until it returns: memory grows with the
  stream, not the batch.
