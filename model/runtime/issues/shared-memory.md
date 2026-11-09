# Shared memory and the stream registry

Changes here touch locks and ownership: model first.

- Spec text behind the code: topology.md says only the nexus unlinks
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

- **Argument blocks outside an eval arena are never freed.** [read
  2026-10-20] Gap in SHM-15 (who releases an argument's blocks after it is
  sent). The nexus remote path (morloc-nexus dispatch.rs
  `run_remote_command`) opens no eval arena, so an argument the nexus
  reads stays allocated until the run ends; a record unrolled from flags
  or an `@many` list, which SHM-15 copies into one block before sending,
  is held twice for that time. A failed `@many` element leaves the
  elements read before it (cli.rs `parse_cli_data_argument_list`), and
  `cli_token_to_json` never frees its scratch root or the value it read.

- **SHM-15 is checked only for arguments the nexus and daemon build.**
  [read 2026-10-20] A pool sends its results and its calls to other pools
  through `make_data_packet_auto` without the check; the C++, Python, R
  and Rust writers lay a value out in one block by construction, which no
  test covers.

- **A pure command cannot `@load` a table.** [read 2026-10-20] A
  table's value is a whole Arrow block, not a slot of its schema's width,
  so the eval refuses to place it in a slot ("a table cannot be copied
  into a slot", eval_ffi.rs `take_value`, voidstar.rs `deep_copy`). A
  `@load` of an `.arrow` or `.parquet` file typed as a table in a command
  with no pool fails. No golden covers it. Gap: what a table value is
  inside the eval.

- Not a bug, recorded so it is not re-reported: an eval scope keeps every
  intermediate until it ends (SHM-14; eval_ffi.rs `take_value`), but the
  nexus never evaluates a loop. A recursive function compiles to a pool
  command (checked 2026-10-20: a pure recursive `@next` drain built as
  `"type":"remote"`), so a scope's memory is bounded by the program text
  times the size of each value, not by the length of a stream.
- **A view registered while another thread forks keeps its references
  for the run.** [read, 2026-10-21] Violates FORK-15. fork_policy.rs
  `prepare_fork_body` decides whether to stage a lease from
  `arrow_shm::any_views()`, a Relaxed read of VIEW_COUNT (arrow_shm.rs:
  1379), before it takes BORROWABLE. A view registered (borrow_register,
  :1383) in that gap leaves `staged == None` with `rels` non-empty: the
  child's references are taken, "could not record a forked child's view
  references" is printed, and they are never released. Reachable where
  user code forks a multithreaded process holding Arrow/NumPy views. Fix
  direction: stage speculatively and discard. tla/Lease.tla does not model
  the pre-check. Status stays `implemented` until fixed, since the Rust
  model check forbids a deviation that names tests.

- **Some header lookups in shm.rs bypass `header_at`.** [read,
  2026-11-09] `shincref`, `shfree_unlocked` and `incref_held` (shm.rs)
  find a block header themselves; `shfree_unlocked` has no alignment,
  volume or wrap check.
- **A header a split is writing can pass the SHM-13 check.** [read,
  2026-11-09] `split_block` (data/rust/morloc-runtime/src/shm.rs) stores a
  new free header's magic before its count, size and generation, all
  relaxed, while readers check the header with no lock. Under
  `MORLOC_SHM_POISON=1` the count reads as the poison word (nonzero, so
  held) and the generation can equal the parent's (a merge keeps the
  larger), so a stale packet racing the split is accepted. Fix with the
  checked-accessor step: publish a header's magic last with release
  ordering and read it with acquire (model first).
- **A claim that finds its block in use still advances its generation.**
  [read, 2026-11-09] SHM-13. `claim` (shm.rs) bumps the generation before
  the 0 -> 1 compare-exchange and does not undo it when the exchange
  fails (the corrupt-list case it reports), so the live holder's stamped
  packets are refused from then on and its references leak. Fix by undoing
  the bump on failure, not by bumping after the exchange: the SHM-13 check
  reads the count before the generation and relies on a claim it sees
  having already advanced the generation.
- **An arena entry recorded without a generation is never released.**
  [read, 2026-11-09] SHM-14. `record` (eval_arena.rs) stores generation 0
  when the block's header cannot be read; claimed generations skip 0, so
  release reports the entry as stale and leaks the arena's reference.
- **Persisting a packet may misread a reference packet.** [read,
  speculative, 2026-11-09] `rewrite_data_packet_for_persistence`
  (handle_scan.rs ~352, from the nexus stage.rs ~111) does not test the
  source byte; on a reference packet it would walk the relptr as an
  inline root value.
- **A value holding the block magic at an interior offset forges a header.**
  [read, 2026-11-09] SHM-13. A header is recognised by its magic word
  (`header_at`, shm.rs); data that happens to hold that word just before
  an interior address reads as a block there, so the check (and the
  builder's refusal of interior addresses) can accept an address no
  block begins at. Distinguishing a real header needs more than the magic
  (e.g. a check that the header lies on the block chain).
- **A late release can drop a later tracker entry for the same block.**
  [read, 2026-11-09] SHM-4. Binding trackers (C++ `_shm_tracker_release_one`,
  Python and R `shm_tracker_release_one`, rustmorloc `release_tracked`)
  release by address. A packet released after its dispatch's flush already
  gave its reference back, whose block another holder kept alive (same
  generation), passes the SHM-13 check and removes the entry the current
  dispatch holds for that block, so that reference is dropped early. A
  generation tells allocations apart, not holders; a late release needs to
  be told apart from the current entry (an entry per packet, or a dispatch
  epoch on each entry).
