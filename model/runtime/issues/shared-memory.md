# Shared memory and the stream registry

Changes here touch locks and ownership: model first.

- **New blocks are not zero.** [read] Violates SHM-6 ("a new one [reads] as
  zero"). Default-mode `shmalloc` does not zero (only poison mode,
  morloc-runtime/src/shm.rs:1946), and `scan_volume` (shm.rs:2215-2255)
  leaves each merged neighbor's 16-byte header (0x0CB1DEAD, size) inside the
  survivor. The SHM-6 test checks only poison mode (shm.rs:2342). Voidstar
  padding is therefore not guaranteed zero, which breaks byte hashing (see
  cache.md). Open: zero on allocation, zero merged headers, or drop the
  guarantee and make hashing skip padding.
- **Registry slot count not read back.** [read] stream.rs:345 writes it,
  :316 each process uses its own `MORLOC_REGISTRY_SLOT_COUNT`; a mismatch
  rejects handles or fails attach after 5 s.
- [speculative] stream.rs:1840-1872 `clear_slot_fields` keeps `diag`; an
  IFile over a plain data packet shows the previous stream's diagnostics.
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
