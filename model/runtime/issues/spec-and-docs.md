# Runtime spec text and manual drift

Runtime items whose text, cited tests or manual pages no longer match the
code. Found by the 2026-10-21 audit [read unless marked].

- **FMT-5 contradicts SHM-6, SHM-12 and SHM-13.** FMT-5 lists the volume
  header as `... reserved u64, lock, cursor` and the block header as
  `magic (merged: 0x0CB1DEAD), refcount, size u64`. The code
  (shm_types.rs ShmHeader/BlockHeader) has a `poison` word where
  "reserved" is, `next_index` and `grow_lock` after `cursor`, a zeroed
  absorbed header (no DEAD magic), and a 16-bit generation in the top bits
  of the size word. arch-build-shm.asc:28-38, :50, :52 repeat the stale
  layout; :58 says first-fit (it is next-fit) and :20-21 "twice the last"
  (it is twice the largest this process maps, SHM-12).
- **FMT-2 omits the reference generation.** Data packets carry the SHM-13
  generation at command bytes 18-19 (little-endian, 0 = unchecked;
  packet.rs:447). FMT-2 and arch-build-packets.asc:45-55 list bytes 13-17
  only. arch-build-packets.asc:21 calls `mode` reserved (bit 0 is
  MODE_KEEP, FMT-1, NET-7) and :87-88 says each call opens and closes its
  own connection, against NET-7 and arch-build-workers.asc:161-164.
- **SHM-6 does not say who zeroes a new block under poison.** Under
  MORLOC_SHM_POISON=1 a released block holds 0xDB; shcalloc relies on a new
  block reading zero (shm.rs:1203), which holds because shmalloc_unlocked
  zeroes when poisoning (shm.rs:1837). The rule states only the release
  side. Its test zeroes the freed block itself before checking that a new
  block reads zero, so that half proves nothing.
- **Cited tests that do not test their rule.** SHM-1 cites
  `threads_allocating_together_never_share_a_block`, which never reads a
  count; `a_donated_reference_survives_the_sender_release`
  (packet_ffi.rs:2310) asserts a count of 1 after shmalloc. STATE-3 cites
  golden placement-representability, whose `rustCaptured` calls the
  capturing closure once on one thread. FORK-7 cites
  `a_fork_from_a_thread_holding_a_reset_lock_aborts`, a FORK-8 test
  (FORK-7's other two tests cover it).
- **FORK-9 does not cover once primitives in the binders.** cppmorloc.cpp
  `frame_init` uses `pthread_once(&frames_key_once_, ...)`; a child forked
  by user code while another thread is inside `frames_key_make` waits
  forever at its first frame. The window is one `pthread_key_create`.
  FORK-11 covers only std stream/env locks.
- **Undocumented runtime variables.** environmental-variables.asc omits
  MORLOC_SHM_POISON (SHM-6), MORLOC_PY_POOL, MORLOC_CHANNEL_DEPTH,
  MORLOC_WRITE_BEHIND_DEPTH, MORLOC_ARROW_NO_BORROW, MORLOC_MALLOC_TUNING,
  MORLOC_R_GC_BUDGET, MORLOC_FORBID_SELF_CALL, MORLOC_REQUIRE_NAMESPACES,
  MORLOC_MCP_TOKEN, MORLOC_EVAL_ALLOW_NO_AUTH. Some may be internal.
- **arch-build-artifacts.asc:35-37** says a failed build leaves the
  previous build in place; false for ART-3's kill window and for a refused
  rebuild (build-and-text.md). Build.hs:328's "Atomically replace" comment
  is false for the same reason.
- **More cited tests that miss their rule.** [read, 2026-10-21] NET-3:
  `the_header_names_every_kind_as_the_runtime_does` and
  `a_reply_is_refused_before_it_is_built_only_when_its_payload_is_past_the_frame`
  test other rules. PANIC-10: the cited test asserts only that REPLY is
  restored, not that the built packet is freed. SLOT-16: no test of a pool
  finishing stdio streams before replying. SLOT-24: cited tests check
  frame cutting, not that a block outlives a running job. DAEMON-14: the
  tests miss the swallowed-infra case (failure-classification.md).
  `a_request_trickling_in_ends_at_its_deadline` (NET-2) is defined in both
  net_limits.rs:92 and mcp.rs, so deleting one leaves the citation green.
- **README's model list is incomplete.** runtime/README.md lists 10 TLA
  models; items also cite RunSweep, DaemonShutdown, CrossingConn,
  ForkPool, PanicExit, SlotSeqlock, LocalSweep, QueueHandoff,
  SlotRelease, ChannelWait, QueueWake, AheadJob.
- **Streaming and serving pages.** arch-build-streaming.asc:96-97 gives the
  queue bound as 7 (the maximum; the default is `frame_workers().clamp(2,
  4)`, lowered by SLOT-32); :109-112 and :42 give only the CLI ending of an
  unclosed stream. arch-build-workers.asc:11-13 says the MCP server runs a
  fixed worker set; it serves up to 128 connections and limits only
  `tools/call` (DAEMON-16). environmental-variables.asc also omits
  XDG_RUNTIME_DIR (NET-4) and MORLOC_STATE, and does not state
  MORLOC_WRITE_BUFFER_BYTES's 4096 floor or MORLOC_REGISTRY_SLOT_COUNT's
  65536 cap and 0-as-default (SLOT-17).
