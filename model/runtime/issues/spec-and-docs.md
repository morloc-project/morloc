# Runtime spec text and manual drift

Runtime items whose text, cited tests or manual pages no longer match the
code. Found by the 2026-10-21 audit [read unless marked].

- **SHM-6 does not say who zeroes a new block under poison.** Under
  MORLOC_SHM_POISON=1 a released block holds 0xDB; shcalloc relies on a new
  block reading zero (shm.rs:1288), which holds because shmalloc_unlocked
  zeroes when poisoning (shm.rs:2002-2003). The rule states only the release
  side. Its test zeroes the freed block itself before checking that a new
  block reads zero, so that half proves nothing.
- **Cited tests that do not test their rule.** SHM-1 cites
  `threads_allocating_together_never_share_a_block`, which never reads a
  count; `a_donated_reference_survives_the_sender_release`
  (packet_ffi.rs:2396) asserts a count of 1 after shmalloc. STATE-3 cites
  golden placement-representability, whose `rustCaptured` calls the
  capturing closure once on one thread. FORK-7 cites
  `a_fork_from_a_thread_holding_a_reset_lock_aborts`, a FORK-8 test
  (FORK-7's other two tests cover it).
- **FORK-9 does not cover once primitives in the binders.** cppmorloc.cpp
  `frame_init` uses `pthread_once(&frames_key_once_, ...)`; a child forked
  by user code while another thread is inside `frames_key_make` waits
  forever at its first frame (`arm_jobs_key_once_` is the same). The window is one `pthread_key_create`.
  FORK-11 covers only std stream/env locks.
- **Runtime variables with no ruling on whether users set them.** [read]
  MORLOC_CHANNEL_DEPTH (default 4), MORLOC_ARROW_NO_BORROW (disables table
  pass-through), MORLOC_MALLOC_TUNING (`off` skips glibc tuning),
  MORLOC_R_GC_BUDGET (bytes, default 32 MiB, 0 off) and MORLOC_FORK_HEAP_PAD
  (bytes of glibc top pad and trim threshold in each Python or R fork-pool
  worker, default 16 MiB, `off` for none; a tunable by ruling 2026-11-14) are read by the runtime
  and absent from environmental-variables.asc.
- **More cited tests that miss their rule.** [read, 2026-10-21] NET-3:
  `the_header_names_every_kind_as_the_runtime_does` and
  `a_reply_is_refused_before_it_is_built_only_when_its_payload_is_past_the_frame`
  test other rules. PANIC-10: the cited test asserts only that REPLY is
  restored, not that the built packet is freed. SLOT-16: no test of a pool
  finishing stdio streams before replying. SLOT-24: cited tests check
  frame cutting, not that a block outlives a running job. DAEMON-14: the
  tests miss the swallowed-infra case (failure-classification.md).
  `a_request_trickling_in_ends_at_its_deadline` (NET-2) is defined in both
  morloc-runtime-types net_limits.rs:91 and mcp.rs, so deleting one leaves the citation green.
- **README's model list is incomplete.** runtime/README.md lists 10 TLA
  models; items also cite RunSweep, DaemonShutdown, CrossingConn,
  ForkPool, PanicExit, SlotSeqlock, LocalSweep, QueueHandoff,
  SlotRelease, ChannelWait, QueueWake, AheadJob.
- **LIFE-6 and ART-5 disagree on stale programs.** [read 2026-11-26]
  LIFE-6 says MORLOC_ABI_VERSION alone governs whether a program built
  against an older runtime runs, so a runtime update never forces a
  rebuild. ART-5 (draft) and morloc-manifest lib.rs:1194-1205 refuse any
  manifest whose build.morloc_version differs from the nexus version, so
  every release forces one. Ruled 2026-11-26: the ABI decides; ART-5 and
  the code change (plan /work/plans/runtime-interface-cleanup.md step 3).
