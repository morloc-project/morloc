# Flaky tests

- **Sweeper test spawn fails.** [reproduced once, 1 in ~7 runs] Spec gap:
  no item covers spawning a process group. `cargo test --workspace` on
  2026-10-10 failed
  `process::tests::the_sweeper_waits_for_a_group_to_end_then_sweeps_its_dead_run`
  at morloc-nexus/src/process.rs:2125, the `.unwrap()` on
  `Spawn::run`; five isolated reruns and one workspace rerun passed. The
  error value was not captured; cause unknown.
- **Reader slot test fails under the gate.** [reproduced once] Against
  SLOT-9. The full gate on 2026-10-11 failed
  `stream::tests::a_readers_slot_for_a_stream_another_process_closed_is_released`
  (morloc-runtime/src/stream.rs, the final `assert!` of the forked child's
  result). 40 isolated runs and 4 full `cargo test -p morloc-runtime --lib`
  runs passed. The child's diagnostic line ("slot before ..., released ...,
  references held ...") is not kept by the gate's filter, so which of the
  three conditions failed is unknown; a rerun should keep the full test
  output.
- **A wall-clock typecheck test times out under load.** [reproduced once
  2026-10-14] `long non-tuple composition chain stays linear`
  (morloc-test) has a 1 s timeout; under `-j 6` it hit TIMEOUT at 1.05 s,
  while alone it takes 0.07 s (3 runs). A wall-clock bound in a parallel
  suite measures the machine; counting steps would test linearity.
- **The integration suite deletes every morloc run's resources on the
  machine.** [reproduced 2026-10-14] Gap: no item says what a test may
  remove. `cleanupMorlocResources`
  (test-suite/integration/Morloc/Test/Common.hs:456) runs `rm -f
  /dev/shm/mlc-*` and `rm -rf /tmp/morloc.*` at the start of each stress
  and shm test. The suite defaults to one thread for this reason, but a
  bare `stack test` given `--ta '-j 6'` passes it to both test suites
  and overrides that: the stress groups delete each other's live pool
  directories, every group reports leaks, and a `cpp-py` nexus (`run ...
  foo 2`) sleeps forever with its pools alive and idle. The same deletion
  hits any morloc program the user runs during the suite. Scope the
  cleanup and the counts to the test's own run (its pids or workDir).
- **A nexus whose run directory is deleted waits forever.** [observed
  once, under the deletion above] The nexus sat in `hrtimer_nanosleep`
  for 55 minutes while its Python pools polled. Which wait it was is not
  known; no item bounds it, and no peer died.
- **Block-count tests see another test's frees.** [reproduced, ~1 in 20
  full lib runs] Spec gap: no item says which threads may touch the shared
  test arena outside a guard.
  `stream::tests::a_value_handed_to_a_pool_is_one_block` ("46 live blocks
  -> 45 over 8 calls") and
  `stream::tests::a_failed_publish_frees_the_blocks_the_slot_never_took`
  count live blocks process-wide under the exclusive guard while a
  custodian thread of an earlier test was still freeing (seen: a
  `morloc-custody` thread in `free_uncounted` while another test held the
  guard). Releasing an exclusive guard now waits for every stream writer
  to finish, which should end this; not yet confirmed over enough runs.
- **A forked test child blocks on a lock held at fork.** [reproduced
  2026-10-15, about 1 in 40 full lib runs] Against FORK-11 (a recorded
  deviation). A child of `exits_cleanly_in_a_forked_child` runs until its
  5 s alarm (exit 114), e.g.
  `stream::channel_tests::a_settled_channel_leaves_no_reference_counted_to_its_process`.
  The child is forked from a process with dozens of test threads; a std
  lock one of them holds at that instant stays held in the child. Which
  lock is not yet known (the environment lock is ruled out:
  `set_test_env` refuses to run with other threads).
- **A full lib run hung.** [observed once, 2026-10-15; none in about 80
  clean runs since, 55 under gdb] `cargo test -p morloc-runtime --lib` ran
  24 min (normally 85 s); 11 `stream::tests::*` threads, three
  `stream::channel*` threads, write-behind and custodian threads sat in
  futex waits. The `ipc_ffi::reuse_*` and `arrow_ipc_reade*` threads in
  socket reads were listener threads of finished tests, not stuck tests.
  Unconfirmed guess: a channel test holding the exclusive guard waits
  forever. gdb cannot attach in the container (no CAP_SYS_PTRACE); run the
  test binary as gdb's inferior and interrupt it to see every stack.
