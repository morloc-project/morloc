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
- **Block census test sees a block change under it.** [reproduced once,
  1 in 6 runs] Spec gap: no item says which tests may touch shared
  memory without the exclusive test guard. `cargo test -p morloc-runtime
  --lib` on 2026-10-12 failed
  `shm::tests::live_block_stats_counts_what_is_held` at its first
  assert (held != baseline + 2) while holding `own_test_registry`; five
  later runs passed. Something allocated or freed a block outside that
  guard during the test: a test without a guard, or a thread a previous
  test left running (a custodian, a write-behind worker). Not found.
  Seen again 2026-10-13 in a full workspace run with held = 6105935:
  not a block or two off, so the census walked a volume that changed
  under it (torn down or reinitialised), not one stray allocation.
  Again 2026-10-15 (held 902006 against 32770), 1 in 7 full lib runs.
- **Fork-pool unretired-worker test fails under load.** [reproduced once]
  Against SHM-8. `cargo test --workspace` on 2026-10-12 failed
  `fork_pool::tests::a_worker_that_ends_unretired_ends_the_pool`
  (morloc-runtime/src/fork_pool.rs ~l.626); five isolated reruns and two
  full runtime runs passed. Which assertion failed (both pings, or the
  coordinator's exit status within 10 s) was not captured. 2026-10-14:
  it and `a_fork_pool_serves_calls_and_stops_on_sigterm` ("a call went
  unanswered") failed in 2 of 3 `cargo test -p morloc-runtime --lib` runs
  while another test compressed 34 MiB at zstd 19 on all cores, and in 0
  of 3 without it: they fail under CPU load, not by chance. Again in a
  full `cargo test --workspace` at host load average ~6 (assertion at
  fork_pool.rs:634, the coordinator's exit status); five isolated reruns
  passed.
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
- **Nested arena reader test fails under a full lib run.** [reproduced
  once] `test_arena_lock_tests::a_nested_reader_is_not_blocked_by_a_queued_writer`
  (morloc-runtime/src/lib.rs, ~line 160) failed in one full
  `cargo test -p morloc-runtime --lib` on 2026-10-15 (load average about 5)
  and passed in three isolated runs. The test sleeps 100 ms for the writer
  to queue, so a writer scheduled later than that may change what it
  observes. Later on 2026-10-15 it failed in 4 of 5 full runs of HEAD
  6915296a at load average 2-5: "nested readers deadlocked behind a
  queued writer: Timeout".
- **Queue-bytes share test fails under a full lib run.** [reproduced, 1
  in 5 runs of HEAD 6915296a, 2 in 7 with later edits]
  `stream::write_behind_tests::streams_share_the_queue_bytes_and_keep_no_more_spares_than_their_depth`
  (SLOT-32): "the two streams left did not take the others' share", depth
  1 against 2 after its 10 s wait. Passes alone.
- **A failed-publish test fails under a full lib run.** [reproduced once,
  1 in 5 runs of HEAD 6915296a]
  `stream::tests::a_failed_publish_frees_the_blocks_the_slot_never_took`;
  message not kept.
- **A full lib run hung.** [observed once, 2026-10-15, 1 in about 12 full
  runs] `cargo test -p morloc-runtime --lib` ran 24 min (normally 85 s);
  11 `stream::tests::*` threads, a `stream::channel*` thread, write-behind
  and custodian threads sat in futex waits and 6 `ipc_ffi::reuse_*` /
  `arrow_ipc_reade*` threads in socket reads. Killed before the names of
  the stuck tests were captured (output was filtered); no gdb or perf in
  the container.
