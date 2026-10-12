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
