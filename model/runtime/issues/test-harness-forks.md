# Test children inherit other tests' descriptors

Gap: model/runtime/fork.md says nothing about isolating tests that fork
from the multithreaded cargo test harness. A child forked without exec keeps
every descriptor open at that instant, including ones another test thread
believes it has closed.

- `daemon_ffi::endpoint_claim_tests::a_live_listener_keeps_its_path_and_a_stale_file_is_replaced`
  (daemon_ffi.rs:4384-4386): after `drop(live)` a sibling's child can still
  hold the listening socket, so the reclaim is refused with "a daemon is
  already serving". Evidence: observed once in `cargo test --workspace`
  (2026-10-09), not reproduced on demand.
- Same family, fixed: exec of a just-written script failing with ETXTBSY
  (router_ffi and lifeline tests now use `write_test_executable`).

Open design question: a harness-wide remedy (children close descriptors
they did not create, or forking tests serialize against descriptor-sensitive
ones) versus per-test fixes.
