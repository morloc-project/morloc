# Process-wide state (STATE, INIT)

Every value that lives for the life of a process and can change -- a lock,
an atomic, a once-initialised cell, a thread-local, a lock inside a struct,
a mutable static in a language binder or in code the compiler emits -- is a
row in the registry below, with a class that says what happens to it
across fork.

### STATE-1 Every process-wide mutable value is registered with a fork class
Status: implemented
Checked by: every_process_wide_value_is_registered, every_registry_row_names_a_value_in_code, registry_rows_obey_their_class, deviating_rows_only_decrease

### INIT-1 A process-wide value is initialised once, by a fork-safe primitive
Status: deviation

Lazy initialisation built by hand (a null check, then a set) lets two
threads both initialise, and the second can destroy what the first built:
the stream registry's bootstrap can unmap a live mapping, and the emitted
Futhark context is created outside its mutex. The primitive is either a
value set at startup, before the process has threads, or a lazy value whose
initialiser runs under a held lock so the prepare handler waits for it
(see FORK-9).

## Classes

- `held` -- taken by the prepare handler across fork, in `Rank` order; the
  child sees a consistent copy (FORK-5, FORK-7).
- `reset` -- replaced by a fresh value in a forked child (FORK-8).
- `unreachable` -- coordinates threads a child does not have; a child never
  reaches it (FORK-12).
- `exec-only` -- lives in a process that forks only to exec (the nexus and
  the daemon); the child runs async-signal-safe code until exec.
- `startup` -- set once before the process starts threads; the child sees
  the same value.
- `fork-scoped` -- describes one process; a child recomputes or forgets it.
- `counter` -- an atomic whose inherited value is harmless in a child
  (statistics, sequence numbers, flags).
- `thread` -- per-thread state; in a child only the forking thread's
  survives, and it describes that thread.
- `instance` -- a lock inside an object owned by another registered value.
- `paired` -- a condition variable that waits on a registered lock.
- `test-only` -- compiled only into tests.

A row's `Cites` lists the deviation items it does not yet meet; an empty
`Cites` means it meets its class today. `Rank` is given for `held` rows only.

## Registry

| Id | Class | Rank | Cites |
|---|---|---|---|
| data/lang/cpp/cppmorloc.cpp::-::mlc_frame_ | thread |  |  |
| data/lang/cpp/cppmorloc.hpp::recur_env::stack | thread |  |  |
| data/lang/cpp/mlc_rec.hpp::-::q | thread |  |  |
| data/lang/cpp/pool.cpp::-::_shm_tracker_store | thread |  |  |
| data/lang/py/pymorloc.c::-::shm_view_ctx | thread |  |  |
| data/lang/py/pymorloc.c::-::shm_tracker | thread |  |  |
| data/lang/py/pymorloc.c::-::shm_tracker_count | thread |  |  |
| data/lang/py/pymorloc.c::-::shm_tracker_cap | thread |  |  |
| data/lang/py/pymorloc.c::-::shm_tracker_gen | thread |  |  |
| data/lang/py/pymorloc.c::-::PyMorlocException | startup |  |  |
| data/lang/py/pymorloc.c::-::PyMorlocInternalError | startup |  |  |
| data/lang/py/pymorloc.c::-::recur_env_stack | thread |  |  |
| data/lang/py/pymorloc.c::-::recur_env_depth | thread |  |  |
| data/lang/py/pymorloc.c::-::recur_env_cap | thread |  |  |
| data/lang/py/pymorloc.c::-::Methods | startup |  |  |
| data/lang/py/pymorloc.c::-::pymorloc | startup |  |  |
| data/lang/r/rmorloc.c::-::shm_tracker | fork-scoped |  |  |
| data/lang/r/rmorloc.c::-::shm_tracker_count | fork-scoped |  |  |
| data/lang/r/rmorloc.c::-::shm_tracker_cap | fork-scoped |  |  |
| data/lang/r/rmorloc.c::-::shm_tracker_gen | fork-scoped |  |  |
| data/lang/r/rmorloc.c::-::recur_env_stack | thread |  |  |
| data/lang/r/rmorloc.c::-::recur_env_depth | thread |  |  |
| data/lang/r/rmorloc.c::-::recur_env_cap | thread |  |  |
| data/lang/r/rmorloc.c::-::r_frames | thread |  |  |
| data/lang/r/rmorloc.c::-::r_frames_cap | thread |  |  |
| data/lang/r/rmorloc.c::-::daemon_creator_pid | fork-scoped |  |  |
| data/lang/r/rmorloc.c::-::r_shutting_down | thread |  |  |
| data/lang/r/rmorloc.c::mlc_trace_ipc::cached | thread |  |  |
| data/lang/r/rmorloc.c::run_job_c::budget | thread |  |  |
| data/lang/r/rmorloc.c::run_job_c::budget_read | thread |  |  |
| data/lang/py/pool.py::-::stop | unreachable |  | FORK-12 |
| data/lang/py/pool.py::-::job_q | unreachable |  | FORK-12 |
| data/lang/py/pool.py::-::sched | unreachable |  | FORK-12 |
| library/Morloc/CodeGenerator/Guest/Futhark.hs::-::m | reset |  | FORK-8 |
| library/Morloc/CodeGenerator/Guest/Futhark.hs::-::c | startup |  | INIT-1 |
| library/Morloc/CodeGenerator/Pools/CAbi/Members/RustPrinter.hs::-::SCHEMA_STRS | startup |  |  |
| library/Morloc/CodeGenerator/Pools/CAbi/Members/RustPrinter.hs::-::SCHEMA_TABLE | startup |  |  |
| morloc-runtime/arrow_ffi.rs::stats_enabled::FLAG | startup |  | FORK-9 |
| morloc-runtime/arrow_shm.rs::-::COPIED_BYTES | counter |  |  |
| morloc-runtime/arrow_shm.rs::-::LIVE_VIEW_BYTES | counter |  |  |
| morloc-runtime/arrow_shm.rs::-::BORROWABLE | reset |  | FORK-8 |
| morloc-runtime/arrow_shm.rs::borrowing_disabled::FLAG | startup |  | FORK-9 |
| morloc-runtime/cache.rs::pool_hash::CACHED | startup |  | FORK-9 |
| morloc-runtime/cache.rs::cache_base::CACHED | startup |  | FORK-9 |
| morloc-runtime/cache.rs::-::CACHE_HITS | counter |  |  |
| morloc-runtime/cache.rs::-::CACHE_MISSES | counter |  |  |
| morloc-runtime/cache.rs::-::CACHE_STORES | counter |  |  |
| morloc-runtime/cache.rs::read_cache_compression_level::CACHED | startup |  | FORK-9 |
| morloc-runtime/cell.rs::proc_tag::TAG | fork-scoped |  | FORK-13 |
| morloc-runtime/cell.rs::-::CELL_REGISTRY | reset |  | FORK-8 |
| morloc-runtime/cli.rs::-::STDIN_CLAIMED | counter |  |  |
| morloc-runtime/cli.rs::-::SPOOLED | reset |  | FORK-8 |
| morloc-runtime/crash.rs::-::LANG | startup |  |  |
| morloc-runtime/crash.rs::-::FRAME_FN | startup |  |  |
| morloc-runtime/crash.rs::-::PENDING | counter |  |  |
| morloc-runtime/daemon_ffi.rs::-::SHUTDOWN_REQUESTED | counter |  |  |
| morloc-runtime/daemon_ffi.rs::-::G_EVAL_TIMEOUT | startup |  |  |
| morloc-runtime/daemon_ffi.rs::-::G_DAEMON_OUTPUT_PACKET | startup |  |  |
| morloc-runtime/daemon_ffi.rs::-::G_DAEMON_COMPRESSION | startup |  |  |
| morloc-runtime/daemon_ffi.rs::-::CURRENT_OUTPUT_PACKET | thread |  |  |
| morloc-runtime/daemon_ffi.rs::-::CURRENT_OUTPUT_HTTP | thread |  |  |
| morloc-runtime/daemon_ffi.rs::-::CURRENT_OUTPUT_MEDIA_BYTES | thread |  |  |
| morloc-runtime/daemon_ffi.rs::-::G_EVAL_SANDBOX | startup |  |  |
| morloc-runtime/daemon_ffi.rs::-::G_EVAL_ALLOWED | exec-only |  |  |
| morloc-runtime/daemon_ffi.rs::-::RECOVERY_IN_PROGRESS | counter |  |  |
| morloc-runtime/daemon_ffi.rs::-::REQUESTS_IN_FLIGHT | exec-only |  |  |
| morloc-runtime/daemon_ffi.rs::-::REQUESTS_DRAINED | exec-only |  |  |
| morloc-runtime/daemon_ffi.rs::-::POOL_STATUS | exec-only |  |  |
| morloc-runtime/daemon_ffi.rs::-::BINDING_STORE | exec-only |  |  |
| morloc-runtime/daemon_ffi.rs::-::BINDING_FINISHED | exec-only |  |  |
| morloc-runtime/daemon_ffi.rs::-::REAPED_PID | counter |  |  |
| morloc-runtime/daemon_ffi.rs::-::REAPED_STATUS | counter |  |  |
| morloc-runtime/daemon_ffi.rs::-::REAPED_SEQ | counter |  |  |
| morloc-runtime/daemon_ffi.rs::-::REAPED_NEXT | counter |  |  |
| morloc-runtime/daemon_ffi.rs::-::WorkerContext.queue | exec-only |  |  |
| morloc-runtime/daemon_ffi.rs::-::WorkerContext.cond | exec-only |  |  |
| morloc-runtime/debug.rs::-::FRAMES | thread |  |  |
| morloc-runtime/debug.rs::-::DUMPED_COUNT | thread |  |  |
| morloc-runtime/debug.rs::-::MIDX_COUNTERS | thread |  |  |
| morloc-runtime/debug.rs::-::CACHE_DEPTH | startup |  | FORK-9 |
| morloc-runtime/debug.rs::-::CACHE_MAX | startup |  | FORK-9 |
| morloc-runtime/debug.rs::-::RECURSION_CAP | startup |  | FORK-9 |
| morloc-runtime/debug.rs::-::OVERFLOW_REPORTED | counter |  |  |
| morloc-runtime/eval_arena.rs::-::ARENA | thread |  |  |
| morloc-runtime/eval_arena.rs::-::TEARDOWN_PROBE | test-only |  |  |
| morloc-runtime/fork_local.rs::-::GENERATION | counter |  |  |
| morloc-runtime/fork_local.rs::register::ONCE | startup |  | FORK-9 |
| morloc-runtime/intrinsics.rs::-::TEMP_REGISTRY | reset |  | FORK-8 |
| morloc-runtime/intrinsics.rs::-::TEMP_OWNER_COUNTER | counter |  |  |
| morloc-runtime/intrinsics.rs::-::CURRENT_TEMP_OWNER | thread |  |  |
| morloc-runtime/ipc_ffi.rs::-::PING_PEEK_ACTIVE | counter |  |  |
| morloc-runtime/ipc_ffi.rs::trace_close_enabled::T | startup |  | FORK-9 |
| morloc-runtime/ipc_ffi.rs::-::SELF_SOCKET | held | 12 | FORK-5 |
| morloc-runtime/ipc_ffi.rs::forbid_self_call::FORBID | startup |  | FORK-9 |
| morloc-runtime/ipc_ffi.rs::-::HELD | test-only |  |  |
| morloc-runtime/ipc_ffi.rs::-::PEER | test-only |  |  |
| morloc-runtime/ipc_ffi.rs::-::SEEN_COUNT | test-only |  |  |
| morloc-runtime/ipc_ffi.rs::-::PEER_HAD_DATA | test-only |  |  |
| morloc-runtime/lib.rs::-::TestArenaLock.state | test-only |  |  |
| morloc-runtime/lib.rs::-::TestArenaLock.turn | test-only |  |  |
| morloc-runtime/lib.rs::ensure_test_arena::INIT | test-only |  |  |
| morloc-runtime/lib.rs::ensure_test_arena::SWEPT | test-only |  |  |
| morloc-runtime/lib.rs::ensure_test_arena::AT_EXIT | test-only |  |  |
| morloc-runtime/lifeline.rs::-::OWN | startup |  | FORK-9 |
| morloc-runtime/lifeline.rs::-::ADOPTED | startup |  | FORK-9 |
| morloc-runtime/lifeline.rs::guard::WATCHING | startup |  | FORK-9 |
| morloc-runtime/lifeline.rs::morloc_lifeline_child_env::ENTRY | startup |  | FORK-9 |
| morloc-runtime/log.rs::-::CALL_COUNTER | counter |  |  |
| morloc-runtime/log.rs::pool_pid::CACHED | fork-scoped |  | FORK-13 |
| morloc-runtime/log.rs::color_enabled::CACHED | startup |  | FORK-9 |
| morloc-runtime/log.rs::quiet::CACHED | startup |  | FORK-9 |
| morloc-runtime/log.rs::trace_enabled::CACHED | startup |  | FORK-9 |
| morloc-runtime/log.rs::-::BENCH_FILE | held | 13 | FORK-5, FORK-10 |
| morloc-runtime/manifest_ffi.rs::-::NAMED_FUNCTIONS | thread |  |  |
| morloc-runtime/packet.rs::-::INLINE_THRESHOLD | startup |  |  |
| morloc-runtime/packet.rs::-::SHM_ENABLED | startup |  |  |
| morloc-runtime/packet.rs::-::TEST_CONFIG_LOCK | test-only |  |  |
| morloc-runtime/packet.rs::-::TMPDIR | held | 11 | FORK-5 |
| morloc-runtime/packet.rs::-::CONFIG_INIT | startup |  | FORK-9 |
| morloc-runtime/packet_ffi.rs::make_file_data_packet_voidstar::SEQ | counter |  |  |
| morloc-runtime/pool_ffi.rs::-::SHUTTING_DOWN | counter |  |  |
| morloc-runtime/pool_ffi.rs::-::BUSY_COUNT | counter |  |  |
| morloc-runtime/pool_ffi.rs::-::JobQueue.state | unreachable |  | FORK-12 |
| morloc-runtime/pool_ffi.rs::-::JobQueue.cond | unreachable |  | FORK-12 |
| morloc-runtime/run.rs::-::RUN | startup |  | FORK-9 |
| morloc-runtime/run.rs::-::TEE_HANDLES | held | 14 | FORK-5, FORK-10 |
| morloc-runtime/run.rs::-::RUN_TEE_HANDLE | held | 15 | FORK-5, FORK-10 |
| morloc-runtime/run.rs::-::RunContext.command | held | 16 | FORK-5 |
| morloc-runtime/run.rs::-::RunContext.error | held | 17 | FORK-5 |
| morloc-runtime/run.rs::-::CONTEXT | startup |  | FORK-9 |
| morloc-runtime/shm.rs::page_size::CACHED | startup |  | FORK-9 |
| morloc-runtime/shm.rs::-::CURRENT_VOLUME | counter |  |  |
| morloc-runtime/shm.rs::-::VOLUMES | held | 5 | FORK-10 |
| morloc-runtime/shm.rs::-::ALLOC_MUTEX | held | 4 | FORK-10 |
| morloc-runtime/shm.rs::-::ALLOC_FORK_HELD | thread |  |  |
| morloc-runtime/shm.rs::register_fork_handlers::ONCE | startup |  | FORK-9 |
| morloc-runtime/shm.rs::-::RNG_STATE | thread |  |  |
| morloc-runtime/shm.rs::-::COMMON_BASENAME | held | 8 |  |
| morloc-runtime/shm.rs::-::FALLBACK_DIR | held | 9 |  |
| morloc-runtime/shm.rs::-::ATEXIT_REGISTERED | counter |  |  |
| morloc-runtime/shm.rs::-::SHCLOSE_HOOKS | held | 10 | FORK-5 |
| morloc-runtime/shm.rs::-::OWNER_PID | startup |  |  |
| morloc-runtime/shm.rs::-::POISON | startup |  |  |
| morloc-runtime/shm_companion.rs::-::MORLOC_COMPANION_NAMES | startup |  |  |
| morloc-runtime/shm_companion.rs::-::COMPANION_TOTAL_BYTES | counter |  |  |
| morloc-runtime/shm_stats.rs::-::COUNTERS | startup |  |  |
| morloc-runtime/shm_stats.rs::-::SEGMENT | held | 7 | FORK-5, FORK-10 |
| morloc-runtime/stream.rs::-::REGISTRY_BASE | startup |  | INIT-1 |
| morloc-runtime/stream.rs::-::REGISTRY_SLOT_COUNT | startup |  | INIT-1 |
| morloc-runtime/stream.rs::-::REGISTRY_SEGMENT | held | 6 | FORK-5 |
| morloc-runtime/stream.rs::stdout_staged::STAGED | startup |  | FORK-9 |
| morloc-runtime/stream.rs::-::PROCESS_LOCAL_SLOTS | held | 2 |  |
| morloc-runtime/stream.rs::-::PROCESS_LOCAL_RETURNED | paired |  |  |
| morloc-runtime/stream.rs::-::LOCKED_FDS | held | 3 |  |
| morloc-runtime/stream.rs::-::FORK_HELD | thread |  |  |
| morloc-runtime/stream.rs::register_fork_handlers::ONCE | startup |  | FORK-9 |
| morloc-runtime/stream.rs::slot_probe_seed::SEED | counter |  |  |
| morloc-runtime/stream.rs::-::RELEASE_PASS | held | 1 | FORK-10 |
| morloc-runtime/stream.rs::-::RELEASE_SERVICE | reset |  | FORK-8 |
| morloc-runtime/stream.rs::-::BEFORE_STREAM_LOCK | test-only |  |  |
| morloc-runtime/stream.rs::-::CURRENT_CALL_ID | thread |  |  |
| morloc-runtime/stream.rs::-::STDIO_SOCK | thread |  |  |
| morloc-runtime/stream.rs::-::DRAIN_GAP_HOOK | test-only |  |  |
| morloc-runtime/stream.rs::-::SWEEPER_TX | reset |  | FORK-8 |
| morloc-runtime/stream.rs::-::SWEEPER_HANDLE | reset |  | FORK-8 |
| morloc-runtime/stream.rs::generate_call_id::FALLBACK_COUNTER | counter |  |  |
| morloc-runtime/stream.rs::a_forked_child_leaves_its_parents_cached_reads_alone::HANDLE | test-only |  |  |
| morloc-runtime/stream.rs::channel_depth::DEPTH | startup |  | FORK-9 |
| morloc-runtime/stream.rs::a_batch_sealed_while_a_drain_finishes_is_still_drained::TARGET | test-only |  |  |
| morloc-runtime/utility.rs::create_beside::SEQ | counter |  |  |
| morloc-runtime/write_behind.rs::-::Job.input | instance |  |  |
| morloc-runtime/write_behind.rs::-::Job.result | instance |  |  |
| morloc-runtime/write_behind.rs::-::Job.done | instance |  |  |
| morloc-runtime/write_behind.rs::-::Service.queue | instance |  |  |
| morloc-runtime/write_behind.rs::-::Service.ready | instance |  |  |
| morloc-runtime/write_behind.rs::-::SERVICE | reset |  | FORK-8 |
| morloc-runtime/write_behind.rs::-::SEALED | reset |  | FORK-8 |
| morloc-runtime/write_behind.rs::-::THREAD_SEALED | thread |  |  |
| morloc-runtime-types/compression.rs::frame_workers::CACHED | startup |  | FORK-9 |
| morloc-runtime-types/owner_word.rs::-::HELD | thread |  |  |
| morloc-runtime-types/process.rs::token::CACHED | fork-scoped |  |  |
| rustmorloc/lib.rs::-::CSCHEMA_CACHE | thread |  |  |
| rustmorloc/lib.rs::-::LIVE_CSCHEMAS | test-only |  |  |
| rustmorloc/lib.rs::-::TEST_BASE | thread |  |  |
| rustmorloc/lib.rs::-::RECUR_ENV | thread |  |  |
| rustmorloc/lib.rs::-::SHM_TRACKER | thread |  |  |
| rustmorloc/lib.rs::-::TRACEBACK | thread |  |  |
| rustmorloc/lib.rs::-::CURRENT_FRAME | thread |  |  |
| rustmorloc/lib.rs::-::ACTIVE | thread |  |  |
| rustmorloc/lib.rs::-::QUEUE | thread |  |  |
| rustmorloc/lib.rs::-::TMPDIR | startup |  |  |
| morloc-nexus/main.rs::-::FRONTEND_ROUTER | exec-only |  |  |
| morloc-nexus/mcp.rs::new_session_id::CTR | exec-only |  |  |
| morloc-nexus/mcp.rs::-::Frontend.locks | exec-only |  |  |
| morloc-nexus/mcp.rs::-::Frontend.sessions | exec-only |  |  |
| morloc-nexus/parse_arg.rs::-::NEXUS_READS_STDIN | exec-only |  |  |
| morloc-nexus/parse_arg.rs::-::CONTEXTS | exec-only |  |  |
| morloc-nexus/parse_arg.rs::-::STAGE_DIR | exec-only |  |  |
| morloc-nexus/process.rs::-::RECOVERY_GENERATION | exec-only |  |  |
| morloc-nexus/process.rs::-::RECOVERY_CONTEXT | exec-only |  |  |
| morloc-nexus/process.rs::-::POOL_CHECK | exec-only |  |  |
| morloc-nexus/process.rs::-::PIDS | exec-only |  |  |
| morloc-nexus/process.rs::-::PGIDS | exec-only |  |  |
| morloc-nexus/process.rs::-::EXIT_STATUSES | exec-only |  |  |
| morloc-nexus/process.rs::-::POOL_START_TIMES | exec-only |  |  |
| morloc-nexus/process.rs::-::POOL_SWEPT | exec-only |  |  |
| morloc-nexus/process.rs::-::POOL_LANGS | exec-only |  |  |
| morloc-nexus/process.rs::-::CLEANING_UP | exec-only |  |  |
| morloc-nexus/process.rs::-::EXIT_CODE | exec-only |  |  |
| morloc-nexus/process.rs::-::BROKEN_PIPE | exec-only |  |  |
| morloc-nexus/process.rs::-::SHM_BASENAME_PREFIX | exec-only |  |  |
| morloc-nexus/process.rs::-::RUN_TMPDIR | exec-only |  |  |
| morloc-nexus/process.rs::-::SPAWN_TO_RECORD_DELAY_MS | test-only |  |  |
| morloc-nexus/runlog.rs::-::State.command | exec-only |  |  |
| morloc-nexus/runlog.rs::-::State.error | exec-only |  |  |
| morloc-nexus/runlog.rs::-::STATE | exec-only |  |  |
| morloc-nexus/stage.rs::-::STAGE | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::STDIN_SLOT | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::STDOUT_SLOT | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::STDERR_SLOT | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::STAGE_SLOT | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::STAGE_SCHEMA | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::STAGE_STDOUT_BROKEN | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::STDOUT_ROUTE | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::NEXUS_PID | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::DAEMON_MODE | exec-only |  |  |
| morloc-nexus/stdio_server.rs::-::RENDER_CFG | exec-only |  |  |
| morloc-nexus/stdio_server.rs::start::INIT | exec-only |  |  |
| morloc-nexus/view.rs::-::STDIN_TEMP_FILES | exec-only |  |  |
| morloc-nexus/view.rs::-::STDIN_ATEXIT_REGISTERED | exec-only |  |  |

## Fork sites

Every call to `fork` outside tests, and what its child does.

| Site | Child |
|---|---|
| morloc-nexus/process.rs::start_language_server | exec |
| morloc-runtime/daemon_ffi.rs::compile_binding | exec |
| morloc-runtime/daemon_ffi.rs::fork_morloc_command | exec |
| morloc-runtime/router_ffi.rs::router_start_program | exec |
| morloc-runtime/slurm_ffi.rs::submit_morloc_slurm_job | exec |
| morloc-runtime/lifeline.rs::teardown | signal-safe |
