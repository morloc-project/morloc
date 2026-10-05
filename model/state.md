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
threads both initialise, and the second can destroy what the first built.
The primitive is either a value set at startup, before the process has
threads, or a `lazy` value whose initialiser runs under a lock and checks
again once it holds it; that lock must be held across fork so a child never
inherits it mid-initialisation (see FORK-5, FORK-9). The emitted Futhark
context follows it. Model: `tla/LazyInit.tla` (`LazyInit` passes;
`LazyInit_unlocked.bug` uses a destroyed value; `LazyInit_unheld.bug` leaves
the child blocked).

### INIT-2 A value whose construction waits on another process is built outside its lock
Status: implemented
Checked by: first_uses_on_two_threads_build_one_registry, tla:LazyInit_outside, tla:LazyInit_outside_blind.bug, tla:LazyInit_locked_waits.bug

A held lock taken across construction makes prepare wait for it (FORK-10):
opening a shared segment can wait seconds for the process sizing it. Such
a value is built without the lock; the lock is taken only to check again
and install it. A thread that finds a value already installed discards its
own, releasing only what it made: a segment is unmapped, never unlinked,
since the installed mapping names the same segment, and only an installed
segment joins the crash-sweep list. The stream registry, the statistics
segment, the benchmark and tee files, and the runtime configuration (whose
environment reads can wait on a thread forking through the standard
library, which holds the environment lock across prepare) follow it.

### INIT-3 A value set on first use without a lock is published by compare-and-swap
Status: implemented
Checked by: first_uses_on_many_threads_share_one_value_and_one_side_effect, a_child_forked_while_another_thread_builds_gets_a_value_at_once, no_runtime_value_waits_for_another_thread_to_initialise_it, tla:OncePublish, tla:OncePublish_oncelock.bug, tla:OncePublish_effect_each.bug

The standard library's once cells make later callers wait while one thread
initialises; a fork in that window leaves the child waiting forever on a
thread it does not have (FORK-9). A runtime value set on first use is
instead built by each first caller without a lock and published by
compare-and-swap: a caller that loses drops its own value and takes the
published one, and only the caller that won performs the value's side
effects, such as publishing the run directory to the environment or
starting the lifeline's watcher. Nothing waits, so a child either inherits
the published value or builds its own. Between the publication and the
side effect another thread, or a child forked in that window, sees the
value without the effect; a side effect that panics is not retried.
Model: `tla/OncePublish.tla`.

## Classes

- `held` -- taken by the prepare handler across fork, in `rank` order; the
  child sees a consistent copy (FORK-5, FORK-7).
- `reset` -- replaced by a fresh value in a forked child (FORK-8).
- `unreachable` -- coordinates threads a child does not have; a child never
  reaches it (FORK-12).
- `exec-only` -- lives in a process that forks only to exec (the nexus and
  the daemon); the child runs async-signal-safe code until exec.
- `startup` -- set once before the process starts threads; the child sees
  the same value.
- `lazy` -- created on first use: under a lock held across fork and checked
  again once taken (INIT-1), or built without one and published by
  compare-and-swap (INIT-3).
- `fork-scoped` -- describes one process; a child recomputes or forgets it.
- `counter` -- an atomic whose inherited value is harmless in a child
  (statistics, sequence numbers, flags).
- `thread` -- per-thread state; in a child only the forking thread's
  survives, and it describes that thread.
- `instance` -- a lock inside an object owned by another registered value.
- `paired` -- a condition variable that waits on a registered lock.
- `test-only` -- compiled only into tests.

A row's `cites` lists the deviation items it does not yet meet; an empty
`cites` means it meets its class today. `rank` is given for `held` rows only.

## Registry

The registry is `registry.tsv`: one row per value, tab-separated, with the
header `id class rank cites`. Empty fields are empty strings; `cites` is a
comma-separated list.

## Fork sites

Every call to `fork` outside tests, and what its child does: `exec`, code
that is `signal-safe`, or a pool `worker` that runs any code, forked only
through the thread-count gate (FORK-6). Processes are started with
`posix_spawn`, which runs no fork handler; a new `fork` call must be listed
here.

| Site | Child |
|---|---|
| morloc-runtime/fork_policy.rs::morloc_fork_worker | worker |
