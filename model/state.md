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
inherits it mid-initialisation (see FORK-5, FORK-9). The stream registry and
the emitted Futhark context follow it; the registry's lock is not yet held
across fork. Model: `tla/LazyInit.tla` (`LazyInit` passes;
`LazyInit_unlocked.bug` uses a destroyed value; `LazyInit_unheld.bug` leaves
the child blocked).

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
- `lazy` -- created on first use under a lock, checked again once the lock
  is taken (INIT-1); fork-safe when that lock is held across fork.
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

Every call to `fork` outside tests, and what its child does. Processes are
started with `posix_spawn`, which runs no fork handler; a new `fork` call
must be listed here.

| Site | Child |
|---|---|
