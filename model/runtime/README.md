# Runtime spec

Processes, threads, locks and shared-memory ownership in a built program:
the nexus, the pools, the daemon and libmorloc.

These files describe the system as it is. Where the code does not yet meet
a rule, the rule is listed with `Status: deviation` and says what is
missing. A rule changes in the same commit as any code that changes it.

Its items use `Status:` and `Checked by:` until they migrate to the shared
format in `../README.md`:

- `Status: implemented` becomes `Intent: ruled` with `Code: conforms`.
- `Status: deviation` becomes `Intent: ruled` with `Code: deviates`.
- `Status: draft` becomes `Intent: proposed`.
- `Checked by:` becomes `Tests:` (Rust test names, `tla:<config>`, spec
  tests).

Until then:

- `Status` is `implemented`, `deviation`, `draft` or `retired`. A draft
  states behavior that has not yet been checked against the code or ruled
  on.
- `Checked by` lists the tests that fail if the rule is broken: Rust test
  function names, `golden:<dir>` for a golden test, or `tla:<config>` for a
  model-checking run in `tla/`. An implemented item must name at least one.
  A deviation or a draft names none.

## Citing an item from code

In the files listed in the compiler `CLAUDE.md`, a comment is a reference
to an item, optionally followed by how the line applies it:

    // FORK-1: the cache's references are the parent's; forget them.

## Enforcement

`source_rule_tests::model_items_are_checked` in
`data/rust/morloc-runtime/src/lib.rs` fails when an ID is defined twice, an
implemented item names no test or a test that does not exist, a deviation
names a test, or a comment in the Rust crates cites an ID that no item
defines.

## Models

`tla/` holds PlusCal models of the protocols where interleavings, process
death and fork matter. `tla/check.sh` model-checks them: each `<Module>.cfg`
must check clean, and each `<Module>_<name>.bug.cfg`, a deliberately broken
variant of the protocol, must report a violation, which shows the model can
see the fault. It needs Java 11 or later and downloads the pinned TLA+ tools
release.

## Files

- `topology.md`: processes, threads and how they talk
- `shm.md`: shared-memory blocks and volumes (SHM)
- `fork.md`: what crosses fork and exec (FORK)
- `daemon.md`: the long-running daemon and its recovery (DAEMON)
- `streams.md`: file-backed streams and their registry slots (SLOT)
- `state.md`: fork classes and process-wide mutable values (STATE, INIT)
- `panic.md`: what a panic does in each process (PANIC)
- `network.md`: remote listeners and local endpoints (NET)
- `registry.tsv`: every process-wide mutable value with its fork class
- `tla/ShmHandoff.tla`: a block crossing between pools, with death and fork
- `tla/DaemonRecovery.tla`: pool-crash recovery against running requests
- `tla/ForkLocks.tla`: process-wide locks across fork
- `tla/LazyInit.tla`: initialising a process-wide value on first use
- `tla/PoolGroup.tla`: a process group id held while the nexus may signal it
- `tla/RouterRestart.tla`: the router restarting a program's daemon
- `tla/EndpointClaim.tla`: daemons claiming one socket path
- `tla/StreamQueue.tla`: writers, the queue and the custodian of a written stream
