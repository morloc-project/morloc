# model/

Specifications the code is held to. `effects.md` covers the effect system;
the other files cover threads, processes, locks and shared-memory ownership
in the runtime (`data/rust`, `data/lang/*` binders).

These files describe the system as it is. Where the code does not yet meet
a rule, the rule is listed with `Status: deviation` and says what is
missing. A rule changes in the same commit as any code that changes it.

## Items

Every rule is an item with a stable ID:

    ### FORK-1 A forked child never releases what its parent holds
    Status: implemented
    Checked by: a_value_dropped_in_a_forked_child_is_forgotten

    Prose stating the rule and why it holds.

- `Status` is `implemented` or `deviation`.
- `Checked by` lists the tests that fail if the rule is broken: Rust test
  function names, `golden:<dir>` for a golden test, or `tla:<config>` for a
  model-checking run in `tla/`. An implemented item must name at least one.
  A deviation names none.
- IDs are never reused. A retired rule keeps its ID with `Status: retired`.

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

- `topology.md` -- processes, threads and how they talk
- `shm.md` -- shared-memory blocks and volumes (SHM)
- `fork.md` -- what crosses fork and exec (FORK)
- `daemon.md` -- the long-running daemon and its recovery (DAEMON)
- `streams.md` -- file-backed streams and their registry slots (SLOT)
- `state.md` -- fork classes and rules for process-wide mutable values (STATE, INIT)
- `registry.tsv` -- every process-wide mutable value with its fork class
- `effects.md` -- the effect system
- `tla/ShmHandoff.tla` -- a block crossing between pools, with death and fork
- `tla/DaemonRecovery.tla` -- pool-crash recovery against running requests
- `tla/ForkLocks.tla` -- process-wide locks across fork
- `tla/LazyInit.tla` -- initialising a process-wide value on first use
