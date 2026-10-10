# CLAUDE.md

Guidance for Claude Code when working with this repository.

## Project Overview

Morloc is a multi-lingual typed workflow language enabling function composition
across Python, C++, and R under a unified type system.

## General Rules

See @../../../CONVENTIONS.md for the workspace-wide rules (git, bug reporting,
correctness, test-first, comments, ASCII-only). Repo-specific rules follow.

- Performance is critical
  - Morloc programs may run for days or nanoseconds
  - All between process communication must be as fast as possible (no more than
    a few microseconds), and yet the processes must accommodate very large
    packets and programs that run for a very long time. 

## Checking code

After making a substantial change to the Haskell code, run:

$ stack install --no-run-tests 
$ stack test morloc:morloc-test  # This is the usual test

To run the full heavy integrated test suite, run:

$ stack test # ONLY do this at the very end of a session; IT IS EXPENSIVE

If you make any change to the non-haskell code in data/, then you MUST run

$ MORLOC_RUST_DIR=$PWD/data/rust morloc init -f

from the repo root. This rebuilds shared libraries, the nexus executable, and
language bindings. `MORLOC_RUST_DIR` is required: a bare `morloc init -f`
rebuilds from the installed copy of the runtime sources, not your working tree,
so your edit is silently absent from the library under test and the change
appears to have no effect.

After changing anything under `data/rust/`, run:

$ cargo test --workspace --manifest-path data/rust/Cargo.toml

- Stack test runs unit tests and golden-tests
- Golden-tests are full morloc programs
  - Each golden test is in the path @test-suite/golden-tests/<testname>
  - Every directory there is discovered and run; nothing needs registering
  - These tests produce build errors in `build.err` and runtime errors in
    `obs.err`. These outputs are VITAL to debugging errors.

If the required morloc libraries may have changed, you may run:

$ morloc install --force <remote-model-name>

## Specification

`model/` holds the spec the code is held to, in three parts that share the
format in `model/README.md`: `model/language/` (what programs mean),
`model/compiler/` (internal contracts) and `model/runtime/` (below).
`bash model/check.sh` checks the items and their test citations. A spec
test is named for its item (`spec-alias-3-1`): a case in
`test-suite/SpecTests.hs`, or a golden `test-suite/golden-tests/spec-*/`.
Before changing behavior a spec item governs, read the item; a `ruled`
item outranks the code.

## Issues

`model/runtime/issues/` is where every known issue in morloc is recorded,
so that none is lost between sessions. Record an issue there the moment you
find it, before working on it and whether or not you fix it. It replaces
loose findings files under `/work/plans`; GitHub issues remain for what the
user chooses to publish.

- Frame every issue against the model: either the code violates a named
  spec item, or the spec is incomplete (it says nothing, or something the
  user has not ruled on). Name the item, or the file whose gap it is.
- Keep entries minimal: what is wrong, the evidence level (reproduced,
  read, speculative), a reproduction or file:line, and any open design
  question. Cached state that helps the next session (a reproduction
  program, a narrowed cause, a partial ruling) belongs in the entry.
- Related issues share a file in `issues/`; `issues/LIST.md` lists every
  file with a one-line description. Add a line when you add a file.
- When an issue is solved, delete its entry completely, and delete a file
  (and its LIST.md line) once it is empty. The fix's commit and the spec
  item it now satisfies are the record; no solved or archived issues are
  kept here.
- Issue files are read by the model checks: keep them ASCII, and never
  start a heading with an item ID (`### SHM-6 ...`), which would define a
  duplicate item. Cite IDs in the text.

## Thread and memory model

`model/runtime/` holds the spec of threads, processes, locks and shared-memory
ownership. Every process-wide mutable value (lock, atomic, once-cell,
thread-local, lock field, binder or emitted static) needs a row in
`model/runtime/registry.tsv` with its fork class; a test fails until it has one, and
prints the row to fill in. It describes the system as it is; where code breaks it, the
break is listed in its deviations section. Read the relevant section before
changing that code, and change the spec in the same commit as any protocol
change.

In these files the only comments allowed are spec references:

- morloc-runtime/src: stream.rs, write_behind.rs, handle_scan.rs, pins.rs,
  cache.rs, shm.rs, shm_companion.rs, eval_arena.rs, cell.rs, crash.rs,
  daemon_ffi.rs, pool_ffi.rs, ipc_ffi.rs, router_ffi.rs, arrow_shm.rs,
  lifeline.rs, fork_policy.rs, panic_ffi.rs, custody.rs
- morloc-runtime-types/src: recoverable_lock.rs, shm_lock.rs,
  owner_word.rs, stream_handle.rs, dispatch_guard.rs, fd.rs, panic.rs,
  wait_word.rs
- morloc-nexus/src: process.rs

The form is `// <ID>: <how this line applies it>`, with `// SAFETY: <ID>: ...`
on unsafe blocks. The ID must name a spec item; the clause after it is
optional and says only why this line is written as it is. Format, parsing
and walk code in these files is exempt until it is split out. Each file
moves to this form only once its spec sections exist; until then, do not add
new comments to it that assert behaviour.

## Haskell Coding Style
- comments explain complex code; they never assert threading or ownership
  behaviour (see Thread and memory model)
- avoid non-total functions when possible
- an unused pattern binding becomes bare `_`, never `_oldname`; if it is truly
  unused, drop the name rather than leaving it visible

## ChangeLog

Do not edit `ChangeLog.md`, here or in any other repo, unless asked. Release
note wording and grouping are written by hand. When a change would normally
merit an entry, skip it and say so in the summary.

## Testing Conventions
- tests should be written for all new features
- tests may be unit tests or integrated golden-tests
- test strategies, and justification for why they cover the new feature, should be provided

## Development Commands

```bash
# Typecheck only
morloc typecheck script.loc

# Dump intermediate representations
morloc dump script.loc

# Run specific tests
stack test --test-arguments=--pattern=native-morloc
```

## Code Style

- Haskell (GHC 9.6.7, LTS 22.44)
- Build tool: Stack
- Module naming: `Morloc.CodeGenerator.Generate`
- Morloc syntax: Functional, ML-style
