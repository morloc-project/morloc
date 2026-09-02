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

## Haskell Coding Style
- comments should explain complex code and a rationale
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
stack test --test-arguments="--pattern='native-morloc'"
```

## Code Style

- Haskell (GHC 9.6.7, LTS 22.44)
- Build tool: Stack
- Module naming: `Morloc.CodeGenerator.Generate`
- Morloc syntax: Functional, ML-style
