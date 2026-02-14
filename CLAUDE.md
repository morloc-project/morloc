# CLAUDE.md

Guidance for Claude Code when working with this repository.

## Project Overview

Morloc is a multi-lingual typed workflow language enabling function composition
across Python, C++, and R under a unified type system.

## General Rules

- Errors are ALWAYS serious
- Do not ignore an error because it is pre-existing
- If an unexpected error is found, stop and describe it
- Performance is critical
  - Morloc programs may run for days or nanoseconds
  - All between process communication must be as fast as possible (no more than
    a few microseconds), and yet the processes must accommodate very large
    packets and programs that run for a very long time. 

## Checking code

After making a substantial change to the Haskell code, run:

$ stack install --no-run-tests 
$ stack test

If you make any change to the non-haskell code in data/, then you MUST run
`morloc init -f`. This will rebuild shared libraries, the nexus executable, and
language bindings.

- Stack test runs unit tests and golden-tests
- Golden-tests are full morloc programs
  - Each golden test is in the path @test-suite/golden-tests/<testname>
  - These tests produce build errors in `build.err` and runtime errors in
    `obs.err`. These outputs are VITAL to debugging errors.

If the required morloc libraries may have changed, you may run:

$ morloc install --force <remote-model-name>

## Other rules

Never ignore errors

## Git Rules

- DO NOT commit code
- DO NOT use any destructive git commit
- DO USE for `git log` and `git diff` variants

## Haskell Coding Style
- comments should be sparse and succinct
- comments should explain complex code and a rationale
- avoid non-total functions when possible

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

- Haskell (GHC 9.6.6, LTS 22.44)
- Build tool: Stack
- Module naming: `Morloc.CodeGenerator.Generate`
- Morloc syntax: Functional, ML-style
- **ASCII only** in all source files (C, C++, Haskell, Python, R, templates). No Unicode characters (em-dashes, smart quotes, etc). Non-ASCII in Template Haskell-embedded files causes silent truncation under POSIX locale.
