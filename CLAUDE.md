# CLAUDE.md

Guidance for Claude Code when working with this repository.

For details, follow links like [[path/to/file]]. Ask before writing to doc files. For scratch notes, use `claude-cache/`. Keep docs under 100 lines.

## Project Overview

Morloc is a multi-lingual typed workflow language enabling function composition across Python, C++, and R under a unified type system. The compiler parses `.loc` files, performs type checking, and generates a JSON manifest and language-specific pools. A pre-compiled static nexus binary (built once during `morloc init`) reads the manifest at runtime to orchestrate cross-language calls via msgpack serialization.

## Haskell Coding Style
- comments should be sparse and succinct
- comments should explain complex code and a rationale
- avoid non-total functions when possible

## Testing Conventions
- tests should be written for all new features
- tests may be unit tests or integrated golden-tests
- test strategies, and justification for why they cover the new feature, should be provided

## Morloc Coding Style

All morloc signatures should have a one-line docstring (--' prefix), e.g.:

```
--' this function doubles a value
double :: Int -> Int
```

Use the . and $ operators as in Haskell

## Quick Start

```bash
# Build compiler
stack install --no-run-tests 

# Initialize environment (REQUIRED before tests)
morloc init -f
morloc install internal root root-py root-cpp root-r math

# Run tests
stack test

# Compile morloc program
morloc make -o foo script.loc
./foo <subcommand> <args>
```

## Documentation Index

**Formal Specification** -- [[spec/SPEC.md]] (master table of contents)

- [[spec/language/LANGUAGE.md]] - Language reference (syntax, expressions, declarations)
- [[spec/types/TYPES.md]] - Type system (inference, subtyping, records, typeclasses)
- [[spec/modules/MODULES.md]] - Module system (imports, resolution, packages)
- [[spec/interop/INTEROP.md]] - Cross-language interop (FFI, type mappings, serialization)
- [[spec/runtime/RUNTIME.md]] - Runtime system (nexus, pools, IPC, CLI generation)
- [[spec/compiler/COMPILER.md]] - Compiler architecture (pipeline, parsing, codegen, build)

**Internal Notes** (implementation-level, in `claude-memory/`):

- [[claude-memory/DEBUGGING.md]] - Debugging tips and techniques
- [[claude-memory/TESTS.md]] - Test suite structure and usage
- [[claude-memory/QC.md]] - Code quality workflow
- [[claude-memory/QC-TOOLS.md]] - Detailed tool documentation
- [[claude-memory/BUILD-TODO.md]] - Build system issues and improvements
- [[claude-memory/FIXITY-BUGS.md]] - Known fixity system issues
- [[claude-memory/DATA-STRUCTURES.md]] - Core Haskell data types
- [[claude-memory/TRANSLATOR-IR.md]] - Translator IR internals (LowerConfig, IProgram)

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
