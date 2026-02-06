# CLAUDE.md

Guidance for Claude Code when working with this repository.

For details, follow links like [[path/to/file]]. Ask before writing to doc files. For scratch notes, use `claude-cache/`. Keep docs under 100 lines.

## Project Overview

Morloc is a multi-lingual typed workflow language enabling function composition across Python, C++, and R under a unified type system. The compiler parses `.loc` files, performs type checking, and generates a C nexus executable that orchestrates cross-language calls via msgpack serialization.

## Quick Start

```bash
# Build compiler
stack build && stack install

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

**Architecture & Systems:**
- [[claude-memory/ARCHITECTURE.md]] - Compilation pipeline and major components
- [[claude-memory/FRONTEND.md]] - Parser and lexer
- [[claude-memory/TYPECHECKING.md]] - Type system and inference
- [[claude-memory/CODEGEN.md]] - Code generation
- [[claude-memory/RUNTIME.md]] - Nexus, pools, and language bindings
- [[claude-memory/SYSGEN.md]] - Build system and program generation
- [[claude-memory/MODULES.md]] - Module system and resolution
- [[claude-memory/DATA-STRUCTURES.md]] - Core data types

**Language & Syntax:**
- [[claude-memory/MORLOC_SYNTAX.md]] - Morloc syntax reference
- [[claude-memory/IMPLEMENTATION-POLYMORPHISM.md]] - Multi-language implementations
- [[claude-memory/INFIX-OPERATORS.md]] - Operator syntax and fixity
- [[claude-memory/RECORDS.md]] - Record types
- [[claude-memory/TYPECLASSES.md]] - Typeclass system

**Parser Details:**
- [[claude-memory/PARSING-ALGORITHMS.md]] - Precedence climbing
- [[claude-memory/FIXITY-BUGS.md]] - Known fixity system issues

**Development:**
- [[claude-memory/TESTS.md]] - Test suite structure and usage
- [[claude-memory/DEBUGGING.md]] - Debugging tips and techniques
- [[claude-memory/QC.md]] - Code quality workflow (before pushing to master)
- [[claude-memory/QC-TOOLS.md]] - Detailed tool documentation

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
