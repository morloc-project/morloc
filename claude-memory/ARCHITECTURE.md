# Architecture

## Overview

Morloc compiles `.loc` files into multi-language executables. The pipeline: parse → typecheck → generate code → build.

## Compilation Pipeline

Defined in `library/Morloc.hs`:

**1. Frontend** (`library/Morloc/Frontend/`)
- **Parser.hs** - Parses `.loc` into AST
- **Lexer.hs** - Tokenization
- **Link.hs** - Module resolution and imports
- **Merge.hs** - Combines modules into single representation
- **Restructure.hs** - Resolves type aliases
- **Treeify.hs** - Converts to annotated syntax tree
- **Typecheck.hs** - Type inference and checking
- **Valuecheck.hs** - Resolves implementations for general signatures

See [[FRONTEND.md]] for details.

**2. Type System** (`library/Morloc/`)
- **TypeEval.hs** - Type evaluation and resolution
- **Typecheck/Internal.hs** - Core type checking algorithms
- Supports general types (`TypeU`) that resolve to language-specific types (`Type`)
- Handles typeclasses, higher-kinded types, type synthesis

See [[TYPECHECKING.md]] for details.

**3. Code Generation** (`library/Morloc/CodeGenerator/`)
- **Generate.hs** - Main entry point
- **Realize.hs** - Reality checking (validates implementations exist)
- **LambdaEval.hs** - Lambda evaluation and application
- **Docstrings.hs** - Processes docstrings for CLI generation
- **Nexus.hs** - Generates C nexus orchestrator
- **Serial.hs** - Cross-language serialization (msgpack)
- **Grammars/Translator/** - Language-specific code translators (Python, C++, R)

See [[CODEGEN.md]] for details.

**4. Program Building** (`library/Morloc/ProgramBuilder/`)
- **Build.hs** - Writes generated code and invokes compilers

See [[SYSGEN.md]] for details.

## Data Flow

```
.loc files
  → Frontend: parse, link, typecheck
  → TypeU (general types) resolved to Type (language-specific)
  → Code Generator: realize, generate nexus + pools
  → Program Builder: write files, compile C/C++, link
  → Executable (nexus + pools)
```

## Major Components

| Component | Purpose | Key Files |
|-----------|---------|-----------|
| Frontend | Parsing, type checking | Parser.hs, Typecheck.hs |
| Type System | Type inference, resolution | TypeEval.hs, Typecheck/Internal.hs |
| Code Generator | Generate nexus + pools | Generate.hs, Nexus.hs, Serial.hs |
| Runtime | Execute across languages | nexus.c, pool.py, pool.cpp, pool.R |
| Module System | Load and resolve modules | Module.hs, Link.hs |

## Key Data Structures

See [[DATA-STRUCTURES.md]] for details.

**Namespace.hs** - Core types:
- `Expr` / `ExprI` - Expression AST
- `Type` / `TypeU` / `TypeF` - Type representations
- `Lang` - Language enumeration
- `MorlocMonad` - Compiler monad

**Module.hs** - Module system:
- Module path resolution
- Package metadata
- GitHub installation

## Runtime Architecture

Generated programs consist of:
- **Nexus** (C) - Orchestrator that dispatches function calls
- **Pools** (Python/C++/R) - Language-specific implementations
- **Message passing** - Msgpack serialization between nexus and pools

See [[RUNTIME.md]] for details.

---
*See also: [[FRONTEND.md]], [[TYPECHECKING.md]], [[CODEGEN.md]], [[RUNTIME.md]], [[DATA-STRUCTURES.md]]*
