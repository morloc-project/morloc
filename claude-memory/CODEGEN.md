# Code Generation

## Overview

Code generation transforms type-checked morloc expressions into a C nexus executable and language-specific pool files (Python, C++, R).

## Pipeline

Located in `library/Morloc/CodeGenerator/`:

1. **Generate.hs** - Main entry point
   - Orchestrates code generation
   - Manages language pool creation
   - Coordinates nexus and pool generation

2. **Realize.hs** - Reality checking
   - Validates implementations exist for all required functions
   - Ensures language-specific code is available
   - Reports missing implementations

3. **LambdaEval.hs** - Lambda evaluation
   - Evaluates lambda expressions
   - Handles function application
   - Manages closure conversion

4. **Docstrings.hs** - CLI generation
   - Extracts docstrings from function definitions
   - Generates command-line argument parsers
   - Creates help text

5. **Nexus.hs** - Nexus generation
   - Generates C orchestrator code
   - Creates dispatch tables
   - Manages inter-pool communication

6. **Serial.hs** - Serialization
   - Handles msgpack serialization/deserialization
   - Generates type-specific serializers
   - Manages data format conversions

## Language Translators

See [[TRANSLATOR-IR.md]] for the full IR architecture.

Located in `library/Morloc/CodeGenerator/Grammars/`:

- **Common.hs** — Shared infrastructure: `PoolDocs`, naming conventions, record collection/unification, dispatch extraction, AST inversion
- **Macro.hs** — Template macro expansion

Located in `library/Morloc/CodeGenerator/Grammars/Translator/`:

- **Imperative.hs** — Shared imperative IR (`IStmt`/`IExpr`), lowering functions, `LowerConfig`, `IProgram`, `buildProgram`/`buildProgramM`
- **Cpp.hs** — C++ translator (~592 lines). Monadic (`CppTranslator` state monad). Handles C++ types, struct generation, forward declarations.
- **Python3.hs** — Python translator (~223 lines). Pure (uses `IndexM`). Handles Python imports, namespacing.
- **R.hs** — R translator (~165 lines). Pure (uses `IndexM`). Handles R source loading, dynlib linking.
- **PseudoCode.hs** — Debug pseudocode renderer for diagnostics

Located in `library/Morloc/CodeGenerator/Grammars/Translator/Printer/`:

- **Cpp.hs** — C++ syntax printer (`printExpr`/`printStmt`/`printProgram`, struct/serializer templates, dispatch)
- **Python3.hs** — Python syntax printer (`printExpr`/`printStmt`/`printProgram`)
- **R.hs** — R syntax printer (`printExpr`/`printStmt`/`printProgram`)

### Translation Architecture

Each language translator follows the same pattern:

```
SerialManifold AST
  → preprocess (invertSerialManifold)
  → fold with defaultFoldRules(langLowerConfig)  -- shared IR lowering
  → PoolDocs (rendered MDoc fragments)
  → buildProgram/buildProgramM                    -- assemble IProgram
  → Printer.printProgram                          -- fill pool template
  → Script
```

The `LowerConfig` record (~30 fields) parameterizes all language-specific behavior: type rendering, accessors, constructors, let-binding syntax, function/lambda forms. The fold itself (`defaultFoldRules`) is written once in Imperative.hs.

## Generated Output

**Nexus** (`nexus.c`)
- C executable that orchestrates execution
- CLI argument parsing (from docstrings)
- Function dispatch to appropriate pools
- Msgpack serialization/deserialization

**Pools** (e.g., `pool_py_0.py`, `pool_cpp_1.cpp`, `pool_r_2.R`)
- Language-specific implementations
- Receive msgpack messages from nexus
- Execute native functions
- Return results via msgpack

## Code Generation Process

```
Type-checked AST
  → Realize: validate implementations
  → LambdaEval: evaluate lambdas
  → Docstrings: extract CLI info
  → Generate pools for each language
  → Generate nexus orchestrator
  → Serial: add serialization code
  → Output: write .c, .py, .cpp, .R files
```

## Key Data Flow

1. Function call in morloc -> nexus dispatch
2. Nexus serializes arguments -> msgpack
3. Nexus sends message -> appropriate pool
4. Pool deserializes -> calls native function
5. Pool serializes result -> msgpack
6. Nexus receives -> deserializes -> returns

See [[RUNTIME.md]] for runtime execution details.

---
*See also: [[ARCHITECTURE.md]], [[TRANSLATOR-IR.md]], [[RUNTIME.md]], [[SYSGEN.md]]*
