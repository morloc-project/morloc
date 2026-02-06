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

Located in `library/Morloc/CodeGenerator/Grammars/Translator/`:

**Python Translator**
- Generates Python pool code
- Maps morloc types to Python types
- Handles Python-specific idioms

**C++ Translator**
- Generates C++ pool code
- Maps morloc types to C++ types
- Manages memory and templates

**R Translator**
- Generates R pool code
- Maps morloc types to R types
- Handles R vector semantics

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

1. Function call in morloc → nexus dispatch
2. Nexus serializes arguments → msgpack
3. Nexus sends message → appropriate pool
4. Pool deserializes → calls native function
5. Pool serializes result → msgpack
6. Nexus receives → deserializes → returns

See [[RUNTIME.md]] for runtime execution details.

---
*See also: [[ARCHITECTURE.md]], [[RUNTIME.md]], [[SYSGEN.md]]*
