# Runtime System

## Overview

Morloc programs execute as a C nexus orchestrating language-specific pools via msgpack message passing.

## Architecture

```
User calls ./nexus foo --arg=42
  → Nexus parses CLI args
  → Nexus dispatches to pool_py_0
  → Python pool executes foo(42)
  → Returns result via msgpack
  → Nexus prints result
```

## Nexus (C Orchestrator)

**Source:** `data/nexus.c` (static program, compiled once during `morloc init`)
**Binary:** `~/.local/share/morloc/bin/morloc-nexus`
**Manifest reader:** `data/morloc/manifest.h` + `data/morloc/manifest.c`

**Responsibilities:**
- Load JSON manifest from `argv[0] + ".manifest"` at startup
- Parse command-line arguments (data-driven from manifest)
- Dispatch function calls to appropriate language pools
- Serialize/deserialize data via msgpack
- Manage pool processes (spawn, communicate, terminate)

**Manifest contents (per-program JSON):**
- Pool definitions (language, exec args, socket names)
- Command definitions (name, type, mid, pool index, arg schemas)
- Argument metadata (positional, optional, flag, group)
- Pure expression trees (for commands without pool dispatch)

## Language Pools

**Templates:** `data/pools/`

**pool.py** - Python runtime
- Imports generated Python code
- Receives msgpack messages on stdin
- Calls Python functions
- Returns msgpack on stdout

**pool.cpp** - C++ runtime
- Includes generated C++ code
- Receives msgpack messages
- Calls C++ functions
- Returns msgpack results

**pool.R** - R runtime
- Sources generated R code
- Receives msgpack messages
- Calls R functions
- Returns msgpack results

## Language Bindings

**Located:** `data/lang/`

**py/pymorloc.c** - Python C extension
- Msgpack serialization for Python types
- Built during `morloc init`
- Imported by Python pools

**cpp/cppmorloc.hpp** - C++ header
- Msgpack serialization for C++ types
- Template-based serialization
- Included by C++ pools

**r/rmorloc.c** - R C interface
- Msgpack serialization for R types
- Handles R vectors and lists
- Loaded by R pools

## Message Passing Protocol

**Request Format:**
```
{function_id: int, args: [msgpack_data, ...]}
```

**Response Format:**
```
{status: 0/1, result: msgpack_data}
```

**Data Types Supported:**
- Primitives: int, float, bool, string
- Collections: lists, vectors
- Records: named tuples/structs
- Nested structures

## Initialization

`morloc init` builds runtime components:
1. Compiles `libmorloc.so` (C runtime library)
2. Compiles `morloc-nexus` static binary → `~/.local/share/morloc/bin/`
3. Compiles `libcppmorloc.a` and precompiled header
4. Compiles `pymorloc.c` → Python C extension
5. Copies `cppmorloc.hpp` to morloc home
6. Compiles `rmorloc.c` → R shared library

Location: `~/.local/share/morloc/lib/` and `~/.local/share/morloc/bin/`

## CLI Generation

Function docstrings → argparse code in nexus:

```morloc
"""
Add two numbers

@arg x First number
@arg y Second number
"""
add :: Int -> Int -> Int
```

Generates CLI: `./nexus add --x=1 --y=2`

---
*See also: [[CODEGEN.md]], [[ARCHITECTURE.md]]*
