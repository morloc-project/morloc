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

**Template:** `data/nexus.c`

**Responsibilities:**
- Parse command-line arguments (auto-generated from docstrings)
- Dispatch function calls to appropriate language pools
- Serialize/deserialize data via msgpack
- Manage pool processes (spawn, communicate, terminate)

**Generated Sections:**
- Argument parser (from function docstrings)
- Dispatch table (maps function names to pool IDs)
- Serialization calls (type-specific)
- Main function (CLI entry point)

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

`morloc init` builds language bindings:
1. Compiles `pymorloc.c` → Python C extension
2. Copies `cppmorloc.hpp` to morloc home
3. Compiles `rmorloc.c` → R shared library

Location: `~/.local/share/morloc/lib/`

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
