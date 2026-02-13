# Runtime System

Compiled morloc programs execute as a **nexus-pool** architecture: a single C orchestrator (the nexus) dispatches function calls to language-specific worker processes (pools) via Unix domain sockets and msgpack serialization.

## Design Rationale

The nexus-pool model separates orchestration from computation:

- **The nexus** is a pre-compiled static C binary, built once during `morloc init`. It is data-driven: a per-program JSON manifest tells it which commands exist, which pools to start, and how to parse arguments. The nexus handles CLI parsing, argument serialization, pool lifecycle, dispatch, and result presentation.

- **Pools** are language-specific processes (one per language per program). Each pool loads generated code for its language, listens for requests from the nexus, executes functions, and returns serialized results. Pools for interpreted languages (Python, R) run the interpreter directly; C++ pools are compiled to native executables.

This separation means the nexus never changes between programs -- only the manifest and pool code differ. It also means language runtimes are isolated in their own processes, preventing memory corruption across language boundaries.

## Components

| Component | Language | Lifecycle |
|-----------|----------|-----------|
| Nexus binary | C | Built once (`morloc init`), copied per program |
| Manifest | JSON | Generated per program by `morloc make` |
| Python pool | Python + C extension | Generated per program |
| C++ pool | C++ | Generated and compiled per program |
| R pool | R + C extension | Generated per program |

## Subfiles

- [[execution-model.md]] -- Nexus lifecycle, pool management, dispatch flow, errors
- [[ipc.md]] -- Unix socket protocol, message format, data flow
- [[manifest.md]] -- JSON manifest schema
- [[cli.md]] -- Automatic CLI generation from type signatures
