# Module System

Morloc code is organized into modules with explicit exports and imports. Modules form a directed acyclic graph (DAG) of dependencies. The module system supports hierarchical naming, namespace isolation, and a plane-based organization for package distribution.

## Core Concepts

**Modules** are the unit of compilation and distribution. Each `.loc` file defines at most one module. A module declares its name, its exports, and its imports from other modules.

**Planes** are top-level namespaces that partition the module universe. The default plane contains the standard library and user-installed packages. Custom planes allow organizations to maintain private module collections.

**The dependency DAG** tracks which modules import which others. The compiler resolves this DAG during parsing, loading each module exactly once and accumulating state (fixity tables, type definitions, typeclasses) across the graph.

## State Accumulation

As modules are loaded, three kinds of state accumulate:

- **Parser state**: fixity tables propagate across modules so that operator precedence is consistent.
- **Compiler state**: type signatures, typeclass definitions, type aliases, and source declarations are collected globally.
- **Dependency graph**: the DAG records import relationships for linking and merge phases.

## Subfiles

- [[imports-and-exports.md]] -- Module declarations, import forms, export lists, visibility
- [[resolution.md]] -- Path resolution: local, system, and plane lookups
- [[packages.md]] -- Package metadata, versioning, and installation
