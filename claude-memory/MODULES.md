# Module System

## Overview

Morloc modules are organized by "planes" (namespaces) with hierarchical names like `foo.bar.baz`.

Default location: `~/.local/share/morloc/src/morloc/plane/default/`

## Module Loading Pipeline

```
API.parse(mainModulePath, code)
  → Parse main module → ExprI
  → Extract imports (ImpE nodes)
  → parseImports (recursive resolution)
    → For each import:
      - findModule() - locate file
      - loadModuleConfig() - settings
      - loadModuleMetadata() - package.yaml
      - readProgram() - parse with accumulated state
      - Extract imports, recurse
  → Return DAG (modules + dependencies)
```

## Path Resolution

**Case 1: No Context (top-level file)**

Search:
1. Local: `./foo.loc` or `./foo/main.loc`
2. System: `$MORLOC_LIB/morloc/foo/main.loc`
3. Plane: `$MORLOC_LIB/default/foo/main.loc`

**Case 2: Import with No Common Prefix**

In module `foo.bar.baz`, importing `bif.buf`:

Search only system/plane (prevents local masking):
- `$MORLOC_LIB/morloc/bif/buf/main.loc`
- `$MORLOC_LIB/default/bif/buf/main.loc`

**Case 3: Import with Common Prefix**

In module `foo.bar.baz`, importing `foo.bif`:

Relative resolution:
- `../../bif/main.loc` (from `foo/bar/baz/main.loc`)
- Ensures namespace consistency

## State Accumulation

**ParserState:** Shared during parsing
- `stateFixityTable` accumulates across modules
- Passed to each `readProgram` call

**MorlocState:** Compiler-wide information
- Package metadata
- Function signatures
- Typeclasses
- Type definitions

**DAG:** Module dependency graph
- Nodes: Module variables (MVar)
- Edges: Import dependencies
- Data: ExprI (module expressions)

## Standard Library Modules

- `root` - Core morloc functions
- `root-py` - Python implementations
- `root-cpp` - C++ implementations
- `root-r` - R implementations
- `math` - Mathematical functions
- `internal` - Compiler internals

## Package Metadata

**File:** `package.yaml` in module directory

**Contents:**
- Module name
- Version
- Dependencies
- GitHub repository info

---
*See also: [[FRONTEND.md]], [[ARCHITECTURE.md]]*
