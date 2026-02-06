# Frontend

## Overview

The frontend transforms `.loc` files into a typed intermediate representation.

## Compilation Pipeline Position

```
.loc files
  → Lexer → Tokens
  → Parser → AST (ExprI)
  → Module System → DAG
  → Link/Merge → Unified compiler state
  → Restructure → Resolve type aliases
  → Treeify → Annotated syntax tree
  → Typecheck → Type-checked program
  → Code Generation
```

## Key Files

| File | Purpose |
|------|---------|
| `Frontend/Lexer.hs` | Tokenization, ParserState |
| `Frontend/Parser.hs` | Expression parsing, precedence climbing |
| `Frontend/API.hs` | Module loading orchestration |
| `Frontend/Link.hs` | Module linking into compiler state |
| `Frontend/Merge.hs` | Implementation conflict resolution |
| `Module.hs` | Module path resolution, metadata |

## Lexer

**Location:** `library/Morloc/Frontend/Lexer.hs`

**Parser Type:**
```haskell
type Parser a = StateT ParserState (Parsec Void Text) a
```

**ParserState:**
- `stateFixityTable` - Operator precedence/associativity
- `stateMinPos` - Indentation tracking
- `stateExpIndex` - Unique expression IDs
- `stateModuleConfig` - Module settings

**Features:**
- Indentation-sensitive (like Haskell)
- Docstring comments (`--'`)
- String interpolation (`#{}`)
- Multiple number formats (hex, octal, binary)

## Parser

**Location:** `library/Morloc/Frontend/Parser.hs`

**Entry Point:**
```haskell
readProgram :: Maybe MVar -> Path -> Text -> ParserState -> Either String (ExprI, ParserState)
```

**Top-Level Expressions:**
- Imports, type definitions, typeclasses, instances
- Fixity declarations
- Function assignments, type signatures
- Source declarations (FFI)

**Infix Operators:** Uses precedence climbing algorithm. See [[PARSING-ALGORITHMS.md]].

## Module System

**See:** [[MODULES.md]]

Three-case path resolution (local, system library, plane library).

State accumulates during loading:
- ParserState (fixity table)
- MorlocState (signatures, typeclasses, type defs)
- DAG (dependency graph)

## Link and Merge

**Link.hs:** Transfers terms/types/typeclasses from DAG to MorlocState.

**Merge.hs:** Combines multiple implementations (implementation polymorphism).

Example:
```morloc
import root-py (map)
import root-cpp (map)
-- map now has both Python and C++ implementations
```

## Known Issues

See [[FIXITY-BUGS.md]] for critical fixity system bugs.

---
*See also: [[PARSING-ALGORITHMS.md]], [[MODULES.md]], [[FIXITY-BUGS.md]], [[ARCHITECTURE.md]]*
