# Data Structures

## Overview

Key data types defined in `library/Morloc/Namespace.hs` and `library/Morloc/Module.hs`.

## Namespace.hs

**Expr** - Expression AST (untyped)
- Function applications
- Lambda abstractions
- Variables
- Literals
- Used during parsing

**ExprI** - Annotated expression (with metadata)
- Includes source location
- Type annotations
- Language tags
- Used after parsing

**Type** - Resolved, language-specific types
```haskell
data Type
  = VarT TVar
  | FunT Type Type
  | AppT Type Type
  | ForallT TVar Type
  | ArrT [Type]       -- Array/list
  | NamT EVar Type    -- Named type
```

**TypeU** - Unresolved/general types
- Language-agnostic
- Used before type resolution
- Can contain unification variables

**TypeF** - Foreign types
- Maps to actual language types
- Examples: `int`, `std::vector<int>`, `list`

**Lang** - Language enumeration
```haskell
data Lang
  = Python3
  | Cpp
  | R
  | MorlocLang  -- Pure morloc
```

**MorlocMonad** - Compiler monad
```haskell
type MorlocMonad = ExceptT MorlocError (StateT CompilerState IO)
```
- Error handling via `ExceptT`
- Compiler state (modules, type env)
- IO for file operations

## Module.hs

**Module** - Represents a loaded module
- Module path
- Exports/imports
- Definitions
- Type signatures

**ModuleName** - Qualified module name
- Plane (namespace)
- Path components

**Package** - Package metadata
- Name, version
- Dependencies
- GitHub repo info

## Common Patterns

**Annotated Trees:**
Many AST nodes use `Indexed` wrapper to add metadata:
```haskell
type ExprI = Indexed Expr
```

**Source Locations:**
Expressions track source positions for error reporting.

**Type Variables:**
- `TVar` - Type variables (e.g., `a`, `b`)
- `EVar` - Expression variables (e.g., function names)

---
*See also: [[ARCHITECTURE.md]], [[TYPECHECKING.md]]*
