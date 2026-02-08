# Type Checking Implementation

## Overview

Morloc has a multi-stage type system: general types (`TypeU`) resolve to language-specific types (`Type`), which map to foreign types (`TypeF`).

## Key Files

| File | Purpose |
|------|---------|
| `Frontend/Typecheck.hs` | Main typechecker, synthesis/checking |
| `Typecheck/Internal.hs` | Subtyping, instantiation, context ops |
| `TypeEval.hs` | Type alias expansion, evaluation |
| `Namespace.hs` | Type data structures (`TypeU`, `Gamma`) |

## Type Representations

- **TypeU**: Unresolved/general types with existentials
- **Type**: Resolved, language-specific types
- **TypeF**: Foreign types for FFI boundaries

## Core Functions

### Synthesis (`synthE`, `synthG`)

```haskell
synthE :: Int -> Gamma -> ExprS ... -> MorlocMonad (Gamma, TypeU, ExprS ...)
```
- Infers type from expression structure
- Returns updated context and typed expression
- Handles: literals, variables, applications, lambdas

### Checking (`checkE`, `checkG`)

```haskell
checkE :: Int -> Gamma -> ExprS ... -> TypeU -> MorlocMonad (Gamma, TypeU, ExprS ...)
```
- Verifies expression matches expected type
- Falls back to synthesis + subtyping via Sub rule

### Subtyping (`subtype`)

```haskell
subtype :: Scope -> TypeU -> TypeU -> Gamma -> Either TypeError Gamma
```
- Determines if first type is more polymorphic than second
- **Contravariant** for function arguments
- Solves existentials as side effect

### Instantiation (`instantiate`)

```haskell
instantiate :: Scope -> TypeU -> TypeU -> Gamma -> Either TypeError Gamma
```
- Solves existential = concrete type
- Handles functions, records, foralls

## Context Operations

```haskell
(+>)   :: Gamma -> GammaIndex -> Gamma       -- Add single entry
(++>)  :: Gamma -> [GammaIndex] -> Gamma     -- Add multiple entries
apply  :: Gamma -> TypeU -> TypeU            -- Substitute solved existentials
cut    :: GammaIndex -> Gamma -> Gamma       -- Remove entries up to marker
access1 :: TVar -> [GammaIndex] -> Maybe ... -- Find existential in context
lookupU :: TVar -> Gamma -> Maybe TypeU      -- Look up solved existential
lookupE :: EVar -> Gamma -> Maybe TypeU      -- Look up term variable type
```

## Lambda Synthesis Flow

```
synthE for LamS vs x:
  1. Create existentials for bound variables vs
  2. Add to context: g ++> zipWith AnnG vs paramTypes
  3. Synthesize body with bound vars in scope
  4. If body returns function:
     - Eta-expand: add new params, apply body to them
     - Construct result type directly (no re-synthesis)
  5. Return FunU paramTypes bodyType
```

## Function Subtyping Flow

```
subtype for FunU as1 ret1 <: FunU as2 ret2:
  1. Check arities match
  2. Subtype all args (contravariant): zip as2 as1
  3. Apply context to return types ONCE
  4. Subtype return types (covariant)
```

## Common Patterns

**Creating fresh existentials:**
```haskell
let (g', t) = newvar "prefix_" g
-- g' has ExistG added, t is ExistU
```

**Applying context to types:**
```haskell
let resolvedType = apply gamma unresolvedType
```

**Subtyping with error handling:**
```haskell
case subtype scope typeA typeB gamma of
  Left err -> throwError (GeneralTypeError err)
  Right gamma' -> return gamma'
```

## Error Types

| Error | Cause |
|-------|-------|
| `SubtypeError` | Types don't match in subtype check |
| `InstantiationError` | Can't solve existential |
| `OccursCheckFail` | Infinite type detected |
| `ApplicationOfNonFunction` | Applying non-function |
| `TooManyArguments` | More args than function accepts |

## Performance Optimizations (2026-02)

**Batch function subtyping** (`Internal.hs:188-200`):
- Process all function arguments before applying context
- Reduces O(n²) to O(n) for n-argument functions

**Direct eta expansion** (`Typecheck.hs:375-420`):
- Synthesize body once with proper context
- Construct expanded form directly without re-synthesis
- Reduces O(2ⁿ) to O(n) for nested lambdas

## Debugging

Enable verbose output:
```haskell
MM.sayVVV $ "message" <+> pretty value
```

Key debug functions in `Internal.hs`:
- `seeGamma` - print context
- `seeType` - print type
- `enter`/`leave` - trace call depth

---
*See also: [[TYPECHECKING-THEORY.md]], [[TYPECLASSES.md]], [[DATA-STRUCTURES.md]]*
