# Phase-Indexed Types (Trees That Grow)

## Concept

**Trees That Grow** is a design pattern for AST evolution across compiler phases.

Instead of separate AST types per phase:
```haskell
-- Don't do this:
data ParsedExpr = ...
data TypedExpr = ...
data GeneratedExpr = ...
```

Use a single parameterized type:
```haskell
data Expr p = ...
  where p encodes the phase
```

## Morloc Usage

**Current state:** Morloc partially uses phase-indexed types.

**ExprI (Indexed Expression):**
```haskell
data ExprI = ExprI Int Expr
```

The `Int` index tracks:
- Source location
- Graph node IDs
- Expression identity

**Potential extension:**
```haskell
data Expr p
  = VarE (VarX p) EVar
  | AppE (AppX p) (Expr p) [Expr p]
  | LamE (LamX p) [EVar] (Expr p)
  ...

type family VarX p
type family AppX p
```

Different phases can attach different metadata.

## Benefits

- **Single AST definition** - No duplication
- **Type-safe phases** - Can't mix parsed/typed expressions
- **Extensibility** - Add new annotations without changing core structure

## Example Phases

```haskell
data Parsed
data TypeChecked
data Generated

type instance VarX Parsed = ()
type instance VarX TypeChecked = Type
type instance VarX Generated = (Type, Lang)

-- Usage
parseExpr :: Text -> Expr Parsed
typecheck :: Expr Parsed -> Expr TypeChecked
generate :: Expr TypeChecked -> Expr Generated
```

## References

- **GHC:** Uses Trees That Grow for HsSyn
- **Paper:** "Trees that Grow" by Najd & Peyton Jones (2016)

## Current Status in Morloc

Morloc doesn't fully implement this pattern. Expressions use `ExprI` with simple indexing. Future refactoring could adopt phase-indexed types for better type safety across compilation stages.

---
*See also: [[ARCHITECTURE.md]], [[DATA-STRUCTURES.md]]*
