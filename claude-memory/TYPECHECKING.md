# Type Checking

## Overview

Morloc has a multi-stage type system: general types (`TypeU`) resolve to language-specific types (`Type`), which can be further refined to foreign types (`TypeF`).

## Type Representations

**TypeU** - Unresolved/general types
- Used during parsing and initial type checking
- Language-agnostic type expressions
- May contain type variables, applications, constraints

**Type** - Resolved, language-specific types
- Specific to a language (Python, C++, R)
- Result of type resolution and manifold selection
- Used for code generation

**TypeF** - Foreign types
- Maps to actual language types (e.g., `int` in C++)
- Used in serialization and FFI boundaries

## Type System Features

**Parametric Polymorphism**
```
id :: a -> a
```

**Type Constraints**
```
add :: Num a => a -> a -> a
```

**Higher-Kinded Types**
```
map :: (a -> b) -> [a] -> [b]
```

**Type Aliases**
```
type Vector a = [a]
```

## Type Evaluation Pipeline

1. **Parse** - Types parsed into TypeU AST
2. **Link** - Resolve type references across modules
3. **Restructure** - Expand type aliases
4. **Typecheck** - Infer and check types
5. **Valuecheck** - Resolve language-specific implementations

## Key Files

**TypeEval.hs** - Type evaluation and resolution
- Evaluates type expressions
- Resolves type applications
- Handles type synthesis

**Typecheck/Internal.hs** - Core type checking
- Unification algorithm
- Constraint solving
- Type inference

**Typecheck.hs** (Frontend) - Frontend type checking
- Manages type checking context
- Integrates with parser output

## Typeclass System

**Class Definition:**
```
class Eq a where
  eq :: a -> a -> Bool
```

**Instance Declaration:**
```
instance Eq Int where
  eq = Int::eq
```

**Constraint Resolution:**
- Type checker resolves typeclass constraints
- Selects appropriate instance for each use site
- Validates instance coverage

See [[TYPECLASSES.md]] for syntax details.

## Type Inference

Morloc uses Hindley-Milner style type inference with extensions:
- Bidirectional type checking
- Let-polymorphism
- Typeclass constraint inference
- Language-specific type refinement

## Manifold Resolution

When a general function has multiple language implementations:
1. Type checker validates all implementations match signature
2. Valuecheck selects appropriate implementation based on context
3. Each implementation gets language-specific type refinement

See [[IMPLEMENTATION-POLYMORPHISM.md]] for details.

---
*See also: [[ARCHITECTURE.md]], [[TYPECLASSES.md]], [[IMPLEMENTATION-POLYMORPHISM.md]], [[DATA-STRUCTURES.md]]*
