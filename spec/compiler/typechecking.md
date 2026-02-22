# Type Checking

The morloc typechecker implements bidirectional type checking based on Dunfield and Krishnaswami (2013). It infers types for unannotated expressions, verifies annotated expressions, and resolves polymorphism through subtyping and existential instantiation.

## Strategy

The typechecker makes a single pass over the annotated syntax tree, alternating between two modes:

- **Synthesis mode**: infer the type of an expression from its structure.
- **Checking mode**: verify that an expression matches an expected type.

The Sub rule bridges the modes: when checking, if no specific rule applies, synthesize the type and verify it is a subtype of the expected type.

## Type Representations

The typechecker operates on general types (`TypeU`), which include:

| Form | Description |
|------|-------------|
| `VarU a` | Universal type variable |
| `ExistU a-hat constraints` | Existential (unsolved) type variable |
| `ForallU a T` | Universal quantification |
| `FunU [A1,...,An] R` | Multi-argument function type |
| `AppU F [A1,...,An]` | Type constructor application |
| `NamU kind name params fields` | Named/record type |

## Context Threading

The typechecker threads an ordered context (Gamma) through every judgment. The context records:

- **Universal markers**: type variables currently in scope.
- **Term annotations**: the type of each bound variable.
- **Unsolved existentials**: type variables awaiting solution.
- **Solved existentials**: type variables resolved to a concrete type.
- **Scope markers**: boundaries for scoped bindings.

Context operations:
- **Extend**: add a new binding.
- **Apply**: substitute all solved existentials into a type.
- **Cut**: remove bindings beyond a scope marker (when exiting a quantifier's scope).
- **Lookup**: find a term's type or an existential's solution.

## Subtyping

The subtyping judgment `A <: B` checks whether A is at least as polymorphic as B:

- **Reflexivity**: `A <: A`.
- **Functions**: contravariant in arguments, covariant in returns.
- **Forall-left**: instantiate with a fresh existential.
- **Forall-right**: abstract over a fresh universal.
- **Existentials**: delegate to instantiation.

For multi-argument functions, all arguments are checked contravariantly in a single batch before applying accumulated context updates. This avoids quadratic behavior.

See [[../types/subtyping.md]] for the formal rules.

## Instantiation

When an existential must equal a concrete type:

- **Solve**: if the type is a monotype and passes the occurs check, solve the existential directly.
- **Function decomposition**: split the existential into argument and return existentials.
- **Forall decomposition**: instantiate the quantifier and continue.

## Expression Rules

### Literals

Literals synthesize their natural type: `42 => Int`, `3.14 => Real`, `"hello" => Str`, `True => Bool`.

### Variables

A variable synthesizes the type found at its binding in the context.

### Application

To typecheck `f x`:
1. Synthesize the type of `f`.
2. If `f : forall a. A`, instantiate `a` with a fresh existential.
3. If `f : FunU [A1,...] R`, check `x` against `A1`; result is the remaining function or return type.
4. If `f : ExistU a-hat`, decompose into `a-hat1 -> a-hat2` and retry.

### Lambda

Checked against `FunU [A1,...] R`: bind parameters to `A1,...`, check body against `R`.

Synthesized (no expected type): create fresh existentials for parameters, synthesize the body. If the body's type is a function, eta-expand to make all arguments explicit.

### Where Clauses

Each binding in a `where` clause is synthesized independently. The resulting types are added to the context before typechecking the main expression.

## Error Reporting

Type errors include source location information (derived from expression indices) and describe the mismatch:

| Error | Cause |
|-------|-------|
| Subtype error | Types incompatible in subtype check |
| Instantiation error | Cannot solve existential |
| Occurs check failure | Infinite type (e.g., `a = [a]`) |
| Application of non-function | Applying a value that is not a function |
| Too many arguments | More arguments than the function accepts |

## Multi-Stage Checking

The full type checking pipeline has two stages:

1. **Frontend typecheck**: operates on general types (`TypeU`). Produces a tree annotated with general types and potentially multiple language implementations.
2. **Value check**: resolves which language implementations to use, matching general types to concrete implementations. Verifies that implementations exist for all required functions.
