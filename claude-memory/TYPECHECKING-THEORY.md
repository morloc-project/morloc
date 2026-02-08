# Type System Theory

## Theoretical Foundation

Morloc's typechecker implements **Dunfield-style bidirectional type checking** based on "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism" (Dunfield & Krishnaswami, 2013). This provides:

- Sound and complete type inference for higher-rank polymorphism
- Decidable type checking without explicit type annotations on lambdas
- Principled handling of existential and universal quantification

## Judgments

### Bidirectional Checking

Two judgment forms drive inference:

**Synthesis** `Γ ⊢ e ⇒ A ⊣ Δ`
- Given context Γ and expression e, output type A and updated context Δ
- Used for literals, variables, applications

**Checking** `Γ ⊢ e ⇐ A ⊣ Δ`
- Given context Γ, expression e, expected type A, output updated context Δ
- Used when type is known (function arguments, annotations)

**Sub rule** connects them:
```
  Γ ⊢ e ⇒ A ⊣ Θ       Θ ⊢ [Θ]A <: [Θ]B ⊣ Δ
  ─────────────────────────────────────────── Sub
            Γ ⊢ e ⇐ B ⊣ Δ
```

### Subtyping

Subtyping `Γ ⊢ A <: B ⊣ Δ` captures when A is more polymorphic than B:

```
           Γ ⊢ A <: A ⊣ Γ                              (<:Var)

  Γ ⊢ B₁ <: A₁ ⊣ Θ       Θ ⊢ [Θ]A₂ <: [Θ]B₂ ⊣ Δ
  ──────────────────────────────────────────────       (<:→) contravariant args
          Γ ⊢ A₁→A₂ <: B₁→B₂ ⊣ Δ

  Γ,â ⊢ [â/a]A <: B ⊣ Δ,â,Θ
  ────────────────────────────                         (<:∀L) instantiate left
      Γ ⊢ ∀a.A <: B ⊣ Δ

  Γ,a ⊢ A <: B ⊣ Δ,a,Θ
  ─────────────────────                                (<:∀R) abstract right
    Γ ⊢ A <: ∀a.B ⊣ Δ

      â ∉ FV(A)       Γ[â] ⊢ A ≤: â ⊣ Δ
  ─────────────────────────────────────────            (<:InstantiateL/R)
           Γ[â] ⊢ â <: A ⊣ Δ
```

### Instantiation

When existentials meet concrete types:

```
  Γ[â],â₁,â₂,â=â₁→â₂ ⊢ A₁ ≤: â₁ ⊣ Θ    Θ ⊢ â₂ ≤: [Θ]A₂ ⊣ Δ
  ──────────────────────────────────────────────────────────── InstLArr
              Γ[â] ⊢ A₁→A₂ ≤: â ⊣ Δ

                    Γ[â] ⊢ τ
  ──────────────────────────────────                   InstLSolve
     Γ[â],Θ ⊢ â ≤: τ ⊣ Γ[â=τ],Θ
```

## Context (Gamma)

The context is an **ordered list** of bindings - order matters for scoping:

| Entry | Meaning |
|-------|---------|
| `VarG a` | Universal type variable marker |
| `AnnG x A` | Term variable x has type A |
| `ExistG â` | Unsolved existential variable |
| `SolvedG â τ` | Existential solved to τ |
| `MarkG â` | Scope marker for cutting |

**Invariant**: Variables can only reference bindings to their right (earlier in context).

## Types (TypeU)

```haskell
data TypeU
  = VarU TVar                -- Universal variable (a)
  | ExistU TVar params recs  -- Existential (â) with constraints
  | ForallU TVar TypeU       -- Universal quantifier (∀a. A)
  | FunU [TypeU] TypeU       -- Function (A₁ → ... → Aₙ → B)
  | AppU TypeU [TypeU]       -- Type application (F a b)
  | NamU ...                 -- Records/named types
```

## Key Properties

### Monotonicity of Context

Once an existential is solved (`SolvedG â τ`), it remains solved. Later operations can only add more solutions. This enables:
- Batched processing of function arguments
- Deferred context application

### Well-Scopedness

Existentials can only be solved to types containing variables to their right in context. The `access1`/`access2` functions maintain this.

### Occurs Check

Before solving `â = τ`, verify `â ∉ FV(τ)` to prevent infinite types.

## Morloc Extensions

### Multi-arity Functions

Morloc normalizes curried functions to multi-argument form:
```
(A → B) → C   normalizes to   FunU [A, B] C
```

### Existentials with Constraints

```haskell
ExistU v (params, openness) (records, openness)
```
- `params`: Type parameters (for parameterized types)
- `records`: Key-value pairs (for record types)
- `openness`: Open (can extend) or Closed

### Eta Expansion

When a lambda body returns a function type, morloc eta-expands:
```
\x -> f     where f : A → B
becomes
\x y -> f y
```

## Complexity Considerations

### Function Subtyping

For `A₁ → ... → Aₙ → R <: B₁ → ... → Bₙ → S`:
- Naive: Apply context after each argument → O(n²)
- Optimized: Batch all arguments, apply once → O(n)

### Eta Expansion

For nested lambdas returning functions:
- Naive: Re-synthesize after expansion → O(2ⁿ)
- Optimized: Construct expanded form directly → O(n)

---
*See also: [[TYPECHECKING-IMPLEMENTATION.md]], [[DATA-STRUCTURES.md]], [[TYPECLASSES.md]]*
