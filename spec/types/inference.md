# Type Inference

Morloc uses bidirectional type checking based on the Dunfield-Krishnaswami algorithm ("Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism", 2013). This provides sound and complete type inference for higher-rank polymorphism without requiring type annotations on lambda parameters.

## Judgment Forms

### Synthesis

```
Gamma |- e => A -| Delta
```

Given context Gamma and expression `e`, infer type `A` and produce updated context Delta.

Synthesis applies to:
- **Literals**: synthesize the literal's type (e.g., `42 => Int`)
- **Variables**: look up the type in the context
- **Applications**: synthesize the function type, then check arguments
- **Annotations**: use the declared type

### Checking

```
Gamma |- e <= A -| Delta
```

Given context Gamma, expression `e`, and expected type `A`, verify that `e` has type `A` and produce updated context Delta.

Checking applies to:
- **Lambdas with known argument types**: push argument types into the context, check the body
- **Expressions with type annotations**: verify the annotation matches

### The Sub Rule

When checking, if no specific checking rule applies, the checker falls back to synthesis plus subtyping:

```
  Gamma |- e => A -| Theta       Theta |- [Theta]A <: [Theta]B -| Delta
  -----------------------------------------------------------------------
                      Gamma |- e <= B -| Delta
```

This bridges synthesis and checking: synthesize the actual type, then verify it is a subtype of the expected type.

## Context

The context (Gamma) is an **ordered list** of bindings. Order matters: variables can only reference bindings that appear earlier.

| Entry | Meaning |
|-------|---------|
| Universal variable marker | A type variable is in scope |
| Term annotation `x : A` | Variable `x` has type `A` |
| Unsolved existential `a-hat` | An unknown type, to be solved |
| Solved existential `a-hat = tau` | Existential resolved to `tau` |
| Scope marker | Boundary for cutting |

### Context Operations

- **Extend**: add a binding to the end of the context
- **Apply**: substitute all solved existentials in a type
- **Cut**: remove all bindings after a scope marker (used when exiting a quantifier scope)
- **Lookup**: find a variable's type or an existential's solution
- **Fresh variable**: generate a new existential and add it to the context

### Monotonicity

Once an existential is solved, it remains solved. Later operations can only add more solutions, never retract them. This property enables batched processing: the compiler can process multiple function arguments before applying the accumulated solutions.

## Type Checking Strategy

### Literals

Literals synthesize their obvious type: integers as `Int`, floating-point as `Real`, strings as `Str`, booleans as `Bool`.

### Variables

A variable synthesizes the type recorded in the context at its binding site.

### Lambda Expressions

When a lambda is *checked* against a function type `A -> B`, the parameter is bound to type `A` and the body is checked against `B`.

When a lambda is *synthesized* (no expected type), fresh existentials are created for parameters. The body is synthesized, and if it returns a function type, the lambda is eta-expanded to make all arguments explicit.

### Application

To synthesize `f x`:

1. Synthesize the type of `f`, yielding `A`.
2. Apply the context to `A`.
3. If `A` is a function type `A1 -> A2`, check `x` against `A1`; the result type is `A2`.
4. If `A` is a universal `forall a. B`, instantiate `a` with a fresh existential and retry.
5. If `A` is an existential, decompose it into a function existential and retry.

### Where Clauses

Bindings in a `where` clause are type-checked as local definitions. Each binding's type is synthesized and added to the context before the main expression is checked.

## Occurs Check

Before solving an existential `a-hat = tau`, the checker verifies that `a-hat` does not appear free in `tau`. This prevents infinite types (e.g., `a = [a]`).

## Performance

Two optimizations avoid exponential behavior:

- **Batched function subtyping**: all arguments of a multi-argument function are subtype-checked before applying context updates, reducing O(n^2) to O(n).
- **Direct eta expansion**: when a lambda body returns a function, the expanded form is constructed directly without re-synthesizing, reducing O(2^n) to O(n) for nested lambdas.
