# Subtyping

Morloc's subtyping relation captures when one type is *more polymorphic* than another. This is not subtyping in the object-oriented sense; it governs how polymorphic types may be used where less polymorphic types are expected.

## Subtyping Judgment

The judgment `A <: B` means "A is at least as polymorphic as B" -- any value of type A can be used where a value of type B is expected.

## Rules

### Reflexivity

A type is a subtype of itself:

```
A <: A
```

### Function Types (Contravariant Arguments)

Function subtyping reverses the direction for arguments:

```
  B1 <: A1       A2 <: B2
  -------------------------
  A1 -> A2  <:  B1 -> B2
```

Arguments are contravariant: if `f : A1 -> A2` is used where `g : B1 -> B2` is expected, then `f` must accept a *wider* range of inputs (`B1 <: A1`) and produce a *narrower* range of outputs (`A2 <: B2`).

For multi-argument functions, each argument is checked contravariantly and the return type covariantly.

### Universal Quantification (Left)

A universally quantified type on the left is instantiated with a fresh existential:

```
  [a-hat/a]A <: B
  -----------------
  forall a. A <: B
```

This means a polymorphic type can be used where a specific type is expected, by choosing an appropriate instantiation.

### Universal Quantification (Right)

A universally quantified type on the right requires the left side to work for all instantiations:

```
  A <: B            (a is fresh)
  ----------------
  A <: forall a. B
```

### Existential Instantiation

When an unsolved existential variable meets a concrete type, the subtyping relation solves the existential:

```
  a-hat not in FV(A)       A <=: a-hat
  --------------------------------------
           a-hat <: A
```

The `<=:` (instantiation) relation handles the mechanics of solving existentials. See [[#instantiation]].

## Instantiation

The instantiation judgment `A <=: a-hat` (or `a-hat <=: A`) solves the existential variable `a-hat` to a specific type.

### Solve

If A is a monotype (no quantifiers, no unsolved existentials):

```
  a-hat <=: tau     solves as     a-hat = tau
```

Subject to the occurs check: `a-hat` must not appear in `tau` (prevents infinite types).

### Function Decomposition

If the existential must be a function type:

```
  a-hat = a-hat1 -> a-hat2       A1 <=: a-hat1       a-hat2 <=: A2
  -------------------------------------------------------------------
                        A1 -> A2 <=: a-hat
```

The existential is decomposed into fresh existentials for the argument and return types.

## Context Threading

Subtyping and instantiation thread a context (Gamma) through the judgment. Each step may add solved existentials to the context, which subsequent steps can observe. The context is an ordered list; variables can only reference bindings earlier in the list. See [[inference.md]] for context operations.
