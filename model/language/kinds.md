# Kinds

Kinds of types (types, naturals, strings, constructors) and kind checking. Prefix: KIND.

### KIND-1 The kinds are `Type`, `Nat`, `Str`, `Rec`, `List` and `Set`
Intent: proposed
Code: unaudited

A parameter is given a kind as `(n :: K)` in its type's declaration; an
unannotated parameter has kind `Type`, except an alias parameter (ALIAS-8). Any other kind name is a parse error.
`List` and `Set` hold `Str` elements.

### KIND-2 Only a `Type`-kinded type classifies values
Intent: proposed
Tests: spec-kind-2-1, spec-kind-2-2, spec-kind-2-3, spec-kind-2-4, spec-kind-2-5, spec-kind-2-6, spec-kind-2-7, spec-kind-2-8, spec-kind-2-9
Code: conforms 2026-10-30

`Nat`, `Str`, `Rec`, `List` and `Set` expressions exist only in the type
system and are erased before anything runs. A type that classifies a term
(a signature, an annotation, a field) must be of kind `Type`. An alias may
name a type of any kind (ALIAS-11); its uses are checked like any type of
that kind.

### KIND-3 Nat arithmetic is integer arithmetic
Intent: proposed
Code: unaudited

`+ - * /` on `Nat` are evaluated whenever both operands are ground; `/`
truncates. A check on an expression with an inferred variable waits until
the variable is solved; one with a rigid variable (POLY-7) is decided by
DIM-7.

### KIND-4 A Nat is never negative
Intent: ruled 2026-10-29
Tests: spec-kind-4-1, spec-kind-4-2
Code: deviates (issues/kinds-dimensions.md)

A `Nat` expression that reduces below zero is a type error where it
occurs. One over rigid variables (POLY-7) is an error when it is negative for
some value of them (`n - 1` with `n` rigid); one over inferred variables
needs a solution that keeps it non-negative (DIM-6, DIM-7). Every signature
is checked, whether or not its term is used.

Why: a signed quantity would be a separate kind.

### KIND-5 Missing non-`Type` arguments are filled gradually
Intent: proposed
Tests: spec-kind-5-1, spec-kind-5-2, spec-kind-5-3, spec-kind-5-4
Code: deviates (issues/kinds-dimensions.md)

A type constructor given fewer arguments than it declares has its missing
non-`Type` positions filled, left to right within each kind, with
placeholders that make no claim. `Type` positions are never filled: omitting
one is a type error at the signature.

### KIND-6 Gradual filling through a `type` alias
Intent: ruled 2026-10-08
Tests: spec-kind-6-1, spec-kind-6-2, spec-kind-6-3
Code: deviates (issues/kinds-dimensions.md)

Omitted non-`Type` arguments of an alias are filled as in KIND-5, whether
the parameter's kind is declared or inferred (ALIAS-8). ALIAS-3 counts only
`Type`-kinded parameters.
