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
Code: deviates (unfiled: types-kinds.asc says it fails only at code generation)

`Nat`, `Str`, `Rec`, `List` and `Set` expressions exist only in the type
system and are erased before anything runs. A type that classifies a term
(a signature, an annotation, a field) must be of kind `Type`. An alias may
name a type of any kind (ALIAS-11); its uses are checked like any type of
that kind.

### KIND-3 Nat arithmetic is integer arithmetic
Intent: proposed
Code: unaudited

`+ - * /` on `Nat` are evaluated whenever both operands are ground; `/`
truncates. A check on an expression with a free variable waits until the
variable is solved.

### KIND-4 A negative Nat
Intent: open
Code: unaudited

`3 - 10` reduces to `-7`, and a signature with a negative dimension is
accepted though no call can satisfy it. Candidates: (a) a `Nat` that reduces
below zero is a type error where it is reduced; (b) subtraction clamps at 0;
(c) negative values are legal.

### KIND-5 Missing non-`Type` arguments are filled gradually
Intent: proposed
Code: deviates (unfiled: types-kinds.asc says bare `Vector` passes typecheck)

A type constructor given fewer arguments than it declares has its missing
non-`Type` positions filled, left to right within each kind, with
placeholders that make no claim. `Type` positions are never filled: omitting
one is a type error at the signature.

### KIND-6 Gradual filling through a `type` alias
Intent: ruled 2026-10-08
Tests: spec-kind-6-1, spec-kind-6-2, spec-kind-6-3
Code: conforms 2026-10-09

Omitted non-`Type` arguments of an alias are filled as in KIND-5, whether
the parameter's kind is declared or inferred (ALIAS-8). ALIAS-3 counts only
`Type`-kinded parameters.
