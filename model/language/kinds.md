# Kinds

Kinds of types (types, naturals, strings, constructors) and kind checking. Prefix: KIND.

### KIND-1 The kinds are `Type`, `Nat`, `Int`, `Str`, `Rec`, `List` and `Set`
Intent: proposed
Tests: spec-kind-1-1, spec-kind-1-2
Code: conforms 2026-11-25

A parameter is given a kind as `(n :: K)` in its type's declaration; an
unannotated parameter has kind `Type`, except an alias parameter (ALIAS-8). Any other kind name is a parse error.
`List` and `Set` hold `Str` elements. `Int` is the kind of the value a
label `x@Int` binds (POLY-3), a signed integer; no parameter is declared
with it, and it fills a `Nat` position (KIND-4).

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
Code: conforms 2026-11-25

`+ - * /` on `Nat` and `Int` are integer operations: a part of an expression may be
negative (`(3 - 10) + 20` is 13), and `/` truncates toward zero. They are
evaluated whenever both operands are ground. A check on an expression with
an inferred variable waits until the variable is solved; one with a rigid
variable (POLY-7) is decided by DIM-7.

### KIND-4 A Nat is never negative
Intent: ruled 2026-10-30
Tests: spec-kind-4-1, spec-kind-4-2, spec-kind-4-3, spec-kind-4-4, spec-kind-4-5, spec-kind-4-6, spec-kind-4-7, spec-kind-4-8, spec-kind-4-9, spec-kind-4-10, spec-kind-4-11, spec-kind-4-12, spec-kind-4-14, spec-kind-4-15, spec-kind-4-16, spec-kind-4-17, spec-kind-4-18, spec-kind-4-19
Code: deviates (issues/kinds-dimensions.md)

A Nat variable is a natural, and so is the whole expression that fills a
Nat position of a type, though its parts need not be (KIND-3); an Int
variable is any integer: `Vector (n - w + 1)` requires `n + 1 >= w`, and
`Vector (n + k)` with `k@Int` requires `n + k >= 0`. A divisor is positive.
These conditions over a signature's Nat and Int variables are its domain. A signature is a type error where
it is written when it holds a ground expression that breaks them, or when no
value of its variables satisfies them all (`Vector (n - (n + 1))`); otherwise
it is accepted as written. Each use of a term must lie in the domain of its
signature, at the values the use gives its variables: a position that
reduces below zero, or a divisor that is not positive, is a type error at
the use. The body of a signed definition may assume its own domain (POLY-7). A
label whose value is known only at run time is checked then (DIM-3).

Why: a signed quantity is a separate kind, `Int`. A subtraction in a
signature states what its callers must satisfy, not something to hold for
every natural.

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
