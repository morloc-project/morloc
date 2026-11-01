# Polymorphism

Quantifiers, instantiation, generalization, scope of type variables. Prefix: POLY.

### POLY-1 Type variables in a signature are universally quantified
Intent: proposed
Code: deviates (issues/typecheck.md)

A lowercase name in a type signature is a type variable, quantified over
the whole signature. Every use of the term may instantiate it differently.

### POLY-2 All polymorphism is resolved before the program runs
Intent: proposed
Code: conforms 2026-10-21

Every type variable in a realized program is instantiated to a concrete
type at compile time. No value crosses a language boundary at a type that
still contains a variable.

### POLY-3 A variable's kind comes from its declaration or its label
Intent: proposed
Code: conforms 2026-10-21

An unannotated type parameter has kind `Type`. `(n :: K)` in a type
declaration gives kind `K`. In a signature, `x@Int` binds an `Int`, `x@Str`
a `Str` and `x@[Str]` a `List` to the value of that argument. A label
labels a whole argument; elsewhere in a type it is an error, except in a
newtype's representation (DIM-9).

### POLY-4 Type variables in a local signature
Intent: ruled 2026-10-30
Tests: spec-poly-4-1, spec-poly-4-2, spec-poly-4-3, spec-poly-4-4
Code: deviates (issues/kinds-dimensions.md)

A variable in a `where` signature, of any kind (type, Nat, Int, effect), is
quantified over that signature alone, even where an enclosing signature
writes the same name, as in Haskell. Each use of the local definition
instantiates its signature afresh, constraints included. Checking a body
against its signature (POLY-7), a local signature's variables are rigid in
its own body and its constraints are given to it. This item does not cover
an annotation in a body (`(x :: a)`), which still means the enclosing
signature's variable.

### POLY-5 Generalization of local definitions
Intent: open
Code: unaudited

Is a local definition without a signature (a) generalized, so that each use
may instantiate it differently, or (b) monomorphic, with one type fixed by
its uses?

### POLY-6 An undetermined type variable is a located error
Intent: ruled 2026-10-09
Tests: spec-poly-6-1, spec-poly-6-2, spec-poly-6-3, spec-poly-6-4, spec-poly-6-5, spec-poly-6-6
Code: conforms 2026-10-16

A type variable that nothing in the program determines, and that a native
representation needs (POLY-2), is rejected at the term where it occurs, and
the error names the variable. One that no representation needs, such as a
phantom parameter of an argument-free `data` type, is not an error.

### POLY-7 Nat and Int variables in a signature are rigid
Intent: ruled 2026-10-29
Tests: spec-poly-7-1, spec-poly-7-2, spec-poly-7-3, spec-poly-7-4, spec-poly-7-5, spec-poly-7-6, spec-poly-7-7, spec-poly-7-8, spec-poly-7-9, spec-poly-7-10, spec-poly-7-11, spec-poly-7-12, spec-poly-7-13, spec-poly-7-14, spec-poly-7-15, spec-poly-7-16, spec-poly-7-18, spec-poly-7-19, spec-poly-7-20
Code: deviates (issues/kinds-dimensions.md)

A `Nat` or `Int` variable in a signature is universally quantified, as a
`Type` variable is (POLY-1), over the signature's domain (KIND-4). The body must
typecheck for every value of it there: `f :: Vector n Int -> Int` may not
pass its argument where `Vector (m + 1) Int` is required, while `f :: Vector
n Int -> Vector (n - 1) Int` may, since its domain has `n >= 1`. A label
(`w@Int`) names such a variable whether or not it occurs elsewhere in the
type, and a body that passes the labeled argument to another label passes
that variable. A label on an unsigned type (`UInt`, `U8` to `U64`) adds
its variable's non-negativity to the domain. A Nat that only the
signature's result has, and that no label names, is one the function
chooses, not one it is given: it is not rigid in the body, and at each call
it is observed (DIM-10), so `sliceRows :: start@Int -> end@Int -> Table n r
-> Table m r` returns a length no equation may fix. Every Nat position in the
body's types must be non-negative there too. These conditions are decided by DIM-7.
