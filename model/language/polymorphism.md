# Polymorphism

Quantifiers, instantiation, generalization, scope of type variables. Prefix: POLY.

### POLY-1 Type variables in a signature are universally quantified
Intent: ruled 2026-11-07
Tests: spec-poly-1-1, spec-poly-1-2, spec-poly-1-3, spec-poly-1-4, spec-poly-1-5, spec-poly-1-6, spec-poly-1-7, spec-poly-1-8, spec-poly-1-9, spec-poly-1-10
Code: deviates (issues/typecheck.md)

A lowercase name in a type signature is a type variable, quantified over
the whole signature. Every use of the term may instantiate it differently.
The body must hold for every value of each variable. This holds for
variables of every kind: a `Str`, `Rec`, `List` or `Set` variable is never
fixed by the body, even one that occurs only in the result, so
`mvp :: n@U64 -> Table n r` may not return a table whose row the body
chooses; the signature names the row. Nat and Int variables follow POLY-7.

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
labels a whole argument, and a parameter of a parenthesized last group is a
whole argument (APP-1): `Int -> (n@Int -> V n)` is `Int -> n@Int -> V n`.
A signature labels each name once. Elsewhere in a type, on the result, in
a constraint or under a wrapper (`?n@Int`, `<IO> n@Int`), a label is an
error, except in a newtype's representation (DIM-9).

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

### POLY-5 A closed local definition is generalized; an open one has one type
Intent: ruled 2026-11-08
Tests: spec-poly-5-1, spec-poly-5-2
Code: deviates (compiler/issues/specialization.md)

A local (`where`, `let`) definition without a signature is generalized, as
a top-level one is (POLY-8), if it is closed: each free variable of its body
is a top-level or imported term, or a closed local. One that mentions a
variable bound by an enclosing lambda or pattern, or an open local, has one
type, fixed by its uses. This is GHC's MonoLocalBinds.

```morloc
pairs n s = (dup n, dup s)
  where
    dup y = [y, y]            -- closed: used at Int and at Str

scale k xs = map step xs
  where
    step y = y * k            -- open: one type, fixed by its use

tagBoth t p = (tag (.0 p), tag (.1 p))
  where
    tag :: a -> (Str, a)      -- open and used at two types: needs a signature
    tag y = (t, y)
```

A length the result of an open local chooses (DIM-10) is still chosen at
each application: one type for the local does not make its calls return one
length.

Why: generalizing an open local means splitting its constraints and its
kind-level variables between it and the enclosing definition; such locals
are rarely used at two types, and a local signature (POLY-4) covers those.

### POLY-8 An unsigned top-level definition is generalized
Intent: ruled 2026-11-08
Tests: spec-poly-8-1, spec-poly-8-2, spec-poly-8-3, spec-poly-8-4, spec-poly-8-5, spec-poly-8-6, spec-poly-8-7, spec-poly-8-8
Code: deviates (compiler/issues/specialization.md)

A top-level definition without a signature is given the most general type
its body has, its constraints included, and each use instantiates it, with
or without arguments: `showAll = map show` is `Show a => [a] -> [Str]`. There
is no monomorphism restriction. Definitions that refer to each other without
signatures are inferred together; a signed one is referred to by its
signature.

Why: morloc instantiates a term once per type it is used at, so a
generalized constant is computed once per type (EVAL-3), not once per use as
under dictionary passing, the monomorphism restriction's reason.

### POLY-9 A definition over an open tuple or record is a pattern
Intent: ruled 2026-11-08
Tests: spec-poly-9-1, spec-poly-9-2, spec-poly-9-3, spec-poly-9-4, spec-poly-9-5
Code: conforms 2026-11-08

An unsigned definition whose type keeps a tuple or record whose other
elements or fields are unknown is a pattern, such as `getFst = .0`,
`setX v r = .(.x = v) r` or `add10Fst = (+) 10 . .0`. No signature can
state its type. It is checked once, where it is defined, as SIG-10 asks
of every definition. It is then typed at each use, at the shape that use
gives, in its own module or one that imports it, so `getFst (1, "a")` and
`getFst ("b", True, 3)` do not constrain each other. A tuple or record whose
elements or fields are all known, such as the type of
`.(.x = 99) { x = 0, y = "hi" }`, is not a pattern.

A pattern is not a command. Naming one defined in the root module among
the root module's exports is an error. A pattern the root module imports
and exports is skipped, as a generic export is (SIG-5).

### POLY-6 An undetermined type variable is a located error
Intent: ruled 2026-10-09
Tests: spec-poly-6-1, spec-poly-6-2, spec-poly-6-3, spec-poly-6-4, spec-poly-6-5, spec-poly-6-6, spec-poly-6-7
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
its variable's non-negativity to the domain. A Nat that occurs only
outward in the signature (DIM-10), and that no label names, is one the
function chooses, not one it is given: it is not rigid in the body, and at
each call it is observed, so `sliceRows :: start@Int -> end@Int -> Table n r
-> Table m r` returns a length no equation may fix. Every Nat position in the
body's types must be non-negative there too. These conditions are decided by DIM-7.
