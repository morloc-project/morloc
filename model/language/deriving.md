# Deriving

`deriving` clauses: instances the compiler builds from a declaration. Prefix: DERIV.

### DERIV-1 A deriving clause declares instances
Intent: ruled 2026-10-18
Tests: spec-deriv-1-1, spec-deriv-1-2, spec-deriv-1-3
Code: conforms 2026-10-18

`newtype N vs = T deriving (C, ...)`, `data D vs = ... deriving (C, ...)`
and `record R vs deriving (C, ...) where ...` declare an instance of each
class named for the declared type. The classes are `Eq`, `Ord`, `Show` and
`Hashable` (DERIV-2), and for a newtype also `Packable` (NEWT-8); any other
is an error, and so is a class that is not in scope where the type is
declared.

### DERIV-2 Derived instances act on the wire value
Intent: ruled 2026-10-18
Tests: spec-deriv-2-1, spec-deriv-2-2, spec-deriv-2-3, spec-deriv-2-4, spec-deriv-2-5
Code: conforms 2026-10-18

A derived `==` is `@eq`, `<=` is `@le` (INTR-7), `show` is `@show` and
`hash` is `@hash`, each on the value as it crosses between languages. The
instances of the type's components are not used, so the result is the same
in every language and needs no instance for the type's parameters.

### DERIV-3 The order of wire values
Intent: ruled 2026-10-18
Code: conforms 2026-10-18

Fields compare left to right; a `data` value by its constructor's
declaration order, then its fields; an absent optional before a present one;
lists lexicographically; strings by their UTF-8 bytes; integers by value;
floats by IEEE totalOrder. Two values are equal exactly when every float in
them has the same bits, so equal values have equal hashes. INTR-7's tests
exercise this order.

### DERIV-4 The wire form is compared as it is
Intent: ruled 2026-10-18
Tests: spec-deriv-4-1
Code: conforms 2026-10-18

Deriving compares the wire form whether or not it is canonical: two maps
with the same entries held in different orders are not equal under a
derived `==`. A type that needs the equality of a canonical form writes its
instance.

### DERIV-5 A type does not both derive and implement a class
Intent: ruled 2026-10-18
Tests: spec-deriv-5-1, spec-deriv-5-2
Code: conforms 2026-10-18

A deriving clause and a hand-written instance of the same class for the
same type are a compile error. A newtype's own `unpack` (NEWT-7) is not a
deriving clause.
