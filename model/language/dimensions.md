# Dimensions

Nat-indexed types (vectors, tensors), dimension arithmetic, runtime checks. Prefix: DIM.

### DIM-1 A Nat parameter is a compile-time number
Intent: proposed
Code: unaudited

A `(n :: Nat)` parameter is part of the type and is erased before anything
runs. `n@Int` declares an `Int` argument and binds the Nat `n` to its value.

### DIM-2 Dimensions are checked statically
Intent: proposed
Code: unaudited

`+`, `-`, `*` and `/` (integer division) on Nats are evaluated when their
operands are ground. Two dimensions that reduce to different numbers in one
slot are a type error. A check on a free variable waits until it is solved.

### DIM-3 A dimension the compiler cannot see is checked at run time
Intent: ruled 2026-09-26
Code: deviates (unfiled: Nat dims unchecked at runtime)

Where a value whose length is not known statically (returned by sourced
code, or read from input) flows into a slot whose Nat is concrete or shared
with another slot, its length is checked against the slot at run time, and
a mismatch is a failure.

### DIM-4 Omitted Nat arguments are gradual
Intent: proposed
Code: unaudited

A type constructor may be given fewer Nat arguments than it declares. The
ones given fill the leading Nat positions, and the rest are unknown
dimensions. A concrete dimension flows into an unknown one. Type-kinded arguments are
never omitted.

### DIM-5 An unknown dimension flowing into a known one
Intent: open
Code: unaudited

DIM-4 lets `Vector 3 a` flow into `Vector a`. May `Vector a` flow into
`Vector 3 a`? Candidates: (a) no, a type error; (b) yes, with the DIM-3
runtime check at that point.
