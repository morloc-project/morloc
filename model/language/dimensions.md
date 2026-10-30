# Dimensions

Nat-indexed types (vectors, tensors), dimension arithmetic, runtime checks. Prefix: DIM.

### DIM-1 A Nat parameter is a compile-time number
Intent: proposed
Code: conforms 2026-10-21

A `(n :: Nat)` parameter is part of the type and is erased before anything
runs. `n@Int` declares an `Int` argument and binds the Nat `n` to its value.

### DIM-2 Dimensions are checked statically
Intent: proposed
Code: conforms 2026-10-21

`+`, `-`, `*` and `/` (integer division) on Nats are evaluated when their
operands are ground. Two dimensions that reduce to different numbers in one
slot are a type error. A check on an inferred variable waits until it is
solved; one on a rigid variable (POLY-7) is decided by DIM-7.

### DIM-3 A dimension the compiler cannot see is checked at run time
Intent: ruled 2026-09-26
Code: deviates (issues/kinds-dimensions.md)

Where a value whose length is not known statically (returned by sourced
code, or read from input) flows into a slot whose Nat is concrete or shared
with another slot, its length is checked against the slot at run time, and
a mismatch is a failure.

### DIM-4 Omitted Nat arguments are gradual
Intent: proposed
Code: conforms 2026-10-21

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

### DIM-6 An open Nat equation is accepted when it has a solution
Intent: ruled 2026-10-09
Tests: spec-dim-6-1, spec-dim-6-2, spec-dim-6-3, spec-dim-6-4, spec-dim-6-5, spec-dim-6-6
Code: conforms 2026-10-09

A Nat equation still open when inference ends, such as `n + m = 4` with
`n` and `m` otherwise free, is accepted when it has a solution in the
naturals: dimensions are erased before anything runs (KIND-2), so the free
variables need no value (POLY-6). An equation with no solution, or one the
compiler cannot decide, is a type error at the term that produced it.

### DIM-7 One procedure decides Nat equations
Intent: ruled 2026-10-29
Tests: spec-dim-7-1, spec-dim-7-2, spec-dim-7-3
Code: deviates (issues/kinds-dimensions.md)

Every Nat equation and every non-negativity condition (KIND-4) is decided
by one procedure, over the integer arithmetic of KIND-3 with every variable
a natural. A condition holds when, for every value of its rigid variables
(POLY-7) that satisfies the assumptions (a signature's domain, KIND-4), some
value of its inferred variables satisfies it (DIM-6); with no inferred
variables this is validity, with no rigid ones satisfiability. A division
adds the condition that its divisor is positive. The procedure answers
holds, refuted (with a value of the rigid variables, inside the
assumptions, that has no solution), or unknown. An inferred variable that an equation defines over the others, as
written (`v = e`) or by its normal form, is eliminated first; the conditions left are split into groups that share no
variable, each decided on its own. A group is decided exactly when it has
no variables; when it has no assumptions and is one condition over rigid
variables without division (a linear inequality by its coefficients, an
equation by polynomial identity with `*`, up to a bound on its degree and
variable count); and when it is one linear equation or inequality over
inferred variables without division. A condition also holds when it is an
assumption, when it is a quotient's non-negativity and its dividend's
holds, or when it exceeds a small multiple of one assumption, or the sum of
two, by a polynomial with non-negative coefficients. Any other group holds when a
small set of candidate witnesses proves it, and is refuted by a bounded
search for a counterexample inside the assumptions. Unknown is a type error at the term
that produced the condition, naming it.
