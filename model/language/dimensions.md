# Dimensions

Nat-indexed types (vectors, tensors), dimension arithmetic, runtime checks. Prefix: DIM.

### DIM-1 A Nat parameter is a compile-time number
Intent: proposed
Code: conforms 2026-10-21

A `(n :: Nat)` parameter is part of the type and is erased before anything
runs. `n@Int` declares an `Int` argument and binds the Int `n` (KIND-1) to
its value.

### DIM-2 Dimensions are checked statically
Intent: proposed
Code: conforms 2026-10-21

`+`, `-`, `*` and `/` (integer division) on Nats are evaluated when their
operands are ground. Two dimensions that reduce to different numbers in one
slot are a type error. A check on an inferred variable waits until it is
solved; one on a rigid variable (POLY-7) is decided by DIM-7.

### DIM-3 A dimension the compiler cannot see is checked at run time
Intent: ruled 2026-10-30
Tests: spec-dim-3-1, spec-dim-3-2
Code: deviates (issues/kinds-dimensions.md)

Where a value whose length the compiler cannot see flows into a slot whose
Nat is concrete or shared with another slot, its length is checked against
the slot at run time, at one of two levels:

- **Data**: a value from outside the program (a command's input, a file
  it reads, a label value given on the command line). A mismatch is bad
  data, reported as an error in the input. These checks always run.
- **Code**: a value one part of the program hands another (a sourced
  function's result, a value passed between pools or between calls). A
  mismatch is a defect in the program. These checks are off by default and
  run in a debug build (`morloc make --debug`).

A check the compiler cannot place is a type error at the term that needs
it (DIM-7).

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
Intent: ruled 2026-10-30
Tests: spec-dim-7-1, spec-dim-7-2, spec-dim-7-3, spec-dim-7-4, spec-dim-7-5, spec-dim-7-6
Code: deviates (issues/kinds-dimensions.md)

Every Nat equation and every non-negativity condition (KIND-4) is decided
by one procedure, over the integer arithmetic of KIND-3, with every Nat
variable a natural and every Int variable an integer, decided as the
difference of two naturals. A condition holds when, for every value of its rigid variables
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
holds, when it exceeds a small multiple of one assumption, or the sum of
two, by a polynomial with non-negative coefficients, or when it is proven
by cases on small values of a variable and the rest shifted past them. A
group over a single variable is decided exactly: linear conditions by the
interval they allow, polynomial ones by the values up to the bound past
which no condition changes sign. Any other group holds when a
small set of candidate witnesses proves it, and is refuted by a bounded
search for a counterexample inside the assumptions. Unknown is not an
error: the condition is checked at run time at the term that produced it
(DIM-3), and the compiler warns, naming the condition and the term. A
condition with a variable that has no value at run time (one that only an
equation defines, such as `m` in `Vector (m * m)`) cannot be checked then,
and an Unknown one is a type error at that term.

### DIM-8 A Nat is a length only through Vector or a Table's rows
Intent: proposed
Tests: spec-dim-8-1
Code: deviates (issues/kinds-dimensions.md)

The first parameter of `Vector n a` is the length of its list, and the
`n` of `Table n r` is its row count. A Nat parameter of any other type is
a length only where its representation passes it to one of these (`Matrix
m n a`, whose wire form holds `Vector (m * n) a`); otherwise it is a
phantom (NEWT-3), and no run-time check reads it. Every DIM-3 check of a
length follows this rule. A header label (DIM-9) also gives a run-time
value to the Nat it names.

### DIM-9 A label in a newtype's representation names a header
Intent: ruled 2026-10-31
Tests: spec-dim-9-1, spec-dim-9-2, spec-dim-9-3, spec-dim-9-4, spec-dim-9-5, spec-dim-9-6, spec-dim-9-7, spec-dim-9-8
Code: conforms 2026-10-31

In `newtype N vs = T`, a label `v@I` on an integer type `I`, at a position
of `T` reached through tuple fields alone, says that the integer there is
the value of the Nat parameter `v`. `newtype Matrix (m :: Nat) (n :: Nat)
a = ((m@Int, n@Int), Vector (m * n) a)` ties its header to its dimensions:
the DIM-3 checks read `m` and `n` from the header, check each against the
type and against every other slot that shares it, require each to be
non-negative (KIND-4), and check the data's length against `m * n`. The
label adds nothing else: `T` with its labels removed is the
representation (NEWT-9). A label anywhere else inside a type is an error:
one under a list, optional or record, one naming no `(v :: Nat)` parameter, one on
a type other than `Int`, `I8` to `I64`, `UInt` and `U8` to `U64`, two
naming the same parameter, and one inside a signature's argument rather
than on the whole argument (POLY-3).
