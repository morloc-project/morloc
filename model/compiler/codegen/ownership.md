# Ownership

How generated code turns each read of a named value into a borrow, a copy
or a move. Prefix: OWN.

A morloc variable may be read any number of times (EVAL-3). The generated
code of an owning language may not: there each read must borrow the value,
copy it, or move it. Choosing among these is an elaboration of an
unrestricted source term into an affine target term. Every point where the
source uses a value twice becomes an explicit copy, and every point where
it stops using one becomes an explicit release. This is the translation of
intuitionistic terms into linear ones, with contraction and weakening made
explicit. The target term means what the source term means only when a copy
and a release have no observable effect. EVAL-7 requires exactly that of
every native type, so any placement that keeps the target affine is correct,
and the items below choose one that copies less.

The items govern generated Rust. C++ is not yet held to them. Python and R
share values by reference, so a copy there is never made.

### OWN-1 No path reads a value after it is moved
Intent: ruled 2026-10-17
Tests: spec-own-1-1
Code: conforms 2026-10-17

In generated code, once a read moves a value, no later read on any path
through the same evaluation reads that value, unless a binding of the same
name has replaced it. A value that is never moved is released once it is
out of scope.

### OWN-2 A move happens only at a last read
Intent: ruled 2026-10-17
Tests: spec-own-2-1, spec-own-2-2, spec-own-2-3, spec-own-2-4, spec-own-2-5, spec-own-2-6
Code: conforms 2026-10-17

A read may move a value it owns only if no later read on its path reads
that value, and no other read in the same step does. A step is one let's
right-hand side, one result, or the values one loop iteration passes to the
next taken together. Reads within a step happen in an unspecified order
(EVAL-2), so a value read twice in one step is moved by neither read.

### OWN-3 A read that may run more than once never moves
Intent: ruled 2026-10-17
Tests: spec-own-3-1
Code: conforms 2026-10-17

A read inside a lambda, a nested loop, a separate manifold or a `do` block
counts as unboundedly many reads. Such a read never moves, and no earlier
read of the same value moves either. In linear logic this is the promotion
rule: a term that may be duplicated captures only unrestricted values.

### OWN-4 A loop's guard borrows
Intent: ruled 2026-10-17
Tests: spec-own-4-1
Code: conforms 2026-10-17

A read in a condition a loop's iteration branches on, including a test of
a sum type's tag, borrows its value and never moves it, since the chosen
branch may still read it.

### OWN-5 Projecting from a moved value moves the field
Intent: ruled 2026-10-17
Tests: spec-own-5-1, spec-own-5-2
Code: conforms 2026-10-17

When a constructor field or a record field is read from a value at that
value's last read, the field is moved out instead of copied, and the rest
of the value is released. This holds for a native type with its own
destructor, as EVAL-7 allows: the value's own destructor does not run, and
each field is released as its own value. A Rust struct mapped to a record
therefore holds exactly the record's fields; a program whose struct holds
another fails to compile where a field is taken, rather than leak it. A
field a getter names twice, or names along with a part of it, is copied
for all but one of those reads.

### OWN-6 A loop that only walks what it was lent borrows it
Intent: ruled 2026-10-17
Tests: spec-own-6-1, spec-own-6-2, spec-own-6-3, spec-own-6-4, spec-own-6-5, spec-own-6-6, spec-own-6-7, spec-own-6-8, spec-own-6-9
Code: conforms 2026-10-17

A value a tail loop carries is a borrow, not a copy, when it starts from a
value the loop's manifold borrows and every iteration replaces it with that
value itself, a constructor field or a record field of a borrow. A loop let
bound to such a part is a borrow too, unless its index is bound twice or
its type is one a copy of costs nothing. Walking a borrowed structure then
copies none of it, however deep it is. A carried value that some iteration
replaces with a value it builds is owned, and starts from a copy.

### OWN-7 A function that keeps an argument takes it from its caller
Intent: ruled 2026-10-17
Tests: spec-own-7-1
Code: conforms 2026-10-17

A fully applied manifold takes a parameter by value instead of by
reference when its body reads that parameter once, on every evaluation, and
keeps what it reads: as the start of a loop value that is not a borrow
(OWN-6), or as an element of a value it builds. A parameter kept on only
one branch stays a reference, so the branch that drops it copies nothing. Each call then moves the argument when the caller
reads it for the last time, and copies it otherwise, so a copy happens at
most once per call and only where the caller still needs the value. A
closure keeps taking its parameters by reference, since its signature is
shared by every caller.
