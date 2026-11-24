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

The items govern generated Rust and C++. Where either copies more than
they allow, the copy is recorded in compiler/issues/codegen-copies.md. Python and R share values by reference,
so a copy there is never made.

### OWN-1 No path reads a value after it is moved
Intent: ruled 2026-10-17
Tests: spec-own-1-1, spec-own-1-2, spec-own-1-3, spec-own-1-4, spec-own-1-5, spec-own-1-6, spec-own-1-7, spec-own-1-8, spec-own-1-9, spec-own-1-10, spec-own-1-11, spec-own-1-12, spec-own-1-13, spec-own-1-14, spec-own-1-15, spec-own-1-16, spec-own-1-17, spec-own-1-18, spec-own-1-19, spec-own-1-20, spec-own-1-21, spec-own-1-22, spec-own-1-23, spec-own-1-24
Code: conforms 2026-10-17

In generated code, once a read moves a value, no later read on any path
through the same evaluation reads that value, unless a binding of the same
name has replaced it. A value that is never moved is released once it is
out of scope.

### OWN-2 A move happens only at a last read
Intent: ruled 2026-10-17
Tests: spec-own-2-1, spec-own-2-2, spec-own-2-3, spec-own-2-4, spec-own-2-5, spec-own-2-6, spec-own-2-7, spec-own-2-8, spec-own-2-9, spec-own-2-10, spec-own-2-11, spec-own-2-12, spec-own-2-13, spec-own-2-14, spec-own-2-15, spec-own-2-16, spec-own-2-17, spec-own-2-18, spec-own-2-19
Code: conforms 2026-10-18

A read may move a value it owns only if no later read on its path reads
that value, and no other read in the same step does. A step is a
conditional's test, the value a body ends in, or the values one loop
iteration passes to the next taken together. A let's right-hand side is a
body of its own, and each arm of a conditional is a path of its own, so a
let bound to a conditional splits into the test and one body per arm. The
lets a loop's next values or its result depend on run before them, one
value after another, so each is a step of its own. Reads within a step happen in an unspecified order
(EVAL-2), so a value read twice in one step is moved by neither read. A
read of a local that refers into a value, directly or through other such
locals, is also a read of that value.

The parts of one expression -- the arguments of a call, the elements of a
value it builds -- that are not single locals run as lets of their own,
in an order the compiler chooses (EVAL-2). A part that keeps a value
another part only reads runs after it, so the keeping read is the last
read and may move the value: in `keyp c + .z (holdP c)` the read by `keyp`
comes first and `holdP` takes `c`. A part that names a local which refers
into the value counts as reading it, so no reference is read after the
value moves. Parts that call into another language, or that force a
suspension, are not reordered. Amended 2026-11-21 (tentative).

### OWN-3 A read that may run more than once never moves
Intent: ruled 2026-10-18
Tests: spec-own-3-1, spec-own-3-2, spec-own-3-3, spec-own-3-4, spec-own-3-5
Code: conforms 2026-11-09

A read inside a lambda, a nested loop, a separate manifold or a `do` block
counts as unboundedly many reads. Such a read never moves, and no earlier
read of the same value moves either. A lambda or separate manifold that
holds its own copy of each value it captures reads each of them once, when
it is built; that read belongs to the step that builds it and may move like
any other read there (OWN-2), while the reads inside read the copy and never
move it. A block that refers to a value instead of holding a copy reads it
for as long as it lives: a `do` block bound by a let may hold a reference
to what it reads for as long as the let lives, so no read of a value such a
block reads moves, later ones included. A value a `do` block binds is
a new value each time the block runs; its reads within the block may move
under OWN-2 and this item as any value's may. So is a closure's own
bound parameter in the closure's by-value form (OWN-7): each call is
given a new one. A wrapper lambda a generated function runs once (a fork
guard, a log template's timing) is not a repeated read. In linear logic this is the
promotion rule: a term that may be duplicated captures only unrestricted
values.

### OWN-4 A loop's guard borrows
Intent: ruled 2026-10-17
Tests: spec-own-4-1
Code: conforms 2026-10-17

A read in a condition a loop's iteration branches on, including a test of
a sum type's tag, borrows its value and never moves it, since the chosen
branch may still read it.

### OWN-5 Projecting from a moved value moves the field
Intent: ruled 2026-10-17
Tests: spec-own-5-1, spec-own-5-2, spec-own-5-3, spec-own-5-4
Code: conforms 2026-10-17

When a constructor field, a record field or a tuple field is read from a
value at that value's last read, the field is moved out instead of copied,
and the rest of the value is released. So is a part read from a part moved out this
way, and in Rust a list element read from a list at its last read; in C++
a constructor field of such an element is taken from its arm in place,
and another field of the value a field was taken from may still be read. This holds for a native type with its own
destructor, as EVAL-7 allows: the value's own destructor does not run, and
each field is released as its own value. A Rust struct mapped to a record
therefore holds exactly the record's fields; a program whose struct holds
another fails to compile where a field is taken, rather than leak it. A
field a getter names twice, or names along with a part of it, is copied
for all but one of those reads.

### OWN-6 A loop that only walks what it was lent borrows it
Intent: ruled 2026-10-17
Tests: spec-own-6-1, spec-own-6-2, spec-own-6-3, spec-own-6-4, spec-own-6-5, spec-own-6-6, spec-own-6-7, spec-own-6-8, spec-own-6-9, spec-own-6-10, spec-own-6-11, spec-own-6-12, spec-own-6-13, spec-own-6-14, spec-own-6-15, spec-own-6-16, spec-own-6-17, spec-own-6-18, spec-own-6-19
Code: conforms 2026-10-17

A value a tail loop carries is a borrow, not a copy, when it starts from a
value the loop's manifold borrows and every iteration replaces it with that
value itself, a constructor field, a record field or a list element of a
borrow or of such a part (list elements ruled 2026-10-19), or a `match`
against constructors of such parts whose every arm is such a part or the
failure when no arm matches. A let, in a loop or
not, bound to such a part is a borrow too, unless its index is bound twice
or its type is one a copy of costs nothing. A constructor field read from a
value that is not taken there is read in place, and copied only where a
value is kept. Walking a borrowed structure then
copies none of it, however deep it is. A carried value that some iteration
replaces with a value it builds is owned, and starts from a copy.

### OWN-7 A function that keeps an argument takes it from its caller
Intent: ruled 2026-11-21
Why: amended from one form, where a parameter kept on one branch stayed a reference
Tests: spec-own-7-1, spec-own-7-2, spec-own-7-3, spec-own-7-4, spec-own-7-5
Code: conforms 2026-11-23

A fully applied manifold takes a parameter by value instead of by
reference when its body reads that parameter once, on every evaluation, and
keeps what it reads: as the start of a loop value that is not a borrow
(OWN-6), as an element of a value it builds, or as an argument a fully
applied manifold it calls takes by value, so the mode carries up a chain of
calls. Each call then moves the argument when the caller reads it for the
last time, and copies it otherwise, so a copy happens at most once per call
and only where the caller still needs the value.

A parameter the body keeps, or keeps a part of, on only some evaluations
has two forms of the manifold. The first takes it by reference, and copies
what a branch keeps. The second takes it by value: OWN-2 and OWN-5 apply to
it as to a local the manifold owns, so it is moved where it is kept and
released where it is not. A call uses the second form when it may move
every such argument, being a temporary or a value it reads for the last
time (OWN-2, OWN-3), and the first otherwise, so a branch that keeps
nothing copies nothing, and a copy happens only where the caller still
needs its value. In C++ the forms are overloads of one name, `const T&` and
`T&&`, so the call's arguments select the form; the second moves each such
parameter into a local when it starts, so what it does not keep is
released when it returns. A scalar or function-valued parameter has one
form. A field of a C++ `data` arm is taken only from a value the manifold
the take is in owns, and only as CPP-6 allows. A closure's first form takes
its parameters by reference, since its signature is shared by every caller;
the step of a fold that captures nothing is the exception, taking its
accumulator by value (COPY-7).

In C++ (amended 2026-11-23, ruled 2026-11-24), passing a parameter whole to a
sourced call that is not an operator also keeps it, for the second form
only: a caller that lends it still calls the first form and copies
nothing, and the second gives it to the sourced function, whose `T&&`
overload may take it (CPP-1). A closure has a second form too, at the bound
parameters it keeps, never at the values it captures. Its callable forwards
what it is called with, so a call with an rvalue selects the second form; a
let holding it as a `std::function` holds it as an `mlc::owning`, which
`mlc::give` reaches through any copy (CPP-7). Since a call selects the
second form only when it may move every argument the form takes by value,
a position at which every call passes a parameter its caller takes by
reference in every form (a captured value, or one it neither keeps nor
gives on) is not one of the form's. The analyses of OWN-5, OWN-8 and OWN-10
see only the second forms of the paragraphs above.

### OWN-8 A function that returns what it was lent returns it lent
Intent: open
Code: conforms 2026-11-22

A fully applied manifold that is never a function value and is not called
from inside itself except by its own tail loop may return from its
by-reference form a result lent from parameters it takes by reference and
may keep (OWN-7), or owned: in Rust a `Cow` whose lifetime is those
parameters' alone, in C++ an `mlc::lent_or` from both of its forms. Its
body is one of:
- that tail loop, some base leaf of which returns one such parameter as
  the loop was lent it, in a slot no iteration changes, or a part of it the
  loop reads in place (OWN-6) that is not optional;
- past the lets it binds, a conditional whose arms are, as for an OWN-10
  conditional, such conditionals, such parameters, parts of them, or values
  of their own, nothing in it keeping such a parameter or a part of one.
Only one parameter of a loop may be returned lent; a conditional's arms
may lend from several. An arm lends only where the form takes the
parameter by reference, so the by-value form of C++ returns its arms
owned. Every other leaf's value is owned, as it was.

A call reads the result where it lies when it is the right-hand side of a
let bound once, its argument at each such parameter is a named local, and
the let is only read: by a call that takes it by reference, a test, an
intrinsic that only reads, or a projection whose value is cheap to copy or
is bound by reference and only read in turn. The let is never kept (built
into a value, returned, a loop's start or next value, an argument a
function takes or may take by value), captured or bound by another let.
Such a let is a read of every local it may be lent from wherever it is
read (OWN-2), so no such local moves while the let is read, a part of the
let is a reference, not a take (OWN-5), and no part of such a local is
taken in the manifold the let is in. Any other call takes the result
owned, copying a lent result only, as the by-reference form copied it
before. MORLOC_NO_MOVES builds without this form.

Open: a non-tail recursion passing a lent result on; a call that passes
another such function's lent result on; and a sourced reader that takes
its parameter by value, which copies a result it could have taken when
the result was owned.

### OWN-9 A dead `data` arm is reused for a constructor of the same shape
Intent: open
Code: unaudited

When a manifold reads the last of a `data` value it holds alone and builds
a constructor whose arm has the same layout, the new arm is written into
the old one instead of freeing it and allocating anew (reset and reuse).
It pays for persistent structures updated in place -- a chain rebuilt link
by link, a tree insert, a syntax tree rewrite -- saving an allocation and a
free per node, and only when the value is unshared at run time; otherwise
the arm is allocated as today. Lists are not `data` arms and gain nothing.
It needs OWN-5's run-time sharing test. Writing into an arm is sound only
for an arm morloc allocated, so either the arms morloc allocates are
tagged, or sourced code is ruled never to build a const arm nor keep a weak
reference to one (CPP-6 rules out the weak reference).

### OWN-10 A value a conditional chooses is read where it lies
Intent: proposed
Tests: spec-own-10-1, spec-own-10-2
Code: conforms 2026-11-22

A let bound to a conditional that chooses between a value the frame still
holds, or a part of one, and another value is not copied for a reader that
only reads it. The pool tree is rewritten before code generation, in the
same language as the let and the conditional:
- a let of a conditional between record or tuple literals, read only one
  field at a time, binds each field it reads to a conditional of its own
  (the condition computed once), and no record is built; a field nothing
  reads is dropped only where both arms hold a value for it;
- a small reader of the let (at most 48 nodes, no function value or
  suspension) is copied into each arm, so the arm that names a held value
  reads it in place; a reader that also names that value runs its one read
  of the let first (EVAL-2), and is otherwise left as it is;
- a read of a let that refers into a value, written after a let that keeps
  the value, runs before that let (OWN-2).
EVAL-3 holds: each copy of the reader runs on its own path.

### OWN-11 A constructor built only to be matched is never built
Intent: proposed
Tests: spec-own-11-1
Code: conforms 2026-11-22

A constructor literal that a match reads gives no value two owners. The
pool tree is rewritten before code generation, in the language of the
literal, the match and the helper:
- a tag test or field read of a constructor literal, or of a let bound to
  one (directly or through another let naming it), is the answer it must
  give, and a conditional on a known answer is the arm it chooses. The
  fields of a let-bound literal that are not variables or scalar literals
  are bound to lets of their own where the literal was (EVAL-1, EVAL-3), so
  each is computed once and a field nothing reads is computed as it was
  when the literal was built (EVAL-5 is open); the literal's let goes once
  nothing reads it. A guard of the chosen arm
  runs; a nested pattern the literal does not settle keeps its tag test;
  a match on a let read otherwise has only its tag tests answered;
- a call of a self-recursive helper that passes such a literal at a
  parameter the helper tag-tests calls a copy of the helper taking the
  literal's fields in that parameter's place, with the literal reduced in
  its body as above: one step, since the copy calls the helper. The copy
  keeps the helper's name and definition in a failure's trace (DIAG-5),
  and is not made where it would still hold the literal. No copy is made
  for a labeled call, a helper holding a label, a staged helper, a
  mutually recursive one, a call in the helper itself, or a helper of
  more than 64 nodes, and at most 16 per helper.
Effects are untouched: a suspension is not split and a forced effect is
never moved.

A let bound once to a conditional these rewrites leave, and only read as
OWN-8 lists, holds the value the taken arm chose where it lies, or the
arm's own value: in C++ an `mlc::lent_or`, in Rust a `Cow`. Each arm,
past the lets it binds, is such a conditional, a part of a local bound
outside the conditional (a projection that has a value of its own type in
memory, OWN-6, and is not optional, through lets bound to such parts), a
local itself, or a value of its own. An arm that is a local itself lends
it only where the local is named after the let, in the same step, or in a
body that may run again; elsewhere it owns it, as a move there copies
nothing. The arms may lend from more than one local. Nothing in the
conditional keeps such a local or a part of one, no let in it takes a
payload (OWN-5), and the let's type is not one a copy of costs nothing (a
C++ scalar, CPP-1), a function or an optional. An arm lends a part only
where the member binds it as a reference into the local, in C++ through
`mlc::place`; any other arm is owned. Besides the reads OWN-8 lists, the
let may be the argument an OWN-8 manifold may lend its result from, when
every such call is bound by a let that reads the result where it lies.
The let is a read of every such local wherever it is read (OWN-2), so in
the manifold the let is in no part of one is taken (OWN-5), as for an
OWN-8 let, and in Rust a part of one is bound by reference. A let that
binds such a conditional only to return it is that conditional.
MORLOC_NO_MOVES builds without this form.

## Copies these items leave

A copy remains where two owners of one value are alive at once and no
order of the pure work separates them, or where a ruling requires it:
- A value kept while the caller still needs it, by a function that is not
  inlined and builds a record or list holding it (golden own7-matrix rec
  ctor/call/list cases), or returns it or a part of it where OWN-8 does not
  apply.
- A list built from a held value for a sourced function such as `map`: the
  list owns its elements, and of a sourced `map` the compiler assumes only
  `map id == id` (NEWT-10), not `map g [x] == [g x]`.
- A callback's argument a callback keeps: callbacks take their arguments
  by reference (HOST-8), and a Rust boxed callback owns what it captures.
- A value kept twice in one step, or by a closure run more than once
  (OWN-3): one copy per extra owner.
- A user-mapped type whose copy is deep: the user chose a representation
  that is not shared (a generated `data` type shares its arms).
Every other copy is recorded in compiler/issues/codegen-copies.md.
