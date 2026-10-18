# Evaluation

Evaluation order, strictness, sharing of named values, and what may be reordered. Prefix: EVAL.

## Strictness

### EVAL-1 Pure application is strict
Intent: proposed
Code: unaudited

A pure argument expression is evaluated once, at the application, however
many times the function uses it. A suspension argument is passed as the
suspension, not run (see `EFF`).

### EVAL-2 Pure work may be reordered; only effects are ordered
Intent: ruled 2026-10-18
Code: unaudited

The compiler chooses the order of pure work. It may move, parallelize or
reorder it, including the arguments of one call, to make it faster or to
let a later read take a value an earlier one only looked at. Only effect
statements in a `do` block keep their written order, and none is shared,
duplicated or dropped (see `EFF`). When a pure abort happens, and which of
two pure aborts is reported, is unspecified.

## Named values

### EVAL-3 A named value is computed at most once per evaluation of its scope
Intent: proposed
Code: unaudited

A `where` or `let` binding, a pure `let` in a `do` block, or a top-level
constant is computed at most once each time its scope is evaluated, at the
nearest point every use passes through. A top-level value is computed at
most once per command. A value read by several languages is computed once
and passed to each.

### EVAL-4 Placement respects `@try` regions and effect barriers
Intent: proposed
Code: unaudited

A value read only inside a `@try` argument is computed inside it, so its
failure is that `@try`'s `Err`. A pure `let` in a `do` block is never
computed before an effect statement written above it.

### EVAL-5 Whether an unused pure `let` is computed
Intent: open
Code: unaudited

The manual says an unused `where` binding is never computed but an unused
`let` "still runs where it is written". EVAL-3 places a value at its uses,
and an unused value has none. Observable when the value fails. Either (a) an
unused pure binding is never computed, `let` or `where`; or (b) `let` is
computed where written and `where` is not.

### EVAL-6 Where a value used only inside a lambda is computed
Intent: open
Code: unaudited

The manual says once, when the lambda is built; EVAL-3 would place it in the
body, once per call. Either (a) at construction, so a failing value fails
even if the lambda is never called; (b) on each call; or (c) at construction
only when it does not depend on the lambda's parameters and cannot fail.

## Values

### EVAL-7 A type that crosses into morloc behaves as a value
Intent: ruled 2026-10-17
Tests: spec-eval-7-1, spec-eval-7-2
Code: conforms 2026-10-17

morloc lets each language's code stay idiomatic, but a native type that a
morloc type maps to, or that sourced code returns into morloc, must behave
as a value:

- a copy is an independent value equal to the original, and changing one
  never changes the other;
- copying it, moving it and releasing it have no effect a program depends
  on: no output, no counter, no resource opened, closed or locked;
- it refers into no other value: it holds no reference, pointer or view
  (a C++ `std::string_view`, `std::span`, `T*` or `T&`, or a struct holding
  one) into data that something else owns, so a sourced function never
  returns one into its arguments (ruled 2026-10-18);
- it can be built from, assigned from and passed moved values: in C++ a
  mapped record's constructor takes no field by non-const lvalue
  reference, and no field type deletes its move constructor (ruled
  2026-10-18).

Sourced code never changes a value it is given: no C++ parameter of type
`T&`, no in-place change of a Python or R argument. A sourced function
that needs to mutate works on its own copy. This holds until morloc has a
way to say where a value may be changed (ruled 2026-10-18).

A program that breaks one of these rules has no defined result: what it
computes may change with the compiler's placement of copies and moves, and
a view may outlive the value it refers into.

morloc makes as many or as few copies of a value as it likes, moves a value
instead of copying it, and releases each copy wherever it is done with it.
How many times a type's copy constructor or `Clone` runs, and when its
destructor or `Drop` runs, is unspecified, and changes between versions of
the compiler. This is how C++ already treats copy elision.

So a program means the same for every placement of copies, moves and
releases that gives each read a value equal to the one named. The compiler
relies on this to choose a placement (OWN).

Work that must happen at a definite time, such as writing to a file or
releasing a lock, is an effect (`EFF`), done by a function morloc calls in
order, not a side effect of copying or releasing a value.

Why: moving a value at its last use is what keeps a loop over a Rust
`Box`-armed type linear (#179); a type whose copies or releases carry
effects would see them change with every such improvement.
