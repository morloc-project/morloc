# Signatures

What a signature declares: one term with many implementations, terms without signatures, generic terms. Prefix: SIG.

### SIG-1 A signature declares one term
Intent: proposed
Code: unaudited

`f :: T` declares the term `f` with general type `T`. Every morloc
definition of `f` and every sourced function named `f` in scope is an
implementation of that one term, and each must have type `T`.

### SIG-2 `=` states substitutability, not binding
Intent: proposed
Code: unaudited

`f = e1` and `f = e2` together state that `e1` and `e2` may each stand for
`f`. A second definition adds an alternative; it never shadows the first.
Which alternative a program runs is chosen by realization (realization.md).

### SIG-3 Definitions whose literal values disagree are a compile error
Intent: proposed
Code: unaudited

Two definitions of one term that reduce to different literals, or to
containers of different sizes, are rejected. Disagreement the compiler
cannot see (inside sourced code or arithmetic) is the programmer's claim and
is not checked.

### SIG-4 When a term may omit its signature
Intent: open
Code: unaudited

A term without a signature gets the type inference gives it. Inference
fails today for a point-free use of class methods and for a term that
matches on constructors. Is that (a) the rule, with a rejection that asks
for a signature, or (b) a gap, so that any term whose type is determined by
its uses needs none?

### SIG-5 An exported term whose type keeps a class constraint
Intent: open
Code: unaudited

An export whose type still has a class constraint cannot be given an
instance. Is it (a) skipped with a warning, as today, or (b) a compile
error? The same question applies to an export with a free type variable
and no constraint.
