# Signatures

What a signature declares: one term with many implementations, terms without signatures, generic terms. Prefix: SIG.

### SIG-1 A signature declares one term
Intent: proposed
Code: conforms 2026-10-21

`f :: T` declares the term `f` with general type `T`. Every morloc
definition of `f` and every sourced function named `f` in scope is an
implementation of that one term, and each must have type `T`.

### SIG-2 `=` states substitutability, not binding
Intent: proposed
Code: conforms 2026-10-21

`f = e1` and `f = e2` together state that `e1` and `e2` may each stand for
`f`. A second definition adds an alternative; it never shadows the first.
Which alternative a program runs is chosen by realization (realization.md).

### SIG-3 Definitions whose literal values disagree are a compile error
Intent: proposed
Code: conforms 2026-10-21

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

### SIG-6 A module gives a term one signature
Intent: ruled 2026-10-14
Tests: spec-sig-6-1, spec-sig-6-2, spec-sig-6-3, spec-sig-6-4, spec-sig-6-5
Code: conforms 2026-10-14

Two signatures for one name in one scope are a compile error, even when
their types are equal; a class method's signature counts. A signature may
have many implementations; an implementation has one signature. A `where`
block is its own scope.

### SIG-7 A signature shadows a term imported with its whole module
Intent: ruled 2026-10-14
Tests: spec-sig-7-1, spec-sig-7-2, spec-sig-7-3
Code: conforms 2026-10-14

If a module imports `m` without an import list, and `m` exports `f`, a
signature `f :: T` in the module declares a new term `f`, which replaces the
imported one in the module and in every module that imports `f` from it.
The new term's implementations are those the module gives (definitions and
sources of `f`); every implementation of the imported `f` is dropped, so
the implementations of two different terms never mix. This holds when
several whole-module imports export `f`.

Why: a module that imports a library wholesale must keep working when the
library gains a name the module already uses.

### SIG-8 A signature for a term imported by name is a compile error
Intent: ruled 2026-10-14
Tests: spec-sig-8-1, spec-sig-8-2, spec-sig-8-3, spec-sig-8-4, spec-sig-8-5
Code: conforms 2026-10-14

If a module names `f` in an import list (`import m (f)`), or names a class
whose method is `f`, and also declares `f :: T`, the program is rejected,
even when `T` is the imported type. A namespaced import (`import m as n
(f)`) brings `n.f`, not `f`. The
fix is to import `f` under another name (`import m (f as g)`) or to leave
it out of the list.

### SIG-9 A definition without a signature adds an implementation
Intent: ruled 2026-10-14
Tests: spec-sig-9-1, spec-sig-9-2
Code: conforms 2026-10-14

A definition `f = e` with no signature for `f` in the module is one more
implementation of the term `f` in scope (SIG-1, SIG-2), imported or local.
This is how a language module adds implementations to a language-agnostic
module's terms. A definition the author meant as a new term, under a name
the module imports, silently joins the imported term instead; giving every
new term a signature avoids it.
