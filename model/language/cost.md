# Cost

What the realizer minimizes when it chooses an implementation and a
language for each call (REAL-2): crossings between pools, deepest first,
then a fixed language preference, then size. A placement depends only on
the program and the language order. Prefix: COST. Design and the models
considered: /work/plans/realization-minimal-model.md.

## The objective

### COST-1 The realizer minimizes crossings, deepest first
Intent: proposed
Tests: spec-cost-1-1
Code: deviates (../compiler/issues/realization.md)

Of the placements COST-15 allows, the realizer chooses one that is least by
these, in order:

1. the crossings (COST-3) at each depth (COST-4), compared from the
   greatest depth down: fewer crossings at a greater depth wins whatever
   the counts at smaller depths;
2. changes of language within one pool (a guest language and its host);
3. language preference (COST-16): fewer calls placed in the least preferred
   language, then in the next, and so on;
4. size: fewer sourced calls, so a sourced implementation beats a
   definition in morloc that does the same work in the same languages by
   calling several.

A call has a choice of language only through its implementations; a node
with none of its own (a let, a conditional, a container, a do-block) that
ties takes its parent's language. Two sources of one term in one language
are refused before placement, naming both; sources in different languages
differ in preference unless neither language is in the order, and two
sources of one type still tied after rule 4 are an error at build naming
both. Either
way the program removes one from scope (imports, namespaces). Definitions
in morloc that tie are interchangeable (REAL-1), and a fixed one is used.
Starting a pool costs nothing.

### COST-2 A call costs its language's coefficient times its self time
Intent: retired 2026-10-26
Code: unaudited

Replaced by COST-1.

### COST-3 A crossing is one edge between two pools
Intent: proposed
Code: deviates (../compiler/issues/realization.md)

Generated code moves values hop by hop: a subtree placed in a pool other
than its parent's becomes a foreign call that receives the subtree's free
variables and returns its result. Each such edge is one crossing, however
many values it carries; a value made in one pool, passed through a second
and used in a third crosses twice. Languages that share a pool (one
language, or a guest language in its host's pool) cross nothing. Every
crossing counts the same: neither the data a crossing moves nor the pair of
pools it joins is weighed.

### COST-4 An argument of a sourced function, and a recursion's body, is one deeper
Intent: proposed
Tests: spec-cost-4-1
Code: deviates (../compiler/issues/realization.md)

Code at the root of an exported function is at depth 0. A function or
suspension passed to a sourced function, or to a shared specialization, is
one deeper than the call: it may run more than once per call. A function
value passed to a function defined in morloc is placed where the
definition's body uses it, at that use's depth. The body of a recursive
function is one deeper than the call that enters it (COST-14). A call of a
function value whose origin the realizer cannot see (a closure a source
returned, a container element, a function chosen by a conditional) is at
the depth of the call.

## Directives

### COST-5 `@wiresize` and `@dimension` state the size of a value
Intent: retired 2026-10-26
Code: unaudited

Replaced by COST-3: sizes are not weighed.

### COST-6 `@calls` and `@selftime` state counts and times
Intent: retired 2026-10-26
Code: unaudited

Replaced by COST-4.

### COST-7 A formula is arithmetic over a signature's names, with units
Intent: retired 2026-10-26
Code: unaudited

No directive takes a formula.

## Sizes

### COST-8 A value's size comes from what made it
Intent: retired 2026-10-26
Code: unaudited

Sizes are not weighed (COST-3).

### COST-9 Defaults follow the structure of the type
Intent: retired 2026-10-26
Code: unaudited

Sizes are not weighed (COST-3).

## Allowed placements, preference and the scorer

### COST-10 Coefficients come from the performance model
Intent: retired 2026-10-26
Code: unaudited

Replaced by COST-16: no per-language or per-pair costs are stated.

### COST-15 Only placements that run are compared
Intent: proposed
Tests: spec-cost-15-1, spec-cost-15-2
Code: deviates (../compiler/issues/realization.md)

Whether a program runs never depends on a comparison. Before COST-1
compares anything, a node's candidate languages are limited to those where:

1. every value the node makes or receives has a form (REAL-3);
2. a function made by host code stays in its host's language (HOST-9);
3. a lambda is built in the language of the expression that builds it;
4. the functions on one recursion cycle run in one pool;
5. a pool tree keeps the language its manifold is built in once fixed;
6. instance resolution (CLS-2) has chosen among implementations whose
   types differ;
7. a function a sourced call returns, and its binding, stay in that
   source's pool (HOST-9).

A let-bound value that holds a function and computes nothing to build (a
lambda, or a record, tuple or list of such values; not one carrying a
label) is built again in each language that uses it: a use in another
language than the let calls its own copy, placed as the realizer priced it.
Any other let-bound function value (one computed, or one carrying a label)
is built once, with its let, at its home: its definition's least language
(COST-16 breaking ties), and every use reaches it there. A function a
sourced call returns has its home among that term's sources.

The same limits apply when the placement is built, so the placement built
is the least placement found (COST-11).

### COST-16 Languages are preferred in one fixed order
Intent: proposed
Tests: spec-cost-16-1
Code: deviates (../compiler/issues/realization.md)

The order is shipped with the compiler and is the only place it is stated:
compiled languages first, then interpreted ones (cpp, rust, futhark, py,
r), a language with no entry last. It decides only between placements with
the same crossings at every level (COST-1).

### COST-11 The realizer places what code generation builds
Intent: proposed
Tests: spec-cost-11-1
Code: deviates (../compiler/issues/realization.md)

Choosing and building use one count of crossings (COST-3) and one set of
limits (COST-15). A node that is always generated in its parent's language
(a lambda, a constructor application, a container of functions, a function
argument carried into a sourced call, a literal, a variable) is counted
only in that language. The placement built is least by COST-1 among those
COST-15 allows, with no exceptions.

### COST-12 The statements of a do-block may run in different languages
Intent: proposed
Tests: spec-cost-12-1
Code: conforms 2026-10-24

A do-block's statements are placed one by one; placing a statement and
everything after it in another pool than the statement before is one
crossing (COST-3), which carries every value still in use. A split keeps
effect order, evaluates each statement once, keeps failure handling and
reruns a suspension at each bind.

### COST-14 A recursive definition's body is one deeper
Intent: proposed
Tests: spec-cost-14-1
Code: deviates (../compiler/issues/realization.md)

The body of a function that calls itself, directly or through other
functions, may run more than once per call, so its crossings count one
deeper than the call that enters it (COST-4).

### COST-13 What remains open
Intent: open
Code: unaudited

Open: a mark for an argument a sourced function calls at most once (a
resource or scoping combinator), should counting it one level deeper cost a
program measurably; how a parallel map's workers, and a stream read from
another pool, are counted; computing in the caller an argument that only
the caller's language can compute; whether a label (ANN) wraps the same
work when a do-block splits (../compiler/issues/realization.md); and
whether a heavy implementation should be placed in the most preferred
language before crossings are counted (a `@heavy` mark;
/work/plans/realization-minimal-model.md, M3).
