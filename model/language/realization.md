# Realization

Choosing an implementation and a language for each call; implementations are interchangeable. Prefix: REAL.

### REAL-1 Implementations of one term are interchangeable
Intent: proposed
Code: deviates (issues/values.md)

A term with several implementations, in one language or several, gives the
same value whichever is chosen. Declaring them under one name is the
author's claim that this holds. The choice changes cost, never meaning.

### REAL-5 Implementations that provably differ are rejected
Intent: proposed
Code: conforms 2026-10-31

Two implementations of one term at one type are rejected when they provably
give different values: numbers or other literals that differ, a value and
null, containers of different length or with an element that provably
differs, records or constructor applications with such a field, two
different constructors, `@show` of values of one type that provably
differ, or lambdas
whose bodies differ once their shared parameters are named alike, where two
different parameters, or a parameter and a constant, differ at a type with
more than one value. A name is compared through its own implementations.
Anything that cannot be decided, such as an application, a branch or an
intrinsic that may map different arguments to one value, is accepted as
REAL-1's claim, as is a comparison not decided within a fixed number of
steps. Two sources of one name in one language are rejected as
ambiguous.

### REAL-2 The compiler chooses an implementation and a language for each call
Intent: proposed
Code: conforms 2026-10-21

A term defined in morloc has no fixed language; it is realized in whatever
language its calls need. The choice is the same each time the same program
is built by the same compiler.

### REAL-3 A call with no realizable implementation is rejected at build
Intent: proposed
Code: deviates (issues/diagnostics-docs.md)

If no implementation of a term can be used where it is called, for example
because a type it needs has no native mapping in that language, the build
fails naming the term. It is never a run-time error.

### REAL-4 How the choice is made, and whether the user can steer it
Intent: open
Code: unaudited

Open: whether a program may pin a call to a language. What the choice
minimizes is cost.md (COST-1).
A pin would make REAL-1 checkable by building both ways.
