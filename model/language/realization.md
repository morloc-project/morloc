# Realization

Choosing an implementation and a language for each call; implementations are interchangeable. Prefix: REAL.

### REAL-1 Implementations of one term are interchangeable
Intent: proposed
Code: unaudited

A term with several implementations, in one language or several, gives the
same value whichever is chosen. Declaring them under one name is the
author's claim that this holds. The choice changes cost, never meaning.

### REAL-2 The compiler chooses an implementation and a language for each call
Intent: proposed
Code: unaudited

A term defined in morloc has no fixed language; it is realized in whatever
language its calls need. The choice is the same each time the same program
is built by the same compiler.

### REAL-3 A call with no realizable implementation is rejected at build
Intent: proposed
Code: unaudited

If no implementation of a term can be used where it is called, for example
because a type it needs has no native mapping in that language, the build
fails naming the term. It is never a run-time error.

### REAL-4 How the choice is made, and whether the user can steer it
Intent: open
Code: unaudited

Open: what the choice minimizes (per-language cost, number of crossings,
data volume crossed), and whether a program may pin a call to a language.
A pin would make REAL-1 checkable by building both ways.
