# Patterns

Destructuring, `match`, guards, pattern chains, exhaustiveness. Prefix: PAT.

## Binding patterns

### PAT-1 Irrefutable patterns match every well-typed receiver
Intent: proposed
Code: unaudited

The forms are a name, `_`, a tuple of full arity, a record `{k = p, ...}`,
an as-pattern (BIND-5), and any nesting of these. A record pattern needs the
receiver to have the named keys and ignores the rest. They appear as lambda
parameters, definition parameters, `let` left-hand sides and `do` binds; a
record pattern after `let` or `do` is parenthesized.

### PAT-2 Clauses are tried in order and the first match wins
Intent: proposed
Code: unaudited

A refutable pattern is an irrefutable one that may also contain literals
and constructor patterns. A literal matches by the `Eq` instance of its type.
A definition's clauses carry one pattern per parameter; a `match e` clause
carries one pattern. A `match` clause list ends at the first token that cannot
begin a clause, so a `|` always continues the innermost list.

### PAT-3 A clause list is exhaustive
Intent: proposed
Code: unaudited

The last clause is irrefutable, or the clauses cover every value of the
matched type: both `Bool` literals, or every constructor of a sum type.
Anything else is a compile error at the definition or the `match`.

### PAT-4 Literal patterns on reals
Intent: open
Code: unaudited

PAT-2 makes a `Real` literal pattern an `Eq` test, so `NaN` never matches and
`0.0` matches `-0.0`. Candidates: (a) allowed with those semantics; (b) a
`Real` literal pattern is rejected.

## Pattern chains

### PAT-5 A chain step must fit the shape of its receiver
Intent: proposed
Code: unaudited

The head of a getter or setter chain matches the receiver: a list takes a
bracket step `.[i]` or `.[i:j:k]`, a record a key step `.k`, a tuple an index
step `.n`. A key or index step on a list is a type error.

### PAT-6 Only a bracket slice broadcasts its tail
Intent: proposed
Code: unaudited

After `.[i:j:k]` the rest of the chain applies to each element of the slice:
`.[s].t xs` is `map .t (.[s] xs)`. After `.[i]` the tail applies to the one
element. No other step broadcasts.
