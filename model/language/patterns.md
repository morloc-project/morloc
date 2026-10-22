# Patterns

Destructuring, `match`, guards, pattern chains, exhaustiveness. Prefix: PAT.

## Binding patterns

### PAT-1 Irrefutable patterns match every well-typed receiver
Intent: proposed
Code: deviates (issues/typecheck.md)

The forms are a name, `_`, a tuple of full arity, a record `{k = p, ...}`,
an as-pattern (BIND-5), and any nesting of these. A record pattern needs the
receiver to have the named keys and ignores the rest. They appear as lambda
parameters, definition parameters, `let` left-hand sides and `do` binds; a
record pattern after `let` or `do` is parenthesized.

### PAT-2 Clauses are tried in order and the first match wins
Intent: proposed
Code: conforms 2026-10-21

A refutable pattern is an irrefutable one that may also contain literals
and constructor patterns. A literal matches by the `Eq` instance of its type.
A definition's clauses carry one pattern per parameter; a `match e` clause
carries one pattern. A `match` clause list ends at the first token that cannot
begin a clause, so a `|` always continues the innermost list.

### PAT-3 A clause list is exhaustive
Intent: proposed
Code: deviates (issues/typecheck.md)

The last clause is irrefutable, or the clauses cover every value of the
matched type: both `Bool` literals, or every constructor of a sum type.
Anything else is a compile error at the definition or the `match`.

### PAT-4 Literal patterns on reals
Intent: open
Code: unaudited

PAT-2 makes a `Real` literal pattern an `Eq` test, so `NaN` never matches and
`0.0` matches `-0.0`. Candidates: (a) allowed with those semantics; (b) a
`Real` literal pattern is rejected.

### PAT-7 A list pattern matches a list of exactly its length
Intent: ruled 2026-10-10
Tests: spec-pat-7-1, spec-pat-7-2, spec-pat-7-3, spec-pat-7-4, spec-pat-7-5, spec-pat-7-6
Code: conforms 2026-10-10

A refutable pattern may be a list `[p1, ..., pn]`, n >= 0. It matches a
`List a` of length n whose i-th element matches pi. Its receiver is `List a`
and no other container, `Vector n a` included. No clause set over list
patterns covers `List a`, so PAT-3 requires a catch-all. Matching reads only
the length and the elements the pattern tests or binds, and needs no
instance.

## Pattern chains

### PAT-5 A chain step must fit the shape of its receiver
Intent: proposed
Tests: spec-pat-5-1, spec-pat-5-2, spec-pat-5-3, spec-pat-5-4, spec-pat-5-5, spec-pat-5-6, spec-pat-5-7, spec-pat-5-8, spec-pat-5-9, spec-pat-5-10, spec-pat-5-11
Code: conforms 2026-10-21

The head of a getter or setter chain matches the receiver: a record takes a
key step `.k`, a tuple an index step `.n`, and a list or `IFile` a bracket
step `.[i]` or `.[i:j:k]`. Any other receiver takes a bracket step only
through an instance of `Indexable` (for `.[i]`), `Sliceable` or
`SliceableDim` (for a slice), or `PatternAccessible`; a newtype does not
inherit its representation's instance (NEWT-1). Anything else is a type
error, as is a key or index step on a list.

### PAT-6 Only a bracket slice broadcasts its tail
Intent: proposed
Code: conforms 2026-10-21

After `.[i:j:k]` the rest of the chain applies to each element of the slice:
`.[s].t xs` is `map .t (.[s] xs)`. After `.[i]` the tail applies to the one
element. No other step broadcasts.
