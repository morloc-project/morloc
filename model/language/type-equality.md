# Type equality

When two types are the same type. Prefix: TEQ.

### TEQ-1 Equality is structural after alias expansion
Intent: proposed
Tests: spec-teq-1-1, spec-teq-1-2
Code: conforms 2026-10-09

`T == U` when, with every alias unfolded (ALIAS-1), they are the same
constructors applied to equal arguments, up to renaming of bound type
variables. Under a recursive alias (ALIAS-7) the unfoldings are infinite and
equality is coinductive: a pair met again while it is being compared holds. A type name denotes its declaration, not its spelling (MOD-14).

### TEQ-2 Sugar names the type it abbreviates
Intent: proposed
Code: unaudited

`[a] == List a`, `() == Unit`, and `(a1, ..., an) == TupleN a1 ... an`
for n >= 2.

### TEQ-3 A newtype is equal only to itself
Intent: proposed
Code: unaudited

`newtype N = T` gives `N == N` and never `N == T` (NEWT-1).

### TEQ-4 Record schemas are equal up to field order
Intent: proposed
Code: deviates (unfiled: types-kinds.asc shows `(a # l) <: (a # l)` failing)

`{x = Int, y = Str} == {y = Str, x = Int}` at kind `Rec`. A type-level
expression is equal to an identical expression, whether or not it reduces.

### TEQ-5 Equality of Nat expressions with free variables
Intent: open
Code: unaudited

Ground `Nat` expressions are equal when they reduce to the same number. With
free variables, is `m + n == n + m`? Candidates: (a) equal when they
normalize to the same polynomial; (b) equal only when syntactically
identical after reducing ground parts.

### TEQ-6 Two record declarations with the same fields
Intent: open
Code: unaudited

Given `record A = A { x :: Int }` and `record B = B { x :: Int }`, is
`A == B`? Candidates: (a) no, records are nominal; (b) yes, a record is its
schema, and the name is an alias for it.
