# Type aliases

`type` aliases: parameters, transparency, expansion. Prefix: ALIAS.

### ALIAS-1 An alias is the same type as its expansion
Intent: ruled 2026-10-08
Tests: spec-alias-1-1, spec-alias-1-2, spec-alias-1-3, spec-alias-1-4, spec-alias-1-5, spec-alias-1-6
Code: conforms 2026-10-09

Given `type A x1 ... xn = T`, `A U1 ... Un == T[x1 := U1, ..., xn := Un]` in
every position a type can appear. Two aliases with one expansion are the same
type as each other, even when their arguments differ. An alias adds nothing
to its expansion but its docstrings (DOC-3).

### ALIAS-2 Every alias parameter appears in the body
Intent: ruled 2026-10-08
Tests: spec-alias-2-1, spec-alias-2-2, spec-alias-2-3, spec-alias-2-4
Code: conforms 2026-10-09

A `type` parameter that the body does not use is a declaration error. Uses may
be in kind expressions.

### ALIAS-3 An alias is applied to all its `Type` parameters
Intent: ruled 2026-10-08
Tests: spec-alias-3-1, spec-alias-3-2, spec-alias-3-3, spec-alias-3-4, spec-alias-3-5, spec-alias-3-6, spec-alias-3-7, spec-alias-3-8, spec-alias-3-9, spec-alias-3-10, spec-alias-3-11
Code: conforms 2026-10-09

An alias applied to fewer `Type` arguments, or more arguments, than it
declares is rejected, wherever it is written: signatures, constraints,
annotations, instance heads and type bodies. Omitted arguments of other kinds
are filled (KIND-6).

### ALIAS-4 A non-regular recursive alias is rejected at its declaration
Intent: ruled 2026-10-08
Tests: spec-alias-4-1, spec-alias-4-2, spec-alias-4-3, spec-alias-4-4
Code: conforms 2026-10-09

An alias whose body mentions the alias at arguments other than its own
parameters, as in `type N a = (a, [N [a]])`, is a declaration error. The
arguments are compared after unfolding (ALIAS-1).

### ALIAS-5 Error messages name the alias, not its expansion
Intent: ruled 2026-10-08
Tests: spec-alias-5-1, spec-alias-5-2, spec-alias-5-3, spec-alias-5-4, spec-alias-5-5, spec-alias-5-6
Code: conforms 2026-10-09

Where a type was written with an alias, a diagnostic prints the alias name.

### ALIAS-6 An alias owns no instances and no native forms
Intent: proposed
Tests: spec-alias-6-1, spec-alias-6-2, spec-alias-6-3, spec-alias-6-4, spec-alias-6-5
Code: conforms 2026-10-09

`instance C A` and `type L => A = ...` are rejected when `A` is a `type`
alias. Every alias shares the instances and native forms of its expansion
(NATIVE-2). A declaration with no right-hand side is not an alias (NEWT-5).

### ALIAS-7 Recursive aliases are legal if guarded by an optional or a list
Intent: ruled 2026-10-08
Tests: spec-alias-7-1, spec-alias-7-2, spec-alias-7-3
Code: conforms 2026-10-09

A regular recursive alias whose every recursive occurrence sits under `?`
or `[ ]`, as in `type T a = (a, ?(T a))`, is legal. Two such aliases with
equal unfoldings are the same type (TEQ-1).

### ALIAS-8 An alias parameter takes the kind of the slots it fills
Intent: ruled 2026-10-08
Tests: spec-alias-8-1, spec-alias-8-2, spec-alias-8-3, spec-alias-8-4, spec-alias-8-5, spec-alias-8-6, spec-alias-8-7, spec-alias-8-8, spec-alias-8-9, spec-alias-8-10, spec-alias-8-11, spec-alias-8-12, spec-alias-8-13, spec-alias-8-14, spec-alias-8-15, spec-alias-8-16
Code: conforms 2026-10-09

An unannotated alias parameter that fills only slots of one kind `K` other
than `Type` has kind `K`, as if written `(x :: K)`. One that fills slots of
two kinds is a declaration error. A declared kind stands.

### ALIAS-9 An alias is declared once
Intent: ruled 2026-10-08
Tests: spec-alias-9-1, spec-alias-9-2, spec-alias-9-3, spec-alias-9-4, spec-alias-9-5, spec-alias-9-6, spec-alias-9-7
Code: conforms 2026-10-09

A second general declaration of an alias's name is a declaration error,
located at the second.

### ALIAS-10 A type is spelled at its most specific common unfolding
Intent: ruled 2026-10-08
Code: deviates (unfiled: an inference variable takes the spelling of the first constraint that solves it)

A spelling is a type expression as written, aliases included. `S ->1 S'`
replaces one alias application in `S` by its body; `->*` is its
reflexive-transitive closure. Spellings of one type differ only in spelling
(ALIAS-1). At one position, unfolding the head alias over and over gives a
chain `S = S0 ->h S1 ->h ... ->h Sk` ending in a non-alias head; ALIAS-9
makes the chain unique.

    S' is at least as general as S     iff  S ->* S'
                                            (S may add docstrings, nothing else)

The join `S |_| S'` of two spellings of one type is defined position by
position:

- the first element of `S`'s chain that is also an element of `S'`'s chain,
  where two elements are the same when their heads are the same name and
  their arguments are pairwise the same spelling; that element's arguments
  are then joined in turn;
- if no alias element is shared, the chains' last elements, whose heads
  agree, with their arguments joined;
- two kind-level expressions (`Nat`, `Str`, rows) that are equal but
  written differently join to their normal form (`1 + 3` and `2 + 2` to
  `4`).

- Checking a term against an expected spelling `E` gives the term the
  spelling `E` at that site, whatever spelling it synthesized: passing a
  `WindSpeed` where `Real` is expected yields a `Real`.
- An inference variable constrained by spellings `S1 ... Sk` is solved to
  `S1 |_| ... |_| Sk`. The join is commutative and associative, so the
  result does not depend on the order of constraints.

### ALIAS-15 The join of two distinct recursive aliases
Intent: open
Code: unaudited

`type P = [P]` and `type Q = [Q]` are one type (ALIAS-7, TEQ-1), but no
finite spelling unfolds from both, so ALIAS-10 gives no join. Candidates:
(a) such a position keeps no alias spelling: it is printed one level
unfolded and carries no alias docstrings; (b) two distinct recursive
aliases are different types after all, which amends TEQ-1.

### ALIAS-13 Docstrings at an inferred position
Intent: open
Code: unaudited

A position whose type is inferred rather than written has the spelling
ALIAS-10 gives it. Candidates: (a) it carries the docstrings of that
spelling (DOC-3); (b) it carries none, and only written signatures inherit
type docstrings.

### ALIAS-11 An alias names a type of any kind, over its own parameters
Intent: ruled 2026-10-08
Tests: spec-alias-11-1, spec-alias-11-2, spec-alias-11-3, spec-alias-11-4, spec-alias-11-5
Code: conforms 2026-10-09

An alias may name a row, a table or any other kind of type
(`type Cols = {x = Int, y = Bool}`, `type MyTable n = Table n Cols`). Every
type variable in its body is one of its parameters; a free variable is a
declaration error. A `Type` is required only where a type classifies a term
(KIND-2).

### ALIAS-12 Two applications of one alias are compared through the expansion
Intent: ruled 2026-10-09
Tests: spec-alias-12-1, spec-alias-12-2, spec-alias-12-3
Code: conforms 2026-10-09

`A U1 ... Un <: A V1 ... Vn` holds exactly when the unfoldings are related
(ALIAS-1). Comparing the arguments pairwise, `Ui <: Vi`, decides it only
for a parameter that the body uses covariantly and injectively; a solver
never fixes an inference variable by matching the arguments of an alias
application. Spellings are tracked apart from comparison (ALIAS-10).

### ALIAS-14 Other recursive aliases
Intent: open
Code: unaudited

`type L a = (a, L a)` (every value infinite) and `type P = Box P` with
`data Box a = B a | E` (bounded by the `data` type) are rejected today.
Candidates: (a) a recursive alias is legal when every cycle passes through
`?`, `[ ]`, or a `data` or `newtype` with a non-recursive alternative, so
the type has finite values; (b) only `?` and `[ ]` guard (ALIAS-7 as is).
