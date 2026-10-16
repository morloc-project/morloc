# Cost

What the realizer minimizes when it chooses an implementation and a
language for each call (REAL-2), and the docstring directives that inform
it. Prefix: COST. Design and worked examples:
/work/plans/realization-cost-model.md.

The cost is an estimate used to rank placements of one program, not a
prediction of its run time. Every quantity the program does not state takes
a default.

## The objective

### COST-1 The realizer minimizes estimated time
Intent: proposed
Code: deviates model/compiler/issues/realization.md

Of the placements REAL-2 allows, the realizer chooses one of least total
cost, the sum of COST-2 over calls and COST-3 over crossings, in seconds.
Ties are broken by fewer crossings, then deterministically. A placement no
language can realize is excluded, never priced.

### COST-2 A call costs its language's coefficient times its runtime, plus the language's call overhead
Intent: proposed
Code: unaudited

    call(f, L) = coeff(L) * runtime(f) + overhead(L)

`runtime(f)` is the value of `f`'s `@runtime` (COST-6) at the sizes of its
arguments, or a small constant without one. `coeff(L)` and `overhead(L)`
are per language (COST-10). Language-free code evaluated by the nexus costs
nothing. A function defined in morloc costs the sum of its body.

### COST-3 A crossing costs a latency plus per-element and per-byte terms
Intent: proposed
Code: unaudited

A value that a call in language A produces and a call in language B
consumes costs, when A and B are in different pools,

    cross(A, B, v) = latency(A, B) + slope_e(A, B) * elements(v)
                                   + slope_b(A, B) * wiresize(v)

and nothing when they share a pool (one language, or a guest language in
its host's pool). Each edge is priced once, at its consumer. Elements and
wiresize come from COST-8.

### COST-4 A callback costs its body times its call count
Intent: proposed
Code: unaudited

A function value passed to a function `h` costs, per call of `h`, its call
count times the cost of one call of it: its own call (COST-2) and every
crossing its body and its arguments and result make (COST-3), with its
argument sizes from `h`'s signature (COST-7). The count is `h`'s `@calls`
for that argument (COST-6); a suspension argument is forced once; for a
sourced `h` without `@calls` the count is 1000. For an `h` defined in
morloc the count comes from its body, never from a default.

## Directives

### COST-5 `@wiresize` and `@dimension` state the size of a value
Intent: proposed
Code: unaudited

`@wiresize <bytes>` states a value's expected size on the wire.
`@dimension <f1> ... <fk>` states its shape: factors whose product is its
wiresize, each a number, a name (COST-7), or a type variable of the
signature standing for the size of a value of that type; the last factor,
if a number, is in bytes, and the others count elements. They are mutually
exclusive on one position. Allowed above: an argument or the result in a
signature; a type alias (applying wherever a signature writes the alias,
nested positions included); a record field. Sizes are language
independent.

### COST-6 `@calls` and `@runtime` state counts and times
Intent: proposed
Code: unaudited

Above a function-typed argument, `@calls <formula>` states how many times
the function calls it, and `@runtime <name>` binds the name to that
argument's runtime (COST-2). Above a signature, `@runtime <formula>` states
the function's runtime before its language's coefficient. `@runtime` on a
term defined in morloc is an error: its cost is its body's.

### COST-7 A formula is arithmetic over a signature's names, with units
Intent: proposed
Code: unaudited

A formula has numbers with units, names, `+ - * / ^`, `log` (natural),
`log2` and parentheses. Units: `B KB MB GB TB` (powers of 1000), `KiB MiB
GiB TiB` (powers of 1024), `ns us ms s`. A name is local to one signature
and means one quantity everywhere in it, as a type variable does; a name
in a type alias is renamed at each use, so two uses never share it. A
name used in `@calls` or `@runtime` that no `@dimension` or `@runtime` of
the signature introduces is an error. Units are checked: a size is in
bytes, a count has none, a runtime is a time, and an exponent or a `log`
argument has none. A value that is undefined, infinite or negative counts
as the default (COST-9) for that position.

## Sizes

### COST-8 A value's size comes from what made it
Intent: proposed
Code: unaudited

The size of a value at an edge is, first that applies:

1. A literal size at its producer: the result position of the function
   that made it, or the parameter position, in the enclosing function's
   signature, of a parameter.
2. Its producer's annotation with names, evaluated with the names bound
   from the producer's arguments.
3. A literal size at the consumer's argument position.
4. COST-9's default for its type.

A name at a consumer's argument position binds, it does not claim: it takes
its value from the value that arrives. When two arguments bind one name, a
known value beats a default and the larger known value wins. Two term
signatures merged into one keep both sets of directives; two that state
different sizes for one position are an error.

### COST-9 Defaults follow the structure of the type
Intent: proposed
Code: unaudited

A fixed-width value is its width. A variable-width leaf (`Str`, a byte
vector) is 4 KB. A list is n elements times its element, with n = 10. A
record or tuple is the sum of its fields; a sum type its largest
constructor; an optional its payload. A type that contains itself counts
its inner occurrence as a variable-width leaf. A stream, an IFile and a
function value are 64 B (a handle; their cost is in their use, COST-4).
An unbound name is 10.

## Coefficients and the scorer

### COST-10 Coefficients come from the performance model
Intent: proposed
Code: unaudited

`latency`, `slope_e` and `slope_b` per pair of pools, and `coeff` and
`overhead` per language, are one table shipped with the compiler, filled
from the measurements of model/performance XL-1, XL-2 and XL-3. The table is
the only place they are stated.

### COST-11 Every place the realizer prices an edge prices it the same way
Intent: proposed
Code: deviates model/compiler/issues/realization.md

Choosing and scoring use one edge cost (COST-3). A use of a let-bound value
or of a parameter is priced from the language the value lives in to the
language of the use. The components of a list, tuple or record are all
priced. A function argument of a function defined in morloc is priced once,
in its body. Sizes are computed while scoring, in data-flow order.

### COST-12 What remains open
Intent: open
Code: unaudited

Open: whether the statements of a do-block may be placed in different
languages (each let is placed on its own today); a per-implementation
`@runtime`; binding a name to an argument's value when the argument is a
literal; how a parallel map's workers, and a stream read from another pool,
are costed.
