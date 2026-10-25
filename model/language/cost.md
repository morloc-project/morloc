# Cost

What the realizer minimizes when it chooses an implementation and a
language for each call (REAL-2), and the docstring directives that inform
it. Prefix: COST. Design and worked examples:
/work/plans/realization-cost-model.md; implementation plan:
/work/plans/realization-cost-model-implementation.md.

The cost is an estimate used to rank placements and implementations of one
program, not a prediction of its run time. Every quantity the program does
not state takes a default.

## The objective

### COST-1 The realizer minimizes estimated time
Intent: ruled 2026-10-16
Code: deviates (../compiler/issues/realization.md)

Of the placements REAL-2 allows, the realizer chooses one of least total
cost, in seconds: the cost of each root's call, which is its own COST-2 plus
the COST-3 boundaries and COST-4 callbacks of the calls it makes, each
counted once; a callback's body is counted only through COST-4. Ties are
broken by fewer crossings between pools, then fewer changes of language
within one pool, then deterministically. A placement no language can realize
is excluded, never priced. Starting a pool costs nothing: pools start once
per run.

### COST-2 A call costs its language's coefficient times its self time, plus the language's call overhead
Intent: ruled 2026-10-16
Code: deviates (../compiler/issues/realization.md)

    call(f, L) = coeff(L) * selftime(f) + overhead(L)

`selftime(f)` is the function's own time, excluding the functions it calls
back (COST-4) and the values it moves (COST-3), evaluated at the sizes of
its arguments: for a sourced implementation, its own `@selftime` (COST-6),
else the `@selftime` of its signature, else 1 us. Implementations in one
language may differ in self time; the cheaper is chosen at each call, from
the sizes there. `coeff(L)` and `overhead(L)` are per language (COST-10). An
implementation defined in morloc, a class default (CLS-8) included, costs
the sum of its body; a signature's `@selftime` does not apply to it.
Language-free code the nexus evaluates costs nothing.

### COST-3 A crossing costs one round trip, plus encoding where a value is made and decoding where it is used
Intent: proposed
Code: deviates (../compiler/issues/realization.md)

Deferred 2026-10-25: not implemented until a program needs it, one that
moves a value no pools can share (an immutable Arrow table, and some
tensors, are shared at no cost) and whose transfer costs far more than a
crossing's fixed cost (about 20 to 40 us). Until then a crossing costs its
latency only. Sizes that count elements (COST-5, COST-8) are not deferred:
they bind the names of self times and call counts.

Generated code moves values hop by hop: a subtree placed in a pool other
than its parent's becomes a foreign call that receives the subtree's free
variables as arguments and returns its result. Each such boundary, from
parent language A to child language B in different pools, costs one
`latency(A, B)` per call, for its arguments and result together. A value
made in pool P and decoded for native use in pool Q costs, for that pair,
`slope_e(P, Q) * elements + slope_b(P, Q) * bytes`; a pool between them
that only forwards the value keeps it serialized and adds only its
boundaries' latencies. A value decoded natively in two pools is charged for
each. Nothing is charged between languages that share a pool (one
language, or a guest language in its host's pool). Elements and bytes come
from COST-8.

### COST-4 A callback costs one call of it times its call count
Intent: ruled 2026-10-16
Tests: spec-cost-4-1
Code: deviates (../compiler/issues/realization.md)

A function value passed to a function `h` costs, per call of `h`, its call
count times the cost of one call of it: its own call (COST-2) and every
crossing and callback its body makes (COST-3, COST-4), with its argument
sizes from `h`'s signature (COST-5). The count is `h`'s `@calls` for that
argument (COST-6); a suspension argument is forced once; for a sourced `h`
without `@calls` the count is 1000. For an `h` defined in morloc the count
comes from its body: each place the body calls the function counts once,
times any count around it. A call of a function value whose cost is unknown
(a closure a source returned, a container element, a function chosen by a
conditional) costs one default call.

## Directives

### COST-5 `@wiresize` and `@dimension` state the size of a value
Intent: ruled 2026-10-16
Tests: spec-cost-5-1, spec-cost-5-2, spec-cost-5-3, spec-cost-5-4, spec-cost-5-5, spec-cost-5-6, spec-cost-5-7, spec-cost-5-8, spec-cost-5-9, spec-cost-5-10, spec-cost-5-11, spec-cost-5-12, spec-cost-5-13
Code: deviates (../compiler/issues/realization.md)

The cost of copying data across a boundary is deferred 2026-10-25 with
COST-3: `@wiresize` and sizes in bytes are not implemented until a program
needs them. `@dimension` is in scope: its factors count elements and bind
the names of `@selftime` and `@calls` formulas (COST-7).

`@wiresize <bytes>` states a value's expected size on the wire; a number
without a byte unit is an error. `@dimension <f1> ... <fk>` states its
shape: factors whose product is its size, each a number, a name (COST-7),
or a type variable of the signature standing for the size of one value of
that type, in bytes. At most one factor is bytes; the product is bytes, the
size, or a plain number, which counts elements of the innermost element
type, each of its own size (one byte for a character of a `Str`; a tuple
element is the sum of its fields). Any other product, and a type name as a
factor, is an error. A type variable applied to one element (`f a`) is a
container of one level, whose count is known where the
container is a list and is the default otherwise. `@wiresize` and `@dimension` are mutually exclusive on
one position; between an alias and the signature that writes it, the
nearer wins (DOC-5). Allowed
above: an argument or the result in a signature; a type alias (applying
wherever a signature writes the alias, nested positions included; its names
are renamed at each use, so two uses never share one); a record field.
Sizes are language independent.

### COST-6 `@calls` and `@selftime` state counts and times
Intent: ruled 2026-10-16
Tests: spec-cost-6-1, spec-cost-6-2, spec-cost-6-3, spec-cost-6-4, spec-cost-6-5, spec-cost-6-6, spec-cost-6-7, spec-cost-6-8, spec-cost-6-9, spec-cost-6-10, spec-cost-6-11, spec-cost-6-12, spec-cost-6-13, spec-cost-6-14, spec-cost-6-15, spec-cost-6-16, spec-cost-6-17, spec-cost-6-18, spec-cost-6-19, spec-cost-6-20, spec-cost-6-21, spec-cost-6-22, spec-cost-6-23, spec-cost-6-24, spec-cost-6-25, spec-cost-6-26, spec-cost-6-27
Code: deviates (../compiler/issues/realization.md)

Above a function-typed or suspension-typed argument of a signature,
`@calls <formula>` states how many times a sourced implementation calls it
per call, rounded up to a whole number; where a call's implementations
differ, the largest count is used, one whose signature states none counting
the default (COST-4). Anywhere else, and above an argument that is not a
function or a suspension once aliases are expanded, it is an error. Above a
signature, `@selftime <formula>` states the self time (COST-2) of every
sourced implementation that states none of its own. Above an item of a
`source` block or list (DOC-1), `@selftime <formula>` states that
implementation's self time, in the names of its term's signature. Neither
applies to an implementation defined in morloc, whose counts and cost come
from its body.

### COST-7 A formula is arithmetic over a signature's names, with units
Intent: ruled 2026-10-25
Tests: spec-cost-7-1, spec-cost-7-2
Code: deviates (../compiler/issues/realization.md)

A formula has numbers with units, names, `+ - * / ^`, `log` (natural),
`log2` and parentheses. Units: `B KB MB GB TB` (powers of 1000), `KiB MiB
GiB TiB` (powers of 1024), `ns us ms s`. A name is a plain number, local to
one signature, meaning one quantity everywhere in it, as a type variable
does. Only a literal carries a unit, and every formula has a dimension, a
power of time and a power of bytes: the operands of `+` and `-` have one
dimension (adding a time to a plain number is an error), the argument of
`log` and `log2` and an exponent are plain numbers, and a base with a
dimension takes only an integer literal exponent. `@selftime` is a time,
`@wiresize` bytes, `@calls` a plain number. A name in `@calls` or
`@selftime` that no `@dimension` of the signature introduces is an error. A
formula without names whose value is undefined, infinite or negative is an
error; one with names that takes such a value at a call counts as the
default (COST-9) for that position.

Names are introduced only by `@dimension` (COST-5).

## Sizes

### COST-8 A value's size comes from what made it
Intent: ruled 2026-10-16
Code: deviates (../compiler/issues/realization.md)

Only element counts are computed until COST-3's copy cost is built
(deferred 2026-10-25); they bind the names a formula uses.

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
signatures merged into one keep both sets of cost directives; two that
state different literal sizes for one position are an error.

### COST-9 Defaults follow the structure of the type
Intent: ruled 2026-10-16
Code: deviates (../compiler/issues/realization.md)

Element counts follow these defaults; byte sizes are deferred with COST-3
(2026-10-25).

A fixed-width value is its width. A variable-width leaf (`Str`, a byte
vector) is 4 KB. A list is 10 elements times its element, or as many as a
literal `Nat` in its type states. A record or tuple is the sum of its
fields; a sum type its largest constructor; an optional its payload. A type
that contains itself counts its inner occurrence as a variable-width leaf. A
stream, an IFile, a function value and a suspension are 64 B (a handle or a
callable, WIRE-5; their cost is in their use, COST-4). An unbound name is
10.

## Coefficients and the scorer

### COST-10 Coefficients come from the performance model
Intent: ruled 2026-10-16
Code: deviates (../compiler/issues/realization.md)

`latency`, `slope_e` and `slope_b` per pair of pools, and `coeff` and
`overhead` per language, are one table shipped with the compiler, filled
from the measurements of model/performance XL-1, XL-2 and XL-3. The table is
the only place they are stated.

### COST-11 The realizer prices what code generation builds
Intent: proposed
Code: deviates (../compiler/issues/realization.md)

Choosing and scoring use one boundary cost (COST-3). A node that is always
generated in its parent's language (a lambda, a constructor application, a
container of functions, a function argument carried into a sourced call, a
literal, a variable) is priced only in that language. The components of a
list, tuple or record are all priced. Among implementations of one term
whose types differ, instance resolution (CLS-2) chooses before cost does,
so cost never hides an ambiguity. Sizes are computed while scoring, in
data-flow order. The cost of the placement built equals the least cost
found, except where a recursive call forces its language or a node moves
to a language that can hold its type.

### COST-12 The statements of a do-block may run in different languages
Intent: proposed
Tests: spec-cost-12-1
Code: conforms 2026-10-24

A do-block's statements are placed one by one; placing a statement and
everything after it in another pool than the statement before costs one
boundary (COST-3), with every value still in use crossing it. A split keeps
effect order, evaluates each statement once, keeps failure handling and
reruns a suspension at each bind.

### COST-14 A recursive definition runs its body 1000 times per call
Intent: ruled 2026-10-24
Tests: spec-cost-14-1
Code: conforms 2026-10-24

A call of a function whose definition calls itself costs its body's cost
times 1000, the same default as a callback's count (COST-4): the body's
calls, crossings and callbacks each happen once per level. Where the
recursion's depth is known from its arguments no directive yet says so
(COST-13).

### COST-13 What remains open
Intent: open
Code: unaudited

Open: a per-instance `@calls` (a class method's count applies to every
instance today); binding a name to an argument's value when the argument is
a literal; how a parallel map's workers, and a stream read
from another pool, are costed; size directives inside function, suspension
and optional aliases, which are expanded before signatures are read;
computing in the caller an argument that only the caller's language can
compute; and whether a label (ANN) wraps the same work when a do-block
splits (../compiler/issues/realization.md).
