# Host calls

How morloc and sourced code call each other: arguments, callbacks, suspensions. Prefix: HOST.

### HOST-1 Morloc calls a sourced function by the arity of its source signature
Intent: ruled 2026-09-24
Code: unaudited

The number of arguments passed in each call to a sourced function follows its
source signature. On the morloc side `a -> b -> c` and `a -> (b -> c)` are
one type (see `APP`).

### HOST-2 A sourced function calls a callback by the callback's type as written
Intent: ruled 2026-09-24
Code: unaudited

A callback parameter written `(a -> b -> c)` in the source signature is
called with both arguments at once; one written `(a -> (b -> c))` is called
with one argument and returns a function. Morloc passes any function of that
type in that grouping, however it was defined.

### HOST-3 `rsize` declares the call groups of a curried foreign function
Intent: proposed
Code: unaudited

`--' rsize: n1 n2 ...` gives the sizes of the leading call groups; the last
group takes the rest. Each size is at least 1 and leaves at least one
argument for the group after it, or the build is rejected.

### HOST-4 Whether parentheses in a sourced function's own type set its call groups
Intent: open
Code: unaudited

HOST-2 honors parentheses in a callback's type, but the manual says writing
`f :: Real -> ([Real] -> [Real])` does not change how `f` is called; only
`rsize` does. Either (a) the outer type's parentheses set the groups too,
replacing `rsize`; or (b) only `rsize` does, and the asymmetry is the rule.

### HOST-5 A suspension at a host boundary is a callable of no arguments
Intent: proposed
Code: unaudited

A host has no thunks: calling is forcing. The adapter between morloc and a
host is decided by type at the boundary, follows the source signature, and
absorbs the terminal suspension of a sourced function into its call. The
rules are `EFF`'s.

### HOST-6 A foreign exception is a failure of the call that raised it
Intent: proposed
Code: unaudited

An exception escaping a sourced function, in any language and across any
number of pools, reaches morloc as a failure that `@try` can catch (see
`FAIL`). A pool that is killed outright is not such a failure.
