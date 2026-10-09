# Guest languages

Languages compiled into another language's pool. Prefix: GUEST.

### GUEST-1 A guest language runs in its host language's pool
Intent: proposed
Code: unaudited

A guest has no pool of its own. Its sourced functions are compiled into the
pool of its declared host language and called from there, so a call between
the guest and its host does not cross a pool. Futhark is a guest of C++.

### GUEST-2 A guest signature is checked against the guest's own declaration
Intent: proposed
Code: unaudited

Unlike SRC-3, the build checks a guest function's morloc signature against
the entry point the guest compiler reports: arity, element types and rank.
A mismatch is a build error naming the term.

### GUEST-3 A guest signature may use only types the guest can represent
Intent: proposed
Code: unaudited

For Futhark that is fixed-width numeric scalars and vectors and matrices of
them. Any other type in a guest signature is rejected at build.

### GUEST-4 Whether a guest's build backend may change results
Intent: open
Code: unaudited

`futhark:backend` (`c`, `multicore`, `cuda`, ...) is a build parameter, and
parallel backends may reassociate floating-point reductions. Either (a) every
backend must give the same value; or (b) floating-point results may differ
within the guest language's own guarantees, and the build parameter is part
of the program's meaning.
