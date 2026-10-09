# Bindings

Top-level and local definitions, `where`, `let`, lambdas, `@` binders, shadowing. Prefix: BIND.

### BIND-1 A definition binds a name to a body over parameter patterns
Intent: proposed
Code: unaudited

`f p1 ... pn = e` defines `f`; each `pi` is an irrefutable pattern (PAT-1)
whose names scope over `e`. Several `|`-clauses in place of one body are a
refutable definition (PAT-2).

### BIND-2 `where` bindings are order-invariant
Intent: proposed
Code: unaudited

A `where` block scopes over the body it hangs from and over every binding in
it, and sees the enclosing parameters and outer `where` blocks. A name is
bound at most once per block, may not repeat a parameter name, and bindings
may not be mutually recursive.

### BIND-3 `let` bindings are sequential and non-recursive
Intent: proposed
Code: unaudited

Each `let` binding scopes over the bindings after it and the body after
`in`, not over itself. A later binding may shadow an earlier one.

### BIND-4 A lambda takes at least one parameter
Intent: proposed
Code: unaudited

`\p1 ... pn -> e` with n >= 1, each `pi` an irrefutable pattern. A lambda
captures the names free in `e` from the enclosing scope. `\ -> e` is a parse
error.

### BIND-5 `@` is the one name binder
Intent: proposed
Code: unaudited

`x@a`, written without whitespace (LEX-5), binds `x` to the whole of `a`: to
the receiver in an irrefutable pattern (`xs@(a, b)`), to the value of an
expression, and in a type (`n@Int`), where `x` names the argument's value at
the type level as well as at runtime.

### BIND-6 Local recursion and shadowing of outer names
Intent: open
Code: unaudited

The manual rules out mutual recursion in `where` and says nothing else.
Open: (1) may a `where` binding refer to itself? (2) may a `where`, `let`
or lambda binding shadow a top-level or imported term? Candidates for each:
allowed, or a load error naming both bindings.
