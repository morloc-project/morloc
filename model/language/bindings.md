# Bindings

Top-level and local definitions, `where`, `let`, lambdas, `@` binders, shadowing. Prefix: BIND.

### BIND-1 A definition binds a name to a body over parameter patterns
Intent: proposed
Code: conforms 2026-10-21

`f p1 ... pn = e` defines `f`; each `pi` is an irrefutable pattern (PAT-1)
whose names scope over `e`. Several `|`-clauses in place of one body are a
refutable definition (PAT-2).

### BIND-2 `where` bindings are order-invariant
Intent: ruled 2026-10-29
Tests: spec-bind-2-1
Code: conforms 2026-10-31

A `where` block scopes over the body it hangs from and over every binding in
it, and sees the enclosing parameters and outer `where` blocks. A name is
bound at most once per block and may not repeat a parameter name. Bindings
may refer to each other in any order, recursively and mutually recursively,
within BIND-7.

### BIND-3 `let` bindings are sequential and non-recursive
Intent: proposed
Code: conforms 2026-10-21

Each `let` binding scopes over the bindings after it and the body after
`in`, not over itself. A later binding may shadow an earlier one.

### BIND-4 A lambda takes at least one parameter
Intent: proposed
Code: conforms 2026-10-21

`\p1 ... pn -> e` with n >= 1, each `pi` an irrefutable pattern. A lambda
captures the names free in `e` from the enclosing scope. `\ -> e` is a parse
error.

### BIND-5 `@` is the one name binder
Intent: proposed
Code: conforms 2026-10-21

`x@a`, written without whitespace (LEX-5), binds `x` to the whole of `a`: to
the receiver in an irrefutable pattern (`xs@(a, b)`), to the value of an
expression, and in a type (`n@Int`), where `x` names the argument's value at
the type level as well as at runtime.

### BIND-6 Shadowing of outer names
Intent: open
Code: unaudited

May a `where`, `let` or lambda binding shadow a top-level or imported term?
Candidates: allowed, or a load error naming both bindings. A `where` binding
may refer to itself (BIND-2).

### BIND-7 A recursive binding is a function or a suspension
Intent: proposed
Tests: spec-bind-7-1, spec-bind-7-2, spec-bind-7-3, spec-bind-7-4, spec-bind-7-5, spec-bind-7-6, spec-bind-7-7, spec-bind-7-8, spec-bind-7-9, spec-bind-7-10, spec-bind-7-11, spec-bind-7-12, spec-bind-7-13, spec-bind-7-14, spec-bind-7-15, spec-bind-7-16, spec-bind-7-17
Code: conforms 2026-10-31

A value is computed at most once per evaluation of its scope (EVAL-3), so
computing a binding may not need its own value. In a cycle of references
among top-level and `where` bindings, follow what computing a binding
demands of the next one round the cycle:

- a lambda's body is demanded only when the lambda is applied to all its
  parameters, and a `do` block's only when it is forced;
- the head of an application is applied unless it is given fewer arguments
  than its parameters, and an argument, a `let` value or an element of a
  container may be applied;
- a partial application, though eta-expanded, is computed when it is
  evaluated.

A binding whose computation comes round to demand itself is rejected at that
binding, naming the cycle, unless on the way a lambda that holds it in its
`where` blocks (directly or through nested blocks) is applied: each such
call computes it anew. This holds for each implementation of a term, and
through class methods.
