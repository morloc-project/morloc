# Typechecking (TYCK)

`morloc typecheck` wall time against the size and shape of a generated
program. Every program imports root-py.

### TYCK-1 Loading the standard library is the fixed cost
Intent: proposed
Bench: typechecking
Model: t0 = time to typecheck one definition importing root-py
Measured: pending
Limit: empty <= 1000 ms

### TYCK-2 A chain of definitions is linear in its length
Intent: proposed
Bench: typechecking
Model: t(n) = t0 + b*n; `f_i x = f_{i-1} (x + i)`, n definitions
Measured: pending
Limit: chain.k <= 1.3

`k` is the exponent of t - t0 against n (t = t0 + a*n^k); 1 is linear.

### TYCK-3 Calls to a function in the same file are linear in their number
Intent: proposed
Bench: typechecking
Model: t(K) = t0 + b*K; a list of K calls `f 7`
Measured: pending
Limit: calls-same.k <= 1.3

### TYCK-4 Naming a definition twice per level does not compound
Intent: proposed
Bench: typechecking
Model: t(d) = t0 + b*d; `f_i x = f_{i-1} x + f_{i-1} (x + 1)`, 2^d paths through d definitions
Measured: pending
Limit: ladder.k <= 1.3

The sharing that makes this linear landed in compiler 31f11c77 (before it,
2^d). The whole-export fallback that re-arms 2^d when a sibling has an
unsolved type is not covered by this bench.

### TYCK-5 Calls to an imported function are linear in their number
Intent: proposed
Bench: typechecking
Model: t(K) = t0 + b*K, as TYCK-3 with `f` imported from a local module
Measured: pending
Limit: calls-imported.k <= 1.3

Issue #158 reports this as quadratic (256 calls: 15.8 s and 417 MiB,
against 0.7 s in one file). Until it is fixed this limit fails.

### TYCK-6 Class method uses in one definition are linear in their number
Intent: proposed
Bench: typechecking
Model: t(n) = t0 + b*n; `g x = x > 0 && x > 1 && ...`, n uses of an Ord method in one signed definition
Measured: pending
Limit: method-gt.k <= 1.3, method-gt.b <= 2 ms

Each use checks the instances it may resolve to. Before compiler 5ad090eb2
each check trimmed the whole typing context, which grew with the
definition, so this was quadratic.

### TYCK-7 A method use costs about what a call does
Intent: proposed
Bench: typechecking
Model: t(n) = t0 + b*n; `g x = x != 0 && x != 1 && ...`, n uses of `!=` in one signed definition
Measured: pending
Goal: method-ne.b <= 0.2 ms
Limit: method-ne.k <= 1.3, method-ne.b <= 2 ms

`!=` has candidate instances whose own bodies use `==` again (tuples,
lists), so a use that copies and checks every candidate body costs far
more than one that does not: 36 ms a use while checking a signature
copied the candidates' bodies, 0.4 ms once it did not (2026-11-03). A
plain call costs `calls-same.b` (TYCK-3), the goal.
