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
