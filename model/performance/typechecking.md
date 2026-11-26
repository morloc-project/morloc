# Typechecking (TYCK)

`morloc typecheck` wall time against the size and shape of a generated
program. Every program imports root-py. `perf.py typechecking --check`
checks each series once at its second-largest size against a 10 s limit;
CI runs it to catch a blowup, not drift.

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
Limit: ladder.k <= 1.3, ladder-indexed.k <= 1.3, ladder-open-sibling.k <= 1.3, ladder-stuck-sibling.k <= 1.3

The sharing that makes this linear landed in compiler 31f11c77 (before it,
2^d). `ladder-indexed` runs the ladder over a type indexed by a row count
and a column schema (`Table n {...}`), which the sharing once refused to key.
`ladder-open-sibling` puts it over an index no use fixes, beside a
definition whose element type nothing fixes; `ladder-stuck-sibling` beside
a method use whose type is open until the method is expanded (`tally []`).
Either once cost every term of the export its sharing.

### TYCK-5 Calls to an imported function are linear in their number
Intent: proposed
Bench: typechecking
Model: t(K) = t0 + b*K, as TYCK-3 with `f` imported from a local module
Measured: pending
Limit: calls-imported.k <= 1.3

Issue #158 reported this as quadratic (256 calls: 15.8 s and 417 MiB,
against 0.7 s in one file); on 2026-11-26, 1024 calls took 0.22 s.

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

### TYCK-8 Parenthesized nesting is linear in its depth
Intent: proposed
Bench: typechecking
Model: t(n) = t0 + b*n; one expression nesting n applications `f (f (... x))`
Measured: pending
Limit: apply-nested.k <= 1.3, apply-annotated.k <= 1.3, apply-multiarg.k <= 1.3, tuple-nested.k <= 1.3

The series vary what opens each parenthesis: an argument (`apply-nested`),
an annotated argument `(e :: I64)` (`apply-annotated`), the function of a
two-argument call `f (e) 1` (`apply-multiarg`) and a tuple-returning call
(`tuple-nested`, n pairs). Three parser cases that all began at `(` once
backtracked into one another, 3^n (compiler 908eb90f0).

### TYCK-9 A composition chain is linear in its length
Intent: proposed
Bench: typechecking
Model: t(n) = t0 + b*n; `g = h . h . ... . h`, n uses of `h`
Measured: pending
Limit: compose.k <= 1.3, compose-poly.k <= 1.3, compose-partial.k <= 1.3

`h` is a signed identity (`compose`), an unsigned one (`compose-poly`) and a
partial application `inc = add 1` (`compose-partial`). Eta-expanding the
chain once made this exponential.

### TYCK-10 Signed definitions nested in one `where` are linear in their depth
Intent: proposed
Bench: typechecking
Model: t(d) = t0 + b*d; `g n = q_d n where q_i x = q_{i-1} x`, each `q_i` signed
Measured: pending
Limit: where-chain.k <= 1.3

Checking an application once discarded the function's synthesized type and
derived it again, doubling the cost per level of nesting (compiler
31f11c77).

### TYCK-11 Method uses of an open type are linear in their number
Intent: proposed
Bench: typechecking
Model: t(n) = t0 + b*n; n uses of `tally []` in one definition, `tally :: f a -> I64`
Measured: pending
Limit: stuck-uses.k <= 1.3

A use whose type stays open until its method is expanded is expanded in
place, with every use of that method; the cost is uses times instances.
Before compiler c10129011 one such use expanded the whole definition, and
the tree grew exponentially.
