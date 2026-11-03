# Conditional sequences (CHAIN)

Build and run cost of a long chain of `&&` operands or of guards, in each
pool language and in a command the nexus evaluates itself. These measure
the flat forms of EMIT-4 and EMIT-5 (`compiler/codegen/emit.md`): a chain
must build, load and run at any length, and cost no more per element as it
grows. Every element is evaluated: the last one decides the chain.

Build time is the wall time of `morloc make`. Run time in a pool is the
per-call mean of the labeled chain from the benchmark summary (ANN), over
200 calls in one command with the slowest call dropped, so pool start,
loading and a one-time compile are not in it. A command the nexus
evaluates cannot carry a label; its run time is the slope of the command's
wall time, which includes reading the larger manifest.

### CHAIN-1 Building a chain of `&&` is linear in its length
Intent: proposed
Bench: chains
Model: t(n) = c_L + b_L*n; n operands `x >= i` (nexus: `a`), L the language
Measured: pending
Limit: and-py.make.k <= 1.3, and-r.make.k <= 1.3, and-cpp.make.k <= 1.3, and-rs.make.k <= 1.3, and-nexus.make.k <= 1.3

`k` is the exponent of t - c against n; 1 is linear. Typechecking the
operands is part of `b` (TYCK-6).

### CHAIN-2 Building a chain of guards is linear in its length
Intent: proposed
Bench: chains
Model: t(n) = c_L + b_L*n; n arms `? x == i = i` (nexus: `? b = i`)
Measured: pending
Limit: guard-py.make.k <= 1.3, guard-r.make.k <= 1.3, guard-cpp.make.k <= 1.3, guard-rs.make.k <= 1.3, guard-nexus.make.k <= 1.3

### CHAIN-3 A chain of any measured length runs
Intent: proposed
Bench: chains
Model: max = the largest chain built and run, with the expected result
Measured: pending
Limit: and-py.run.max >= 2000, and-r.run.max >= 2000, and-cpp.run.max >= 2000, and-rs.run.max >= 2000, and-nexus.run.max >= 2000, guard-py.run.max >= 800, guard-r.run.max >= 800, guard-cpp.run.max >= 800, guard-rs.run.max >= 800, guard-nexus.run.max >= 800

The bench fails outright on a chain that does not build or run; the
parameter records how far it went.

### CHAIN-4 Each element of a chain costs a constant to run
Intent: proposed
Bench: chains
Model: t(n) = c + b*n per call; b the cost of evaluating one element
Measured: pending

A flat chain costs what the same tests written in sequence cost natively.
In C++ and Rust the label's own cost, about 1.4 us a call, hides `b` at
these lengths.

### CHAIN-5 Compiling a chain on first use is linear in its length
Intent: proposed
Bench: chains
Model: w(n) = how much longer the slowest of the 200 calls takes than the others, against n
Measured: pending
Limit: and-r.first.k <= 1.3, guard-r.first.k <= 1.3

R byte-compiles a closure on its second call. Its compiler is superlinear
in the nesting depth of an expression, and R parses `a && b && c` as
nested calls: one run of 32, 64, 128 and 256 operands takes 41 ms,
129 ms, 505 ms and 2.6 s to compile, while 1024 operands in runs of 16,
each behind a flag, take 1.2 s, and 1024 plain statements 0.7 s
(`compiler::cmpfun`, outside morloc, 2026-11-03).
Python, C++ and Rust compile ahead of the first call; their `w` is noise.
