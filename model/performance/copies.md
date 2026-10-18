# Copies in generated code (COPY)

What the copies generated Rust and C++ make cost. Each item times one
copy-heavy shape and counts the copies it makes per step, so a change to
ownership in code generation shows up both as a count and as time. The
counts of every shape, including those that copy nothing, are pinned by
the goldens `copy-census-rust` and `copy-census-cpp`; the shapes and why
they copy are in `compiler/issues/codegen-copies.md` and
`codegen-ownership.md`.

A Rust copy is one link of `Chain`, a user-mapped type whose `Clone`
recurses and counts each link. A C++ copy is one payload list of 20000 or
more ints (80 KB), counted as an allocation of 64 KiB or more that the
payload builder did not make. Each series is timed at one size and at a
control that does the same setup with no steps (`.ctl` in the points
file), so `net` is the cost of the steps alone. Rust pools are built with
`rust:lto=off`, as the goldens are.

Every item uses the model `net = s * q * u`: `s` steps, `q` copies per step
(`per_step`), `u` the time per copy (`u`, including the step's other
work). `t` is the wall time of the timed run, nexus and pool start
included.

### COPY-1 A nested constructor pattern in a loop copies the value it steps through
Intent: proposed
Bench: copies
Model: net = s * q * u; Rust: a chain of n walked two links per step (s = n/2), q about n; C++: k walks of a 100-link chain, two links per step (s = 50k), q = 1
Measured: results/2026-10-17-1847-9f24abe4+dirty-copies-baseline: rs n=4000: t 0.236 s, net 0.202 s, per_step 4001, u 25.2 ns; cpp k=10000: t 0.822 s, net 0.779 s, per_step 1, u 1557 ns; results/2026-10-18-0115-38da1c87+dirty-s3-cpp-places: rs n=4000: t 0.0435 s, per_step 0; cpp k=10000: t 0.0547 s, per_step 0

Rust reads the inner field in place (OWN-6), so q = 0; the cited run
predates that, when the field was copied under both the tag test and the
projection and a walk was quadratic. C++ bound the inner payload to a
local by copy each step until its loop lets read projections in place
(OWN-6); both are now 0.

### COPY-2 A Rust loop stepped by `tl` copies the rest of its value
Intent: proposed
Bench: copies
Model: net = s * q * u; k steps of `tl` down a chain of n (s = k), q about n - k/2
Measured: results/2026-10-17-1847-9f24abe4+dirty-copies-baseline: k=1000 n=10000: t 0.274 s, net 0.238 s, per_step 9500, u 25.1 ns

The loop value is only read (`chainLen` at the end), yet each step copies
the rest of the chain that `tl` returns. The same loop in C++ copies
nothing (golden `copy-census-cpp`, `tlLoopRead`), so it has no series here.

### COPY-3 A list pattern in a loop copies the elements it binds
Intent: proposed
Bench: copies
Model: net = s * q * u; k steps reading both elements of a carried two-element list (s = k); Rust: chains of n, q = 2n; C++: q = 2
Measured: results/2026-10-17-1847-9f24abe4+dirty-copies-baseline: rs k=1000 n=5000: t 0.297 s, net 0.261 s, per_step 10000, u 26.1 ns; cpp k=250000: t 0.363 s, net 0.327 s, per_step 2, u 654 ns; results/2026-10-18-0115-38da1c87+dirty-s3-cpp-places: cpp k=250000: t 0.0468 s, per_step 0 (rs unchanged)

### COPY-4 A lambda copies the value it captures
Intent: proposed
Bench: copies
Model: net = s * q * u; Rust: a lambda capturing a chain of n mapped over k values, keeping it (s = k, q = n); C++: k loop steps each calling a lambda that captures a carried list (s = k, q = 1)
Measured: results/2026-10-17-1847-9f24abe4+dirty-copies-baseline: rs k=1000 n=10000: t 0.294 s, net 0.256 s, per_step 10000, u 25.6 ns; cpp k=500000: t 0.636 s, net 0.601 s, per_step 1, u 1202 ns

### COPY-5 A C++ loop carrying a rebuilt tuple copies its payload
Intent: proposed
Bench: copies
Model: net = s * q * u; k steps carrying `(.0 t, k)` (s = k), q = 2
Measured: results/2026-10-17-1847-9f24abe4+dirty-copies-baseline: k=250000: t 0.632 s, net 0.595 s, per_step 2, u 1190 ns

The tuple is built from its own projection each step, which a move could
hand over without copying.

### COPY-6 A C++ loop step that reads a value and builds onto it copies the value
Intent: proposed
Bench: copies
Model: net = s * q * u; a walk of a chain of n whose step past the second link reads the rest of the chain (`sumKeys 0 rest`) and builds `Link (mkL 0) rest` (s = n), q the payload copies per step
Measured: results/2026-10-18-0115-38da1c87+dirty-s3-cpp-places: n=400: t 0.0682 s, net 0.0247 s, per_step 2

The borrowing read and the keeping read of `rest` are in one step. Before
C++ read fields in place, the walk inside the step copied each payload it
read, so q grew with the rest of the chain and the walk was quadratic
(golden `copy-census-cpp`, `mixedLoop`: 5149 copies at n=100, now 198).
