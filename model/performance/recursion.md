# Recursion depth (DEPTH)

How deep recursion goes in morloc against the same recursion written
natively in each language. These are the measurements for RECUR-2 and
RECUR-4 in `language/recursion.md`. A depth is the largest n for which a
recursion n levels deep returns n, found by bisection. The native program
is the sourced function run by a plain driver in the same language; C++ and
Rust are built with the pools' optimization level and run on a pool
worker's stack size, so a ratio compares frames, not stacks (RECUR-5).

### DEPTH-1 Recursion written in morloc reaches the native depth
Intent: ruled 2026-10-16
Bench: recursion-depth
Model: ratio_L = native_L / morloc_L; native_L the depth of the native function in language L, morloc_L the depth of the same non-tail recursion written in morloc and realized in L
Measured: results/2026-10-16-1616-ba28426d+dirty-recursion-depth-baseline: py 2.01, r 18.2, cpp 13.9, rs 10.9
Goal: py.ratio <= 1.1, r.ratio <= 1.1, cpp.ratio <= 1.1, rs.ratio <= 1.1

The ratio is the number of native frames, or bytes of native stack, morloc
spends per level of recursion against the one frame a native function
spends.

### DEPTH-2 Morloc adds a constant to recursion in sourced code
Intent: ruled 2026-10-16
Bench: recursion-depth
Model: gap_L = native_L - sourced_L; sourced_L the depth of the native function sourced into morloc and called once
Measured: results/2026-10-16-1616-ba28426d+dirty-recursion-depth-baseline: py 2, r 15, cpp -1029, rs -1127
Limit: py.gap <= 50, r.gap <= 50, cpp.gap <= 2000, rs.gap <= 2000

The frames morloc places below a sourced function are a constant, so the
gap does not grow with depth. A negative gap in C++ and Rust is stack
accounting: a native main thread's stack also holds the environment and
the loader's frames, a worker thread's does not.

### DEPTH-3 Tail recursion runs in constant stack
Intent: ruled 2026-10-16
Bench: recursion-depth
Model: tail_L = the largest depth up to 10^6 that tail recursion written in morloc reaches in L
Measured: results/2026-10-16-1616-ba28426d+dirty-recursion-depth-baseline: 10^6 in every language
Limit: py.tail >= 1000000, r.tail >= 1000000, cpp.tail >= 1000000, rs.tail >= 1000000
