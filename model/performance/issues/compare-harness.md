# compare.py and the recorded runs

The benchmark harness against performance/README.md's measurement rules.
Found by the 2026-10-21 audit; full notes in
/work/plans/model-audit-2026-10-21/K-performance.md.

- **The 10% tolerance is below run-to-run noise for ZIP rates.**
  [reproduced] Two compression runs of one commit (7306021b+dirty, 14:30
  and 17:25) give 22 REGRESS (real-walk.z3 +23%, u64-uniform.z1 -41%);
  negative fits give "-156% better".
- **Five benches have never run.** startup, xl-payload, par-scaling,
  typechecking, build: the Limits of START-1..3, XL-3, PAR-1/2 and
  TYCK-1..5 have never been checked.
- **Benchmarks use `Int`** against the 2026-10-13 ruling: build/bench.py,
  typechecking/bench.py, and par-scaling through parallel.bench's
  `Int -> ...` seqMap/parMap (PAR-2's overhead is affected).
- **XL-2's native control is optimized away for C++ and Rust.**
  [measured] results/2026-10-16-2156: native-rs b = -0.137 ns (r2 0.38),
  native-cpp 0.41 ns per iteration; the `native` loops in
  benchmarks/xl-calls/f.rs and f.hpp are folded, so `w_L` cannot be fitted.
  Fix shape: `black_box` / an opaque call.
