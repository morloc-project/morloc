# compare.py and the recorded runs

The benchmark harness against performance/README.md's measurement rules.
Found by the 2026-10-21 audit; full notes in
/work/plans/model-audit-2026-10-21/K-performance.md.

- **The default comparison never compares a bench with its own past.**
  [reproduced] Violates the README's regression level. With no arguments
  compare.py takes the two newest run directories (`runs_available()[-2:]`
  in `main`); every run holds one bench, so today it lines up copies
  (2026-10-19-1900) against pure-args (2026-10-20-0959): 36 one-sided rows,
  "0 REGRESS", exit 0. Limits are checked only for benches in the newest
  directory (TBL-1/2's 8 FAILs go unreported); run.sh ends with the same
  default. Fix shape: per bench, the newest earlier run holding it.
- **A missed Goal is not reported.** [reproduced] compare.py:171 appends
  `goal` only when met; `./compare.py 2026-10-16-2156-7dd39628+dirty-frame-stack`
  shows all six XL-1 pairs (22-38 us vs goal 20) with a blank status.
- **The load-average rule is not applied.** [read] Runs at load 9.27/9.28
  (2026-10-19-0111, -1900) serve as baselines; the README says such runs
  are not citable.
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
