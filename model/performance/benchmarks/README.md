# Performance benchmarks

Each directory is the evidence for the items that cite it in `Bench:`. It
holds `bench.py` (`run(b)`, using the harness in `perf.py`) and the morloc
sources it builds.

    ./run.sh                     # every benchmark, then compare.py
    ./run.sh xl-calls --reps 7   # one benchmark
    ./run.sh --label before-fix  # name the run
    ./run.sh startup --quick     # smoke test: two sizes, two reps, nothing saved

Run on a quiet machine: nothing else compiling or testing. A full run takes
roughly an hour.

A run writes `results/<date>-<commit>[-<label>]/`, one
`<name>.points.tsv` (every timed point) and `<name>.fit.tsv` (fitted
parameters by item) per benchmark. The first line of each records date,
load average, morloc version, compiler commit and CPU. Commit the run
directories of quiet-machine runs: they are the baseline the next run is
compared against. After a run, copy the fitted values an item cites into
its `Measured:` line.

    ./compare.py                         # latest run against the one before
    ./compare.py RUN_A RUN_B RUN_C       # any runs, oldest first
    ./compare.py --last 5 --item XL-1    # one item across the last five
    ./compare.py --changed               # only rows with a status

`compare.py` exits 1 when the newest run breaks a `Limit:` or is worse than
the previous run by more than `--tolerance` (default 0.10). Comparing runs
from different hosts compares the hosts; each run's header names its CPU.

| bench | items |
|---|---|
| startup | START-1..3 |
| xl-calls | XL-1, XL-2 |
| xl-payload | XL-3, XL-4 |
| encoding | ENC-1..3 |
| compression | ZIP-1..7, ZIP-10 |
| table-read | TBL-1, TBL-2 |
| par-scaling | PAR-1, PAR-2 |
| typechecking | TYCK-1..5 |
| build | BUILD-1 |
