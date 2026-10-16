# model/performance/

What morloc costs: the time and space each operation takes, as a function
of the sizes that drive it. Every item is a model with fitted parameters,
and every parameter comes from a benchmark in `benchmarks/`. An item with no
measurement says `pending`; it is a hypothesis until a benchmark fills it.

Chapters:

- `startup.md` (START): nexus start, pool start per language, pools started together
- `cross-language.md` (XL): cost of a call between pools, by language pair and payload
- `encoding.md` (ENC): CLI input and output forms; bytes per element on the wire
- `tables.md` (TBL): reading a table argument against a plain Rust reader; time and peak memory
- `compression.md` (ZIP): stream write and read throughput by compression level
- `parallelism.md` (PAR): `pmap` scaling with workers; per-element dispatch overhead
- `typechecking.md` (TYCK): `morloc typecheck` time against program size and shape
- `build.md` (BUILD): `morloc make` time per language
- `recursion.md` (DEPTH): recursion depth in morloc against the native depth in each language

## Items

The format of `model/README.md`, with these fields:

    ### XL-1 A foreign call costs about 20 us per round trip
    Intent: ruled 2026-10-13
    Bench: xl-calls
    Model: t(n) = c + b*n; n calls, b the cost of one round trip
    Measured: pending
    Goal: py->cpp.b <= 20 us
    Limit: py->cpp.b <= 40 us

    Prose: what the model covers and what it leaves out.

- `Intent` is `ruled <date>` when the user has set the goal and limit,
  `proposed` when they only record what was measured, `open` when the
  shape of the model itself is undecided.
- `Bench` names the directory in `benchmarks/` that produces the evidence.
- `Model` gives the form, with every variable defined. Prefer a form a
  reader can compute with: `t = c + b*x` with units, not "linear".
- `Measured` cites the run directory in `benchmarks/results/` and the
  fitted values, or says `pending`. Values from a run with a load average
  above half the core count are not citable.
- `Goal` and `Limit` list `<param> <= <value> <unit>` (or `>=`), comma
  separated; `<param>` is a name the benchmark records for this item.
  Either may be absent.

Every parameter is held to three levels:

1. The goal: where the code should be. Missing it is reported, never a
   failure.
2. The limit: where the code must be. Breaking it fails the comparison.
3. The past: the same parameter in earlier runs. Getting worse than the
   previous run by more than the tolerance (default 10%) fails the
   comparison, long before a limit is reached. This is the regression test. Lower is
   better unless the item's Limit or Goal says `>=` or the parameter is a
   speedup.

`benchmarks/compare.py` prints every run given to it side by side with the
goal, the limit and a status per parameter. A model that a measurement
contradicts is not loosened to fit: either the code regressed (record an
issue in `runtime/issues/`) or the model's form is wrong (mark it `open`
and say how).

## Measurement rules

- Benchmarks run on a quiet machine. Each records the load average when it
  starts; check it before trusting a result.
- A sweep is interleaved: each repetition visits every size once, and the
  best time per size is fitted. Load that changes during a sweep then
  shifts every point instead of tilting the slope.
- Wall time comes from the harness, never from memguard (it samples every
  0.2 s). Peak memory is `ru_maxrss` of the waited process.
- A benchmark measures what a user gets: a built program through `./nexus`,
  so the fixed cost of each point includes nexus and pool start (START).
  Slopes are what the item is about; the intercept is START's.
- Every cross-language slope has an in-pool control in the same run, so the
  item can state the crossing's own cost.
