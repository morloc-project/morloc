# Labels

What a label (`lab@f`) wraps. ANN-7 to ANN-12.

- **Stage work of a labeled partial application is not measured.** [reproduced]
  `map (l@st 1) xs` with `st a = let k = slow a in \b -> ...` runs `slow 1`
  once, outside the hooks; each call of the result is one event. No item
  states staging yet (a gap in `evaluation.md`); the rule the user gave for it
  says log and benchmark labels cover the staged work as well as the calls.
  Golden `spec-ann-7-1` (`staged`) pins the count, not the stage measurement.
  Deferred by the user 2026-10-13.
- **Constants a labeled function computes run outside its hooks.** [speculative]
  A constant a definition computes is evaluated once per command (Share), so
  `l@h x` with `h x = g x big where big = load 0` evaluates `big` before the
  hooks, and in the parent under a remote label. Open: is such a constant
  part of one call of `h` (ANN-7)?
- **A cache entry is per call site.** [reproduced, 2026-10-21] Gap in
  ANN-4. `seqf x = do { a <- c@say x; b <- c@say x; a + b }` with `c:
  {cache: true}` runs `say` twice and stores two keys; two runs of one
  `where`-bound suspension in one command also miss. runs-caching.asc:7-10
  says the next equal call is served "in this run or any later one".
  Candidates: key per function and argument values; or per call site, and
  ANN-4 says so. Repro: /work/plans/model-audit-2026-10-21/repro/E/ann/s2.loc, s3.loc.
