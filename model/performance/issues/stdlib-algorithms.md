# Standard library algorithms

- **Streaming fold-maps are quadratic for copying monoids.** [read,
  2026-10-28] Gap: no performance item covers stdlib list functions.
  parallel's `psfoldMapWith` and `psfoldMapWithPure` combine each batch into
  a running accumulator with `<>`; list and Str `<>` copy both operands in
  every root-* module, so folding a stream of n elements into a list or Str
  copies O(n^2) elements. The in-memory `concat`, `concatMap`,
  `intercalate` and `pfoldMapWith` go through Monoid's `mconcat`, which is
  one pass for Str, List, Deque and Set; a stream cannot reach it without
  holding every part, so a fix combines each batch with `mconcat` and keeps
  the running accumulator's copies per batch rather than per element.

- **R `sortBy` is a bubble sort.** [read 2026-11-05] root-r
  `morloc_sortBy` (root-r/R/core.R:334-349) is O(n^2) comparator calls.
  Any rule that routes `sort` through `sortBy (<=)` (S2 ruling Q4) makes
  R's `sort` quadratic until it is a merge sort.

- **Python and R `length` is a morloc fold.** [reproduced 2026-11-13]
  root `length` (root/main.loc:315-316) has no native Python or R source,
  so it folds element by element: about 1-2 us per element in R (counting
  64 MiB of Reals: 16.9 s against 0.64 s with a native count), about
  100 ns in Python. Fix: source `len` in root-py and `length` in root-r.
