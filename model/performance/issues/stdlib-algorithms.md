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
