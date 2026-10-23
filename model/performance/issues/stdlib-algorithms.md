# Standard library algorithms

- **`concat` and `concatMap` are quadratic.** [read, 2026-10-22] Gap: no
  performance item covers stdlib list functions. root/main.loc defines
  `concat = fold (<>) mempty`; root-rust's list `(<>)` is
  `morloc_list_add`, which copies both operands into a new vector
  (root-rust/core.rs), and `morloc_fold_vec` threads the accumulator by
  value, so `concat` of k lists of total length n copies O(k n) elements.
  Concatenating the transcript models of ~60k genes (~250k models) would
  copy ~10^10 elements. root-cpp's `morloc_list_add` (core.hpp:128) also
  copies both operands [measured 2026-10-23: tests/bioprojects sirna, which
  joins one model list per gene, ran 12% slower at 400 Mb with `concat`
  than with a linear flatten]. Python and R were not read. biolib uses its
  own linear `flatten` (util, Rust) instead, and sirna a local C++ one.
