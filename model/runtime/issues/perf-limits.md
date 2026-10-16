# Performance limits that do not fit what they measure

- [reproduced] XL-2 sets `r->r.b <= 50000 ns`, but the benchmark's native R
  loop costs 83182 ns per call by itself (results/2026-10-16-2156-7dd39628+
  dirty-frame-stack): `I64` is a `bit64` integer64 in R, whose `+` and
  `as.integer64` dispatch cost about 80 us per iteration. morloc adds about
  0.85 us. Either the limit becomes an overhead bound (`w_R`), or the
  benchmark uses a type R adds natively. Needs a ruling.
