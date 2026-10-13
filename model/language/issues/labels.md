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
