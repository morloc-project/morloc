# Effect boundaries

EBND items against the code, and the README rule that a boundary item
fails loudly naming itself.

- **The effect-boundary checker never visits four of its seven
  boundaries.** [read, 2026-10-21] Violates EBND-1. In
  CodeGenerator/EffectBoundary.hs `walk`/`descend` reach only `ExportRoot`,
  `LocalRoot` and `ForeignCalleeReturn`; `CallbackReturn`, `SourceCall`,
  `SerializeSink` and `ForeignCallerReceive` appear only in the data type,
  `boundaryExpectsPlain` and comments. So a lambda handed to a foreign
  consumer is never checked for a plain return, and since `mismatch` is
  False for every pass-through context, the suspend direction (a plain
  value where `<E> T` is declared) is never checked. Failures are
  `throwCompilerBug` messages that do not name EBND-1.
- **No check names a TC, EBND or EMIT item.** [read, 2026-10-21] Gap
  against compiler/README.md ("stops with an internal error naming the
  item"). `grep 'TC-\|EBND-' library/` finds nothing; EBND-1/2 throw
  unnamed compiler bugs; only EMIT-3's missing return names its item, via
  Haskell `error` (Imperative.hs:3112). EBND-3 states no checker and none
  exists (candidate: count `PolyEval` per path after insertion and after
  `lowerSuspensions`). Either the README rule becomes "items that have a
  checker", or each item gets one.
- **A label may move work into a suspension.** [read, speculative impact,
  2026-10-21] Suspension.hs `moveLabelIntoSuspension`: when a labeled
  manifold returns a suspension it did not build, the body becomes a
  closure running `PolyEval inner`, so `inner` (e.g. `f x`) is re-evaluated
  on every run. Run count is unchanged (EBND-3) but the pure work repeats.
  No item says whether a label may do this (ANN-10 covers placement only).
