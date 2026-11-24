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
  Haskell `error` (Imperative.hs:3349). EBND-3 states no checker and none
  exists (candidate: count `PolyEval` per path after insertion and after
  `lowerSuspensions`). Either the README rule becomes "items that have a
  checker", or each item gets one.
- **A label may move work into a suspension.** [read, speculative impact,
  2026-10-21] Suspension.hs `moveLabelIntoSuspension`: when a labeled
  manifold returns a suspension it did not build, the body becomes a
  closure running `PolyEval inner`, so `inner` (e.g. `f x`) is re-evaluated
  on every run. Run count is unchanged (EBND-3) but the pure work repeats.
  No item says whether a label may do this (ANN-10 covers placement only).
- [speculative, 2026-10-23] A loop's base leaves are forced with
  forceReturnPosition (EffectBoundary.hs goLoop), which descends into
  lambda-form manifolds; forceValuePosition does not. A tail loop whose base
  returns a closure with an effectful body may have that body forced.
- **Storing a sourced host function in a host-convention record field
  wraps it twice.** [reproduced, 2026-11-15] Performance, no item. With
  `record Ops where step :: Int -> <IO> Int` and `{step = bump, ...}`, where
  `bump` is a sourced C++ function (already in host convention), the field
  holds an adapter that calls morloc's view of `bump`, which builds a
  suspension (a `std::bind` allocation and a frame push) and then runs it:
  16.8 ns per call from a C++ source against 1.7 ns for `bump` in a
  `std::function` (10M calls, 3 runs). The adapter could store a sourced
  function whose host form matches the field as it is. The same holds for
  user-written structs. Repro: a C++ source taking `const Ops&` and timing
  `o.step(acc + i)` over 10M iterations with `std::chrono::steady_clock`.
