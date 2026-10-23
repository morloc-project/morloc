# Guest languages

Guest-language signatures (GUEST) against the code. Reproduced on 0.109.0
(dev 3a238d96).

- **Futhark guest check takes any applied type as a scalar.** [reproduced,
  2026-10-21] Violates GUEST-2 and GUEST-3. `classifyMorloc`
  (CodeGenerator/Guest/Futhark.hs:449-452) ignores the head of an applied
  type, so `addone :: [I32] -> I32` against `entry addone (x: i32) : i32`
  passes and fails in g++ "invalid initialization of reference of type
  'const int32_t&' from ... 'const std::vector<int>'". `Vector n I32` is
  caught. Repro: /work/plans/model-audit-2026-10-21/repro/F/fut/g4.loc, g5.loc.
- **An unsourced Futhark entry with a `bool` parameter breaks every build
  of its file.** [reproduced, 2026-10-21] Violates GUEST-3 (which governs
  sourced signatures). `readManifest` converts every entry (Futhark.hs:331),
  and `bool` is missing from `elemTokenSet` (:207), so sourcing another
  entry fails "Futhark entry pick: unknown opaque type bool". Gap: GUEST-3
  says "fixed-width numeric scalars"; whether `Bool` is allowed is
  unstated. Repro: /work/plans/model-audit-2026-10-21/repro/F/fut/k.fut, g1.loc.
- **A GUEST-3 rejection names no term or location.** [reproduced,
  2026-10-21] "Futhark: unsupported type Int (only fixed-width numeric
  scalars, ...)". Repro: /work/plans/model-audit-2026-10-21/repro/F/fut/g1.loc.
- **Manual.** lang-futhark.asc covers only `backend`/`device`; it states
  none of GUEST-1..4.
