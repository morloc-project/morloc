# Guest languages

Guest-language signatures (GUEST) against the code. Reproduced on 0.109.0
(dev 3a238d96).

- **An unsourced Futhark entry with a `bool` parameter breaks every build
  of its file.** [reproduced, 2026-10-21] Violates GUEST-3 (which governs
  sourced signatures). `loadManifest` converts every entry (Futhark.hs:344),
  and `bool` is missing from `elemTokenSet` (:219), so sourcing another
  entry fails "Futhark entry pick: unknown opaque type bool". Gap: GUEST-3
  says "fixed-width numeric scalars"; whether `Bool` is allowed is
  unstated. Repro: /work/plans/model-audit-2026-10-21/repro/F/fut/k.fut, g1.loc.
- **A GUEST-3 rejection names no term or location.** [reproduced,
  2026-10-21] "Futhark: unsupported type Int (only fixed-width numeric
  scalars, ...)". Repro: /work/plans/model-audit-2026-10-21/repro/F/fut/g1.loc.
- **Manual.** lang-futhark.asc covers only `backend`/`device`; it states
  none of GUEST-1..4.
