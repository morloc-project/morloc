# Internal invariants checked silently or not at all

- **Plain `zip` truncates.** [read, 2026-10-23] About 340 `zip`/`zipWith`
  calls in library/ pair lists the compiler assumes have equal length; a
  mismatch drops elements instead of failing. One instance (a command's
  documented arguments vs its wire forms in Nexus.hs validateArgSpecs and
  argsJson) now fails as an internal error. Morloc.Internal already has
  safeZip/safeZipWith/safeZipWithM; sweep the zips over lists whose lengths
  are an invariant to those, failing loudly.
