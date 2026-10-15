# Ownership in generated pool code

Where a pool takes a value instead of copying it, and a later read of the
same name sees what was taken.

- **The `Ok v` / `Err e` unwrap of a fresh `@try` result always copies the
  payload.** [read] A copy, not a correctness fault; gap in the ownership
  rules (no spec item says when a projection may take). `consumableProjectionLets`
  (library/Morloc/CodeGenerator/Grammars/Translator/Imperative.hs) requires
  the subject's only use to be the projection, and sums uses across
  branches, so `match t | (Ok v) = f v | (Err e) = @throw e` counts two
  projections on mutually exclusive paths and copies `v`. This is the
  stdlib's usual unwrap (internal/main.loc, parallel/main.loc). Candidate:
  replace the count with the evaluation-order walk alone (`moveWalk` in
  Imperative.hs, which now filters the count's candidates:
  a let's right-hand side before its body, condition before arms, the max
  over branches rather than the sum, suspensions and closures that name
  the subject poisoned). The count's throw-arm exemption then goes too.
  `moveWalk` re-walks nested manifolds once per candidate; one pass with
  a verdict per subject would cost what the count costs.
