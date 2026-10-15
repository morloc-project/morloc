# Ownership in generated pool code

Where a pool takes a value instead of copying it, and a later read of the
same name sees what was taken.

- **Deciding a take walks the manifold once per candidate.** [read] Cost,
  not correctness; gap in the ownership rules (no spec item says when a
  projection may take). `consumableProjectionLets`
  (library/Morloc/CodeGenerator/Grammars/Translator/Imperative.hs) counts
  uses and then runs `moveWalk` for each surviving candidate over the
  whole top-level body, and at every nested manifold or do-block the walk
  re-walks that subtree (`rnodeBinds`, `rnodeNames`). One pass in
  evaluation order with a verdict per subject could replace both the count
  (with its throw-arm exemption and max over arms) and the per-candidate
  walks.
- **A `Try` bound from a suspension's result is copied when unwrapped.**
  [read] Cost, not correctness; same gap. `buildsFreshValue` accepts only
  a `@try` or a constructor literal as the subject, so `r <- pull` followed
  by `match r | (Err e) = @throw e | (Ok xs) = ...` (lib/stdlib parallel
  `psconcatMapWithPure` and its siblings) copies `xs`, a whole batch per
  step of a stream drain. The value a forced suspension returns is the
  frame's own, but whether a `<IO> (Try e a)` the program passed in may
  share its payload with another holder is what a rule would have to say.
- **`otherArmCpp` copies though no path reads the payload after a take.**
  [reproduced] Cost; same gap. In test-suite/golden-tests/projection-consume,
  `match t | (Ok v) = lenCpp v | (Err _) = lenCpp (match t | (Ok w) = w | ...)`
  keeps both projections copies; cause not traced.
