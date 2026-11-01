# Evaluation

- **No item says how often the work a function does before the function it
  returns runs.** [read, 2026-10-27] Spec gap beside EVAL-1, EVAL-3 and
  EVAL-6. The code stages such a function: applied to the arguments before
  the function it returns (`f a` for `f a = let k = g a in \b -> ..`, or a
  conditional or recursion choosing the function), the work runs once and
  the returned closure reuses it; the goldens staged-partial-application,
  staged-function-roots and staged-recursion-returns-function pin this,
  and arch-compiler-sharing.asc relies on it, but no spec item states it or
  cites those tests. Needs an item (likely under EVAL, next to EVAL-6, which
  asks the related question for a value used only inside a lambda).
