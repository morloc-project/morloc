# Evaluation

- [reproduced, 2026-11-04] A partial application whose work is a source
  call reached through a generic function runs that call once per use, not
  once where it is applied (APP-3). With `mkS :: Int -> a` (a source that
  returns a function), `apply1 f = f 1` and `each xs = map (apply1 mkS) xs`,
  the Python pool builds `apply1`'s specialized root as `\x ->
  make_adder1(1)(x)`, so `make_adder1(1)` runs once per element. A root
  stays a lambda over its inputs, so the call is not hoisted; the staging
  that hoists work before a returned function does not see it. The result
  is right and the source is pure (any effect is in its type), so the cost
  is time only. Candidate: stage a root whose body applies a value its
  parameters do not reach.
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
