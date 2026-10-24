# Realization

Defects in how the realizer (CodeGenerator/Realize.hs) prices a placement,
found reviewing /work/plans/realization-cost-model.md. The objective is
COST-1 (ruled); each entry is wrong under any reading of it. [read; not
reproduced]

- **A list, tuple or record is priced by its cheapest element.**
  `scoreMany` (~665-682) builds one merged table of every element's scores
  and takes the per-language minimum, which is right for alternatives of one
  term (`VarS (Many ..)`, one is chosen) and wrong for the components of a
  container (all must exist): in `[pyA, rustB]` placed in Rust, `pyA`'s
  crossing costs nothing whenever `rustB` is cheaper. Sum the components.
- **A use of a let-bound value is priced from where it was computed.**
  `refTable` (~529-557) prices a reference in language l as moving the value
  from its home h to l, but the value lives where the let runs; for a value
  computed in Rust, bound in a Python do-block and used in Rust, the second
  crossing costs nothing. lib/biolib rnaseq R36 is this shape.
- **A parameter is free in every language.** A non-function `BndS` scores 0
  everywhere (~514-519), so a value passed in and used in two pools is never
  priced.
- **Collapse and scoring price an edge differently.** Collapse picks a
  child's language with `transScore` (`cost`, ~776; `chooseLanguage`,
  ~1037), scoring with `crossScore`; once the payload term is non-zero the
  DP is no longer exact. `holding` (~420, ~432) also uses `transScore`.
- **A morloc-defined head prices its function arguments twice**, once
  inside its body (re-scoring through `rBndVars`, ~514-518) and again at the
  outer `scoreApp` (~502-505).
- **Merging two signatures keeps only the first one's docstrings.**
  `mergeEdocs x _ = x` (Frontend/Merge.hs ~83-84) and `weaveTermTypes`
  (~61-64): a directive on the second signature of one term is dropped,
  depending on order. Affects any directive, cost directives included
  (COST-8 requires both sets kept).
- **The cost model is not implemented.** [reproduced, 2026-10-21]
  COST-1..COST-11 have no code (COST-12, split do-blocks, is today's
  behaviour). `@wiresize`, `@dimension`, `@calls` and
  `@selftime` warn "docstring directive ... is not recognized" and are
  dropped; call and crossing costs are the integer tables in
  data/lang/languages.yaml (lang.yaml `cost:` is never read, see
  language-registry.md; Realize.hs `pairwiseCost`, `crossingPayloadCost _ =
  0`). Plan: /work/plans/realization-cost-model.md and the implementation
  plan it links. Unbuilt design, not a regression.
- **A constructor's arguments are scored and then ignored.** [read,
  2026-10-24] `scoreExpr` for `ConS` scores its children and returns
  `zipLang` (Realize.hs, the ConS case), so a constructor whose argument is
  computed in another pool is priced as if it moved nothing.
- **Every edge in one language charges the language's call cost.** [read,
  2026-10-24] `transScore l l` is `same_language_costs[l]`, added for every
  child that has a language, so a deep expression in Python costs
  more per node than the same expression placed in C++ even when nothing
  moves; COST-2 charges a call cost per call, not per edge.
- **A bracket step through an instance is never placed in a pool.**
  [read, 2026-10-21] Realize gives a PatCall no language (Realize.hs:521),
  so a pure `f m = .[0] m` on a receiver that is not a list on the wire
  (a newtype over a tuple with a sourced `Indexable` instance) stays on the
  nexus, which refuses it ("needs its instance, which no language here can
  call") although a pool could run it. Placement should require a language
  that has the instance whenever the receiver is not a list or IFile.
- **The nexus brackets a newtype over a list natively.** [read, 2026-10-21]
  Gap in PAT-5. `newtype Deque a = List a` with its own `Indexable`
  instance is a list on the wire, so the nexus indexes it as a list while a
  pool calls the instance; the two agree only if the instance means list
  indexing (true of stdlib `Vector`). Needs a ruling: an instance on a
  list-represented newtype must agree with its representation, or such
  brackets must go to a pool.
- **An argument computable in the caller is computed by a callback.**
  [reproduced, 2026-10-23] A cut is a subtree (Express.hs, the cross-pool
  AppS clause ~1674-1688, and PatCall ~1616, eta ~1714, expressContainer
  ~2347): in `inc (dbl (inc x))` with `inc` in Python and `dbl` in C++, C++
  calls back into Python for the inner `inc x`, two round trips where one
  would do; `inc (add (inc x) (inc y))` takes three. A `where` value used
  only inside the foreign argument is placed there too. EVAL-1/2 permit
  hoisting each maximal caller-language argument subtree that is data (not
  a function or suspension), has its free variables bound in the caller,
  and is reached only through strict positions (no conditional arm, lambda
  or `@try` region). Open in COST-13: whether EVAL-3 lets such a value be
  computed in the caller's pool.

- **A statement that only names a value is placed in another pool.**
  [reproduced, 2026-10-24] In a recursive do-block whose last bound value
  `b` comes from C++ and whose base case is `do b`, with the block driven
  from Python, the base case becomes a C++ manifold whose body returns its
  argument: Python sends `b` to C++ and C++ sends a copy back, one round
  trip per base case for no work (golden spec-cost-12-1, `loop`). The
  let-bound value is priced where it was computed rather than where it is
  used; boundary pricing (COST-3, COST-11) removes it.
