# Realization

Defects in how the realizer (CodeGenerator/Realize.hs) prices a placement,
found reviewing /work/plans/realization-cost-model.md. The objective is
COST-1 (ruled); each entry is wrong under any reading of it. [read; not
reproduced]

- **A reference to a let-bound function value is priced from where its
  definition is cheapest.** The `LetS` clause of `scoreExpr` prices a
  reference in language l as reaching the right-hand side's cheapest home
  from l, but a lambda is built in its parent's language (COST-11;
  `pinNamedLambda` at collapse), so scoring and collapse disagree. Data
  values are free at their references (COST-3).
- **A value read across a boundary moves for free.** A parameter or a
  let-bound value read by a subtree in another pool rides that subtree's
  one boundary latency (COST-3), but its per-element and per-byte cost is
  never added: `crossingPayloadCost` is 0 and no node records the sizes of
  the free variables it carries down.
- **Collapse and scoring price an edge differently.** Collapse picks a
  child's language with `boundary` (`cost`, `chooseLanguage`, `holding`,
  `holdingLang`), scoring arguments with `crossScore`; once the payload term
  is non-zero the DP is no longer exact.
- **A function argument is priced by where it is called, so a term
  with both morloc and sourced implementations prices it twice for the
  morloc one.** [read, 2026-10-24] The `AppS` clause of `scoreExpr` leaves
  out of the application's price only the function arguments every
  implementation's lambda binds (`boundArgs`, the fewest over
  alternatives); with one sourced alternative that is none, so the
  argument is priced at the application and again inside the morloc
  alternative's body. Each alternative needs its own argument pricing
  (plan step e3b).
- **Building a function value a morloc head never calls costs nothing.**
  [read, 2026-10-24] A function argument bound by the head's lambda is
  priced only where the body calls it; one the body ignores, such as a
  partial application whose data arguments are expensive, is never priced
  even if code generation still builds it.
- **Scoring and collapse decide separately which nodes stay in their
  parent's language.** [read, 2026-10-24] COST-11 lists the nodes code
  generation builds in their parent's language. Scoring prices as pinned
  a constructor application, a container of function values and a label
  around either (`pinnedToParent`); collapse pins those plus a lambda, a
  function argument carried into a sourced call (`collapseCarried`), an
  implementation that is functional data (`handleOne`) and the language of
  a named lambda (`pinNamedLambda`), each by its own rule. One predicate
  both consult would keep them from drifting apart.
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

- **Cost chooses between instances before their types do.** [reproduced,
  2026-10-24] Collapse keeps the cheapest implementations of a term
  (`minsBy` in the `VarS` clause of `collapseExpr`) and only resolves
  instances by type among ties (`resolveInstances`), so when an instance of
  a less specific or incompatible type is cheaper it is taken without a
  type check; COST-11 says instance resolution chooses first. Resolving
  types first is blocked on `compatibleType`, which does not match
  Nat-dimensioned types: with types first, golden tensor-comprehensive
  fails "No matching implementation found for 'pack' at type ((Int, Int),
  Vector 6 Real) -> Matrix 2 3 Real" (vector-comprehensive likewise). The
  recursion count (COST-14) makes cost differences between instances
  larger, not new.
