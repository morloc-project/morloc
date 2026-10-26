# Realization

Defects in how the realizer (CodeGenerator/Realize.hs) prices a placement,
found reviewing /work/plans/realization-cost-model.md and since. The
objective is COST-1 (crossings by loop level, then language preference,
then size).

- **A reference to a let-bound function value is priced from where its
  definition is cheapest.** The `LetS` clause of `scoreExpr` prices a
  reference in language l as reaching the right-hand side's cheapest home
  from l, but a lambda is built in its parent's language (COST-11;
  `pinNamedLambda` at collapse), so scoring and collapse disagree. Data
  values are free at their references (COST-3).
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
- **The placement collapse builds does not always cost the least the scorer
  found.** [reproduced, 2026-10-26] Violates COST-11. `morloc typecheck -v
  --realize` rescores each pool tree as placed after normalization, before
  co-location of host-made functions (whose moves and copies are forced and
  not checked), and lists a difference; 7 golden `main*.loc` programs
  differ (closure-conformance main-home-*/main-away-*, shared-specialization
  main-mixed: "lambda in py not [cpp]"). All are nested lambdas that
  evaluation threaded through shared specializations: a function argument
  applied inside a lambda built in its parent's language is placed apart
  from where it is bound, and the scorer does not see the extra crossing.
- **A function argument that LambdaEval inlines is placed for where it is
  built, not where it runs.** [read, 2026-10-25] Most of the COST-11
  differences above. A lambda argument is collapsed in its parent's
  language, but `applyLambdas` (after collapse) substitutes an argument used
  at most once at its reference, where the scorer priced it; a closure that
  survives (used twice, forwarded to a recursion, put in a container) is
  priced by the scorer as living where its body is cheapest, though it
  lives where it is built. Reducing calls of single-lambda terms before
  scoring (normalizePop1) cut the differences from 31 to 14 but broke
  collapse's invariant that a lambda takes its builder's language
  (closure-conformance `mkE "abcde"`). Candidate fix: collapse an inlined
  argument at its reference site, and score a surviving closure's
  references per build language, memoized on the bound tables.
  Placement after normalization (Realize.hs `placeNormalized`) places the
  inlined argument where it runs; the choice of implementation is still
  made at the build site.
- **A crossing recursion given both a function made by host code and a
  value its pool cannot hold is refused.** [reproduced, 2026-10-26] Such a
  recursion keeps its serial entry (Express.hs `allHeld`: callers of a
  native entry pass one value per argument and cannot pass the packet an
  unheld value is carried as), so its self-calls would serialize the host
  function; the build stops with "a function made by host code would be
  serialized here". Supporting it needs native entries that take a packet
  for an unheld argument. Repro: a C++ host `cpp_feed(k)` calling `k` with a
  C++ function, `walk n g b ? n == 0 = b : pyBump (walk (cppSub n 1) g b) (g
  n)` with `b` a record mapped only to Python, `run n = cppFeed (\g ->
  pyBoxValue (walk n g (pyMkBox 0)))`; should print 60 for `run 3`.
- **A function value made out of sight of co-location can be applied to a
  host-made function in another pool.** [read, 2026-10-26] Co-location
  (Realize.hs `colocateHostFunctions`) builds in the host's language every
  function value it can see reach a host-made function: lambdas, and the
  function-holding arguments of a copied recursive helper. A function value
  that arrives through a parameter of an uncopied function, or as the result
  of a sourced call, and is then applied to a host-made function, may live in
  another pool; the call sends the host-made function (or a closure that
  captures it) there and fails at run time with the reify error. The
  compile-time refusal (Serialize.hs `refuseHostFunction`) catches only a
  host-made parameter serialized directly. No witness yet.
- **Co-location moves nodes into the host's language without checking that
  it can hold their values.** [read, 2026-10-26] `colocateHostFunctions`
  (`relang`, `relangAll`) places a node that uses a host-made function, a
  copied helper's body, and a copy's function-holding arguments in the
  host's language whatever their types; a value the host's language cannot
  hold (a record mapped only to Python, in C++) then fails at the pool's
  compile or later, instead of as a located refusal. Spec gap beside COST-11
  and HOST-9: what a program must do when the host's language cannot hold a
  value computed from a host-made function.
- **Placement after normalization can abort a build that placement before
  it accepted.** [read, 2026-10-26] `placeNormalized` scores and collapses
  every pool tree again and keeps no earlier placement, so an error raised
  while scoring or collapsing the normalized shape stops the build, as does
  a manifold language its tree's table does not offer (Realize.hs
  `pinTable`). `pickLanguage` can also return a recursion head's language
  that the node's table lacks (its `hostedWith` fallback), which collapse
  then cannot build. No witness yet.
- **Under `--debug`, a label or a cache, a same-pool recursion serializes
  its arguments.** [read, 2026-10-26] `splittable` (Express.hs
  `addNativeRecEntries`) refuses a body under a PolyDebugWrap, PolyCacheBody
  or Preserved manifold, so every self-call goes through AppRecS and
  serializes each argument (Serialize.hs RecCallP); a function made by host
  code among them fails at run time with the reify error, even when the
  whole recursion is in one language. `addLoopWraps` is also off under
  `--debug`.
- **A recursion reached only through back-edges marked as crossing gets no
  native entry.** [read, 2026-10-26] `addNativeRecEntries` keeps only
  targets of `(mid, Nothing)` back-edges, but segmentation can move a marked
  edge into its target's pool (Serialize.hs RecCallP comment), giving a
  same-pool call that serializes every argument.
- **A foreign body may be lowered with the caller's language.** [read,
  2026-10-26] Express, lambda whose body is one application with a head in
  another language: `expressPolyApp parentLang funExpr xs'` lowers the head
  in the caller's language though the body is emitted in the callee's
  (also the PatCall fallback near it). Harmless for a recursive call (a
  probe with the recursion in Python and the callback in Rust calls the
  helper directly in the Python pool); a pattern call would pick the
  caller's language's instances, which matters only for a newtype whose
  indexing instances differ by language. No witness yet.
- **The realizer's placements are checked only against themselves.**
  [read, 2026-10-25] COST-11. No test enumerates the placements of a small
  tree to confirm the DP finds the least; no test compares the crossings
  the placement listing shows with those code generation emits (the
  PolyRemoteInterface calls after lowering); there are no unit tests that
  pin a placement directly (PlacementTests.hs was planned). The consistency
  check of `typecheck -v --realize` runs in a few goldens only, not over the
  suite.
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

- **Instance resolution cannot compare Nat-dimensioned types.** [reproduced,
  2026-10-24] Collapse resolves instances by type before cost (COST-15)
  only when a term's implementations differ in type, because
  `compatibleType` does not match Nat-dimensioned types: resolving `pack`
  in golden tensor-comprehensive fails "No matching implementation found
  for 'pack' at type ((Int, Int), Vector 6 Real) -> Matrix 2 3 Real"
  (vector-comprehensive likewise). Implementations of differing
  Nat-dimensioned types would hit the same failure.
