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
- **A value read across a boundary is charged no per-element or per-byte
  cost.** Deferred 2026-10-25 (COST-3): `crossingPayloadCost` is 0 and no
  node records the sizes of the free variables a subtree carries down.
- **Collapse and scoring price an edge differently.** Collapse picks a
  child's language with `boundary` (`cost`, `chooseLanguage`, `holding`,
  `holdingLang`), scoring arguments with `crossScore`. The two agree while
  a crossing costs its latency only; they part once COST-3's payload term
  is built (deferred 2026-10-25).
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
  found.** [reproduced, 2026-10-25] Violates COST-11. `morloc typecheck -v
  --realize` rescores the built placement and lists a difference; 32 of
  1336 golden programs differ. Seen: a lambda built in its parent's
  language around a body in another (closure-conformance,
  let-captured-by-closure, recursion-crosspool-*, records-*,
  string-encoding-utf8, uint8-cross-language), the extra crossings counted
  at a callback's call count; function values in a container placed with
  the container (functional-data-1: `funf`, `fung`); a sourced call left in
  a lambda's language rather than the scorer's (nested-closure-crossing,
  `pyHofRustArg`, 3 crossings built against 1); a recursive call forced to
  its definition's language (recursion-crosspool-io-tuple, crosspool-
  mutual-tail-loop), which COST-11 allows but the check does not yet
  exempt; and a built placement cheaper than the least found
  (recursion-crosspool-mutual, recursion-crosspool-mutual-list), where the
  scorer overprices mutual recursion; a let whose value only one language
  can hold placed in another (placement-representability topConstant and
  whereBinding: a C++ record let in Python). Not yet classified:
  binding-evaluation-count, errors,
  packets-interop, spec-eff-8-1, remote-streaming-consolidated,
  shared-specialization (main-mixed), spec-cost-12-1, spec-newt-7-8.
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
  LambdaEval now moves a substituted lambda to its reference's language, so
  the program builds, but the lambda's body keeps the placement collapse
  chose for the old site: forwarded through a helper into a Rust callback,
  `show` runs in Python with a crossing per element where the direct form
  calls Rust's own `show`.
- **The cost model is partly implemented.** [read, 2026-10-24] Scores are
  seconds: a call costs its language's coefficient times its self time
  (`@selftime` on its signature or `source` block item, else 1 us) plus
  the language's overhead (COST-2), a crossing between pools its pair's
  latency (COST-3 without the per-element and per-byte slopes), a
  recursive definition its body times 1000 (COST-14), a function argument
  of a sourced function 1000 calls of it (COST-4). Sizes (COST-5, COST-8,
  COST-9) are element counts only: `@dimension` above a signature's
  argument or result binds names from a list literal's length, a let
  variable's value, a sourced call's result `@dimension`, or COST-9's
  default of 1000 per level. Not yet: `@dimension` above a type alias or a
  record field (rejected), a type variable as a factor (rejected), a
  consumer's literal factor beating a default (COST-8 rule 3, ignored), a
  parameter's size from its enclosing signature (COST-8 rule 1, default
  used), a morloc-defined call's result size (default used), and a
  literal `Nat` length in a type (`Vector 5 Int`) as the default count.
  `@wiresize` and byte sizes are deferred with COST-3's copy cost
  (2026-10-25). A function value
  inside another type handed to a sourced function is priced as one
  call.
- **A recursive helper whose body crosses pools cannot take a function
  made by host code.** [reproduced, 2026-10-26] Its manifold gets no native
  entry (Express.hs `addNativeRecEntries`, the `crosses` gate in
  `splittable`), so a self-call in the same pool passes every argument
  serialized, and serializing a host-made function fails at run time
  ("cannot reify a function that morloc did not create"). The same helper
  with a body in one language works (native entry). Repro: R `feed <-
  function(k) k(function(x) x * 2L)`, `total n g ? n == 0 = 0 : pyAdd (g n)
  (total (pySub n 1) g)`, `recursive n = feed (\g -> total n g)` with the
  helper placed in R. Also blocks stream-r (`pullSource` given R's `sink`).
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
  it accepted.** [read, 2026-10-26] `placeNormalized` scores each pool tree
  again; it keeps the old placement when the new one cannot be built
  (`finish` returns Left, or the root's language would change), but an error
  raised while scoring the normalized shape stops the build. No witness yet.
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
- **A repetition count scales the tie-breakers too.** [read, 2026-10-25]
  `scaleScore` multiplies a score's crossing and switch counts by the same
  count as its time (a callback's 1000 calls, a recursion's 1000 levels),
  so two placements whose times tie are compared by crossings counted a
  thousand times over in one branch and once in another. COST-1 breaks ties
  by the number of crossings; whether a repeated crossing counts once or per
  repetition there is not stated.
- **The realizer's placements are checked only against themselves.**
  [read, 2026-10-25] COST-11. No test enumerates the placements of a small
  tree to confirm the DP finds the least; no test compares the crossings
  the placement listing shows with those code generation emits (the
  PolyRemoteInterface calls after lowering); there are no unit tests that
  pin a placement directly (PlacementTests.hs was planned). `@calls` on a
  suspension argument and the largest-count rule across implementations
  that state different counts have code but no test. The consistency check
  of `typecheck -v --realize` runs in five goldens only, not over the suite.
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
