# Realization

Defects in how the realizer (CodeGenerator/Realize.hs) scores a placement,
found reviewing /work/plans/realization-cost-model.md and since. The
objective is COST-1 (crossings by depth, then switches, then language
preference, then size).

- **Remaining gaps around values a pool cannot hold.** [read 2026-11-04]
  COST-15 condition 1 is not yet a limit before comparison. A value a
  pool only relays is a packet (Serialize.hs `inferHeld`,
  `heldSerial`, `letState`), but the universal-inference branches
  (`prepareArg`, `contextArg`, `boundArg` on a `Left` typemap entry) have
  no index to check with and still infer the native type. A real native
  use of an unmapped type fails "Could not resolve type applied variable"
  (TypeEval.hs) naming no language. `letWrap.wrapAsNeeded` returns the
  body unchanged for a native request on an untyped serial slot. biolib's
  convert test (convert/test/main.loc:243, Py Tuple4) was not re-run.
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
- **A function argument that LambdaEval inlines has its implementation
  chosen where it is built, not where it runs.** [read, 2026-10-25]
  Placement after normalization (Realize.hs `placeNormalized`) places an
  argument `applyLambdas` inlines where it runs, but the choice of
  implementation is still made at the build site. Reducing calls of
  single-lambda terms before scoring (normalizePop1) broke collapse's
  invariant that a lambda takes its builder's language
  (closure-conformance `mkE "abcde"`).
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
- **A copy of a recursive function given a host-made function is built in
  the host's language without checking that it can hold its values.**
  [reproduced 2026-11-02: a tail recursion copied into C++, carrying a
  Python-only record, emits C++ naming an undeclared `Box` (g++ fails);
  `inferType` in a compiled language resolves an undeclared record to its
  morloc name instead of failing; briefs/A7-deep.md] [read, 2026-10-26] HostProvenance.hs `makeCopy` (`relangRoot`) sets the
  copy's manifold to the host's language; a parameter or body value of a
  type the host's language cannot hold (a record mapped only to Python, in
  C++) then fails at the pool's compile or as a compiler bug at `pinTable`,
  instead of as a located refusal (COST-15 conditions 1 and 2 conflict
  here). Spec gap beside COST-11 and HOST-9: what
  a program must do when the host's language cannot hold a value computed
  from a host-made function.
- **A stage entry's body is expressed under the stage entry's own id.**
  [read, 2026-10-27] Express.hs `expressCore` gives a root lambda's id to
  its body. A recursive function returning a function, called partially,
  has a stage entry (Realize.hs `addStageEntries`) whose body has a function
  type; if that body reaches the eta clause of `expressPolyExpr` (any form
  that is not a value and not in `isComputedValue`, e.g. a newtype
  coercion of a function), it is wrapped in a closure manifold under the
  same id, and the build stops with "two different manifolds in one pool
  share the id". A conditional, the common case, is a computed value. No
  witness for the remaining forms.
- **Placement after normalization can abort a build that placement before
  it accepted.** [read, 2026-10-26] `placeNormalized` scores and collapses
  every pool tree again and keeps no earlier placement, so an error raised
  while scoring or collapsing the normalized shape stops the build, as does
  a manifold language its tree's table does not offer (Realize.hs
  `pinTable`). `pickLanguage` can also return a recursion head's language
  that the node's table lacks (its `hostedWith` fallback), which collapse
  then cannot build. No witness yet. COST-15 condition 5 holds only if
  these cannot happen.
- **The realizer's placements are checked only against themselves.**
  [read, 2026-10-25] COST-11. No test enumerates the placements of a small
  tree to confirm the DP finds the least; no test compares the crossings
  the placement listing shows with those code generation emits (the
  PolyRemoteInterface calls after lowering); there are no unit tests that
  pin a placement directly (PlacementTests.hs was planned). The consistency
  check of `typecheck -v --realize` runs over every golden program
  (spec-cost-11-1).
- **A surviving map intrinsic resolves its instance again at code generation.**
  [read, 2026-11-07] Express (`IntrinsicS IntrMap`) and EffectBoundary
  `functorMapSource` call the resolver on the realized type instead of reading
  evidence Typecheck selected and dropped, so one use is selected twice. Not
  reproduced as a wrong selection. Store evidence on the node, as patterns do.
- **Every bracket chain's map use is fully checked, then dropped.** [read,
  2026-11-07, not measured] Treeify adds a `map` use to each `.[:]` chain;
  `checkInstances` checks it on every lenient pass and `resolveInstances` then
  discards it when the intrinsic stays. Compile time on many chains: measure.
- **A plugin language's root map starts a pool.** [read, 2026-11-07, not run]
  `Instances.nativeServes` lists root and vector modules of the four stdlib
  languages; a plugin's `root-foo` is a method use: correct output, but a pool
  where the ruling wants none, with nothing failing when a language is missed.
- **An argument computable in the caller is computed by a callback.**
  [reproduced, 2026-10-23] A cut is a subtree (Express.hs `expressPolyExpr`: the
  cross-pool AppS, PatS and eta clauses; `expressContainer`, Express.hs:2208): in `inc (dbl (inc x))` with `inc` in Python and `dbl` in C++, C++
  calls back into Python for the inner `inc x`, two round trips where one
  would do; `inc (add (inc x) (inc y))` takes three. A `where` value used
  only inside the foreign argument is placed there too. EVAL-1/2 permit
  hoisting each maximal caller-language argument subtree that is data (not
  a function or suspension), has its free variables bound in the caller,
  and is reached only through strict positions (no conditional arm, lambda
  or `@try` region). Open in COST-13: whether EVAL-3 lets such a value be
  computed in the caller's pool.
