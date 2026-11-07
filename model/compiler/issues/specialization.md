# Specialization

- **What only generic exports reach is checked only at a concrete use.**
  [read, 2026-11-02] A generic export is checked with its references left
  as signatures and is not realized (SIG-5), so the checks that run over
  realized or spliced trees never see a helper or instance that only
  generic exports use: `checkPatterns` and `checkRecursion`
  (library/Morloc.hs `typecheckFrontend`), REAL-5's comparison of
  alternatives (two `instance Foo Int` with bodies 5 and 6, used only by
  `f :: Foo a => a; f = bar`, is accepted), and the label, auto-require
  and docstring checks of the realization passes, which no longer run on
  the generic export itself either. A library of generic functions, such
  as root-py, gets these errors at its first concrete use, not when it is
  typechecked or installed. Spec gap: PAT-1 and REAL-5 check at the types
  a term is used at, and say nothing of a term no concrete term uses.
  Running these checks over the validated definitions would cover it.
- **A definition reached again through an instance is rejected as
  polymorphic recursion.** [reproduced, 2026-11-02] Frontend/Specialize.hs
  `splice` rejects any definition already in the splice stack at another
  key. `cmpC :: Cmp a => a -> a -> Int` calling the method `lt`, whose
  tuple instance calls `cmpC` at each component, fails for
  `f x = cmpC (x, 2.0) (3, 4.0)` though the types shrink: "Polymorphic
  recursion is not supported". The root container instances have this
  shape (`compare` on tuples). Allowing a repeat past a method frame is not
  enough on its own: back-edges, recursion tokens (`recName v k`) and the
  deletion of a frame's own back-edge are keyed by term, so a term met at
  two keys would bind a recursive call to the wrong copy. A fix keys them by
  term and key, and admits a repeat past a method frame only at a key
  smaller than every earlier key of that term (the 64-deep method bound
  stays as a backstop). Analysis:
  /work/plans/eq-ord-methods/briefs/da-connect-recursion.md.
- **Every use of a method copies its whole elaboration.** [reproduced,
  2026-11-02] Methods are never shared (Specialize.hs `splice`, `closed`
  requires a definition), so each comparison copies the instance body it
  resolves to, with every definition and method use inside it. Per use under
  root-py, `morloc typecheck`: about 0.65 ms for `x > k` and 0.4 ms for
  `x != k`, against 0.15 ms for a call to a definition in the same file
  (bench typechecking, TYCK-3, TYCK-6, TYCK-7, quick run 2026-11-03). The
  copies also reach code generation.
- **A key holding a Type variable the checker generated is never shared.**
  [read, 2026-11-06] Frontend/Specialize.hs `shareable` refuses a key with
  `generatedTypeVars`; it was written for Realize's selection by type, in
  which an unsolved variable was a wildcard, and collapse no longer
  selects by type. Whether the exclusion is still needed, and what lifting
  it saves in copies and compile time, is not measured. Lifting it changes
  which elaborations are shared, hence the generated pools.
- **A method with one stuck use is expanded at every use.** [read, 2026-11-07]
  Frontend/Specialize.hs `collectElaborable` expands a method in place at all of
  its uses in the tree once one use has an unsolved type, each use bringing every
  instance's body; the cost grows as uses x instances. Expanding only the stuck
  use needs a key that survives re-collection: expression indices of copied
  instance bodies are fresh on every pass, so a use-keyed set never stops
  growing (tried: spec-cls-2-10 did not terminate).

