# Specialization

- **The in-place fallback expands every instance of every method.**
  [reproduced, 2026-11-02] When a concrete export still holds a reference
  whose type is unsolved once no definition is left to expand
  (Frontend/Specialize.hs `collectElaborable`), the whole tree is collected
  again with every term expanded in place (`collectRoot Nothing`): each
  method use brings every instance's body, recursively through the
  definitions those bodies use, and the result is typechecked. With
  container instances that use their class again (`Ord a => Ord [a]`,
  tuples) the tree grows with instances x body raised to the number of
  methods that reach each other. Reached by POLY-6-exempt variables and by
  method uses nothing determines (goldens records-complex-1/2,
  spec-poly-6-1..6, spec-newt-7-12 reach it today). Repro, with a local
  class of 14 primitive and 8 container instances:
  `k :: a -> b -> a; f :: Int -> Int; f x = k x (lt [])`. Generic exports
  no longer reach it (SIG-5). The general fix is S2's instance evidence
  (/work/plans/error-clusters/s2-instance-evidence.md).
- **The fallback turns a method's use inside its own expansion into a
  back-edge.** [read, 2026-11-02] Frontend/Treeify.hs, the `namerExpanding`
  case: in full collection, the list instance's `<` body using `<` at the
  element type becomes a `CallS` to the method being expanded, though it
  resolves to another instance. Not reproduced as a wrong result.
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
