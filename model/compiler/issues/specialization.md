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
- **Every use of a method copies its whole elaboration.** [reproduced,
  2026-11-02] Methods are never shared (Specialize.hs `splice`, `closed`
  requires a definition), so each comparison copies the instance body it
  resolves to, with every definition and method use inside it. Per use under
  root-py, `morloc typecheck`: about 0.65 ms for `x > k` and 0.4 ms for
  `x != k`, against 0.15 ms for a call to a definition in the same file
  (bench typechecking, TYCK-3, TYCK-6, TYCK-7, quick run 2026-11-03; taken
  before methods were elaborated from bodies checked once, so re-measure).
  The copies also reach code generation.
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

- **Some elaborations still infer the body again at the key.** [read,
  2026-11-08] Frontend/Specialize.hs: a definition is instantiated from its
  body checked once against its signature, except where its collection
  expands a method in place (non-empty `inline`), where the instantiated tree
  keeps an unsolved type, where the key is not solved, and, for a method, at a
  derived comparison, a representation cast, or an implementation its check
  refuses. There the body is checked at the key as before, so a body that
  holds at its signature but not under inference at the key (OPT-7's
  `nullish`, a literal defaulted to Real under a Numeric wanted) is refused
  there. Never a wrong result: the old rule only refuses. In the sweep corpus
  only exports reach it (an unsolved type).
- **A nested signature's constraint-only variables are not paired with its
  use's.** [read, 2026-11-08] Typecheck.hs `signaturePairs` pairs a nested
  signed definition's quantified and kind variables with its use's, not a
  variable only its constraints name (CLS-20), which the use makes a fresh
  existential. Such a variable is solved separately in the body and at the
  use, or not at all, which falls back to inference at the key.
- **Each signed definition is checked against its signature twice.**
  [read, 2026-11-08] `validate` checks every module's definitions in a scratch
  state and discards the result; Specialize checks the used ones again in its
  own collection. Method uses still collect every candidate instance's
  implementations although only the selected instance's are checked.
- **Opening a checked tree walks every type once per signature variable.**
  [read, 2026-11-08] Typecheck/Internal.hs `openVars` folds `substitute` over
  the rigid set, for every annotation and every type gamma stores, at each key.
  A one-pass walk was tried and refused four unit tests (spec-alias-1-5, the
  Foldable over newtype tests): `substitute` reaches a variable in a form the
  walk did not, as the head of an application. Cost grows with the number of
  nested signatures in a tree.
- **A method expanded in place is not a method frame for recursion.**
  [read, 2026-11-08] Frontend/Specialize.hs `splice` admits a definition met
  again at a new key only past a method frame on its stack; a method expanded
  in place (a stuck use, `inline`) puts none there, so the same recursion
  through an instance is refused where the method use was stuck.
- **Memo keys carry the program's whole set of referenced definitions.**
  [read, 2026-11-08] Specialize's checked-tree memos are keyed by the
  collection's `only` set, now every module's signed definitions, compared
  element by element; and a chain of stuck imported definitions is expanded one
  layer per collection pass. Cost only.
- **Some unsigned definitions are still expanded into each use.** [read, 2026-11-08]
  Deviates from POLY-5, POLY-8, NUM-5 and NUM-10. Frontend/Generalize.hs infers
  each unsigned top-level definition once, exported ones included, and
  installs its type as its signature, but keeps expansion (Treeify inlines it
  at each reference, typed there) for every local (`where`, `let`), closed or
  open, and for a top-level definition whose type holds a Nat, Int, Str, Rec,
  List, Set or effect variable, a tuple or record no named type gives, an
  undecided equation, a set constraint, a constraint neither an instance nor
  its variables answer, or a literal whose class is not in scope. An expanded
  definition's literals are decided with the definition it is expanded into.
  `morloc typecheck -v` prints each top-level definition's outcome. A pattern
  (POLY-9) is typed at each use by expansion, as POLY-9 asks; a scheme with a
  rigid shape variable (a field constraint such as `Has 0 t a` without
  syntax) would let it be referenced instead
  (/work/plans/typecheck-once/briefs/check-once-design-da.md, D).
- **An unused local definition is never checked.** [reproduced, 2026-11-08]
  Deviates from SIG-10. Treeify inlines a `where` or `let` binding at its
  uses, so one nothing uses is dropped unchecked:
  `ok x = x + 1 where bad = 1 "x"` compiles. Also a local used only by another
  unused local, and an unused local inside a pattern's body.
