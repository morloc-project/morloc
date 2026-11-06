# Typeclass and instance issues

- **Term and method names: two open questions.** [reproduced,
  2026-11-04] Spec incomplete (CLS-3, SIG-9, MOD-12); brief
  /work/plans/error-clusters/briefs/ruling-term-method-collision.md.
  (1) An unsigned top-level definition named like a method of an
  imported class becomes a new term that shadows the method
  (`import root-py` then `abs x = 7.0`): refuse it, or keep it?
  (2) Class methods ignore a namespace: with `import root-py as rp`, bare
  `abs` resolves (Restructure.prefixAlias passes classes through), so two
  namespaced libraries sharing a method name collide (CLS-3), against
  SIG-8's "a namespaced import brings `n.f`, not `f`".
  (3) A method of a class the module declares, named like a term it
  imports with a whole module, is refused (spec-cls-3-3). Should the
  method shadow the term (SIG-7 for methods)? Doing so needs every name
  resolver to agree: Link, the export resolver (Restructure), Treeify
  `definitionOf` and TerminalActions `visibleSigs`; a Link-only shadow left
  the module's export of the name pointing at the imported term, and its
  importers hit a compiler bug. This refusal, and the refusal of an
  unsigned top-level definition named like a local method (spec-cls-3-1),
  are where SIG-7 (a method's signature counts, SIG-6) and SIG-9 deviate:
  sound, but narrower than the ruled text. Before, both silently
  bypassed the instances.
- **Import conflicts are reported without a location.** [read 2026-11-04]
  Violates DIAG-2. Link.hs `mergeLinkStates` ("Illegal masking of type
  signatures", the term/class conflict) uses `throwInheritanceError`,
  which names the module but no import line.
- **Two classes never in one scope may not share a method name.**
  [reproduced 2026-11-04] Violates CLS-3 ("in scope"). Link.hs
  `insertWithCheck` checks method names against the global
  `stateTypeclasses`: `ca` declares `class A a where foo :: a -> Int`,
  `cb` declares `class B a where foo :: a -> Int`, `main` imports
  `.ca (bar)` and `.cb (baz)`: "The typeclasses 'A' and 'B' have
  conflicting definitions of the term 'foo'". Instances are keyed by
  method name, so the fix keys them by class and method.
- [speculative, 2026-11-04] An instance whose type extends a method's
  parameters (`mk :: Int -> a` at `Int -> Int` is `Int -> Int -> Int`,
  APP-1) keeps the class signature's argument docs, written for fewer
  parameters (Link.hs `substituteInstanceTypes`, `et {etype = t}`); code
  pairing docs with inputs would misalign them.

- **An omitted method falls through to a more general instance.** [read]
  Spec gap: CLS-12 (open). Instances.methodSources takes the most
  specific more general instance that gives an implementation of the method,
  so `instance C Int` without `g` runs `g` from `instance C a`, whether or
  not `instance C Int` has a body. root's bodiless `instance Eq (List a)`
  relies on this for `==` on lists. Option (a) of CLS-12 would make these
  uses errors.

- [reproduced] Violates CLS-2 (#160). Inside an instance body, a use of the
  class's own method at another type is bound to the instance being
  defined. With `instance Eq Q where (==) a b = .num a * .den b == .num b
  * .den a`, `{num = n, den = 2} == {num = 2 * n, den = 4}` lowers the
  inner Int `==` to a self-call of the Q instance (a `while True:` loop in
  Python that crashes). The same happens with `Ord` and `<=`; both work
  when the operands come from a signed `mkq :: Int -> Int -> Q`. [read]
  Cause: the record literals leave an existential in the method
  reference's type, so Specialize.collectElaborable falls back to full
  expansion (`collectRoot Nothing`), where Treeify.collectExprS turns any
  reference to a term already being expanded into a back-edge
  (`CallS`); the exemption for methods (a use may resolve to another
  instance) applies only in shallow mode. The literal trigger is rejected
  since REC-4, and Typecheck.assertBackEdgeTypes stops a build whose typed
  tree holds a back-edge at a ground type other than its definition's, so
  no known program miscompiles; the binding by term id remains in full
  expansion. Fix direction: after typechecking, re-resolve by type each
  method back-edge whose type differs from its definition's.
- **Classes the compiler names still live in `root`.** [read] Violates
  CLS-15. `Functor` (`map`, found by name in
  library/Morloc/CodeGenerator/Instance.hs `findFunctorMap`) and `Integral`
  and `Numeric` (literal defaulting, library/Morloc/BaseTypes.hs) are
  declared in stdlib `root`, so a replacement prelude without them breaks
  code generation.
- **A sourced function with a class constraint ignores the instance.**
  [reproduced] Violates CLS-21. root's `sort :: Ord a => [a] ->
  [a]` is sourced in every root-* as `morloc_sort`, which uses the native
  comparison, not the selected `Ord` instance. With
  `newtype Z = Real deriving (Eq, Ord)`, Python `sort [0.0, -0.0]` gives
  `[0,-0]` while `<=` and `min` on the same values order `-0` first. On a
  `data` type deriving `Ord`, C++ `sort` fails in g++ (no `operator<` for
  the generated struct) instead of being rejected or using `<=`. The same
  holds for hand-written instances and any other sourced constrained
  function. So a root-* cannot source `!=`, `<`, `>` or `>=` natively:
  the source would be chosen at every type, and `v < w` on a vector-py
  Vector would run numpy's `<` (reproduced 2026-11-02: a run failure)
  instead of the vector instance.
- **R orders strings by locale collation, Python and C++ by code point.**
  [reproduced, 2026-11-02] `"B" < "a"` is FALSE in R (under C.UTF-8) and
  True in Python, so a comparison's result depends on which language it is
  placed in. Spec gap: DERIV-3 orders a derived instance's strings by
  UTF-8 bytes, while DERIV-6 makes the primitive instance the host
  operator; whether `Str`'s host order must agree across languages needs
  a ruling.
- **An instance declared without a body omits a method silently.**
  [reproduced, 2026-11-05] An instance whose declarations do not mention a
  method is reported at a use of it ("selects the instance Foo Int, which
  gives no implementation of it"). A bodiless declaration (`instance Sh
  A`) lists every method with no implementation, so a use selects it and
  typechecks, and only realization says "No implementation found for
  'sh'", naming no instance. Open: CLS-14.
- **Instances are matched three ways.** [read] Violates CLS-16 (one rule).
  Typecheck selects one instance per use through the resolver; but
  Realize.collapseExpr still picks among a use's implementations with its
  own `compatibleType` (UnkT a wildcard), and CodeGenerator/Instance.hs,
  used for calls the compiler builds (`__get_slice__`, `__access_index__`,
  `__to_index__`, `map` across effects), takes the first sourced match,
  with no specificity and blind to an instance whose body is morloc.
- **Realization cannot order a Nat-kinded instance head against a literal
  one.** [read] Violates CLS-16. Realize.collapseExpr orders heads after
  erasure, where a Nat variable is a placeholder that matches any Nat in
  both directions, so `Vec n Int` and `Vec 3 Int` are equivalent there and
  the choice falls to the alias chain. The typechecker orders them
  correctly; realization should use its choice (C3 in the brittleness plan).
- **A field of a Rec alias used as a record type cannot be read.**
  [reproduced] Violates KIND-2's record-type reading (ALIAS-11). `type R =
  {x = Int}; f :: R -> Int; f r = .x r` passes the kind check and fails at
  make "Subtype error: {x=Int} <: a [] [(x, b)]"; the inline `f :: {x =
  Int} -> Int` and `f :: R -> R; f r = r` work.
- **A Nat-omitting Packable instance head fails NEWT-9.** [reproduced]
  `newtype Vec (n :: Nat) a = [a]; instance Packable [a] (Vec a)` is
  rejected "The Packable instance for 'Vec a' packs from '[a]', but the
  representation of 'Vec a' is '[a]'"; `Vec n a` builds. The head seems to
  be compared before its Nat is filled (KIND-5).

- **`findPackableWireForm` keeps only type-variable parameters.** [read,
  2026-11-02] A native with a Nat parameter gets no tier-2 wire form; the
  first match wins, with no specificity and no check on repeated variables.

- [read, 2026-11-05] Desugar.hs declHead writes a newtype's Nat parameter
  as a type variable (`VarU n`) in the derived Packable instance head, so
  that head may not match through instance resolution (S2's resolver);
  check before Packable selection is routed through it. Possibly the
  cause of the NEWT-9/KIND-5 entry in kinds-dimensions.md.

- **Sourced root sort/unique are not the instance's relation.** [read
  2026-11-05] CLS-21. root-r `morloc_sort` (root-r/R/core.R:316-331)
  sorts with `method="radix"`: NA/NaN dropped (length changes), C-locale
  string order where R's `<` collates by locale, and a list of lists by
  its first element only. root-py `morloc_unique` (core.py:288) dedups
  through a `set`: TypeError on list or record elements, hash equality
  instead of the selected Eq. So a primitive-instance test alone does not
  make a sourced constrained term agree with the instance. Reproduced
  separately: `sort` at `newtype Z = Real deriving (Eq, Ord)` in Python
  gives [0,-0,1] (derived order is totalOrder). Open: rulings Q4/Q6 in
  /work/plans/error-clusters/s2-rulings.md. Fixed by CLS-21's classes.
- **map-r's specific Packable head serves every language.** [read
  2026-11-05] map-r/main.loc:35 declares `Packable [(Str, val)] (Map Str
  val)` with R implementations only. Language plays no part in selection
  (CLS-16), so it is selected at `Map Str v` in Python and C++ too, which
  then depend on the unruled CLS-12 fall-through to map's general head.

- **A `Packable` class over a Nat head escapes the contract check.**
  [reproduced, 2026-11-05] Violates CLS-5 and CLS-16. With `class Packable
  a b where pack :: a -> b; unpack :: b -> a`, `newtype Vec (n :: Nat) a
  = List a`, a sourced `instance Packable [a] (Vec 3 a)` and `g :: Vec n
  Str -> Vec n Str; g t = pack (unpack t)`, `morloc typecheck` accepts and
  prints two signatures for `g` (`Vec 3 Str -> Vec 3 Str` and `Vec a Str
  -> Vec a Str`). The same program with the methods named `pk`/`unpk` is
  rejected at validation.
- **A signed term's class constraints cannot be instantiated when its
  signature binds a label.** [reproduced 2026-11-05, latent] Typecheck.hs
  `signatureConstraints` instantiates the signature with `instantiateAll`
  and subtypes the use against it; a label (`l@[Str] -> Table a b -> Table
  a (b # l)`) becomes a plain existential, so `{x=Int} <: {x=Int, y=Str} #
  l` fails. Today only row constraints occur with labels and no class
  constraint is owed, so it returns none; a signature with both would stop
  at its compiler bug. Instantiate labels as the checker does at the use.
- **An instance no use selects is never checked for its own uses.**
  [reproduced 2026-11-06, open question] An undetermined or missing-instance
  use inside an instance body is reported only at a use that selects the
  instance (probes q5, v2 in the step 3 r2 scratch); a broken instance is
  accepted until something uses it. Spec gap in CLS-2/CLS-5: say whether an
  instance body must check on its own.
- **stdlib time fails typecheck: `addSat`/`subSat` at I64.** [reproduced
  2026-11-06, on base too] lib/stdlib/time/main.loc's `after`, `before`,
  `since` and `instance Semigroup Duration` call root's `addSat`/`subSat` at
  `I64`, which needs `Integral I64`; root has `Integral` only at `Int` and
  `Real`: "No instance found for Integral::zero at type I64". A stdlib
  defect (CLS-2 holds); root needs `Integral` at the fixed-width integers
  or time must not use them there.
