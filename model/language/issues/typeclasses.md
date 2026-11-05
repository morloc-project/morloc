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
- **A signature inside an instance body is refused.** [2026-11-04] Spec
  incomplete. The grammar accepts `m :: T` in an instance body and Link
  refuses it with a located error (it crashed before). Open: should it be
  legal, checked against the instance type, as Haskell's InstanceSigs?

- **An omitted method falls through to a more general instance.** [read]
  Spec gap: CLS-12 (open). Typecheck.resolveInstances chooses the most
  specific instance among those that give an implementation of the method,
  so `instance C Int` without `g` runs `g` from `instance C a`, whether or
  not `instance C Int` has a body. root's bodiless `instance Eq (List a)`
  relies on this for `==` on lists. Option (a) of CLS-12 would make these
  uses errors.

- **A class constraint is never checked unless a method of the class is
  used.** [reproduced] Violates CLS-5's obligation reading.
  `ident :: Numeric a => a -> a; ident x = x` applied at Int builds and
  runs, though root has no `Numeric Int`.

- **An instance on a legacy-form record is not found where its type was
  evaluated.** [read] Violates CLS-2. Instance filtering
  (Frontend/Typecheck.hs `compatibleTypeU`) compares an evaluated record
  (`NamU`) by its name, but a record declared `record Foo = Bar {...}`
  evaluates to a `NamU` named by its constructor `Bar` (Desugar.hs ~3563),
  so `instance Semigroup Foo` used where the type was evaluated (a field of
  a record literal) is still "No instance found". Not reproduced.
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
  [reproduced] Violates CLS-19. root's `sort :: Ord a => [a] ->
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
- **An instance that omits a method is reported as no instance.**
  [reproduced, 2026-10-21] `instance Foo Int` gives `m` but not `n`; using
  `n` at Int says "No instance found for Foo::n at type Int -> Int". It
  should say the instance lacks `n` (CLS-5). Repro: /work/plans/model-audit-2026-10-21/repro/D/cls/c14.
- **Two newtypes over one type deriving `Eq`/`Ord` break comparison of
  unpacked values.** [reproduced, 2026-11-01] Violates CLS-2 and CLS-16.
  Under root-py this is rejected with "No matching implementation found
  for '<=' at type I64 -> I64 -> Bool":
  `newtype A = I64 deriving (Packable, Eq, Ord)`,
  `newtype B = I64 deriving (Packable, Eq, Ord)`,
  `g :: A -> A -> Bool; g d t = unpack t <= unpack d`,
  `f :: I64 -> Bool; f x = g (pack x) (pack x)`. Both operands come from
  `A`; declaring `B` alone breaks it, and `==` fails the same way under
  `deriving (Eq)`. It builds when only one newtype derives `Ord`, when the
  value goes through a function typed `A -> I64`, or with one operand
  annotated (`unpack t :: I64`). It reaches constrained library functions:
  stdlib `time`'s `pack (addSat (unpack t) (unpack d))` over `Instant` and
  `Duration` fails inside root's `(>)`. Speculative cause: the derived
  instances of `A` and `B` both stand at the representation `I64` beside
  the catch-all `Ord a` (entry above), and the unannotated `unpack` result
  leaves the choice among them open at realization.
- **Instance selection matches through a coercion.** [read] Violates
  CLS-17. Typecheck.resolveInstances accepts a candidate when
  `tryCoerce` turns its method type into the use's
  (Frontend/Typecheck.hs, `isCompatible`), so a value-like method of
  `instance C T` (`mempty :: T`) can serve a use at `?T`, wrapped in a
  present optional.
- **Instances are matched four ways.** [read] Violates CLS-16 (one rule).
  Typecheck.resolveInstances keeps every subtype-or-coercion match after
  alias filtering; Realize.collapseExpr picks among them with its own
  `compatibleType` (UnkT a wildcard); CodeGenerator/Instance.hs, used for
  calls the compiler builds (`__get_slice__`, `__access_index__`,
  `__to_index__`, `map` across effects), takes the first sourced match,
  with no specificity and blind to an instance whose body is morloc.
- **An ambiguous use is resolved silently in realization.** [read]
  Violates CLS-16 and CLS-18. When a use's type holds an unsolved Type
  variable (an open `mempty` inside a body), Realize.collapseExpr keeps the
  nearest of several incomparable instances instead of rejecting the use
  (`containsUnk` case of its resolveInstances).
- **Realization cannot order a Nat-kinded instance head against a literal
  one.** [read] Violates CLS-16. Realize.collapseExpr orders heads after
  erasure, where a Nat variable is a placeholder that matches any Nat in
  both directions, so `Vec n Int` and `Vec 3 Int` are equivalent there and
  the choice falls to the alias chain. The typechecker orders them
  correctly; realization should use its choice (C3 in the brittleness plan).
- **Instances are chosen per method, not whole.** [reproduced] Violates
  CLS-16. A bodied instance registers only the methods it writes, so a use
  of a method sees only the instances that write it. With `class C a`
  (`c1`, `c2`), `instance C (Int, b)` writing only `c1` and `instance C (a,
  Str)` writing both, `f :: (Int, Str) -> Str; f x = c2 x` builds and runs
  the second instance, while `c1` at the same type is rejected as
  ambiguous.
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

- **An instance context is not enforced at a use.** [reproduced,
  2026-11-01, morloc typecheck] Violates CLS-5. Link.hs:428 matches
  `IstE origin cls _ ts es` and drops the context; `Instance`
  (Namespace/State.hs:442) has no field for it. With `instance Sz a => Sz
  (W a)` and no `Sz Str`, `useIt s = sz (mkW s)` typechecks. Repro:
  /work/plans/error-clusters/repro/instance-context-use.loc.
- **Use-level selection treats any two Nats as equal.** [read,
  2026-11-05] `compatibleTypeU` (Frontend/Typecheck.hs) matches any two
  Nat expressions, so `instance Sz (Vec 3 Int)` is a candidate at every
  `Vec k Int`. The contract check now rejects a use at a signature's Nat
  variable (spec-cls-16-7), and a literal mismatch is rejected at the use
  (`Vec 4 Str`); no program is known to reach the wildcard. CLS-16.
- **`findPackableWireForm` keeps only type-variable parameters.** [read,
  2026-11-02] A native with a Nat parameter gets no tier-2 wire form; the
  first match wins, with no specificity and no check on repeated variables.

- [read, 2026-11-05] Desugar.hs declHead writes a newtype's Nat parameter
  as a type variable (`VarU n`) in the derived Packable instance head, so
  that head may not match through instance resolution (S2's resolver);
  check before Packable selection is routed through it. Possibly the
  cause of the NEWT-9/KIND-5 entry in kinds-dimensions.md.
