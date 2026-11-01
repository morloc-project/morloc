# Typeclass and instance issues

- **Signature inside an instance body crashes the compiler.** [reproduced]
  Violates DIAG-1. The grammar accepts `m :: T` in an instance body; Link
  has no case for it and calls `error` (Frontend/Link.hs linkInstance,
  "linkInstance: unexpected declaration in an instance body"). Repro:
  `class Foo a where foo :: a -> a` and
  `instance Foo Int where foo :: Int -> Int; foo x = x`, then use `foo`.
  Open: is the signature legal (checked against the instance type) or a
  located rejection?

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

- **Link-time superclass check ignores types.** [read] CLS-5.
  `matchesSuperInstance` (Link.hs:790) passes when the superclass has an
  instance at any type. The precise check (`checkSuperclassInstance`,
  Frontend/Typecheck.hs:3459) covers only instances in the root module
  (Treeify.hs:575 `rootInstances`), so an imported module's `instance D T` without `C T` is
  likely accepted. Not reproduced across modules.
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
  function.
- **An instance that omits a method is reported as no instance.**
  [reproduced, 2026-10-21] `instance Foo Int` gives `m` but not `n`; using
  `n` at Int says "No instance found for Foo::n at type Int -> Int". It
  should say the instance lacks `n` (CLS-5). Repro: /work/plans/model-audit-2026-10-21/repro/D/cls/c14.
- **Every type has `Eq` and `Ord` through a catch-all host-operator
  instance.** [reproduced] Violates DERIV-6. root-py, root-cpp, root-r and
  root-rust declare `instance Eq a` and `instance Ord a` mapped to the host
  `==` and `<=`. The typechecker therefore grants `Eq T` for every `T`,
  including functions, and `==` on a `data` type typechecks and then fails
  in g++ (`no match for operator==`, root-cpp core.hpp). No `Eq`/`Ord`
  constraint or instance context can be unmet. Repro:
  /work/plans/brittleness-2026-10-23/repros/lang/m/datacmp.
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
  `compatibleType` (UnkT a wildcard); Typecheck.validate's `covers` uses
  `matchTypeU` with no alias chain; CodeGenerator/Instance.hs, used for
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
- **The signature check ignores an instance context.** [reproduced,
  2026-11-01] Violates CLS-5. `g :: W a -> Int; g w = sz w` (no `Sz a`)
  typechecks: `covers` (Frontend/Typecheck.hs ~3521) accepts any head that
  matches without reading its context. Repro:
  /work/plans/error-clusters/repro/instance-context-signature.loc.
- **`matchTypeU` binds only type variables and ignores effect rows.**
  [read, 2026-11-01] `matchTypeU` (Frontend/Typecheck.hs ~3590), used by
  `covers`, never binds a Nat variable, so a head such as `Eq (Vector n
  a)` does not cover a use at `Vector 3 a`; and `<E> T` matches `<F> T`.
