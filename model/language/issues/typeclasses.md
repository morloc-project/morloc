# Typeclass and instance issues

- **Signature inside an instance body crashes the compiler.** [reproduced]
  Violates DIAG-1. The grammar accepts `m :: T` in an instance body; Link
  has no case for it and calls `error` (Frontend/Link.hs:546, "Unreachable,
  instances may only contain sources and instances"). Repro:
  `class Foo a where foo :: a -> a` and
  `instance Foo Int where foo :: Int -> Int; foo x = x`, then use `foo`.
  Open: is the signature legal (checked against the instance type) or a
  located rejection?

- **Annotation inside an instance method body crashes the compiler.**
  [reproduced] Violates DIAG-1. `instance Foo Int where foo x = (x :: Int)`
  dies with "Bug in collectExprS" (Frontend/Treeify.hs:750):
  linkAndRemoveAnnotations (Treeify.hs:433) never descends into instance
  bodies. Default bodies in classes (CLS-7) would take the same path.

- **Whether an omitted method falls through to a catch-all instance depends
  on whether the instance has a body.** [reproduced] Spec gap: CLS-12.
  With `instance C a` sourcing `f` and `g`: `instance C Int where f x = ...`
  makes `g` at Int run the catch-all; bodiless `instance C Int` makes `g`
  at Int fail with "No implementation found for 'g'". Cause: a bodiless
  instance registers an empty implementation set for every method
  (Link.hs linkEmptyInstance), a bodied one only for the methods it writes
  (Link.hs linkInstance).

- **A class constraint is never checked unless a method of the class is
  used.** [reproduced] Violates CLS-5's obligation reading.
  `ident :: Numeric a => a -> a; ident x = x` applied at Int builds and
  runs, though root has no `Numeric Int`.

- **Link-time superclass check ignores types.** [read] CLS-5.
  `matchesSuperInstance` (Link.hs:694) passes when the superclass has an
  instance at any type. The precise check (`checkSuperclassInstance`,
  Frontend/Typecheck.hs:3643) covers only instances in the root module
  (Treeify.hs:512), so an imported module's `instance D T` without `C T` is
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
- [reproduced] A valid program is rejected (POLY-6 message). An instance
  for a parameterized type whose method body uses the same method at the
  parameter type, `instance Eq a => Eq (W a) where (==) x y = .val x ==
  .val y` with `record W a where val :: a`, fails with "The type of this
  term is not determined: nothing in the program fixes 'a', 'a'" at the
  instance body, even when every use is ground (`mkW [n] == mkW [n]`,
  `mkW :: a -> W a`). Same with constructor-pattern clauses over `data
  Wrap a = Wrap a` (`(==) | (Wrap x) (Wrap y) = x == y`).
- [reproduced] With both a ground and an open-element use of that `Wrap`
  instance in one module (`Wrap [n] == Wrap [n]`, `Wrap [] == Wrap []`),
  the build fails with "Cannot resolve recursive call to ==`t...@..." from
  CodeGenerator/Express.hs, with no source location: the inner `==` became
  a back-edge, the shape of #160.
- **Classes the compiler names still live in `root`.** [read] Violates
  CLS-15. `Functor` (`map`, found by name in
  library/Morloc/CodeGenerator/Instance.hs `findFunctorMap`) and `Integral`
  and `Numeric` (literal defaulting, library/Morloc/BaseTypes.hs) are
  declared in stdlib `root`, so a replacement prelude without them breaks
  code generation.

