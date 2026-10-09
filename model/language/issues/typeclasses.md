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
