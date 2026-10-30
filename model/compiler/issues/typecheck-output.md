# What the typechecker hands to code generation

Code generation receives types the typechecker should have rejected, and
reports them at the export list. Language-side entries are in
model/language/issues/typecheck.md.

- **Gap: no item states what a typechecked type may contain.** frontend/
  has no item saying every type leaving the typechecker is `Type`-kinded
  and has no unreduced type-level expression. Written types are now
  checked (KIND-2, KIND-5, TAB-3), but a list column in a `Table` passes
  typecheck and fails when code generation serializes it ("A declared
  table column must be a primitive ..."). Candidate: TC-4, enforced by a
  check at the boundary that fails as an internal error.
- **Gap: locations are lost after typechecking.** No item says a
  diagnostic raised after the typechecker names the construct that caused
  it (DIAG-2). Every code-generation rejection above is reported at the
  module's export list (`main.loc:1:14`).
- **`morloc typecheck` prints a signed export's inferred type.**
  [reproduced, 2026-10-30] `f :: Vector n Int -> Vector (n - 1) Int`
  prints as `f :: Vector (1 + a) Int -> Vector a Int`, and `f :: w@Int ->
  Vector n Real -> Vector (n - w + 1) Real` as `... -> Vector ((1 + a) +
  (-1 * b)) Real`: the normal form, with renamed variables and no label.
- **`morloc typecheck` drops class constraints from signed exports.**
  [reproduced, 2026-10-21] `f :: Integral a => a -> a` and `s ::
  Semigroup a => a -> a` print as `f :: a -> a`, `s :: a -> a`.
- **Two erasures to `Type` disagree.** [read] Namespace/Type.hs `typeOf`
  and `unresolvedType2type`: an existential becomes a record or `UnkT` in
  one and an `error` in the other; `a -> (b -> c)` is flattened by
  `typeOf` only (APP-1), yet
  EffectBoundary and Rust.hs call `unresolvedType2type`; a row literal
  becomes a record in one and a Rec placeholder in the other.
- **An unreduced field projection erases to a Nat placeholder.** [read]
  Namespace/Type.hs `typeOf` gives `NatVoidT` for an `OpU` of kind Type
  (`ProjectField r f`), so a sourced signature such as table's `getCol ::
  f@Str -> Table n r -> Vector n (ProjectField r f)`, read by EffectBoundary
  and the Rust members, carries a value type that `kindOfT` calls a Nat.

