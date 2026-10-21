# Kinds and dimensions

- **Signature Nat variables are not rigid.** [reproduced] Spec gap: POLY-1
  (proposed) says signature variables are universal; for Nat variables the
  code solves them like existentials. `h :: Vector (m + 1) Int -> Int`,
  `f :: Vector n Int -> Int; f x = h x` is accepted and `morloc typecheck`
  reports `f :: Vector (1 + a) Int -> Int`; `f :: Vector (n - 1) Int` is
  reported as `Vector a Int`. The DIM-6 check likewise treats every Nat
  variable as existential: `f :: Vector (n * n) Int` calling `h` above is
  accepted (witness n=1, m=0) though n=0 has no m. Design question: (a)
  rigid, as Type variables, making these type errors (the DIM-6 check must
  then quantify them universally: decidable for linear equations, not in
  general); (b) existential, with a spec item saying so and the inferred
  type reported.
- [read] Violates DIM-6 ("at the term that produced it"). A Nat equation
  deferred during instance resolution (after synthesis) has no recorded
  term; the error falls back to the export (Frontend/Typecheck.hs, the
  deferred recheck after resolveInstances).
- [reproduced] Kind-equation diagnostics print renamed variables:
  `4 ~ ((5 + a) + b)` for a signature written with `n` and `m`.
- **Newtype and primitive applications are not kind-checked.** [reproduced]
  Violates KIND-5 and KIND-2. `newtype Tbl (n :: Nat) (r :: Rec) = Int`:
  `f :: Tbl Int -> Int` and `f :: Tbl 3 4 -> Int` both typecheck. Aliases
  are checked (ALIAS-3, ALIAS-8). Repro: `/work/plans/issues/164/repro/s5-fill/`.
- [reproduced] Violates KIND-6. A reserved type used without its declaring
  module has no parameter kinds: with only `import root`,
  `type MyTable n = Table n {x = Int, y = Bool}; g :: MyTable -> Int` fails
  "takes 1 argument, but is applied to 0"; with `import table (Table)` it is
  accepted (`n` inferred Nat and filled). Repro:
  `/work/plans/issues/164/repro/tables-alias/a.loc`.
- [read] Operator reclassification in refineKinds (Restructure) misses two
  forms: its set test does not match a non-empty set literal, so
  `s - {'a}` stays Nat subtraction; it runs before named builtins are
  rewritten, so `Singleton k v + Singleton g w` stays Nat addition. Alias
  kind inference classifies both correctly.
- [read] Violates KIND-5 (one reading of an under-applied head). Restructure
  reads an under-applied kinded head three ways: collectKindedVarsFromScope
  matches arguments to parameters by position, fillMissingKindArgs by kind
  bucket, and alias kind inference treats every argument as Type.
- **Division by zero at the Nat level is not reported as such.**
  [reproduced, 2026-10-21] Gap in KIND-3. `g :: Buf (7 / 0) -> Int` against
  `Buf 3` fails "cannot decide whether these kind-level equations have a
  solution". Candidates: a located type error where reduced; undefined.
- **Recursive newtypes through a container.** [reproduced, 2026-10-21] Gap
  in NEWT-6. `newtype T = [T]` and `newtype T = (Int, ?T)` are accepted
  (the latter round-trips `[1,[2,null]]`), but `newtype A = [B]; newtype
  B = (C, Int); newtype C = A` is rejected as mutual recursion. Candidates:
  (a) a cycle guarded by `[ ]`/`?` is legal, self or mutual (as ALIAS-7);
  (b) every representation cycle is an error. Repro: /work/plans/model-audit-2026-10-21/repro/B/t/rec1.loc,
  rec2.loc, cyc3.loc.
- **Hand-written Packable instances bypass NEWT-8's determinacy rule.**
  [read, 2026-10-21] NEWT-8 forbids deriving `pack` for a parameter the
  representation does not determine, since the claim would be unchecked,
  but NEWT-9 allows `instance Packable (List a) (Buffer n a)` (manual
  buffer.loc, stdlib `Vector`). No item says whether such a `pack` is
  trusted, checked at run time, or illegal.
- **A slice of a `Vector` takes any length.** [reproduced, 2026-10-21]
  Violates DIM-3 on a path the compiler creates. `s3 :: Vector 3 Real ->
  Vector 7 Real; s3 y = .[0:2] y` typechecks and prints `[1,2]`; the
  manual's conv example then returns 7 elements typed `Vector 6`. No item
  types a slice of `Vector n a` (COLL-3 covers `[T]`). Repro: /work/plans/model-audit-2026-10-21/repro/C/dim/sl.loc,
  conv.loc.
