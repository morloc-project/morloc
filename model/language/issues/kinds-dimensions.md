# Kinds and dimensions

- **A label known only at run time is not checked.** [read] Violates DIM-3
  and KIND-4. `main w = roll w src`, with `roll :: w@Int -> Vector n Real ->
  Vector (n - w + 1) Real` and `w` read from the command line, leaves `w`
  inferred and is accepted; no check stops `w = 12` against a 10-element
  `src`. A generated check must divide as KIND-3 does
  (toward zero), unlike Python `//` and R `%/%`.
- [read] Solving a Nat equation (NatSolver solveSOP) keeps only solutions
  whose coefficients are non-negative, so `k ~ n - w + 1` is deferred
  rather than solved; KIND-3 allows the solution, since the position
  carries its own condition.
- [read] Violates DIM-7 ("at the term that produced the condition"). The
  POLY-7 body check (Frontend/Typecheck.hs checkRigidNats) reports an
  undecided group at the innermost term holding any of its conditions,
  not one that is undecided; a solved inferred variable records no term.
- [read] A Nat position whose arithmetic holds a node with no expression
  form (Typecheck/Internal.hs natPositionExpr: an unknown dimension `_`, an
  unexpanded alias application) is skipped by the KIND-4 checks. For `_`
  this is a gap in DIM-4: whether a gradual dimension inside arithmetic is
  checked at run time (DIM-3) is not ruled.
- [read] Violates DIM-7 (ruled 2026-10-30): an undecided condition is a
  type error (Frontend/Typecheck.hs checkRigidNats, the DIM-6 check after
  inference) where DIM-7 makes it a run-time check with a warning. Waits
  on the DIM-3 run-time checks.
- [read] DIM-7 decides an Int variable as the difference of two naturals,
  which hides facts its coefficients showed: `h * h >= 0` with `h@Int`
  rigid is not proven (Typecheck/NatDecide.hs decideAssuming), so a body
  needing it is rejected as undecided, unless the condition is settled
  before the split (every Int at an even power).
- [reproduced, 2026-10-30] Messages about a local signature's variables,
  checking against signatures: an effect variable it fixes is reported at
  the enclosing definition and by its internal name (`fixes the effect
  variable 'e@e0'`), and a constraint the enclosing body lacks is reported
  with the local signature's constraints listed as given, because they
  stay among the givens after the local body (Frontend/Typecheck.hs,
  validate). The verdicts are right (POLY-4).
- [read] Gap in POLY-4: an annotation in a body (`(x :: a)`) still means
  the enclosing signature's variable, while a `where` signature's `a` is
  its own.
- [read] Violates POLY-7 and POLY-4. Only a signed top-level definition of the root
  module is checked against its signature (Frontend/Treeify.hs
  Validation); a `where` signature inside an unsigned definition, or in an
  imported module, never has its Nat and Int variables held rigid, so
  `f x = g x where g :: Vector k Int -> Int; g y = h y` with `h ::
  Vector (m + 1) Int -> Int` is accepted.
- [read] The POLY-7 check locates a refutation at the innermost condition
  that conflicts with the body's equations; when the equations conflict
  only together it falls back to the definition.
- [read] Violates DIM-6 ("at the term that produced it"). A Nat equation
  deferred during instance resolution (after synthesis) has no recorded
  term; the error falls back to the export (Frontend/Typecheck.hs, the
  deferred recheck after resolveInstances).
- [reproduced] Kind-equation diagnostics print renamed variables:
  `4 ~ ((5 + a) + b)` for a signature written with `n` and `m`.
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

