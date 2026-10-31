# Kinds and dimensions

- **Some dimensions are not checked.** [read, 2026-10-30] Violates DIM-3.
  Input decoders check a schema's fixed array lengths (the JSON reader,
  the msgpack decoder, voidstar landing and in-place checks), but a value a
  sourced function returns is never checked (each binding writes it with
  no length check, and a by-reference packet is not walked). A header
  (DIM-9) and a Table's row count are checked only by the command-entry
  check, not by the decoders (no schema carries them).
- [read, 2026-10-31] Gap in DIM-3 (code level): a debug build checks a
  value one pool hands another only where it is decoded into shared memory
  (a file the program wrote, a msgpack packet). Every pool reads an inline
  voidstar packet in place, with no fixed-length check: C++
  (lang/cpp/cppmorloc.hpp mlc_read_inline_packet), Python (pymorloc.c
  from_voidstar on the payload), R (rmorloc.c) and Rust (rustmorloc
  lib.rs, MorlocSpace::payload). A value passed by reference is not walked
  either.
- [read, 2026-10-31] A debug build's `MORLOC_CODE_CHECKS=1`
  (data/rust/morloc-runtime/src/origin.rs) is inherited by any morloc
  program a sourced function starts, which then runs code-level checks
  although it was built for release. Cost only; nothing goes unchecked.
- [speculative, 2026-10-31] An `@unpack` argument is built by the nexus as
  the parent command loads it (morloc-nexus/src/dispatch.rs, the Embed
  arm), checked against the parent's schema; the entry decodes it as a
  value the program built. If the entry's schema could differ from the
  parent's, its fixed lengths would never be checked. Not reproduced.
- **A label known only at run time is not checked inside a body.** [read]
  Violates DIM-3 and KIND-4. A command's own signature is checked at entry
  (the nexus and daemon read its `dims` constraint), but `main w = roll w
  src`, with `roll :: w@Int -> Vector n Real -> Vector (n - w + 1) Real`
  and `w` and `src` given to `main`, leaves `w` inferred and is accepted;
  no check stops `w = 12` against a 10-element `src`. Needs a quantifier
  for a variable a run-time value observes.
- [read] Gap in DIM-3: a command's dimension relations are checked only on
  its arguments at entry. A value a pool reads from a file (`@open`, a load
  function) is not checked.
- [read] Gap in DIM-3: a `@default` that violates a command's dimension
  relations is reported only when the command runs with it, not at
  compile time.
- [read] Gap in DIM-3: an exported term without a signature gets no entry
  check. The contract is read from the signature (CodeGenerator/
  DimContract.hs dimConstraints); the command's own type has its Nat
  variables erased, so `g xs ys = dot xs ys` exported unsigned accepts
  `[1,2] [3]`.
- [read] Gap in DIM-3: a length expression whose variables no other slot
  binds is not checked. `f :: Vector (n + 1) Int -> Int` accepts an empty
  vector (`n = -1`); checking it needs the expression solved for its one
  unbound variable. A header (DIM-9) is solved only when its expression is
  `v + c` or `v - c` (CodeGenerator/DimContract.hs `invert`), so a header
  `m * 2` leaves `m` unchecked.
- [read] Cost of the DIM-3 entry check (cost ruled out of scope for now,
  2026-10-30): on a remote command the nexus decodes in full each argument
  a relation reads (data/rust/morloc-nexus/src/dispatch.rs
  check_dims_of_packets), so a file argument is read twice and a streamed
  file is loaded whole into the nexus's shared memory, doubling its peak;
  the daemon parses each command's relations on every request
  (morloc-runtime/src/cli.rs dims_of). Reading only the array headers a
  relation needs, and parsing at manifest load, would remove both.
- [read] The entry check refuses a label value beyond 128 bits ("too
  large to check its dimensions", morloc-runtime-types/src/dims.rs
  int_value) even where its relations would hold.
- [reproduced, 2026-10-30] Gap in records.md: `record Entry n where a ::
  Vector n Int` is rejected ("'n' has kind Nat, but a type that classifies
  a value has kind Type"); `record Entry (n :: Nat)` is accepted. A record
  parameter's kind is not inferred from its fields.
- [reproduced, 2026-10-30] Gap in records.md: a record field may name a Nat
  variable the record does not take (`record TwoOpts where left :: Vector
  n Int; right :: Vector n Int`); all fields share it, which no item
  states. Golden spec-dim-3-2 relies on it.
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
- [read] DIM-7 has no complete procedure for linear arithmetic: a group
  of linear conditions over several inferred variables, beyond one
  equation with lower bounds, is decided by a bounded search, so a
  satisfiable group with only large solutions is reported undecided
  (Typecheck/NatDecide.hs satisfiable). The Omega test would make every
  linear group exact.
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

