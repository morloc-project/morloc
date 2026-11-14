# Typechecking gaps

Programs that typecheck and fail later, rejections with no location, and
literal and record gaps. Reproduced on 0.109.0 unless marked.

- [read, 2026-11-04] An over-application through an unsolved result is
  checked by rewriting `f xs1 xs2` to `(f xs1) xs2` and checking that from
  the context before `f` was synthesized (Typecheck.hs `overApplied`, APP-4),
  so `f` is synthesized twice. A flat chain (`format "a" 1 2 3`) costs one
  extra synthesis of the head; a head that is itself such an application
  doubles the work at each level, `2^d` for `d` nested over-applied heads.
  Candidate: continue from the synthesized head and apply its result to
  the rest.
- [reproduced, 2026-11-05] Typechecking a composition chain grows
  superlinearly: `r = (id . id . ... . id) 42` takes 0.2 s at 200 links, 0.6
  s at 400 and 28 s at 1600 (installed 0.112.0 and dev alike). The unit test
  "long non-tuple composition chain stays linear" checks 200 links under a
  1 s timeout, so it passes only on a quiet machine.
- [read, 2026-11-05] A lambda with more parameters than its expected
  arrow group (Typecheck.hs checkE LamS, third case) checks the remaining
  parameters as `AnnoS i i (LamS rest body)`, reusing the outer lambda's
  index, so an annotation on the outer lambda (`(\x y -> x + y :: Int ->
  Int -> Int)`) would be applied again to the inner one. Not reproduced.
- **Unreduced Rec expression not equal to itself.** [reproduced] Violates
  TEQ-4. `newtype Frame (r :: Rec); select, mySelect :: l@[Str] -> Frame r
  -> Frame (Restrict r l); mySelect l t = select l t` fails "Cannot compare
  Rec expressions (a # l) <: (a # l)". No function over the Rec operators
  can have a body.
- **List-of-string type stays unreduced.** [reproduced] Violates TAB-3.
  `select :: Frame r -> Frame (Restrict r ["x"])` typechecks as
  `Frame (a # ["x"])`; the error comes at a use (`narrow = select` against
  `Frame {x = Int}`: "Cannot compare Rec expressions"). The tick form
  `['x]` works. Open: should `["x"]` at kind List be read as `['x]` or be
  rejected at the signature?
- **Sort key checked at run time.** [reproduced] Violates TAB-3. `byPop =
  sortRows [("poop", False)] census` builds; `./sort byPop` fails "Invalid
  sort key column: No match for FieldRef.Name(poop)" and prints the whole
  table in the error.
- **Constraint violations unlocated.** [reproduced] Violates DIAG-2. One
  line, no file or line, for each: `Subset: literal set missing 'zip'`
  (`narrow = select ["name", "zip"]`), `Member: 'q' not in literal set`
  (`f :: (Member "q" (Keys r)) => Frame r -> Int` at `Frame {x = Int}`),
  `Disjoint: shared element(s) 'x'`. An overlap through `+` is located.
- **Rejections located at the export list.** [reproduced] Violates DIAG-2
  (location of the construct): a list or record table column (`t :: Table 2 {xs =
  [Int]}`, accepted by typecheck, refused by make), point at
  `main.loc:1:14`. See model/compiler/issues/typecheck-output.md.
- **Exported method error.** [reproduced] Violates MOD-2 and MOD-10 in
  spirit. A module whose export list is `(Pretty, exclaim, pretty)`, with
  `pretty` a method of `Pretty`: importing it fails "Module '.numops' does
  not export the following terms or types: [pretty]", unlocated, not naming
  the class, and reads backwards (the list does name it). The import-side
  message is good. Open: is listing a method in an export list an error
  (MOD-2 says so for undefined names only)?
- **`pack` needs an inner annotation.** [reproduced] `import tensor-py;
  m :: Matrix 2 3 Real; m = pack ((2, 3), [1.0, 2.0, 3.0, 4.0, 5.0, 6.0])`
  fails "No instance found for Packable::pack / Are you missing a top-level
  type signature?" although one is present; `:: Vector 6 Real` on the list
  passes. Gap: SIG-4 open; the hint is wrong either way.
- **`@collect (psmap f (@next s))` leaves the element type unsolved.**
  [reproduced, 0.109.0] With `f :: Str -> Real`, `s :: IStream Str` and
  `import parallel-cpp`, `morloc make` fails "cannot serialize unknown type
  variable: b" at the export. `psmap :: (a -> b) -> ... -> ([b] -> <IO> ())
  -> <IO> ()` fixes `b = Real`, so the sink type is determined. Works when
  the producer goes through a signature (`scores :: IStream Str -> ([Real]
  -> <IO> ()) -> <IO> (); scores s = psmap f (@next s)`) or the sink is
  ascribed in a lambda. The `parallel` module docstring advertises the
  failing form. Repro: web/morloc-studio/public/assets/home-examples/streams
  with `@collect (psmap complexity (@next s))` inlined.
- **Effect rows with two tails, or left labels the right row lacks, are
  solved rather than included.** [read] Violates EFF-8. Two guard branches
  whose rows carry different tails (a rigid `e` in one, an existential from
  an unannotated lambda parameter in the other) are not joined by
  `joinSuspensions` (Frontend/Typecheck.hs) and fall back to subtyping one
  into the other, which solves the left tail; when that tail is a
  signature's rigid variable, `validate` rejects the definition. Separately,
  `subtypeEffRows` open <: open with distinct tails (Typecheck/Internal.hs)
  requires the left labels to be a subset of the right labels, where the
  surplus could be taken by the right tail. Not reproduced.
- **A tree skipped as generic for a variable no representation needs.**
  [speculative] Morloc.hs skips any tree whose type `containsUnk`; a call
  into one reports POLY-6 only when `Infer.needed` finds a variable. A
  helper whose only undetermined variable is one POLY-6 exempts (a phantom
  `data` argument, an erased dimension) would still be skipped, and its
  caller would get the unlocated "Cannot resolve recursive call". Such a
  tree should be generated, not skipped.
- **A pure branch fixes the row of an unsigned function's callback.**
  [reproduced] Violates EFF-8. `pick3 b f ? b = do () : f 1` with no
  signature, used as `pick3 True pyLog` (`pyLog :: Int -> <IO> ()`), fails
  "body performs effect(s) <IO> ... <IO> Unit <: <> Unit". The callback's
  result is still an existential when the branches are joined, so
  `joinSuspensions` (Frontend/Typecheck.hs) does not apply and the old
  subtype path solves it to `<> Unit`. With the signature `pick2 :: Bool ->
  (Int -> <e> ()) -> <e> ()` it typechecks. Open: instantiate an
  existential branch to a suspension with a fresh row before joining.
- **A newtype over a `type` alias reports a phantom instance.** [reproduced,
  2026-10-21] `type Meters = Int; newtype Wrapped = Meters` fails "Cannot
  declare instance on transparent alias 'Meters'" though no instance is
  written. No NEWT item says an alias may not be a representation.
- **Poor diagnostics on getters.** [reproduced, 2026-10-21] `.[0] p` on a
  record: "Subtype error: Type mismatch fall through a b <: P"; `.a` on a
  list prints "expected: a [] [(a, b)]", an internal row encoding
  (/work/plans/model-audit-2026-10-21/repro/A/pat/s5.loc, s1.loc). "Conflicting fixity definitions for <+>" and
  "Module 'main' does not export ... [nope]" (an export of an undefined
  name, worded backwards) carry no location (DIAG-2; /work/plans/model-audit-2026-10-21/repro/A/op/o8.loc,
  /work/plans/model-audit-2026-10-21/repro/A/mod/m2).
- **Internal names in record and constructor errors.** [reproduced,
  2026-10-21] `f x = match x | Foo = 1 |
  (Bar n) = n` with no signature fails "'Foo' is not a constructor of
  *x`1560@1_x1" (SUM-6 (b) promises a request for a signature). Repro:
  /work/plans/model-audit-2026-10-21/repro/C/rec/getbad.loc, /work/plans/model-audit-2026-10-21/repro/C/sum/sum6.loc.
- **`let` signatures do not parse.** [reproduced, 2026-10-21] Gap in
  POLY-4, which rules `where` signatures: `let g :: a -> a` fails
  "unexpected '::'".
  Repro: /work/plans/model-audit-2026-10-21/repro/D/poly4d.
- **EVAL-5's candidates miss what the code does.** [reproduced,
  2026-10-21] An unused `let` is computed (`let y = boom x in x` fails),
  an unused `where` is not, and a used `let` is placed at its use, so an
  untaken branch skips it. That is a third answer, (c), which
  features-where.asc:126-135 describes. Repro: /work/plans/model-audit-2026-10-21/repro/E/eval.
- **The `@read` rejection suggests `@load`.** [reproduced, 2026-10-21]
  For a function result type the message proposes "`@load path :: <IO>
  (Try Str Int)`". Repro: /work/plans/model-audit-2026-10-21/repro/E/intr2/c.loc.
- **`[Int] ++ [Int]` does not typecheck.** [reproduced, 2026-10-21]
  `pick n xs ++ xs` fails "Cannot compare types [Int] and *f@q1 m@n3
  *a@q2": root's `(++) :: f m a -> f n a -> f (m+n) a` admits only sized
  containers, so plain lists have no `++`. Gap in collections.md: no item
  says how lists are appended.

- **Silent drops left in existential merging.** [read, 2026-11-01] Gaps
  under REC-5, COLL-7 and PAT-1, found reviewing 2617a9e4d; none reproduced
  as a wrong result. In Typecheck/Internal.hs:
  - the ExistU/ExistU `instantiate` rule returns its input unchanged when
    neither existential is in the context (both `access2` probes fail), so
    a merge with a cut or trimmed existential records nothing;
  - the two rules for a bare existential against a record-bearing one
    (above InstLAllR) solve the older bare existential to the newer one,
    breaking context order; only the ExistU/ExistU rule articulates;
  - `apply` on a solved ExistU drops the expression's own slots and keys
    (the FIXME in `instance Applicable TypeU`); `subtype` now checks a
    structured copy against its solution first, but other callers of
    `apply` do not;
  - `solveExist ... >>= maybe (return g) return` in the FunU, EffectU and
    OptionalU articulation rules drops the constraint when the existential
    is already solved;
  - the comment on `accumulatedRecords`' short circuit says merges write
    keys into the ExistG entry; no code does.
- **A native form that leaves out a phantom parameter names no form.**
  [reproduced 2026-11-04] Violates DIAG-1. `newtype Q a b = [b]` with
  `"std::vector<$1>" b` fails "Could not reduce type in broadest scope"
  (CodeGenerator/Infer.hs:544) with no form named.
- **`{x=?Int}` does not parse.** [reproduced 2026-11-02] `=?` lexes as one
  operator ("expected one of: '=', '::'"), and the typecheck printer writes
  `{x=?Int}`, which cannot be read back.
- **A do-block final of unsolved type is not run.** [reproduced
  2026-11-02] `f = (\h -> do h 2) pyLog` against `<IO> ()` fails "inferred
  <> (<IO> Unit)": the block is built as a nested suspension. Same class as
  the EFF-8 entry: an existential's shape is decided before it is solved.
- **A record literal against a record alias fails.** [reproduced
  2026-11-02] `f :: Int -> R; f n = {x = n}` fails: `instantiate`'s alias
  arms recurse into `instantiate`, skipping the closed-row bridge only
  `subtype` applies. ALIAS-11's "Code: conforms" is stale.
- **A curried head under `@collect` loses a type.** [reproduced
  2026-11-02, Python only] `score x = @collect (prod inc x)` fails
  "cannot serialize unknown type variable: b"; the `\k ->` form builds.
- [reproduced, 2026-11-06] A getter takes an effectful receiver without
  checking the field: with `getR :: Int -> <IO> R`, `bad :: Int -> Str;
  bad x = .zzz (getR x)` typechecks although `R` has no `zzz`, because the
  suspension solves the selector's open record variable. `bad :: Int -> Int;
  bad x = .a (getR x)` builds and fails at run time with `'functools.partial'
  object is not subscriptable`. The same receiver through a bracket or a
  setter is rejected.
- [reproduced, 2026-11-06] The plain getter typechecks its receiver twice
  (`synthE` PatGet: `synthG`, then `checkG` on the same expression), so time
  is exponential in nested getters: `bad x = .a (step (.a (step ... x)))`
  takes 2.6 s at depth 12 and exceeds 25 s at depth 14. The index rule reuses
  its synthesized receiver.
- [reproduced, 2026-11-06] Surplus arguments to a getter make it a setter:
  `bad :: R -> R; bad r = .a r 5` compiles as an update returning a record,
  and with a function field `.f (mkr k) 5` is rejected while `(.f (mkr k)) 5`
  runs, against APP-1 (`applyPattern` decides by argument count; only the
  setter syntax should build a setter).
- **A record applied to a variable does not match a class variable.**
  [reproduced, 2026-11-07, also on b05f877b5] With `class Other f where other ::
  f a -> Str`, `record Box a where val :: [a]`, `instance Other Box` and
  `mkB :: [a] -> Box a`, the definition `k :: [a] -> Str; k xs = other (mkB
  xs)` fails "Type mismatch: expected: a b, inferred: Box a". The same call at a
  ground type (`other (mkB [n])` with `n :: Int`) and a `newtype Box a = [a]` in
  place of the record both work. Spec gap or defect against CLS-2: a record is a
  type constructor like any other.
- **A literal at a constrained variable fails inside the body at some
  instances.** [reproduced, 2026-11-07, installed 0.112.0] Violates SIG-1.
  `inc :: Numeric a => a -> a; inc x = x + 1` passes the check against its
  signature (`literalAtRigid`), and used at `Vector 3 Real` (vector-py's
  `instance Numeric a => Numeric (Vector n a)`) is refused at the literal in
  its body: "Numeric literal cannot have type Vector 3 Real". The signature
  admits a type its body does not. Bound up with the literal ruling (PLAN
  item 4 of /work/plans/typecheck-once).
- **A Set variable in a signature is never instantiated.** [reproduced,
  2026-11-07, installed 0.112.0] Violates POLY-1. With `type Tags (s :: Set)`,
  `size :: Tags s -> Int` and `mkA :: Int -> Tags (Keys {a = Int})`, the
  definition `f :: Int -> Int; f k = size (mkA k)` fails "expected: Tags s,
  inferred: Tags {"a"} ... Type mismatch fall through". Typecheck/Internal.hs
  `renameWithMap` freshens Nat, Str, Rec, List and effect variables per use
  but not Set variables (there is no Set collector), and the subtype rules do
  not take the signature's `s` for a Set expression.
- **A type mismatch can print two variables as one.** [reproduced,
  2026-11-07] Spec gap in diagnostics.md. `both :: Tag u -> Tag u -> Int; q ::
  Tag s -> Tag t -> Int; q a b = both a b` reports "expected: Tag a, inferred:
  Tag a ... a <: a": each side's variables are renamed for display on their
  own, so `s` and `t` both print as `a`, and the message cannot show which
  variables collided.

- **A definition whose tree cannot be collected is left ungeneralized
  silently.** [read, 2026-11-08, c3a7b3bb3] library/Morloc/Frontend/Generalize.hs
  ~118-121: when `definitionTree`/`referenced` raises a program error, `visit`
  only prints it under `-v` ("not generalized ... could not be collected")
  and goes on. The user's error is lost, and the definition is expanded at
  each use, where the failure shows up later in a less specific form, or
  never if the definition is unused. Also, the doc comment "The unsigned
  top-level terms a definition's tree expands." appears twice (~156-158).
- **A NUM-10 default that conflicts is dropped silently.** [read,
  2026-11-08, c3a7b3bb3] library/Morloc/Frontend/Typecheck.hs ~161:
  `defaultNumeric`'s `solveTo` discards a `Left` from `solveExist` and keeps
  gamma, so an existential that NUM-10 says to default, but that cannot
  be solved to Int or Real, stays unsolved without a message. The failure
  surfaces later as an unrelated ambiguity error, or not at all.
- **Outside shadow mode, a definition the instantiated check refuses
  fails the build, even where checking at the key accepts it.** [read,
  2026-11-08, c3a7b3bb3] library/Morloc/Frontend/Specialize.hs ~406: the
  fallback to checking at the key runs only when the result still holds an
  existential. The shadow mode (`MORLOC_SHADOW_ELABORATE`) reports exactly
  this divergence, so the case occurs. That mode also ships in the
  compiler and changes which tree is compiled when it is set (~99): the
  generated code depends on an environment variable.
- **Every compile typechecks every imported module.** [read, 2026-11-08,
  c3a7b3bb3] library/Morloc.hs ~91: each `morloc make` and `typecheck`
  validates every signed definition and instance body of every imported
  module, and infers every unsigned definition, where before only the root
  module's were checked. Generalize collects a definition's tree two or
  three times. Compile time then grows with the stdlib, not the program;
  measure it and consider caching per module.
