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
- **Missing column found at code generation.** [reproduced] Violates TAB-3.
  `oops = getCol "poop" census` (census a `Table 2 {state = Str, pop =
  Int}`): typecheck gives `Vector 2 {state=Str, pop=Int}."poop"`; make fails
  "The type of this term is not determined: nothing in the program fixes
  'a'" at the export list, not naming the column. table/main.loc's comment
  on getCol claims a compile-time error.
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
  (location of the construct): every make-time failure above, and a list
  or record table column (`t :: Table 2 {xs =
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
- **Literal in a point-free section ignores a `Real` context.**
  [reproduced] Violates LIT-2. `divTwo :: [Real] -> [Real]; divTwo = map
  (2 /)` fails "expected: [Real] -> [Real], inferred: a Int -> a Int";
  `divTwo xs = map (\x -> 2 / x) xs`, `half x = x / 2`, `w :: Real; w = 1`
  and `[1, 0xff, 2.5] :: [Real]` all work.
- Manual stale: the Packable mismatch error no longer prints compiler
  internals, but types-custom-types.asc quotes the old message. [not
  re-verified 2026-10-31]
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
- **A class variable that only a constraint mentions.** [reproduced,
  2026-10-18, 0.109.0] Violates POLY-6 and DIAG-2. In `f :: C a => Int ->
  Str`, nothing at any call can fix `a`, but the signature is accepted and
  `typecheck` passes every caller; what happens next depends on the body
  and on how many instances are in scope:
  - `class Gen a where gen :: Int -> [a]`, `render :: Gen a => Int -> Str;
    render n = showList (gen n)`, `useIt n = render n`. With only `instance
    Gen Int` in scope, `make` builds and `./prog useIt 5` prints `"[5]"`: the
    sole instance is chosen though nothing chose it. Add `instance Gen Str`
    and `make` fails "The type of this term is not determined: nothing in
    the program fixes 'a'" at `gen n`. An instance added by an unrelated
    import changes a working program into a rejected one.
  - The same with a body that calls a library function: `ingest :: C a =>
    Int -> Str -> Str -> <IO> (); ingest l i o = saveSource l o (readFormat
    i)` fails at `@open` inside the library's `saveSource`, not at
    `ingest`.
  - Two-parameter class: `class Conv f a where conv :: f -> [a]`; `size ::
    Conv f a => f -> Int; size x = length (conv x)` fails in typecheck with
    "This definition needs Conv f (a) but its signature does not declare it
    (the signature's constraints and their superclasses give Conv f a)",
    which reads as a missing constraint the signature visibly has. The
    message should say `a` is undetermined.
  - `count :: Named a => Int -> Int; count n = n + 0` (no method used)
    builds silently.
  CLS-18 (ruled 2026-10-29) makes this an error at the signature, naming
  the variable; the code deviates. Re-reproduced 2026-10-31 (`count`).
  The mechanism: resolution solves a use's open type from the one matching
  instance group (Frontend/Typecheck.hs `resolveInstances`, from the
  implementations or, in a shallow pass, the instance types). The same
  step fixes a newtype's representation in `map unpack ts`, which needs a
  ruling on whether a Packable instance determines its packed type.
- **A method use nothing determines is accepted inside a concrete
  export.** [reproduced, 2026-11-02] Violates the second sentence of
  CLS-18. With `instance Monoid Str` and `instance Monoid (List a)`,
  `k :: a -> b -> a; x :: Bool; x = k True mempty` typechecks; nothing
  fixes the type of `mempty`. It also sends `x` to the specializer's
  in-place fallback (model/compiler/issues/specialization.md).
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
  2026-10-21] `g p = .c p` on `record P {a, b}` fails "expected: a []
  [(c, b)] ... Error in NamU with conflicting open keysets ... hint: 'P' is
  a newtype"; it should name the missing field. `f x = match x | Foo = 1 |
  (Bar n) = n` with no signature fails "'Foo' is not a constructor of
  *x`1560@1_x1" (SUM-6 (b) promises a request for a signature). Repro:
  /work/plans/model-audit-2026-10-21/repro/C/rec/getbad.loc, /work/plans/model-audit-2026-10-21/repro/C/sum/sum6.loc.
- **A class obligation through a type alias is not covered.**
  [reproduced, 2026-10-30, not bisected] With `type Model k v = [(k, v)]`,
  `modelStep :: Ord k => ... ; ... without k xs <> [(k, v)] ...` fails
  "This definition needs Semigroup (Model k Int) but its signature does
  not declare it", though `[a]` has a Semigroup instance. Repro:
  /work/tests/sumtypes/07-persistent-redblack/core/rbtree/main.loc:265.
- **`let` signatures do not parse.** [reproduced, 2026-10-21] Gap in
  POLY-4, which rules `where` signatures: `let g :: a -> a` fails
  "unexpected '::'".
  Repro: /work/plans/model-audit-2026-10-21/repro/D/poly4d.
- **Literal defaulting exports a constrained generic at Int.**
  [reproduced, 2026-10-21] Violates SIG-5 and CLS-18 (no defaulting).
  `f :: Integral a => a -> a; f x = x + x` is skipped with a warning, but
  `f x = x + 1` is exported at Int with no warning: `f 1.5` fails
  "expected integer for Int", while `g y = f y` at Real gives 2.5. Repro: /work/plans/model-audit-2026-10-21/repro/D/sig5b, sig5c.
- **SIG-4's premise is stale.** [reproduced, 2026-10-21] It says
  inference fails for a point-free use of class methods; `g = (+)` and
  `concatTo = fold (<>)` build and run unsigned. Only constructor matching
  still fails, with "'Ci' is not a constructor of *s`1591@1_x1" rather than
  a request for a signature (SIG-4 (a)). Repro: /work/plans/model-audit-2026-10-21/repro/D/sig4, sig4b.
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

- **An application through a type variable that stands for a function is
  rejected.** [reproduced, 2026-10-27] Awaits APP-4 (open): under its
  candidate (a), with APP-1 (`a -> b -> c` and `a -> (b -> c)` are one
  type), it is well typed. `first2 :: [Int -> Int -> c] -> Int -> c` and
  `d3 :: Int -> Int -> Int -> Int`: `first2 [d3] x 7` is refused with
  "Invalid function application of type: [Int -> Int -> a] -> Int -> a",
  while `apply1 (first2 [d3] x) 7` builds and runs. With `c = Int -> Int`
  the three-argument application is well typed.
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
- **UNSOUND: a native type form's argument order is ignored.** [reproduced
  2026-11-02] A form `"std::tuple<$2,$1>" b a` builds a value whose fields
  are swapped: `sndp '[3,2.75]'` prints `2`. Infer.hs `appArgsCorrespond`.
  Analysis in /work/plans/source-type-forms.md.
- **`{x=?Int}` does not parse.** [reproduced 2026-11-02] `=?` lexes as one
  operator ("expected one of: '=', '::'"), and the typecheck printer writes
  `{x=?Int}`, which cannot be read back.
- **A do-block's literal final ignores the expected payload type.**
  [reproduced 2026-11-02] `f :: <IO> Real; f = do 2` fails "inferred <>
  Int"; `do 1 + 2` too, while `do [1,2]` passes: a bare literal is
  synthesized at once and there is no single-layer check rule for a do
  block (briefs/B16-B17.md).
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
