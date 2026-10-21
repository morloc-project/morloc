# Typechecking gaps

Programs that typecheck and fail later, rejections with no location, and
literal and record gaps. Reproduced on 0.109.0 unless marked.

- **Non-`Type` signature passes typecheck.** [reproduced] Violates KIND-2.
  `import root-py; type R = Singleton "x" Int; f :: R -> Int; f x = 1`:
  typecheck prints `f :: R -> Int`; make fails "cannot serialize type
  Singleton "x" Int -- no per-language alias resolution for Singleton",
  located at the export list.
- **Bare `Vector` passes typecheck.** [reproduced] Violates KIND-5.
  `import vector-py; a :: Vector -> Str; a t = "x"`: typecheck accepts;
  make fails "cannot serialize parameterised pure morloc type: Vector" at
  the export list.
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
  (location of the construct): every make-time failure above, and an
  exported `g :: Buffer (3 - 10) Int -> Int` ("A dimension may not be
  negative"), and a list or record table column (`t :: Table 2 {xs =
  [Int]}`, accepted by typecheck, refused by make), point at
  `main.loc:1:14`. See model/compiler/issues/typecheck-output.md.
- [reproduced] KIND-4 open: an unexported, unused
  `f :: Buffer (3 - 10) Int -> Int` is accepted silently; exported or
  called, it is rejected.
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
- **Rigid Nat variables, run-time consequence.** [reproduced] Instance of
  kinds-dimensions.md "Signature Nat variables are not rigid". Python
  `outer` returns the flat outer product; `f :: Vector m Real -> Vector p
  Real -> Vector (m * p) Real`, `g :: Vector n Real -> Vector k Real ->
  Vector (n + k) Real; g = f` builds, and `./prog g '[1,2]' '[1,2,3]'`
  prints 6 elements typed `Vector 5`. A concrete caller `h :: Vector 2 Real
  -> Vector 3 Real -> Vector 5 Real; h = g` is rejected (6 ~ 5).
- **Literal in a point-free section ignores a `Real` context.**
  [reproduced] Violates LIT-2. `divTwo :: [Real] -> [Real]; divTwo = map
  (2 /)` fails "expected: [Real] -> [Real], inferred: a Int -> a Int";
  `divTwo xs = map (\x -> 2 / x) xs`, `half x = x / 2`, `w :: Real; w = 1`
  and `[1, 0xff, 2.5] :: [Real]` all work.
- **Manual table examples do not typecheck.** [reproduced] features-tables.asc
  builds `census` with `setCol "pop" pops (asCol "state" states)`; the
  installed `setCol` replaces a column (`Table n (l + Singleton f b + r) ->
  ...`), so it fails "Rec is missing the field that pins a row variable:
  pop". `addCol` appends. Every manual example using `setCol` to add a
  column (census, withDensity, stacked.loc) is stale.
- Not reproduced, manual stale: `cbind` of two tables sharing a column is
  now rejected ("both sides have: state"); the Packable mismatch error no
  longer prints compiler internals (types-custom-types.asc quotes the old
  message).
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
- **A complete set of nested patterns is rejected as not exhaustive.**
  [reproduced, 2026-10-17, 9f24abe4] Violates PAT-3.
  `data L = Nil | Cons Str L`;
  `f xs = match xs | (Cons x (Cons y rest)) = 2 | (Cons x Nil) = 1 | Nil = 0`
  fails "`|` patterns for 'match' are not exhaustive; missing Cons" though
  the clauses cover every value; writing `(Cons x _)` for the second clause
  builds. The check is `checkRefutCoverage`
  (library/Morloc/Frontend/Desugar.hs); cause not traced.
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
  Gap: no item says when a constrained signature is ambiguous. Candidate:
  a signature whose constraint names a variable that its type does not
  mention (and that no other constraint ties to one it does) is rejected at
  the signature, naming the variable.
- **UNSOUND: bracket getters accept any `f a` receiver, slices any
  receiver; the built program segfaults.** [reproduced, 2026-10-21]
  Violates PAT-5. `f :: Int -> Int; f x = .[0:1] x` typechecks and builds;
  `f 5` exits 139 (SIGSEGV). `g :: (Int, Int, Int) -> (Int, Int, Int);
  g x = .[0:2] x` and `tidx :: (Int, Int) -> Int; tidx p = .[0] p` build
  and fail "rel2abs: relptr 1 is in volume 0, which is never mapped".
  Cause [read]: Frontend/Typecheck.hs:1200-1209 types the index receiver as
  `AppU fExist [elem]` (so `(,) Int` matches `(Int, Int)`); :1224-1251 put
  no constraint on a slice receiver. Repro: /work/plans/model-audit-2026-10-21/repro/A/pat/sl.loc, rt.loc.
- **A tuple pattern of smaller arity matches a larger tuple.** [reproduced,
  2026-10-21] Violates PAT-1. `part :: (Int, Int, Int) -> Int;
  part = \(a, b) -> a + b` builds; `part [1,2,3]` prints 3. Repro:
  /work/plans/model-audit-2026-10-21/repro/A/pat/rt.loc.
- **Mutually recursive `where` functions are accepted; value cycles crash
  `make`.** [reproduced, 2026-10-21] Violates BIND-2. `f x = ev x where
  ev n = ? n == 0 = True : od (n - 1); od n = ? n == 0 = False : ev (n - 1)`
  builds and `f 4` prints `true` (/work/plans/model-audit-2026-10-21/repro/A/bind/b9.loc). `f x = a where a = b +
  1; b = a + 1` passes typecheck and `make` dies "This step should be
  unreachable" (CodeGenerator/Serialize.hs:308; /work/plans/model-audit-2026-10-21/repro/A/bind/b2.loc). A
  top-level cycle `a = b + 1; b = a + 1; f = a` dies "Can represent MonoSrc
  as SerialExpr" (Serialize.hs:241; /work/plans/model-audit-2026-10-21/repro/A/lex/x3.loc) instead of the cyclic
  binding error. Either BIND-2 or the code must move for functions; value
  cycles need a located rejection either way.
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
- **A clause after a catch-all is accepted.** [reproduced, 2026-10-21]
  Violates SUM-3. `data A = Foo | Bar; f | Foo = 1 | _ = 2 | Bar = 3`
  builds; `f Bar` prints 2. A repeated constructor clause is rejected; only
  clauses shadowed by `_` are missed. Repro: /work/plans/model-audit-2026-10-21/repro/C/sum/dead.loc.
- **Internal names in record and constructor errors.** [reproduced,
  2026-10-21] `g p = .c p` on `record P {a, b}` fails "expected: a []
  [(c, b)] ... Error in NamU with conflicting open keysets ... hint: 'P' is
  a newtype"; it should name the missing field. `f x = match x | Foo = 1 |
  (Bar n) = n` with no signature fails "'Foo' is not a constructor of
  *x`1560@1_x1" (SUM-6 (b) promises a request for a signature). Repro:
  /work/plans/model-audit-2026-10-21/repro/C/rec/getbad.loc, /work/plans/model-audit-2026-10-21/repro/C/sum/sum6.loc.
- **A type variable in a local signature can never be instantiated.**
  [reproduced, 2026-10-21] Violates POLY-1. `f :: Int -> Int; f x = g x
  where { g :: a -> a; g y = y }` fails "expected: a, inferred: Int ...
  Cannot compare types Int and a@q0" at `g x`. Without its signature `g` is
  generalized and works. POLY-4 also names `let` signatures, which do not
  parse ("unexpected '::'"). Repro: /work/plans/model-audit-2026-10-21/repro/D/poly4c, poly4d, poly5.
- **Literal defaulting exports a constrained generic at Int.**
  [reproduced, 2026-10-21] Gap in SIG-5 and POLY-6. `f :: Integral a => a
  -> a; f x = x + x` is skipped with a warning (SIG-5), but `f x = x + 1`
  is exported at Int with no warning: `f 1.5` fails "expected integer for
  Int", while `g y = f y` at Real gives 2.5. Candidates: (a) a constrained
  export is skipped or rejected regardless of literals; (b) class
  variables default (Integral -> Int, Numeric -> Real) and SIG-5/POLY-6
  say so. Repro: /work/plans/model-audit-2026-10-21/repro/D/sig5b, sig5c.
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
