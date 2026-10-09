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
