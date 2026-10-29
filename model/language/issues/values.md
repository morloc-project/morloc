# Values in each language

Values that are wrong, dropped or unsupported in some language. Reproduced
on 0.109.0 unless marked.

- **Native form dropped silently.** [reproduced] Violates NEWT-4.
  `import root-py; newtype Path = Str; type Py => Path = "pathlib.Path";
  source Py from "native.py" ("kind" as pathKind); pathKind :: Path -> Str`
  with `def kind(x): return type(x).__name__` builds without a warning;
  `./prog pathKind notes/report.txt` prints `"str"`.
- **A received newtype with no Packable instance ignores its form.**
  [reproduced, 2026-10-28] Violates NEWT-4 and NATIVE-9. `newtype Frozen a =
  [a]; type Py => (Frozen a) = "tuple" a` with no Packable instance builds; a
  `Frozen Int` returned by a C++ function reaches Python as a list, while a
  Frozen literal is refused for want of an instance. Repro:
  /tmp/claude-1000/-work/b22ec8aa-199c-401c-8f03-0b140cbccdb1/scratchpad/n10/noinst2.loc.
  NEWT-4 asks which forms a binding can build without an instance; nothing
  records that today.
- **C++ cannot send or receive a sum type whose arm holds a Packable.**
  [reproduced, 2026-10-29] Violates NATIVE-9. `mk :: Int -> Try Str (Set
  Int)` from Python into a C++ `size :: Try Str (Set Int) -> Int` does not
  build: "the `data` type Try_7b5513 is used with two field types for its
  constructor Ok: (std::set<int>) and (std::vector<int>)". The wire form of
  the variant is given the native variant's name
  (library/Morloc/CodeGenerator/Pools/CAbi/Members/Cpp.hs, `rawDoc` and
  `collectCppVariants`), so the decoded and native forms collide. Writer
  intrinsics avoid this by leaving such a value to the variant's own
  marshaller, which unpacks only its outer type, so in C++ `@show`,
  `@hash` or `@save` of a value holding such a sum type below its top
  (`[Try Str (Set Int)]`, a record with a `Try Str (Set Int)` field) still
  reaches the runtime in native form and fails "no marshaller for a value of
  this C++ type". Python and R take it apart at every level.
- **An R set of compound elements is not sorted.** [read, 2026-10-29]
  Violates COLL-6. set-r sorts a set of scalars before it is sent, but
  leaves a set of tuples or records in the order it was built, since R has
  no ordering of them that matches C++ and Python, so `@hash` and `@eq` of
  equal `Set (Int, Str)` values built in different orders can differ.
- **A recursive record with a Packable field is rebuilt at its top level
  only.** [read, 2026-10-28] Violates NATIVE-9. A back-reference
  (`SerialRec`) counts as serializable, so `record Tree where tags :: Set
  Int; kids :: [Tree]` received or read is rebuilt at its top level and its
  `kids` are decoded straight into their type, with no packer for `tags`.
- **A Python Vector built by library code has numpy's default dtype.**
  [read, 2026-10-28] Violates NATIVE-7, which makes a `Vector n U8` uint8
  however the program builds it. vector-py's `map`
  (`morloc_vec_map`) and an explicit `pack` of a list (`morloc_unpackVector`)
  call `np.asarray`, so a `Vector n U8` result is int64 and a `Vector n Int`
  is int64 rather than object; the functions are never given the element
  type, and such an array is writable where a literal or received one is
  read-only (NATIVE-9 covers only those). Open question: pass such functions
  the target schema, or narrow NATIVE-7 to literals.
- **A C++ source returning a nested Vector does not build.** [reproduced,
  2026-10-28] `makeN :: Int -> Vector n (Vector 2 Real)` sourced from C++
  (`std::vector<std::vector<double>>`), with root-cpp, vector, vector-cpp:
  `morloc make` stops with "Recursive type alias `Vector` has no concrete
  C++ mapping" (library/Morloc/CodeGenerator/Pools/CAbi/Members/Cpp.hs,
  leakError), though nothing is recursive. Repro:
  /tmp/claude-1000/-work/b22ec8aa-199c-401c-8f03-0b140cbccdb1/scratchpad/n96/.
- **A native form for an applied type is not passed to the decoder.**
  [reproduced, 2026-10-28] Violates NATIVE-1. A schema hint is taken from
  the head's general mapping, so a form declared for an applied type
  (`type L => (T X) = "..." X` beside `type L => (T a) = "..." a`) never
  reaches the decoder. Seen with Python `List U8 = "bytes"`, now refused by
  NATIVE-7; R's `List U8 = "raw"` works only because rmorloc routes every
  U8 array to raw.
- **A literal of a newtype over a Packable type is rejected.** [reproduced,
  2026-10-18] `import root-py; import map-py; newtype Holder = Map Str Int`;
  `[("a", 1)] :: Holder` fails "Cannot construct a value of 'Holder' from a
  List literal in py ... declare 'instance Packable List Holder'", though
  `pack ([("a", 1)] :: Map Str Int)` (with `deriving (Packable)`) builds the
  same value. The literal code (library/Morloc/CodeGenerator/Express.hs,
  `maybePackableWrap`) looks for an instance of the outer newtype only and
  does not compose the one of the type it wraps. Gap: no item says how a
  literal reaches a type through several representations.
- **Comparison on `data` with fields.** [reproduced] Violates SUM-5.
  `data Shape = Circle Real | Rect Real Real | Dot`, `lt a b = a < b`,
  `eq a b = a == b`. Python is correct. R: `==` works, `<` fails at run
  time "comparison of these types is not implemented" (root/main.loc:28).
  C++: `==` and `<` fail to build (root-cpp core.hpp:654, :659 "no match for
  operator==/operator<=" on `Shape`). Enum-only `data Color = Red | Green |
  Blue` works in all three.
- **Table column sum fails in Python.** [reproduced] Violates WIRE-1.
  Census table built with `addCol`; `summarize t = fold (+) 0 (getCol "pop"
  t)` fails for CSV, Parquet, inline JSON and stdin input: "Expected int for
  MORLOC_INT, but got numpy.int64" (pymorloc.c:744, py_size_step). The
  manual shows `54585374`.
- **No rule forbids a sourced function mutating its argument.** [read] A
  gap in EVAL-7. In Python a morloc value read twice is one shared object,
  so a sourced `def f(xs): xs.sort(); return len(xs)` changes the value
  every other reader of `xs` sees, and whether it does depends on whether
  the compiler shared or copied it. EVAL-7 requires that copying be
  unobservable but says nothing about mutation; R copies on modify, C++ and
  Rust pass shared values as `const&`/`&`. Needs a ruling: forbid it
  (unchecked, as EVAL-7 is) or have the Python pool copy arguments.
- **A `where` binding that names a parameter is rejected as cyclic.**
  [reproduced, 2026-10-17, 9f24abe4] `f x = d where d = x` typechecks but
  `morloc make` fails "Self-referential or cyclic binding: 'd`1467'";
  `let d = x in d` builds. `hasGround` returns False for a bound variable
  (library/Morloc/Frontend/Valuecheck.hs, `groundCheck`), so a binding whose
  only definition is a parameter counts as ungrounded. Spec gap: no item
  says what a binding may name, and `typecheck` and `make` disagree.
- **NEWT-4, output side: an unsupported native form crashes at run time.**
  [reproduced, 2026-10-21] `newtype Path = Str; type Py => Path =
  "pathlib.Path"`, Py `mk :: Str -> Path` returning `pathlib.Path(s)`, Cpp
  caller `u s = unpack (mk s)`: builds with no warning; running `u
  notes.txt` fails "Expected str or bytes for MORLOC_STRING, but got
  PosixPath (pymorloc.c:892)". Repro: /work/plans/model-audit-2026-10-21/repro/B/t/n7/c2.loc.
- **An unmapped record passed to a sourced C++ function does not compile.**
  [reproduced, 2026-10-21] Violates NATIVE-4; contradicts NATIVE-3.
  `record P where x :: Int; y :: Real`, no `record Cpp =>`, sourced
  `getx :: P -> Int` (`template <class T> int getx(const T& p)`),
  `f p = getx p`: g++ fails "pool.cpp:137: 'P' was not declared in this
  scope". With no sourced function (`f p = .x p`) the same record builds
  and runs in C++ and Rust, so a struct is generated on one path only,
  though NATIVE-3 (ruled) requires a user struct. Either reject naming P
  and Cpp, or always generate (#66). Repro: /work/plans/model-audit-2026-10-21/repro/B/t/nat/b.loc, a.loc, c.loc.
- **A Python record class form is accepted and ignored.** [reproduced,
  2026-10-21] Gap in NATIVE-5 (open). `record Py => P = "MyCls"` builds;
  the Python function receives a `dict`. No candidate answer describes a
  silent drop. Repro: /work/plans/model-audit-2026-10-21/repro/B/t/nat/n5.loc.
- **Out-of-range list index gives a wrong value in C++ and R.**
  [reproduced, 2026-10-21] Violates COLL-3 (same result in every language);
  every COLL-4 candidate excludes a silent wrong value. With a pool-computed
  list (`ir xs = (.[5] (incR xs)) + 1.0`): R prints `"nan"` for `[1,2,3]`,
  `"NA"` for a `[Str]`, and `.[-5]` on 3 elements gives the wrong element
  (R drop-index); an `[Int]` fails "Cannot pack NA or NaN into MORLOC_INT".
  C++ `.[5]` on 3 elements returns garbage (`1`); `.[-1]` on `[]` is
  SIGSEGV. Python and the nexus raise "index out of range"; Rust throws.
  Cause: root-cpp core.hpp:72-77 `morloc_at` has no bound check; root-r
  R/core.R:207-216 adds `length(xs)` and indexes `xs[i + 1]`. Repro:
  /work/plans/model-audit-2026-10-21/repro/C/coll/i-r.loc, /work/plans/model-audit-2026-10-21/repro/C/num/get-cpp.loc.
- **`size` of a `Str` counts code points in Python and R, bytes in C++ and
  Rust.** [reproduced, 2026-10-21] BASE-5 is open, but the languages
  disagree today (COLL-3). `slen s = size s` on U+00E9: 1 in py and r, 2 in
  cpp and rust. Repro: /work/plans/model-audit-2026-10-21/repro/C/num/, /work/plans/model-audit-2026-10-21/repro/C/lit/nul.loc.
- **R drops `Null` values from a `Map Str ?v`.** [reproduced, 2026-10-21]
  Violates OPT-1. `optmap xs = pack xs` (`[(Str, ?Int)] -> Map Str ?Int`,
  map-r) on `[["b",null],["a",2]]` prints `[["a",2]]`; py and cpp keep both
  keys. Cause: map-r R/map.R:10-16 and :30-36 assign `result[[k]] <- NULL`,
  which deletes the key.
- **C++ realizes `?(?T)` as a nested `std::optional`.** [reproduced,
  2026-10-21] Violates OPT-2 and OPT-4. A sourced
  `cnest :: ?(?Int) -> Str` written as `std::string cnest(const
  std::optional<int>&)` fails to compile: the pool passes
  `std::optional<std::optional<int>>`. Python and R collapse. Repro:
  /work/plans/model-audit-2026-10-21/repro/C/opt/n.loc. Related: tables-tensors.md `?(?Int)` column.
  A collapse only in morloc's types was tried and reverted 2026-10-27.
  Collapsing needs, together: every `OptionalU` construction collapsing,
  `apply` and `subtype` included (else `find absent xs` at `[?Int]` is
  typed `??Int` and rejected, and `dflt :: a -> ?a -> a` at `?Int` fails);
  the structural walks (Infer `structuralCompound`) and `weaveTypeF` at
  unequal depths; and an adapter at the native call boundary, since a C++
  template or Rust generic instantiated at `a = ?T` returns
  `optional<optional<T>>` (golden optional-cpp's natives take nested
  optionals and need rewriting). Languages also disagree on a value:
  `fromNull :: a -> ?a -> a` at `a = ?Int` with `state2 :: ?(?Int) -> Str`,
  `state2 (fromNull (Null :: ?Int) Null)` is present-with-inner-absent in
  C++ and Rust and absent in Python.
- **NUM-3's text is stale.** [reproduced, 2026-10-21] NUM-3 (ruled
  2026-10-04) says R holds `I64`/`U64` as doubles exact below 2^53; the
  2026-10-16 ruling and the code use bit64 `integer64` (`cI64 5` has class
  integer64; I64 9007199254740993 round-trips; U64 2^64-1 fails "does not
  fit in R's integer64"). That ruling also makes `[U8]` an R `raw` and
  truncates fractional doubles at integer slots; no item records either.
  Needs NUM-3 rewritten from the newer ruling.
- **Number rules with no item.** [reproduced, 2026-10-21] Gaps in
  numbers.md.
  - Real division by zero: `1.0 / 0.0` is `inf` in cpp, r and rust, an
    error "float division by zero" in py (likewise `//`). Candidates: IEEE
    everywhere; failure everywhere; stated per language.
  - Real `%` is floored in all four (`-7.0 % 2.0` is `1`) but in no item.
  - `x :: Int; x = 3000000000 + 1` under root-cpp builds and prints
    -1294967295: NUM-5 does not say whether an `Int` literal is checked
    against `[[Int]]_L` of its realization (/work/plans/model-audit-2026-10-21/repro/C/lit/intc.loc).
  - Map pair order: py and r keep insertion order, cpp sorts by key. Is
    order part of a map's value (COLL-5 covers repeated keys only)?
- **R overflow message names the wrong R type for `Int`.** [reproduced,
  2026-10-21] data/lang/r/rmorloc.c:1701-1704: a multi-limb `Int` arriving
  in R fails "does not fit in R's numeric type (max 2^53 ...)"; `[[Int]]_R`
  is R integer, as the single-limb branch (:1694-1697) says.
- **`@schema` and `@typeof` evaluate their argument.** [reproduced,
  2026-10-21] Violates INTR-4. `schemaBoom x = @schema (boom x)` with a
  raising Python `boom` fails "run failed / boom". features-intrinsics.asc:
  449-450 says the argument is not evaluated. Repro: /work/plans/model-audit-2026-10-21/repro/E/intr/main.loc.
- **REAL-1 vs NUM-1: the realizer's choice changes a program's value.**
  [reproduced, 2026-10-21] REAL-1 says the choice changes cost, never
  meaning; NUM-1 (ruled) makes `Int` width per language. With sourced
  identities `pid` (py) and `cid` (cpp), `hc x = pid (cid x * 3)` prints
  -1294967296 for 1000000000 (the realizer placed `* 3` in C++) and
  `hp x = pid (pid x * 3)` prints 3000000000. C++ signed overflow is also
  undefined behaviour, not an overflow behavior. Same for `size` of a
  `Str` and real division by zero (above). Candidates: (a) REAL-1 holds
  only for values every candidate language represents, stated so; (b)
  `Int` arithmetic is pinned to one checked semantics; (c) the realizer
  may move `Int` arithmetic only between languages of equal width.
  Repro: /work/plans/model-audit-2026-10-21/repro/F/wire/re.loc.
- **INTR-5 says a program that is not installed sees `p` unchanged.**
  [reproduced, 2026-10-23] Pools and (since 2026-10-23) the nexus join a
  literal `@datafile` path with the program's source root, installed or
  not, so a built program reads its data file from any working directory.
  The item's wording needs updating to that, or the code to the wording.
