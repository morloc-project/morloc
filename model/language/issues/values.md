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
  [reproduced 2026-11-02, briefs/A5-B4.md] set-r/R/set.R:30-37
  `morloc_unpackSet` returns any non-scalar set unsorted. Needs a ruling beyond the set-r sort: `Set (Set Int)` (the outer unpack
  runs before the inner ones, so it sorts unsorted lists) and `Set Color`
  from an R source returning the enum as character (sorted by name, not
  tag) cannot be ordered without the element schema. Options: give
  Packable functions the element schema, unpack inner-first, or refuse
  these element types in R sets at compile time.
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
- **Table column sum fails in Python.** [reproduced] Violates WIRE-1.
  Census table built with `addCol`; `summarize t = fold (+) 0 (getCol "pop"
  t)` fails for CSV, Parquet, inline JSON and stdin input: "Expected int for
  MORLOC_INT, but got numpy.int64" (pymorloc.c:960-963, py_size_step: MORLOC_INT requires PyLong_Check). The
  manual shows `54585374`.
- **No rule forbids a sourced function mutating its argument.** [read] A
  gap in EVAL-7. In Python a morloc value read twice is one shared object,
  so a sourced `def f(xs): xs.sort(); return len(xs)` changes the value
  every other reader of `xs` sees, and whether it does depends on whether
  the compiler shared or copied it. EVAL-7 requires that copying be
  unobservable but says nothing about mutation; R copies on modify, C++ and
  Rust pass shared values as `const&`/`&`. Needs a ruling: forbid it
  (unchecked, as EVAL-7 is) or have the Python pool copy arguments.
- **NEWT-4, output side: an unsupported native form crashes at run time.**
  [reproduced, 2026-10-21] `newtype Path = Str; type Py => Path =
  "pathlib.Path"`, Py `mk :: Str -> Path` returning `pathlib.Path(s)`, Cpp
  caller `u s = unpack (mk s)`: builds with no warning; running `u
  notes.txt` fails "Expected str or bytes for MORLOC_STRING, but got
  PosixPath (pymorloc.c:1000)" [reproduced 2026-11-14]. Repro: /work/plans/model-audit-2026-10-21/repro/B/t/n7/c2.loc.
- **A Python record class form is accepted and ignored.** [reproduced,
  2026-10-21] Violates GEN-11. `record Py => P = "MyCls"` builds;
  the Python function receives a `dict`. The golden object-1-py passes only
  because its function is an identity and the output is JSON. Repro: /work/plans/model-audit-2026-10-21/repro/B/t/nat/n5.loc.
- **A Python record literal is an OrderedDict, a received record a dict.**
  [read, 2026-11-15] Violates NATIVE-9. Literals are built with
  `ldRecordConstructor: "OrderedDict"` (data/lang/py/lang.yaml:50;
  PseudoCode.hs:122 matches it); pymorloc.c reads every record with
  `PyDict_New` (MORLOC_MAP, ~2178). `type()` and `repr()` differ.
- **Reading a field of a Python `object` emits attribute access on a dict.**
  [read, 2026-11-15, not reproduced] Violates GEN-11. genericRecordAccessor
  (Grammars/Translator/Generic.hs:768-769) emits `.f` for `NamObject`, but
  pymorloc builds the value as a dict, so `.f p` on an `object Py` record
  should fail at run time. object-1-py never reads a field.
- **Python equality of class records is the class's, not morloc's.**
  [read, 2026-11-15] Gap in GEN-11. root-py core.py `morloc_eq` compares
  dicts field by field through itself, so a dict record's fields compare
  as morloc values (a list equals a tuple, numpy fields compare
  elementwise); a dataclass or pydantic record falls to its class's `==`,
  which compares fields with Python `==` and type. Fix in root-py: compare
  class records field by field like dicts (not done: root-py had another
  session's uncommitted edits on master, 2026-11-15).
- **Out-of-range list index gives a wrong value in C++ and R.**
  [reproduced, 2026-10-21] Violates COLL-3 (same result in every language);
  every COLL-4 candidate excludes a silent wrong value. With a pool-computed
  list (`ir xs = (.[5] (incR xs)) + 1.0`): R prints `"nan"` for `[1,2,3]`,
  `"NA"` for a `[Str]`, and `.[-5]` on 3 elements gives the wrong element
  (R drop-index); an `[Int]` fails "Cannot pack NA or NaN into MORLOC_INT".
  C++ `.[5]` on 3 elements returns garbage (`1`); `.[-1]` on `[]` is
  SIGSEGV. Python and the nexus raise "index out of range"; Rust throws.
  Cause: root-cpp core.hpp:73-78 `morloc_at` has no bound check; root-r
  R/core.R:224-234 adds `length(xs)` and indexes `xs[i + 1]`. Repro:
  /work/plans/model-audit-2026-10-21/repro/C/coll/i-r.loc, /work/plans/model-audit-2026-10-21/repro/C/num/get-cpp.loc.
- **The C++ `Deque` overload of `morloc_at` wraps no negative index and
  checks no bound.** [read, 2026-11-14] Latent COLL-3 violation. root-cpp
  core.hpp:591-593 returns `xs[*oi]`, so index -1 reads `xs[-1]`
  (undefined behaviour), where the vector overload (:73-78) wraps
  negatives. No program reaches it today: no stdlib module declares
  `Indexable Deque` (only `Indexable List`, root-cpp main.loc:196-197).
  Fix it with the vector overload, or delete it.
- **`size` of a `Str` counts code points in Python and R, bytes in C++ and
  Rust.** [reproduced, 2026-10-21] Violates BASE-5, ruled 2026-11-08: a
  `Str` is text and its length is in characters (bytes belong in
  `Vector n U8`). C++ `morloc_size` is `x.size()` (root-cpp/core.hpp:331),
  Rust `String::len` (root-rust/core.rs:239-241). Every stdlib `Str` operation
  (index, slice, take, drop, reverse, split positions) needs the same
  audit (COLL-3). `slen s = size s` on U+00E9: 1 in py and r, 2 in
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
  C++ and Rust and absent in Python. Likewise a generic `safeHead :: [a] ->
  ?a` at `[?Int]` on `[null, 2]`: a C++ `isNull` of the result is false,
  Python's true (OPT-7); spec-opt-7-1 leaves the C++ form out for this.
- **R integer rules with no item.** [reproduced, 2026-10-21] The
  2026-10-16 R integer ruling truncates fractional doubles at integer
  slots; no item records it.
- **Number rules with no item.** [reproduced, 2026-10-21] Gaps in
  numbers.md.
  - Real division by zero: `1.0 / 0.0` is `inf` in cpp, r and rust, an
    error "float division by zero" in py (likewise `//`). Candidates: IEEE
    everywhere; failure everywhere; stated per language.
  - Real `%` is floored in all four (`-7.0 % 2.0` is `1`) but in no item.
  - Map pair order: py and r keep insertion order, cpp sorts by key. Is
    order part of a map's value (COLL-5 covers repeated keys only)?
- **R overflow message names the wrong R type for `Int`.** [reproduced,
  2026-10-21] data/lang/r/rmorloc.c:2069-2074 (array) and :2232-2235
  (scalar): a multi-limb `Int` arriving in R fails "does not fit in R's
  numeric type (max 2^53 ...)"; `[[Int]]_R` is R integer, as the
  single-limb branches (:2076-2081, :2225-2227) say.
- **`@schema` and `@typeof` evaluate their argument.** [reproduced,
  2026-10-21] Violates INTR-4. `schemaBoom x = @schema (boom x)` with a
  raising Python `boom` fails "run failed / boom". features-intrinsics.asc:
  449-450 says the argument is not evaluated. Repro: /work/plans/model-audit-2026-10-21/repro/E/intr/main.loc.
- **INTR-5 says a program that is not installed sees `p` unchanged.**
  [reproduced, 2026-10-23] Pools and (since 2026-10-23) the nexus join a
  literal `@datafile` path with the program's source root, installed or
  not, so a built program reads its data file from any working directory.
  The item's wording needs updating to that, or the code to the wording.
- **A newtype with its own C++ form and no C++ packer builds, then fails
  when it crosses.** [reproduced 2026-11-02] `newtype Box = Int`, `type Cpp
  => Box = "BoxT"`, a C++ `mk` and a Python `use`: `./nx go 3` fails "no
  marshaller for a value of this C++ type (schema type 18)"; an export
  returning it from a C++ root fails the same way. Violates REAL-3 and
  NEWT-7; the build should refuse it (briefs/S5-steps3-6.md).
- [read, 2026-11-06] map-r builds a map with `result[[k]] <- v` at any `Map a b`
  (lib/stdlib/map-r/R/map.R `morloc_from_list`, `morloc_insert`). An integer key
  assigns by position, so a `Map Int v` built and used only in R, which never
  reaches a Packable check, would be corrupted (MAP rules, NATIVE). From R's
  semantics, not run: a NULL value deletes its key (`Map Str (?Int)`, OPT-1) and an
  empty-string key is lost in unpack, since `x[[""]]` returns NULL.

- **An integer literal that does not fit its type wraps silently.**
  [reproduced, 2026-11-08, c3a7b3bb3] Violates NUM-5 (ruled 2026-11-08: "A
  literal that does not fit the type it gets is a compile error") and
  NUM-4. `f n = ident [300, 1]` with `ident :: [I8] -> [I8]` from C++ builds
  and prints `[44,1]`: the C++ printer writes `static_cast<int8_t>(300)`.
  Repro: `module main (f)`, `import root`, `import root-cpp`,
  `source Cpp from "h.hpp" ("ident")`, `ident :: [I8] -> [I8]`,
  `f :: Int -> [I8]`, `f n = ident [300, 1]`, with h.hpp defining
  `inline std::vector<int8_t> ident(const std::vector<int8_t>& x){ return x; }`;
  `./nexus f 1` prints `[44,1]`. Likewise `x :: Int; x = 3000000000 + 1`
  under root-cpp builds and prints -1294967295
  (/work/plans/model-audit-2026-10-21/repro/C/lit/intc.loc). No literal
  fit check exists; CppPrinter.hs:73 emits `static_cast<T>(i)`.
