# Limits of generated code

Generated code that a language's own tools refuse.

- [reproduced] R's parser refuses nesting past about 50 levels
  (`contextstack overflow`): 50 nested `if`/`else` blocks fail, where the
  same logic written as `if .. else if ..` parses at any length. morloc
  writes a guard chain (`? c1 = .. : ? c2 = .. : ..`) as nested `if`/`else`
  blocks, so a chain of about 50 cases fails to load in an R pool. Fix:
  write it as `else if` where a case's condition needs no statements of
  its own. Spec gap: no item bounds the nesting of generated code
  (EMIT-4 covers `&&`/`||` chains only).
- [reproduced, 2026-10-17, 9f24abe4] A C++ conditional whose result type
  has no default constructor does not compile: the lowered `? :` declares
  `T helperN;` before assigning an arm (Cpp.hs lcMakeIf). With
  `struct Tok { int v; explicit Tok(int x) : v(x) {} };` mapped by
  `newtype Tok = Int`, `type Cpp => Tok = "Tok"` and a Packable instance,
  `pickT n ? n > 0 = mkT 1 : mkT 2` fails with "no matching function for
  call to 'Tok::Tok()'". Tail loops hold their result in `std::optional<T>`
  for this reason; a conditional does not. Spec gap: no item says what a
  native type must provide (here, a default constructor).
- [reproduced, 2026-10-17, 9f24abe4] A tail-recursive C++ function that
  builds a `data` value whose field is a Packable-mapped newtype does not
  build. With `newtype Counted = Int`, `type Cpp => Counted = "Counted"`, a
  Packable instance, and `data Chain = End | Link Counted Chain`,
  `build k acc ? k == 0 = acc : build (k - 1) (Link (one k) acc)` fails with
  "C++ pool: the data type Chain is used with two field types for its
  constructor Link: (Counted, Chain) and (int, Chain)"
  (Grammars/Common.hs mergeVariantOccurrences). A non-recursive
  `wrap k acc = Link (one k) acc` builds. `[Counted]` fails the same way
  with `std::vector<int>`. Cause not traced; the recursive manifold's
  serial entry seems to see the packed form.
- [reproduced, 2026-10-17, 9f24abe4] A function called from several sites
  is emitted once per site: a Rust pool held 19 identical loop bodies for
  `count`, a C++ pool about 9 for `build`. Code size and build time only.
  Spec gap: no item says when two specializations are one function.
- [speculative, 2026-10-18] Two hazards in the C++ tail-loop walk, read
  but not reproduced:
  - Its temporaries and copied lets are declared `auto`, so an element
    of a `std::vector<bool>` would be a proxy into the vector, which a
    later reassignment of a carried slot could free before the proxy is
    read. A `match` on a carried list first copies it into a local of the
    iteration (`auto n5 = n12`), so the swapping loop `go k [a, b] acc =
    go (k - 1) [b, a] a` reads correctly under ASan.
  - A loop let bound to a void C++ call would become `auto n = f();`. A
    unit-valued let in a loop body lowers through the ordinary let path
    (`f(n); mlc::Unit n17{};`) in the shape tried.
- **A compiler-built call of a shared source is a compiler bug.**
  [reproduced, 2026-10-18] `sourceSignature`
  (library/Morloc/CodeGenerator/EffectBoundary.hs) needs the signature of
  the term a call names; a call the compiler builds (a literal's `pack`)
  carries no term, so it falls back to the host function's signatures and
  throws when there are several. root-py's `morloc_idpy` is `idpy`, Deque's
  `pack` and `unpack`, and several `into`s, so building a Python `Deque`
  literal through its `pack` failed "a call of 'morloc_idpy' names no term,
  and the host function has more than one signature". No path builds that
  call today (a Deque's Python form is its representation's, so no pack is
  inserted); a pack whose source is shared and whose form differs would.
  Candidate: give a compiler-built call the instance method it implements.
- [reproduced, 2026-10-20, d352b5d4] A Rust manifold whose body is a
  value it received serialized does not build: it returns its
  `*const u8` argument as its `*mut u8` result (`return s1;`, E0308 "types
  differ in mutability"). It arises when a Python caller hands a
  Rust-made value to a Rust `do` branch that only yields it. With
  `record R where on :: Bool; n :: Int` (Rust "RR", Py "dict"),
  `noR = mkR 0` from Rust and `pyR :: Str -> <IO> R` from Python,
  `riboOf path = do ? path == "" = do noR : do pyR path` fails to build.
  biolib rnaseq (`riboOf` in lib/prepare.loc, the same shape) built with
  the compiler at ee516029 on 2026-10-19 and fails now, so this is
  probably a regression in ee516029..d352b5d4. A passthrough result needs
  a copy into a reply packet (or the caller should keep its own packet).
- [read, 2026-10-21] The C++ enum collector (Cpp.hs `collectCppEnums`) does
  not descend into a `data` type's parameters, where the Rust one does. With
  `data Tag a = Tag Int` used as `Tag Color`, Color an argument-free `data`
  reached nowhere else, the C++ pool renders `Tag<Color>` without declaring
  Color. Not reproduced.
- **Python and C++ write a `&&`/`||` chain as nested conditionals; a
  Python chain of ~100 operands fails to load.** [reproduced, 2026-10-21]
  Violates EMIT-4 and EMIT-1. `noneOf x = x != 0 && x != 1 && ... && x !=
  119` (root-py) builds; `noneOf 500` fails `IndentationError: too many
  levels of indentation` and the pool dies. Python and C++ nest one `if`
  per operand; R and Rust write the chain flat with each operand a
  function. Cause [read]: Translator/Imperative.hs `shortCircuit` writes
  the operator only when the second operand has no prior statements; in
  Python and C++ `x != k` (root `(!=) x y = not (x == y)`) arrives inlined
  with statements rather than as the operand manifold Serialize.hs keeps.
  Why only these two languages was not traced. spec-emit-4-1 misses it:
  its `x > -k`/`x == k` operands do not trigger it. Repro: /work/plans/model-audit-2026-10-21/repro/H/chain/.
- **Python gives every named function a per-call `try`/`except` that
  re-raises as `RuntimeError`.** [reproduced in generated code,
  2026-10-21] Violates EMIT-3. `fib`'s native recursive entry `__mm_1780`
  wraps its body in `try: ... except Exception as e: raise
  RuntimeError(f"{e!s}\n  at fib [py] ...")`, so the exception type is lost
  and a failure at depth d builds O(d^2) message characters. Source:
  data/lang/py/lang.yaml `ldErrorWrapOpen`/`ldErrorWrapClose`, applied by
  Grammars/Translator/Generic.hs `lcMakeFunction` `wrapError`.
  spec-emit-3-1 covers only C++ and Rust. Repro: /work/plans/model-audit-2026-10-21/repro/H/rec/.
- **A sourced function whose signature ends in a function-type alias does
  not build when partially applied.** [reproduced, 2026-10-22] Gap: no
  item says an alias in a sourced signature generates the same code as its
  expansion (ALIAS items cover typing, not codegen). With `type Reader a =
  Str -> Sink a -> <IO> ()`, `feed :: Int -> Reader Int` sourced from C++
  and used as `run (feed 1 p)` fails `conversion from 'void' to
  non-scalar type 'std::function<mlc::Unit()>'`: the call is emitted as
  returning a suspension. Written flat (`feed :: Int -> Str -> Sink Int ->
  <IO> ()`) it builds and runs. Rust fails the same way (`expected
  function, found ()`, and the sink passed as `&Arc<dyn Fn>` to an `S: Fn`
  parameter). With no leading argument (`feed :: Reader Int`) both forms
  build. Repro: /work/plans/biolib-convert-repro/alias-signature/
  (alias2.loc fails, flat2.loc builds).
- **`Null` in a field of a parameterized record literal is not given a
  type.** [reproduced, 2026-10-22] Violates OPT-1 (`Null` has type `?T`).
  With `record Outer t c` holding `inner :: ?(Inner c)`, the top-level
  value `xs :: [Outer Str Str]; xs = [{ann = "a", inner = {...}}, {ann =
  "b", inner = Null}]` fails in Express.hs "Cannot infer type for Null
  literal of type ?(Inner Str)". An annotation `(Null :: ?(Inner Str))`
  fails the same way. Repro: /work/plans/biolib-convert-repro/null-in-record/.
