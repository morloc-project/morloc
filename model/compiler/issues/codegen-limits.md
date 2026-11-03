# Limits of generated code

Generated code that a language's own tools refuse.

- [speculative, 2026-11-03] A chain of guards is written as one block only
  where each else arm reaches the lowering as the next conditional, or as a
  let that binds one only to return it (Imperative.hs `guardChain`); behind
  any other wrapper (a coercion to an optional, a serial let, an owned
  copy that changes the arm's expression) it falls back to one nested
  block per guard, which R refuses past about 25 and Python past 100.
  Violates EMIT-5 there. Fix direction: recognize the chain in the IR.
- [reproduced, 2026-11-03] A command the nexus evaluates itself nests its
  manifest expression one level per `&&` operand, and the nexus refuses to
  load a manifest nested past serde_json's recursion limit: `flat a b = a
  && b && ...` with 6000 operands gives a 6002-deep expression and every
  command of the program fails with "Failed to parse manifest JSON:
  recursion limit exceeded". Violates EMIT-5 (block depth bounded by
  source nesting) in the manifest; the nexus evaluator would also recurse
  per operand. A chain of guards the nexus evaluates likely nests the same
  way (not reproduced). Fix direction: an n-ary chain node in the nexus
  expression.
- [speculative, 2026-11-03] A `?R` stored in pointer form inside R
  other than as a bare field may not build where it is read or built
  (CPP-6). Getters, bracket steps and setters of `?T` fields go through
  `mlc::read_field` / `mlc::wrap_field` (golden spec-cpp-6-2); untested:
  `[?R]` and `(Int, ?R)` fields (Cpp.hs gives the pointer form at any
  depth, `read_field` converts only a bare pointer) and a `data` arm `?T`
  payload. Rust has the same position rule (Rust.hs `Option<Box<T>>`);
  root-rust `morloc_require` on such a field would return `Box<R>` and
  clone deeply (read). Analysis:
  /work/plans/eq-ord-methods/briefs/da-cpp-recursive-optional.md.
- [read, 2026-11-03] root-cpp `template<class A> bool morloc_isNull(A)`
  and `morloc_eq(const A&, std::nullopt_t)` (core.hpp) are catch-alls that
  answer "not null" for any nullable C++ form other than `std::optional`
  (a `shared_ptr`, a user-mapped optional), with no build error, so
  `isNull` on such a value is wrong at run time. They should be limited to
  types that cannot be null.
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
  with `std::vector<int>`. [traced 2026-11-02, briefs/A5-B4.md] Recursion
  is not the cause: exporting a plain `mkC`/`lenC` fails the same way, and
  Rust too ("... (Counted, Chain) and (i64, Chain)"). The variant
  collectors read the wire type (`serialAstToType`) under the native
  type's name, and `wireField` converts a field between its two forms only
  for closures. Fix: the collectors use the native type; `wireField` also
  converts a field that contains a pack.
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
- [read, 2026-10-21] The C++ enum collector
  (CodeGenerator/Pools/CAbi/Members/Cpp.hs `collectCppEnums`) does
  not descend into a `data` type's parameters, where the Rust one does. With
  `data Tag a = Tag Int` used as `Tag Color`, Color an argument-free `data`
  reached nowhere else, the C++ pool renders `Tag<Color>` without declaring
  Color. [reproduced 2026-11-02] Only when the `data` is user-mapped to a
  template (`data Cpp => (Tag a) = "MyTag<$1>" a`); unmapped it renders as
  `Tag_<hash>`. A record's phantom parameter is missed the same way, in
  C++ and in Rust (`collectRustEnums`): `record Box a` mapped to
  `"MyBox<$1>"`, used as `Box Color`, fails "Color not declared" / "cannot
  find type `Color`". Root fix: one shared child walk for the six `seek`
  collectors in Cpp.hs and Rust.hs (briefs/A5-B4.md).
- **Python gives every named function a per-call `try`/`except` that
  re-raises as `RuntimeError`.** [reproduced in generated code,
  2026-10-21] Violates EMIT-3. `fib`'s native recursive entry `__mm_1780`
  wraps its body in `try: ... except Exception as e: raise
  RuntimeError(f"{e!s}\n  at fib [py] ...")`, so the exception type is lost
  and a failure at depth d builds O(d^2) message characters. Source:
  data/lang/py/lang.yaml `ldErrorWrapOpen`/`ldErrorWrapClose`, applied by
  Grammars/Translator/Generic.hs `lcMakeFunction` `wrapError`.
  spec-emit-3-1 covers only C++ and Rust. Repro: /work/plans/model-audit-2026-10-21/repro/H/rec/.
  [reproduced 2026-11-02, wrong result] A Python `except ValueError` in user
  code around a morloc callback never fires: the per-function `try`
  re-raises everything as RuntimeError. Fix: R's `ldFrameLabelStmt`
  mechanism, the trace built from `e.__traceback__` at dispatch and in
  `mlc_try` (briefs/B7-G4.md, prototype gives identical trace text).
- **A record field named by a C++ or Rust keyword breaks the native build.**
  [reproduced 2026-11-02] A C++ "struct" record with a field `new` or
  `this` builds a pool g++ rejects ("expected unqualified-id before
  'new'"); a Rust `self`/`super`/`crate`/`Self` field reaches a raw
  `error`. Field names are neither escaped nor refused: one per-language
  field-name rule in lang.yaml, checked at the record declaration
  (briefs/B8-B9.md).
- **A native-form template with a `$` not followed by a digit crashes the
  compiler.** [read 2026-11-02] e.g. Rust `$crate`: Grammars/Macro.hs
  `error (show err')`. A check when the template is declared covers it with
  the `$0` and missing-argument cases.
