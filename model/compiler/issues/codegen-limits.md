# Limits of generated code

Generated code that a language's own tools refuse.

- [read, 2026-11-03] Lowering a chain re-renders its open run at every
  level: Imperative.hs `currentChain` and `keepChain` compare rendered text
  to tell whether docs still hold the chain, and `renderChain` re-folds the
  head run, so lowering costs O(n x run cap) renders. Linear in n: with
  root-py, 2000 `x >= i` operands take 1.2 s past typechecking as a chain
  and 0.5 s as a list of the same comparisons (measured 2026-11-03). A
  cheaper check must still see any rewrite of the docs' expression, or a
  rewritten chain would be reused; a version tag set by the code that
  builds a chain is not enough, since record updates elsewhere keep it.
- [speculative, 2026-11-03] A chain of guards, or of `&&`/`||`, is
  written as one block (pools: Imperative.hs `guardChain`, `shortCircuit`)
  or one manifest node (nexus: Nexus.hs `guardArms`, `shortChain`) only
  where each next element is reached through conditionals and lets. Behind
  any other node the chain would nest one level per element again: in a
  pool, a coercion or an owned copy that changes an arm's expression; in
  the nexus, a do block or a forced suspension, which it treats as the
  expression inside,
  which R refuses past about 25 levels, Python past 100 and the nexus past
  128. Not reproduced, 200 to 300 elements in R, Python and the nexus:
  optional results with a `Null` arm, a let before every element,
  effectful arms, tests that call another language. Would violate EMIT-5.
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
- **A C++ schema hint keeps a form's unfilled template.** [read
  2026-11-04, speculative] For a templated form the pool's schema strings
  hold `<std::tuple<$1,$2>>` rather than the filled native type
  (Serial.hs near `evaluatedType`); harmless only if nothing reads the
  hint's text.
- **A record field named by a C++ or Rust keyword breaks the native build.**
  [reproduced 2026-11-02] A C++ "struct" record with a field `new` or
  `this` builds a pool g++ rejects ("expected unqualified-id before
  'new'"); a Rust `self`/`super`/`crate`/`Self` field reaches a raw
  `error`. Field names are neither escaped nor refused: one per-language
  field-name rule in lang.yaml, checked at the record declaration
  (briefs/B8-B9.md).
- [read, 2026-11-15] The generated Rust arm aliases of a pool-private
  `data` type (`<name>_<Ctor>`, declared at the crate root) are not checked
  against the crate root's other names, as public types' are (Rust.hs
  planRustPublic collision check); a clash with another generated type or a
  sourced file's item reaches rustc as E0428 instead of a morloc error.
  Likewise the crate root's `mlc` module, declared when the pool generates
  arms or handles, clashes with a sourced `mod mlc`.
- [reproduced, 2026-11-21] A Rust pool does not build when a user `data`
  type is named like a type in Rust's prelude. With `data Box = Box Pay Int
  | NoBox` and a record with a field of type `Box`, the generated record's
  derives resolve `Box` to `std::boxed::Box` ("`Box` doesn't implement
  `Debug`", "the trait bound `Box: Default` is not satisfied"); renaming
  the type to `Bx` builds. Generated type names need qualifying, or a
  prelude name refusing with a message.
- [reproduced, 2026-11-21] A Rust pool for a `data` type whose arm holds
  a user-mapped newtype (`data Bx = Bx Pay Int | NoBox`, `newtype Pay =
  Int`, `type Rust => Pay = "Pay"`, a Packable instance) may not compile,
  depending on which functions are exported: golden own7-matrix-rust's
  main.loc with its export list cut to
  `(c000, c288, c056, c348)` fails rustc E0277 "`Pay: ToVoidstar` is not
  satisfied" in the generated `Bx` impls (the whole list builds). Same
  result before and after the 2026-11-21 Rust marking change.
