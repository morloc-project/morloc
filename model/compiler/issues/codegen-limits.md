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

