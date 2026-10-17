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
