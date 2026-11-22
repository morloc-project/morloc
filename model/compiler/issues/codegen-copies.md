# Copies in generated C++ and Rust

Where an owning backend copies a value that no later read needs. All of
these are cost, not correctness, against the items of
model/compiler/codegen/ownership.md, which bind generated Rust and C++. Python and R share values by reference and copy only at the
wire. Reproduced 2026-10-17 at 9f24abe4 by counting copies (a counting Rust
`Clone`, C++ allocations of a 64 KiB payload); entries already in
codegen-ownership.md are not repeated.

Generated `data` arms are shared nodes in both languages (a generated Rust
arm type over `Arc`, C++ `shared_ptr`), so a copy of a `data` value is O(1); lists, strings, tuples,
records and user-mapped types copy deeply.

- **A part read through an inlined match on a `data` value is copied.**
  [reproduced 2026-11-22, golden own7-matrix] After the in-place rewrites
  (OWN-6: a let of a conditional is read in place; OWN-2: reads run before
  keeps), the inlined cases still copying are `boxPay` (a match on `Bx`
  returning a field) in a loop or kept twice (c640, c641: 3 and 1), and
  caller aliasAfter with kinds L and D where the value is also kept by a
  record, a list or a wrapper call (c454, c475, c496, c558, c580, c601,
  c622: 1 each). The rewrite that would bind the part in place stops at
  its size bound (48 nodes, LambdaEval `bindLet`) or does not see a
  constructor-field arm as a part of the matched value.
- **A recursive function returns a copy of what it was lent (OWN-8).**
  [reproduced 2026-11-22, golden own7-matrix rec cases] A callee that is
  not inlined and returns its argument, or a part of it, on some path
  copies it in its by-reference form when the caller still needs the
  argument (46 copies in each language), and one that builds an owned
  record or list holding it copies it too (80). Removing the first needs
  OWN-8 (a result that is a reference into the argument; a result slot
  passed in is the narrow form); removing the second for a caller that
  only projects the result needs a worker that returns the fields.
- **A let no spine reaches is never moved.** [read, 2026-11-09; G1 not
  reproduced] Gap in OWN-2. Marking finds owned lets only along a spine
  of let forms starting at a manifold, loop or do-block body (Imperative.hs
  `markSpineN`/`markSpineS`). Nested lets in arguments, containers and
  lambdas do not reach marking (`invertSerialManifold` floats them out to
  weave sites; lambdas are manifolds), so the gaps are weave sites and
  let forms the spine grammar lacks:
  G1. `markSpineN` has no `SerialLetN` case, though weave emits one per
  serial dependency of a native body (Common.hs ~390, Serialize.hs ~1509-
  1523): everything below it is one step and its native lets own nothing.
  Three C++ shapes with a Python call (`length [mkL (pyf n)]`, a call
  beside a list literal, either order) still moved: those calls lower
  to a helper manifold, not a `SerialLetN`; the shape that produces one
  is not yet found.
  G3. A loop's fallback-woven base and continue values (`sealLoop`
  ~369-380) and a `LoopSLet` right-hand side get one step each; no copy
  is measured from them. (A conditional's test and a loop's guard are
  spines of their own.)
  Fix shape: every weave site is a spine root and every let form weave
  emits is a spine node (not a fallback spine for unreached lets, which
  would hide grammar gaps; assert in a test that every let is visited).
  Cost: marking walks each body twice per segment, once to mark and once
  in `lastBuilds` to collect builds, which returns the same captures; one
  pass returning both would do. `heldReads` is recomputed per nested
  root and `stepReads` walks the owned list per step (quadratic in a
  spine's lets); not measured against g++/rustc time.
- **root-cpp takes optionals by value.** [reproduced 2026-11-21] Gap in
  stdlib root-cpp (not the compiler): `morloc_isNull` and `morloc_require`
  take `std::optional<A>` by value and `require` returns `x.value()`, so
  `pOptIn x ? isNull x = 0 : keyp (require x)` makes 3 copies of the
  payload where 0 are needed (golden cpp-copy-shapes entryOpt). Fix, in
  root-cpp core.hpp (on master, so left to its maintainer):
  `bool morloc_isNull(const std::optional<A>&)`, `bool morloc_isNull(const
  A&)`, `const A& morloc_require(const std::optional<A>& x)` returning
  `x.value()`, and `A morloc_require(std::optional<A>&& x)` returning
  `std::move(x).value()`, so a temporary is moved rather than lent.
- **A gather sink copies its batch into the suspension it returns.**
  [reproduced, stage 6 prototype] A C++ closure takes its parameters by
  reference (OWN-7), so the sink of a whole-list gather copies the batch
  into the suspension it builds, and the suspension, which may run again
  (OWN-3), copies it once more into the gather's list (golden
  cpp-copy-count). Taking a kept parameter by value in a closure, or a
  capture held in shared storage, would remove one or both.
- **Some captures copy a dead owned value.** [reproduced] A C++ closure that
  may cross a pool (a `MorlocClosure`, pap or staged form) copies its
  captures, since its reify thunk or stage call names them again, and a
  `[=]` do-block thunk copies. A setter on a loop-carried record copies
  every unchanged field each iteration, as the carried local is not among
  the lets a setter may take (`setLoop (.(.k = j) r) (j - 1)` emits
  `{n4.xs, n5}`: 100 copies over 100 steps, 2026-10-19).
- **Closure and wire paths copy what they only serialize.** [read] Rust:
  a staged closure that can cross copies each argument before its stage
  point twice per application, into the level's shared holder and into
  the `pap` that keeps its origin (`stagedDoc`, Rust.hs), where a `pap`
  over the holder would copy once; a reflected closure's origin,
  captured packet bytes included, is cloned twice when reflected and once
  per application; a closure's non-closure slots are cloned into the wire
  tuple; a variant arm needing wire conversion clones each field, though on
  the deserialize side the raw value is dead.
- **`map unpack` over a newtype with no native form copies every element.**
  [reproduced] The cast is erased in native code (NEWT-7), but the `map`
  around it is not: Rust renders `morloc_map_vec` with an identity
  function, cloning each element (copy-census-rust mapUnpack: 600 links
  for 3 chains of 100 and two maps). Writing `map cast xs` as `xs` assumes
  `map` keeps the identity, a law of the sourced `Functor` instance the
  compiler is not told; it needs a ruling. biolib stream's `unpackReader` (`mapReader
  (map unpack)`) pays it on every batch: ingesting a 100 Mb FASTA through
  bio's `readFasta` takes ~0.70 s and 271 MiB against ~0.67 s and 254 MiB
  with the tagged reader directly; the stream files are identical. A map
  whose function is a representation cast could be the identity on the
  list.
- **Under `--debug` a base case calls its own pool through the socket.**
  [reproduced by review, 2026-10-24] Building golden spec-cost-12-1 with
  `morloc make --debug`, the C++ base case of `loop` (`do b`) becomes a
  closure that is reified to a packet, decoded, and invoked with
  `foreign_call_v` into the C++ pool itself: one self round trip and two
  encode/decode passes per base case. The output is correct; the cost is
  not. The non-debug build calls nothing.

- **A read of a recursive `?R` field copies the node.** [read, 2026-11-03]
  CPP-6 stores the field as `std::shared_ptr<const R>` and `mlc::read_field`
  (cppmorloc.hpp) turns it into the `std::optional<R>` a `?R` value is, by
  copying `*p`: the node's other fields are copied, its own cycle field is
  shared. `isNull (.tail x)` then `require (.tail x)` copies the node twice
  per step of a walk; a node holding a large vector copies the vector each
  time. root-cpp `morloc_isNull` and `morloc_require` take their argument by
  value and copy it again. A read that only tests for null needs no copy,
  and a read from an owned receiver could move (`mlc::take_field`, OWN-5).

## A mapped Rust enum's optional field is always read as a copy [read]

RUST-4 lets a `?T` field of a user-written enum hold `Option<Box<T>>` or
`Option<Arc<T>>`, which has no `Option<T>` in memory to lend, and the
compiler cannot see which form the user wrote; Rust.hs rustCaps therefore
never lends such a field (OWN-6) and reads it as a copy, deep behind a Box.
A field held as plain `Option<T>` could be lent but is copied too.
Candidate: a trait the generated code asks for the field's form at build
time (an associated const) so `Option<T>` fields lend.
