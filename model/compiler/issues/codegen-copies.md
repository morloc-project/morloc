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

- **A function returns a copy of what it was lent where OWN-8 does not
  apply.** [read, 2026-11-22] OWN-8 removes the copies of a callee whose
  body is a tail loop or a conditional returning lent parameters or parts
  of them (every own7-matrix rec ret/part case). Left, unmeasured: a
  non-tail recursion, and a call passing another callee's lent result on.
  A callee that is not inlined and builds an owned record or list holding
  its argument copies it too (own7-matrix 80); removing that for a caller
  that only projects the result needs a worker that returns the fields.
- **A conditional with a take in it is copied.** [read, 2026-11-22] OWN-10
  does not hold a let of a conditional in place when a let in it takes a
  part (a part a by-value form moves out, OWN-5), as takes are decided per
  let, not per form.
- **A sourced reader taking its parameter by value copies an owned OWN-8
  result.** [read, 2026-11-22] A let that reads a lent-or-owned result in
  place is a reference, so a sourced function whose parameter is `T` (C++
  by value, Rust `T`) is given a copy even when the result was owned and
  could have moved. No golden measures it (own7-matrix reads with `keyp`,
  a reference).
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
- **A C++ optional read with `require` at its last read is copied.**
  [reproduced 2026-11-22, golden cpp-copy-shapes entryOpt: 1] `keyp
  (require x)` on an owned optional at its last read binds
  `Pay n = morloc_require(std::as_const(x))`: the optional is lent, so
  the value it lends is copied into the owned let. Moving the optional at
  its last read (root-cpp's `require` takes a temporary by value and moves
  its content out) would copy nothing.
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
  time. A read that only tests for null needs no copy, and a read from an
  owned receiver could move (`mlc::take_field`, OWN-5).
- **A lent-or-owned result passed on to another call is copied.**
  [reproduced 2026-11-22, goldens own7-fuzz-cpp p090, own7-fuzz-rust
  p058: 1 each] Caller `twice` over a chain `g2 (g1 (g0 x))`: the let bound
  to one OWN-8 call's result is passed to another OWN-8 call (p058, `g2 =
  g1` with `g1` a tail loop), or is also an arm of an inlined conditional
  (p090, `g2 j x = ? j > 1 = g1 (j - 1) x : x`), so it is not only read and
  takes the result owned, copying a lent one (OWN-8 open).

## A mapped Rust enum's optional field is always read as a copy [read]

RUST-4 lets a `?T` field of a user-written enum hold `Option<Box<T>>` or
`Option<Arc<T>>`, which has no `Option<T>` in memory to lend, and the
compiler cannot see which form the user wrote; Rust.hs rustCaps therefore
never lends such a field (OWN-6) and reads it as a copy, deep behind a Box.
A field held as plain `Option<T>` could be lent but is copied too.
Candidate: a trait the generated code asks for the field's form at build
time (an associated const) so `Option<T>` fields lend.
- **A shared handle is copied where a later read could take it.**
  [reproduced 2026-11-22, golden spec-own-10-2 twoMoves, C++] `holdD b`
  is given a copy of `b`'s `data` handle (a reference count increment,
  not a payload copy) rather than a move, as LambdaEval moves a let only
  one read ahead (`readFirst`).
