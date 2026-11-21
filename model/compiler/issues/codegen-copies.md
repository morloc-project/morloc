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

- **A projection is copied when its consumer only reads it.** [reproduced]
  Rust: a field of a borrowed tuple or record bound by a let (`n16.0.clone()`,
  1000 clones for a 1000-link chain), a list pattern element
  of a list the frame owns, where an owned sink needs it, and a field of an
  element (`[Link _ rest]` clones `rest`, since a list element is not a
  borrowable projection root), a tag test on a nested field, a conditional
  whose arms are fields (`count 0 (match r | Good c = c ...)`), and a
  non-tail recursion that rebuilds a chain (`inc`, quadratic). C++: lets borrow, but a
  tail loop's lets and a list element do not (`auto n = ...`), so a nested
  pattern in a loop copies each bound field per step. A Rust let bound to
  a part of a part of a value the frame owns is read in place (OWN-2,
  OWN-6; spec-own-6-18, spec-own-6-19). The cases of goldens
  own7-matrix-cpp and own7-matrix-rust still above OWN-7's prediction
  (column 7 of cases.tsv) are kinds L and D with caller aliasAfter, one
  copy each: the part is taken through a function that returns a value
  (`lhead`, `boxPay`), which is OWN-8, 2026-11-21.
- **A move into a call is decided per callee, not per call site (OWN-2,
  OWN-7).** [read 2026-11-21, Imperative.hs lastBuilds] A call may move a
  value only when every call of that manifold is the value's last read, so
  a manifold called at two sites copies at both even where one could move.
  It feeds cppMovesInto and rustBuildOwnership. Not yet reproduced.
- **A parameter read under its caller's let index is not found by-value
  (OWN-7).** [read 2026-11-21, Imperative.hs consumedParams] Only BndVarN
  reads in loop starts and built values count, so a parameter the callee
  reads as a LetVarN gets two forms instead of one by-value form, and the
  mode stops carrying up the chain. Not yet reproduced.
- **A C++ arm whose fields are kept beside a nested pattern copies them
  (OWN-5).** [reproduced 2026-11-21] With `data T3 = T3 Pay Pay Bx`,
  `match t | (T3 a b (Bx q k)) = sum (map keyp (pair a b)) + keyp q + k`
  on a fresh value copies `a` and `b` (2 copies) where `firstTwo`, with a
  plain third field, takes both (0).
- **A C++ function value copies each argument it is called with.**
  [reproduced 2026-11-21, golden own7-matrix-cpp R ret nested pap] A
  partial application bound to a let is a `std::function<R(T)>`, whose
  parameter is by value, so calling it on an lvalue copies the argument
  even when the function keeps nothing (2 copies at j=1, 1 at j=0; the same
  partial application passed inline is a binder that forwards a reference,
  and copies nothing). The copy is in the C++ type of a function value,
  which user headers receive, not in OWN-7's choice of form; declaring
  such a let `auto` breaks reified closures, nested binders and returned
  function values (13 goldens).
- **A `match` arm that builds a constant makes a walking loop own its
  value.** [reproduced] `tl c = match c | End = End | (Link _ rest) = rest`
  stepped in a Rust tail loop copies the rest of the chain each step
  (golden copy-census-rust tlLoopRead: 294 clones for 3 steps over 100
  links), because the `End` arm is a value the loop builds (OWN-6). The
  same loop with `End = c` copies nothing (spec-own-6-1 walkTail); the
  constant was borrowed only where an earlier pass hoisted it into a
  parameter.
- **A last read moves only at some keeping positions.** [reproduced]
  Outside them a value read twice is copied at every keeping use including
  the last (`prepend 1 c` then `prepend 2 c`: 2 copies, 1 needed), a field
  taken from a value the frame owns needs the value used once and built by
  a constructor or `@try` (a sourced call's result is never taken), and a
  value matched twice is copied. C++ moves an owned local at its last read
  into a constructor, tuple, record, optional, list literal, loop continue,
  local closure capture, by-value manifold argument or source argument
  named once in its call, or a conditional's arm (OWN-7 two forms,
  2026-11-21).
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
  G2. A conditional's test is woven (Common.hs ~879) but marked as one
  step, so lets inside it own nothing.
  G3. A loop's guard, its fallback-woven base and continue values
  (`sealLoop` ~369-380) and a `LoopSLet` right-hand side get one step
  each. Moving a guard-local let needs a ruling: OWN-4 says a read in a
  guard borrows.
  Fix shape: every weave site is a spine root and every let form weave
  emits is a spine node (not a fallback spine for unreached lets, which
  would hide grammar gaps; assert in a test that every let is visited).
  Cost: marking walks each body twice per segment, once to mark and once
  in `lastBuilds` to collect builds, which returns the same captures; one
  pass returning both would do. `heldReads` is recomputed per nested
  root and `stepReads` walks the owned list per step (quadratic in a
  spine's lets); not measured against g++/rustc time.
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
- **A pap with no stage clones each argument it holds per level.** [read]
  Cost. The per-level `__xj.clone()` of a partial application with no
  stage feeds only `pap(&...)` and could borrow.
- **C++ closure forms copy each capture several times.** [read; not
  reproduced 2026-10-19] Per
  capture not moved: a `MorlocClosure` 2 (binder and reify thunk), a pap 6
  (data/lang/cpp/pool.cpp `mlc_pap` ~1581-1600 copies `fn` and `reify`
  into the closure, then into `apply1`), a staged form 8; each pap step
  copies every capture and the applied argument 6-8 times (named by-value
  parameters). Reify reads through `const&` and needs no copy.
  copy-census-cpp does not reach these forms: they are built only for
  closures that cross (`crossingClosures`, Common.hs).
  Measured at the floor in every shape tried (spec-own-3-4's census and a
  probe: a crossing lambda and pap with one and two captures, each dead or
  read again): one large allocation per capture that crosses, none locally;
  the remaining claim is pap steps applied one argument at a time.
- **Closure and wire paths copy what they only serialize.** [read] Rust:
  staged closures re-clone every earlier argument at each level (about n^2
  argument clones for an n-ary function), including at the level that
  calls the stage, which only borrows them (`stagedDoc`, Rust.hs); a reflected closure's origin,
  captured packet bytes included, is cloned twice when reflected and once
  per application; a closure's non-closure slots are cloned into the wire
  tuple; a variant arm needing wire conversion clones each field, though on
  the deserialize side the raw value is dead. C++: `IIfNotNull` copies the
  optional's value.
- **A C++ optional mapped through a function is copied twice.** [read]
  `MapOptionalN` and the optional-map intrinsic render `auto src = ...;
  T u = src.value();` (Pools/CAbi/Members/CppPrinter.hs `IIfNotNull`, ~260-280), copying the optional and
  then its value; `const auto&` would copy neither.
- **`map unpack` over a newtype with no native form copies every element.**
  [reproduced] The cast is erased in native code (NEWT-7), but the `map`
  around it is not: Rust renders `morloc_map_vec` with an identity
  function, cloning each element. biolib stream's `unpackReader` (`mapReader
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
