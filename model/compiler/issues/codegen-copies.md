# Copies in generated C++ and Rust

Where an owning backend copies a value that no later read needs. All of
these are cost, not correctness, and gaps in model/compiler/codegen/ownership.md:
OWN-1..7 cover Rust tail loops and by-value parameters only, and C++ is held
to none of them. Python and R share values by reference and copy only at the
wire. Reproduced 2026-10-17 at 9f24abe4 by counting copies (a counting Rust
`Clone`, C++ allocations of a 64 KiB payload); entries already in
codegen-ownership.md are not repeated.

Generated `data` arms are shared boxes in both languages (Rust `RecBox`, C++
`shared_ptr`), so a copy of a `data` value is O(1); lists, strings, tuples,
records and user-mapped types copy deeply.

- **A projection is copied when its consumer only reads it.** [reproduced]
  Rust: a field of a borrowed tuple or record bound by a let (`n16.0.clone()`,
  1000 clones for a 1000-link chain), a list pattern element
  of a list the frame owns, where an owned sink needs it, and a field of an
  element (`[Link _ rest]` clones `rest`, since a list element is not a
  borrowable projection root), a tag test on a nested field, a conditional
  whose arms are fields (`count 0 (match r | Good c = c ...)`), and a
  non-tail recursion that rebuilds a chain (`inc`, quadratic). Rust binds
  a let to a reference only when its root is borrowed, so a getter on a
  record the function built and reads twice copies the field
  (`let h = {c = mkChain n, k = 3} in .k h + chainLen (.c h)`: 100 clones;
  golden spec-own-6-14 takenRec). C++: lets borrow, but a
  tail loop's lets and a list element do not (`auto n = ...`), so a nested
  pattern in a loop copies each bound field per step.
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
  local closure capture or by-value manifold argument; conditional arms
  and by-value source arguments still copy one (golden copy-census-cpp
  letSource and matchCond: 1 copy each; re-checked 2026-10-19 at
  25d09170).
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
  Rust first needs parity with C++: it marks with no aliases or held set
  (Rust.hs ~1430), so wider marking could move a value a reference into
  it still reads (rustc E0505); and it marks without by-value parameter
  seeds while finding builds with them (~1460), so a by-value parameter
  is never moved at its last read (OWN-7; read), and its seeds do not
  drop loop slots as C++ does (Cpp.hs ~2142).
  Cost: marking walks each body twice per segment, once to mark and once
  in `lastBuilds` to collect builds, which returns the same captures; one
  pass returning both would do. `heldReads` is recomputed per nested
  root and `stepReads` walks the owned list per step (quadratic in a
  spine's lets); not measured against g++/rustc time.
- **A C++ loop copies a carried value no step changes.** [reproduced] A
  loop slot every continue passes on unchanged is a borrow in Rust (OWN-6)
  but a copy of its start in C++ (golden copy-census-cpp throughLoop,
  loopCarried). Binding it by const reference was tried and dropped: it
  blocks the move of the slot into the loop's result. EVAL-7 (ruled
  2026-10-18) forbids a source taking `T&`, which was the other obstacle.
- **Some captures copy a dead owned value.** [reproduced] A C++ closure that
  may cross a pool (a `MorlocClosure`, pap or staged form) copies its
  captures, since its reify thunk or stage call names them again, and a
  `[=]` do-block thunk copies. A setter on a loop-carried record copies
  every unchanged field each iteration, as the carried local is not among
  the lets a setter may take (`setLoop (.(.k = j) r) (j - 1)` emits
  `{n4.xs, n5}`: 100 copies over 100 steps, 2026-10-19).
- **A Rust staged function passed as a value also builds an unused closure.**
  [reproduced 2026-10-19] Cost. `capStaged` in copy-census-rust (`f x =
  let z = chainLen c + x in \y -> z + y`, `f` passed to a function that
  applies it) emits `let n14 = { let __caps = (n9.clone(),); local1(...) }`
  that nothing reads, besides the staged value: one full clone of the
  captured chain (100 of the shape's 100). The per-level `__xj.clone()` of
  a pap with no stage feeds only `pap(&...)` and could borrow.
- **C++ closure forms copy each capture several times.** [read; not
  reproduced 2026-10-19] Per
  capture not moved: a `MorlocClosure` 2 (binder and reify thunk), a pap 6
  (data/lang/cpp/pool.cpp `mlc_pap` ~1397-1440 copies `fn` and `reify`
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
  the deserialize side the raw value is dead. C++: `IMapList` binds
  `auto x = coll[i]` and `IIfNotNull` copies the optional's value.
- **A comment states a convention OWN-7 broke.** [read] `borrowableRoot`
  (library/Morloc/CodeGenerator/Grammars/Translator/Imperative.hs) says
  every native manifold parameter is passed by const reference, as the
  reason a projection take may not move out of a bound variable. A Rust
  parameter taken by value is owned. No wrong take was found.
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
- **A Rust value kept inside an inlined nested function is cloned there.**
  [reproduced 2026-10-19] Gap in OWN-7. `both a c = a + count 0 c` called
  as `both (chainLen c) (prepend 0 c)` (or with the arguments swapped)
  reads `c` with `chain_len(&c)` first, then passes `&c` to a manifold
  whose nested manifold builds `Link(0, n6.clone())`: one full clone of the
  chain (100 for 100 links), though the caller's read is its last. The
  order of the two reads is already right in and out of loops, in C++ and
  Rust; the copy is the nested manifold's parameter mode, which stays a
  borrow although it keeps the value. C++ copies nothing here (`data`
  arms are shared). A let read once by a constructor copies the same way:
  `let c = mkChain n in chainLen (Link 1 c)` builds the `Link` in a nested
  manifold given `&c` (100 clones, reproduced 2026-11-09).

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
