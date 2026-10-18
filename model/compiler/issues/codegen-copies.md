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
  (`(n)[0].clone()`, always), a tag test on a nested field, a conditional
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
  into a constructor, tuple, record, optional or loop continue only; a let
  whose right-hand side ends in one, as when a call result is hoisted into
  a typed local and then copied into the let (golden copy-census-cpp
  loopLet and loopGrow: 1 copy per step), list literals, `std::bind` captures, conditional arms, a loop's
  exit and by-value source arguments still copy one (`accL`: 2k+3 copies
  for k+1 values built). A C++ list literal
  copies each element twice (an initializer list).
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
  the lets a setter may take.
- **A Rust staged closure deep-copies the captures it just took.** [read]
  Rust.hs ~1073 emits `let __sc = __caps.clone();` for every closure with a
  stage, right after `capInit` filled `__caps` by moving dead values, so
  the OWN-3 move is undone and each capture is copied once more. The
  per-level `__xj.clone()` of a pap with no stage feeds only `pap(&...)`
  and could borrow.
- **C++ closure forms copy each capture several times.** [read] Per
  capture not moved: a `MorlocClosure` 2 (binder and reify thunk), a pap 6
  (pool.cpp ~1391-1392 copy `fn` and `reify` into the closure, then into
  `apply1`), a staged form 8; each pap step copies every capture and the
  applied argument 6-8 times (pool.cpp ~1371, ~1394-1395: named
  by-value parameters). Reify reads through `const&` and needs no copy.
  copy-census-cpp does not reach these forms: they are built only for
  closures that cross (`crossingClosures`, Common.hs).
- **Closure and wire paths copy what they only serialize.** [read] Rust:
  staged closures re-clone every earlier argument at each level (about n^2
  argument clones for an n-ary function); a reflected closure's origin,
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
- **C++ crossing closures copy each capture about four times.** [read] A
  closure that may cross a pool is a `MorlocClosure`, pap or staged value
  holding a `std::bind` binder and a reify thunk that copies the same
  captures; the runtime's `mlc_pap::make`/`mlc_staged::make` take the binder
  by value and copy it into their state and again into the apply lambda,
  and a pap step copies each applied argument twice.
- **Rust staged closures clone what the stage call only borrows.** [read]
  At the level that calls the stage, every earlier argument and the capture
  tuple are cloned though the call borrows them (`stagedDoc`, Rust.hs).
