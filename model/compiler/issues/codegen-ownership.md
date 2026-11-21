# Ownership in generated pool code

Where a pool takes a value instead of copying it, and a later read of the
same name sees what was taken.

- **Deciding a take walks the manifold once per candidate.** [read] Cost,
  not correctness; gap in the ownership rules (no spec item says when a
  projection may take). `consumableProjectionLets`
  (library/Morloc/CodeGenerator/Grammars/Translator/Imperative.hs) counts
  uses and then runs `moveWalk` for each surviving candidate over the
  whole top-level body, and at every nested manifold or do-block the walk
  re-walks that subtree (`rnodeBinds`, `rnodeNames`). One pass in
  evaluation order with a verdict per subject could replace both the count
  (with its throw-arm exemption and max over arms) and the per-candidate
  walks.
- **A C++ `Try` bound from a suspension's result is copied when unwrapped.**
  [reproduced] Cost; gap in OWN-5. `r <- pull; match r | (Ok xs) = ...`
  copies `xs` (golden cpp-copy-count `runParts`: 4 allocations per batch, 3
  with a take; Rust already takes through `take_with`). Treating a forced
  result as a take subject was tried and reverted: its arms may come from a
  sourced function, built as a const object or also held by a user's
  `std::weak_ptr`, and `mlc::take_field` moves through `const_cast` after a
  `use_count` test that sees neither. Options: tag the arms morloc
  allocates (a deleter costs a second allocation per arm), or rule that
  sourced code never builds a const arm nor keeps a weak reference to one.
  Stdlib sites: parallel `psconcatMapWithPure`, `psfoldPureLoop`,
  `psmapWithPure`, `psfilterWithPure`, `pullParts`. Ruled 2026-10-18: a
  forced result is the frame's own once every way a suspension can return
  what it captured is handled; the arm origin above is what remains.
- **`otherArmCpp` copies though no path reads the payload after a take.**
  [reproduced] Cost; same gap. In test-suite/golden-tests/projection-consume,
  `match t | (Ok v) = lenCpp v | (Err _) = lenCpp (match t | (Ok w) = w | ...)`
  keeps both projections copies; cause not traced.

- **A record field passed to a by-reference parameter is cloned first.**
  [read] Cost, not correctness. In a Rust pool, `f (.models r)` where `f`
  takes its argument by reference generates `let n: T = r.models.clone();`
  and then `f(&n)`, where `f(&r.models)` would do. In lib/biolib/rnaseq's
  per-batch chain (batch.loc, `scanSelected` and the duplicate-key steps)
  this copies the transcript models three times, plus the QC references and
  the rRNA screen, for every batch on every worker thread. Seen in the
  generated pool for `rnaseq` (pools/rust/src/main.rs, `n1625.models.clone()`).
- **A C++ conditional moves its result twice.** [reproduced] Cost, not
  correctness; no item bounds the moves a lowered conditional adds. A
  `? :` result is assigned to a default-constructed helper
  (`helper0 = std::move(n4)`) and then moved into the returned local
  (`T n5 = std::move(helper0)`), so each level of a recursion returning a
  value moves it twice, where a native conditional return moves it at most
  once (the default constructor it needs is in codegen-limits.md). Seen in
  golden:spec-emit-3-1 (`up 3`: 8 moves). An arm whose value is a
  borrowed `const T&` local is "moved" too (`helper0 = std::move(n54)`),
  which copies while reading as a move, since `armOwnsResult` counts a
  borrowed let as owned. Binding the let as `T&&` to the helper was tried
  (2026-10-19) and gains nothing where the let is returned: the return
  then moves instead of eliding; the helper itself would have to be the
  returned variable.
- **A nested constructor pattern in Rust copies each inner field it tests.**
  [reproduced] Cost; no ownership item covers a projection read only to be
  tested or projected again. `match c | (Link _ (Link _ rest)) = rest`
  renders the inner field as `match &(c) { Chain::Link(mlc_b) =>
  mlc_b.1.clone(), .. }` both inside the `matches!` tag test and under the
  outer projection, so every nested match copies the inner value twice. For
  a user type with a recursive `Clone` (a `Box` chain) the copy is the whole
  rest of the value, so a loop stepping with such a match is quadratic and a
  deep value overflows the stack. Because of these copies OWN-6 does not
  treat a nested match as a part either: its test is a `Bool` local and its
  field is read from a field.
- **A Rust loop stepped by a function that returns part of its argument
  copies the part.** [reproduced] Cost; OWN-6 sees only parts the loop body
  reads itself. A `match` with a guarded arm is not inlined, so
  `tlG c = match c | (Link x rest) ? x > 0 = rest : c | _ = c` stays a
  manifold returning an owned `Chain`, and `dropG (k - 1) (tlG c)` copies
  the rest of the chain each step (3 steps over 1000 links: 2994 clones).
  A result that borrows from an argument needs the by-reference return
  convention OWN-8 proposes (open, unaudited).
- **A function's local and remote-worker segments share one plan key.**
  [read, speculative impact] A C++ or Rust pool can hold two segments for
  one manifold id, the local one and its remote-worker twin
  (`emittedId`, library/Morloc/CodeGenerator/Emit.hs). The pool plan keys
  payload takes (OWN-5) by manifold id and let index
  (`consumableProjectionLets`), so a take found in one twin also applies to
  the let with the same index in the other. That is wrong only if the
  twins' let indices differ; no golden showed it, and the ownership check
  reported nothing. C++ moves (OWN-2) are planned per segment.
- **The C++ OWN-1 check runs only under `MORLOC_CHECK_OWNERSHIP`.**
  [read, 2026-10-21] Against compiler/README.md (fail loudly).
  Pools/CAbi/Members/Cpp.hs:555 reads the variable; only
  GoldenMakefileTests.hs sets it, so a user build compiles a C++ pool that
  reads a moved-from value. Rust relies on rustc, whose error does not
  name OWN-1. Cost of enabling it by default is unmeasured.
- **Do OWN-1..7 bind C++?** [read, 2026-10-21] ownership.md's preamble
  and codegen-copies.md say C++ copies more than OWN-1..7 allow, yet the
  items name no language, are marked conforms, and Cpp.hs cites OWN-2/3/5/7.
  Either each item says it is Rust-only, or C++ is `deviates` against
  codegen-copies.md.

- **A same-pool serial call's result is never released.** [read, 2026-10-24]
  `isCallResult` (Translator/Imperative.hs) excludes `AppRecS` on the
  premise that a callee may return an argument packet as it is. In C++ and
  Rust a manifold that returns an argument now returns a copy
  (`lcDupPacket`, tagged by `serialManifoldArgsBound` in
  Grammars/Common.hs), so every serial manifold result there is a packet of
  its own; the caller of an `AppRecS` never frees it, leaking the packet
  bytes until the process ends and its shared memory until the dispatch's
  flush. Needs a reproduction (a same-pool serial recursion in C++ whose
  callee returns an argument) before changing who releases it.

- **The ownership check trusts the last-read marks.** [reproduced,
  2026-11-21, by a planted bug] `MORLOC_CHECK_OWNERSHIP` checks the takes
  and moves a member decided against evaluation order, given which reads
  are marked as last reads; it does not check that a rendered move sits at
  one. A C++ member changed to move an owned let at any read in a keeping
  position built cleanly under the check, and the runtime payload checks of
  golden own7-matrix-cpp caught it (15 cases read a moved payload). A check
  that recomputes last reads, or that reads the moves from the rendered
  code, would close the gap.
- **A call of a local function is not a read in last-read marking.**
  [reproduced in Rust, read in C++] Code violates OWN-2: `readCounts`
  (Imperative.hs) counts `LetVarN`/`BndVarN` only, so in `f a` (an
  `AppExeN` of `LocalCallP f`) `f` is not read, and a value `f` refers
  into may be marked last before the call. Rust binds no function-typed
  let as a reference into an owned value (`ownedPartLets` skips them); a
  version that did failed rustc E0505 in golden
  variant-closures-crossing-rust (`unboxG`: `f` borrowed from `b`, then
  `.1 b` took `b`). C++ binds a constructor-field let of function type by
  reference (`cppLetMode` LetBorrow), so the same shape may read a moved
  closure there. Probed 2026-11-21 under ASan and payload checks: a closure
  stored in a tuple and called after, and a function field of a `data`
  value bound by reference and called beside a move of the value into a
  list, both give the right value; the generated code calls the function
  before the move, an order nothing guarantees. `usesIndex` already counts
  `LocalCallP` and `PapplyP` as uses.
