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
