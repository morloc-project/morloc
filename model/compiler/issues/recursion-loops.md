# Tail calls kept as calls

RECUR-2 (ruled) promises constant stack for any tail call, but the loop
lowering (Express.hs addLoopWraps ~275-345, rewriteLoopTail ~394-432) leaves
these as real calls; each fails past Python's recursion limit at depth
200000. [reproduced, 2026-10-23; probes in a session scratch dir, rebuild
from the shapes below]

- **`--debug` builds** keep every tail call as a call. Needs a ruling:
  exempt debug builds in RECUR-2, or keep the loop and record a capped trace
  per iteration (MORLOC_DEBUG_RECURSION_CAP exists).
- **A labeled tail call** is not a loop, even with a hookless group
  (`lab: {}`). A label's post-call hooks run after the callee returns, so
  the call is not in tail position. Needs a ruling: RECUR-2 should say a
  labeled call is not a tail call; a hookless label could still loop.
- **Tail and non-tail self-calls in one function** (`? n == 7 = 1 + f ..
  : f ..`) fall back to recursion for the whole function. Code fix: let the
  surviving non-tail calls call the looped function through its native
  entry.
- **A tail call bound by `where`/`let`** (`: step where step = f (acc+1)
  (n-1)`) becomes `PolyLet i call (PolyLetVar i)`, which the lowering treats
  as a leaf. Code fix: treat that shape as a tail `call`; RECUR-2 should
  define tail position to include it.
- The `effectfulLoop`/`returnEager` gate in addLoopWraps: [read, one probe,
  2026-11-02] it does fire for a nullary recursive `<IO>` value (the
  CallS-as-value fallback in Express.hs keeps EffectT); delete it only
  after a nullary test passes, which two bugs below block
  (briefs/B7-G4.md).
- [reproduced 2026-11-02] A Python loop whose continue changes no carried
  variable (`retry k = do {x <- tick k; ? x > 5000 = tick x : retry k}`)
  is written with an empty `else:`; the pool fails to load
  (IndentationError). `genericMakeLoop` drops unchanged slots and
  `renderIf` writes an empty block.
- [reproduced 2026-11-02, after hand-fixing the empty `else:`] A nullary
  recursive `<IO>` value returned by an export fails "cannot reify a
  callable that morloc did not create (None)": its serial entry wraps the
  int result in `mlc_reify` with a closure schema. The non-recursive
  equivalent works. Cause not traced.
