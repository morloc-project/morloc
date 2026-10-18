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
- **A `Try` bound from a suspension's result is copied when unwrapped.**
  [read] Cost, not correctness; same gap. `buildsFreshValue`
  (library/Morloc/CodeGenerator/Grammars/Translator/Imperative.hs) accepts
  only a `@try` or a constructor literal as the subject, so
  `r <- pull; match r | (Err e) = @throw e | (Ok xs) = ...` copies `xs`: a
  whole batch per step of a stream drain, in a C++ or Rust pool. Sites in
  lib/stdlib parallel/main.loc: `psconcatMapWithPure`, `psfoldPureLoop`,
  `psmapWithPure`, `psfilterWithPure`, `pullParts` (the `runParts` case of
  golden cpp-copy-count counts this copy). Open question for a ruling: may
  the value a forced `<IO> (Try e a)` returns be treated as the frame's
  own? A suspension the program passed in may return a value it also
  keeps (a closure over a captured `Try`), so the rule needs either a
  proof that the forced result is fresh (it came from a `@try` or a
  constructor inside the suspension) or a by-value convention for
  suspension results.
- **`otherArmCpp` copies though no path reads the payload after a take.**
  [reproduced] Cost; same gap. In test-suite/golden-tests/projection-consume,
  `match t | (Ok v) = lenCpp v | (Err _) = lenCpp (match t | (Ok w) = w | ...)`
  keeps both projections copies; cause not traced.

- **A function called with one variable for two parameters gets a
  duplicate parameter in Python.** [reproduced, 2026-10-15, c4da6cf3]
  `two xs xs`, where `two :: [Str] -> [Str] -> <IO> Str` is a morloc
  do-block that calls a Python source twice with both parameters, generates
  `def m..(n6, n6)`; the Python pool dies at load with "SyntaxError:
  duplicate argument". Repro:

      source Py from "h.py" ("show2", "tmp")
      show2 :: [Str] -> [Str] -> <IO> Str
      tmp :: Str -> <IO> Str
      two :: [Str] -> [Str] -> <IO> Str
      two a b = do
        p <- tmp "x"
        x <- show2 a b
        y <- show2 a b
        p <> x <> y
      go :: <IO> Str
      go = do
        let xs = ["a", "b"]
        two xs xs

  Met in biolib's biocoverage test (`writeKeys "small" contigs contigs ...`).
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
  borrowed let as owned.
- **An owned Rust loop local stepped through a `match` copies the rest of
  its value every step.** [reproduced] Cost; OWN-2 counts the `match`'s tag
  tests and its field read as reads in one step, so the field is copied
  rather than taken (OWN-5). A loop whose local is owned (it is replaced by
  a built value on some iteration) and steps with `tl c`, `tl` a `match`
  returning a constructor field, copies the tail each iteration: quadratic,
  and with a recursive `Clone` a deep value overflows the stack. Open
  question for a ruling: may a conditional's test be sequenced before its
  arms, so a read in the chosen arm is the last read?
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
  Open question: a result that borrows from an argument needs a by-reference
  return convention, which no item states.
- **A function's local and remote-worker segments share one plan key.**
  [read, speculative impact] A C++ or Rust pool can hold two segments for
  one manifold id, the local one and its remote-worker twin
  (`emittedId`, library/Morloc/CodeGenerator/Emit.hs). The pool plan keys
  payload takes (OWN-5) by manifold id and let index
  (`consumableProjectionLets`), so a take found in one twin also applies to
  the let with the same index in the other. That is wrong only if the
  twins' let indices differ; no golden showed it, and the ownership check
  reported nothing. C++ moves (OWN-2) are planned per segment.
- **A C++ move into a user-mapped record needs an rvalue-accepting field.**
  [read] An owned local at its last read is moved into a record literal
  (`T a = {std::move(x)}`). A user type mapped with `record Cpp => R =
  "user_t"` whose constructor takes a field by non-const lvalue reference,
  or whose field type deletes its move constructor, built before and now
  fails to compile. Open question, beside rule S (whether a C++ source may
  take a non-const `T&`): what a mapped C++ type must accept.
