# Aliases

- **Spelling depends on constraint order.** [reproduced] Violates ALIAS-10.
  `type WindSpeed = Int; pick :: a -> a -> a`: `pick w i` is typed
  `WindSpeed`, `pick i w` is `Int` (both should be `Int`). With
  `type A = B; type B = Int`, `pick a b` is `A`, `pick b a` is `B` (both
  should be `B`). Cause: an existential is solved to the first spelling it
  meets, and every later comparison sees the solution already substituted
  (`apply g` before `subtype`), so the variable is gone by the second
  meeting; other solutions also store the old spelling. Design question,
  awaiting a ruling:
  (a) record each existential's spellings apart from its solution (expected
  types passed unapplied where existentials are solved), join after
  solving, re-spell the annotated tree -- large change to
  Frontend/Typecheck.hs and Typecheck/Internal.hs;
  (b) a post-pass re-deriving spellings from written signatures -- needs a
  constraint graph the typechecker does not keep;
  (c) amend ALIAS-10 so the spelling at an inferred position is
  unspecified. Only printed types, CLI metadata of unannotated exports and
  docstrings at inferred positions (ALIAS-13, open) depend on it; deciding
  ALIAS-13 first narrows the choice.
- ALIAS-14 is open, and the unit test "mutually recursive aliases are
  rejected" (`type A = [B]; type B = [A]`, test-suite/UnitTypeTests.hs)
  asserts one answer. If the ruling makes such recursion legal under
  ALIAS-7, the test becomes an accept test.
- [read] Several parallel unfolding mechanisms (TEQ-1). A newtype with no
  per-language form takes its parent's form in both TypeEval.pairEval
  (expandNewtypeBodyOneStep) and Infer.inheritedForm; the general-side
  steppers (stepTowardCompound, weave's wStep) use evaluateStep, which
  treats newtypes as opaque. `subtype` has the alias entry rule (viaUnfold)
  beside reduceAliasHead / subtypeEvaluated / reduceType arms with
  different stop sets and termination guards. filterByAliasChain is made
  finite by fuel rather than by lazy unfolding. No failing program known.
- [speculative] Desugar.hs expandParseEntries (~4783): a `@parse` command
  whose return type is a function alias flattening to more parameters than
  there are argument docs may fail with an internal error. Not run.
