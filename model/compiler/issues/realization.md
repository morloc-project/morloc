# Realization

Defects in how the realizer (CodeGenerator/Realize.hs) prices a placement,
found reviewing /work/plans/realization-cost-model.md. The objective is
COST-1 (proposed); each entry is wrong under any reading of it. [read; not
reproduced]

- **A list, tuple or record is priced by its cheapest element.**
  `scoreMany` (~665-682) builds one merged table of every element's scores
  and takes the per-language minimum, which is right for alternatives of one
  term (`VarS (Many ..)`, one is chosen) and wrong for the components of a
  container (all must exist): in `[pyA, rustB]` placed in Rust, `pyA`'s
  crossing costs nothing whenever `rustB` is cheaper. Sum the components.
- **A use of a let-bound value is priced from where it was computed.**
  `refTable` (~529-557) prices a reference in language l as moving the value
  from its home h to l, but the value lives where the let runs; for a value
  computed in Rust, bound in a Python do-block and used in Rust, the second
  crossing costs nothing. lib/biolib rnaseq R36 is this shape.
- **A parameter is free in every language.** A non-function `BndS` scores 0
  everywhere (~514-519), so a value passed in and used in two pools is never
  priced.
- **Collapse and scoring price an edge differently.** Collapse picks a
  child's language with `transScore` (`cost`, ~776; `chooseLanguage`,
  ~1037), scoring with `crossScore`; once the payload term is non-zero the
  DP is no longer exact. `holding` (~420, ~432) also uses `transScore`.
- **A morloc-defined head prices its function arguments twice**, once
  inside its body (re-scoring through `rBndVars`, ~514-518) and again at the
  outer `scoreApp` (~502-505).
- **Merging two signatures keeps only the first one's docstrings.**
  `mergeEdocs x _ = x` (Frontend/Merge.hs ~83-84): a directive on the second
  signature of one term is dropped, depending on order. Affects any
  directive, cost directives included.
- **The cost model is not implemented.** [reproduced, 2026-10-21]
  COST-2..COST-10 (proposed) have no code. `@wiresize`, `@dimension`,
  `@calls` and `@runtime` warn "docstring directive ... is not recognized"
  and are dropped; call and crossing costs are the integer tables in
  data/lang/languages.yaml and each lang.yaml `cost:` (LangRegistry.hs:
  145-150; Realize.hs:380-392, 792 `crossingPayloadCost _ = 0`, 805). No
  manual page covers the directives. Unbuilt design, not a regression.
- **A bracket step through an instance is never placed in a pool.**
  [read, 2026-10-21] Realize gives a PatCall no language (Realize.hs:521),
  so a pure `f m = .[0] m` on a receiver that is not a list on the wire
  (a newtype over a tuple with a sourced `Indexable` instance) stays on the
  nexus, which refuses it ("needs its instance, which no language here can
  call") although a pool could run it. Placement should require a language
  that has the instance whenever the receiver is not a list or IFile.
- **The nexus brackets a newtype over a list natively.** [read, 2026-10-21]
  Gap in PAT-5. `newtype Deque a = List a` with its own `Indexable`
  instance is a list on the wire, so the nexus indexes it as a list while a
  pool calls the instance; the two agree only if the instance means list
  indexing (true of stdlib `Vector`). Needs a ruling: an instance on a
  list-represented newtype must agree with its representation, or such
  brackets must go to a pool.
