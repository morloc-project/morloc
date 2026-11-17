# Native forms

Where the code does not yet treat a form's list as NATIVE-1 states (the
declared parameters work like a function's: the list after the form names
any of them in any order, `$i` is its i-th type item, a form with no `$i` is
its text). To be resolved together in the macro-expansion refactor.

- **A type alias's form must list its parameters in declaration order.**
  [read, 2026-11-17] Deviates from NATIVE-1. Restructure.hs formListProblem
  refuses a list out of the declaration's order, because later passes pair
  a form's list with the declared parameters by position.
- **Several passes pair a form's list with the declared parameters by
  position.** [read, 2026-11-17] Deviates from NATIVE-1. Desugar.hs gives a
  record form with no list the declared parameters as its list;
  Restructure completeValue replaces a form's list with the declaration's;
  GenDefs recordArgs/useArgs refill arguments by parameter position; the
  C++ marshalling header (Cpp.hs makeSerial templateTerms) declares a
  template parameter for every declared type parameter, including ones no
  `$i` spells, so `record Cpp => Ph k = "ph_t" k` does not compile.
- **The C++ record renderer appends the arguments to a form without `$i`.**
  [reproduced, 2026-11-17] Deviates from NATIVE-1. Cpp.hs
  renderTemplatedType spells `record Cpp => Box a = "BoxS"` as `BoxS<int>`
  at record sites, while type expressions in signatures spell `BoxS`
  (expandMacro). spec-cost-15-4 and object-1-c rely on the appending (their
  C++ types are templates written without `$1`). A value built from the
  wire is then spelled two ways (issues/wire-spelling.md).

- **A reordered or partial record list in C++ does not build.** [reproduced,
  2026-11-17] Deviates from NATIVE-1. `record Cpp => P a b = "PObj<$1,$2>" b a`
  (fields `x :: a`, `y :: b`, used at `P Int Real`) spells both `PObj<int,double>`
  and `PObj<double,int>`, and g++ refuses the conversion; `"PObj<$1>" b` gives
  "template parameters not deducible in partial specialization". Both fail at
  the C++ compiler, never at run time.
- **spec-native-1-6 asserts the deviation.** [read, 2026-11-17] It passes by
  refusing an out-of-order type form list, which NATIVE-1 allows; it turns red
  when the order rule is lifted, and is then rewritten as an accept.
- **A per-language line's own parameter list decides which items are kinds.**
  [read, 2026-11-17] Restructure isTypeItem reads the kind from the line's
  parameters, so `record Cpp => P n a = "PObj<$2>" n a` for a general
  `record P (n :: Nat) a` counts `n` as a type item and accepts `$2`, which
  codegen (kind args dropped) leaves unfilled. Affects type forms too.
