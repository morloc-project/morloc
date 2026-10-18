# Evaluation order in generated pools

- **Arguments are evaluated right to left in every pool.** [read, one
  snapshot] The inverter (`atomize`/`weave`,
  library/Morloc/CodeGenerator/Grammars/Common.hs) lets each argument's
  dependencies in reverse order, so the last argument's lets run first: in
  copy-census-cpp twiceKeep, `n59 = (n55 + n58)` computes `n58` (the right
  operand) first. Python and R go through the same inverter. EVAL-2 (Intent:
  proposed) allows any order of pure work; if it were ruled the other way,
  labelled calls and aborts inside one expression would already be out of
  order. Open question: EVAL-2's ruling.
