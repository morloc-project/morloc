# Kind arguments (Nat, Str, Rec) in native forms and checks

Type arguments that are not types (`n :: Nat`, `r :: Rec`, string
literals) are told apart from type arguments by several separate rules,
and they disagree. Spec gap: model/language/native-types.md does not say
how kind arguments reach a native form.

- [reproduced] A record form cannot specialize on a Nat literal: `record
  Cpp => Foo (3) a = ...` beside `record Cpp => Foo (4) a = ...` is
  refused as conflicting forms. Restructure.hs `completeValue` replaces
  the form's parameters with the general declaration's before the
  conflict check, so both look alike; with one such form, its literal
  never reaches makeSerial.
- [read, 2026-11-05] Three walks decide whether a ground row fills a Rec
  parameter (Infer.hs argWith/argWith2, weaveArgs, and retagArg after a
  walk, also used by Serialize.hs record literals), each with its own row
  forms. They agree on the forms tested (row term, anonymous record, a
  form that drops the row); one slot-aware helper for all three is the
  general fix.
- [read] Serial.hs `generalTypeToSerialAST'` (data arms, ~:2050): `zip
  params ts` pairs only the `Left` parameters of a `data` declaration with
  all its arguments, so a `data` with a specialized parameter misaligns
  them; and the arms are erased before instantiation, so `data Box (n ::
  Nat) = Box (Vector n Int)` may lose its dimension on the nexus path
  (DIM-8), where the alias path substitutes first. Unreproduced.
