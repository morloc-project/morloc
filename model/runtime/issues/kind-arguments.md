# Kind arguments (Nat, Str, Rec) in native forms and checks

Type arguments that are not types (`n :: Nat`, `r :: Rec`, string
literals) are told apart from type arguments by several separate rules,
and they disagree. Spec gap: model/language/native-types.md does not say
how kind arguments reach a native form.

- [read] CPP-3/CPP-4 are also skipped for a type applied to a ground row:
  Cpp.hs `collectCppTypes` sees the row as an anonymous record type whose
  name is not a type name, so the whole applied type is dropped.
- [reproduced] A record form cannot specialize on a Nat literal: `record
  Cpp => Foo (3) a = ...` beside `record Cpp => Foo (4) a = ...` is
  refused as conflicting forms. Restructure.hs `completeValue` replaces
  the form's parameters with the general declaration's before the
  conflict check, so both look alike; with one such form, its literal
  never reaches makeSerial.
- [reproduced] A record field whose type applies a Rec-parameterized
  record (`f :: Foo {y = Int} Int`, with `record Foo (r :: Rec) a` and
  `record Cpp => Foo (r :: Rec) a = "foo_t"`) fails `morloc make` with
  "Failed to infer concrete type for Bar: Could not reduce type in broadest
  scope", with no source location; with `record Foo a` it builds and runs.
  Past that, Cpp.hs `showDefType` renders every type argument of a field,
  a ground row included, where the macro path drops a row filling a Rec
  parameter [read].
  Reproduction: /work/plans/error-clusters/repro/rec-field.
- [reproduced] A native form whose `$N` names a kind parameter
  (`type Cpp => F (n :: Nat) = "std::array<int,$1>" n`) is accepted by the
  form check (NATIVE-1 counts kind items) but `$N` indexes type arguments
  only, so `$1` reaches pool.cpp unexpanded (`expandMacro t [] _ = t` skips
  the arity check) and g++ fails at `'$1' was not declared`, first on the
  CPP-3 static_assert line. Needs a located error at the form.
- [read] Macro.hs `expandMacro` raises a raw `error` (unparsable macro,
  index out of range) instead of a located error; a `$3` template on a
  two-parameter type should reach a user error first, unchecked.
- [read] Serial.hs `generalTypeToSerialAST'` (data arms, ~:2050): `zip
  params ts` pairs only the `Left` parameters of a `data` declaration with
  all its arguments, so a `data` with a specialized parameter misaligns
  them; and the arms are erased before instantiation, so `data Box (n ::
  Nat) = Box (Vector n Int)` may lose its dimension on the nexus path
  (DIM-8), where the alias path substitutes first. Unreproduced.
