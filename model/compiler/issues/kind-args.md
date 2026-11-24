# Kind-level arguments read as values

A ground Rec-kinded argument (the row `{x = Int}` of `Table n {x = Int}`)
lowers to `NamT NamRecord @REC [] fields`, the constructor of a value-level
anonymous record (Namespace/Type.hs `typeOf` on `RecExtendU`, and `LitU
(LRec ..)`), because the same row is a record type where it fills a
Type-kinded position. Every other non-Type argument keeps its kind when
erased (`VoidT k`), and `kindOfT/F/U` read it from the form; whether a
ground row is a value is decided by the declared parameter kinds
(CodeGenerator/ValueArgs.hs). Spec gap: KIND-2 says non-Type kinds are
erased; no item says how the lowered IR marks a ground row's kind.

- [read] Walks that still read a ground row as a record (harmless today,
  since a row holds only primitive, Str and optional columns): EffectBoundary
  `hostRecords` collects `@REC`; the Nexus glossary emits an `@REC` entry
  its filter then drops (Nexus.hs:2052); Grammars/Common.hs `collectRecords` walks a row's
  fields (it collects only generated `"struct"` records); Serial.hs
  `runtimeTs` and its tuple-arity check count a ground row as a runtime
  type. The collectors that emit code read the `RowK` tag Infer.hs:801
  `retagArg` sets from CodeGenerator/ValueArgs.hs; these walk the `Type`
  form, where `argOf` (Namespace/Type.hs:1089) still makes a ground row a
  record.
- [read] Cpp.hs showDefType counts a ground row as a `$N` type argument:
  it renders a `Type` whose head is the concrete name, so it cannot read
  the declared kinds as the other `$N` sites do. Left for the native-form
  macro refactor.
- [read, 2026-10-29] Namespace/Type.hs `argOf` maps an `OpU` of the wrong
  arity (arity is enforced only by the mk* smart constructors) to the erased
  placeholder of its kind, so it erases silently.
- [read, 2026-10-29] Typecheck/Internal.hs typeUToNatExpr treats any VarU
  or ExistU as a Nat variable, so a deferred pair of two Type-kinded
  variables is classified Nat; isNatExpr misses OpSize, ExistU and
  Nat-kinded VarU, so the "filter Nat args" retry skips `Vector n a` with
  an existential `n`.
