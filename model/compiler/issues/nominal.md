# Nominal type identity

Code generation does not yet identify declared types by key (NOM-1..6).
Plan: /work/plans/nominal-types/PLAN.md, phase A stages P1-P4.

- **A declared type reaches code generation in several shapes.** [read,
  2026-11-16] Deviates from NOM-1 and NOM-6. A record is an expanded
  occurrence, a tied back-reference, or an opaque reference by its morloc
  name (the name as its form, where pairEval protected a recursive alias).
  A `data` type is an expanded instance, an armless back-edge, or an
  args-less back-edge. About a dozen functions decide identity, each over
  its own subset of these shapes.
- **An occurrence's fields can disagree with its definition.** [read,
  2026-11-16] Deviates from NOM-2. Every occurrence carries its own inline
  fields or arms, built where it is met, and agreement with the definition
  is checked afterwards (GenDefs checkUse) rather than holding by
  construction.
- **Native identity is decided by rendered text.** [read, 2026-11-16]
  Deviates from NOM-5. Merging `data` occurrences, grouping GEN-14
  conversions and deduplicating C++ records all compare rendered native
  text.
- **A `data` instance built by the universal walk resolves its fields by
  the module walk.** [read, 2026-11-16] Speculative. The two walks read
  the same scope tables, but inferVariantArms, reached from the universal
  walk, resolves each arm field by re-entering the module walk
  (resolveField calls inferConcreteType), which reports errors at source
  positions and steps types differently. Whether any output differs is
  unknown.
- **A record update through a back-reference crashes the compiler.**
  [read, 2026-11-16] Grammars/Common.hs patternSetter descends a set path
  only through tuples and expanded records (`NamF`). On any other type,
  such as a field whose type reaches code generation as a back-reference
  (`RecF`) or a bare name, it calls `error` ("a set path descends into a
  type with no fields"). Generic.hs accessRecord's `error` is reached only
  from patternSetter's `NamF` branch, so it cannot fire. Not reproduced:
  a probe would be a record update (`{label = ...}`) on a field reached
  through a recursive record's back-reference, in Python or R.
- **unweaveTypeF drops the arguments of `data` types.** [read,
  2026-11-16] Serial.hs `unweaveTypeF` maps `EnumF` and `VariantF` to a
  bare `VarU` of the name, discarding their arguments. The off-path `RecF`
  unfold and the `data` storage re-inference (Serial.hs makeSerialAST')
  therefore re-infer `Choice Int` as `Choice`. So does Packable instance
  matching (resolveP), so an instance whose head is `Choice Int` is tested
  against `Choice` and may be missed or wrongly chosen. Not reproduced.
- **The `data` non-termination bound counts every instantiation on the
  path.** [read, 2026-11-16] Infer.hs inferVariantArms refuses at 64
  ancestors (`Set.size anc >= 64`), counting distinct `data`
  instantiations rather than the depth of one type, so a program nesting
  64 distinct instantiations on one path is refused as non-terminating.
- **A record nested in itself is expanded inline at every nesting.**
  [reproduced, 2026-11-16] Deviates from NOM-2's intent (one definition,
  expanded on demand). Each instance is expanded in every field that holds
  it, so the size grows as (fields holding it)^(depth); a record's own
  arguments are references (898a2f4a0), but an argument wrapped in a list,
  optional or function (`Box [Box Int]`) and a `data` type's arguments are
  still expanded beside the fields. `Box` (two fields) nested 11 deep takes
  4.5 s and 3.1 GB allocated. A key spelled through an alias (`C I` with
  `type I = Int`) is not equal to the same instance spelled directly, so it
  unrolls one level more.
- **A back-reference can carry its argument by name.** [read, 2026-11-16]
  Deviates from NOM-3. In the Rust uses of `Container (Container Int)`
  (spec-gen-9-6), the use of a tied back-reference to the outer instance
  carries its argument as `AppF Container [i64]`, the record named by its
  morloc name, where the definition at those arguments has the expanded
  record. Output
  is correct today, since C++ and Rust spell an instance from its fields,
  but the two have different keys. Probably the instance-protected
  back-reference whose arguments pairEval leaves as written, reaching a
  walk that pairs them instead of tying the reference.
- **Intrinsic schemas are derived from the unpacked type of a packed
  argument.** [read, 2026-11-17] Deviates from NOM-4. Serialize.hs
  intrinsicSchema (about 935-1013) runs after packDataArg, so @save, @savej,
  @show, @hash, @eq, @le and @write call makeSerialAST on `typeFof` of a
  NativeToWireN, an unpacked type, instead of the node's stored serial form.
  Output is the same today only because re-deriving from the unpacked type
  gives the same schema.

- **A record's self-reference through an alias of the record itself is
  refused.** [reproduced, 2026-11-17] Gap in ALIAS-1. With `type Self = R`
  and `kids :: [Self]` in `record R`, the frontend refuses "Mutual recursion
  between type definitions is not supported unless a `data` type cuts the
  cycle. Cycle: R, Self": the alias counts as a definition of the cycle.
  /work/plans/nominal-types/tests/alias-knot/selfalias.loc.
- **A generated record field written through an alias of its parameter is
  refused.** [reproduced, 2026-11-17] Violates GEN-9. `other :: Id a` in
  `record P a`: "its field `other` is `int` there but `T_a` in the
  definition" -- at placeholders `Id &a` is not resolved to `&a` before the
  agreement check. /work/plans/nominal-types/tests/alias-knot/flat.loc.

- **Keys carry no self-argument.** [read, 2026-11-17] Deviates from NOM-1.
  A self-containing instance's reference carries no arguments and is
  matched to the one instantiation of its name on the path (Serial.hs, the
  back-reference cut).
- **C++ spells a record with a definition but no struct by its bare name.**
  [read, 2026-11-17] Pools/CAbi/Members/Cpp.hs (the generated-record case of
  the type renderer, about 356) returns the record's morloc name with no
  error when the record has a definition but no generated struct.
