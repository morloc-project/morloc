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
- **A record's arguments are paired with the concrete side.** [read,
  2026-11-16] Deviates from NOM-3. A record occurrence's arguments are
  walked against the pairEval side, and a `data` type's against the general
  side only. Uses on the unpacked side carry the native arguments, so
  identity is read back from fields (GEN-9 deviation entry in
  ../../language/issues/records.md).
- **An occurrence's fields can disagree with its definition.** [read,
  2026-11-16] Deviates from NOM-2. Every occurrence carries its own inline
  fields or arms, built where it is met, and agreement with the definition
  is checked afterwards (GenDefs checkUse) rather than holding by
  construction.
- **An unpacked occurrence carries its native arguments.** [read,
  2026-11-16] Deviates from NOM-4. The unpacked projection of a serial
  form keeps the native-level arguments on a record or `data` node whose
  fields are unpacked (Serial.hs serialAstToType), so neither its
  native-level key nor its unpacked arguments can be read from it.
- **Native identity is decided by rendered text.** [read, 2026-11-16]
  Deviates from NOM-5. Merging `data` occurrences, grouping GEN-14
  conversions and deduplicating C++ records all compare rendered native
  text.
- **A wire back-reference whose arguments match no declaration binds to
  the nearest one of the same name, or to none.** [read, 2026-11-16]
  Deviates from NOM-6. Serial.hs resolveWireName falls back to the name
  when no declaration on the path has the reference's arguments. That is
  right for a declaration that carries no arguments (a recursive list or
  tuple alias), but for a declared type spelled at other arguments it
  silently names the wrong nested declaration. With no declaration of that
  name on the path at all, it emits a dangling `^name`. Those two cases
  should be compiler errors. Probably reachable only through the refused
  leak cases.
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
  expanded on demand). Since instance protection (pairEval), `C (C (C
  Int))` expands each inner instance in every field that holds it, so the
  size grows as (fields holding it)^(depth): with two such fields, depth 10
  compiled in 8.7 s and 982 MiB where the name cut took 0.4 s. Instances of
  recursive records nested more than 6 deep in each other (counted along
  the path, across declarations) fall back to the name cut, which C++ and
  Rust refuse and Python and R marshal as before (a depth-7 value round
  trips exactly). Alternating recursive records `A (B (A ...))` with three
  fields each, 10 deep, take 8.9 s and 770 MiB (the name cut: 5.5 s and
  408 MiB). A key spelled through an alias (`C I` with `type I = Int`) is
  not equal to the same instance spelled directly, so it unrolls one level
  more before being cut. These go away when declared types are referred to
  by key (phase B).
- **Nexus value validation resolves back-references by name only.**
  [read, 2026-11-16] Deviates from NOM-6. Nexus.hs validateValueAgainstAST
  keeps a name-keyed environment of the records on the path (RecEnv) and
  resolves a back-reference by name, ignoring arguments and `#k`, so an
  @unroll or default value can be checked against the wrong instance where
  a back-reference means an outer instance of the same name. Same class as
  the resolveWireName fallback above.
