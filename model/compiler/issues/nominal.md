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
- **A wire back-reference whose arguments match no declaration binds to
  the nearest one of the same name.** [read, 2026-11-17] Deviates from
  NOM-6. Serial.hs resolveWireName, and the nexus value check, fall back to
  the nearest enclosing declaration of the reference's name when none on
  the path has its arguments. A declaration that carries no arguments (a
  recursive list or tuple alias) and a newtype head renamed over its parent
  rely on it; for a declared type spelled at other arguments it would name
  the wrong nested declaration. Probably unreachable. A reference with no
  declaration of its name on the path is now a compiler error.
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
- **A record's type arguments are expanded beside the fields that hold
  them.** [reproduced, 2026-11-17] Deviates from NOM-2's intent. A resolved
  record carries its arguments fully expanded next to the fields holding the
  same type, and every walk over both (inference, Infer.hs NamU case;
  GenDefs recordUses comparing whole trees; Serial.hs unpackedArgsOf;
  throughNodes) multiplies: a non-recursive `Box a` (two fields) nested 12
  deep takes about 28 s and 42.8 GB allocated, growing 3x per level, where a
  parameterless record of the same 4096-leaf shape takes 3.5 s and 3.7 GB.
  Generated code is the same size either way. A heap profile also shows
  hundreds of thousands of retained compiler states (314 MB of a 566 MB peak
  at depth 11), probably lazy state restores (Infer.hs withModified,
  hostConvention). /work/plans/nominal-types/scouts/bugs/5-cost.md.
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
- **A native-to-wire node hands its unpacked type out as native.** [read,
  2026-11-17] Deviates from NOM-4. Serialize.hs builds an intrinsic's
  NativeToWireN with the unpacked projection as a plain TypeF, and
  Namespace.typeFof returns it unmarked. A generated record and a `data` type
  unpack as their native types, so this matters only for a user-mapped
  record rebuilt field by field, which C++'s record collectors then see
  with its unpacked fields under its native key.

- **Intrinsic schemas are derived from the unpacked type of a packed
  argument.** [read, 2026-11-17] Deviates from NOM-4. Serialize.hs
  intrinsicSchema (about 935-1013) runs after packDataArg, so @save, @savej,
  @show, @hash, @eq, @le and @write call makeSerialAST on `typeFof` of a
  NativeToWireN, an unpacked type, instead of the node's stored serial form.
  Output is the same today only because re-deriving from the unpacked type
  gives the same schema.

- **A record's self-reference through a type alias is not tied.**
  [reproduced, 2026-11-17] Violates GEN-1. The knot key is the record name
  with its general arguments compared as written, so `sub :: ?(P (Id a))`
  in `record P a` (with `type Id a = a`) is a new instance, unrolled until
  the walk stops by name: `morloc make` fails with "No Packable instance
  for type 'Id (Id Int)'". /work/plans/nominal-types/tests/alias-knot/main.loc.
- **A generated record field written through an alias of its parameter is
  refused.** [reproduced, 2026-11-17] Violates GEN-9. `other :: Id a` in
  `record P a`: "its field `other` is `int` there but `T_a` in the
  definition" -- at placeholders `Id &a` is not resolved to `&a` before the
  agreement check. /work/plans/nominal-types/tests/alias-knot/flat.loc.

- **A user-mapped `data` template is compared by its unexpanded form when
  deciding a Packable wrap.** [read, 2026-11-17] Deviates from NOM-1.
  Express.hs renderConcreteForm renders a VariantF by its raw form
  (`MyTree<$1>`), so a native and a wire type that are different
  instantiations of one template render alike, and maybePackableWrap may
  skip a wrap it needs.

- **Keys carry no self-argument.** [read, 2026-11-17] Deviates from NOM-1.
  A self-containing instance's reference carries no arguments and is
  matched to the one instantiation of its name on the path (Serial.hs, the
  back-reference cut).
- **C++ spells a record with a definition but no struct by its bare name.**
  [read, 2026-11-17] Pools/CAbi/Members/Cpp.hs (the generated-record case of
  the type renderer, about 356) returns the record's morloc name with no
  error when the record has a definition but no generated struct.
