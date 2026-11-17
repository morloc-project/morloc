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
- **An unpacked occurrence of a type whose form spells no argument carries
  its native arguments.** [read, 2026-11-17] Deviates from NOM-4. A record or
  `data` node at the unpacked level carries its unpacked arguments only where
  its native form spells them (a `$i`, Namespace.hs UnpackedArgs); a
  generated record is native at both levels, and any other node keeps the
  native arguments, so its key is its native type's.
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
- **A wire back-reference is cut by one relation and named by another.**
  [read, 2026-11-16] Deviates from NOM-6. Where the wire-form builder ties
  a back-reference or a `data` back-edge, the declaration it names is
  chosen by resolveWireName, which matches arguments structurally and falls
  back to the nearest declaration of the same name. Any change that ties by
  key must choose the name by key in the same step, or a reference tied to
  one instance can name another (`Rose#1` for an outer `Rose`).
- **A native-to-wire node hands its unpacked type out as native.**
  [reproduced, 2026-11-17] Deviates from NOM-4. Serialize.hs (the NativeToWireN
  built from `unpackedAsTypeF (serialAstToType ast)`) stores the unpacked
  type as a plain TypeF, and Namespace.typeFof returns it unmarked, so every
  fold over NativeExprs (Common.poolTypesBy, GenDefs.inPoolSerialASTs `held`,
  the C++/Rust record and variant collectors) sees an unpacked type as a
  native one. Generated records stay native inside unpacked types, so
  serializers are unaffected, but the collectors merge the two levels:
  `data Shape = Circle Date | Box Int` (Date packed to Str) is refused for
  an identity function in C++ and Rust ("the `data` type Shape is used with
  two field types for its constructor Circle: (date_t) and (std::string)",
  /work/plans/nominal-types/scouts/adversarial/c6-data-packed-arm/ctl/min-cpp.loc, min-rust.loc), and in Rust a
  user-mapped record holding `Shape Date` is refused as two instantiations
  (/work/plans/nominal-types/scouts/adversarial/c6-data-packed-arm/ctl/holder-rust.loc). Fix: the node carries `Unpacked` and typeFof unwraps it
  with its level.

- **Intrinsic schemas are derived from the unpacked type of a packed
  argument.** [read, 2026-11-17] Deviates from NOM-4. Serialize.hs
  intrinsicSchema (about 935-1013) runs after packDataArg, so @save, @savej,
  @show, @hash, @eq, @le and @write call makeSerialAST on `typeFof` of a
  NativeToWireN, an unpacked type, instead of the node's stored serial form.
  Output is the same today only because re-deriving from the unpacked type
  gives the same schema.

- **C++ intrinsic writers pass a nested packed variant unpacked.** [read,
  2026-11-17] Deviates from NOM-4. Serialize.hs packDataArg (the C++
  `packedVariant` branch, about 845-851) peels only an outermost SerialPack;
  when the packed sum type sits inside another type, @show/@save/@hash hand
  the native value to a marshaller that does not pack it.

- **A `data` back-reference reaching code generation as a record reference
  is spelled wrongly.** [read, 2026-11-17; unreachable in the goldens,
  stdlib and biolib by census: no `data` RecF reaches the renderers] Deviates
  from NOM-1. Serial cuts (the VarF and AppF cuts, and the general-side
  builder) give a `data` type's back-reference as SerialRec, projected to
  RecF, alongside the armless VariantF form. In C++ the RecF renderer's data
  branch (Cpp.hs about 410) spells a template data type by its bare name,
  with no arguments; Rust's RecF renderer (Rust.hs about 437-452) has no data
  case and reports "no Rust form"; and the shared_ptr/Box rule for an
  optional back-reference (Cpp.hs about 424, Rust.hs about 365) boxes a data
  RecF where the armless VariantF spelling of the same value gives
  optional/Option -- one type, two native spellings.

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

- **Keys carry no level and no self-argument.** [read, 2026-11-17]
  Deviates from NOM-1. Code-generation keys are a declaration's name and
  argument keys only. An unpacked occurrence whose form spells its
  arguments has its unpacked arguments' key; any other has its native
  type's (`data Shape = Circle Date` is one key at both levels, which the
  `data` merge refuses as two field types for one constructor). A
  self-containing instance's reference carries no arguments, matched to the
  one instantiation of its name on the path (Serial.hs, the back-reference
  cut).

- **The agreement of a use with its definition does not instantiate kind
  parameters.** [speculative, 2026-11-17] Deviates from GEN-9. GenDefs.hs
  checkUse instantiates only type parameters, so a field spelling a `Nat`
  parameter (`xs :: Vector n a` in `record V (n :: Nat) a`) keeps the
  definition's placeholder kind argument and differs by key from every use
  (`Vector 3 Int`). Rust checks user-mapped records too, so such a record
  mapped to a Rust form would be refused. Not reproduced: root-rust has no
  `Vector` form.
