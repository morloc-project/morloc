# Records

- **Are written anonymous record types part of the language?** Design
  decision, not a bug; needs a ruling. Gap in REC-3, which says
  "Anonymous records are not supported" but was ruled about record
  literals with no type from context (REC-4). It does not say whether a
  type written inline, without a `record` declaration, is legal.
  [reproduced] On 0.109.0 they work: `pts :: [{x = Int, y = Int}]`, a
  record-list literal, and `c4 = .[:3].x pts` build and run in Python and
  C++ (`[0,1,2]`); the manual teaches them (features-patterns.asc:22-27).
  Candidates:
  (a) legal: an inline record type is a structural type, and REC-3 covers
  only literals with no type. Then decide TEQ-6 (is an inline type equal to
  a declared record with the same fields?), and how C++ and Rust get a
  native form without the user struct NATIVE-3 requires (C++ builds one
  today; how, and whether Rust does, is unchecked).
  (b) illegal: every record type needs a `record` declaration. Then REC-3
  gains a rule rejecting inline record types at the signature, and the
  manual's pattern chapter and existing programs using them must change.

- **A generic Rust struct cannot carry a phantom parameter.** [read] Gap in
  NATIVE-3. A record whose parameter no field uses, mapped to a generic
  struct (`record Rust => Loc k = "Loc<$1>"`), needs the struct to use `k`,
  which Rust allows only through a field such as `PhantomData<K>`; the
  generated reader (CodeGenerator/Pools/CAbi/Members/RustPrinter.hs
  `printRecordImpls`, `Self { .. }` from the schema's fields) has no value
  for that field, so the pool does not compile. Mapping such a record to a
  non-generic struct works (one impl serves every instantiation). Open:
  fill fields the schema does not name with `Default::default()`, or
  document the non-generic form as the way.
- **Closure fields of a templated Rust record share one wire schema across
  instantiations.** [read] Rust.hs `generateRustStructs` harvests each
  record's crossing closure fields keyed by (general record name, field), so
  a record `R a` with a field `f :: a -> a`, mapped to `"R<$1>"` and used at
  `R Int` and `R Str` with both crossing, reflects both instantiations'
  closure fields with whichever schema was harvested last. Key the harvest
  by instantiation. Not reproduced.
- **A phantom parameter still needs a Python form when a Python function
  takes the record.** [reproduced] 82a8d98b lets a record with a phantom
  parameter pass through a pool without a language form for the parameter,
  but a record literal `{pos = 7} :: Loc Tag` that a Python-sourced function
  takes, where `Tag` has only a Rust form, fails at the Python use
  (`pyPos loc`; reproduced 2026-11-14, TypeEval.hs:253): "Could not
  resolve type for variable 'Tag'". The Python form `record Py => (Loc k) =
  "dict" k` never uses `k`. Repro: lib/biolib/rnaseq/notes/repros/
  phantom-py-arg/ (`morloc make -o phantom main.loc`; drop `pyPos` from `f`
  and it builds). Seen in the bio suite, whose checks run in Python on
  `Mapping Transcript Genome` values.

- **TEQ-4 contradicts the code's field-order rule.** [reproduced] TEQ-4
  (proposed) says `{x = Int, y = Str} == {y = Str, x = Int}`; c74060f6
  made field order part of a Rec, and `Frame {x,y}` vs `Frame {y,x}` is
  rejected "Rec field order differs" (/work/plans/model-audit-2026-10-21/repro/B/t/kind/fo.loc). Needs a ruling:
  (a) amend TEQ-4, order is part of a Rec (matches the positional wire
  layout); (b) keep TEQ-4 and reorder at boundaries. The code now rejects
  a conversion between a record and an anonymous type in another order,
  which is sound under either answer; (b) would replace that rejection
  with an inserted reordering.
- **Recursive records: SUM-4 contradicts ALIAS-7, and the code splits the
  difference.** [reproduced, 2026-10-21] SUM-4 makes a cycle of records
  and aliases alone an error; ALIAS-7 (ruled) allows a guarded recursive
  alias. `type T = (Int, [T])` and `record R { v :: Int, next :: ?R }` build
  and run in py; two records cycling through `?` are rejected as mutual
  recursion. The self-recursive record fails in g++ (`MlcNode<R>` before
  `R`). Question: is a record cycle guarded by `?`/`[ ]` legal, self and
  mutual alike? Repro: /work/plans/model-audit-2026-10-21/repro/C/sum/cyc1..4.loc.
- **A JSON record argument with unknown keys is accepted.** [reproduced,
  2026-10-21] `'{"a":1,"b":"x","c":4}'` for `P {a, b}` runs in py, cpp and
  r; a missing key is refused. No item covers extra keys on input.

- [reproduced, 2026-11-05] Violates REC-4: comparing two bare record
  literals (`{x = 1} == {x = 2}`) is refused at the `==`, not at the
  literal, with "No matching implementation found for '==' at type Record
  -> Record -> Bool"; the error should say a bare record literal needs a
  named record type.
- **spec-rec-4-1 passes on another rule's rejection.** [reproduced, 2026-11-07,
  installed 0.112.0] Its program is refused by CLS-18 at the use of `same`
  ("nothing fixes record_2 in Same (a [] [...])"), before Typecheck.hs
  `rejectInstanceTypedRecords` runs, so the REC-4 check and its message are
  untested and the error is not at the literal as REC-4 requires. Instance
  selection never fixes an open class argument (Instances.hs `resolve` returns
  Undetermined first), so only Packable improvement (CLS-20) can reach the REC-4
  check. The message also shows the internal name `record_2`.
- **GEN-9 does not yet cover recursive parameterized records or `data`.**
  [read, 2026-11-16] Violates GEN-9. Every generated record a C++ or Rust
  pool uses is defined from its declaration (GenDefs.hs), but one that has
  native parameters and refers to itself is refused with a planning error
  (Cpp.hs planPublicTypes, Rust.hs planRustPublic `refused`), a recursive
  record with a packed field is refused by the frontend, and generated
  `data` types keep per-instantiation names with a hash suffix (Infer.hs
  instanceName). A record with a parameter no field uses gets no default
  form in any language (Restructure.hs withDefaultKinds), so it still needs
  a line such as `record Py => Box a = dict` (spec-cost-15-4); without it
  the error misleads ("gives no cpp implementation of
  '__extract_pattern__'" [reproduced]). Plan: GEN9-DESIGN.md (in
  /work/plans/generated-native-types) stages S9.3-S9.5.
- **A parameterized generated record holding a parameterized generated
  `data` type is refused in C++ and Rust.** [reproduced, 2026-11-16]
  Violates GEN-9. `record R a where d :: Choice a` with `data Choice a =
  Pick a | Skip`, used at `R Int` from a sourced C++ function: "the
  generated C++ type for R cannot serve one of its uses: its field `d` is
  `Choice_97d1f6` there but `Choice_8fa600` in the definition". The
  definition (GenDefs.genDefs) names the field's `data` instance by a hash
  of the placeholder argument (Infer.instanceName), a type that does not
  exist, so the refusal also keeps a wrong template from being emitted.
  Before the declaration-driven definitions such a record got one struct
  per use. Fix: generated `data` as templates/generics (stage S9.5), so a
  definition's field names `Choice<T_a>`.
- **Instance names of parameterized generated `data` can collide.** [read,
  2026-11-15] Violates GEN-1 (one definition per type). Infer.hs
  instanceSuffix names an instantiation by a 24-bit djb2 hash of its
  rendered arguments, so two different instantiations can share a name,
  and nothing checks that same-named definitions agree; with fields that
  convert implicitly (int64 vs double in C++) the wrong definition could
  compile. GEN-9 (stage S9.5) removes the suffix; until then a collision
  check on the rendered arguments would make it an error.
- **A public Rust `data` type is not checked for names it cannot import.**
  [speculative, 2026-11-16] GEN-10. Rust.hs planRustPublic adds every
  eligible variant and enum to the names a public record may hold, but
  never checks a variant's own arms, so a public variant whose arm holds a
  pool-private record is written into the type files and the standalone
  `morloc types` crate would name a type it lacks. Reproduce with a
  variant arm holding a record with a sourced field.
