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
- **Parameterized and phantom records still need a per-language line in
  C++ and Rust.** [read, 2026-11-15] Violates GEN-1 and GEN-9. A
  generated record with type parameters is still built per pool from its
  uses (Grammars/Common.hs unifyRecords) and is never declared in its
  module's file, so a recursive one (`record Container a` with
  `sub :: ?(Container a)`) is refused in C++ and Rust, asking for a
  `record Cpp =>` line (Cpp.hs and Rust.hs, RecF "struct"). A record with
  a parameter no field uses gets no default form in any language
  (Restructure.hs withDefaultKinds), as before GEN-1, so it still needs a
  line such as `record Py => Box a = dict` (spec-cost-15-4). Without the
  line the error misleads: spec-cost-15-4 reports that the instance
  PatternAccessible Box "gives no cpp implementation of
  '__extract_pattern__'" [reproduced], not that Box has no Python form.
  Both need the declaration-driven definition GEN-9 describes.
