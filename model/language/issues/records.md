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

- **Closure fields of a templated Rust record share one wire schema across
  instantiations.** [read] Rust.hs `generateRustStructs` harvests each
  record's crossing closure fields keyed by (general record name, field), so
  a record `R a` with a field `f :: a -> a`, mapped to `"R<$1>"` and used at
  `R Int` and `R Str` with both crossing, reflects both instantiations'
  closure fields with whichever schema was harvested last. Key the harvest
  by instantiation. Not reproduced.
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
- **A C++ template handle cannot be built from a braced list.** [read,
  2026-11-16] Deviates from CPP-6 (lang-cpp.md), which gives `R{{...}}` as
  a handle's construction form. A template handle takes its fields only
  through `template <class F> requires std::is_same_v<F, R_fields<T>>`:
  a non-template constructor from `R_fields<T>` makes deciding whether `R<T>`
  can be moved instantiate `R_fields<T>`, which holds `std::optional<R<T>>`,
  a cycle g++ rejects. So `R<int>{{...}}` does not compile and a source
  writes `R<int>(R_fields<int>{...})`. Non-template handles take both.
- **A public Rust `data` type is not checked for names it cannot import.**
  [speculative, 2026-11-16] GEN-10. Rust.hs planRustPublic adds every
  eligible variant and enum to the names a public record may hold, but
  never checks a variant's own arms, so a public variant whose arm holds a
  pool-private record is written into the type files and the standalone
  `morloc types` crate would name a type it lacks. Reproduce with a
  variant arm holding a record with a sourced field.
- **A record woven without the monadic walk skips the host convention.**
  [read, 2026-11-16] Possible deviation from GEN-8. Only the two monadic
  walks of concrete inference apply the host convention to a built record
  (CodeGenerator/Infer.hs, `hostConvention` at its two call sites). A
  record built by the pure weave keeps suspension-typed fields as written,
  so a C++ or Rust record reached only through weave would hold a
  suspension where the convention gives a value. Reachable, by reading,
  through a phantom-Nat alias over a record (the weave steps the alias to
  its expansion) and through a Packable whose unpacked form is a record
  (Serial.hs resolvePacker). Not reproduced. Resolved by stage P2 of
  /work/plans/nominal-types, which makes weave monadic.
- **A self-containing `data` instance is named from its spelling.**
  [speculative, 2026-11-16] Possible deviation from GEN-9. `Rose ?X` with
  `type X = Rose ?X`, the same with `type Y = Rose ?Y`, and
  `Rose ?(Rose ?X)` may be one morloc type, but they get different native
  names (Infer.hs instanceName and selfContaining, mangled from the written
  arguments). Needs a probe to check whether the typechecker unifies them.
- **A record whose arguments hold the record itself does not build in C++
  or Rust.** [reproduced, 2026-11-16] Violates GEN-1. Gap in GEN-9, which
  rules a self-containing instance a type of its own, named by its
  arguments, for `data` only. With `record Box a where v :: Int; w :: a` and `type X
  = Box ?X`, `trip :: X -> X` builds and round-trips in Python. C++ refuses
  it with "Recursive type alias `X` has no concrete C++ mapping", which
  is misleading: Box is a generated record. Rust reaches rustc as E0425.
  Fix in /work/plans/nominal-types: key the instance with a self-argument
  (NOM-1) and name it as a self-containing `data` instance is named.
- **A C++ record mapped to a `$`-free user form takes an argument it cannot
  resolve as written.** [read, 2026-11-17] Gap in GEN-9. C++ spells every
  argument of a user form with no `$k` slots (`record Cpp => Box a =
  "box_t"` is `box_t<int>` at `Box Int`), so its arguments are resolved;
  TypeEval keeps an argument that is rejected as written, so `Box T` with
  `T` mapped only in another language reaches g++ as `box_t<T>` instead of
  a morloc error naming `T`. A Python form that drops an argument the
  general type holds (`type Py => Tag a = "str"`) likewise does not erase
  it in Python, which still resolves it.
- **A Rust self-referential record is not spelled as GEN-1 says.** [read,
  2026-11-18] Violates GEN-1. GEN-1 gives `pub struct R(pub Arc<RFields>)`;
  RustPrinter.hs printRustHandle emits
  `pub struct R(pub ::std::mem::ManuallyDrop<::std::sync::Arc<RFields>>)`,
  so a source building `R(Arc::new(f))` does not compile (`R::new(f)`
  does). Fix the code or amend GEN-1 to this spelling and `R::new`.
- **Languages define no `data` kinds.** [reproduced, 2026-11-18] Violates
  GEN-2. lang.yaml and LangRegistry.hs TypeKinds hold only record kinds;
  `data L => D = word` parses as a type alias, so `data Cpp => Shape =
  variant`, `data Py => Shape = pydantic` and `data Py => Shape = bogus` all
  fail "Could not resolve type for variable 'variant'", naming no kind and
  listing none. Python `data` types get no class file. Repro: /work/plans/generated-native-types/audit-repros/d1, d2, d3.
- **A Rust field named `self`, `super`, `crate` or `Self` crashes the
  compiler.** [reproduced, 2026-11-18] Violates GEN-5. RustPrinter.hs
  (rustFieldIdent, about 330) calls `error`; the output is a CallStack
  naming no type. Repro: /work/plans/generated-native-types/audit-repros/p5self.
- **A C++ field named like a standard macro reaches g++.** [reproduced,
  2026-11-18] Violates GEN-5. `record P where errno :: Int` fails in g++
  ("expected unqualified-id"); Cpp.hs checkFields checks keywords only.
  Repro: /work/plans/generated-native-types/audit-repros/p5errno.
- **A Rust type file imports the whole crate root.** [read, 2026-11-18]
  Violates GEN-6 (a file imports only what its definitions name).
  Rust.hs rustTypeFiles writes `use crate::*;` atop every type file (and
  private modules), so a sourced crate-root item outranks a prelude name
  the file uses unqualified (`Vec`, `String`, `Option`). Not reproduced.
- **A Rust record named like a prelude type is kept in the pool.** [read,
  2026-11-18] Gap in GEN-10. Rust.hs planRustPublic `nameOk` keeps a
  record named `String` or `Vec` (and every record holding it) out of the
  Rust type files, while C++ makes it public; GEN-10 does not list the
  case. Repro: /work/plans/generated-native-types/audit-repros/p6opt.
- **Legacy form warnings are given only in the program's own modules.**
  [reproduced, 2026-11-18] Gap in GEN-3. Desugar.hs (about 1394) warns of a
  legacy form (`"dict"`, `"struct"`) in the program's modules, never in an
  installed package's; GEN-3 does not say which.
