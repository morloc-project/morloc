# Kind-level arguments read as values

A ground Rec-kinded argument (the row `{x = Int}` of `Table n {x = Int}`)
lowers to `NamT NamRecord @REC [] fields`, the constructor of a value-level
anonymous record (Namespace/Type.hs `typeOf` on `RecExtendU`, and `LitU
(LRec ..)`). A polymorphic row, and every List- or Set-kinded argument,
lowers to `NatVoidT`. Code generation decides whether an argument is a
runtime value by its shape (`isKindTypeF/T/U`, `partitionKindArgsF/T/U` in
CodeGenerator/Namespace.hs), so a ground row passes as a value and a
polymorphic one as a Nat. Spec gap: KIND-2 says non-Type kinds are erased;
no item says how the lowered IR marks an argument's kind.

- [read] Walks that still read a ground row as a record (harmless today,
  since a row holds only primitive, Str and optional columns): EffectBoundary
  `hostRecords` collects `@REC`; the Nexus glossary emits an `@REC` entry
  its filter then drops; C++ `collectCppTypes` writes no static_assert for
  any typed table; Grammars/Common.hs `collectRecords` walks a row's
  fields (it collects only generated `"struct"` records). The collectors that emit code decide through
  CodeGenerator/ValueArgs.hs; these do not.
- [reproduced, 2026-10-20, d352b5d4] `$N` in a native form counts a
  ground row as a type argument and skips a polymorphic one. With `type
  Frame (r :: Rec) a` and `type Cpp => (Frame (r :: Rec) a) =
  "std::vector<$1>" r a`, `Frame {x = Int} Real` renders `std::vector<@REC>`
  while `Frame r Real` renders `std::vector<double>`. Sites:
  Express.hs renderConcreteForm and maybePackableWrap, Cpp.hs
  dataTypeName, the AppF and NamF branches of cppTypeOf, showDefType,
  Rust.hs nominalTypeName and the AppF branch of rustTypeOf. Left for the
  native-form macro refactor; `$N` is not changed before it. The C++
  case fails the same way when the functions are also sourced in Python
  and placement chooses C++ (`mkF`/`lenF` sourced in both, `dbl` in C++
  only, `bar k = dbl (lenF (mkF k))`).
- [read] `isKindTypeT` leaves out the Str forms (`StrLitT`, `StrVoidT`,
  `StrConcatT`) that `isKindTypeF` and `isKindTypeU` include, so the
  `Type`-level `$N` callers (Cpp.hs showDefType via `partitionKindArgsT`,
  the list-element lookups in Express.hs) count a label as a type argument.
  Left for the macro refactor with the other `$N` sites.
- [read] C++ `makeSerial` (Cpp.hs, `isTypeParam`/`templateTerms`) counts a
  Rec-, List- or Set-kinded parameter as a template slot (`T<r>`) but
  declares no `template <class>` for it. Left for the macro refactor.
- [read] Rec, List and Set placeholders erase to `NatVoidT` and lift back
  as `NatVoidU` (`type2typeu`), a Nat wildcard: Realize `compatibleType`
  accepts one against a Nat literal, and Docstrings prints it as `NAT`.
