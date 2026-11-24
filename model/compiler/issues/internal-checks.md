# Internal invariants checked silently or not at all

The entries below are fallbacks that turn an impossible shape into wrong
output. Spec gap: no item says an internal invariant fails loudly. Type
changes that remove each class are in /work/plans/gadt/type-audit.md. All
[read, 2026-10-29].

- **A data type's declared parameters paired with its arguments.** [read,
  2026-11-05] CodeGenerator/Infer.hs (needed, inferVariantArms),
  CodeGenerator/Serial.hs (appliedTypeToSerialAST) and TypeEval.hs
  (dataCtorsOf) pair the parameters a `data` declaration gives, which drop a
  specialized one, with its arguments, exactly. Not reachable today: the
  frontend refuses a bare parameterized `data` (arity check) and a `data`
  with a specialized parameter (`data Box Int b = Full b | Empty`). That
  refusal is unclear, though: "Cannot compare types Box b@q0 and Box Int
  b@q0" at the declaration [reproduced]. Namespace/Type.hs paramPairs
  pairs both cases if specialized `data` parameters are ever allowed.
- **Pairings whose lengths only an earlier check guarantees.** [read,
  2026-11-05] Restructure.hs checkAliasApplications: the kind check reads
  the first scope entry as an alias while the arity check requires every
  entry to be one, so a name with mixed entries would skip the arity check
  and reach the exact pairing (no such program found: conflicting
  declarations are refused first). Namespace/Type.hs paramPairs: its
  fallback faults when an application has more arguments than variable
  parameters; Serial.hs appliedTypeToSerialAST calls it unguarded.
  TypeEval.hs dataCtorsOf is given no arguments for a bare head.
- **Intrinsic arguments left unchecked.** Typecheck.hs:2142: the
  checkIntrinsicArgs catch-all `_ -> return g` accepts an intrinsic with no
  listed case without checking its arguments (an arity `intrinsicArity`
  fixes is refused first, :2098). Ownership.hs:154-155: an intrinsic
  argument pattern `(x : _)` falls through to Nothing.
- **Selector leaf with a missing key is dropped.** Typecheck/Internal.hs
  selectorGetter `maybe [] ...` drops the leaf, so a multi-key selection
  yields a shorter tuple.
- **Effect variables dropped.** Namespace/Type.hs resolveEffectSet turns an
  unsolved effect variable into no effect when casting to Type.
- **Unknown type becomes passthrough.** Serialize.hs turns an UnkF argument
  into Passthrough (TC-3 is the upstream check).
- **Recursive types.** Serial.hs isSerializable is True for SerialRec, for
  the back-reference SerialVariant (empty arm list) and for SerialUnknown,
  so a non-serializable recursive value can pass through as wire form.
  SerialRec lowers to `RecF v`, dropping its type arguments, so a templated
  recursive mapping loses them. Rust.hs renders RecF from the CVar slot
  that Namespace.hs documents as unreliable (C++ looks the name up in
  cscope instead).
- **Closure capture types filtered, then paired.** Rust.hs:1714 and
  Cpp.hs:686 build `ctxTs` by keeping only captures with a native type
  (`orNativeType`), then pair it with the closure table's schemas
  (`zip3Same` Rust.hs:1729, `zipSame` Cpp.hs:694); one untyped capture is
  a compiler fault rather than a located error, and the filter hides which
  capture it was.
- **Two tuple pairings bypass Morloc.Data.Zip.** [read, 2026-11-14]
  Cleanup. Frontend/Typecheck.hs:1279 and :2748 pair with `NE.zip es0
  slots`; the lengths are equal by construction, but the repo rule is to
  pair through `Morloc.Data.Zip` (`zipSame`).
