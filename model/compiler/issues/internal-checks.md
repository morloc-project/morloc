# Internal invariants checked silently or not at all

- **Plain `zip` truncates.** [read, 2026-10-23] About 340 `zip`/`zipWith`
  calls in library/ pair lists the compiler assumes have equal length; a
  mismatch drops elements instead of failing. One instance (a command's
  documented arguments vs its wire forms in Nexus.hs validateArgSpecs and
  argsJson) now fails as an internal error. Morloc.Internal already has
  safeZip/safeZipWith/safeZipWithM; sweep the zips over lists whose lengths
  are an invariant to those, failing loudly. [read, 2026-10-29] Candidates
  found: Serialize.hs carriedTypes (loop ids), Express.hs lambdaTypeMap
  (missing types become untyped), EffectBoundary.hs adaptCallbackParams
  (extra parameters go unadapted), HostProvenance.hs `zipWith fit (dataKids
  t) ps` (two sites unguarded, a third guarded).

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
- **Name-prefix tuple tests.** `isPrefixOf "Tuple"` decides tuple-ness in
  Rust.hs rustIsCopy, isVarWidthF and tupleArgs, Desugar.hs
  extractConstraints (two sites) and Typecheck/Internal.hs
  classifyTupleShape. TypeNames.hs reserves only `Tuple<digits>`, so a user
  type `TupleFoo` counts as a tuple. [reproduced 2026-11-01] A class
  named `TupleLike` is read as a tuple of constraints (Desugar.hs
  extractConstraints): /work/plans/error-clusters/repro/tuple-prefix-class.loc fails to
  typecheck, and the same file with the class renamed `PairLike` passes.
  Elsewhere such a type is assumed Copy, sized by schema width,
  projected with `.0`. [reproduced 2026-11-02, UNSOUND] A user type named
  `TupleBox a` (a newtype over `[a]`) accepts `.0`: in Python it returns the
  list's first element with no error; in Rust it fails to build (taken as
  Copy, then the `.0` projection); a record holding one segfaults the Rust
  pool, since isVarWidthF calls it fixed-width and the size step
  undercounts. The Rust sites fire only on an applied type. Repros:
  /work/plans/error-clusters/briefs/A6-A8.md.
- **Intrinsic arguments left unchecked.** Typecheck.hs: the
  checkIntrinsicArgs catch-all `_ -> return g` accepts an intrinsic with no
  listed case, or a listed one at an unlisted arity, without checking its
  arguments. Ownership.hs: an intrinsic
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
- **Closure capture types filtered, then zipped.** Rust.hs and Cpp.hs build
  `ctxTs` by filtering captures with a native type, then zip it with the
  closure table's schemas; one untyped capture misaligns the rest.
- **Parameterize can hang zipping against an infinite list.** [read,
  2026-11-02] Parameterize.hs `safeZipWith Arg ids (map EV (freshVarsAZ
  []))`: `safeZipWith` checks both lengths, so forcing the result never
  terminates. Reached only by a root that is a bare function-typed `BndS`,
  which no tree has today. Use `zipWith`.
