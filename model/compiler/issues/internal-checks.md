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
  t) ps` (two sites unguarded, a third guarded), Valuecheck.hs TupP.

The entries below are fallbacks that turn an impossible shape into wrong
output. Spec gap: no item says an internal invariant fails loudly. Type
changes that remove each class are in /work/plans/gadt/type-audit.md. All
[read, 2026-10-29].

- **Ownership lowering skips its work on a mismatch.** Grammars/Translator/
  Imperative.hs lowers a node from its folded layer plus the original node
  passed separately, and re-pairs them by shape and length. adaptOwnedElems,
  keepElems and storeElems return their input when lengths differ; the
  `elemEs`/`slotTypes` re-matches of origExpr fall to `[]`; argOwnerships
  defaults to `repeat Owned`; the loop-body pass and bindCallArgs pass
  through; several plain zipWith/zipWith3 over owners and arguments. A
  misalignment would show up as a copy or borrow error in C++/Rust, not as
  a compiler error.
- **Name-prefix tuple tests.** `isPrefixOf "Tuple"` decides tuple-ness in
  Rust.hs rustIsCopy, isVarWidthF and tupleArgs, Desugar.hs
  extractConstraints (two sites) and Typecheck/Internal.hs
  classifyTupleShape. TypeNames.hs reserves only `Tuple<digits>`, so a user
  type `TupleFoo` counts as a tuple. [reproduced 2026-11-01] A class
  named `TupleLike` is read as a tuple of constraints (Desugar.hs
  extractConstraints): /work/plans/error-clusters/repro/tuple-prefix-class.loc fails to
  typecheck, and the same file with the class renamed `PairLike` passes.
  Elsewhere such a type is assumed Copy, sized by schema width,
  projected with `.0`. A `TupleFoo` alias typechecks and indexes correctly
  (/work/plans/gadt/repro/idx-TupleFoo.loc); the Rust effects were not
  built.
- **Intrinsic arguments left unchecked.** Typecheck.hs: the
  checkIntrinsicArgs catch-all `_ -> return g` accepts an intrinsic with no
  listed case, or a listed one at an unlisted arity, without checking its
  arguments. Ownership.hs: an intrinsic
  argument pattern `(x : _)` falls through to Nothing.
- **Selector leaf with a missing key is dropped.** Typecheck/Internal.hs
  selectorGetter `maybe [] ...` drops the leaf, so a multi-key selection
  yields a shorter tuple.
- **Valuecheck record comparison.** NamP checks only that the first
  record's keys are in the second, so extra keys pass.
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
- **Futhark tuple input passed unmarshalled.** Guest/Futhark.hs marshalIn
  `(FTTuple Nothing _)` returns the host expression unchanged.
- **Variant merge drops a constructor.** Grammars/Common.hs
  mergeVariantOccurrences takes the longer occurrence's names, so on equal
  lengths a constructor present only in the other occurrence is lost.
