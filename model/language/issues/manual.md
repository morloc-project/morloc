# Manual text against the spec

The manual (docs repo, `src/content/`) contradicts a spec item or itself,
and the fix needs a ruling or more than a small edit. Where the manual is
right and the compiler is wrong, the entry lives with the code issue, not
here.

Waiting on a ruling:

- features-patterns.asc:22-27 teaches "anonymous record types"
  (`pts :: [{x = Int, y = Int}]`); whether they stay depends on the
  decision in records.md.
- features-records.asc:33-34, 161 write `record Py => Person = "dict"` and
  `record R => Person = "list"` in every example; NATIVE-5 is open, and a
  record literal currently needs them (records.md).
- SRC-4 open: features-source.asc says backtick names are emitted verbatim,
  any two-argument infix operator, against SRC-1's name check.
- CLI-4 open: cli-reference.asc:56 documents `@default <json>`;
  cli-parameterization.asc:275 writes `@default dot` for a `data` argument.
- STRM-3 open: runs-streaming.asc:13 types `IFile a` as the data in the file
  while :43 opens an `IFile [Person]`.
- ANN-6 open: runs-logging.asc:82 says `{lang}` is "the pool language"; its
  example shows `cpp` for a Python function, explained by caller-side timing.
- EVAL-5, EVAL-6 open: features-where.asc:132-135 says a value used in a
  lambda is computed "when the lambda is built", and an unused `let` "still
  runs where it is written" while an unused `where` never does (the code
  agrees; typecheck.md).
- INTR-6 open: features-intrinsics.asc presents `@lang`, whose value depends
  on the realization, against REAL's interchangeable implementations.
- SUM-4 vs ALIAS-7: features-sum-types.asc:396-425 says records or aliases
  alone never close a cycle; a guarded self-recursive alias or record builds
  (records.md).
- BASE-5 open: features-strings.asc NUL section says `size` counts bytes; in
  Python and R it counts code points (values.md).
- NUM-9 open: features-floats.asc type table calls `Real` language-dependent;
  every language uses a double.
- DOC-1/DOC-3: cli-docstrings.asc's attachment table has no `newtype` row; a
  newtype's docstring attaches but reaches nowhere in `-h`, and no item says
  where it should.
- features-effects.asc: `!` outside a do-block and several effect rules are
  documented with no item (effects.md).

Deferred, needs a new example:

- features-functions.asc:111-125 teaches that `concatTo = fold (<>)` needs a
  signature; it now typechecks unsigned (as `a -> b a -> a`, constraints
  dropped from the printed type).
- cli-intro.asc:343-350: the standalone `@epilogue` illustration uses
  `total :: Str -> Str -> Int` and `sift total needle ./notes/today.md`,
  which contradicts sift's real `total`.
- features-integers.asc factR transcript waits on the R overflow message
  naming the wrong R type (values.md).
- features-modules.asc gives Python startup as 132 ms (recorded xl-calls
  data suggests about 58 ms); re-measure before changing.
- features-intrinsics.asc does not document `@streamLayout`, which is
  user-reachable (Namespace/Expr.hs:736); decide whether it is public.
