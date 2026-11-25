# Manual text against the spec

The manual (docs repo, `src/content/`) contradicts a spec item or itself,
and the fix needs a ruling or more than a small edit. Where the manual is
right and the compiler is wrong, the entry lives with the code issue, not
here.

Waiting on a ruling:

- features-patterns.asc:22-27 teaches "anonymous record types"
  (`pts :: [{x = Int, y = Int}]`); whether they stay depends on the
  decision in records.md.
- SRC-4 open: features-source.asc says backtick names are emitted verbatim,
  any two-argument infix operator, against SRC-1's name check.
- CLI-4 open: cli-reference.asc:56 documents `@default <json>`;
  cli-parameterization.asc:278 writes `@default dot` for a `data` argument.
- ANN-6 open: runs-logging.asc:89 says `{lang}` is "the pool language"; its
  example (:398) shows `cpp` for a Python function, explained by caller-side timing.
- EVAL-5, EVAL-6 open: features-where.asc:132-135 says a value used in a
  lambda is computed "when the lambda is built", and an unused `let` "still
  runs where it is written" while an unused `where` never does (the code
  agrees; typecheck.md).
- INTR-6 open: features-intrinsics.asc presents `@lang`, whose value depends
  on the realization, against REAL-1, under which merged implementations may substitute for each other.
- SUM-4: features-sum-types.asc says records alone never close a cycle; a
  guarded self-recursive record builds (records.md).
- BASE-5 (ruled 2026-11-08: `size` counts characters): features-strings.asc:193
  still says "Five bytes". No longer waits on a ruling; the wording waits on
  the C++/Rust `size` fix (values.md).
- NUM-9 open: features-floats.asc type table calls `Real` language-dependent;
  every language uses a double.
- DOC-1/DOC-3: cli-docstrings.asc's attachment table has no `newtype` row; a
  newtype's docstring attaches but reaches nowhere in `-h`, and no item says
  where it should.
- features-effects.asc: `!` outside a do-block and several effect rules are
  documented with no item (effects.md).

Deferred, needs a new example:

- features-integers.asc factR transcript waits on the R overflow message
  naming the wrong R type (values.md).
- features-intrinsics.asc does not document `@streamLayout`, which is
  user-reachable (Namespace/Expr.hs:558, :749); decide whether it is public.
