# Manual text against the spec

The manual (docs repo, `src/content/`) contradicts a spec item or itself.
Each is [read]: verified by reading the manual, not the compiler. Where the
manual describes compiler behavior, the matching entry in another file
says whether it reproduces.

- features-patterns.asc:22-27 teaches "anonymous record types"
  (`pts :: [{x = Int, y = Int}]`); whether they stay depends on the
  decision in records.md.
- Contradicts NATIVE-3 (Python and R need nothing): features-records.asc:33-34,
  161 write `record Py => Person = "dict"` and `record R => Person = "list"`
  in every example. NATIVE-5 is open on whether these lines are legal.
- Stale: features-optionals.asc:290-292 says sum types are "planned but not
  yet supported".
- features-patterns.asc:249 says "Pass `Null` through as `Nothing`"; morloc
  has no `Nothing`.
- features-sum-types.asc:490 and :827 both say Python and R "declare
  nothing" in one foldout.
- Contradicts DOC-6 and SRC-2: features-source.asc:37, :52, :102 use the
  colon forms `--' name:` and `--' rsize:` that cli-docstrings.asc calls a
  compatibility trap.
- SRC-4 open: features-source.asc says backtick names are emitted verbatim,
  any two-argument infix operator, against SRC-1's name check.
- HOST-4 open: features-source.asc says outer parentheses in a sourced
  signature are ignored (only `rsize` counts) while a callback's parentheses
  are honored.
- CLI-4 open: cli-reference.asc:56 documents `@default <json>`;
  cli-parameterization.asc:275 writes `@default dot` for a `data` argument.
- CLI-2 spells shape fields `form: list`, `source: file`; the manual
  (cli-shape.asc:86-98) and the compiler use `@form list`; the compiler's own
  header hint (cli-shape.asc:241) says `form: list`. CLI-2's spelling and the
  hint are stale.
- STRM-3 open: runs-streaming.asc:13 types `IFile a` as the data in the file
  while :43 opens an `IFile [Person]`.
- ANN-6 open: runs-logging.asc:82 says `{lang}` is "the pool language"; its
  example shows `cpp` for a Python function, explained by caller-side timing.
- EVAL-5, EVAL-6 open: features-where.asc:132-135 says a value used in a
  lambda is computed "when the lambda is built", and an unused `let` "still
  runs where it is written" while an unused `where` never does. EVAL-3 places
  a value where its uses meet, which implies neither.
- INTR-6 open: features-intrinsics.asc presents `@lang`, whose value depends
  on the realization, against REAL's interchangeable implementations.
- features-intrinsics.asc: the `@typeof`/`@schema` example exports
  `showSchema` and `showType`; its transcript runs `typeofInt`, `typeofList`,
  `typeofOpt` and `typeofTup`.
- interface-data-transfer.asc:9 says inline is "less than 64 KiB", :52 and
  :58 say `<= 64k`; :17 and :89 call temp files MessagePack `.mpk`.
- cli-intro.asc:320, :337 run `sift total needle ./notes`; `total` takes one
  `Str`.
