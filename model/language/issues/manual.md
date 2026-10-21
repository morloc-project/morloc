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
- types-newtype.asc "Declarations with no body" says a bodiless
  `newtype Map key val` with native forms takes a `Packable` instance;
  NEWT-9 now rejects any Packable instance on a bodiless type (stdlib `Map`
  is `newtype Map a b = [(a, b)]`).
- types-custom-types.asc:30-59 and :178-182 declare a bodiless
  `newtype Map key val` with a Packable instance, which NEWT-9 rejects, so
  the worked example does not build; :30-31 and :137-139 say stdlib
  declares it bodiless (it is `newtype Map a b = [(a, b)]`). With a body
  it builds and prints as shown. [reproduced 2026-10-21]
- types-kinds.asc:628-641 calls `type R = Singleton "x" Int` a category
  error; ALIAS-11 (ruled) allows it, and `g :: Int -> Frame R` typechecks.
- types-kinds.asc prints types with parentheses the compiler no longer
  prints (:147-148, :233, :287-288, :387-388, :430-432, e.g.
  `(Buffer 2 Int) -> ...`), and :289 shows the schema `{age=Int, name=Str}`
  reordered; the compiler prints `{name=Str, age=Int}`.
- types-newtype.asc:502 shows the cycle error without its "unless a
  `data` type cuts the cycle" clause.
- features-infix.asc:156-185: `import root-py` + `import .ops ((|>))`
  fails "Illegal masking of type signatures for '|>'" (MOD-12); :220's
  expected-token list now also has `'<>'`. [reproduced 2026-10-21]
- features-pattern-matching.asc:484-489 says list patterns are not yet
  supported (PAT-7, ruled, supports them); :354 and :454 quote "a literal
  pattern cannot cover its type" (now "a literal or list pattern ..."), and
  :443's transcript omits the reason.
- features-functions.asc:111-118: `concatTo = fold (<>)` with no
  signature now typechecks as `a -> b a -> a` (constraints dropped from
  the printed type); the quoted "No instance found" error does not appear.
- features-where.asc:31-32 says `where` bindings may not be mutually
  recursive; they build and run (issues/typecheck.md).
- features-modules.asc:152-156, :215-226 say dotted imports resolve from
  the entry file's directory; MOD-6 (ruled) resolves from the importing
  module's package root. :74-75 names `~/.local/share/morloc/lib/` as the
  system module location; this install uses
  `/opt/morloc-state/src/morloc/plane/default/`.
- cli-docstrings.asc WARNING transcript quotes "unknown docstring
  directive 'metvar' (recognized: ...)"; the compiler now says "did you
  mean one of: metavar?" (Desugar.hs:666-683). Its attachment table lists
  five places; DOC-1 also attaches to `newtype` and `source` list items.
- features-integers.asc: "at a glance" says `Int` is arbitrary precision
  (NUM-1: native default); the R column and text give `U64` as `numeric`
  exact to 2^53 (it is `integer64`), `[U8]` arrives as `raw` not
  `integer`; the factR transcript shows the wrong R type and stale trace
  frames. [reproduced 2026-10-21]
- features-tables.asc:4-15 says `cbind` does not reject duplicate columns
  unless the result type is written (it does); :609-619 shows a list column
  failing at run time (make now refuses it, and optionals of primitives
  are allowed); :624-630 says a table cannot be piped in (`-` works).
- features-sum-types.asc:396-425 says records or aliases alone never close
  a cycle; a guarded self-recursive alias or record builds (ALIAS-7).
- features-strings.asc NUL section says `size` counts bytes; in Python and
  R it counts code points (issues/values.md).
- features-floats.asc type table calls `Real` language-dependent; NUM-9 is
  open and every language uses a double.
- types-typeclasses.asc:108-118 says the `Addable` module compiles in
  isolation with `import internal`; `typecheck` passes but `make` fails
  "Failed to infer concrete universal type for lang py" (`internal` has no
  Python forms). [reproduced 2026-10-21]
- features-intrinsics.asc "Caveats" states FAIL-6 (a) (a killed pool is
  not catchable) though FAIL-6 is open; the code behaves so.
- test-suite/golden-tests/effect-escapable-py/main.loc:47-50 comment says
  `($)` with an effectful argument is rejected; it builds and runs (EFF-2).
- features-recursion.asc:51-56 says some languages lack tail-call
  optimization; RECUR-2 (ruled) makes TCO morloc's obligation everywhere.
- runs-streaming.asc:30-32 says an IFile element mismatch errors at open
  (it does not, see runtime/issues/ifile-access.md); :24-26 and :119-120
  say `@open` returns an integer handle and `@next` returns `[]` at EOF
  (both return a `Try`); :95-97 and the features-intrinsics.asc `@write`
  row give the level per sub-packet (one level per stream is enforced).
- features-intrinsics.asc:449-450 says `@schema`/`@typeof` do not
  evaluate their argument (they do); its table omits `@eq` and `@le`
  (INTR-7), and `@streamLayout` is reachable and undocumented.
- runs-caching.asc:7-10: see issues/labels.md (cache key per call site).
- runs-logging.asc:78-79: an all-null program-wide `log-template` with
  `log: true` builds and logs nothing (only per-label is rejected); :99-100:
  an unknown placeholder error cites the `.loc` label site, not the YAML.
- features-source.asc:114-117, :132 show pool lines as
  `curry.scale(n0) (n1)`; the pool writes `__morloc_src_curry.scale(...)`.
  Its builtins section omits `source Py ("sum" as bsum)` with no `from`,
  which works. [reproduced 2026-10-21]
- arch-compiler-calls.asc:6-7 promises constant-stack cross-language
  tail calls; RECUR-6 is open. arch-compiler-pipeline.asc's stage list
  omits effect-boundary insertion, suspension lowering, segmentation and
  ownership planning. internals-protocols.asc:36 gives
  `pools/python3/pool.py`; the path is `pools/py/pool.py`. [read 2026-10-21]
- cli-actions.asc:61-66, :299-305 and cli-streaming.asc:293-301 show the
  old Return block (`default: [Hit]`); CLI-11 prints `type:`/`desc:`/
  `actions:`. cli-actions.asc:325 `./ramp -f json 4` fails on a
  single-export program (needs `-f json @ 4`); ~:117 says a `@render`
  handler returns `Str` or `[U8]` (CLI-8 allows more, and its own `rep` is a
  `()` sink); ~:393-394 says an impossible `-f` is refused before the run
  (it is refused after). [reproduced 2026-10-21]
- cli-zones.asc:95-108: `./greet -h` prints `Say hello` (the module
  docstring), not `Greet someone by name`.
- cli-output.asc:76-77 csv error transcript stale; :194-200 pool error
  transcript stale (now `Error (pymorloc.c:4088 in pybinding__mlc_next):`,
  located at `sift.loc:102:1`).
- cli-introspection.asc:43 shows `"name": "arg0"` (it is `"_1"`);
  cli-reference.asc:79 says `@metavar` becomes the property name (it never
  is); cli-reference.asc:4-6 says a misplaced directive is only warned
  (most are errors).
- cli-stdin.asc WARNING and cli-intro.asc:4-5 describe reports/0031 as
  live; it is fixed. deploy-eval.asc:139-140 omits the `<expr>:` location
  prefix.
- runs-compression.asc:104-108 says compression goes multithreaded above
  1 MiB and decompression is single-threaded; frames (ZIP-9) compress and
  decompress in parallel and there is no 1 MiB threshold. :93 says level 1
  is ~2x faster than 2 (data: 1.08-1.56x); :94-95 never says level 3 is
  the level to use (ZIP-8). runs-streaming.asc:257 points to the
  compression page for MORLOC_FRAME_WORKERS, which does not name it;
  MORLOC_WRITE_BEHIND_DEPTH is undocumented. internals-overview.asc says a
  cross-pool call costs "a few microseconds" (XL-1: 22-38 us).
  features-modules.asc gives Python startup as 132 ms (data ~58 ms) and
  calls the 13 ms C++ pool start "crossing". [read 2026-10-21]
