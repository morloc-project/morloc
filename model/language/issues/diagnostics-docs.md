# Diagnostics and docstrings

- [reproduced] Violates POLY-6 (the error names the variable). Two
  variables written with one name print as that name twice
  (CodeGenerator/Infer.hs:177-198 `undeterminedError`):
  `ingest loadSource 3 p q` (biolib stream, both signatures say `a`) gives
  "nothing in the program fixes 'a', 'a'", which does not say which terms
  they belong to.
- [read] Violates DOC-3. A `@mime` on an optional or effect alias is
  dropped where the alias is expanded: Restructure.writtenAliasDocs carries
  the alias's other directives into the signature but not its media type,
  because the signature-level check (Docstrings.rejectAuthoredMime) would
  read it as written there.
- [reproduced] A `match` whose branches differ in type (a value beside a
  `do` suspension) is reported as "Branches of guard/if have incompatible
  types" with "then-branch" and "else-branch", constructs the program does
  not contain: `match` reaches Typecheck.hs:1678 through IfS. Repro: in a
  `do` block, `match r | (Err e) = Err e | (Ok xs) = do { f xs; Ok xs }`.
- [reproduced] Violates DIAG-5. A call of a named, non-recursive morloc
  function is beta-reduced into its caller (LambdaEval `reduce`), so it has
  no generated function and no trace line: with `inner x = boom (boom x)`,
  `middle x = inner (boom x)`, `outer x = middle (boom x)` and a C++ `boom`
  that throws past 2, `outer 0` reports only `at outer`. The application is
  named only on some reduction paths, and a name on an index with no
  generated function prints nothing. With the frame stack (STATE-5) an
  inlined named call could push its label around its inlined statements
  and need no function of its own.
- **Conflicting native-form error mislabels its snippets.** [reproduced,
  2026-10-21] Module m: `type Py => T = "int"`; main: `type Py => T =
  "float"`. The header says `main.loc:4:1` above m.loc's line, then "and
  here:" shows main.loc's line twice. Repro: /work/plans/model-audit-2026-10-21/repro/B/t/nat/n6.
- **`-hh` drops the blank line between a docstring's first line and its
  detail.** [reproduced, 2026-10-21] The detail prints directly under the
  first line.
- **REAL-3's rejection names neither the term nor a location.**
  [reproduced, 2026-10-21] Violates REAL-3. `type Foo; type Py => Foo =
  "dict"`, Cpp-sourced `getv :: Foo -> Int`, `g x = getv x` fails "Failed
  to infer concrete universal type for lang cpp for type Foo ... missing a
  language-specific type definition", naming no term. Repro:
  /work/plans/model-audit-2026-10-21/repro/F/real/r3.loc.
  [reproduced, 2026-10-25] When the type has no mapping in any language
  (`Int` with no `type Py => Int`), the error points at the module's export
  list (`module main (f)`, column of `f`) rather than at a call; which node
  is blamed changes with the placement chosen.
- **A record literal in a generic slot gets wrong advice.** [reproduced,
  2026-10-28] Anonymous records are ruled unsupported, but `import root-py;
  lenn :: [a] -> Int; na x = lenn [{ann = "a", x = inc x}]` is refused
  with "No concrete py type for 'Record'... or import a module that
  provides one (e.g. root-py)", naming a module already imported
  (CodeGenerator/Infer.hs:957-963 fallback; C++ names root-cpp the same
  way).
- **An R pool load failure is printed twice.** [reproduced, 2026-10-21]
  A sourced R file assigning `.__morloc` prints "failed to load pool
  sources: cannot change value of locked binding" bare and again after
  "Error: run failed". Repro: /work/plans/model-audit-2026-10-21/repro/F/src/s6R.loc.
- **The compiler dies with an uncaught exception on two inputs.**
  [reproduced, 2026-10-21] Violates DIAG-1 and DIAG-2.
  - A non-UTF-8 source: "morloc: bin.loc: hGetContents: invalid argument
    (cannot decode byte sequence ...)".
  - `import nosuchmod`: auto-install runs `git clone
    https://github.com/morloclib/nosuchmod`, then "morloc: callProcess: git
    ... (exit 128): failed". A typo in an import triggers a network fetch
    and an unlocated process exception.
  Repro: /work/plans/model-audit-2026-10-21/repro/G/diag/bin.loc, nomod.loc, cfg/.
- **Wrong or internal wording.** [reproduced, 2026-10-21] `@render` on an
  argument is reported as `@with` (Frontend/Desugar.hs:1131-1137
  `rejectWithHere`). Repro:
  /work/plans/model-audit-2026-10-21/repro/G/diag/mis2.loc. Open: a duplicate morloc definition (`f :: Int -> Int`,
  `f x = x` twice) now builds and runs with no warning [reproduced
  2026-11-14]; no item says whether two morloc definitions of one term are
  two implementations (REAL-1) or an error.
- **A literal `-0.0` loses its sign in the nexus.** [reproduced,
  2026-10-21] `c = -0.0` is stored as `"value":"0.0"` in the manifest and
  prints `0`; the same value from a CSV cell prints `-0`.
- **A pool error prints runtime-internal detail and the wrong location.**
  [reproduced, 2026-10-21] The cli-output.asc sift example (`total f = do`
  reading a stream) now prints "Error (pymorloc.c:4088 in
  pybinding__mlc_next):" before the message and locates it at the `total f =
  do` line rather than the `@next` call (DIAG-3).
- [reproduced, 0.109.0] Violates DIAG-5 under `--debug`: named calls
  reduced by LambdaEval (`viaHelper n = idpy (helper n)`) have no frame
  (the same loss as the default-build entry above). Cause class: a
  manifold's label lives in hand-maintained `Int`-keyed side maps
  (`stateName`, `stateSourceMap`, `stateNameSource`, `stateDefinitionSite`,
  `stateDebugCallee`) that each minting site updates partially; carrying
  the callee on the manifold node would close it.
- [read] Violates DIAG-5 under `--debug` in Rust: `lcDebugWrap` is the
  identity (CodeGenerator/Pools/CAbi/Members/Rust.hs, `lcDebugWrap = \_ _
  body -> return body`), so a Rust pool dumps no arguments; its frame
  labels use the debug lookup and push an empty name for every manifold.
- [speculative] Under `--debug`, frames are drained only at the dispatcher
  (py pool.py, cpp pool.cpp, rmorloc.c) and flushed when a dispatch starts:
  a failure caught by `@try` leaves its frames and spends the dump budget,
  so a later uncaught failure in the same dispatch may print stale frames.
  The debug wrap re-raises unchanged, so a `@try` `Err` message in a
  `--debug` build may carry no trace (DIAG-5 requires one). Frames, the
  dump count and the overflow note are per thread, so a catch on a helper
  thread (C++ parallel stdlib) is never drained or reset. Needs repros.
- **An unbound variable in a newtype's representation is reported as a
  mismatch.** [reproduced, 2026-10-23] `newtype Bag = [a]` reports
  `expected: [a] inferred: [a] Cannot compare types a and a@q0` instead of
  naming the unbound `a`.
- **Docstrings inside a function-type alias are dropped.** [reproduced,
  2026-10-25] Violates DOC-3. In the stdlib `stream` module, `type Reader a
  = --' Input file / --' @check.path r / Str -> ...` (and `Writer`) attach
  their argument docstrings to a type inside an alias body, which no reader
  looks up, so the `@check.path` never reaches a signature that writes
  `Reader`. Building `stream` itself warns that these docstrings document
  nothing (stream/main.loc:58 and :64); importers are not warned. Same
  class as the COST-13 point on directives inside structural aliases.
- **A where-local or instance-body definition's docstring replaces a
  top-level term's.** [read, 2026-10-25] Violates DOC-1. Local
  definitions go through desugarTopLevel, so captureDeclDocs files their
  docstring in dsTermDocs under the bare name (Desugar.hs captureDeclDocs,
  `Map.insert`), overwriting a documented top-level term of the same name;
  help and `@with` lookups for that term then show the local docstring.
  Needs a repro.
- **A DIAG-5 trace lists a function passed as a value twice per call.**
  [reproduced 2026-11-02] `safe down 0` makes one call of the named
  recursive `down` and prints two `at down` lines: the closure wrapper
  manifold takes the named function's trace label (traceInfo). [read
  2026-11-04] The entry wrappers that decode a remote call's arguments
  (`__mm_4134(s3): ... __mm_4766(n3)`) take it too, so a call from another
  pool lists the function twice. Every language labels by the same
  traceInfo; label only the manifold that is the function's body.
- **An intrinsic's diagnostics underline the wrong span.** [reproduced
  2026-11-04] Violates DIAG-2. The saturated part of an over-applied
  intrinsic (INTR-9) takes the whole application's span, so an error in it
  also underlines the extra arguments; a bare intrinsic eta-expanded with
  no arguments (`f = @throw`) puts one caret under `@`. Desugar.hs
  `etaExpandIntrinsic` has only the expression's span.

- **Renamed type variables named like an English word keep their
  internal names.** [read, 2026-11-08, c3a7b3bb3] library/Morloc/Monad.hs
  `sourceNames` (~300-312) writes `a@q0` as `a` only when no plain word
  `a` appears in the message. The article "a" is such a word, so `a`, the
  most common variable name, is almost never shown by its source name.
  Plain words should be matched only where they could be read as the
  variable (type positions), not against all of the prose.
- **A C++ type with no concrete mapping is rejected with no location.**
  [read, 2026-11-25] Violates DIAG-2. library/Morloc/CodeGenerator/Pools/
  CAbi/Members/Cpp.hs:491-508 `leakError` records a pool error, thrown as
  "C++ pool: Recursive type alias `Vector` has no concrete C++ mapping
  ..." with no source location (it no longer calls `error`, since
  8d05dc60c). Reached by the nested-Vector source in values.md
  (`makeN :: Int -> Vector n (Vector 2 Real)` from C++).
- **A user primitive that crosses languages is refused with an internal
  message.** [reproduced, 2026-11-25] DIAG-1, NEWT-5. `type Foo` with Py
  and Cpp forms, `mk :: Int -> Foo` from Python and `un :: Foo -> Int`
  from C++, `f n = un (mk n)`: the build fails "Cannot find constructor in
  VarF "Foo"  finalType=Foo" at the call. The refusal is right (NEWT-5);
  the message should say that `Foo` has no morloc form and cannot cross.
  The same program with both functions from C++ builds and runs.
