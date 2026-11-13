# @collect, @fold, @parse, @render

- **Cross-language @fold crosses pools with its cell.** Violates STRM-4.
  Cells live in one pool process (morloc-runtime/src/cell.rs) but
  realization can split `@cellnew` and `@cellreduce` across pools (S5 in
  /work/plans/error-clusters). [2026-11-02] Python `produce`, C++ fold
  (`-s`) now prints `c=15`, but only because the handle goes cpp -> py ->
  cpp and returns to the process holding the cell.
  The other direction aborts too [reproduced, 2026-10-29]: a C++
  `produceParallel` calling its sink from four threads, with the `@fold`,
  `@init` and `@combine` functions in Python (or R), kills the C++ pool with
  "mlc_cell_get: no such fold accumulator; the handle is stale or was
  already released", whatever the accumulator type (an `Int` is enough).
- **@render return type unchecked.** [reproduced] Violates CLI-8. A
  handler returning `Int` builds and fails after the command ran ("-f raw
  requires Str, [Str], Vector U8, or [Vector U8]"). `@mime` has the check
  (CodeGenerator/Nexus.hs:325 `mediaSerialValid`); terminals (:3388) do not.
- **@render ignores -f.** [reproduced] Contradicts CLI-5; CLI-9 open.
- STRM-5 open; code merges accumulators in first-fold thread order
  (cell.rs:31).
- Gap: two `@collect`s under `-f json` print two JSON arrays
  (morloc-nexus/src/stdio_server.rs:698).
- Gap: `-o FILE` is a truncating redirect, not write-then-rename like action
  outputs; a failed multi-output run leaves partial output.
  `morloc-nexus view` has the same shape: `view sc.dat -f parquet -o
  x.parquet` on a non-table fails "requires a Table return type" and
  leaves an empty `x.parquet` [reproduced, 0.109.0].
- Gap: model/language/arguments-and-output.md has no rules for @parse.
- [read] morloc-nexus/src/orchestrate.rs:198 error text omits voidstar and
  raw.
- **`--json-help` drops render media types.** [reproduced, 0.109.0] For a
  command with `@render --png=png` where `png :: Molecule -> PNG` and
  `PNG` carries `@mime image/png`, `--json-help` gives each terminal
  `"mime": null` and `"type": {"morloc": "Str"}` (the alias and its
  docstring are lost), while `-h`, `GET /discover` (`renders[].mime`) and
  the MCP tool all carry the media type. The three introspection surfaces
  disagree. Repro: web/morloc-studio/public/assets/home-examples/smiles.
- **`morloc typecheck` lists generated render entries as exports.**
  [reproduced, 0.109.0] A module exporting `draw` with
  `--' @render -o/--out=png` prints `mlcp_draw_out :: Str -> PNG` and
  `mlcr_draw_out :: [U8] -> PNG` after the real exports. Repro:
  web/morloc-studio/public/assets/home-examples/trees.

- **A threaded `@collect` writing to a file aborts the C++ pool.**
  [reproduced 2026-11-02] The shipped collect-fold-cpp program,
  `./nexus pstream --parallel=p.txt`, prints a partly interleaved list, then
  "attach_process_local_slot called on a stdio slot ... missing its stdio
  RPC short-circuit" (stream.rs), SIGABRT, and writes no p.txt. The
  Python-fold variant fails the same way; `-p` to stdout passes. Repro:
  /work/plans/error-clusters/briefs/S5-steps0-2.md.
- **A pure IFile whole-list handler is refused.** [reproduced, 2026-11-13]
  Gap in STRM-4. `sliceFile :: IFile [I64] -> [I64]` as a `@render`
  handler fails to compile with "Cannot force a non-effectful value (got
  type Str)", pointing at the user's `@collect`: the IFile branch of
  wrapWholeCollect (Desugar.hs ~4650) binds the handler with `<-`. A pure
  `[a] -> b` handler is accepted. Workaround: a `do` body.
- **A sink called after the run aborts its pool.** [reproduced,
  2026-11-13] Spec gap: no item says what a late sink call does. A C++
  sink stored and called from `std::atexit` aborts the pool with SIGABRT
  and a backtrace on stderr ("slot index ... out of range; registry not
  initialised or handle is corrupt"); the command still exits 0 with the
  right output. A late call inside the run fails cleanly ("generation
  mismatch ... handle was closed").
