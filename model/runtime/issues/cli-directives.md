# @collect, @fold, @parse, @render

- **Cross-language @fold crosses pools with its cell.** Violates STRM-4.
  Cells live in one pool process (morloc-runtime/src/cell.rs) but
  realization can split `@cellnew` and `@cellreduce` across pools (S5 in
  /work/plans/error-clusters). A Python producer with a C++ fold works:
  its `@cellsink` is placed in C++, so the handle goes cpp -> py -> cpp
  (golden gather-relay foldPyCpp). The other direction fails
  [reproduced 2026-11-14]: a C++ producer calling its sink from four
  threads, with `@fold`, `@init` and `@combine` in Python: every
  callback's `@cellget` fails with "mlc_cell_get: no such fold
  accumulator" and the run fails. Cause not narrowed; the likely one is
  that the callbacks run in another Python process than the one that made
  the cell (SIDE-11 makes such a miss certain rather than a wrong cell).
- **A fold batch with no wire form cannot come from a forked child.**
  [reproduced, stage 6 prototype] SIDE-10: a batch type with a packer
  (`[Map Str I64]` in C++ or Python) sent from a forked child poisons the
  side file and the fold fails ("no wire form"); the parent's own batches
  fold natively. The child could pack the batch first (golden
  gather-native mapsFold*).
- **A side-file writer's forked grandchild holds its lock.** [read]
  Violates SIDE-10's intent that take waits only for writes in flight. A
  forked child that forks again while it holds the flock on its side-file
  descriptor (a fork inside a sink call, from another thread of the child)
  hands the descriptor, and so the lock, to the grandchild; the owner's
  take blocks until the grandchild exits or closes it. O_CLOEXEC does not
  help across fork. Open question: refuse a fork while the lock is held
  (an atfork check), or time-bound the take's lock wait.
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
- **A replay of a packed element type does not build.** [reproduced,
  2026-11-14] Gap in STRM-4. A streaming command whose batch type is
  `[T]`, `T` a native type with a packer and no serializer of its own
  (golden gather-fold-packed `Tally`), gets a replay entry for its
  `@render` actions (Desugar synthStreamingReplay); its C++ `_mlc_replay<T>`
  instantiates `from_voidstar` on `T` ("no matching function for call to
  'Tally::Tally()'") and its Rust `replay` needs `T: FromVoidstar`, so the
  pool does not compile. The replay reads frames as the native type
  instead of the wire type and packing each element. Workaround: give the
  producer through a lambda, which leaves the command without a replay
  entry.
- **A file-route gather leaves its stream open when the producer
  raises.** [read, 2026-11-14] Gap in STRM-4 / SIDE: `_mlc_gather_file`
  (pool.cpp), `gather_file` (rustmorloc), `mlc_gather` (pool.py, pool.R)
  open an OStream on a temp file, call the producer, then close, load and
  unlink; an exception or R error from the producer skips the close and
  the unlink, so the stream's slot and its temp file stay until the
  dispatch's sweep (if any reaches them). The native route drops its entry
  on the same path.
