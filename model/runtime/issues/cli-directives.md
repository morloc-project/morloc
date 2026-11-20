# @collect, @fold, @parse, @render

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
- Gap: `morloc-nexus view -o` is a truncating redirect: `view sc.dat -f
  parquet -o x.parquet` on a non-table fails "requires a Table return type"
  and leaves an empty `x.parquet` [reproduced, 0.109.0]. Command runs stage
  `-o` and rename on success (SLOT-40); view writes `-o` itself in some
  modes and keeps the direct redirect.
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
- **A file-route gather leaves its stream open when the producer
  raises.** [read, 2026-11-14] Gap in STRM-4 / SIDE: `_mlc_gather_file`
  (pool.cpp), `gather_file` (rustmorloc), `mlc_gather` (pool.py, pool.R)
  open an OStream on a temp file, call the producer, then close, load and
  unlink; an exception or R error from the producer skips the close and
  the unlink, so the stream's slot and its temp file stay until the
  dispatch's sweep (if any reaches them). The native route drops its entry
  on the same path.
- **A batch that fails to cross to a sink in another pool marks nothing.**
  [read 2026-11-20] STRM-9 deviates. When the `@collect` sink lives in
  another pool, the producer's pool encodes each batch to cross before the
  sink's guarded put runs; an encode failure raises in the producer's pool
  only, so a producer that catches it (or a thread whose excepthook drops
  it) finishes with the batch missing and the command exits 0. Fix
  direction: the crossing closure's caller marks the sink (its handle is
  among the closure's captured arguments) before re-raising. A remote job's
  put is the same case (issues/remote-dispatch.md).
- **A forked child cannot put into a stdio `@collect` sink.** [reproduced
  2026-11-20] Gap in SLOT-16: a Python producer that forks and puts from the
  child is refused ("stdio stream cannot cross a fork boundary"). Since
  STRM-9 the refusal fails the command (golden spec-strm-9-1, inChild),
  where before the batch was dropped and the command exited 0. A gather or
  fold sink takes the side file from a child (SIDE-4); a whole-list or
  streamed stdout sink has no such route. Open question: give stdio sinks a
  child route, or refuse a forking producer at the sink. The refusal's advice
  ("re-open the stream in this process") is impossible for a run's one
  stdout. Analysis: /work/plans/forked-child-stdio-sink-analysis.md.
- **An offset per-batch handler reads @tell and writes in two steps.**
  [read 2026-11-20, DA review] The desugared handler (Desugar.hs ~4796-4815)
  calls `@tell` and then `@write`, so two producer threads can both read
  the same offset before either writes; a forked child's `@tell` returns 0
  (stream.rs ~510). No spec item says what offsets a threaded producer
  sees.
