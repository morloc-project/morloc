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
- **A fold's slots are never reclaimed while it runs.** [read
  2026-11-15] FOLD-1, FOLD-6 say nothing on it. fold.rs keeps one slot per
  ThreadId that ever put, until take; MAX_SLOTS (4096) then fails the put.
  Pool worker threads retire and respawn, so a long fold fed by
  cross-pool callbacks (each batch on whichever worker serves it) can
  reach the cap with few live threads. The message now names both causes.
  Fix direction: let a FREE slot whose thread has ended be merged, by
  combine, into another slot or a carried value (needs combine reachable
  from the runtime, or the binding to do it at checkout), or key slots by
  worker index instead of ThreadId. Model the change in ForkSide.tla
  first (it touches FOLD-2's seal and FOLD-4's count).
- **A sequential producer's fold batches may be combined out of order.**
  [speculative 2026-11-15, DA review] Callbacks from another pool run on any
  idle C++/Rust pool worker, and fold slots are keyed by thread
  (morloc-runtime fold.rs), so a sequential producer in Python feeding a
  native C++ fold can split its batches across slots; take combines in slot
  order, so a non-commutative fold (concatenation) is reordered. STRM-5
  requires commutativity only for threaded or forking producers. Same with
  the former SHM cells. Repro to try: warm a C++ pool with a parallel map,
  then fold 100 string batches from a sequential Python producer. Fix
  direction: checkout takes any FREE slot (re-entry by a per-thread flag);
  also bounds slots under worker turnover. Plan:
  /work/plans/next-session-open-issues.md.
- **A producer that swallows a sink error exits 0 with short output.**
  [read 2026-11-15] A Python thread's default excepthook drops the error
  of a failed put (encode failure, closed stream), and nothing marks the
  stream failed, so the command succeeds with missing batches. Same for a
  remote job. Needs a spec item: a failed write fails its stream.
