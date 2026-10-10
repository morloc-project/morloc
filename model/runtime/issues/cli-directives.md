# @collect, @fold, @parse, @render

- **Cross-language @fold fails on stdout.** [reproduced] Violates STRM-4.
  Python `produce :: [Int] -> ([Int] -> <IO> ()) -> <IO> ()`, C++
  `addBatchC`, `mergeC`, `showC`; `--' @render -s/--sum=showC
  @fold=addBatchC @init=zero @combine=mergeC` on `emit xs = @collect
  (produce xs)`. `./fx emit '[1,2,3,4,5]' -s` -> "mlc_cell_count: no such
  fold accumulator", exit 1; `--sum=out.txt` (replay path) gives `c=15`.
  Cells live in one pool process (morloc-runtime/src/cell.rs:68-80) but
  realization can split `@cellnew` and `@cellreduce` across pools.
- **@render return type unchecked.** [reproduced] Violates CLI-8. A
  handler returning `Int` builds and fails after the command ran ("-f raw
  requires Str, [Str], Vector U8, or [Vector U8]"). `@mime` has the check
  (CodeGenerator/Nexus.hs:318 `mediaSerialValid`); terminals (:3388) do not.
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
- **MCP `_render` dispatches a command the pool does not have.**
  [reproduced, 0.109.0] `morloc-nexus mcp ./smiles` lists `_render` enum
  `[raw, mol, pdb, xyz, png, svg]` for `structure`, but
  `tools/call structure {"_1":"CCO","_render":"png"}` returns isError
  "Unknown command: mlcp_structure_png". `raw` works, and the HTTP daemon's
  `/call/structure?render=png` returns `image/png`. Dispatch is
  morloc-nexus/src/mcp.rs:474 (`dispatch_for`). Repro:
  web/morloc-studio/public/assets/home-examples/smiles.
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
