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
- Gap: model/language/arguments-and-output.md has no rules for @parse.
- [read] morloc-nexus/src/orchestrate.rs:198 error text omits voidstar and
  raw.
