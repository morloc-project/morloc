# collect-formatters

Polyglot coverage for the `@collect` output formatters (`with` / `with.buffer`
/ `render.buffer`; whole-list `render` lives in `../collect-formatters-render-whole`).

## Layout (stdlib polyglot model)

- `collect/main.loc` — the language-agnostic suite: the sink-taking producers,
  the handler/data **signatures**, and every terminal-action command with its
  `--'` docstrings. Whole-list `IFile` handlers are morloc-native (`@flen`);
  value-transform handlers are signatures sourced per language.
- `main-{cpp,py,r}.loc` — three collapses: `import .collect` + `import
  root-<lang>` + `source <Lang> from impl.<ext>`. Each builds one nexus running
  the identical command suite in that language's pool.
- `impl.{hpp,py,R}` — the per-language producer-data and handler bodies, kept
  byte-for-byte equivalent so all three produce the same output.

## Coverage

- `with` (list, via `@load`) under the full `-f` matrix: json, jsonl, and the
  binary formats voidstar / mpk / packet (`-z 9`), round-tripped through
  `nexus view -f json`.
- `with` (IFile `[a]`, via `@flen`) — constant-memory random access.
- `with.buffer` ± offset, `render.buffer` ± offset.
- String and numeric element streams (`streamStrs` / `streamInts`).
- Cross-cutting: IFile-vs-list agreement, offset continuity across batches
  (0, 3, 6), temp-file cleanup after a call loop, and the `@close`-non-temp
  guard (`badClose`).

## Generating exp files (required once, after building the runtime)

The runtime (`mlc_tmpfile` / `mlc_unlink_tmp` / stream-aware `@load`) must be
built first: `morloc init -f`. Then:

```
make                      # builds all three nexuses, runs the matrix -> obs.txt
# inspect obs.txt carefully; when correct:
cp obs.txt exp.txt
```

## Known-red until wired (proper TDD, per the suite design)

- **R offset** (`streamInts --tag-off`, `--text-off`): the R `_mlc_tell`
  wrapper is deferred, so those invocations fail at runtime in the R pool until
  it lands. `exp.txt` encodes the intended (working) output.
- **Whole-list `render`**: see the sibling `collect-formatters-render-whole`
  directory — currently a compile-time `dfail`, RED until the raw return path
  is wired.
