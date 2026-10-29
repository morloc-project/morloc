# Commands, arguments and run output

Reproduced on 0.109.0 unless marked.

- **Single-export command name.** [reproduced] CMD-5 open. `greet.loc`,
  `hello :: Str -> Str` the only export: `./greet -f jsonl hello Weena` and
  `./greet -f jsonl Weena` fail "unexpected argument '-f' found"; `./greet -f
  jsonl @ Weena` works. `./greet hello` takes `hello` as the command and
  fails "required arguments were not provided: <_1>" (an internal metavar);
  greeting the string "hello" needs `./greet hello hello`.
- **Argument numbering.** [reproduced] Help numbers positionals from 1, run
  errors from 0 (`failed to parse argument #0: file 'nosuch.json' not
  found`), and build errors from 1: one `Options` record argument is
  `argument #2` at run time and `argument #3` at build time. Gap: no CLI
  item says how arguments are named in diagnostics.
- **`@check.path r` help.** [reproduced] Help says "path to a readable
  file"; a directory is accepted. Either the text or the check is wrong.
- **`@stdin` not shown in help.** [reproduced] Gap in CLI-3: an `@stdin`
  argument prints as a plain required positional in `-h` and `-hh`.
- **`@stdin` on a list.** [reproduced] docs reports/0030: `@form list` with
  `@stdin` on `[Str]` is refused at build ("`@check.path` requires a `Str`
  argument; got wire schema `as`"). CLI-3 says an `@stdin` argument is a
  `Str`; open whether a list form is meant to be allowed. reports/0031 is
  fixed.
- **`@default` messages for `data`.** [reproduced] CLI-4 open. `@default
  dot`, `Dot`, `"Dot"` and `{"Dot":[]}` all work. `@default bogus` and
  `@default circle` (a constructor with a field) both say "default value is
  not valid JSON", though bare names are accepted. `{"Rect":[1.0]}` says
  "Expected: one of Circle, Rect, Dot", not that `Rect` takes two fields.
- **Unlocated build rejections.** [reproduced] Violates DIAG-2. None names
  a file or line: "In m:f, argument #3, field maxCount: optional argument
  -m/--max-count must be given a default value"; "a Bool argument cannot use
  `@arg`"; "In two:f, more than one positional declares `@stdin`".
- **Benchmark and log `{lang}` name the caller.** [reproduced] ANN-6 open.
  `compare2 x = (slow@incr x, fast@triple x)`, Python `incr`, C++ `triple`:
  the log line and the benchmark row both report `incr` as `cpp`.
- **Blank line before `@epilogue` stays in the description.** [reproduced]
  Spec gap: no item says whether trailing blank docstring lines belong to
  the description. `--' Search notes` / `--'` / `--' @epilogue` on a module
  gives `--json-help` `program.description` `["Search notes", ""]`.
- **A dotless import collides with a same-named local module.**
  [reproduced, 2026-10-21] Violates MOD-7. With a local `math.loc`,
  `import math (sqrt)` fails "Ambiguous import 'math' ... Found in system
  ... Found locally: ./math.loc". Module.hs:416 `bareModulePaths` adds
  local paths whenever `stateAllowLocalModules` (default True,
  Namespace/State.hs:1090); `findBareModule` (Module.hs:494-506) throws on
  both. Repro: /work/plans/model-audit-2026-10-21/repro/A/mod/m7.
- **Module rules with no item.** [read, 2026-10-21] Gaps in module-names.md.
  No item covers namespaced imports (`import m as n`, `n.f`, the `f.g`
  composition ambiguity in Lexer.hs:476-498) though `namespace-*` goldens
  exist. MOD-12 (open) rejects one name from two modules even with equal
  signatures, so a user `|>` cannot be imported beside root-py's; the
  ruling should cover the prelude case.
- **Two commands with one name.** [2026-10-21] CMD-4 is open. The build
  now refuses two commands with one name, within or across groups, and an
  ungrouped command named after a group, since the daemon's `/call/<cmd>`,
  MCP tools and the CLI each find a command by name alone (IFACE-1). This
  is CMD-4 (a) widened to the whole program; the other answer, names
  scoped by group, needs the serving interfaces to carry the group.
- **CLI-2 errors are not at the docstring line.** [reproduced, 2026-10-21]
  `@form list|table`, `@form auto`, `@source auto`, `@check.path r|w` are
  reported at the type line; `@form table`/`@form bytes`/`@stdin` on `Int`
  carry no location and use the old `form:` spelling. Repro: /work/plans/model-audit-2026-10-21/repro/G/args/b.loc.
- **`--json-help` epilogues lose trailing spaces.** [reproduced,
  2026-10-21] Violates CLI-12. `--'   indented  ` becomes `"  indented"`.
  Repro: /work/plans/model-audit-2026-10-21/repro/G/cmd/e.loc.
- **`@stdin` and a bare `@arg` above a function are dropped silently.**
  [reproduced, 2026-10-21] Against DIAG-4's common ground (every other
  misplaced argument directive is an error). CodeGenerator/Docstrings.hs:
  313-330 `validateCommandLevelDirectives` lists no `stdin`; a value-less
  `@arg` sets no `docArg`. Repro: /work/plans/model-audit-2026-10-21/repro/G/diag/mis3.loc, dirarg.loc.
- **More unlocated rejections.** [reproduced, 2026-10-21] DIAG-2: "Command
  group names collide with ungrouped command names: [math]"; "Source file
  not found: ./nofile.py"; unknown-directive and `@literal` warnings;
  `morloc eval` allow-list refusal. `morloc eval -e 'import root-py;
  @write "x" 1'` is located at `<expr>:3:1-12`, a generated wrapper line.
- **Program rules with no item.** [reproduced, 2026-10-21]
  - Command names: `@name @` and `@name -x` make a command unreachable,
    `@name a b` holds a space. Candidates: a name matches
    `[a-z][a-z0-9-]*` or is a located error; any name, quoted.
  - Sink handlers: CLI-8 makes a non-`Str`/bytes `@render` handler a
    build error, but the manual's `rep` (cli-actions.asc:150-176) renders
    through `tabulate :: Int -> [(Str, Int)] -> <IO> ()` and builds.
  - When an impossible `-f` is reported: the command runs first, then
    "Error: --format=csv|tsv requires ..." (no `--format` flag exists;
    it is `-f/--output-form`); with `-o q.arrow` an empty file is left.
    IFACE-12 requires limits before the work; the manual says refused
    before the run. Check: dispatch.rs:1681-1690,
    arrow_ipc_reader.rs:316. Repro: /work/plans/model-audit-2026-10-21/repro/G/act/ac.loc.
  - argv `Bool` takes only lower-case JSON; a table cell (CLI-17) takes
    any case.
  - Which docstring heads a single-export program's `-h` (it prints the
    module's; cli-zones.asc:95-108 shows the command's).
  - An empty `.loc` builds a program with no commands.
- **A dotted import of a file that declares several modules fails.**
  [reproduced, 2026-10-23] MOD-1 allows several modules in a file, but
  `import .lib` of a lib.loc declaring `helper` and `lib` loads no module
  named `.lib` (the loader renames a file's module to the import name only
  when the file declares exactly one). It used to loop forever and is now a
  load error naming the declared modules. Needs a rule: does `.lib` select
  the module named `lib` in that file? Related to MOD-5 (open): a file
  reached under two names is loaded as two modules; importing the entry
  file is now rejected as a cycle, but no other identity question is
  settled.
