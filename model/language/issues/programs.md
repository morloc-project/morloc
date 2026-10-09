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
