# Nexus command-line parsing

- **`daemon` takes an option's value as the program.** [reproduced,
  0.109.0] Gap in model/runtime/daemon.md (no rule for its command line).
  `morloc-nexus daemon --http-port 18731 ./smiles` fails "Failed to load
  manifest '18731'"; the same for `--port N` and `--socket PATH`.
  `--http-port=18731 ./smiles` and `./smiles --http-port 18731` work, and
  `mcp --http-port N ./smiles` works. [read] `parse_run_or_daemon`
  (morloc-nexus/src/cli.rs:1649) finds the target with
  `ValueTakingFlags::from_nexus()`, the `run` subcommand's value-taking
  flags (cli.rs:1145-1163), which lack the daemon's listener flags; `mcp`
  uses `value_taking_flags_of` (cli.rs:1169). The unit test
  `daemon_listener_options` (cli.rs:2360) passes because it calls clap's
  `parse` directly and never runs the pre-scan.

- **`view --pattern` side defects.** [reproduced, 0.109.0] A failed `view
  -o` leaves an empty output file (`print_result_c` writes after
  `redirect_stdout_to`, dispatch.rs/process.rs). `.(.0;(.1;.0))` does not
  parse: no item says whether a group may hold a group. `.[i]` on a Str is
  accepted by the pattern checker and refused by the walker at run time.

- **`-f voidstar` writes a packet labelled as a shared-memory reference.**
  [reproduced, 0.109.0 a2e21e97+dirty] Gap in language/arguments-and-output.md:
  CLI-5 does not say what `-f voidstar` writes, and CLI-1 does not say
  whether it can be read back. `./nexus -f voidstar -o x mkInts 100000`
  (model/performance/benchmarks/encoding) writes 1600087 bytes holding the
  data inline, but header byte 13 (source, FMT-2) is 0x02, so `./nexus
  lenInts x` refuses it ("carries a shared-memory reference ... only
  self-contained packets can be loaded"). At 1000 elements the source byte
  is 0x00 and it reads back; `-f packet` is correct at every size, 7 bytes
  shorter. [read] dispatch.rs:1476, the `OutputFormat::VoidStar` arm, hands
  the pool's reply packet to `print_morloc_data_packet` unchanged;
  speculative: that function inlines the referenced data but copies the
  original header, so a reply large enough to travel by shared memory keeps
  its RPTR source byte.

- **`@form list` row errors do not say what is wrong.** [reproduced,
  0.109.0] Same gap. The TAB, comma, whitespace passes
  (morloc-runtime/src/cli.rs, `try_list_with_config`) keep only the last
  failure, so every rejected CSV row of a record or tuple reports `form:
  list whitespace fallback: line 1 has 1 whitespace-separated columns;
  expected N`: a non-integer cell (`a,b,1,x,0.5,true`), an extra column, a
  short row on line 2 (still reported as line 1), and a header alike. The
  header hint also fires on a data row whose bad cell is a word
  (`a,b,1,x,...`, `...,nan,...`). A type error that reaches the loader is
  reported against the rebuilt JSON (`expected value at line 1 column
  10`), not the user's row and field. Wanted: row number, field name,
  cell text and expected type. Reproduction:
  test-suite/golden-tests/form-list-record-rows (inputs badint, wide,
  narrow, header; obs.err).

- **UNSOUND: `@form list` loads a header row as data.** [reproduced,
  0.109.0] Spec gap: no rule tells a header from a row. `record Gene =
  Gene { name :: Str, chrom :: Str }`, `--' @form list [Gene] -> [Gene]`
  on `name,chrom\ntp53,chr17\n` returns two records, the first
  `{"name":"name","chrom":"chrom"}`; with the header `chrom,name` the
  values are bound in declaration order, so names and chromosomes swap
  with no error. A header is caught only when a name fails its column's
  type. Headed files now have `@form table` (CLI-13, CLI-14); `@form list`
  still has no rule that tells a header from a row.

- **`@form list` rejects `nan`/`inf` cells for `Real`.** [reproduced,
  0.109.0] Violates the loader's own non-finite rule (json.rs
  `parse_nonfinite` accepts `"nan"`, `"inf"`, `"-inf"`): row `a,b,1,2,nan,true`
  fails because `format_field_as_json` passes non-string cells through
  verbatim and bare `nan` is not JSON.

- **Empty cell for an optional field is undefined.** [reproduced, 0.109.0]
  Spec gap: no rule for absent values in `@form list` rows. Record
  `{label :: Str, n :: ?Int}`: row `b,` is rejected, only `b,null` gives
  null. An empty `Str` cell gives `""`, so `?Str` empty is ambiguous.
  Needs a ruling.

- **`@form list` bool cells accept only `true`/`false`.** [reproduced,
  0.109.0] Spec gap: `True`, `TRUE`, `1` are rejected; whether table
  cells follow JSON spelling needs a ruling.

- **A typo in a directive name on a `@form table` argument only warns.**
  [read] Gap in CLI-13: Desugar.hs appends an unknown key to the
  description with a warning, so `@tabel.header none` is accepted and the
  header-required default applies. Only `@table.*` is reserved. Open:
  should every unknown key be an error on an argument with `@form table`?
- **Argument files are read by extension before their bytes.**
  [reproduced, 2026-10-21] Violates CLI-1. A MessagePack packet copied to
  `p.json` fails "serialization error: JSON parse error"; as `p.txt` it
  works. cli.rs:2483 (`ends_with(".json")`) and :2502 (`.mpk`/`.msgpack`)
  dispatch before the packet and magic checks. A missing `p.mp` gets "is
  not inline JSON ... set `source: file`" (old spelling); a missing
  `p.json` gets "not found". Repro: /work/plans/model-audit-2026-10-21/repro/G/args/a1.loc.
- **`-f packet`/`-f voidstar` of a top-level null or `()` writes
  nothing.** [reproduced, 2026-10-21] Violates CLI-5. `nul x = Null`:
  `-f packet nul 1 | wc -c` is 0 (`-f mpk` writes `c0`; `--keep-null`
  restores it), so a downstream `-` reader cannot tell no value from a
  broken pipe. Deliberate in morloc-nexus dispatch.rs:941-947, to avoid a
  phantom packet after an `@stdout` stream. Either CLI-5 carves out
  streaming commands or the code narrows the condition. Repro:
  /work/plans/model-audit-2026-10-21/repro/G/out/o.loc.
- **Help formats a flag's default two ways.** [reproduced, 2026-10-21]
  `scan -h`: `-i` shows `type: Bool` then `default: false` on its own line;
  `-m` shows `type: Int [default: 0]`.
