# Nexus command-line parsing

- **`view --pattern` side defects.** [reproduced, 0.109.0] A failed `view
  -o` leaves an empty output file (`print_result_c` writes after
  `redirect_stdout_to`, dispatch.rs/process.rs). `.(.0;(.1;.0))` does not
  parse: no item says whether a group may hold a group. `.[i]` on a Str is
  accepted by the pattern checker and refused by the walker at run time.

- **`-f mpk` output read back as an argument file is sniffed.** [read]
  Headerless MessagePack is told apart from JSON and text only by a `.mpk`
  or `.msgpack` name (cli.rs `load_morloc_data_file`); `./prog f -f mpk >
  x; ./prog g x` reads a one-byte value 48..57 as a JSON digit. Gap in
  CLI-1; MessagePack is slated for deprecation.

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
  `p.json` gets "not found". `@load` shares the loader (INTR-3): `@savej
  "v.mpk" 49` reads back through the MessagePack decoder. The `.mpk` name
  is what keeps a one-byte MessagePack value 48..57 from reading as a JSON
  digit, so bytes-only detection waits on MessagePack's deprecation. Repro: /work/plans/model-audit-2026-10-21/repro/G/args/a1.loc.
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

- **`-f voidstar` of a table writes a shared-memory reference.** [read,
  2026-10-29] `print_morloc_data_packet` (packet_ffi.rs) rewrites an
  RPTR+VOIDSTAR reply into a self-contained packet, but prints an
  RPTR+ARROW one, and a FILE-source one, unchanged; the file names memory
  or a temporary file gone after the run, and reading it back is refused.
  Needs the Arrow IPC bytes written in place of the reference.

- **Output-path defects found reading dispatch.rs.** [read, 2026-10-29]
  - `print_result_c`'s `-f voidstar` branch passes an empty `full_packet`
    (a caller holding only a voidstar) to `print_morloc_data_packet` and
    the hex dump; the `-f packet` branch guards the same case.
  - `run_pure_command` passes `is_arrow = false` whatever the return type,
    so a pool-free command returning a `Table` may print wrongly
    (speculative).
  - `run_remote_command` neither checks nor frees the errmsg
    `morloc_packet_size` sets, and the MessagePack output branch drops its
    errmsg on failure.
- **Stream output can fail after writing part of a packet.** [read,
  2026-10-29] voidstar.rs `normalize_data_packet_to_fd` lets a FILE+ARROW
  or FILE+DATA packet onto the streaming path, whose payload readers refuse
  anything but MessagePack, after the header has been written to the fd.
  `estimate_payload_size` bounds a value by its one block's size; a value
  spread over several blocks can exceed it, and a compressed stream then
  fails "frame index exceeds reserved space" (speculative).
