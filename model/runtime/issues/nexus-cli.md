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
