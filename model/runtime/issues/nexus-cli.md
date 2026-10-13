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
