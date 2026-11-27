//! Manifest-aware `-h` / `--help` for the `daemon` and `mcp` backends.
//!
//! A compiled program's `daemon` / `mcp` launcher execs `morloc-nexus
//! daemon|mcp <manifest.json>`, so without decoration clap prints the
//! generic subcommand help -- naming `morloc-nexus`, listing a
//! `<TARGET>` the user never types, and disclosing nothing about the
//! program's own exported commands.
//!
//! [`decorate`] rewrites that help the same way [`crate::help::finalize`]
//! does for the `run` path: it sets a program-named Usage line, a help
//! template, and an `after_help` block listing the exported commands.
//! clap's own `-h`/`--help` then renders it, so argv tokenizing
//! (bundling, `--help` as a flag value, `--`) stays clap-correct with no
//! hand-scanning.
//!
//! The exported-commands block lists each servable command (the same set
//! [`crate::json_help::servable_commands`] exposes -- i.e. what the MCP
//! tool surface / daemon can marshal) with its one-line description,
//! mirroring the CLI's top-level subcommand list. Per-command schema detail
//! is intentionally omitted here -- a served program already exposes it at
//! runtime (the daemon's `discover` RPC and MCP's `tools/list`, or the
//! static `run --json-help` / `--mcp-tools`).

use clap::{Arg, ArgAction, Command as ClapCommand};

use morloc_manifest::Manifest;

use crate::phase2::{first_desc, leak};

/// Render the "Exported commands:" block: one aligned `name  description`
/// row per servable command, matching how the CLI's multi-export help lists
/// its subcommands.
pub fn commands_block(manifest: &Manifest) -> String {
    let cmds = crate::json_help::servable_commands(manifest);

    let mut out = String::from("Exported commands:\n");
    if cmds.is_empty() {
        out.push_str("  (none)");
        return out;
    }

    let name_width = cmds.iter().map(|c| c.name.len()).max().unwrap_or(0);
    for (i, c) in cmds.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        let desc = first_desc(&c.desc);
        if desc.is_empty() {
            out.push_str(&format!("  {}", c.name));
        } else {
            out.push_str(&format!("  {:width$}  {}", c.name, desc, width = name_width));
        }
    }
    out
}

/// Attach the manifest-aware Usage line, help template, and
/// exported-commands `after_help` block to a `daemon`/`mcp` subcommand.
///
/// `name` is the program's declared name (`manifest.name`), matching how
/// the `run` help names the program. `heading` labels the help flag's
/// options section and must match the `next_help_heading` set on the
/// backend's args struct (`DaemonArgs` / `McpArgs`) so the whole options
/// block renders under one heading.
pub fn decorate(
    cmd: ClapCommand,
    name: &str,
    about: &str,
    heading: &'static str,
    block: String,
) -> ClapCommand {
    let usage = format!("{name} [OPTIONS]");
    // Order: headline -> Usage -> exported commands (after_help) ->
    // options. `{options}` (not `{all-args}`) omits the argv-injected
    // `<target>` positional and renders the real clap options, so the
    // help can never drift from what the parser accepts.
    // clap renders `{after-help}` with its own leading blank line, so a
    // single `\n` after the usage line yields exactly one blank line before
    // the exported-commands block (a `\n\n` here would double it).
    const TEMPLATE: &str = "{about}\n\nUsage: {usage}\n{after-help}\n\n{options}\n";

    cmd.disable_help_flag(true)
        .about(leak(about))
        .override_usage(leak(&usage))
        .help_template(TEMPLATE)
        .after_help(leak(&block))
        .styles(crate::help::PLAIN_STYLES)
        // Re-add the help flag ourselves (the derive's `next_help_heading`
        // does not cover clap's auto help flag) so it lands under the same
        // heading as the other options.
        .arg(
            Arg::new("help")
                .short('h')
                .long("help")
                .action(ArgAction::Help)
                .help("Print help")
                .help_heading(heading),
        )
}
