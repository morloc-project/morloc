//! Per-program help decoration for the phase-2 manifest-driven
//! `clap::Command` tree built in [`crate::phase2`].
//!
//! `override_usage` carries the position rule for nexus options
//! (left of `@` or, in multi-export mode, left of the subcommand
//! name) so the printed Usage line matches the convention enforced
//! by [`crate::cli::split_run_argv_at_separator`]. The nexus options
//! themselves are not repeated here: `-h @` renders them.
//!
//! Functional parsing is unchanged: nexus options are still consumed
//! only by phase 1, in the nexus zone. This module emits display
//! strings, nothing more.

use clap::{builder::Styles, Arg, ArgAction, Command as ClapCommand};

use crate::phase2::leak;

/// All-unset clap styles: no bold, no underline, no color. Used in
/// every phase-2 command and applied recursively to the phase-1
/// command tree so help output is plain text throughout.
pub const PLAIN_STYLES: Styles = Styles::plain();

pub fn usage_single_root(prog: &str) -> String {
    format!("{prog} <nexus_options> @ <command_options>")
}

pub fn usage_multi_root(prog: &str) -> String {
    format!("{prog} <nexus_options> <command> <command_options>")
}

pub fn usage_multi_group(prog: &str, grp: &str) -> String {
    format!("{prog} <nexus_options> {grp} <command> <command_options>")
}

pub fn usage_multi_sub(prog: &str, grp: Option<&str>, sub: &str) -> String {
    match grp {
        Some(g) => format!("{prog} <nexus_options> {g} {sub} <command_options>"),
        None => format!("{prog} <nexus_options> {sub} <command_options>"),
    }
}

/// Add an explicit `-h, --help` arg under the "General Options"
/// heading. clap's auto-added help row ignores `next_help_heading` and
/// always lands under the default "Options" heading; replacing it with
/// our own gives us a section title we control AND, since args are
/// rendered in arg-added order, ensures "General Options" sorts before
/// any positionals or user-declared flags added later.
///
/// Both spellings render the same page. How much the page discloses
/// is decided before clap runs, by how many times the flag is
/// repeated (see [`crate::phase2::help_level`]); clap fires on the
/// first `-h` it meets, so the tree it prints must already have been
/// built at the requested level.
pub fn add_general_options(cmd: ClapCommand) -> ClapCommand {
    let help_arg = Arg::new("help")
        .short('h')
        .long("help")
        .action(ArgAction::HelpShort)
        .help(HELP_FLAG_TEXT)
        .help_heading("General Options");
    cmd.disable_help_flag(true)
        .disable_help_subcommand(true)
        .styles(PLAIN_STYLES)
        .arg(help_arg)
}

/// The help flag's own description: the one place the tiers are
/// advertised, so it names every way to see more.
pub const HELP_FLAG_TEXT: &str =
    "Print help; -hh adds details and examples, -hhh adds schemas\n(nexus options: -h @)";

/// Recursively apply [`PLAIN_STYLES`] to a command and every
/// subcommand. clap does not propagate `styles` down the command
/// tree, so each node must be set explicitly.
pub fn strip_styles_recursively(mut cmd: ClapCommand) -> ClapCommand {
    cmd = cmd.styles(PLAIN_STYLES);
    let names: Vec<String> = cmd
        .get_subcommands()
        .map(|s| s.get_name().to_string())
        .collect();
    for name in names {
        if let Some(sub) = cmd.find_subcommand_mut(&name) {
            let new_sub = strip_styles_recursively(sub.clone());
            *sub = new_sub;
        }
    }
    cmd
}

/// Apply the position-rule Usage line.
pub fn finalize(cmd: ClapCommand, usage: String) -> ClapCommand {
    cmd.override_usage(leak(&usage))
}
