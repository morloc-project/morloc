//! Per-program help decoration for the phase-2 manifest-driven
//! `clap::Command` tree built in [`crate::phase2`].
//!
//! Two cosmetic additions:
//!
//! * `override_usage` carries the position rule for nexus options
//!   (left of the wrapper target / `@` separator) so the printed
//!   Usage line matches the convention enforced by
//!   [`crate::cli::split_run_argv_at_separator`].
//! * `after_long_help` carries a rendered "Nexus Options:" block so
//!   `--help` discloses the phase-1 dispatch options while `-h`
//!   stays terse.
//!
//! Functional parsing is unchanged: nexus options are still consumed
//! only by phase 1, before the wrapper target. This module emits
//! display strings, nothing more.
//!
//! The Nexus Options block is rendered once via clap's own formatter
//! against a throwaway command carrying [`crate::cli::DispatchOptions`],
//! so any future field added to `DispatchOptions` appears in the
//! block automatically.

use std::sync::OnceLock;

use clap::{builder::Styles, Arg, ArgAction, Args, Command as ClapCommand};

use crate::cli::DispatchOptions;
use crate::phase2::leak;

/// All-unset clap styles: no bold, no underline, no color. Used in
/// every phase-2 command and applied recursively to the phase-1
/// command tree so help output is plain text throughout.
pub const PLAIN_STYLES: Styles = Styles::plain();

static BLOCK: OnceLock<&'static str> = OnceLock::new();

/// Pre-rendered "Nexus Options:" section, suitable for
/// `Command::after_long_help`. Memoized.
pub fn nexus_options_block() -> &'static str {
    BLOCK.get_or_init(|| {
        let scratch = ClapCommand::new("nexus")
            .disable_help_flag(true)
            .term_width(80)
            .help_template("{options}");
        let mut scratch = DispatchOptions::augment_args(scratch);
        let rendered = scratch.render_long_help().to_string();
        // `{options}` may or may not emit the default "Options:"
        // heading depending on clap's internals. Strip a leading one
        // if present, then re-emit the body under our own header.
        let body = rendered
            .strip_prefix("Options:\n")
            .unwrap_or(&rendered)
            .trim_start_matches('\n')
            .trim_end();
        let block = format!("Nexus Options:\n{body}");
        Box::leak(block.into_boxed_str())
    })
}

pub fn usage_single_root(prog: &str) -> String {
    format!("{prog} <nexus_options> @ <subcommand_options>")
}

pub fn usage_multi_root(prog: &str) -> String {
    format!("{prog} <nexus_options> <subcommand>")
}

pub fn usage_multi_group(prog: &str, grp: &str) -> String {
    format!("{prog} <nexus_options> {grp} <subcommand>")
}

pub fn usage_multi_sub(prog: &str, grp: Option<&str>, sub: &str) -> String {
    match grp {
        Some(g) => format!("{prog} <nexus_options> {g} {sub} <subcommand_options>"),
        None => format!("{prog} <nexus_options> {sub} <subcommand_options>"),
    }
}

/// Attach the position-rule Usage line and the Nexus Options block to
/// a clap command. Usage shows in both `-h` and `--help`; the block
/// shows only in `--help`.
///
/// `after_long_help` overrides `after_help` in long mode, so any
/// pre-existing `after_help` content (e.g. the `Return:` line and
/// schema block set by `build_command_args`) must be carried into the
/// composed long-help text or it would silently disappear under
/// `--help`.
///
/// Add an explicit `-h, --help` arg under the "General Options"
/// heading. clap's auto-added help row ignores `next_help_heading` and
/// always lands under the default "Options" heading; replacing it with
/// our own gives us a section title we control AND, since args are
/// rendered in arg-added order, ensures "General Options" sorts before
/// any positionals or user-declared flags added later.
pub fn add_general_options(cmd: ClapCommand) -> ClapCommand {
    let help_arg = Arg::new("help")
        .short('h')
        .long("help")
        .action(ArgAction::Help)
        .help("Print help (see more with '--help')")
        .long_help("Print help (see a summary with '-h')")
        .help_heading("General Options");
    cmd.disable_help_flag(true)
        .disable_help_subcommand(true)
        .styles(PLAIN_STYLES)
        .arg(help_arg)
}

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

/// Apply the position-rule Usage line and merge the Nexus Options
/// block into `after_long_help`. `after_long_help` overrides
/// `after_help` in long mode, so any pre-existing `after_help` content
/// (the `Return:` line and schema block set by `build_command_args`)
/// is composed in here too or it would silently disappear under
/// `--help`.
pub fn finalize(cmd: ClapCommand, usage: String) -> ClapCommand {
    let existing_after: String = cmd
        .get_after_help()
        .map(|s| s.to_string())
        .unwrap_or_default();
    let block = nexus_options_block();
    let after_long = if existing_after.is_empty() {
        block.to_string()
    } else {
        format!("{existing_after}\n\n{block}")
    };
    cmd.override_usage(leak(&usage))
        .after_long_help(leak(&after_long))
}
