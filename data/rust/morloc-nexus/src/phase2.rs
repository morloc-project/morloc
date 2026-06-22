//! Manifest-driven CLI parse for the trailing argv slice of a
//! `morloc-nexus run` (or `daemon`) invocation.
//!
//! The top-level parse in [`crate::cli`] consumes the mode and the
//! always-on flags. This module walks the manifest's `commands`
//! list and synthesizes a per-program `clap::Command` whose
//! subcommands and arguments mirror the user's morloc
//! declarations. Running the trailing argv through that command
//! gives:
//!
//! * Auto-generated help that matches the rest of clap's output.
//! * Unknown-flag rejection (no more silent typos).
//! * Per-subcommand `--help` for free.
//! * Late-binding of nexus-level flags the user wrote after the
//!   wrapper target (e.g. `./prog --log-dir X cmd`), which would
//!   otherwise be swallowed by the top-level `trailing_var_arg`.
//!
//! The output is a `(cmd_index, Vec<ArgValue>)` tuple consumed by
//! [`crate::dispatch::dispatch_command_parsed`].

use clap::{Arg as ClapArg, ArgAction, ArgMatches, Command as ClapCommand};

use crate::dispatch::{apply_checks, quoted, substitute_stdio_dash, ArgValue};
use morloc_manifest::{Arg as ManifestArg, Command as ManifestCommand, Manifest};

/// Leak a string into a `&'static str` for clap's static-only
/// builder API. The nexus is a short-lived dispatcher (one
/// invocation per command), so the per-arg leaks bounded by the
/// process lifetime are acceptable. Daemons reuse their built clap
/// commands across requests instead of rebuilding, so the leak there
/// is also bounded.
pub(crate) fn leak(s: &str) -> &'static str {
    Box::leak(s.to_string().into_boxed_str())
}

/// Result of parsing a phase-2 invocation.
pub struct ParsedCommand {
    /// Index into [`Manifest::commands`] of the dispatched command.
    pub cmd_index: usize,
    /// Per-arg parsed values, index-aligned with `cmd.args`.
    pub values: Vec<ArgValue>,
}

/// Parse the trailing `rest` from a `Run` mode invocation against the
/// manifest. Builds a root `clap::Command` whose structure matches
/// the manifest's command + group layout, runs clap on `rest`, then
/// projects the resulting [`ArgMatches`] back onto the manifest arg
/// list to produce [`ArgValue`]s.
///
/// On clap parse error (unknown flag, missing positional, bad
/// `-h`/`--help` path), clap prints its diagnostic and the process
/// exits with the clap-recommended status code.
pub fn parse_run(
    manifest: &Manifest,
    user_zone: &[String],
    prog_name: &str,
) -> ParsedCommand {
    let root = build_root(manifest, prog_name);
    let single = manifest.commands.len() == 1 && manifest.groups.is_empty();

    // In single-command mode the user may optionally include the
    // command name as a positional (so both `./prog 5 7` and
    // `./prog mycmd 5 7` dispatch the same way). Strip the prefix
    // when present so clap's positional binding sees only the
    // function arguments.
    let trimmed: Vec<String> = if single
        && !user_zone.is_empty()
        && user_zone[0] == manifest.commands[0].name
    {
        user_zone[1..].to_vec()
    } else {
        user_zone.to_vec()
    };

    let mut argv: Vec<String> = Vec::with_capacity(1 + trimmed.len());
    argv.push(prog_name.to_string());
    argv.extend(trimmed);

    let matches = match root.try_get_matches_from(argv) {
        Ok(m) => m,
        Err(e) => e.exit(),
    };

    if single {
        let cmd = &manifest.commands[0];
        let values = extract_values(cmd, &matches);
        return ParsedCommand { cmd_index: 0, values };
    }

    // Walk down through the optional group layer to reach the
    // command-level matches.
    let (cmd_name, sub_matches) =
        matches.subcommand().expect("clap guarantees subcommand");
    let (chosen_name, chosen_matches) =
        if manifest.groups.iter().any(|g| g.name == cmd_name) {
            // Two-level: <group> <cmd>
            let inner = sub_matches
                .subcommand()
                .expect("clap guarantees inner subcommand");
            (inner.0.to_string(), inner.1)
        } else {
            (cmd_name.to_string(), sub_matches)
        };

    let cmd_index = manifest
        .commands
        .iter()
        .position(|c| c.name == chosen_name)
        .expect("clap-chosen command must be in manifest");
    let cmd = &manifest.commands[cmd_index];
    let values = extract_values(cmd, chosen_matches);
    ParsedCommand { cmd_index, values }
}

/// Build the root `clap::Command` for a Run-mode invocation. In
/// single-command mode, the only command's args live directly on the
/// root (so users can type `./prog 5 7` not `./prog mycmd 5 7`); in
/// multi-command mode, each manifest [`Command`] becomes a subcommand
/// (nested under a group-subcommand layer when the manifest declares
/// `groups`).
///
/// Exposed publicly so the help renderer in [`crate::help`] can build
/// the same command tree to render help text without invoking
/// parsing.
pub fn build_root(manifest: &Manifest, prog_name: &str) -> ClapCommand {
    let single = manifest.commands.len() == 1 && manifest.groups.is_empty();

    if single {
        let cmd = &manifest.commands[0];
        // add_general_options runs first so the "General Options"
        // section sorts before the command's positional/optional args
        // (clap orders sections by first-arg-added).
        let root = crate::help::add_general_options(ClapCommand::new(leak(prog_name)));
        let root = build_command_args(root, cmd)
            .about(leak(first_desc(&cmd.desc)))
            .arg_required_else_help(false);
        return crate::help::finalize(root, crate::help::usage_single_root(prog_name));
    }

    let mut root = ClapCommand::new(leak(prog_name))
        .subcommand_required(true)
        .arg_required_else_help(true);
    root = crate::help::add_general_options(root);
    if let Some(first) = manifest.desc.first() {
        root = root.about(leak(first));
    }
    root = crate::help::finalize(root, crate::help::usage_multi_root(prog_name));

    // Each CmdGroup becomes its own clap subcommand; its members
    // (commands carrying that group) become subcommands beneath it.
    for grp in &manifest.groups {
        let mut grp_cmd = ClapCommand::new(leak(&grp.name))
            .subcommand_required(true)
            .arg_required_else_help(true);
        grp_cmd = crate::help::add_general_options(grp_cmd);
        if let Some(first) = grp.desc.first() {
            grp_cmd = grp_cmd.about(leak(first));
        }
        grp_cmd = crate::help::finalize(
            grp_cmd,
            crate::help::usage_multi_group(prog_name, &grp.name),
        );
        for cmd in &manifest.commands {
            if cmd.group.as_deref() == Some(grp.name.as_str()) {
                let sub = crate::help::add_general_options(
                    ClapCommand::new(leak(&cmd.name))
                        .about(leak(first_desc(&cmd.desc))),
                );
                let sub = build_command_args(sub, cmd);
                let sub = crate::help::finalize(
                    sub,
                    crate::help::usage_multi_sub(
                        prog_name,
                        Some(&grp.name),
                        &cmd.name,
                    ),
                );
                grp_cmd = grp_cmd.subcommand(sub);
            }
        }
        root = root.subcommand(grp_cmd);
    }
    for cmd in &manifest.commands {
        if cmd.group.is_none() {
            let sub = crate::help::add_general_options(
                ClapCommand::new(leak(&cmd.name))
                    .about(leak(first_desc(&cmd.desc))),
            );
            let sub = build_command_args(sub, cmd);
            let sub = crate::help::finalize(
                sub,
                crate::help::usage_multi_sub(prog_name, None, &cmd.name),
            );
            root = root.subcommand(sub);
        }
    }
    root
}


/// Add every manifest [`Arg`] to a [`ClapCommand`] under stable ids
/// (`arg<index>` plus `<arg_id>_neg` for flag negations and
/// `<arg_id>_grpopt`/`<arg_id>_entry<j>` for groups). Returns the
/// command with all args attached.
///
/// Sets `allow_negative_numbers(true)` so positional and option
/// values like `-5` or `-3.14` are accepted as values rather than
/// rejected as unknown short flags. POSIX short flags begin with
/// a letter; the compiler is expected to reject digit-prefixed
/// short flag declarations in user docstrings, making this
/// unambiguous.
fn build_command_args(mut cmd: ClapCommand, mcmd: &ManifestCommand) -> ClapCommand {
    cmd = cmd
        .allow_negative_numbers(true)
        // User-declared flag-style args inherit this heading;
        // positional args are hidden from clap's auto-render below
        // and re-emitted by `render_positional_block` under a
        // "Positional arguments:" header in `after_help`.
        .next_help_heading("Optional arguments");
    // Long help: concatenate every desc line so clap renders the
    // full block under `<sub> --help`.
    if !mcmd.desc.is_empty() {
        cmd = cmd.long_about(leak(&mcmd.desc.join("\n")));
    }

    // Record / Table Schemas block: the schema-walking renderer
    // reconstructs named-type layouts from each typed arg's schema
    // string. Goes into clap's `after_help` so the block sits
    // beneath the args list under `<sub> --help`. The Return line
    // also lives here so it stays attached to the command (clap has
    // no first-class slot for return-type metadata).
    let mut after = String::new();
    let pos_block = render_positional_block(mcmd);
    if !pos_block.is_empty() {
        after.push_str(&pos_block);
    }
    if !mcmd.ret.type_desc.is_empty() {
        if !after.is_empty() {
            after.push_str("\n\n");
        }
        after.push_str(&format!("Return: {}", mcmd.ret.type_desc));
        for line in &mcmd.ret.desc {
            after.push_str(&format!("\n  {}", line));
        }
    }
    if let Some(block) = crate::schemas::render_command_schemas(mcmd) {
        if !after.is_empty() {
            after.push_str("\n\n");
        }
        after.push_str(&block);
    }
    if !after.is_empty() {
        cmd = cmd.after_help(leak(&after));
    }

    // Positional indices are 1-based in clap; track them across args.
    let mut pos_idx: u8 = 1;
    for (i, marg) in mcmd.args.iter().enumerate() {
        let id: &'static str = leak(&format!("arg{}", i));
        match marg {
            ManifestArg::Positional { many, .. } => {
                // Positionals are rendered by `render_positional_block`
                // (above, via `after_help`) so clap's bracketed
                // `<argN>` default doesn't appear in help. They still
                // need `required`/`index` here for clap to parse them.
                let mut a = ClapArg::new(id)
                    .required(true)
                    .index(pos_idx as usize)
                    .hide(true);
                // Variadic positional: accept one or more tokens.
                // The compiler guarantees a `many` positional is the
                // last positional, which is clap's requirement too.
                if *many {
                    a = a.num_args(1..).action(ArgAction::Append);
                }
                cmd = cmd.arg(a);
                pos_idx = pos_idx.saturating_add(1);
            }
            ManifestArg::Optional {
                long_opt,
                short_opt,
                metavar,
                default_val,
                type_desc,
                desc,
                many,
                format,
                ..
            } => {
                let mut a = ClapArg::new(id).action(ArgAction::Set);
                if let Some(l) = long_opt {
                    a = a.long(leak(l));
                }
                if let Some(s) = short_opt {
                    if let Some(c) = s.chars().next() {
                        a = a.short(c);
                    }
                }
                if let Some(m) = metavar {
                    a = a.value_name(leak(m));
                }
                if let Some(d) = default_val {
                    a = a.default_value(leak(d));
                }
                if *many {
                    // Variadic option: collect every value supplied
                    // across the option's occurrences AND every value
                    // passed after a single occurrence (`--xs 1 2 3`).
                    a = a.num_args(1..).action(ArgAction::Append);
                }
                a = a.help(leak(&render_arg_help(desc, type_desc.as_deref(), None, format.as_deref())));
                cmd = cmd.arg(a);
            }
            ManifestArg::Flag {
                long_opt,
                short_opt,
                long_rev,
                default_val,
                desc,
                ..
            } => {
                // Forward (--flag) sets true; reverse (--no-flag)
                // sets false. They share the manifest slot id so
                // either spelling lands in the same place at
                // extraction time. Mutually overrides each other so
                // clap takes the last one on the command line.
                let mut fwd = ClapArg::new(id).action(ArgAction::SetTrue);
                if let Some(l) = long_opt {
                    fwd = fwd.long(leak(l));
                }
                if let Some(s) = short_opt {
                    if let Some(c) = s.chars().next() {
                        fwd = fwd.short(c);
                    }
                }
                // Flags have no value type, but a declared default
                // (`--' default: true`) is still useful context.
                fwd = fwd.help(leak(&render_arg_help(
                    desc,
                    Some("Bool"),
                    default_val.as_deref(),
                    None,
                )));
                cmd = cmd.arg(fwd);

                if let Some(rev) = long_rev {
                    let neg_id: &'static str = leak(&format!("{}_neg", id));
                    let neg = ClapArg::new(neg_id)
                        .long(leak(rev))
                        .action(ArgAction::SetTrue)
                        .overrides_with(id)
                        .hide(true);
                    cmd = cmd.arg(neg);
                }
            }
            ManifestArg::Group {
                group_opt,
                entries,
                metavar,
                type_desc,
                desc,
                ..
            } => {
                // The optional whole-record JSON option.
                if let Some(go) = group_opt {
                    let opt_id: &'static str = leak(&format!("{}_grpopt", id));
                    let mut a = ClapArg::new(opt_id).action(ArgAction::Set);
                    if let Some(l) = &go.long_opt {
                        a = a.long(leak(l));
                    }
                    if let Some(s) = &go.short_opt {
                        if let Some(c) = s.chars().next() {
                            a = a.short(c);
                        }
                    }
                    if let Some(m) = metavar {
                        a = a.value_name(leak(&format!("{}_JSON", m)));
                    } else {
                        a = a.value_name("JSON");
                    }
                    a = a.help(leak(&render_arg_help(
                        desc,
                        type_desc.as_deref(),
                        None,
                        None,
                    )));
                    cmd = cmd.arg(a);
                }
                // Per-field entries.
                for (j, entry) in entries.iter().enumerate() {
                    let eid: &'static str = leak(&format!("{}_entry{}", id, j));
                    cmd = add_group_entry_arg(cmd, eid, &entry.arg);
                }
            }
        }
    }
    cmd
}

/// Add a clap arg representing one group entry (which is itself an
/// Optional or Flag manifest [`Arg`]).
fn add_group_entry_arg(mut cmd: ClapCommand, id: &'static str, marg: &ManifestArg) -> ClapCommand {
    match marg {
        ManifestArg::Optional {
            long_opt,
            short_opt,
            metavar,
            default_val,
            type_desc,
            desc,
            format,
            ..
        } => {
            let mut a = ClapArg::new(id).action(ArgAction::Set);
            if let Some(l) = long_opt {
                a = a.long(leak(l));
            }
            if let Some(s) = short_opt {
                if let Some(c) = s.chars().next() {
                    a = a.short(c);
                }
            }
            if let Some(m) = metavar {
                a = a.value_name(leak(m));
            }
            if let Some(d) = default_val {
                a = a.default_value(leak(d));
            }
            a = a.help(leak(&render_arg_help(desc, type_desc.as_deref(), None, format.as_deref())));
            cmd = cmd.arg(a);
        }
        ManifestArg::Flag {
            long_opt,
            short_opt,
            long_rev,
            default_val,
            desc,
            ..
        } => {
            let mut fwd = ClapArg::new(id).action(ArgAction::SetTrue);
            if let Some(l) = long_opt {
                fwd = fwd.long(leak(l));
            }
            if let Some(s) = short_opt {
                if let Some(c) = s.chars().next() {
                    fwd = fwd.short(c);
                }
            }
            fwd = fwd.help(leak(&render_arg_help(
                desc,
                Some("Bool"),
                default_val.as_deref(),
                None,
            )));
            cmd = cmd.arg(fwd);
            if let Some(rev) = long_rev {
                let neg_id: &'static str = leak(&format!("{}_neg", id));
                let neg = ClapArg::new(neg_id)
                    .long(leak(rev))
                    .action(ArgAction::SetTrue)
                    .overrides_with(id)
                    .hide(true);
                cmd = cmd.arg(neg);
            }
        }
        _ => {
            // Manifest schema guarantees group entries are Optional
            // or Flag; anything else is a compiler bug.
            eprintln!(
                "Internal error: group entry of unexpected kind ({:?})",
                std::mem::discriminant(marg)
            );
            crate::process::clean_exit(1);
        }
    }
    cmd
}

/// Project an [`ArgMatches`] for the chosen command back onto the
/// manifest arg list, producing one [`ArgValue`] per manifest slot in
/// declaration order.
fn extract_values(cmd: &ManifestCommand, matches: &ArgMatches) -> Vec<ArgValue> {
    let mut out = Vec::with_capacity(cmd.args.len());
    for (i, marg) in cmd.args.iter().enumerate() {
        let id = format!("arg{}", i);
        match marg {
            ManifestArg::Positional { quoted: q, many, checks, .. } => {
                if *many {
                    // Variadic positional: clap collects 1..N tokens
                    // via Append action; pull them all and forward as
                    // ArgValue::Many for list-packet assembly in
                    // dispatch. clap's `required(true)` guarantees at
                    // least one token, so an empty vec here is a clap
                    // bug.
                    let toks: Vec<String> = matches
                        .get_many::<String>(&id)
                        .map(|it| it.cloned().collect())
                        .unwrap_or_default();
                    out.push(ArgValue::Many { tokens: toks, literal: *q });
                } else {
                    // Required positional: clap guaranteed a value.
                    let val = matches
                        .get_one::<String>(&id)
                        .cloned()
                        .expect("clap-required positional must have a value");
                    // Unix `-` stdin/stdout shorthand: rewrite to the
                    // `/dev/std{in,out}` path so the pool's fopen
                    // works without any user-side dash handling.
                    let val = substitute_stdio_dash(&val, checks).unwrap_or(val);
                    // Run value-invariant checks on the (possibly
                    // substituted) argv before any wrap.
                    if let Err(e) = apply_checks(&val, checks) {
                        crate::runlog::die_with_error(
                            &format!("argument #{}: {}", i, e));
                    }
                    let v = if *q { quoted(&val) } else { val };
                    out.push(ArgValue::Value(v));
                }
            }
            ManifestArg::Optional {
                quoted: q,
                default_val,
                many,
                checks,
                ..
            } => {
                // Distinguish "user typed the flag" from "clap
                // supplied the default" so manifest-stored defaults
                // pass through verbatim. Defaults for `literal: true`
                // strings are already JSON-quoted in the manifest
                // (e.g. the directive `default: "yolo"` produces the
                // 6-character text `"yolo"`); routing them through
                // `quoted()` again would double-encode and the pool
                // would receive `"yolo"` (with quotes) instead of
                // `yolo`.
                use clap::parser::ValueSource;
                let from_cli = matches.value_source(&id)
                    == Some(ValueSource::CommandLine);
                if *many {
                    // Variadic option: collect all CLI tokens (if any)
                    // and forward as ArgValue::Many. If the user did
                    // not pass the option at all, fall back to the
                    // compiler-supplied default (typically `"[]"`).
                    if from_cli {
                        let toks: Vec<String> = matches
                            .get_many::<String>(&id)
                            .map(|it| it.cloned().collect())
                            .unwrap_or_default();
                        out.push(ArgValue::Many { tokens: toks, literal: *q });
                    } else if let Some(def) = default_val {
                        out.push(ArgValue::Value(def.clone()));
                    } else {
                        out.push(ArgValue::Null);
                    }
                } else if from_cli {
                    let v = matches
                        .get_one::<String>(&id)
                        .cloned()
                        .expect("CLI source guarantees a value");
                    // Unix `-` stdin/stdout shorthand: rewrite to the
                    // `/dev/std{in,out}` path so the pool's fopen
                    // works without any user-side dash handling.
                    let v = substitute_stdio_dash(&v, checks).unwrap_or(v);
                    if let Err(e) = apply_checks(&v, checks) {
                        crate::runlog::die_with_error(
                            &format!("argument #{}: {}", i, e));
                    }
                    let v = if *q { quoted(&v) } else { v };
                    out.push(ArgValue::Value(v));
                } else if let Some(def) = default_val {
                    out.push(ArgValue::Value(def.clone()));
                } else {
                    out.push(ArgValue::Null);
                }
            }
            ManifestArg::Flag {
                default_val,
                long_rev,
                ..
            } => {
                let fwd_set = matches.get_flag(&id);
                let neg_id = format!("{}_neg", id);
                let neg_set = long_rev.is_some() && matches.get_flag(&neg_id);
                // Clap's `overrides_with` ensures only one of the two
                // ends up true if both spellings were given (last
                // wins). Recover the original "true"/"false" string
                // shape the dispatch layer expects.
                let bool_val = if fwd_set {
                    true
                } else if neg_set {
                    false
                } else {
                    default_val
                        .as_deref()
                        .map(|s| matches!(s, "true" | "1"))
                        .unwrap_or(false)
                };
                out.push(ArgValue::Value(
                    if bool_val { "true" } else { "false" }.to_string(),
                ));
            }
            ManifestArg::Group {
                group_opt,
                entries,
                ..
            } => {
                let grp_val = group_opt.as_ref().and_then(|_| {
                    matches
                        .get_one::<String>(&format!("{}_grpopt", id))
                        .cloned()
                });
                // For each record field, `fields[j]` is `Some(v)`
                // *only* when the user actually typed the flag on
                // the command line; `None` means "fall back to the
                // group JSON (if any) or to `defaults[j]`." The
                // distinction is required because the C-side
                // `initialize_unrolled` layer treats every non-NULL
                // slot as an explicit user override and will use it
                // in preference to the JSON-loaded record; filling
                // `fields[j]` with the manifest default would block
                // `--alg-config=algconf.json` from taking effect.
                use clap::parser::ValueSource;
                let mut fields = Vec::with_capacity(entries.len());
                let mut defaults = Vec::with_capacity(entries.len());
                for (j, entry) in entries.iter().enumerate() {
                    let eid = format!("{}_entry{}", id, j);
                    let entry_val: Option<String> = match &entry.arg {
                        ManifestArg::Flag { long_rev, .. } => {
                            let fwd_set = matches.get_flag(&eid);
                            let neg_id = format!("{}_neg", eid);
                            let neg_set =
                                long_rev.is_some() && matches.get_flag(&neg_id);
                            if fwd_set {
                                Some("true".to_string())
                            } else if neg_set {
                                Some("false".to_string())
                            } else {
                                None
                            }
                        }
                        ManifestArg::Optional { quoted: q, .. } => {
                            let from_cli = matches.value_source(&eid)
                                == Some(ValueSource::CommandLine);
                            if from_cli {
                                matches.get_one::<String>(&eid).cloned().map(
                                    |v| if *q { quoted(&v) } else { v },
                                )
                            } else {
                                None
                            }
                        }
                        _ => None,
                    };
                    fields.push(entry_val);
                    defaults.push(entry.arg.default_val().map(|s| s.to_string()));
                }
                out.push(ArgValue::Group {
                    grp_val,
                    fields,
                    defaults,
                });
            }
        }
    }
    out
}

/// First non-empty description line (used as clap's `about` text).
fn first_desc(desc: &[String]) -> &str {
    desc.iter()
        .find(|d| !d.trim().is_empty())
        .map(|s| s.as_str())
        .unwrap_or("")
}

/// Render the help block for a manifest arg: the user's docstring
/// description, followed on indented continuation lines by the arg's
/// morloc type and, for flags with a declared default, the default
/// value. When the docstring is empty the type/format/default lines
/// move up to fill the first slot, so help reads as a compact list
/// of facts rather than padded with a placeholder. clap's renderer
/// wraps continuation lines under the same column as the first line.
fn render_arg_help(
    desc: &[String],
    type_desc: Option<&str>,
    default_val: Option<&str>,
    format_hint: Option<&str>,
) -> String {
    let mut lines: Vec<String> = desc
        .iter()
        .filter(|d| !d.trim().is_empty())
        .cloned()
        .collect();
    if let Some(td) = type_desc {
        if !td.trim().is_empty() {
            lines.push(format!("type: {}", td));
        }
    }
    if let Some(f) = format_hint {
        if !f.trim().is_empty() {
            lines.push(format!("format: {}", f));
        }
    }
    if let Some(d) = default_val {
        if !d.trim().is_empty() {
            lines.push(format!("default: {}", d));
        }
    }
    lines.join("\n")
}

/// Render the "Positional arguments:" block for a command's
/// positional args, replacing clap's `<argN>` bracketed default with
/// a numbered list (`1:`, `2:`, ...). Indices are right-aligned and
/// the type line collapses up into the index line when no docstring
/// is supplied, producing:
///
/// ```text
/// Positional arguments:
///   1:  the first thing
///       type: UInt8
///   2:  type: [UInt8]
/// ```
///
/// Returns the empty string when the command has no positionals.
fn render_positional_block(mcmd: &ManifestCommand) -> String {
    let positionals: Vec<&ManifestArg> = mcmd
        .args
        .iter()
        .filter(|a| matches!(a, ManifestArg::Positional { .. }))
        .collect();
    if positionals.is_empty() {
        return String::new();
    }
    let idx_width = positionals.len().to_string().len();
    let mut out = String::from("Positional arguments:");
    for (i, marg) in positionals.iter().enumerate() {
        let prefix = format!("  {:>width$}:  ", i + 1, width = idx_width);
        let cont = " ".repeat(prefix.len());
        let (type_desc, desc, format_hint) = match marg {
            ManifestArg::Positional { type_desc, desc, format, .. } => (
                type_desc.as_deref(),
                desc.as_slice(),
                format.as_deref(),
            ),
            _ => unreachable!("filtered to Positional only"),
        };
        let mut lines: Vec<String> = desc
            .iter()
            .filter(|d| !d.trim().is_empty())
            .cloned()
            .collect();
        if let Some(td) = type_desc {
            if !td.trim().is_empty() {
                lines.push(format!("type: {}", td));
            }
        }
        if let Some(f) = format_hint {
            if !f.trim().is_empty() {
                lines.push(format!("format: {}", f));
            }
        }
        if lines.is_empty() {
            // Nothing to say beyond the index marker; emit just that
            // so the slot still appears in help.
            out.push_str(&format!("\n{}", prefix.trim_end()));
        } else {
            for (li, line) in lines.iter().enumerate() {
                let pad = if li == 0 { &prefix } else { &cont };
                out.push_str(&format!("\n{}{}", pad, line));
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use morloc_manifest::parse_manifest;

    /// Build a manifest JSON payload that wraps one or more
    /// commands. Synthesizes the required version/build fields so
    /// fixtures stay short.
    fn wrap_manifest(commands_json: &str, groups_json: &str) -> String {
        let v = env!("CARGO_PKG_VERSION");
        format!(
            r#"{{
                "name": "main",
                "build": {{
                    "path": "/tmp/test",
                    "time": 0,
                    "morloc_version": "{}"
                }},
                "pools": [
                    {{"lang": "py", "exec": ["python3", "pool.py"], "socket": "pipe-py", "metadata": {{}}}}
                ],
                "commands": {},
                "groups": {},
                "capabilities": ["log"],
                "metadata": {{}}
            }}"#,
            v, commands_json, groups_json
        )
    }

    /// Single-command "add" manifest with two positional Int args.
    fn fixture_single_add() -> Manifest {
        let json = wrap_manifest(
            r#"[
                {
                    "name": "add",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "desc": ["Add two integers"],
                    "args": [
                        {"kind": "pos", "schema": "i8", "type": "Int", "metavar": "X", "quoted": false, "desc": [], "constraints": [], "metadata": {}},
                        {"kind": "pos", "schema": "i8", "type": "Int", "metavar": "Y", "quoted": false, "desc": [], "constraints": [], "metadata": {}}
                    ],
                    "return": {"schema": "i8", "type": "Int", "desc": [], "constraints": [], "metadata": {}},
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
            "[]",
        );
        parse_manifest(&json).unwrap()
    }

    #[test]
    fn arg_help_blocks_emit_description_then_type_then_default() {
        // Standard arg with description + type.
        let h = render_arg_help(
            &["Take the first integer".into()],
            Some("Int"),
            None,
            None,
        );
        assert_eq!(h, "Take the first integer\ntype: Int");

        // Empty description: the type line becomes the first line
        // so help reads as a compact list of facts without a
        // placeholder "missing description" line above the type.
        let h = render_arg_help(&[], Some("Real"), None, None);
        assert_eq!(h, "type: Real");

        // Multi-line description preserves every non-empty line in
        // order; the type line is appended after.
        let h = render_arg_help(
            &["first".into(), "second".into()],
            Some("Str"),
            None,
            None,
        );
        assert_eq!(h, "first\nsecond\ntype: Str");

        // Default value appears as a final line; used for flags.
        let h = render_arg_help(
            &["Verbose output".into()],
            Some("Bool"),
            Some("false"),
            None,
        );
        assert_eq!(h, "Verbose output\ntype: Bool\ndefault: false");

        // Empty type_desc / default are suppressed (no trailing
        // blank lines in the rendered help).
        let h = render_arg_help(&["just a desc".into()], None, None, None);
        assert_eq!(h, "just a desc");

        // Format hint slots in between type and default.
        let h = render_arg_help(
            &["readable file path".into()],
            Some("Str"),
            None,
            Some("must be the path of an existing readable file"),
        );
        assert_eq!(
            h,
            "readable file path\ntype: Str\nformat: must be the path of an existing readable file"
        );
    }

    #[test]
    fn positional_block_collapses_type_into_index_when_no_desc() {
        // fixture_single_add's two positionals have empty `desc`, so
        // the type line takes the first slot beside the index marker
        // rather than sitting on a continuation line under a
        // placeholder. Indices are 1-based.
        let m = fixture_single_add();
        let block = render_positional_block(&m.commands[0]);
        assert_eq!(
            block,
            "Positional arguments:\n  1:  type: Int\n  2:  type: Int"
        );
    }

    #[test]
    fn single_command_positionals_extracted_in_order() {
        let m = fixture_single_add();
        let parsed = parse_run(&m, &["3".into(), "5".into()], "add");
        assert_eq!(parsed.cmd_index, 0);
        assert_eq!(parsed.values.len(), 2);
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "3"),
            _ => panic!("expected Value"),
        }
        match &parsed.values[1] {
            ArgValue::Value(v) => assert_eq!(v, "5"),
            _ => panic!("expected Value"),
        }
    }

    /// Negative-number values must be accepted as positionals
    /// rather than rejected as short flags. POSIX short flags
    /// begin with a letter; the manifest-driven clap commands
    /// enable `allow_negative_numbers(true)` so `-5` and similar
    /// reach the user's Int-typed args.
    #[test]
    fn single_command_accepts_negative_number_positional() {
        let m = fixture_single_add();
        let parsed = parse_run(&m, &["-5".into(), "-7".into()], "add");
        assert_eq!(parsed.values.len(), 2);
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "-5"),
            _ => panic!("expected Value"),
        }
        match &parsed.values[1] {
            ArgValue::Value(v) => assert_eq!(v, "-7"),
            _ => panic!("expected Value"),
        }
    }

    /// In single-command mode the user may type the command name
    /// explicitly (`./prog add 3 5`) or omit it (`./prog 3 5`);
    /// both forms must produce identical results.
    #[test]
    fn single_command_accepts_explicit_command_name_prefix() {
        let m = fixture_single_add();
        let parsed = parse_run(
            &m,
            &["add".into(), "3".into(), "5".into()],
            "add",
        );
        assert_eq!(parsed.cmd_index, 0);
        assert_eq!(parsed.values.len(), 2);
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "3"),
            _ => panic!("expected Value"),
        }
        match &parsed.values[1] {
            ArgValue::Value(v) => assert_eq!(v, "5"),
            _ => panic!("expected Value"),
        }
    }


    /// Multi-command manifest exercising routing by name.
    fn fixture_multi_cmds() -> Manifest {
        let json = wrap_manifest(
            r#"[
                {"name": "alpha", "type": "remote", "mid": 1, "pool": 0, "needed_pools": [0], "desc": [], "args": [], "return": {"schema": "z", "type": "Unit", "desc": [], "constraints": [], "metadata": {}}, "constraints": [], "metadata": {}, "group": null},
                {"name": "beta",  "type": "remote", "mid": 2, "pool": 0, "needed_pools": [0], "desc": [], "args": [], "return": {"schema": "z", "type": "Unit", "desc": [], "constraints": [], "metadata": {}}, "constraints": [], "metadata": {}, "group": null}
            ]"#,
            "[]",
        );
        parse_manifest(&json).unwrap()
    }

    #[test]
    fn multi_command_routes_by_subcommand_name() {
        let m = fixture_multi_cmds();
        let parsed = parse_run(&m, &["beta".into()], "main");
        assert_eq!(parsed.cmd_index, 1);
    }

    /// Manifest with one Optional arg carrying a default.
    fn fixture_optional_with_default() -> Manifest {
        let json = wrap_manifest(
            r#"[
                {
                    "name": "greet",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "desc": [],
                    "args": [
                        {"kind": "opt", "schema": "s", "type": "Str", "metavar": "NAME", "quoted": false, "short": "n", "long": "name", "default": "world", "desc": [], "constraints": [], "metadata": {}}
                    ],
                    "return": {"schema": "s", "type": "Str", "desc": [], "constraints": [], "metadata": {}},
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
            "[]",
        );
        parse_manifest(&json).unwrap()
    }

    #[test]
    fn optional_user_override_wins_over_default() {
        let m = fixture_optional_with_default();
        let parsed = parse_run(&m, &["--name".into(), "Zeb".into()], "greet");
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "Zeb"),
            _ => panic!("expected Value"),
        }
    }

    #[test]
    fn optional_default_used_when_absent() {
        let m = fixture_optional_with_default();
        let parsed = parse_run(&m, &[], "greet");
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "world"),
            _ => panic!("expected Value"),
        }
    }

    /// Manifest with a Flag arg defaulting to false.
    fn fixture_flag() -> Manifest {
        let json = wrap_manifest(
            r#"[
                {
                    "name": "go",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "desc": [],
                    "args": [
                        {"kind": "flag", "short": "v", "long": "verbose", "long_rev": null, "default": "false", "desc": [], "metadata": {}}
                    ],
                    "return": {"schema": "z", "type": "Unit", "desc": [], "constraints": [], "metadata": {}},
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
            "[]",
        );
        parse_manifest(&json).unwrap()
    }

    #[test]
    fn flag_set_becomes_true() {
        let m = fixture_flag();
        let parsed = parse_run(&m, &["--verbose".into()], "go");
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "true"),
            _ => panic!("expected Value"),
        }
    }

    #[test]
    fn flag_absent_uses_default() {
        let m = fixture_flag();
        let parsed = parse_run(&m, &[], "go");
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "false"),
            _ => panic!("expected Value"),
        }
    }

    #[test]
    fn flag_with_short_form_set_becomes_true() {
        let m = fixture_flag();
        let parsed = parse_run(&m, &["-v".into()], "go");
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "true"),
            _ => panic!("expected Value"),
        }
    }

    /// Quoted-positional manifest: the `--' literal: true` declaration
    /// instructs the nexus to JSON-quote the value before passing it
    /// to the pool. parse_command_args wraps in JSON quotes; the
    /// phase-2 builder must do the same.
    fn fixture_quoted_positional() -> Manifest {
        let json = wrap_manifest(
            r#"[
                {
                    "name": "echo",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "desc": [],
                    "args": [
                        {"kind": "pos", "schema": "s", "type": "Str", "metavar": "S", "quoted": true, "desc": [], "constraints": [], "metadata": {}}
                    ],
                    "return": {"schema": "s", "type": "Str", "desc": [], "constraints": [], "metadata": {}},
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
            "[]",
        );
        parse_manifest(&json).unwrap()
    }

    #[test]
    fn quoted_positional_is_json_escaped() {
        let m = fixture_quoted_positional();
        let parsed = parse_run(&m, &[r#"hello "world""#.into()], "echo");
        match &parsed.values[0] {
            ArgValue::Value(v) => {
                // Whatever clap and serde_json produce must round-trip through
                // serde_json::from_str back to the original.
                let decoded: String = serde_json::from_str(v).unwrap();
                assert_eq!(decoded, r#"hello "world""#);
            }
            _ => panic!("expected Value"),
        }
    }

    /// A single-command manifest exporting `bar` with one
    /// `literal: true` Optional whose default value is the
    /// already-JSON-quoted string `"yolo"` (6 characters). When the
    /// user omits the flag, the pool must receive the exact 6-byte
    /// value from the manifest; routing it through `quoted()` a
    /// second time would double-encode to `"\"yolo\""` and the
    /// pool would parse it as the 6-char `"yolo"` rather than the
    /// 4-char `yolo`.
    fn fixture_literal_optional_with_default() -> Manifest {
        let json = wrap_manifest(
            r#"[
                {
                    "name": "bar",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "desc": [],
                    "args": [
                        {"kind": "opt", "schema": "s", "type": "Str", "metavar": "S", "quoted": true, "short": "y", "long": "yolo", "default": "\"yolo\"", "desc": [], "constraints": [], "metadata": {}}
                    ],
                    "return": {"schema": "i8", "type": "Int", "desc": [], "constraints": [], "metadata": {}},
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
            "[]",
        );
        parse_manifest(&json).unwrap()
    }

    #[test]
    fn literal_optional_default_passes_through_verbatim() {
        let m = fixture_literal_optional_with_default();
        let parsed = parse_run(&m, &[], "bar");
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "\"yolo\""),
            _ => panic!("expected Value"),
        }
    }

    #[test]
    fn literal_optional_cli_value_is_json_quoted() {
        // When the user types `-y a`, the value is JSON-quoted on
        // the way to the pool so the string survives transport.
        let m = fixture_literal_optional_with_default();
        let parsed = parse_run(&m, &["-y".into(), "a".into()], "bar");
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "\"a\""),
            _ => panic!("expected Value"),
        }
    }

    /// Single-command manifest exporting `foo` whose only argument
    /// is a record `AlgConfig` exposing the whole-record JSON option
    /// `--alg-config` plus per-field `-m` and `-n` flags with
    /// non-empty manifest defaults. Models the `record-docstrings`
    /// golden test's failing line:
    /// `./nexus foo --alg-config=algconf.json [1,2,3]`.
    fn fixture_record_group_with_grpopt() -> Manifest {
        let json = wrap_manifest(
            r#"[
                {
                    "name": "foo",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "desc": [],
                    "args": [
                        {
                            "kind": "grp",
                            "schema": "m22m1Int52m1Int5",
                            "type": "AlgConfig",
                            "metavar": "ALG_CONFIG",
                            "desc": [],
                            "group_opt": {"short": null, "long": "alg-config"},
                            "entries": [
                                {"key": "m", "arg": {"kind": "opt", "schema": "i4", "type": "Int", "metavar": "INT", "quoted": false, "short": "m", "long": null, "default": "0", "desc": [], "constraints": [], "metadata": {}}},
                                {"key": "n", "arg": {"kind": "opt", "schema": "i4", "type": "Int", "metavar": "INT", "quoted": false, "short": "n", "long": "nosy", "default": "0", "desc": [], "constraints": [], "metadata": {}}}
                            ],
                            "constraints": [],
                            "metadata": {}
                        }
                    ],
                    "return": {"schema": "i8", "type": "Int", "desc": [], "constraints": [], "metadata": {}},
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
            "[]",
        );
        parse_manifest(&json).unwrap()
    }

    #[test]
    fn group_entry_default_does_not_block_grpopt() {
        // No per-field flags were typed -> fields[*] must be None so
        // the C-side initialize_unrolled layer falls back to the
        // group JSON. If we instead pushed the manifest defaults
        // ("0", "0") into fields, the JSON would never get loaded
        // and the pool would see m=0, n=0.
        let m = fixture_record_group_with_grpopt();
        let parsed = parse_run(
            &m,
            &["--alg-config=algconf.json".into()],
            "foo",
        );
        match &parsed.values[0] {
            ArgValue::Group { grp_val, fields, defaults } => {
                assert_eq!(grp_val.as_deref(), Some("algconf.json"));
                assert!(fields.iter().all(|f| f.is_none()),
                    "fields must be all-None when the user typed no per-field flags");
                assert_eq!(defaults[0].as_deref(), Some("0"));
                assert_eq!(defaults[1].as_deref(), Some("0"));
            }
            _ => panic!("expected Group"),
        }
    }

    #[test]
    fn group_entry_cli_value_fills_field() {
        // Per-field flag wins over the JSON / per-field default.
        let m = fixture_record_group_with_grpopt();
        let parsed = parse_run(
            &m,
            &["-m".into(), "6".into()],
            "foo",
        );
        match &parsed.values[0] {
            ArgValue::Group { fields, .. } => {
                assert_eq!(fields[0].as_deref(), Some("6"));
                assert!(fields[1].is_none());
            }
            _ => panic!("expected Group"),
        }
    }
}
