//! Manifest-driven parse of a program's command line: a `clap::Command`
//! built from the manifest's commands and arguments, whose matches become
//! the [`ArgValue`]s that [`crate::dispatch::dispatch_command_parsed`] runs.

use clap::{Arg as ClapArg, ArgAction, ArgGroup, ArgMatches, Command as ClapCommand};

use crate::dispatch::{preprocess_cli_value, ArgValue};
use morloc_runtime_types::schema::{parse_schema, SerialType};
use morloc_manifest::{Arg as ManifestArg, Command as ManifestCommand, Manifest, Return};

/// Leak a string into a `&'static str` for clap's builder API.
pub(crate) fn leak(s: &str) -> &'static str {
    Box::leak(s.to_string().into_boxed_str())
}

/// A parsed command line.
pub struct ParsedCommand {
    /// Index into [`Manifest::commands`] of the dispatched command, before
    /// any `@parse` redirect (see `parse_arg::redirect`).
    pub cmd_index: usize,
    /// Per-arg parsed values of the parent, index-aligned with its `args`.
    /// A terminal entry takes the same arguments as its parent.
    pub values: Vec<ArgValue>,
    /// A `render` terminal was chosen: its bytes are written raw and `-f`
    /// does not apply.
    pub render: bool,
    /// The command the user named, before any terminal redirect.
    pub parent_index: usize,
    /// Every terminal action named, in the order of `cmd.terminals`.
    pub actions: Vec<Action>,
    /// The terminal that writes standard output, if any: a bare action, or
    /// else the `@default` one when no `-f` and no `--no-stdout` was given
    /// and it was not given a path.
    pub stdout_terminal: Option<usize>,
    /// `--no-stdout` was given.
    pub no_stdout: bool,
    /// The run needs the parent's output staged and its actions replayed
    /// (see `orchestrate`). `cmd_index`/`values`/`render` are then unused.
    pub staged: bool,
}

/// Where one named terminal action writes.
#[derive(Clone, Debug, PartialEq)]
pub enum Dest {
    Stdout,
    File(FileTarget),
}

/// The file a path names, made only by [`resolve_target`]: two targets
/// are equal exactly when they name one file, so the checks between
/// actions and the place a file is finally written cannot disagree.
#[derive(Clone, Debug)]
pub struct FileTarget {
    given: String,
    file: std::path::PathBuf,
}

/// Targets are the files they name, however each path was spelled.
impl PartialEq for FileTarget {
    fn eq(&self, other: &Self) -> bool {
        self.file == other.file
    }
}

impl Eq for FileTarget {}

impl FileTarget {
    /// Whether writing both could write one file: the same resolved path,
    /// the same existing file under two names (a hard link), or names in one
    /// directory that differ only in case. The last is refused everywhere,
    /// not only where the filesystem folds case, so a command line means the
    /// same thing on every machine.
    pub fn may_name_same_file(&self, other: &Self) -> bool {
        use std::os::unix::fs::MetadataExt;
        if self == other {
            return true;
        }
        if let (Ok(a), Ok(b)) = (std::fs::metadata(&self.file), std::fs::metadata(&other.file)) {
            if (a.dev(), a.ino()) == (b.dev(), b.ino()) {
                return true;
            }
        }
        let fold = |p: &std::path::Path| p.file_name().map(|n| n.to_string_lossy().to_lowercase());
        self.file.parent() == other.file.parent() && fold(&self.file) == fold(&other.file)
    }

    /// The path as the user wrote it.
    pub fn given(&self) -> &str {
        &self.given
    }

    /// The file it names.
    pub fn file(&self) -> &std::path::Path {
        &self.file
    }
}

/// One terminal action named on the command line.
#[derive(Clone, Debug)]
pub struct Action {
    /// Index into the parent command's `terminals`.
    pub terminal: usize,
    pub dest: Dest,
}

/// Prefix of the clap ids of terminal-action flags.
const TERMINAL_ID_PREFIX: &str = "term_";
/// Clap id of `--no-stdout`, present on commands with terminal actions.
const NO_STDOUT_ID: &str = "no_stdout";
/// The value of an action flag given without `=PATH`: standard output.
const STDOUT_DEST: &str = "-";

/// Parse a program's command line `user_zone` against the manifest. A
/// parse error, or a help request, ends the process with clap's output.
pub fn parse_run(
    manifest: &Manifest,
    user_zone: &[String],
    prog_name: &str,
    format_explicit: bool,
    output_path: Option<&str>,
) -> ParsedCommand {
    let root = build_root(manifest, prog_name, help_level(user_zone));
    let visible: Vec<(usize, &ManifestCommand)> = manifest
        .commands
        .iter()
        .enumerate()
        .filter(|(_, c)| !c.internal)
        .collect();
    let single = visible.len() == 1 && manifest.groups.is_empty();

    // In single-command mode the command name may be given or left out.
    let trimmed: Vec<String> = if single
        && !user_zone.is_empty()
        && user_zone[0] == visible[0].1.name
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
        Err(e) => {
            if let Some(hint) = nexus_flag_hint_for_error(&e) {
                eprint!("{}", e);
                eprintln!("{}", hint);
                std::process::exit(2);
            }
            e.exit()
        }
    };

    if single {
        let (cmd_index0, cmd) = visible[0];
        let values = extract_values(cmd, &matches);
        return finish_parse(manifest, cmd_index0, cmd, &matches, values, format_explicit, output_path);
    }

    let (cmd_name, sub_matches) =
        matches.subcommand().expect("clap guarantees subcommand");
    let (chosen_name, chosen_matches) =
        if manifest.groups.iter().any(|g| g.name == cmd_name) {
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
    finish_parse(manifest, cmd_index, cmd, chosen_matches, values, format_explicit, output_path)
}

/// Resolve the parsed parent and its terminal actions into the command to
/// dispatch. A run is `staged` when it names an action with a path, gives
/// `--no-stdout`, or sends to stdout an action with no entry of its own;
/// otherwise it is one run of the parent, or of one terminal's entry.
fn finish_parse(
    manifest: &Manifest,
    parent_index: usize,
    cmd: &ManifestCommand,
    matches: &ArgMatches,
    values: Vec<ArgValue>,
    format_explicit: bool,
    output_path: Option<&str>,
) -> ParsedCommand {
    let (actions, no_stdout) = collect_actions(cmd, matches, output_path);
    let bare = actions.iter().find(|a| a.dest == Dest::Stdout).map(|a| a.terminal);
    // Stdout gets a bare action, else the `@default` action unless `-f` asks
    // for the typed value or that action writes to a file: no output goes
    // to both a file and stdout.
    let has_path_to = |t: usize| actions.iter().any(|a| a.terminal == t && a.dest != Dest::Stdout);
    let stdout_terminal = if no_stdout {
        None
    } else if bare.is_some() {
        bare
    } else if !format_explicit {
        cmd.terminals.iter().position(|t| t.default).filter(|&t| !has_path_to(t))
    } else {
        None
    };
    let has_path = actions.iter().any(|a| a.dest != Dest::Stdout);
    let stdout_needs_stage = stdout_terminal
        .map(|t| cmd.terminals[t].entry.is_none())
        .unwrap_or(false);
    let staged = has_path || no_stdout || stdout_needs_stage;
    let (cmd_index, render) = match stdout_terminal {
        Some(t) if !staged => {
            let term = &cmd.terminals[t];
            match term.entry.as_deref().and_then(|e| manifest.command_index(e)) {
                Some(idx) => (idx, term.render),
                None => (parent_index, false),
            }
        }
        _ => (parent_index, false),
    };
    ParsedCommand {
        cmd_index,
        values,
        render,
        parent_index,
        actions,
        stdout_terminal,
        no_stdout,
        staged,
    }
}

/// The terminal actions named on the command line, with their destinations,
/// and whether `--no-stdout` was given. Rejects, before anything runs, the
/// requests that contradict each other: two claims on standard output, one
/// path given to two actions, a path that is also `-o`'s, an empty `=`, and
/// a path naming a directory.
fn collect_actions(
    cmd: &ManifestCommand,
    matches: &ArgMatches,
    output_path: Option<&str>,
) -> (Vec<Action>, bool) {
    use clap::parser::ValueSource;
    let fail = |msg: String| -> ! {
        eprintln!("error: {}", msg);
        std::process::exit(2);
    };
    if cmd.terminals.is_empty() {
        return (Vec::new(), false);
    }
    let mut actions = Vec::new();
    for (i, t) in cmd.terminals.iter().enumerate() {
        let id: &'static str = leak(&format!("{}{}", TERMINAL_ID_PREFIX, i));
        if matches.value_source(id) != Some(ValueSource::CommandLine) {
            continue;
        }
        let v = matches.get_one::<String>(id).map(String::as_str).unwrap_or(STDOUT_DEST);
        let dest = if v.is_empty() {
            fail(format!(
                "--{long}= needs a path; give --{long} alone to write to standard output",
                long = t.long,
            ))
        } else if v == STDOUT_DEST {
            Dest::Stdout
        } else {
            Dest::File(resolve_target(v))
        };
        actions.push(Action { terminal: i, dest });
    }
    let no_stdout = matches.try_get_one::<bool>(NO_STDOUT_ID).ok().flatten().copied().unwrap_or(false);

    let mut claims: Vec<String> = actions
        .iter()
        .filter(|a| a.dest == Dest::Stdout)
        .map(|a| format!("--{}", cmd.terminals[a.terminal].long))
        .collect();
    if no_stdout {
        claims.push("--no-stdout".to_string());
    }
    if claims.len() > 1 {
        let example = actions
            .iter()
            .filter(|a| a.dest == Dest::Stdout)
            .last()
            .map(|a| format!(", e.g. --{}=out.txt", cmd.terminals[a.terminal].long))
            .unwrap_or_default();
        let quantifier = if claims.len() == 2 { "both" } else { "all" };
        fail(format!(
            "{} {} claim standard output; give all but one a path{}",
            claims.join(" and "),
            quantifier,
            example,
        ));
    }

    let mut seen: Vec<(FileTarget, &str)> = Vec::new();
    if let Some(o) = output_path {
        seen.push((resolve_target(o), "-o"));
    }
    for a in &actions {
        if let Dest::File(target) = &a.dest {
            let long = cmd.terminals[a.terminal].long.as_str();
            if target.file().is_dir() {
                fail(format!("--{}={}: the path is a directory", long, target.given()));
            }
            if let Some((k, other)) = seen.iter().find(|(k, _)| k.may_name_same_file(target)) {
                let other = if *other == "-o" { "-o".to_string() } else { format!("--{}", other) };
                if k == target {
                    fail(format!("--{} and {} both write to {}", long, other, target.given()));
                }
                fail(format!(
                    "--{} and {} write to {} and {}, which may be one file: they are the \
                     same existing file, or differ only in case, which a case-insensitive \
                     filesystem (the macOS default) does not distinguish",
                    long, other, target.given(), k.given()
                ));
            }
            seen.push((target.clone(), long));
        }
    }
    (actions, no_stdout)
}

/// The file a path names: symlinks followed, dangling ones included, and
/// the directory resolved. Two paths name one file exactly when their
/// resolutions are equal, and a file an action writes lands at the
/// resolution of its path, as a shell `>` would write it.
pub(crate) fn resolve_target(p: &str) -> FileTarget {
    let mut path = std::path::PathBuf::from(p);
    // A symlink loop is left unresolved after this many steps; opening the
    // path then fails as it would for any tool.
    for _ in 0..40 {
        match std::fs::symlink_metadata(&path) {
            Ok(m) if m.file_type().is_symlink() => match std::fs::read_link(&path) {
                Ok(next) => {
                    path = match path.parent() {
                        Some(dir) if next.is_relative() => dir.join(next),
                        _ => next,
                    };
                }
                Err(_) => break,
            },
            _ => break,
        }
    }
    let dir = match path.parent() {
        Some(d) if !d.as_os_str().is_empty() => d.to_path_buf(),
        _ => std::path::PathBuf::from("."),
    };
    let dir = std::fs::canonicalize(&dir).unwrap_or(dir);
    let file = match path.file_name() {
        Some(name) => dir.join(name),
        None => dir,
    };
    FileTarget { given: p.to_string(), file }
}

/// How much a help page discloses, from how many times the user asked.
///
/// Every `h` in a short cluster (`-h`, `-hh`, `-qhh`) and every `--help`
/// counts one; the sum is clamped to the three tiers. A token after `--`
/// never counts. Returns 1 when no help was asked for.
///
/// 1. `-h`: the synopsis line, the arguments with their types and
///    formats, the return type.
/// 2. `-hh`: the description lines after the first, and epilogues.
/// 3. `-hhh`: the record, data, table and wire-form schemas.
pub fn help_level(user_zone: &[String]) -> u8 {
    let mut n: usize = 0;
    for tok in user_zone {
        if tok == "--" {
            break;
        }
        if tok == "--help" {
            n += 1;
        } else if crate::cli::looks_like_flag(tok) && !tok.starts_with("--") {
            n += tok[1..].chars().filter(|&c| c == 'h').count();
        }
    }
    n.clamp(1, 3) as u8
}

/// A placement hint when clap rejects a token that is a long nexus flag.
fn nexus_flag_hint_for_error(err: &clap::Error) -> Option<String> {
    use clap::error::{ContextKind, ContextValue, ErrorKind};

    if err.kind() != ErrorKind::UnknownArgument {
        return None;
    }
    let token = err.context().find_map(|(k, v)| match (k, v) {
        (ContextKind::InvalidArg, ContextValue::String(s)) => Some(s.clone()),
        _ => None,
    })?;
    let name_only = token.split_once('=').map(|(n, _)| n).unwrap_or(&token);
    if name_only.strip_prefix("--").is_none() {
        return None;
    }
    if !crate::cli::all_long_nexus_flags().contains(name_only) {
        return None;
    }
    Some(format!(
        "hint: `{name}` is a nexus option, not a command option. \
         Place it left of `@` (single-export) or left of the \
         subcommand (multi-export), e.g. `./prog {name} X @ cmd arg` \
         or `./prog {name} X cmd arg`.",
        name = name_only,
    ))
}

/// The program's root `clap::Command`. With one visible command and no
/// groups, its args live on the root; otherwise each command is a
/// subcommand, under its group's subcommand if it has one.
///
/// `level` is the help tier the tree is built for (see [`help_level`]).
pub fn build_root(manifest: &Manifest, prog_name: &str, level: u8) -> ClapCommand {
    let visible_count = manifest.commands.iter().filter(|c| !c.internal).count();
    let single = visible_count == 1 && manifest.groups.is_empty();

    let epilogue = render_epilogues(&manifest.epilogues);

    if single {
        let cmd = manifest
            .commands
            .iter()
            .find(|c| !c.internal)
            .expect("visible_count == 1 implies at least one non-internal command");
        // First, so "General Options" sorts before the command's args
        // (clap orders sections by first arg added).
        let root = crate::help::add_general_options(ClapCommand::new(leak(prog_name)));
        // The root is the program and the command at once, so both
        // docstrings describe it and `build_command_args` sees only one of
        // them.
        let desc = single_root_desc(&manifest.desc, &cmd.desc);
        let mut root = with_description(
            build_command_args(root, cmd, manifest, level),
            &desc,
            level,
        )
        .arg_required_else_help(false);
        if level >= 2 {
            root = append_after_help(root, &epilogue);
            root = append_after_help(root, &render_epilogues(&cmd.epilogues));
        }
        return crate::help::finalize(root, crate::help::usage_single_root(prog_name));
    }

    let mut root = ClapCommand::new(leak(prog_name))
        .subcommand_required(true)
        .arg_required_else_help(true);
    root = crate::help::add_general_options(root);
    root = with_description(root, &manifest.desc, level);
    if level >= 2 {
        root = append_after_help(root, &epilogue);
    }
    root = crate::help::finalize(root, crate::help::usage_multi_root(prog_name));

    for grp in &manifest.groups {
        let mut grp_cmd = ClapCommand::new(leak(&grp.name))
            .subcommand_required(true)
            .arg_required_else_help(true);
        grp_cmd = crate::help::add_general_options(grp_cmd);
        grp_cmd = with_description(grp_cmd, &grp.desc, level);
        grp_cmd = crate::help::finalize(
            grp_cmd,
            crate::help::usage_multi_group(prog_name, &grp.name),
        );
        for cmd in &manifest.commands {
            if cmd.internal {
                continue;
            }
            if cmd.group.as_deref() == Some(grp.name.as_str()) {
                let sub = crate::help::add_general_options(
                    ClapCommand::new(leak(&cmd.name))
                        .about(leak(first_desc(&cmd.desc))),
                );
                let sub = build_command_args(sub, cmd, manifest, level);
                let sub = if level >= 2 {
                    append_after_help(sub, &render_epilogues(&cmd.epilogues))
                } else {
                    sub
                };
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
        if cmd.internal {
            continue;
        }
        if cmd.group.is_none() {
            let sub = crate::help::add_general_options(
                ClapCommand::new(leak(&cmd.name))
                    .about(leak(first_desc(&cmd.desc))),
            );
            let sub = build_command_args(sub, cmd, manifest, level);
            let sub = if level >= 2 {
                append_after_help(sub, &render_epilogues(&cmd.epilogues))
            } else {
                sub
            };
            let sub = crate::help::finalize(
                sub,
                crate::help::usage_multi_sub(prog_name, None, &cmd.name),
            );
            root = root.subcommand(sub);
        }
    }
    root
}


/// The return descriptor shown in help: a `@mime` media type if there is
/// one, else the morloc type name.
fn ret_display(ret: &Return) -> String {
    ret.mime.clone().unwrap_or_else(|| ret.type_desc.clone())
}

/// What a command puts on standard output: its stream's type when it
/// streams (and so returns `()`), else its return type.
fn stdout_display(cmd: &ManifestCommand) -> String {
    match &cmd.stream {
        Some(st) if !st.type_desc.is_empty() => st.type_desc.clone(),
        _ => ret_display(&cmd.ret),
    }
}

/// The `Return:` help block: the type and its description, and with
/// terminal actions, one row per action.
fn render_return_block(mcmd: &ManifestCommand, manifest: &Manifest) -> String {
    if mcmd.terminals.is_empty() {
        if mcmd.ret.type_desc.is_empty() {
            return String::new();
        }
        let mut block = format!("Return: {}", stdout_display(mcmd));
        for line in &mcmd.ret.desc {
            block.push_str(&format!("\n  {}", line));
        }
        return block;
    }

    let mut block = format!("Return:\n  type: {}", stdout_display(mcmd));
    for (i, line) in mcmd.ret.desc.iter().enumerate() {
        let lead = if i == 0 { "desc: " } else { "      " };
        block.push_str(&format!("\n  {}{}", lead, line));
    }
    block.push_str("\n  actions:");

    // A terminal whose entry can't be resolved keeps its row, with an empty
    // type.
    let rows: Vec<[String; 4]> = mcmd
        .terminals
        .iter()
        .map(|t| {
            let label = match t.short {
                Some(c) => format!("-{}/--{}:", c, t.long),
                None => format!("--{}:", t.long),
            };
            let ret = t.output_command(manifest).map(stdout_display).unwrap_or_default();
            let raw = if t.render { "(raw bytes)" } else { "" };
            let mark = if t.default { "default" } else { "" };
            [label, ret, raw.to_string(), mark.to_string()]
        })
        .collect();

    let widths: Vec<usize> = (0..4)
        .map(|c| rows.iter().map(|r| r[c].len()).max().unwrap_or(0))
        .collect();
    for row in &rows {
        let line = format!(
            "{:<w0$} {:<w1$}  {:<w2$}  {}",
            row[0], row[1], row[2], row[3],
            w0 = widths[0], w1 = widths[1], w2 = widths[2],
        );
        block.push_str(&format!("\n    {}", line.trim_end()));
    }
    block
}

/// Add every manifest `Arg` to `cmd` under stable ids: `arg<index>`, with
/// `<id>_neg` for a flag's reverse spelling, `<id>_grpopt`/`<id>_entry<j>`
/// for a record and `<id>_arm<j>` for a constructor choice. Values such as
/// `-5` are taken as values, not short flags.
fn build_command_args(
    mut cmd: ClapCommand,
    mcmd: &ManifestCommand,
    manifest: &Manifest,
    level: u8,
) -> ClapCommand {
    cmd = cmd
        .allow_negative_numbers(true)
        .next_help_heading("Optional arguments");
    cmd = with_description(cmd, &mcmd.desc, level);

    // Positionals, the return block and (from -hhh) the schemas are
    // rendered here, beneath clap's argument list.
    let mut after = String::new();
    let pos_block = render_positional_block(mcmd);
    if !pos_block.is_empty() {
        after.push_str(&pos_block);
    }
    let ret_block = render_return_block(mcmd, manifest);
    if !ret_block.is_empty() {
        if !after.is_empty() {
            after.push_str("\n\n");
        }
        after.push_str(&ret_block);
    }
    if level >= 3 {
        if let Some(block) = crate::schemas::render_command_schemas(mcmd) {
            if !after.is_empty() {
                after.push_str("\n\n");
            }
            after.push_str(&block);
        }
    }
    if !after.is_empty() {
        cmd = cmd.after_help(leak(&after));
    }

    let mut pos_idx: u8 = 1;
    for (i, marg) in mcmd.args.iter().enumerate() {
        let id: &'static str = leak(&format!("arg{}", i));
        match marg {
            ManifestArg::Positional { many, stdin, schema, metavar, key, .. } => {
                // Hidden here and shown by `render_positional_block`. A
                // `@stdin` positional may be omitted to read stdin, and a `?T`
                // one to give null, as `--json-help` and MCP report them.
                let optional = crate::json_help::schema_is_optional(schema.as_deref());
                let mut a = ClapArg::new(id)
                    .required(!stdin && !optional)
                    .index(pos_idx as usize)
                    .hide(true);
                // Names the slot in clap's parse errors, which would otherwise
                // read `<arg0>`.
                if let Some(m) = metavar {
                    a = a.value_name(leak(m));
                } else if !key.is_empty() {
                    a = a.value_name(leak(key));
                }
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
                schema,
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
                    let ctor_typed = crate::json_help::schema_constructor_line(schema.as_deref()).is_some();
                    a = a.default_value(leak(&shown_default(d, ctor_typed)));
                }
                if *many {
                    // Every occurrence's values, and several after one
                    // (`--xs 1 2 3`).
                    a = a.num_args(1..).action(ArgAction::Append);
                }
                let mut help = render_arg_help(
                    desc,
                    type_desc.as_deref(),
                    None,
                    format.as_deref(),
                    crate::json_help::schema_constructor_line(schema.as_deref()),
                );
                if let Some(l) = formats_line(marg.parse()) {
                    if !help.is_empty() {
                        help.push('\n');
                    }
                    help.push_str(&l);
                }
                a = a.help(leak(&help));
                cmd = cmd.arg(a);
            }
            ManifestArg::Flag {
                long_opt,
                short_opt,
                long_rev,
                short_rev,
                default_val,
                desc,
                ..
            } => {
                // The forward spelling sets true and the reverse false; the
                // last one given wins.
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
                    None,
                )));
                cmd = cmd.arg(fwd);

                if long_rev.is_some() || short_rev.is_some() {
                    let neg_id: &'static str = leak(&format!("{}_neg", id));
                    let mut neg = ClapArg::new(neg_id)
                        .action(ArgAction::SetTrue)
                        .overrides_with(id);
                    if let Some(rev) = long_rev {
                        neg = neg.long(leak(rev));
                    }
                    if let Some(s) = short_rev {
                        if let Some(c) = s.chars().next() {
                            neg = neg.short(c);
                        }
                    }
                    neg = neg.help(leak(&reverse_flag_help(
                        long_opt.as_deref(),
                        short_opt.as_deref(),
                    )));
                    cmd = cmd.arg(neg);
                }
            }
            ManifestArg::Group {
                group_opt,
                entries,
                metavar,
                type_desc,
                desc,
                schema,
                ..
            } => {
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
                        None,
                    )));
                    cmd = cmd.arg(a);
                }
                for (j, entry) in entries.iter().enumerate() {
                    let eid: &'static str = leak(&format!("{}_entry{}", id, j));
                    let ctor_line = crate::json_help::group_entry_constructor_line(
                        schema.as_deref(),
                        &entry.key,
                    );
                    cmd = add_group_entry_arg(cmd, eid, &entry.arg, ctor_line);
                }
            }
            ManifestArg::Alt { arms, required, default_val, type_desc, .. } => {
                // One option per constructor, all in one exclusive group.
                // An argument-free constructor is a bare flag; one with
                // fields takes exactly that many values, named by type.
                let mut members: Vec<&'static str> = Vec::with_capacity(arms.len());
                for (j, arm) in arms.iter().enumerate() {
                    let aid: &'static str = leak(&format!("{}_arm{}", id, j));
                    let mut a = ClapArg::new(aid).long(leak(&arm.long));
                    if arm.fields.is_empty() {
                        a = a.action(ArgAction::SetTrue);
                    } else {
                        let names: Vec<&'static str> = arm
                            .fields
                            .iter()
                            .map(|f| leak(f.type_desc.as_deref().unwrap_or("VALUE")))
                            .collect();
                        a = a
                            .action(ArgAction::Set)
                            .num_args(arm.fields.len())
                            .value_names(names);
                    }
                    let mut lines: Vec<String> = arm
                        .desc
                        .iter()
                        .filter(|d| !d.trim().is_empty())
                        .cloned()
                        .collect();
                    let standing = match (required, default_val) {
                        (true, _) => "one is required".to_string(),
                        (false, Some(d)) => format!("default: {}", shown_default(d, true)),
                        (false, None) => "none means null".to_string(),
                    };
                    match type_desc {
                        Some(td) => lines.push(format!("one of {}'s constructors; {}", td, standing)),
                        None => lines.push(standing),
                    }
                    a = a.help(leak(&lines.join("\n")));
                    cmd = cmd.arg(a);
                    members.push(aid);
                }
                let gid: &'static str = leak(&format!("{}_alt", id));
                let mut grp = ArgGroup::new(gid).multiple(false).required(*required);
                for m in members {
                    grp = grp.arg(m);
                }
                cmd = cmd.group(grp);
            }
        }
    }
    cmd = add_terminal_flags(cmd, mcmd);
    cmd
}

/// Add each terminal action's flag, and `--no-stdout`, to `cmd` under
/// "General Options".
fn add_terminal_flags(mut cmd: ClapCommand, mcmd: &ManifestCommand) -> ClapCommand {
    if mcmd.terminals.is_empty() {
        return cmd;
    }
    // `--long` writes to standard output and `--long=PATH` to a file. The
    // value is taken only with `=`, so `--long word` stays a flag followed
    // by a positional.
    for (i, t) in mcmd.terminals.iter().enumerate() {
        let id: &'static str = leak(&format!("{}{}", TERMINAL_ID_PREFIX, i));
        let long: &'static str = leak(&t.long);
        let mut a = ClapArg::new(id)
            .long(long)
            .action(ArgAction::Set)
            .num_args(0..=1)
            .require_equals(true)
            .default_missing_value(STDOUT_DEST)
            .value_name("PATH")
            .help_heading("General Options")
            .help(leak(&t.description));
        if let Some(c) = t.short {
            a = a.short(c);
        }
        cmd = cmd.arg(a);
    }
    cmd.arg(
        ClapArg::new(NO_STDOUT_ID)
            .long("no-stdout")
            .action(ArgAction::SetTrue)
            .help_heading("General Options")
            .help(
                "Write nothing to standard output. Actions given a path \
                 (--action=PATH) still write their files, and several may be \
                 combined in one run.",
            ),
    )
}

/// Add a clap arg representing one group entry (which is itself an
/// Optional or Flag manifest `Arg`).
fn add_group_entry_arg(
    mut cmd: ClapCommand,
    id: &'static str,
    marg: &ManifestArg,
    ctor_line: Option<String>,
) -> ClapCommand {
    match marg {
        ManifestArg::Optional {
            long_opt,
            short_opt,
            metavar,
            default_val,
            type_desc,
            desc,
            format,
            many,
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
                a = a.default_value(leak(&shown_default(d, ctor_line.is_some())));
            }
            if *many {
                a = a.num_args(1..).action(ArgAction::Append);
            }
            a = a.help(leak(&render_arg_help(
                desc,
                type_desc.as_deref(),
                None,
                format.as_deref(),
                ctor_line,
            )));
            cmd = cmd.arg(a);
        }
        ManifestArg::Flag {
            long_opt,
            short_opt,
            long_rev,
            short_rev,
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
                None,
            )));
            cmd = cmd.arg(fwd);
            if long_rev.is_some() || short_rev.is_some() {
                let neg_id: &'static str = leak(&format!("{}_neg", id));
                let mut neg = ClapArg::new(neg_id)
                    .action(ArgAction::SetTrue)
                    .overrides_with(id);
                if let Some(rev) = long_rev {
                    neg = neg.long(leak(rev));
                }
                if let Some(s) = short_rev {
                    if let Some(c) = s.chars().next() {
                        neg = neg.short(c);
                    }
                }
                neg = neg.help(leak(&reverse_flag_help(
                    long_opt.as_deref(),
                    short_opt.as_deref(),
                )));
                cmd = cmd.arg(neg);
            }
        }
        _ => {
            eprintln!(
                "Internal error: group entry of unexpected kind ({:?})",
                std::mem::discriminant(marg)
            );
            crate::process::clean_exit(1);
        }
    }
    cmd
}

/// Read one argv token of argument `i`. An argument with `@parse` formats is
/// first checked for a format prefix or extension; a token that selects a
/// format is a path its handler reads, so none of the value's shape applies.
fn token_value(
    raw: String,
    marg: &ManifestArg,
    checks: &[crate::manifest::Check],
    source: crate::manifest::SourceAtom,
    quoted: bool,
    i: usize,
) -> ArgValue {
    use crate::parse_arg::{select, Selection};
    let label = format!("argument #{}", i);
    let raw = match marg.parse().map(|p| select(&raw, p)) {
        Some(Selection::Format { index, path }) => {
            if path.is_empty() {
                crate::runlog::die_with_error(&format!("{}: no path after the format prefix in '{}'", label, raw));
            }
            let handed = if path == "-" { "/dev/stdin" } else { path };
            return ArgValue::Parsed { format: index, path: handed.to_string(), shown: path.to_string() };
        }
        Some(Selection::Morloc(rest)) => rest.to_string(),
        Some(Selection::Unselected) | None => raw,
    };
    crate::parse_arg::note_unparsed_token(&raw);
    ArgValue::Value(preprocess_cli_value(raw, checks, source, quoted, &label))
}

/// The JSON list a `@many` record field's tokens stand for. A string element
/// is taken as written; any other element must be a JSON value of its type,
/// since a record field is read as one value and has no per-element source.
fn many_field_json(tokens: &[String], literal: bool, key: &str) -> String {
    let items: Vec<String> = tokens.iter().map(|t| {
        if literal {
            return crate::dispatch::quoted(t);
        }
        let t = morloc_manifest::cli_value::maybe_float_special_to_json(t).unwrap_or_else(|| t.clone());
        if serde_json::from_str::<serde_json::Value>(&t).is_err() {
            crate::runlog::die_with_error(&format!(
                "record entry '{}': '{}' is not a value of the element type", key, t));
        }
        t
    }).collect();
    format!("[{}]", items.join(","))
}

/// One [`ArgValue`] per manifest arg of `cmd`, in declaration order, from
/// clap's matches.
fn extract_values(cmd: &ManifestCommand, matches: &ArgMatches) -> Vec<ArgValue> {
    let mut out = Vec::with_capacity(cmd.args.len());
    for (i, marg) in cmd.args.iter().enumerate() {
        let id = format!("arg{}", i);
        match marg {
            ManifestArg::Positional { quoted: q, many, checks, stdin, source, .. } => {
                if *many {
                    let toks: Vec<String> = matches
                        .get_many::<String>(&id)
                        .map(|it| it.cloned().collect())
                        .unwrap_or_default();
                    toks.iter().for_each(|t| crate::parse_arg::note_unparsed_token(t));
                    out.push(ArgValue::Many { tokens: toks, literal: *q });
                } else if *stdin {
                    // Omitted, it reads stdin, unless stdin is a terminal,
                    // where reading would block.
                    let raw = match matches.get_one::<String>(&id).cloned() {
                        Some(val) => val,
                        None => {
                            if unsafe { libc::isatty(0) } == 1 {
                                crate::runlog::die_with_error(&format!(
                                    "argument #{}: no input given and stdin is a terminal; \
                                     pass a file path, `-` for stdin, or pipe data in",
                                    i
                                ));
                            }
                            "/dev/stdin".to_string()
                        }
                    };
                    out.push(token_value(raw, marg, checks, *source, *q, i));
                } else {
                    match matches.get_one::<String>(&id).cloned() {
                        Some(val) => out.push(token_value(val, marg, checks, *source, *q, i)),
                        None => out.push(ArgValue::Null),
                    }
                }
            }
            ManifestArg::Optional {
                quoted: q,
                default_val,
                many,
                checks,
                source,
                ..
            } => {
                // A default is passed as the manifest stores it, never
                // quoted again.
                use clap::parser::ValueSource;
                let from_cli = matches.value_source(&id)
                    == Some(ValueSource::CommandLine);
                if *many {
                    if from_cli {
                        let toks: Vec<String> = matches
                            .get_many::<String>(&id)
                            .map(|it| it.cloned().collect())
                            .unwrap_or_default();
                        toks.iter().for_each(|t| crate::parse_arg::note_unparsed_token(t));
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
                    out.push(token_value(v, marg, checks, *source, *q, i));
                } else if let Some(def) = default_val {
                    out.push(ArgValue::Value(def.clone()));
                } else {
                    out.push(ArgValue::Null);
                }
            }
            ManifestArg::Flag {
                default_val,
                long_rev,
                short_rev,
                ..
            } => {
                let fwd_set = matches.get_flag(&id);
                let neg_id = format!("{}_neg", id);
                let has_rev = long_rev.is_some() || short_rev.is_some();
                let neg_set = has_rev && matches.get_flag(&neg_id);
                let bool_val = if fwd_set {
                    true
                } else if neg_set {
                    false
                } else {
                    morloc_manifest::call_args::flag_default(default_val.as_deref())
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
                // `fields[j]` is set only for a field given on the command
                // line: a set field overrides the whole-record value, so a
                // default there would mask it.
                use clap::parser::ValueSource;
                let mut fields = Vec::with_capacity(entries.len());
                let mut defaults = Vec::with_capacity(entries.len());
                for (j, entry) in entries.iter().enumerate() {
                    let eid = format!("{}_entry{}", id, j);
                    let entry_val: Option<String> = match &entry.arg {
                        ManifestArg::Flag { long_rev, short_rev, .. } => {
                            let fwd_set = matches.get_flag(&eid);
                            let neg_id = format!("{}_neg", eid);
                            let neg_set = (long_rev.is_some()
                                || short_rev.is_some())
                                && matches.get_flag(&neg_id);
                            if fwd_set {
                                Some("true".to_string())
                            } else if neg_set {
                                Some("false".to_string())
                            } else {
                                None
                            }
                        }
                        ManifestArg::Optional { quoted: q, checks, source, many, .. } => {
                            let from_cli = matches.value_source(&eid)
                                == Some(ValueSource::CommandLine);
                            if from_cli && *many {
                                let toks: Vec<String> = matches
                                    .get_many::<String>(&eid)
                                    .map(|it| it.cloned().collect())
                                    .unwrap_or_default();
                                toks.iter().for_each(|t| crate::parse_arg::note_unparsed_token(t));
                                Some(many_field_json(&toks, *q, &entry.key))
                            } else if from_cli {
                                matches.get_one::<String>(&eid).cloned().map(|v|
                                    preprocess_cli_value(v, checks, *source, *q,
                                        &format!("record entry '{}'", entry.key))
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
            ManifestArg::Alt { arms, default_val, .. } => {
                // The chosen arm as JSON: the bare name for an argument-free
                // constructor, else the name keyed to its values, each read
                // as an argument of its type would be.
                let mut chosen: Option<String> = None;
                for (j, arm) in arms.iter().enumerate() {
                    let aid = format!("{}_arm{}", id, j);
                    if arm.fields.is_empty() {
                        if matches.get_flag(&aid) {
                            chosen = Some(serde_json::to_string(&arm.ctor).unwrap_or_default());
                            break;
                        }
                    } else if let Some(vals) = matches.get_many::<String>(&aid) {
                        let mut fields_json: Vec<String> = Vec::with_capacity(arm.fields.len());
                        for (v, f) in vals.zip(arm.fields.iter()) {
                            let js = match f.schema.as_deref() {
                                Some(sch) => {
                                    let tok = arm_field_token(v, sch);
                                    match crate::dispatch::cli_token_to_json(&tok, sch) {
                                        Ok(js) => js,
                                        Err(e) => crate::runlog::die_with_error(&format!(
                                            "--{}: {}", arm.long, e
                                        )),
                                    }
                                }
                                None => v.clone(),
                            };
                            fields_json.push(js);
                        }
                        chosen = Some(format!(
                            "{{{}:[{}]}}",
                            serde_json::to_string(&arm.ctor).unwrap_or_default(),
                            fields_json.join(",")
                        ));
                        break;
                    }
                }
                match chosen {
                    Some(js) => out.push(ArgValue::Value(js)),
                    None => match default_val {
                        Some(d) => out.push(ArgValue::Value(d.clone())),
                        None => out.push(ArgValue::Null),
                    },
                }
            }
        }
    }
    out
}

/// Join the manifest's epilogue blocks into one help footer: lines within a
/// block as written, a blank line between blocks. Empty blocks drop out.
fn render_epilogues(epilogues: &[Vec<String>]) -> String {
    epilogues
        .iter()
        .map(|block| {
            block
                .iter()
                .map(|line| line.trim_end())
                .collect::<Vec<_>>()
                .join("\n")
        })
        .filter(|block| !block.trim().is_empty())
        .collect::<Vec<_>>()
        .join("\n\n")
}

/// Append a block to a command's `after_help`, keeping whatever is already
/// there. A blank `block` leaves the command untouched.
fn append_after_help(cmd: ClapCommand, block: &str) -> ClapCommand {
    if block.is_empty() {
        return cmd;
    }
    let existing = cmd
        .get_after_help()
        .map(|s| s.to_string())
        .unwrap_or_default();
    let merged = if existing.is_empty() {
        block.to_string()
    } else {
        format!("{existing}\n\n{block}")
    };
    cmd.after_help(leak(&merged))
}

/// The description of a program whose args live on the root: the module's
/// docstring, then the sole command's, either alone when the other is empty.
fn single_root_desc(module_desc: &[String], cmd_desc: &[String]) -> Vec<String> {
    if module_desc.is_empty() {
        return cmd_desc.to_vec();
    }
    if cmd_desc.is_empty() || cmd_desc == module_desc {
        return module_desc.to_vec();
    }
    let mut out = module_desc.to_vec();
    out.push(String::new());
    out.extend(cmd_desc.iter().cloned());
    out
}

/// Help text for a boolean flag's reverse spelling, naming the forward
/// flag it turns off so the pair reads as one setting.
fn reverse_flag_help(long_opt: Option<&str>, short_opt: Option<&str>) -> String {
    match (long_opt, short_opt) {
        (Some(l), _) => format!("Turn --{} off", l),
        (None, Some(s)) => format!("Turn -{} off", s),
        (None, None) => "Turn the flag off".to_string(),
    }
}

/// Give a command its description as the help tier shows it. `about`, which
/// the parent's command list also shows, is always the first line; from
/// the second tier up the command's own page shows every line.
fn with_description(cmd: ClapCommand, desc: &[String], level: u8) -> ClapCommand {
    let first = first_desc(desc);
    if first.is_empty() {
        return cmd;
    }
    let cmd = cmd.about(leak(first));
    if level < 2 || desc.len() < 2 {
        return cmd;
    }
    let full = desc
        .iter()
        .map(|l| l.trim_end())
        .collect::<Vec<_>>()
        .join("\n");
    cmd.help_template(leak(&format!(
        "{full}\n\n{{usage-heading}} {{usage}}\n\n{{all-args}}{{after-help}}"
    )))
}

/// The first non-empty description line.
pub(crate) fn first_desc(desc: &[String]) -> &str {
    desc.iter()
        .find(|d| !d.trim().is_empty())
        .map(|s| s.as_str())
        .unwrap_or("")
}

/// A constructor field's token, given the readings a positional of the
/// same type gets before it is parsed: a string field takes the token as
/// the string itself, the way a `Str` positional is quoted, and a real
/// field takes `inf`/`nan` spellings.
fn arm_field_token(tok: &str, schema: &str) -> String {
    let parsed = parse_schema(schema).ok();
    if parsed.as_ref().is_some_and(crate::dispatch::is_float_scalar) {
        if let Some(js) = crate::dispatch::maybe_float_special_to_json(tok) {
            return js;
        }
    }
    let is_str = parsed.map_or(false, |s| match s.serial_type {
        SerialType::String => true,
        SerialType::Optional => s
            .parameters
            .first()
            .map_or(false, |p| p.serial_type == SerialType::String),
        SerialType::Nil | SerialType::Bool | SerialType::Sint8 | SerialType::Sint16 | SerialType::Sint32 | SerialType::Sint64 | SerialType::Uint8 | SerialType::Uint16 | SerialType::Uint32 | SerialType::Uint64 | SerialType::Float32 | SerialType::Float64 | SerialType::Array | SerialType::Tuple | SerialType::Map | SerialType::Int | SerialType::Table | SerialType::Recur | SerialType::IFile | SerialType::OStream | SerialType::IStream | SerialType::Variant | SerialType::Enum => false,
    });
    if is_str {
        crate::dispatch::quoted(tok)
    } else {
        tok.to_string()
    }
}

/// The default as the command line shows and accepts it: a constructor
/// default, stored as a JSON string, unquoted to the bare word.
fn shown_default(default_val: &str, ctor_typed: bool) -> String {
    let d = default_val.trim();
    let is_quoted = d.len() >= 2 && d.starts_with('"') && d.ends_with('"');
    if is_quoted && ctor_typed {
        d[1..d.len() - 1].to_string()
    } else {
        default_val.to_string()
    }
}

/// The help text of a manifest arg: its description lines, then its type,
/// constructors, format and default, each on its own line.
fn render_arg_help(
    desc: &[String],
    type_desc: Option<&str>,
    default_val: Option<&str>,
    format_hint: Option<&str>,
    ctor_line: Option<String>,
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
    // A `data` type's name says nothing about which words are legal here.
    let is_ctor_typed = ctor_line.is_some();
    if let Some(l) = ctor_line {
        lines.push(l);
    }
    if let Some(f) = format_hint {
        if !f.trim().is_empty() {
            lines.push(format!("format: {}", f));
        }
    }
    if let Some(d) = default_val {
        if !d.trim().is_empty() {
            let shown = if is_ctor_typed {
                d.trim().trim_matches('"')
            } else {
                d
            };
            lines.push(format!("default: {}", shown));
        }
    }
    lines.join("\n")
}

/// The help line listing an argument's `@parse` formats, each with the
/// extensions that select it, e.g.
/// `formats: fastq (.fq .fastq), fasta (.fa), morloc (the default)`.
fn formats_line(parse: Option<&crate::manifest::ArgParse>) -> Option<String> {
    let p = parse?;
    let mut items: Vec<String> = p
        .formats
        .iter()
        .map(|f| {
            if f.exts.is_empty() {
                f.name.clone()
            } else {
                format!("{} ({})", f.name, f.exts.join(" "))
            }
        })
        .collect();
    items.push("morloc (the default)".to_string());
    Some(format!("formats: {}", items.join(", ")))
}

/// The "Positional arguments:" help block, a numbered list:
///
/// ```text
/// Positional arguments:
///   1:  the first thing
///       type: UInt8
///   2:  type: [UInt8]
///   3:  type: Color
///       values: Red, Green, Blue
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
    let labels: Vec<String> = positionals
        .iter()
        .enumerate()
        .map(|(i, marg)| {
            let mv = match marg {
                ManifestArg::Positional { metavar, .. } => metavar.as_deref(),
                _ => unreachable!("filtered to Positional only"),
            };
            match mv.map(str::trim).filter(|m| !m.is_empty()) {
                Some(m) => format!("{:>width$}: {}", i + 1, m, width = idx_width),
                None => format!("{:>width$}:", i + 1, width = idx_width),
            }
        })
        .collect();
    let label_width = labels.iter().map(String::len).max().unwrap_or(0);
    let mut out = String::from("Positional arguments:");
    for (i, marg) in positionals.iter().enumerate() {
        let prefix = format!("  {:<width$}  ", labels[i], width = label_width);
        let cont = " ".repeat(prefix.len());
        let (type_desc, desc, format_hint, schema, stdin) = match marg {
            ManifestArg::Positional { type_desc, desc, format, schema, stdin, .. } => (
                type_desc.as_deref(),
                desc.as_slice(),
                format.as_deref(),
                schema.as_deref(),
                *stdin,
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
        if let Some(l) = crate::json_help::schema_constructor_line(schema) {
            lines.push(l);
        }
        if let Some(f) = format_hint {
            if !f.trim().is_empty() {
                lines.push(format!("format: {}", f));
            }
        }
        if let Some(l) = formats_line(marg.parse()) {
            lines.push(l);
        }
        // `?T` says the value may be null, not that the slot may be omitted.
        if !stdin && crate::json_help::schema_is_optional(schema) {
            lines.push(String::from("optional: omit for null"));
        }
        if lines.is_empty() {
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
    #[test]
    fn a_symlink_and_its_target_resolve_to_one_file() {
        let dir = std::env::temp_dir().join(format!("resolve-target-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let real = dir.join("real.txt");
        let dangling_target = dir.join("later.txt");
        std::fs::write(&real, b"x").unwrap();
        std::os::unix::fs::symlink("real.txt", dir.join("link.txt")).unwrap();
        std::os::unix::fs::symlink("later.txt", dir.join("dangling.txt")).unwrap();
        let r = |name: &str| resolve_target(dir.join(name).to_str().unwrap()).file().to_path_buf();
        let t = |name: &str| resolve_target(dir.join(name).to_str().unwrap());
        assert!(t("link.txt") == t("real.txt"));
        assert!(t("./real.txt".trim_start_matches("./")) == resolve_target(&format!("{}/./real.txt", dir.display())));
        assert_eq!(r("link.txt"), r("real.txt"));
        assert_eq!(r("dangling.txt"), r("later.txt"));
        assert_eq!(r("dangling.txt"), std::fs::canonicalize(&dir).unwrap().join("later.txt"));
        assert!(!dangling_target.exists());
        assert_ne!(r("real.txt"), r("other.txt"));
        std::fs::remove_dir_all(&dir).unwrap();
    }

    use super::*;

    #[test]
    fn targets_that_may_be_one_file_are_caught() {
        let dir = std::env::temp_dir().join(format!("phase2-alias-{}", std::process::id()));
        std::fs::create_dir_all(dir.join("sub")).unwrap();
        let at = |n: &str| resolve_target(dir.join(n).to_str().unwrap());
        // Case: one file on a case-insensitive filesystem.
        assert!(at("Out.txt").may_name_same_file(&at("out.txt")));
        // Different directories or names are different files.
        assert!(!at("out.txt").may_name_same_file(&at("sub/out.txt")));
        assert!(!at("a.txt").may_name_same_file(&at("b.txt")));
        // A hard link is one file under two names.
        std::fs::write(dir.join("a.txt"), "x").unwrap();
        std::fs::hard_link(dir.join("a.txt"), dir.join("b.txt")).unwrap();
        let linked = at("a.txt").may_name_same_file(&at("b.txt"));
        let _ = std::fs::remove_dir_all(&dir);
        assert!(linked);
    }
    use morloc_manifest::parse_manifest;

    /// A manifest JSON payload around `commands_json` and `groups_json`.
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
                        {"kind": "pos", "key": "_1", "schema": "i8", "type": "Int", "metavar": "X", "quoted": false, "desc": [], "constraints": [], "metadata": {}},
                        {"kind": "pos", "key": "_2", "schema": "i8", "type": "Int", "metavar": "Y", "quoted": false, "desc": [], "constraints": [], "metadata": {}}
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
        let h = render_arg_help(
            &["Take the first integer".into()],
            Some("Int"),
            None,
            None,
            None,
        );
        assert_eq!(h, "Take the first integer\ntype: Int");

        let h = render_arg_help(&[], Some("Real"), None, None, None);
        assert_eq!(h, "type: Real");

        let h = render_arg_help(
            &["first".into(), "second".into()],
            Some("Str"),
            None,
            None,
            None,
        );
        assert_eq!(h, "first\nsecond\ntype: Str");

        let h = render_arg_help(
            &["Verbose output".into()],
            Some("Bool"),
            Some("false"),
            None,
            None,
        );
        assert_eq!(h, "Verbose output\ntype: Bool\ndefault: false");

        let h = render_arg_help(&["just a desc".into()], None, None, None, None);
        assert_eq!(h, "just a desc");

        let h = render_arg_help(
            &["readable file path".into()],
            Some("Str"),
            None,
            Some("must be the path of an existing readable file"),
            None,
        );
        assert_eq!(
            h,
            "readable file path\ntype: Str\nformat: must be the path of an existing readable file"
        );
    }

    #[test]
    fn arg_help_names_a_data_types_constructors() {
        use crate::json_help::{group_entry_constructor_line, schema_constructor_line};

        let h = render_arg_help(
            &[],
            Some("Color"),
            None,
            None,
            schema_constructor_line(Some("e33Red5Green4Blue")),
        );
        assert_eq!(h, "type: Color\nvalues: Red, Green, Blue");

        // Neither an optional nor a list changes which names are legal.
        assert_eq!(
            schema_constructor_line(Some("ae33Red5Green4Blue")).as_deref(),
            Some("values: Red, Green, Blue")
        );
        assert_eq!(
            schema_constructor_line(Some("?e33Red5Green4Blue")).as_deref(),
            Some("values: Red, Green, Blue")
        );

        assert_eq!(
            schema_constructor_line(Some("v36Circle1f84Rect2f8f83Dot0")).as_deref(),
            Some("constructors: Circle/1, Rect/2, Dot")
        );

        assert_eq!(
            group_entry_constructor_line(Some("m14basee33Red5Green4Blue"), "base").as_deref(),
            Some("values: Red, Green, Blue")
        );
        assert_eq!(
            group_entry_constructor_line(Some("m14basee33Red5Green4Blue"), "missing"),
            None
        );

        // A type with no constructor set adds no line.
        let h = render_arg_help(&[], Some("Int"), None, None, schema_constructor_line(Some("i4")));
        assert_eq!(h, "type: Int");
    }

    #[test]
    fn positional_block_collapses_type_into_index_when_no_desc() {
        let m = fixture_single_add();
        let block = render_positional_block(&m.commands[0]);
        assert_eq!(
            block,
            "Positional arguments:\n  1: X  type: Int\n  2: Y  type: Int"
        );
    }

    #[test]
    fn positional_block_omits_absent_metavar_and_keeps_columns_aligned() {
        let json = wrap_manifest(
            r#"[
                {
                    "name": "scan",
                    "type": "remote",
                    "mid": 1,
                    "pool": 0,
                    "needed_pools": [0],
                    "desc": ["Search"],
                    "args": [
                        {"kind": "pos", "key": "_1", "schema": "s", "type": "Str", "metavar": "PATTERN", "quoted": false, "desc": ["the text to find"], "constraints": [], "metadata": {}},
                        {"kind": "pos", "key": "_2", "schema": "s", "type": "Str", "quoted": false, "desc": ["where to look"], "constraints": [], "metadata": {}}
                    ],
                    "return": {"schema": "s", "type": "Str", "desc": [], "constraints": [], "metadata": {}},
                    "constraints": [],
                    "metadata": {},
                    "group": null
                }
            ]"#,
            "[]",
        );
        let m = parse_manifest(&json).unwrap();
        let block = render_positional_block(&m.commands[0]);
        assert_eq!(
            block,
            concat!(
                "Positional arguments:\n",
                "  1: PATTERN  the text to find\n",
                "              type: Str\n",
                "  2:          where to look\n",
                "              type: Str"
            )
        );
    }

    #[test]
    fn single_command_positionals_extracted_in_order() {
        let m = fixture_single_add();
        let parsed = parse_run(&m, &["3".into(), "5".into()], "add", false, None);
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

    #[test]
    fn single_command_accepts_negative_number_positional() {
        let m = fixture_single_add();
        let parsed = parse_run(&m, &["-5".into(), "-7".into()], "add", false, None);
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

    /// In single-command mode `./prog add 3 5` and `./prog 3 5` parse alike.
    #[test]
    fn single_command_accepts_explicit_command_name_prefix() {
        let m = fixture_single_add();
        let parsed = parse_run(
            &m,
            &["add".into(), "3".into(), "5".into()],
            "add",
            false,
            None,
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
        let parsed = parse_run(&m, &["beta".into()], "main", false, None);
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
                        {"kind": "opt", "key": "name", "schema": "s", "type": "Str", "metavar": "NAME", "quoted": false, "short": "n", "long": "name", "default": "world", "desc": [], "constraints": [], "metadata": {}}
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
        let parsed = parse_run(&m, &["--name".into(), "Zeb".into()], "greet", false, None);
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "Zeb"),
            _ => panic!("expected Value"),
        }
    }

    #[test]
    fn optional_default_used_when_absent() {
        let m = fixture_optional_with_default();
        let parsed = parse_run(&m, &[], "greet", false, None);
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
                        {"kind": "flag", "key": "verbose", "short": "v", "long": "verbose", "long_rev": null, "default": "false", "desc": [], "metadata": {}}
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
        let parsed = parse_run(&m, &["--verbose".into()], "go", false, None);
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "true"),
            _ => panic!("expected Value"),
        }
    }

    #[test]
    fn flag_absent_uses_default() {
        let m = fixture_flag();
        let parsed = parse_run(&m, &[], "go", false, None);
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "false"),
            _ => panic!("expected Value"),
        }
    }

    #[test]
    fn flag_with_short_form_set_becomes_true() {
        let m = fixture_flag();
        let parsed = parse_run(&m, &["-v".into()], "go", false, None);
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "true"),
            _ => panic!("expected Value"),
        }
    }

    /// One `@literal` Str positional.
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
                        {"kind": "pos", "key": "_1", "schema": "s", "type": "Str", "metavar": "S", "quoted": true, "desc": [], "constraints": [], "metadata": {}}
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
        let parsed = parse_run(&m, &[r#"hello "world""#.into()], "echo", false, None);
        match &parsed.values[0] {
            ArgValue::Value(v) => {
                let decoded: String = serde_json::from_str(v).unwrap();
                assert_eq!(decoded, r#"hello "world""#);
            }
            _ => panic!("expected Value"),
        }
    }

    /// `bar` with one `@literal` Optional whose manifest default is the
    /// JSON string `"yolo"`.
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
                        {"kind": "opt", "key": "yolo", "schema": "s", "type": "Str", "metavar": "S", "quoted": true, "short": "y", "long": "yolo", "default": "\"yolo\"", "desc": [], "constraints": [], "metadata": {}}
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
        let parsed = parse_run(&m, &[], "bar", false, None);
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "\"yolo\""),
            _ => panic!("expected Value"),
        }
    }

    #[test]
    fn literal_optional_cli_value_is_json_quoted() {
        let m = fixture_literal_optional_with_default();
        let parsed = parse_run(&m, &["-y".into(), "a".into()], "bar", false, None);
        match &parsed.values[0] {
            ArgValue::Value(v) => assert_eq!(v, "\"a\""),
            _ => panic!("expected Value"),
        }
    }

    /// `foo` taking one record with a whole-record option `--alg-config`
    /// and per-field `-m` and `-n` options that have defaults.
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
                            "key": "opts",
                            "schema": "m22m1Int52m1Int5",
                            "type": "AlgConfig",
                            "metavar": "ALG_CONFIG",
                            "desc": [],
                            "group_opt": {"short": null, "long": "alg-config"},
                            "entries": [
                                {"key": "m", "arg": {"kind": "opt", "key": "m", "schema": "i4", "type": "Int", "metavar": "INT", "quoted": false, "short": "m", "long": null, "default": "0", "desc": [], "constraints": [], "metadata": {}}},
                                {"key": "n", "arg": {"kind": "opt", "key": "n", "schema": "i4", "type": "Int", "metavar": "INT", "quoted": false, "short": "n", "long": "nosy", "default": "0", "desc": [], "constraints": [], "metadata": {}}}
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
        let m = fixture_record_group_with_grpopt();
        let parsed = parse_run(
            &m,
            &["--alg-config=algconf.json".into()],
            "foo",
            false,
            None,
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
            false,
            None,
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

#[cfg(test)]
mod help_level_tests {
    use super::*;
    use morloc_manifest::parse_manifest;

    /// Multi-command manifest with everything the help tiers disclose
    /// progressively: a two-line module description, a module epilogue,
    /// a two-line command description, a command epilogue, and a named
    /// record type in the command's signature.
    fn fixture_tiers() -> Manifest {
        let v = env!("CARGO_PKG_VERSION");
        let json = format!(
            r#"{{
                "name": "main",
                "build": {{"path": "/tmp/test", "time": 0, "morloc_version": "{}"}},
                "pools": [
                    {{"lang": "py", "exec": ["python3", "pool.py"], "socket": "pipe-py", "metadata": {{}}}}
                ],
                "desc": ["A demo toolbox", "It does several things."],
                "epilogues": [["Examples:", "  main add 1 2"]],
                "commands": [
                    {{
                        "name": "add",
                        "type": "remote",
                        "mid": 1,
                        "pool": 0,
                        "needed_pools": [0],
                        "desc": ["Add two integers", "Overflow is not checked."],
                        "epilogues": [["Usage notes:", "  add 1 2"]],
                        "args": [
                            {{"kind": "pos", "key": "_1", "schema": "i8", "type": "Int", "metavar": "X", "quoted": false, "desc": [], "constraints": [], "metadata": {{}}}}
                        ],
                        "return": {{"schema": "{{a:i8}}", "type": "Pair", "desc": [], "constraints": [], "metadata": {{}}}},
                        "named_types": [
                            {{"name": "Pair", "kind": "record", "fields": [{{"key": "a", "type": "Int"}}]}}
                        ],
                        "constraints": [],
                        "metadata": {{}},
                        "group": null
                    }},
                    {{"name": "beta", "type": "remote", "mid": 2, "pool": 0, "needed_pools": [0], "desc": ["Do beta"], "args": [], "return": {{"schema": "z", "type": "Unit", "desc": [], "constraints": [], "metadata": {{}}}}, "constraints": [], "metadata": {{}}, "group": null}}
                ],
                "groups": [],
                "capabilities": ["log"],
                "metadata": {{}}
            }}"#,
            v
        );
        parse_manifest(&json).unwrap()
    }

    fn s(x: &str) -> String {
        x.to_string()
    }

    #[test]
    fn help_level_counts_every_h_and_help() {
        assert_eq!(help_level(&[]), 1);
        assert_eq!(help_level(&[s("add"), s("1")]), 1);
        assert_eq!(help_level(&[s("-h")]), 1);
        assert_eq!(help_level(&[s("--help")]), 1);
        assert_eq!(help_level(&[s("-hh")]), 2);
        assert_eq!(help_level(&[s("add"), s("-h"), s("-h")]), 2);
        assert_eq!(help_level(&[s("--help"), s("--help")]), 2);
        assert_eq!(help_level(&[s("-h"), s("--help"), s("-h")]), 3);
        assert_eq!(help_level(&[s("-hhh")]), 3);
        assert_eq!(help_level(&[s("-hhhhhh")]), 3);
        // Clustered with another short flag.
        assert_eq!(help_level(&[s("-qhh")]), 2);
        // `--` ends option parsing; an `h` after it is a positional.
        assert_eq!(help_level(&[s("--"), s("-hhh")]), 1);
        // A negative number is not a short cluster.
        assert_eq!(help_level(&[s("-5h")]), 1);
        // A long option other than --help is never counted.
        assert_eq!(help_level(&[s("--hhh")]), 1);
    }

    fn root_help(level: u8) -> String {
        build_root(&fixture_tiers(), "main", level).render_help().to_string()
    }

    fn sub_help(level: u8) -> String {
        let mut root = build_root(&fixture_tiers(), "main", level);
        root.find_subcommand_mut("add").unwrap().render_help().to_string()
    }

    #[test]
    fn level_one_shows_synopsis_and_arguments_only() {
        let top = root_help(1);
        assert!(top.contains("A demo toolbox"));
        assert!(!top.contains("It does several things."));
        assert!(!top.contains("Examples:"));
        assert!(!top.contains("Nexus Options"));
        assert!(top.contains("  add   Add two integers"));

        let sub = sub_help(1);
        assert!(sub.contains("Add two integers"));
        assert!(!sub.contains("Overflow is not checked."));
        assert!(!sub.contains("Usage notes:"));
        assert!(sub.contains("Positional arguments:"));
        assert!(sub.contains("Return: Pair"));
        assert!(!sub.contains("Record Schemas:"));
        assert!(!sub.contains("Nexus Options"));
    }

    #[test]
    fn level_two_adds_description_and_epilogues() {
        let top = root_help(2);
        assert!(top.contains("A demo toolbox\nIt does several things."));
        // The command list stays one line per command at every tier.
        assert!(top.contains("  add   Add two integers\n  beta  Do beta\n"));
        assert!(top.contains("Examples:\n  main add 1 2"));
        assert!(!top.contains("Nexus Options"));

        let sub = sub_help(2);
        assert!(sub.contains("Add two integers\nOverflow is not checked."));
        assert!(sub.contains("Usage notes:\n  add 1 2"));
        assert!(!sub.contains("Record Schemas:"));
    }

    #[test]
    fn level_three_adds_schemas() {
        let sub = sub_help(3);
        assert!(sub.contains("Overflow is not checked."));
        assert!(sub.contains("Usage notes:"));
        assert!(sub.contains("Record Schemas:\n  Pair\n    a :: Int"));
        assert!(!sub.contains("Nexus Options"));
    }

    #[test]
    fn help_flag_documents_its_repetition() {
        let sub = sub_help(1);
        assert!(sub.contains("-hh"));
        assert!(sub.contains("-hhh"));
        assert!(sub.contains("-h @"));
    }

    /// clap renders the help flag as an error whose text is the help
    /// page. `--help` must produce the same page as `-h` at the same
    /// level: the tier is the only knob, never the spelling.
    fn help_via_clap(level: u8, argv: &[&str]) -> String {
        let root = build_root(&fixture_tiers(), "main", level);
        let mut full = vec!["main"];
        full.extend_from_slice(argv);
        let err = root.try_get_matches_from(full).unwrap_err();
        assert_eq!(err.kind(), clap::error::ErrorKind::DisplayHelp);
        err.to_string()
    }

    #[test]
    fn long_help_spelling_renders_the_same_page_as_short() {
        for level in 1..=3 {
            assert_eq!(
                help_via_clap(level, &["add", "-h"]),
                help_via_clap(level, &["add", "--help"]),
            );
            assert_eq!(
                help_via_clap(level, &["-h"]),
                help_via_clap(level, &["--help"]),
            );
        }
        // The repeated spelling is parsed by clap as a cluster and
        // still lands on the help page.
        assert_eq!(
            help_via_clap(3, &["add", "-hhh"]),
            help_via_clap(3, &["add", "-h"]),
        );
    }

    /// `structure` returns a `Molecule` and has two output actions: a
    /// `@render` `--mol` and a `@with` `-c/--count`. `mol_default` sets
    /// `@default` on `--mol`.
    fn fixture_actions(mol_default: bool) -> Manifest {
        let v = env!("CARGO_PKG_VERSION");
        let ret = |ty: &str, mime: &str, desc: &str| {
            format!(
                r#"{{"schema": "z", "type": "{}", "mime": {}, "desc": [{}], "constraints": [], "metadata": {{}}}}"#,
                ty, mime, desc
            )
        };
        let cmd = |name: &str, internal: bool, terminals: &str, ret: String| {
            format!(
                r#"{{"name": "{}", "type": "remote", "mid": 1, "pool": 0, "needed_pools": [0], "desc": [], "args": [], "return": {}, "constraints": [], "metadata": {{}}, "group": null, "internal": {}, "terminals": [{}]}}"#,
                name, ret, internal, terminals
            )
        };
        let terminals = format!(
            r#"{{"long": "mol", "entry": "mlcp_structure_mol", "render": true, "default": {}}},
               {{"short": "c", "long": "count", "entry": "mlcp_structure_count"}}"#,
            mol_default
        );
        let json = format!(
            r#"{{
                "name": "main",
                "build": {{"path": "/tmp/test", "time": 0, "morloc_version": "{}"}},
                "pools": [
                    {{"lang": "py", "exec": ["python3", "pool.py"], "socket": "pipe-py", "metadata": {{}}}}
                ],
                "commands": [{}, {}, {}],
                "groups": [],
                "metadata": {{}}
            }}"#,
            v,
            cmd("structure", false, &terminals, ret("Molecule", "null", r#""Atoms and bonds", "with hydrogens""#)),
            cmd("mlcp_structure_mol", true, "", ret("[U8]", r#""chemical/x-mdl-molfile""#, "")),
            cmd("mlcp_structure_count", true, "", ret("Int", "null", "")),
        );
        parse_manifest(&json).unwrap()
    }

    fn return_block(m: &Manifest) -> String {
        render_return_block(m.command_by_name("structure").unwrap(), m)
    }

    #[test]
    fn return_block_marks_the_default_action() {
        assert_eq!(
            return_block(&fixture_actions(true)),
            "Return:\n\
             \x20 type: Molecule\n\
             \x20 desc: Atoms and bonds\n\
             \x20       with hydrogens\n\
             \x20 actions:\n\
             \x20   --mol:      chemical/x-mdl-molfile  (raw bytes)  default\n\
             \x20   -c/--count: Int"
        );
    }

    #[test]
    fn return_block_without_default_marks_nothing() {
        assert_eq!(
            return_block(&fixture_actions(false)),
            "Return:\n\
             \x20 type: Molecule\n\
             \x20 desc: Atoms and bonds\n\
             \x20       with hydrogens\n\
             \x20 actions:\n\
             \x20   --mol:      chemical/x-mdl-molfile  (raw bytes)\n\
             \x20   -c/--count: Int"
        );
    }
}
