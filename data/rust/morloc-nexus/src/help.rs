//! Help text generation matching the C nexus output format.

use crate::manifest::{Arg, Command, GroupEntry, Manifest};

/// Print nexus-level usage (no manifest loaded).
pub fn print_nexus_usage(prog_name: &str) -> ! {
    eprintln!("Usage: {} [OPTION...] COMMAND [ARG...]", prog_name);
    eprintln!();
    eprintln!("morloc-nexus is the morloc program dispatcher.");
    eprintln!();
    eprintln!("Arguments:");
    eprintln!("  <manifest>           Path to a .manifest file or wrapper script");
    eprintln!();
    eprintln!("Nexus options:");
    eprintln!("  -h, --help           Print this help message");
    eprintln!("  -p, --print          Pretty-print output for human consumption");
    eprintln!("  -o, --output-file    Print to this file instead of STDOUT");
    eprintln!("  -f, --output-format  Output format [json|mpk|voidstar]");
    eprintln!();
    eprintln!("Daemon mode:");
    eprintln!("  --daemon             Run as a long-lived daemon");
    eprintln!("  --http-port PORT     Listen on HTTP port");
    eprintln!("  --port PORT          Listen on TCP port");
    eprintln!("  --socket PATH        Listen on Unix socket");
    eprintln!("  --eval-timeout SECS  Timeout for /eval requests (default: 30)");
    eprintln!();
    eprintln!("Router mode:");
    eprintln!("  --router             Run as a multi-program router");
    eprintln!("  --fdb <path>         Path to fdb manifest directory");
    std::process::exit(0);
}

/// Print usage for a multi-command program.
pub fn print_usage(prog_name: &str, manifest: &Manifest) -> ! {
    eprintln!("Usage: {} [OPTION...] COMMAND [ARG...]", prog_name);

    // Module-level description
    if !manifest.desc.is_empty() {
        eprintln!();
        for line in &manifest.desc {
            eprintln!("{}", line);
        }
    }

    eprintln!();
    eprintln!("Nexus options:");
    eprintln!("  -h, --help           Print this help message");
    eprintln!("  -p, --print          Pretty-print output for human consumption");
    eprintln!("  -o, --output-file    Print to this file instead of STDOUT");
    eprintln!("  -f, --output-format  Output format [json|mpk|voidstar]");
    eprintln!();
    eprintln!("Daemon mode:");
    eprintln!("  --daemon             Run as a long-lived daemon");
    eprintln!("  --http-port PORT     Listen on HTTP port");
    eprintln!("  --port PORT          Listen on TCP port");
    eprintln!("  --socket PATH        Listen on Unix socket");
    eprintln!();

    // Ungrouped commands
    let ungrouped: Vec<&Command> = manifest
        .commands
        .iter()
        .filter(|c| c.group.is_none())
        .collect();

    if !ungrouped.is_empty() {
        eprintln!("Commands (call with -h/--help for more info):");
        let longest = ungrouped.iter().map(|c| c.name.len()).max().unwrap_or(0);
        for cmd in &ungrouped {
            eprint!("  {}", cmd.name);
            if let Some(first) = cmd.desc.first() {
                let pad = longest - cmd.name.len() + 2;
                eprint!("{:pad$}{}", "", first, pad = pad);
            }
            eprintln!();
        }
    }

    if !manifest.groups.is_empty() {
        if !ungrouped.is_empty() {
            eprintln!();
        }
        eprintln!("Command groups (call with -h/--help for more info):");
        let longest = manifest.groups.iter().map(|g| g.name.len()).max().unwrap_or(0);
        for grp in &manifest.groups {
            eprint!("  {}", grp.name);
            if let Some(first) = grp.desc.first() {
                let pad = longest - grp.name.len() + 2;
                eprint!("{:pad$}{}", "", first, pad = pad);
            }
            eprintln!();
        }
    }

    // Epilogues
    for epilogue in &manifest.epilogues {
        eprintln!();
        for line in epilogue {
            eprintln!("{}", line);
        }
    }

    std::process::exit(0);
}

/// Print usage for a command group.
pub fn print_group_usage(prog_name: &str, manifest: &Manifest, group_name: &str) -> ! {
    let grp = manifest.groups.iter().find(|g| g.name == group_name);

    eprintln!("Usage: {} {} COMMAND [ARG...]", prog_name, group_name);
    if let Some(g) = grp {
        if !g.desc.is_empty() {
            eprintln!();
            for line in &g.desc {
                eprintln!("{}", line);
            }
        }
    }
    eprintln!("\nCommands:");

    let cmds: Vec<&Command> = manifest
        .commands
        .iter()
        .filter(|c| c.group.as_deref() == Some(group_name))
        .collect();

    let longest = cmds.iter().map(|c| c.name.len()).max().unwrap_or(0);
    for cmd in &cmds {
        eprint!("  {}", cmd.name);
        if let Some(first) = cmd.desc.first() {
            let pad = longest - cmd.name.len() + 2;
            eprint!("{:pad$}{}", "", first, pad = pad);
        }
        eprintln!();
    }

    std::process::exit(0);
}

/// Print help for a specific subcommand.
pub fn print_command_help(prog_name: &str, cmd: &Command) -> ! {
    // Usage line
    if let Some(ref group) = cmd.group {
        eprint!("Usage: {} {} {}", prog_name, group, cmd.name);
    } else {
        eprint!("Usage: {} {}", prog_name, cmd.name);
    }
    print_usage_suffix(cmd);
    eprintln!();
    if !cmd.desc.is_empty() {
        eprintln!();
    }

    print_command_body(cmd);
    std::process::exit(0);
}

/// Print help for a single-command program.
pub fn print_command_help_single(prog_name: &str, cmd: &Command) -> ! {
    eprint!("Usage: {}", prog_name);
    print_usage_suffix(cmd);
    eprintln!();

    // Description
    if !cmd.desc.is_empty() {
        eprintln!();
        for (i, line) in cmd.desc.iter().enumerate() {
            if i == 0 && line.is_empty() {
                continue;
            }
            eprintln!("{}", line);
        }
    }

    // Nexus options
    eprintln!("\nNexus options:");
    eprintln!("  --print          Pretty-print output for human consumption");
    eprintln!("  --output-file    Print to this file instead of STDOUT");
    eprintln!("  --output-form    Output format [json|mpk|voidstar]");
    eprintln!("\nDaemon mode:");
    eprintln!("  --daemon         Run as a long-lived daemon");
    eprintln!("  --http-port PORT Listen on HTTP port");
    eprintln!("  --port PORT      Listen on TCP port");
    eprintln!("  --socket PATH    Listen on UNIX socket");

    print_args_body(cmd);
    print_type_definitions(cmd);
    print_return_info(cmd);
    std::process::exit(0);
}

// -- Helpers ----------------------------------------------------------------

fn print_usage_suffix(cmd: &Command) {
    let has_opts = cmd.args.iter().any(|a| !matches!(a, Arg::Positional { .. }));
    if has_opts {
        eprint!(" [OPTION...]");
    }
    for arg in &cmd.args {
        if let Arg::Positional { metavar, .. } = arg {
            eprint!(" {}", metavar.as_deref().unwrap_or("ARG"));
        }
    }
}

fn print_command_body(cmd: &Command) {
    // Description
    if !cmd.desc.is_empty() {
        for (i, line) in cmd.desc.iter().enumerate() {
            if i == 0 && line.is_empty() {
                continue;
            }
            eprintln!("{}", line);
        }
    }

    print_args_body(cmd);
    print_type_definitions(cmd);
    print_return_info(cmd);
}

fn print_args_body(cmd: &Command) {
    // Positional arguments
    let has_pos = cmd.args.iter().any(|a| matches!(a, Arg::Positional { .. }));
    if has_pos {
        eprintln!("\nPositional arguments:");
        for arg in &cmd.args {
            if let Arg::Positional {
                metavar,
                type_desc,
                desc,
                ..
            } = arg
            {
                eprint!("  {}", metavar.as_deref().unwrap_or("ARG"));
                if let Some(first) = desc.first() {
                    eprint!("  {}", first);
                }
                eprintln!();
                if let Some(td) = type_desc {
                    eprintln!("      type: {}", td);
                }
            }
        }
    }

    // Optional arguments (opts and flags)
    let has_opt = cmd
        .args
        .iter()
        .any(|a| matches!(a, Arg::Optional { .. } | Arg::Flag { .. }));
    if has_opt {
        eprintln!("\nOptional arguments:");
        for arg in &cmd.args {
            print_opt_or_flag(arg);
        }
    }

    // Group arguments
    for arg in &cmd.args {
        if let Arg::Group {
            metavar,
            desc,
            group_opt,
            entries,
            ..
        } = arg
        {
            eprintln!("\nGroup arguments:");
            eprint!("  {}", metavar.as_deref().unwrap_or(""));
            if let Some(first) = desc.first() {
                eprint!(": {}", first);
            }
            eprintln!();

            if let Some(go) = group_opt {
                eprint!("    ");
                if let Some(ref s) = go.short_opt {
                    eprint!("-{}, ", s);
                }
                if let Some(ref l) = go.long_opt {
                    eprint!("--{} {}", l, metavar.as_deref().unwrap_or(""));
                }
                eprintln!();
                eprintln!("        provide record as file or JSON string");
            }

            for entry in entries {
                print_group_entry(entry);
            }
        }
    }
}

fn print_opt_or_flag(arg: &Arg) {
    match arg {
        Arg::Optional {
            short_opt,
            long_opt,
            metavar,
            default_val,
            desc,
            type_desc,
            ..
        } => {
            eprint!("    ");
            match (short_opt.as_deref(), long_opt.as_deref()) {
                (Some(s), Some(l)) => eprint!(
                    "-{}, --{} {}",
                    s,
                    l,
                    metavar.as_deref().unwrap_or("")
                ),
                (Some(s), None) => {
                    eprint!("-{} {}", s, metavar.as_deref().unwrap_or(""))
                }
                (None, Some(l)) => eprint!(
                    "--{} {}",
                    l,
                    metavar.as_deref().unwrap_or("")
                ),
                _ => {}
            }
            eprintln!();
            if let Some(d) = default_val {
                eprintln!("        default: {}", d);
            }
            for d in desc {
                eprintln!("        {}", d);
            }
            if let Some(td) = type_desc {
                eprintln!("        type: {}", td);
            }
        }
        Arg::Flag {
            short_opt,
            long_opt,
            long_rev,
            default_val,
            desc,
            ..
        } => {
            eprint!("    ");
            match (short_opt.as_deref(), long_opt.as_deref()) {
                (Some(s), Some(l)) => eprint!("-{}, --{}", s, l),
                (Some(s), None) => eprint!("-{}", s),
                (None, Some(l)) => eprint!("--{}", l),
                _ => {}
            }
            eprintln!();
            if let Some(rev) = long_rev {
                eprintln!("    --{}", rev);
            }
            if let Some(d) = default_val {
                eprintln!("        default: {}", d);
            }
            for d in desc {
                eprintln!("        {}", d);
            }
        }
        _ => {}
    }
}

fn print_group_entry(entry: &GroupEntry) {
    let ea = &entry.arg;
    eprint!("    ");
    match ea {
        Arg::Optional {
            short_opt,
            long_opt,
            metavar,
            default_val,
            desc,
            ..
        } => {
            match (short_opt.as_deref(), long_opt.as_deref()) {
                (Some(s), Some(l)) => {
                    eprint!("-{}, --{}", s, l);
                    if let Some(m) = metavar {
                        eprint!(" {}", m);
                    }
                }
                (Some(s), None) => {
                    eprint!("-{}", s);
                    if let Some(m) = metavar {
                        eprint!(" {}", m);
                    }
                }
                (None, Some(l)) => {
                    eprint!("--{}", l);
                    if let Some(m) = metavar {
                        eprint!(" {}", m);
                    }
                }
                _ => {}
            }
            eprintln!();
            if let Some(d) = default_val {
                eprintln!("        default: {}", d);
            }
            for d in desc {
                eprintln!("        {}", d);
            }
        }
        Arg::Flag {
            short_opt,
            long_opt,
            default_val,
            desc,
            ..
        } => {
            match (short_opt.as_deref(), long_opt.as_deref()) {
                (Some(s), Some(l)) => eprint!("-{}, --{}", s, l),
                (Some(s), None) => eprint!("-{}", s),
                (None, Some(l)) => eprint!("--{}", l),
                _ => {}
            }
            eprintln!();
            if let Some(d) = default_val {
                eprintln!("        default: {}", d);
            }
            for d in desc {
                eprintln!("        {}", d);
            }
        }
        _ => {}
    }
}

fn print_return_info(cmd: &Command) {
    eprintln!("\nReturn: {}", cmd.ret.type_desc);
    for line in &cmd.ret.desc {
        eprintln!("  {}", line);
    }
}

// -- Schema-walking renderer for the Record / Table Schemas sections ---------
//
// In v2 the manifest no longer carries a parallel `type_definitions` list.
// The same information is reconstructed at help-render time by walking
// each command's args + return value: for every typed entry whose schema
// parses to a Map at the top level, we treat the entry's `type` name as
// the named-type label and its schema's keys + parameter schemas as the
// field list. The record-vs-table distinction comes from the entry's
// `kind` constraint.

/// A rendered named-type layout for the help output, sourced from a
/// parsed schema.
struct TypeLayout<'a> {
    name: &'a str,
    /// "record" | "object" | "table" -- comes from the `kind` constraint.
    kind: &'a str,
    /// (field_name, rendered_type)
    fields: Vec<(String, String)>,
}

/// Pretty-render a parsed `Schema` as a morloc-flavored type string,
/// suitable for the field-type column in the schemas block.
fn render_schema_type(s: &morloc_runtime::schema::Schema) -> String {
    use morloc_runtime::schema::SerialType::*;
    match s.serial_type {
        Nil => "()".into(),
        Bool => "Bool".into(),
        Sint8 => "Int8".into(),
        Sint16 => "Int16".into(),
        Sint32 => "Int".into(),
        Sint64 => "Int64".into(),
        Uint8 => "UInt8".into(),
        Uint16 => "UInt16".into(),
        Uint32 => "UInt32".into(),
        Uint64 => "UInt64".into(),
        Float32 => "Float32".into(),
        Float64 => "Real".into(),
        String => "Str".into(),
        Array => format!(
            "[{}]",
            s.parameters
                .first()
                .map(render_schema_type)
                .unwrap_or_else(|| "?".into())
        ),
        Tuple => {
            // `String` here is fully qualified because the surrounding
            // match brings `SerialType::String` into scope as a variant,
            // shadowing the std `String` type.
            let inner: Vec<std::string::String> =
                s.parameters.iter().map(render_schema_type).collect();
            format!("({})", inner.join(", "))
        }
        Map => {
            // A nested record-ish thing. Use the hint string when present
            // (which carries the language-specific concrete type name);
            // otherwise show an inline placeholder. Either way, the
            // nested record will be listed separately in the same
            // schema block if its name appears as another arg's type.
            s.hint.clone().unwrap_or_else(|| "{..}".into())
        }
        Optional => format!(
            "?{}",
            s.parameters
                .first()
                .map(render_schema_type)
                .unwrap_or_else(|| "?".into())
        ),
        Tensor => format!(
            "Tensor<{}>",
            s.parameters
                .first()
                .map(render_schema_type)
                .unwrap_or_else(|| "?".into())
        ),
    }
}

/// Try to extract a `TypeLayout` from a (name, schema_string, kind)
/// triple. Returns None if any input is missing or the schema does not
/// parse to a top-level Map. Tables (whose fields are arrays in the wire
/// schema) render their fields by the array's element type, mirroring
/// how the user wrote them in the source.
fn extract_named_layout<'a>(
    type_name: Option<&'a str>,
    schema_str: Option<&str>,
    kind: Option<&'a str>,
) -> Option<TypeLayout<'a>> {
    use morloc_runtime::schema::SerialType;
    let name = type_name?;
    let schema = schema_str?;
    let kind = kind?;
    let parsed = morloc_runtime::schema::parse_schema(schema).ok()?;
    if parsed.serial_type != SerialType::Map {
        return None;
    }
    // For a table, every field's wire schema is an Array -- peel one layer
    // off so the user sees `name :: Str` instead of `name :: [Str]`.
    let strip_array = kind == "table";
    let fields = parsed
        .keys
        .iter()
        .zip(parsed.parameters.iter())
        .map(|(k, p)| {
            let inner = if strip_array && p.serial_type == SerialType::Array {
                p.parameters.first().unwrap_or(p)
            } else {
                p
            };
            (k.clone(), render_schema_type(inner))
        })
        .collect();
    Some(TypeLayout { name, kind, fields })
}

/// Walk every arg + the return of a command. For each typed entry, try
/// to build a layout. Deduplicate by type name, preserving discovery
/// order so the rendering matches the order types appear in the
/// signature.
fn collect_command_layouts<'a>(cmd: &'a Command) -> Vec<TypeLayout<'a>> {
    use std::collections::HashSet;
    let mut seen: HashSet<&str> = HashSet::new();
    let mut out: Vec<TypeLayout<'a>> = Vec::new();

    for arg in &cmd.args {
        // Skip unrolled groups without a group_opt: each field already
        // appears as its own flag in the usage, so the schema is redundant.
        // Keep the schema when group_opt is present (the user can pass the
        // entire record as JSON and needs the full field spec).
        if let Arg::Group { group_opt: None, .. } = arg {
            continue;
        }
        if let Some(layout) =
            extract_named_layout(arg.type_desc_str(), arg.schema_str(), arg.kind_constraint())
        {
            if seen.insert(layout.name) {
                out.push(layout);
            }
        }
    }

    let ret_kind = cmd
        .ret
        .constraints
        .iter()
        .find(|c| c.ctype == "kind")
        .and_then(|c| c.value.as_ref().and_then(|v| v.as_str()));
    if let Some(layout) =
        extract_named_layout(Some(&cmd.ret.type_desc), Some(&cmd.ret.schema), ret_kind)
    {
        if seen.insert(layout.name) {
            out.push(layout);
        }
    }

    out
}

/// Print the Record Schemas / Table Schemas sections for any named
/// types referenced in this command's signature. The whole block is
/// skipped when there are none.
fn print_type_definitions(cmd: &Command) {
    let layouts = collect_command_layouts(cmd);
    if layouts.is_empty() {
        return;
    }

    let records: Vec<&TypeLayout> = layouts.iter().filter(|l| l.kind != "table").collect();
    let tables: Vec<&TypeLayout> = layouts.iter().filter(|l| l.kind == "table").collect();

    if !records.is_empty() {
        eprintln!("\nRecord Schemas:");
        print_layouts(&records);
    }
    if !tables.is_empty() {
        eprintln!("\nTable Schemas:");
        print_layouts(&tables);
    }
}

/// Render a list of layouts. Each layout shows its type name on its own
/// line followed by the field list with `::`-aligned column widths.
/// Definitions are separated by blank lines.
fn print_layouts(defs: &[&TypeLayout]) {
    for (i, def) in defs.iter().enumerate() {
        if i > 0 {
            eprintln!();
        }
        eprintln!("  {}", def.name);

        let name_width = def
            .fields
            .iter()
            .map(|(k, _)| k.len())
            .max()
            .unwrap_or(0);
        for (k, v) in &def.fields {
            eprintln!("    {:width$} :: {}", k, v, width = name_width);
        }
    }
}
