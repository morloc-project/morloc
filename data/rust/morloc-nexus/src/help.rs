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

    print_command_body(cmd);
    std::process::exit(0);
}

/// Print help for a single-command program.
pub fn print_command_help_single(prog_name: &str, cmd: &Command) -> ! {
    eprint!("Usage: {}", prog_name);
    print_usage_suffix(cmd);
    eprintln!();

    // Description
    for (i, line) in cmd.desc.iter().enumerate() {
        if i == 0 && line.is_empty() {
            continue;
        }
        eprintln!("{}", line);
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
    print_return_info(cmd);
    std::process::exit(0);
}

// ── Helpers ────────────────────────────────────────────────────────────────

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
    eprintln!("\nReturn: {}", cmd.return_type);
    for line in &cmd.return_desc {
        eprintln!("  {}", line);
    }
}
