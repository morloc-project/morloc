//! A run that names terminal actions with paths (`--fasta=genome.fa`), or
//! gives `--no-stdout`, or sends to stdout an action that runs only on the
//! command's saved output.
//!
//! The parent command runs exactly once, in a stage child (see `stage`),
//! which saves its output and the arguments the actions refer to. Each
//! action then runs in a replay child: the action's `replay` entry applied
//! to the saved output, with the child's stdout pointed at the action's
//! destination. Each child is a whole nexus going through the ordinary
//! single-command path, so formatting, sinks and pool output behave exactly
//! as in a run with that one action.
//!
//! Stdout gets the bare action, or the `@default` action unless it was given
//! a path, or else the command's own output: a replay child writes the
//! stdout action last, or the stage writes the command's output as it runs.
//! No output goes to both a file and stdout. Each file is written to a temporary name beside it
//! and renamed into place once every child has succeeded; on any failure,
//! or a signal, the temporary files and the stage directory are removed.

use crate::dispatch::{NexusConfig, OutputFormat};
use crate::manifest::ActionKind;
use crate::manifest::{Command as ManifestCommand, Manifest};
use crate::phase2::{Dest, FileTarget, ParsedCommand};
use crate::process;
use std::ffi::CString;
use std::os::unix::process::ExitStatusExt;

/// One output of the run: the replay entry that produces it and where it
/// goes.
struct Output {
    long: String,
    entry: String,
    args: Vec<usize>,
    target: Option<Write>,
}

/// How an action's file is written.
enum Write {
    /// Under a temporary name beside the file, renamed onto it once the
    /// whole run has succeeded.
    Staged { temp: String, dest: std::path::PathBuf },
    /// In place, for a path that is a device or a pipe, which a rename
    /// would replace rather than write.
    Direct { path: String },
}

/// Run the parsed command as a multi-output run. Never returns.
pub fn run(
    manifest: &Manifest,
    config: &NexusConfig,
    parsed: &ParsedCommand,
    format_explicit: bool,
) -> ! {
    let parent = &manifest.commands[parsed.parent_index];

    // Every action this run applies to the saved output, path actions first
    // and the stdout action last.
    let mut outputs: Vec<Output> = Vec::new();
    for a in &parsed.actions {
        if let Dest::File(target) = &a.dest {
            outputs.push(output_for(manifest, parent, a.terminal, Some(target)));
        }
    }
    if let Some(t) = parsed.stdout_terminal {
        outputs.push(output_for(manifest, parent, t, None));
    }

    let mut stage_args: Vec<usize> = outputs.iter().flat_map(|o| o.args.iter().copied()).collect();
    stage_args.sort_unstable();
    stage_args.dedup();

    // Every output takes the run's `-f` (a `@render` action writes raw bytes
    // instead). One that cannot take it is refused before anything runs,
    // rather than after the command's work is done.
    if format_explicit {
        let tee = parsed.stdout_terminal.is_none() && !parsed.no_stdout;
        let streams = parent.terminals.iter().any(|t| t.kind.of_streaming_command());
        if tee {
            let shape = if streams { OutputShape::Stream } else { OutputShape::value_of(parent) };
            if let Err(e) = shape.accepts(config.output_format) {
                die(format!("-f {}: the output of `{}` {}", format_label(config), parent.name, e));
            }
        }
        for out in &outputs {
            let term = parent.terminals.iter().find(|t| t.long == out.long);
            if term.map(|t| t.render).unwrap_or(false) {
                continue;
            }
            let shape = match term.map(|t| t.kind) {
                Some(ActionKind::Stream) => OutputShape::Stream,
                _ => match manifest.command_by_name(&out.entry) {
                    Some(c) => OutputShape::value_of(c),
                    None => continue,
                },
            };
            if let Err(e) = shape.accepts(config.output_format) {
                die(format!("-f {}: the output of --{} {}", format_label(config), out.long, e));
            }
        }
    }

    // The children must not be told apart from the SIGCHLD bookkeeping of
    // pools this process never starts, so they are waited for directly.
    unsafe { libc::signal(libc::SIGCHLD, libc::SIG_DFL) };

    let stage_dir = make_stage_dir().unwrap_or_else(|e| die(e));
    crate::sigrm::register(&stage_dir).unwrap_or_else(|e| die(e));

    let exe = std::env::current_exe()
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|e| die(format!("cannot locate the nexus executable: {}", e)));
    let manifest_path = std::env::var("MORLOC_MANIFEST_PATH")
        .unwrap_or_else(|_| die("the manifest path is not known".into()));

    // 1. The stage: the original command line, run once.
    let tee = parsed.stdout_terminal.is_none() && !parsed.no_stdout;
    let original: Vec<String> = std::env::args().collect();
    let mut argv: Vec<String> = vec!["run".into(), "--mlc-stage".into(), stage_dir.clone()];
    if !stage_args.is_empty() {
        argv.push("--mlc-stage-args".into());
        argv.push(stage_args.iter().map(|n| n.to_string()).collect::<Vec<_>>().join(","));
    }
    if tee {
        argv.push("--mlc-stage-tee".into());
    }
    argv.extend(original.iter().skip(2).cloned());
    let mut broken_pipe = false;
    match run_child(&exe, &argv, None, true) {
        0 => {}
        141 if tee => broken_pipe = true,
        code => process::clean_exit(code),
    }

    let input = if parent.stream.is_some() {
        crate::stage::stream_path(&stage_dir)
    } else {
        crate::stage::value_path(&stage_dir)
    };

    // 2. The actions, each on the saved output.
    let mut options: Vec<String> = Vec::new();
    if format_explicit {
        options.push("-f".into());
        options.push(crate::stdio_server::format_name(config.output_format).into());
    }
    if let Some(z) = config.stdout_compression {
        options.push("-z".into());
        options.push(z.to_string());
    }
    if config.print_flag {
        options.push("-p".into());
    }
    if config.keep_null {
        options.push("--keep-null".into());
    }
    for out in &mut outputs {
        let mut argv: Vec<String> = vec!["run".into(), "--mlc-internal".into(), out.entry.clone()];
        for n in &out.args {
            argv.push("--mlc-input".into());
            argv.push(crate::stage::arg_path(&stage_dir, *n));
        }
        argv.push("--mlc-input".into());
        argv.push(input.clone());
        argv.extend(options.iter().cloned());
        argv.push(manifest_path.clone());
        let stdout_file = match &mut out.target {
            Some(t) => Some(open_target(t).unwrap_or_else(|e| die(format!("--{}: {}", out.long, e)))),
            None => None,
        };
        match run_child(&exe, &argv, stdout_file, false) {
            0 => {}
            141 if out.target.is_none() => broken_pipe = true,
            code => process::clean_exit(code),
        }
    }

    // 3. Every output succeeded: move the files into place.
    for out in &outputs {
        if let Some(Write::Staged { temp, dest }) = &out.target {
            if let Err(e) = std::fs::rename(temp, dest) {
                die(format!(
                    "--{}: cannot move the finished file to {}: {}",
                    out.long, dest.display(), e,
                ));
            }
        }
    }
    process::clean_exit(if broken_pipe { 141 } else { 0 })
}

/// What one output of the run writes, as far as `-f` is concerned.
enum OutputShape {
    /// A stream of batches, transcoded frame by frame.
    Stream,
    /// One value; `is_table` when it is a Table.
    Value { is_table: bool },
}

impl OutputShape {
    fn value_of(cmd: &ManifestCommand) -> OutputShape {
        let is_table = morloc_runtime_types::schema::parse_schema(&cmd.ret.schema)
            .map(|s| s.serial_type == morloc_runtime_types::schema::SerialType::Table)
            .unwrap_or(false);
        OutputShape::Value { is_table }
    }

    /// Whether this output can be written in `format`: the same rules the
    /// printer and the stdout stream apply when they write it.
    fn accepts(&self, format: OutputFormat) -> Result<(), &'static str> {
        use OutputFormat::*;
        match (self, format) {
            (OutputShape::Stream, Json | Jsonl | Packet | VoidStar | Raw) => Ok(()),
            (OutputShape::Stream, _) => Err("is a stream, which -f writes only as json, jsonl or packet"),
            (OutputShape::Value { is_table: false }, Arrow | Parquet | Csv | Tsv) => {
                Err("is not a Table, which arrow, parquet, csv and tsv need")
            }
            (OutputShape::Value { .. }, _) => Ok(()),
        }
    }
}

fn format_label(config: &NexusConfig) -> &'static str {
    crate::stdio_server::format_name(config.output_format)
}

/// Report a failure of the run as a whole and exit.
fn die(msg: String) -> ! {
    eprintln!("Error: {}", msg);
    process::clean_exit(1);
}

/// The replay entry of terminal `t`, or a refusal when the action cannot
/// run on the saved output.
fn output_for(
    manifest: &Manifest,
    parent: &ManifestCommand,
    t: usize,
    target: Option<&FileTarget>,
) -> Output {
    let term = &parent.terminals[t];
    let entry = match &term.replay {
        Some(e) if manifest.command_index(e).is_some() => e.clone(),
        _ => {
            let why = term.no_replay.as_deref().unwrap_or("it has no replay entry");
            if term.entry.is_some() {
                die(format!(
                    "--{} cannot be combined with other outputs or written to a file, \
                     because {}. Run it on its own, writing to standard output.",
                    term.long, why,
                ))
            } else {
                die(format!("--{} cannot run, because {}.", term.long, why))
            }
        }
    };
    let target = target.map(|t| plan_write(t).unwrap_or_else(|e| die(format!("--{}: {}", term.long, e))));
    Output { long: term.long.clone(), entry, args: term.args.clone(), target }
}

/// What a path holds before the run writes it.
enum Existing {
    Absent,
    Regular(std::fs::Metadata),
    Other,
}

/// Decide how an action's file is written. A new file or a regular one is
/// staged under a unique temporary name in the directory of the file the
/// path names (`phase2::resolve_target`), so the final rename stays on one
/// filesystem and writes through a symlink. The temporary file takes the
/// mode, and where the process may set it the owner, of the file it
/// replaces, or else the mode a plain create would give. A device or pipe
/// is written in place.
fn plan_write(target: &FileTarget) -> Result<Write, String> {
    let existing = match std::fs::metadata(target.given()) {
        Err(_) => Existing::Absent,
        Ok(m) if m.is_file() => Existing::Regular(m),
        Ok(_) => Existing::Other,
    };
    if let Existing::Other = existing {
        return Ok(Write::Direct { path: target.given().to_string() });
    }
    let dest = target.file().to_path_buf();
    let dir = match dest.parent() {
        Some(d) if !d.as_os_str().is_empty() => d.to_path_buf(),
        _ => std::path::PathBuf::from("."),
    };
    let name = dest.file_name().map(|n| n.to_string_lossy().into_owned()).unwrap_or_default();
    let template = dir.join(format!(".{}.morloc-XXXXXX", name));
    let mut buf = CString::new(template.to_string_lossy().into_owned())
        .map_err(|e| e.to_string())?
        .into_bytes_with_nul();
    let fd = unsafe { morloc_runtime_types::fd::mkstemp(buf.as_mut_ptr() as *mut libc::c_char) };
    if fd < 0 {
        return Err(format!("cannot write in {}: {}", dir.display(), std::io::Error::last_os_error()));
    }
    unsafe {
        match &existing {
            Existing::Regular(meta) => {
                use std::os::unix::fs::MetadataExt;
                libc::fchown(fd, meta.uid(), meta.gid());
                libc::fchmod(fd, (meta.mode() & 0o7777) as libc::mode_t);
            }
            Existing::Absent | Existing::Other => {
                let mask = libc::umask(0);
                libc::umask(mask);
                libc::fchmod(fd, 0o666 & !mask);
            }
        }
        libc::close(fd);
    }
    buf.pop();
    let temp = String::from_utf8_lossy(&buf).into_owned();
    crate::sigrm::register(&temp)?;
    Ok(Write::Staged { temp, dest })
}

/// Open the file an action's child writes to.
fn open_target(w: &Write) -> Result<std::fs::File, String> {
    let path = match w {
        Write::Staged { temp, .. } => temp.as_str(),
        Write::Direct { path } => path.as_str(),
    };
    std::fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
        .map_err(|e| format!("{}: {}", path, e))
}

/// A fresh directory for the saved output.
fn make_stage_dir() -> Result<String, String> {
    let base = std::env::var("MORLOC_TMPDIR")
        .ok()
        .filter(|d| !d.is_empty())
        .unwrap_or_else(|| std::env::temp_dir().to_string_lossy().into_owned());
    let template = format!("{}/morloc-stage-XXXXXX", base.trim_end_matches('/'));
    let mut buf = CString::new(template).map_err(|e| e.to_string())?.into_bytes_with_nul();
    let p = unsafe { libc::mkdtemp(buf.as_mut_ptr() as *mut libc::c_char) };
    if p.is_null() {
        return Err(format!("cannot create a stage directory in {}: {}", base, std::io::Error::last_os_error()));
    }
    buf.pop();
    Ok(String::from_utf8_lossy(&buf).into_owned())
}

/// Run one child nexus and return its exit status (128 + signal when it
/// was killed). `stdout` replaces its standard output; the stage keeps the
/// run's stdin and logging, a replay child reads nothing and runs quietly.
fn run_child(exe: &str, argv: &[String], stdout: Option<std::fs::File>, stage: bool) -> i32 {
    let mut cmd = std::process::Command::new(exe);
    cmd.args(argv);
    if let Some(f) = stdout {
        cmd.stdout(f);
    }
    if !stage {
        cmd.stdin(std::process::Stdio::null());
        cmd.env("MORLOC_QUIET", "1");
        for v in ["MORLOC_SUMMARY", "MORLOC_RUN_DIR", "MORLOC_RUN_PARENT_PID", "MORLOC_LOG_DIR"] {
            cmd.env_remove(v);
        }
    }
    // A child outlives nothing: it watches this process's lifeline and ends
    // itself when this process ends.
    extern "C" {
        fn morloc_lifeline_child_env(read_fd: *mut i32) -> *const libc::c_char;
    }
    let mut lifeline_fd: i32 = -1;
    let lifeline = unsafe { morloc_lifeline_child_env(&mut lifeline_fd) };
    if !lifeline.is_null() {
        let entry = unsafe { std::ffi::CStr::from_ptr(lifeline) }.to_string_lossy();
        if let Some((k, v)) = entry.split_once('=') {
            cmd.env(k, v);
        }
        unsafe {
            use std::os::unix::process::CommandExt;
            cmd.pre_exec(move || {
                libc::fcntl(lifeline_fd, libc::F_SETFD, 0);
                Ok(())
            });
        }
    }
    match cmd.status() {
        Ok(st) => st.code().unwrap_or_else(|| 128 + st.signal().unwrap_or(0)),
        Err(e) => {
            eprintln!("Error: cannot start {}: {}", exe, e);
            1
        }
    }
}
