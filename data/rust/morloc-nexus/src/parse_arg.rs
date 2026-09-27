//! `@parse` arguments.
//!
//! An argument that declares `@parse` formats may be given as a file in one of
//! them: a value prefixed with `<name>:`, or ending in one of the format's
//! extensions, is a path the format's handler reads. The handler runs in a
//! pool, as the first step of the command's compiler-synthesized parse entry,
//! so the nexus only selects the format and rewrites the argument list into
//! the entry's slots. For each parseable argument the entry takes one Bool per
//! format (true for the selected one), the path, the argument's packet as the
//! nexus loads it when no format was selected, and for a stream argument the
//! path the stream is staged at.

use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::dispatch::ArgValue;
use crate::file::json_quote;
use crate::manifest::{Arg, ArgParse, Manifest};

/// How one argv token of a parseable argument is read.
#[derive(Debug, PartialEq, Eq)]
pub enum Selection<'a> {
    /// By format `index`, from `path`.
    Format { index: usize, path: &'a str },
    /// As a morloc value (the `morloc:` prefix, stripped).
    Morloc(&'a str),
    /// As a morloc value (no prefix or extension matched).
    Unselected,
}

/// Select the format a token names. A prefix is recognized only when the
/// text before the first `:` is a declared format name or `morloc`; anything
/// else is the literal token. Without a prefix, the longest declared
/// extension the token ends with, compared without regard to case, selects
/// its format.
pub fn select<'a>(token: &'a str, parse: &ArgParse) -> Selection<'a> {
    if let Some((prefix, rest)) = token.split_once(':') {
        if let Some(index) = parse.formats.iter().position(|f| f.name == prefix) {
            return Selection::Format { index, path: rest };
        }
        if prefix == "morloc" {
            return Selection::Morloc(rest);
        }
    }
    let lower = token.to_ascii_lowercase();
    let mut best: Option<(usize, usize)> = None;
    for (index, f) in parse.formats.iter().enumerate() {
        for ext in &f.exts {
            if lower.len() > ext.len()
                && lower.ends_with(ext.as_str())
                && best.map_or(true, |(_, len)| ext.len() > len)
            {
                best = Some((index, ext.len()));
            }
        }
    }
    match best {
        Some((index, _)) => Selection::Format { index, path: token },
        None => Selection::Unselected,
    }
}

/// True once some argument that no format reads takes its value from stdin,
/// which the nexus then reads itself.
static NEXUS_READS_STDIN: AtomicBool = AtomicBool::new(false);

/// Note that an unparsed argument's token reads stdin.
pub fn note_unparsed_token(token: &str) {
    if token == "-" || token == "/dev/stdin" {
        NEXUS_READS_STDIN.store(true, Ordering::Relaxed);
    }
}

/// What a parse failure report names: the argument, its format, and the
/// path as the user gave it.
struct FailureContext {
    arg: usize,
    display: String,
    format: String,
    path: String,
}

static CONTEXTS: Mutex<Vec<FailureContext>> = Mutex::new(Vec::new());

/// The name an argument has in a parse failure report.
fn display_name(arg: &Arg, index: usize) -> String {
    match arg {
        Arg::Positional { metavar: Some(m), .. } => m.clone(),
        Arg::Positional { name: Some(n), .. } => n.clone(),
        Arg::Optional { long_opt: Some(l), .. } => format!("--{}", l),
        Arg::Optional { short_opt: Some(s), .. } => format!("-{}", s),
        _ => format!("#{}", index + 1),
    }
}

/// Rewrite the values of command `cmd_index` into its parse entry's slots
/// when any argument selected a format. Returns the command to dispatch and
/// its values.
pub fn redirect(
    manifest: &Manifest,
    cmd_index: usize,
    values: Vec<ArgValue>,
) -> (usize, Vec<ArgValue>) {
    if !values.iter().any(|v| matches!(v, ArgValue::Parsed { .. })) {
        return (cmd_index, values);
    }
    let target = &manifest.commands[cmd_index];
    let entry = match target.parse_entry.as_deref().and_then(|e| manifest.command_index(e)) {
        Some(e) => e,
        None => crate::runlog::die_with_error(&format!(
            "internal: command '{}' has `@parse` arguments but no parse entry",
            target.name
        )),
    };

    let stdin_readers = values
        .iter()
        .filter(|v| matches!(v, ArgValue::Parsed { path, .. } if path == "/dev/stdin"))
        .count();
    if stdin_readers > 0 {
        if stdin_readers + NEXUS_READS_STDIN.load(Ordering::Relaxed) as usize > 1 {
            crate::runlog::die_with_error("stdin can only be read by a single argument per command");
        }
        // A pool runs in its own process group, so reading a terminal
        // would stop it rather than wait for input.
        if unsafe { libc::isatty(0) } == 1 {
            crate::runlog::die_with_error(
                "an argument reads stdin, but stdin is a terminal; pipe data in or pass a file path",
            );
        }
    }

    let mut out = Vec::new();
    for (j, (v, arg)) in values.into_iter().zip(target.args.iter()).enumerate() {
        // An argument without formats is read as the target reads it: the
        // entry's own definition of the slot carries none of its shape.
        let parse = match arg.parse() {
            Some(p) => p,
            None => {
                out.push(ArgValue::AsParent { value: Box::new(v), parent: cmd_index, arg: j });
                continue;
            }
        };
        // The slot layout must match `Desugar.synthParseEntry`: one selector
        // per format, the path, the packet, then the stage path for a stream.
        let (selected, path, packet) = match v {
            ArgValue::Parsed { format, path, shown } => {
                if let Ok(mut cs) = CONTEXTS.lock() {
                    cs.push(FailureContext {
                        arg: j,
                        display: display_name(arg, j),
                        format: parse.formats[format].name.clone(),
                        path: shown,
                    });
                }
                // Never decoded: the selected format's branch reads the path.
                let packet = ArgValue::Json("[]".to_string());
                (Some(format), path, packet)
            }
            v => {
                let packet = ArgValue::Embed { value: Box::new(v), parent: cmd_index, arg: j };
                (None, String::new(), packet)
            }
        };
        for k in 0..parse.formats.len() {
            out.push(ArgValue::Json((selected == Some(k)).to_string()));
        }
        out.push(ArgValue::Json(json_quote(&path)));
        out.push(packet);
        if parse.stage {
            let stage = match selected {
                Some(_) => format!("{}/arg-{}", stage_dir(), j),
                None => String::new(),
            };
            out.push(ArgValue::Json(json_quote(&stage)));
        }
    }
    (entry, out)
}

static STAGE_DIR: std::sync::OnceLock<String> = std::sync::OnceLock::new();

/// The run's directory for staged streams, created on first use under
/// `MORLOC_TMPDIR`, else `TMPDIR`, else `/tmp`, and removed when the run ends.
fn stage_dir() -> &'static str {
    STAGE_DIR.get_or_init(|| {
        let base = std::env::var("MORLOC_TMPDIR")
            .or_else(|_| std::env::var("TMPDIR"))
            .unwrap_or_else(|_| "/tmp".to_string());
        let template = std::ffi::CString::new(format!("{}/morloc-parse-XXXXXX", base))
            .unwrap_or_else(|_| crate::runlog::die_with_error("temporary directory path contains NUL"));
        let mut buf = template.into_bytes_with_nul();
        let made = unsafe { libc::mkdtemp(buf.as_mut_ptr() as *mut libc::c_char) };
        if made.is_null() {
            crate::runlog::die_with_error(&format!(
                "cannot create a directory under '{}' to stage parsed input: {}",
                base,
                std::io::Error::last_os_error()
            ));
        }
        let dir = unsafe { std::ffi::CStr::from_ptr(made) }.to_string_lossy().into_owned();
        if let Err(e) = crate::sigrm::register(&dir) {
            crate::runlog::die_with_error(&e);
        }
        dir
    })
}

/// A handler failure arrives framed as `\x01<argument index>\x1f<message>\x02`
/// somewhere inside the error text the pool returns. Returns the report for
/// it, or None when the error is not a parse failure.
pub fn failure_report(err: &str) -> Option<String> {
    // Any text before the frame may itself hold a \x01, so each candidate
    // is tried until one parses as a frame for a parsed argument.
    let cs = CONTEXTS.lock().ok()?;
    err.match_indices('\u{1}').find_map(|(start, _)| {
        let rest = &err[start + 1..];
        let sep = rest.find('\u{1f}')?;
        let arg: usize = rest[..sep].parse().ok()?;
        let body = &rest[sep + 1..];
        let msg = match body.find('\u{2}') {
            Some(end) => &body[..end],
            None => body,
        };
        let c = cs.iter().find(|c| c.arg == arg)?;
        Some(report(c, msg))
    })
}

fn report(c: &FailureContext, msg: &str) -> String {
    format!(
        "error: argument {} (format {}, path {}): {}",
        c.display,
        c.format,
        c.path,
        without_frames(msg)
    )
}

/// A caught failure's message with the trailing frame lines a pool appends
/// (`  at <name> [<lang>] (mid=...)`) removed: they locate compiler-generated
/// code, not the input.
fn without_frames(msg: &str) -> &str {
    let mut end = msg.trim_end().len();
    loop {
        let head = &msg[..end];
        match head.rfind('\n') {
            Some(nl) if is_frame_line(&head[nl + 1..]) => end = head[..nl].trim_end().len(),
            _ => return head,
        }
    }
}

fn is_frame_line(line: &str) -> bool {
    let l = line.trim_start();
    l.starts_with("at ") && l.contains(" [") && l.contains("(mid=")
}

#[cfg(test)]
mod tests {
    use super::*;
    use morloc_manifest::ParseFormat;

    fn parse() -> ArgParse {
        ArgParse {
            formats: vec![
                ParseFormat { name: "fastq".into(), exts: vec![".fq".into(), ".fq.gz".into()] },
                ParseFormat { name: "gz".into(), exts: vec![".gz".into()] },
                ParseFormat { name: "json-ish".into(), exts: vec![] },
            ],
            stage: false,
        }
    }

    #[test]
    fn prefix_selects_declared_format() {
        assert_eq!(select("fastq:r.txt", &parse()), Selection::Format { index: 0, path: "r.txt" });
        assert_eq!(select("json-ish:-", &parse()), Selection::Format { index: 2, path: "-" });
        assert_eq!(select("morloc:x.fq", &parse()), Selection::Morloc("x.fq"));
    }

    #[test]
    fn unknown_prefix_is_literal() {
        assert_eq!(select("{\"k\":\"x:y\"}", &parse()), Selection::Unselected);
        assert_eq!(select("C:data.json", &parse()), Selection::Unselected);
        assert_eq!(select("./fastq:x", &parse()), Selection::Unselected);
    }

    #[test]
    fn longest_extension_wins_regardless_of_case() {
        assert_eq!(select("reads.fq.gz", &parse()), Selection::Format { index: 0, path: "reads.fq.gz" });
        assert_eq!(select("READS.FQ.GZ", &parse()), Selection::Format { index: 0, path: "READS.FQ.GZ" });
        assert_eq!(select("other.gz", &parse()), Selection::Format { index: 1, path: "other.gz" });
        assert_eq!(select("reads.fq", &parse()), Selection::Format { index: 0, path: "reads.fq" });
        assert_eq!(select(".fq", &parse()), Selection::Unselected);
        assert_eq!(select("reads.json", &parse()), Selection::Unselected);
    }

    #[test]
    fn failure_frame_is_found_inside_pool_context() {
        CONTEXTS.lock().unwrap().push(FailureContext {
            arg: 7,
            display: "READS".into(),
            format: "fastq".into(),
            path: "r.fq".into(),
        });
        let err = "Traceback:\n  RuntimeError: \u{1}7\u{1f}bad record at line 3\n  at _ [py] (mid=12, m.loc:1:1)\u{2}\n  at m12 [py]";
        assert_eq!(
            failure_report(err).as_deref(),
            Some("error: argument READS (format fastq, path r.fq): bad record at line 3")
        );
        assert_eq!(failure_report("plain failure"), None);
        let noisy = "raw \u{1}bytes\u{1}7\u{1f}short\u{2}";
        assert_eq!(
            failure_report(noisy).as_deref(),
            Some("error: argument READS (format fastq, path r.fq): short")
        );
    }
}
