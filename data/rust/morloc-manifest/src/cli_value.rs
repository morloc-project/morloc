//! How the command line reads an argument's text, as data: the shape
//! directives of a manifest argument, the text rewrites applied before a
//! value is parsed, and the codes the runtime's shaped parser takes.

use crate::{Arg, Check, FormAtom, SourceAtom};

/// CLI-shape directives pulled out of a manifest arg. Single-atom
/// fields under the simplified grammar (no OR-chains).
#[derive(Debug, Clone)]
pub struct ArgShape<'a> {
    pub source: SourceAtom,
    pub form: FormAtom,
    pub checks: &'a [Check],
    pub list_source: SourceAtom,
    pub list_form: FormAtom,
    pub list_checks: &'a [Check],
}

impl<'a> ArgShape<'a> {
    /// Extract the shape view from a manifest arg, returning `None` for
    /// variants (Flag, Group, Alt) that have no shape vocabulary.
    pub fn from_arg(arg: &'a Arg) -> Option<ArgShape<'a>> {
        match arg {
            Arg::Positional { source, form, checks, list_source, list_form, list_checks, .. } |
            Arg::Optional   { source, form, checks, list_source, list_form, list_checks, .. } => {
                Some(ArgShape {
                    source: *source,
                    form: *form,
                    checks: checks.as_slice(),
                    list_source: *list_source,
                    list_form: *list_form,
                    list_checks: list_checks.as_slice(),
                })
            }
            Arg::Flag { .. } | Arg::Group { .. } | Arg::Alt { .. } => None,
        }
    }

    /// True when every field is at its compiler-emitted default
    /// (`source = Auto`, `form = Auto`, no checks, `list_source =
    /// Inline`, `list_form = Auto`, no list_checks). The default shape
    /// carries the same semantics as the existing
    /// `parse_cli_data_argument` path, so the caller can skip the
    /// shape-aware route entirely.
    /// `source: inline` with a bytes form: the token's escapes are decoded
    /// and it is sent as a JSON list of bytes, unshaped.
    pub fn is_inline_bytes(&self) -> bool {
        self.source == SourceAtom::Inline
            && (self.form == FormAtom::Bytes || self.form == FormAtom::BytesOnly)
    }

    pub fn is_default(&self) -> bool {
        self.source == SourceAtom::Auto
            && self.form == FormAtom::Auto
            && self.checks.is_empty()
            && self.list_source == SourceAtom::Inline
            && self.list_form == FormAtom::Auto
            && self.list_checks.is_empty()
    }
}

/// If the wire schema is a list (`aT` or `aT:<dim>`), return its
/// element schema as a string. Returns `None` for non-list schemas.
pub fn element_schema_str(schema_str: &str) -> Option<String> {
    let mut s = schema_str.trim();
    if let Some(rest) = s.strip_prefix('<') {
        if let Some(end) = rest.find('>') {
            s = &rest[end + 1..];
        }
    }
    let rest = s.strip_prefix('a')?;
    let rest = if let Some(after_colon) = rest.strip_prefix(':') {
        after_colon.trim_start_matches(|c: char| c.is_ascii_digit())
    } else {
        rest
    };
    Some(rest.to_string())
}

/// True if the rendered wire schema reduces to a scalar `Float32`
/// (`f4`) or `Float64` (`f8`) primitive (with optional `?` and hint
/// wrapping). Used to decide whether an argv token like `inf`,
/// `-inf`, or `nan` should be reinterpreted as an IEEE 754 special.
pub fn schema_is_float_scalar(schema_str: &str) -> bool {
    let mut s = schema_str.trim();
    if let Some(rest) = s.strip_prefix('?') {
        s = rest;
    }
    if let Some(rest) = s.strip_prefix('<') {
        if let Some(end) = rest.find('>') {
            s = &rest[end + 1..];
        }
    }
    s == "f4" || s == "f8"
}

/// If the argv string is a recognized IEEE 754 special-value literal,
/// return its JSON-quoted form (`"inf"`, `"-inf"`, `"nan"`). The morloc
/// JSON wire layer accepts these quoted lowercase variants per the
/// non-finite-Real convention; this helper lets a user type
/// `inf` / `nan` / `\-inf` on the command line. `\-inf` is the
/// shell-safe spelling of `-inf` -- the leading backslash bypasses
/// clap's flag-cluster interpretation of the bare `-i`/`-n`/`-f`
/// sequence.
pub fn maybe_float_special_to_json(argv: &str) -> Option<String> {
    let trimmed = argv.trim();
    let stripped = trimmed.strip_prefix('\\').unwrap_or(trimmed);
    let inner = match stripped {
        "inf" => "inf",
        "-inf" => "-inf",
        "nan" => "nan",
        _ => return None,
    };
    Some(format!("\"{}\"", inner))
}

/// Process inline byte-string escape sequences. The default mapping is
/// "argv char -> UTF-8 bytes"; the recognized escapes let users encode
/// arbitrary byte values that wouldn't otherwise survive the shell or
/// argv layer.
///
/// Recognized escapes (errors on anything else):
///   `\\`  literal backslash
///   `\n`  byte 0x0A
///   `\t`  byte 0x09
///   `\r`  byte 0x0D
///   `\0`  byte 0x00
///   `\xNN` byte with the given two-digit hex value
///
/// A `\0` escape produces a real NUL byte in the output buffer even
/// though argv itself cannot contain NUL.
pub fn process_inline_bytes_escapes(argv: &str) -> Result<Vec<u8>, String> {
    let mut out: Vec<u8> = Vec::with_capacity(argv.len());
    let mut chars = argv.chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\\' {
            let mut buf = [0u8; 4];
            let s = c.encode_utf8(&mut buf);
            out.extend_from_slice(s.as_bytes());
            continue;
        }
        match chars.next() {
            Some('\\') => out.push(b'\\'),
            Some('n') => out.push(b'\n'),
            Some('t') => out.push(b'\t'),
            Some('r') => out.push(b'\r'),
            Some('0') => out.push(0),
            Some('x') => {
                let h1 = chars
                    .next()
                    .ok_or_else(|| "incomplete `\\x` escape: needs two hex digits".to_string())?;
                let h2 = chars
                    .next()
                    .ok_or_else(|| "incomplete `\\x` escape: needs two hex digits".to_string())?;
                if !h1.is_ascii_hexdigit() || !h2.is_ascii_hexdigit() {
                    return Err(format!(
                        "invalid `\\x` escape: `\\x{}{}` must be two hex digits",
                        h1, h2
                    ));
                }
                let byte = (h1.to_digit(16).unwrap() as u8) * 16
                    + (h2.to_digit(16).unwrap() as u8);
                out.push(byte);
            }
            Some(other) => {
                return Err(format!(
                    "unrecognized escape: `\\{}`; supported: \\\\ \\n \\t \\r \\0 \\xNN",
                    other
                ));
            }
            None => return Err("trailing backslash with no escape character".to_string()),
        }
    }
    Ok(out)
}

/// Return the `(source_code, form_code)` pair to pass through the
/// shape-aware FFI, or `None` when the runtime can use the cheaper
/// default path. The default case includes the `source: inline +
/// form: bytes/bytes-only` shortcut on `[UInt8]`, which is already
/// JSON-encoded by `shape_argv`.
pub fn shaped_dispatch_codes(shape: &ArgShape) -> Option<(u8, u8)> {
    if shape.is_default() || shape.is_inline_bytes() {
        return None;
    }
    Some((source_atom_code(shape.source), form_atom_code(shape.form)))
}

/// Serialize the per-element shape portion of an ArgShape into the
/// JSON the runtime's `parse_cli_data_argument_shaped` expects.
/// Returns `None` when every per-element field is at its default, so
/// the runtime can skip the per-element pipeline entirely.
pub fn build_list_config_json(shape: &ArgShape) -> Option<String> {
    let list_source_is_default = shape.list_source == SourceAtom::Inline;
    let list_form_is_default = shape.list_form == FormAtom::Auto;
    if list_source_is_default && list_form_is_default && shape.list_checks.is_empty() {
        return None;
    }
    let source_str = source_atom_name(shape.list_source);
    let form_str = form_atom_name(shape.list_form);
    let checks_parts: Vec<String> = shape
        .list_checks
        .iter()
        .map(|c| match c {
            Check::Path(p) => format!(
                "{{\"kind\":\"path\",\"value\":{}}}",
                serde_json::to_string(p).unwrap_or_else(|_| "\"\"".to_string())
            ),
        })
        .collect();
    Some(format!(
        "{{\"source\":\"{}\",\"form\":\"{}\",\"checks\":[{}]}}",
        source_str,
        form_str,
        checks_parts.join(",")
    ))
}

/// Wire-stable u8 code for a source atom. Must match the dispatch
/// arms in the runtime.
pub fn source_atom_code(atom: SourceAtom) -> u8 {
    match atom {
        SourceAtom::Auto => 0,
        SourceAtom::Inline => 1,
        SourceAtom::File => 2,
    }
}

/// Wire-stable u8 code for a form atom. Must match the dispatch arms
/// in `parse_cli_data_argument_shaped` (morloc-runtime).
pub fn form_atom_code(atom: FormAtom) -> u8 {
    match atom {
        FormAtom::Auto => 0,
        FormAtom::Packet => 1,
        FormAtom::Bytes => 2,
        FormAtom::List => 3,
        FormAtom::BytesOnly => 4,
    }
}

pub fn source_atom_name(atom: SourceAtom) -> &'static str {
    match atom {
        SourceAtom::Auto => "auto",
        SourceAtom::Inline => "inline",
        SourceAtom::File => "file",
    }
}

pub fn form_atom_name(atom: FormAtom) -> &'static str {
    match atom {
        FormAtom::Auto => "auto",
        FormAtom::Packet => "packet",
        FormAtom::Bytes => "bytes",
        FormAtom::List => "list",
        FormAtom::BytesOnly => "bytes-only",
    }
}

/// The text the runtime parses for `raw` under `shape`: `raw` itself, or for
/// `source: inline` with a bytes form, its escapes decoded and written as a
/// JSON list of bytes.
pub fn shape_argv(raw: String, shape: Option<&ArgShape>) -> Result<String, String> {
    let shape = match shape {
        Some(s) if !s.is_default() => s,
        _ => return Ok(raw),
    };
    if shape.is_inline_bytes() {
        let bytes = process_inline_bytes_escapes(&raw)?;
        let parts: Vec<String> = bytes.iter().map(|b| b.to_string()).collect();
        return Ok(format!("[{}]", parts.join(",")));
    }
    Ok(raw)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn inline_byte_escapes_decode_and_malformed_ones_are_refused() {
        assert_eq!(process_inline_bytes_escapes("a\\n\\x41\\0").unwrap(), vec![b'a', b'\n', 0x41, 0]);
        assert_eq!(process_inline_bytes_escapes("\u{e9}").unwrap(), "\u{e9}".as_bytes());
        assert!(process_inline_bytes_escapes("\\").is_err());
        assert!(process_inline_bytes_escapes("\\q").is_err());
        assert!(process_inline_bytes_escapes("\\x4").is_err());
        assert!(process_inline_bytes_escapes("\\xzz").is_err());
    }

    #[test]
    fn schemas_and_float_specials_are_read_as_the_command_line_reads_them() {
        assert_eq!(element_schema_str("af8").as_deref(), Some("f8"));
        assert_eq!(element_schema_str("a:3i4").as_deref(), Some("i4"));
        assert_eq!(element_schema_str("i4"), None);
        assert!(schema_is_float_scalar("?<double>f8"));
        assert!(!schema_is_float_scalar("af8"));
        assert_eq!(maybe_float_special_to_json("\\-inf").as_deref(), Some("\"-inf\""));
        assert_eq!(maybe_float_special_to_json("1.5"), None);
    }

    #[test]
    fn an_inline_bytes_shape_sends_a_json_list_and_no_shape_codes() {
        let shape = ArgShape {
            source: SourceAtom::Inline,
            form: FormAtom::Bytes,
            checks: &[],
            list_source: SourceAtom::Inline,
            list_form: FormAtom::Auto,
            list_checks: &[],
        };
        assert_eq!(shape_argv("AC\\x00".into(), Some(&shape)).unwrap(), "[65,67,0]");
        assert_eq!(shaped_dispatch_codes(&shape), None);
        assert_eq!(shape_argv("raw".into(), None).unwrap(), "raw");
    }
}

