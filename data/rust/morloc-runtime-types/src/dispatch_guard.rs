//! No `match` over a schema kind may end in a catch-all arm.
//!
//! Every walker, marshaller and codec dispatches on `SerialType`. A `_` (or
//! bare binding) arm there silently absorbs any kind it does not name,
//! including a kind added later, which is how a new kind reaches code that
//! treats it as something it is not. Listing every kind makes adding one a
//! compile error at each dispatch site that must decide what to do with it.
//!
//! Clippy's `wildcard_enum_match_arm` does not look inside macro bodies,
//! where the pool marshallers dispatch, and cannot be limited to one enum,
//! so this test scans the workspace's sources instead. It checks matches
//! whose scrutinee is a schema kind; a tuple scrutinee (a kind paired with
//! something else) may legitimately fall through.

use std::path::{Path, PathBuf};

fn rust_sources(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for e in entries.flatten() {
        let p = e.path();
        if p.is_dir() {
            if p.file_name().is_some_and(|n| n == "target") {
                continue;
            }
            rust_sources(&p, out);
        } else if p.extension().is_some_and(|x| x == "rs") {
            out.push(p);
        }
    }
}

/// The source with string and char literals and comments blanked to spaces,
/// keeping every byte offset and newline where it was.
fn blank_noise(src: &str) -> Vec<u8> {
    let b = src.as_bytes();
    let mut out = b.to_vec();
    let mut i = 0;
    while i < b.len() {
        match b[i] {
            b'/' if b.get(i + 1) == Some(&b'/') => {
                while i < b.len() && b[i] != b'\n' {
                    out[i] = b' ';
                    i += 1;
                }
            }
            b'/' if b.get(i + 1) == Some(&b'*') => {
                while i < b.len() && !(b[i] == b'*' && b.get(i + 1) == Some(&b'/')) {
                    if b[i] != b'\n' {
                        out[i] = b' ';
                    }
                    i += 1;
                }
                if i < b.len() {
                    out[i] = b' ';
                    out[i + 1] = b' ';
                    i += 2;
                }
            }
            b'"' => {
                let raw_hashes = count_raw_prefix(b, i);
                i += 1;
                while i < b.len() {
                    if raw_hashes.is_none() && b[i] == b'\\' {
                        out[i] = b' ';
                        i += 1;
                    } else if b[i] == b'"' && closes_raw(b, i, raw_hashes) {
                        break;
                    }
                    if b[i] != b'\n' {
                        out[i] = b' ';
                    }
                    i += 1;
                }
                i += 1;
            }
            // A char literal: 'x' or '\x' (a lifetime has no closing quote).
            b'\'' if b.get(i + 2) == Some(&b'\'') || (b.get(i + 1) == Some(&b'\\') && b.get(i + 3) == Some(&b'\'')) => {
                let end = if b.get(i + 1) == Some(&b'\\') { i + 3 } else { i + 2 };
                for k in i + 1..end {
                    out[k] = b' ';
                }
                i = end + 1;
            }
            _ => i += 1,
        }
    }
    out
}

/// For a `"` at `i`, the number of `#` in a preceding `r#...#` prefix, or
/// `None` for an ordinary string.
fn count_raw_prefix(b: &[u8], i: usize) -> Option<usize> {
    let mut k = i;
    let mut hashes = 0;
    while k > 0 && b[k - 1] == b'#' {
        hashes += 1;
        k -= 1;
    }
    (k > 0 && b[k - 1] == b'r').then_some(hashes)
}

fn closes_raw(b: &[u8], i: usize, raw: Option<usize>) -> bool {
    match raw {
        None => true,
        Some(n) => (1..=n).all(|k| b.get(i + k) == Some(&b'#')),
    }
}

/// Byte offset of the `}` matching the `{` at `open`.
fn matching_brace(b: &[u8], open: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (k, &c) in b.iter().enumerate().skip(open) {
        match c {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    return Some(k);
                }
            }
            _ => {}
        }
    }
    None
}

/// The arm patterns of a match body `b[start..end]`: the text before each
/// top-level `=>`, back to the previous arm's end.
fn arm_patterns(b: &[u8], start: usize, end: usize) -> Vec<(usize, String)> {
    let mut pats = Vec::new();
    let mut depth = 0i32;
    let mut arm_start = start;
    let mut k = start;
    while k < end {
        match b[k] {
            b'{' | b'(' | b'[' => depth += 1,
            b'}' | b')' | b']' => {
                depth -= 1;
                // An arm whose body is a block ends at its closing brace.
                if depth == 0 && b[k] == b'}' {
                    arm_start = k + 1;
                }
            }
            b',' if depth == 0 => arm_start = k + 1,
            b'=' if depth == 0 && b.get(k + 1) == Some(&b'>') => {
                let pat = String::from_utf8_lossy(&b[arm_start..k]).trim().to_string();
                pats.push((k, pat));
                k += 1;
            }
            _ => {}
        }
        k += 1;
    }
    pats
}

/// True for a pattern that matches anything unconditionally: `_` or a bare
/// binding. A guarded arm is not one: it absorbs nothing unconditionally,
/// and the compiler still requires every kind to be covered without it.
fn is_catch_all(pat: &str) -> bool {
    if pat.contains(" if ") {
        return false;
    }
    let head = pat.trim();
    head == "_"
        || (!head.is_empty()
            && head.chars().next().is_some_and(|c| c.is_ascii_lowercase() || c == '_')
            && head.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'))
}

fn violations_in(path: &Path, src: &str) -> Vec<String> {
    let b = blank_noise(src);
    let text = String::from_utf8_lossy(&b).to_string();
    let mut out = Vec::new();
    let mut from = 0;
    while let Some(rel) = text[from..].find("match ") {
        let at = from + rel;
        from = at + 6;
        let before_ok = at == 0 || !(b[at - 1].is_ascii_alphanumeric() || b[at - 1] == b'_');
        if !before_ok {
            continue;
        }
        let Some(brace_rel) = text[at..].find('{') else { break };
        let open = at + brace_rel;
        let scrutinee = text[at + 6..open].trim();
        if scrutinee.contains(';') || !scrutinee.contains("serial_type") {
            continue;
        }
        // A tuple scrutinee pairs the kind with something else.
        if scrutinee.starts_with('(') && scrutinee.contains(',') {
            continue;
        }
        let Some(close) = matching_brace(&b, open) else { continue };
        for (pos, pat) in arm_patterns(&b, open + 1, close) {
            if is_catch_all(&pat) {
                let line = text[..pos].matches('\n').count() + 1;
                out.push(format!("{}:{line}: catch-all arm `{pat}` in `match {scrutinee}`", path.display()));
            }
        }
    }
    out
}

#[test]
fn schema_kind_matches_name_every_kind() {
    let workspace = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_path_buf();
    let mut files = Vec::new();
    rust_sources(&workspace, &mut files);
    files.sort();
    let mut all = Vec::new();
    for f in &files {
        let src = std::fs::read_to_string(f).unwrap();
        all.extend(violations_in(f, &src));
    }
    assert!(all.is_empty(), "{} catch-all arms over schema kinds:\n{}", all.len(), all.join("\n"));
}

#[test]
fn guard_finds_catch_alls() {
    let src = r#"
        fn f(s: &Schema) -> u8 {
            match s.serial_type {
                SerialType::Bool => 1,
                _ => 0,
            }
        }
        fn g(s: &Schema) -> u8 {
            match s.serial_type {
                SerialType::Bool => { 1 }
                other if true => 0,
            }
        }
        fn h(s: &Schema) -> u8 {
            match (s.serial_type, 1) {
                (SerialType::Bool, _) => 1,
                _ => 0,
            }
        }
        fn k(s: &Schema) -> &str {
            match s.serial_type {
                SerialType::Bool => "_ => not a pattern",
                SerialType::Nil => '_'.to_string().leak(),
            }
        }
    "#;
    let v = violations_in(Path::new("t.rs"), src);
    assert_eq!(v.len(), 1, "{v:?}");
    assert!(v[0].contains("`_`"), "{v:?}");
}
