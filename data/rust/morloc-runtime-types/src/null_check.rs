//! Stateless half of the NUL-in-Str guard.
//!
//! Only the env-var probe and the JSON walker live here; both are pure
//! and have no SHM dependency, so they can be shared with `morloc-nexus`
//! via the types crate. The SHM-walking variant
//! (`first_null_in_strings`) lives in `morloc-runtime::null_check`
//! because it calls into `shm::rel2abs` which touches process-global
//! `VOLUMES`.

/// Cheap env-var probe used by dispatch sites. Reads MORLOC_SKIP_NULL_CHECK
/// once per call; not cached because the flag is meant for ad-hoc benchmarks
/// and we'd rather respect a mid-run unset than win a few nanoseconds.
pub fn env_skip_null_check() -> bool {
    matches!(
        std::env::var("MORLOC_SKIP_NULL_CHECK").as_deref(),
        Ok("1") | Ok("true") | Ok("TRUE") | Ok("yes")
    )
}

/// The first string in a JSON text that holds a NUL byte, as a path such
/// as `[2].foo[3] (byte 4 of 9)` naming the string and the position of
/// the NUL in its decoded bytes. The check runs before a value is
/// dispatched to a pool, so a NUL found here fails with a clear
/// morloc-level error rather than propagating into a language that
/// cannot hold one.
///
/// The text is scanned once with an explicit container stack, so a value
/// of any depth costs stack space bounded by the schema, not the value.
/// Only string contents are decoded, and only a string that carries a
/// NUL escape. Malformed text is an error, not a pass: the caller
/// decides whether that rejects the request or skips a non-JSON argument.
pub fn first_null_in_json_text(text: &str) -> Result<Option<String>, String> {
    let b = text.as_bytes();
    let mut pos = 0usize;
    // Each open container: an array's element index, or the span of the
    // key of an object's current member. The path is built from these only
    // when a NUL is found.
    enum Ctx {
        Arr(usize),
        Obj(std::ops::Range<usize>),
    }
    let mut stack: Vec<Ctx> = Vec::new();
    let malformed = |pos: usize| format!("malformed JSON at byte {pos}");

    fn skip_ws(b: &[u8], pos: &mut usize) {
        while *pos < b.len() && matches!(b[*pos], b' ' | b'\t' | b'\n' | b'\r') {
            *pos += 1;
        }
    }
    // The span of the string whose opening quote is at `pos`, and whether
    // its contents carry a NUL escape.
    fn scan_string(b: &[u8], pos: &mut usize) -> Option<(std::ops::Range<usize>, bool)> {
        let start = *pos;
        let mut i = start + 1;
        let mut has_nul = false;
        while i < b.len() {
            i += memchr::memchr3(b'"', b'\\', 0, &b[i..])?;
            match b[i] {
                b'"' => {
                    *pos = i + 1;
                    return Some((start..i + 1, has_nul));
                }
                b'\\' => {
                    if i + 1 >= b.len() {
                        return None;
                    }
                    if b[i + 1] == b'u' && i + 5 < b.len() && &b[i + 2..i + 6] == b"0000" {
                        has_nul = true;
                    }
                    i += 2;
                }
                _ => {
                    has_nul = true;
                    i += 1;
                }
            }
        }
        None
    }
    // Position of the NUL inside the decoded string, or, when the string
    // holds a raw control byte and so does not decode, inside its raw text.
    fn nul_offset(slice: &str) -> Result<String, String> {
        match serde_json::from_str::<String>(slice) {
            Ok(s) => {
                let offset = s.as_bytes().iter().position(|&c| c == 0).ok_or_else(|| "no NUL".to_string())?;
                Ok(format!(" (byte {} of {})", offset, s.len()))
            }
            Err(e) => {
                let raw = &slice.as_bytes()[1..slice.len() - 1];
                match raw.iter().position(|&c| c == 0) {
                    Some(offset) => Ok(format!(" (raw byte {} of {})", offset, raw.len())),
                    None => Err(e.to_string()),
                }
            }
        }
    }
    // The key of an object member at `pos`, checked to be a JSON string.
    fn read_key(b: &[u8], text: &str, pos: &mut usize) -> Result<std::ops::Range<usize>, String> {
        skip_ws(b, pos);
        if *pos >= b.len() || b[*pos] != b'"' {
            return Err(format!("malformed JSON at byte {}", *pos));
        }
        let (span, _) = scan_string(b, pos).ok_or_else(|| format!("malformed JSON at byte {}", *pos))?;
        let plain = b[span.start + 1..span.end - 1].iter().all(|&c| c >= 0x20 && c != b'\\');
        if !plain {
            serde_json::from_str::<String>(&text[span.clone()]).map_err(|e| e.to_string())?;
        }
        skip_ws(b, pos);
        if *pos >= b.len() || b[*pos] != b':' {
            return Err(format!("malformed JSON at byte {}", *pos));
        }
        *pos += 1;
        Ok(span)
    }
    fn path_of(stack: &[Ctx], text: &str) -> Result<String, String> {
        let mut path = String::new();
        for c in stack {
            match c {
                Ctx::Arr(i) => path.push_str(&format!("[{i}]")),
                Ctx::Obj(key) => {
                    let k: String = serde_json::from_str(&text[key.clone()]).map_err(|e| e.to_string())?;
                    path.push('.');
                    path.push_str(&k);
                }
            }
        }
        Ok(path)
    }

    loop {
        // A value.
        skip_ws(b, &mut pos);
        if pos >= b.len() {
            return Err(malformed(pos));
        }
        match b[pos] {
            b'[' => {
                pos += 1;
                skip_ws(b, &mut pos);
                if pos < b.len() && b[pos] == b']' {
                    pos += 1;
                } else {
                    stack.push(Ctx::Arr(0));
                    continue;
                }
            }
            b'{' => {
                pos += 1;
                skip_ws(b, &mut pos);
                if pos < b.len() && b[pos] == b'}' {
                    pos += 1;
                } else {
                    let key = read_key(b, text, &mut pos)?;
                    stack.push(Ctx::Obj(key));
                    continue;
                }
            }
            b'"' => {
                let (span, has_nul) = scan_string(b, &mut pos).ok_or_else(|| malformed(pos))?;
                if has_nul {
                    return Ok(Some(format!("{}{}", path_of(&stack, text)?, nul_offset(&text[span])?)));
                }
            }
            _ => {
                let start = pos;
                while pos < b.len() && !matches!(b[pos], b',' | b']' | b'}' | b' ' | b'\t' | b'\n' | b'\r') {
                    pos += 1;
                }
                if pos == start {
                    return Err(malformed(pos));
                }
            }
        }
        // After a value: close containers or move to the next sibling.
        loop {
            skip_ws(b, &mut pos);
            match stack.last_mut() {
                None => {
                    return if pos == b.len() { Ok(None) } else { Err(malformed(pos)) };
                }
                Some(Ctx::Arr(i)) => match b.get(pos) {
                    Some(b',') => {
                        pos += 1;
                        *i += 1;
                        break;
                    }
                    Some(b']') => {
                        pos += 1;
                        stack.pop();
                    }
                    _ => return Err(malformed(pos)),
                },
                Some(Ctx::Obj(key)) => match b.get(pos) {
                    Some(b',') => {
                        pos += 1;
                        *key = read_key(b, text, &mut pos)?;
                        break;
                    }
                    Some(b'}') => {
                        pos += 1;
                        stack.pop();
                    }
                    _ => return Err(malformed(pos)),
                },
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::first_null_in_json_text;

    #[test]
    fn nul_is_located_by_path_and_decoded_offset() {
        assert_eq!(first_null_in_json_text("\"abc\"").unwrap(), None);
        assert_eq!(
            first_null_in_json_text("\"ab\\u0000c\"").unwrap().as_deref(),
            Some(" (byte 2 of 4)")
        );
        assert_eq!(
            first_null_in_json_text("[1, \"x\", {\"foo\": [true, \"\\u00e9\\u0000\"]}]").unwrap().as_deref(),
            Some("[2].foo[1] (byte 2 of 3)")
        );
        // A NUL escape inside a key is not a string value the pool sees.
        assert_eq!(first_null_in_json_text("{\"a\\u0000\": 1}").unwrap(), None);
        // Nested empties and whitespace.
        assert_eq!(first_null_in_json_text(" [ [ ], { } , \"\" ] ").unwrap(), None);
    }

    #[test]
    fn malformed_text_is_an_error_not_a_pass() {
        assert!(first_null_in_json_text("[1,").is_err());
        assert!(first_null_in_json_text("{\"a\" 1}").is_err());
        assert!(first_null_in_json_text("\"unterminated").is_err());
        assert!(first_null_in_json_text("").is_err());
        assert!(first_null_in_json_text("[1] x").is_err());
        assert!(first_null_in_json_text("hello world").is_err());
        assert!(first_null_in_json_text("{\"a\\x\": 1}").is_err());
        assert!(first_null_in_json_text("{\"a\u{1}\": 1}").is_err());
        assert!(first_null_in_json_text("{\"a\": 1, \"b\\q\": 2}").is_err());
    }

    #[test]
    fn a_raw_nul_byte_is_found_not_skipped() {
        assert_eq!(first_null_in_json_text("\"a\u{0}b\"").unwrap().as_deref(), Some(" (raw byte 1 of 3)"));
        assert_eq!(first_null_in_json_text("[\"x\", \"\u{0}\"]").unwrap().as_deref(), Some("[1] (raw byte 0 of 1)"));
    }

    #[test]
    fn nul_path_names_late_elements_and_escaped_keys() {
        let mut items: Vec<String> = (0..1500).map(|i| format!("\"s{i}\"")).collect();
        items[1234] = "\"ab\\u0000\"".to_string();
        let text = format!("{{\"k\\\"ey\": 7, \"xs\": [{}]}}", items.join(","));
        assert_eq!(
            first_null_in_json_text(&text).unwrap().as_deref(),
            Some(".xs[1234] (byte 2 of 3)")
        );
        let text = format!("{{\"k\\\"ey\": [{}]}}", items.join(","));
        assert_eq!(
            first_null_in_json_text(&text).unwrap().as_deref(),
            Some(".k\"ey[1234] (byte 2 of 3)")
        );
    }

    #[test]
    fn deep_text_is_scanned_without_recursion() {
        let depth = 200_000;
        let text = format!("{}\"\\u0000\"{}", "[".repeat(depth), "]".repeat(depth));
        let t = std::thread::Builder::new()
            .stack_size(256 * 1024)
            .spawn(move || first_null_in_json_text(&text).unwrap().unwrap())
            .unwrap();
        assert_eq!(t.join().unwrap(), format!("{} (byte 0 of 1)", "[0]".repeat(depth)));
    }
}
