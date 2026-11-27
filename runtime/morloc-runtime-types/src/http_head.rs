//! The head of an HTTP/1.1 request, read the same way by every server
//! (NET-10).

/// A request head that parsed.
pub struct Head {
    /// One of `METHODS`.
    pub method: &'static str,
    pub path: String,
    /// The part of the target after `?`; empty when absent.
    pub query: String,
    /// Header names lowercased, values trimmed; bytes that are not UTF-8
    /// are replaced.
    pub headers: Vec<(String, String)>,
    pub content_length: usize,
}

/// Why a head was refused; `too_large` marks a length past every cap.
#[derive(Debug)]
pub struct HeadError {
    pub why: String,
    pub too_large: bool,
}

fn refuse(why: impl Into<String>) -> HeadError {
    HeadError { why: why.into(), too_large: false }
}

pub const METHODS: [&str; 5] = ["GET", "POST", "DELETE", "OPTIONS", "HEAD"];

impl Head {
    /// The value of the first header named `name` (any case).
    pub fn header(&self, name: &str) -> Option<&str> {
        header_in(&self.headers, name)
    }
}

/// The value of the first header named `name` (any case) in a lowercased
/// header list.
pub fn header_in<'a>(headers: &'a [(String, String)], name: &str) -> Option<&'a str> {
    headers.iter().find(|(k, _)| k.eq_ignore_ascii_case(name)).map(|(_, v)| v.as_str())
}

/// Parse a request head: its bytes up to and including the blank line that
/// ends it. Lines end in CRLF or a bare LF.
pub fn parse(head: &[u8]) -> Result<Head, HeadError> {
    let mut lines = head.split(|&b| b == b'\n').map(|l| l.strip_suffix(b"\r").unwrap_or(l));
    let (method, path, query) = request_line(lines.next().unwrap_or(b""))?;
    let mut headers = Vec::new();
    for line in lines {
        if line.is_empty() {
            break;
        }
        let text = String::from_utf8_lossy(line);
        let Some((name, value)) = text.split_once(':') else {
            return Err(refuse(format!("malformed header line: {}", text)));
        };
        if name.is_empty() || !name.bytes().all(|b| b.is_ascii_graphic()) {
            return Err(refuse(format!("malformed header name: {:?}", name)));
        }
        headers.push((name.to_ascii_lowercase(), value.trim().to_string()));
    }
    if header_in(&headers, "transfer-encoding").is_some() {
        return Err(refuse("Transfer-Encoding is not supported; send the body with a Content-Length"));
    }
    let content_length =
        content_length(headers.iter().filter(|(k, _)| k == "content-length").map(|(_, v)| v.as_str()))?;
    Ok(Head { method, path, query, headers, content_length })
}

/// The method, path and query of a request line `METHOD SP target SP
/// HTTP/x`, every byte visible ASCII.
fn request_line(line: &[u8]) -> Result<(&'static str, String, String), HeadError> {
    let text = std::str::from_utf8(line)
        .ok()
        .filter(|t| t.bytes().all(|b| b == b' ' || b.is_ascii_graphic()))
        .ok_or_else(|| refuse("the request line holds a byte that is not visible ASCII"))?;
    let parts: Vec<&str> = text.split(' ').collect();
    let [method, target, version] = parts[..] else {
        return Err(refuse(format!("malformed request line: {text}")));
    };
    if !version.starts_with("HTTP/") || target.is_empty() {
        return Err(refuse(format!("malformed request line: {text}")));
    }
    let method = METHODS
        .into_iter()
        .find(|m| *m == method)
        .ok_or_else(|| refuse(format!("unsupported HTTP method: {method}")))?;
    let (path, query) = target.split_once('?').unwrap_or((target, ""));
    Ok((method, path.to_string(), query.to_string()))
}

/// The body length declared by a request's `Content-Length` header values:
/// digits only, and every copy must agree (RFC 9110 8.6); none means 0. A
/// length too large to represent is past every cap.
pub fn content_length<'a>(values: impl IntoIterator<Item = &'a str>) -> Result<usize, HeadError> {
    let mut found: Option<&str> = None;
    for value in values {
        let value = value.trim();
        if value.is_empty() || !value.bytes().all(|b| b.is_ascii_digit()) {
            return Err(refuse(format!("invalid Content-Length: {}", value)));
        }
        match found {
            Some(prev) if prev != value => return Err(refuse("conflicting Content-Length headers")),
            _ => found = Some(value),
        }
    }
    found.map_or(Ok(0), |v| {
        v.parse().map_err(|_| HeadError { why: format!("the declared body of {} bytes is too large", v), too_large: true })
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_content_length_is_digits_and_its_copies_agree() {
        assert_eq!(content_length(["5"]).ok(), Some(5));
        assert_eq!(content_length([]).ok(), Some(0));
        assert!(content_length(["+5"]).is_err());
        assert!(content_length(["5x"]).is_err());
        assert!(content_length(["2", "40"]).is_err());
        assert_eq!(content_length(["2", " 2"]).ok(), Some(2));
        assert!(content_length(["99999999999999999999999"]).is_err_and(|e| e.too_large));
    }

    #[test]
    fn a_request_line_is_three_visible_ascii_parts() {
        let ok = request_line(b"GET /a?b=c HTTP/1.1").ok().unwrap();
        assert_eq!(ok, ("GET", "/a".to_string(), "b=c".to_string()));
        for bad in [
            &b"POST /call/f\0oo HTTP/1.1"[..],
            b"GET /health",
            b"GET  /health HTTP/1.1",
            b"GET /health HTTP/1.1 x",
            b"GET /h\xc3\xa9 HTTP/1.1",
            b"BREW /pot HTTP/1.1",
            b"GET /health FTP",
            b"",
        ] {
            assert!(request_line(bad).is_err(), "{:?}", String::from_utf8_lossy(bad));
        }
    }

    #[test]
    fn a_header_is_found_by_name_in_any_case() {
        let h = parse(b"POST /call/f HTTP/1.1\r\nHost: x\r\nauthorization: Bearer t\r\n\r\n").ok().unwrap();
        assert_eq!(h.header("Authorization"), Some("Bearer t"));
        assert_eq!(h.header("content-length"), None);
        let h = parse(b"GET /authorization:x HTTP/1.1\r\n\r\n");
        assert!(h.is_ok_and(|h| h.header("authorization").is_none()));
    }

    #[test]
    fn a_header_that_is_not_utf8_leaves_the_request_readable() {
        let h = parse(b"GET /health HTTP/1.1\r\nUser-Agent: \xe9t\xe9\r\n\r\n").ok().unwrap();
        assert_eq!(h.path, "/health");
    }

    #[test]
    fn a_head_that_could_be_read_two_ways_is_refused() {
        for bad in [
            &b"POST /x HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n"[..],
            b"POST /x HTTP/1.1\r\nContent-Length : 5\r\n\r\n",
            b"POST /x HTTP/1.1\r\nno colon here\r\n\r\n",
            b"POST /x HTTP/1.1\r\nContent-Length: 2\r\ncontent-length: 5\r\n\r\n",
        ] {
            assert!(parse(bad).is_err(), "{:?}", String::from_utf8_lossy(bad));
        }
        assert_eq!(parse(b"POST /x HTTP/1.1\nContent-Length: 3\n\n").ok().map(|h| h.content_length), Some(3));
    }
}
