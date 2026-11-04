//! C ABI wrappers for HTTP request/response handling.
//! Replaces http.c.

use std::ffi::c_char;
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};

const HTTP_MAX_HEADERS: usize = 8192;
const HTTP_MAX_REQUEST: usize = 4 * 1024 * 1024;

// -- Parsed HTTP requests --

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HttpMethod {
    Get,
    Post,
    Delete,
    Options,
    Head,
}

pub struct HttpRequest {
    pub method: HttpMethod,
    pub path: String,
    pub body: Vec<u8>,
    /// Query string (the part after `?`), empty when absent. Kept separate from
    /// `path` so path matching stays exact.
    pub query: String,
    /// Whether the request carries the daemon's bearer token, or none is set.
    pub authorized: bool,
}

/// Why a request was refused before routing; `too_large` marks one past a
/// cap (NET-10).
pub struct Refusal {
    pub why: String,
    pub too_large: bool,
}

fn refuse(why: impl Into<String>) -> Refusal {
    Refusal { why: why.into(), too_large: false }
}

// -- C-compatible types --

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DaemonMethod {
    Call = 0,
    Discover = 1,
    Health = 2,
    Eval = 3,
    Typecheck = 4,
    Bind = 5,
    Bindings = 6,
    Unbind = 7,
}

#[repr(C)]
pub struct DaemonRequest {
    pub id: *mut c_char,
    pub method: DaemonMethod,
    pub command: *mut c_char,
    pub args_json: *mut c_char,
    pub expr: *mut c_char,
    pub name: *mut c_char,
    /// Output projection selected by `?render=<flag>` on a `/call` (HTTP only);
    /// null when absent. `daemon_dispatch` resolves it to the projection's entry
    /// command; `raw` means the command's own typed value.
    pub render: *mut c_char,
    /// Set by the serving front-end's forward: request the raw-media form so an
    /// `@mime` return comes back as result_bytes+mime (base64 over the wire).
    /// Direct length-prefixed clients leave it false and keep the JSON `result`.
    pub media: bool,
    /// Select the command's `@default` action (its own value when it has
    /// none). Exclusive with `render`; both absent selects the own value.
    pub render_default: bool,
}

// -- http_parse_request --

pub(crate) fn http_parse_request(fd: i32) -> Result<HttpRequest, Refusal> {
    parse_request_within(fd, morloc_runtime_types::net_limits::HEAD_LIMIT)
}

fn parse_request_within(fd: i32, head_limit: std::time::Duration) -> Result<HttpRequest, Refusal> {
    use morloc_runtime_types::net_limits::{body_limit, peek_by, recv_by};
    // NET-2
    let head_until = std::time::Instant::now() + head_limit;

    // Peek, then consume only through the blank line, so the socket is left
    // at the body.
    let mut header_buf = vec![0u8; HTTP_MAX_HEADERS];
    let mut header_len: usize = 0;
    let mut head_done = false;

    while header_len < HTTP_MAX_HEADERS - 1 {
        let room = HTTP_MAX_HEADERS - 1 - header_len;
        let n = match unsafe { peek_by(fd, header_buf.as_mut_ptr().add(header_len), room, head_until) } {
            Ok(0) => return Err(refuse("Connection closed while reading HTTP headers")),
            Ok(n) => n,
            Err(e) => return Err(refuse(format!("Reading HTTP headers: {e}"))),
        };
        let from = header_len.saturating_sub(3);
        let take = match header_buf[from..header_len + n].windows(4).position(|w| w == b"\r\n\r\n") {
            Some(p) => {
                head_done = true;
                from + p + 4 - header_len
            }
            None => n,
        };
        let mut got = 0;
        while got < take {
            match unsafe { recv_by(fd, header_buf.as_mut_ptr().add(header_len + got), take - got, head_until) } {
                Ok(k) if k > 0 => got += k,
                Err(e) => return Err(refuse(format!("Reading HTTP headers: {e}"))),
                Ok(_) => return Err(refuse("Connection closed while reading HTTP headers")),
            }
        }
        header_len += take;
        if head_done {
            break;
        }
    }

    if !head_done {
        return Err(Refusal { why: "the request headers are larger than this server takes".into(), too_large: true });
    }

    let head = morloc_runtime_types::http_head::parse(&header_buf[..header_len])
        .map_err(|e| Refusal { why: e.why, too_large: e.too_large })?;
    let method = match head.method {
        "GET" => HttpMethod::Get,
        "POST" => HttpMethod::Post,
        "DELETE" => HttpMethod::Delete,
        "OPTIONS" => HttpMethod::Options,
        "HEAD" => HttpMethod::Head,
        other => return Err(refuse(format!("unsupported HTTP method: {other}"))),
    };
    let authorized = match crate::daemon_ffi::http_token() {
        None => true,
        Some(token) => morloc_runtime_types::bearer::authorizes(head.header("authorization"), &token),
    };

    let content_length = head.content_length;
    if content_length > HTTP_MAX_REQUEST {
        return Err(Refusal {
            why: format!("the request body is {} bytes; this server takes at most {}", content_length, HTTP_MAX_REQUEST),
            too_large: true,
        });
    }

    let mut body = vec![0u8; content_length];
    // NET-2
    let body_until = std::time::Instant::now() + body_limit(content_length);
    let mut total = 0;
    while total < content_length {
        match unsafe { recv_by(fd, body.as_mut_ptr().add(total), content_length - total, body_until) } {
            Ok(n) if n > 0 => total += n,
            Err(e) => return Err(refuse(format!("Reading HTTP body: {e}"))),
            Ok(_) => return Err(refuse("Connection closed while reading HTTP body")),
        }
    }

    Ok(HttpRequest { method, path: head.path, body, query: head.query, authorized })
}

// -- http_write_response --

fn http_status_text(status: i32) -> &'static str {
    morloc_runtime_types::http_query::reason_phrase(status)
}

pub(crate) unsafe fn write_response(
    fd: i32,
    status: i32,
    content_type: *const c_char,
    body: *const c_char,
    body_len: usize,
) -> bool {
    write_response_ex(fd, status, content_type, body, body_len, ptr::null())
}

/// Same as `http_write_response`, plus an optional `extra_headers` block
/// (one or more `Name: value\r\n` lines, NUL-terminated, may be NULL).
/// Used for adding `Retry-After: 1` on 503 responses.
pub(crate) unsafe fn write_response_ex(
    fd: i32,
    status: i32,
    content_type: *const c_char,
    body: *const c_char,
    body_len: usize,
    extra_headers: *const c_char,
) -> bool {
    crate::daemon_ffi::note_reply_started();
    let ct = if content_type.is_null() {
        "application/json"
    } else {
        std::ffi::CStr::from_ptr(content_type).to_str().unwrap_or("application/json")
    };
    let extra = if extra_headers.is_null() {
        ""
    } else {
        std::ffi::CStr::from_ptr(extra_headers).to_str().unwrap_or("")
    };

    let header = format!(
        "HTTP/1.1 {} {}\r\n\
         Content-Type: {}\r\n\
         Content-Length: {}\r\n\
         Connection: close\r\n\
         Access-Control-Allow-Origin: *\r\n\
         Access-Control-Allow-Methods: GET, HEAD, POST, DELETE, OPTIONS\r\n\
         Access-Control-Allow-Headers: Content-Type, Authorization\r\n\
         {}\r\n",
        status, http_status_text(status), ct, body_len, extra
    );

    // send_all retries on EAGAIN (non-blocking client fd on macOS) and on a
    // short header/body send, rather than truncating the response mid-message.
    if !crate::ipc_ffi::send_all(fd, header.as_ptr(), header.len()) {
        return false;
    }

    if !body.is_null() && body_len > 0 && !crate::daemon_ffi::reply_without_body() {
        if !crate::ipc_ffi::send_all(fd, body as *const u8, body_len) {
            return false;
        }
    }

    true
}

// -- http_to_daemon_request --

/// Read a top-level string field out of a JSON request body.
///
/// A real JSON parser, not a scan: the escape sequences matter here. An
/// expression that imports a module spans lines, and a line break can only
/// reach the daemon as `\n`.
///
/// The body's other members are kept as text and never built into a
/// tree, so their shape and depth cost nothing; a body that is not a JSON
/// object is an error the route reports, not a missing field.
fn extract_json_string(body: &str, key: &str) -> Result<Option<String>, String> {
    let members: std::collections::HashMap<String, &serde_json::value::RawValue> =
        serde_json::from_str(body.trim()).map_err(|e| e.to_string())?;
    let Some(raw) = members.get(key) else { return Ok(None) };
    Ok(serde_json::from_str::<String>(raw.get()).ok())
}

/// The string member `key` of a route's JSON body, or the request-level
/// error for a body that is not JSON.
unsafe fn body_string(
    body: &str,
    key: &str,
    route: &str,
    dreq: *mut DaemonRequest,
    errmsg: *mut *mut c_char,
) -> Result<Option<String>, ()> {
    match extract_json_string(body, key) {
        Ok(v) => Ok(v),
        Err(e) => {
            crate::daemon_ffi::daemon_free_request(dreq);
            set_errmsg(errmsg, &MorlocError::Other(format!("Malformed JSON in {} body: {}", route, e)));
            Err(())
        }
    }
}

/// `text` as a malloc'd C string, or the refusal of the request when its
/// `what` holds a NUL character.
unsafe fn c_dup(
    text: &str,
    what: &str,
    dreq: *mut DaemonRequest,
    errmsg: *mut *mut c_char,
) -> Result<*mut c_char, ()> {
    match std::ffi::CString::new(text) {
        Ok(c) => Ok(libc::strdup(c.as_ptr())),
        Err(_) => {
            crate::daemon_ffi::daemon_free_request(dreq);
            set_errmsg(errmsg, &MorlocError::Other(format!("the {} holds a NUL character", what)));
            Err(())
        }
    }
}

/// The eval-family method a request routes to, if any, read from its method
/// and path alone, before its body.
pub(crate) fn eval_route(req: &HttpRequest) -> Option<DaemonMethod> {
    match (req.method, req.path.as_str()) {
        (HttpMethod::Post, "/eval") => Some(DaemonMethod::Eval),
        (HttpMethod::Post, "/typecheck") => Some(DaemonMethod::Typecheck),
        (HttpMethod::Post, "/bind") => Some(DaemonMethod::Bind),
        (HttpMethod::Get, "/bindings") => Some(DaemonMethod::Bindings),
        (HttpMethod::Delete, p) if p.starts_with("/bindings/") => Some(DaemonMethod::Unbind),
        _ => None,
    }
}

/// Translate a parsed HTTP request into a daemon request. On failure,
/// `errmsg` carries the human-readable cause and `error_kind` carries the
/// classification (DAEMON_ERROR_BAD_REQUEST for malformed body / missing
/// fields, DAEMON_ERROR_NOT_FOUND for an unrecognized route). `error_kind`
/// may be NULL if the caller doesn't care.
pub(crate) unsafe fn http_to_daemon_request(
    req: &HttpRequest,
    errmsg: *mut *mut c_char,
    error_kind: *mut i32,
) -> *mut DaemonRequest {
    clear_errmsg(errmsg);
    // Default classification for any failure below is BAD_REQUEST; the
    // unknown-endpoint branch promotes to NOT_FOUND explicitly.
    if !error_kind.is_null() {
        *error_kind = crate::daemon_ffi::DAEMON_ERROR_BAD_REQUEST;
    }

    let dreq = libc::calloc(1, std::mem::size_of::<DaemonRequest>()) as *mut DaemonRequest;
    if dreq.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Failed to allocate daemon_request_t".into()));
        return ptr::null_mut();
    }

    let path = req.path.as_str();
    let method = req.method;

    let body_str = match std::str::from_utf8(&req.body) {
        Ok(text) => text,
        Err(_) => {
            crate::daemon_ffi::daemon_free_request(dreq);
            set_errmsg(errmsg, &MorlocError::Other("the request body is not UTF-8".into()));
            return ptr::null_mut();
        }
    };

    // GET /health
    if method == HttpMethod::Get && path == "/health" {
        (*dreq).method = DaemonMethod::Health;
        return dreq;
    }

    // GET /discover
    if method == HttpMethod::Get && path == "/discover" {
        (*dreq).method = DaemonMethod::Discover;
        return dreq;
    }

    // POST /eval
    if method == HttpMethod::Post && path == "/eval" {
        (*dreq).method = DaemonMethod::Eval;
        let Ok(expr) = body_string(body_str, "expr", "/eval", dreq, errmsg) else {
            return ptr::null_mut();
        };
        if let Some(expr) = expr {
            let Ok(c) = c_dup(&expr, "expression", dreq, errmsg) else {
                return ptr::null_mut();
            };
            (*dreq).expr = c;
        }
        if (*dreq).expr.is_null() {
            crate::daemon_ffi::daemon_free_request(dreq);
            set_errmsg(errmsg, &MorlocError::Other("Missing 'expr' field in /eval request body".into()));
            return ptr::null_mut();
        }
        return dreq;
    }

    // POST /typecheck
    if method == HttpMethod::Post && path == "/typecheck" {
        (*dreq).method = DaemonMethod::Typecheck;
        let Ok(expr) = body_string(body_str, "expr", "/typecheck", dreq, errmsg) else {
            return ptr::null_mut();
        };
        if let Some(expr) = expr {
            let Ok(c) = c_dup(&expr, "expression", dreq, errmsg) else {
                return ptr::null_mut();
            };
            (*dreq).expr = c;
        }
        if (*dreq).expr.is_null() {
            crate::daemon_ffi::daemon_free_request(dreq);
            set_errmsg(errmsg, &MorlocError::Other("Missing 'expr' field in /typecheck request body".into()));
            return ptr::null_mut();
        }
        return dreq;
    }

    // POST /bind
    if method == HttpMethod::Post && path == "/bind" {
        (*dreq).method = DaemonMethod::Bind;
        let Ok(expr) = body_string(body_str, "expr", "/bind", dreq, errmsg) else {
            return ptr::null_mut();
        };
        if let Some(expr) = expr {
            let Ok(c) = c_dup(&expr, "expression", dreq, errmsg) else {
                return ptr::null_mut();
            };
            (*dreq).expr = c;
        }
        let Ok(name) = body_string(body_str, "name", "/bind", dreq, errmsg) else {
            return ptr::null_mut();
        };
        if let Some(name) = name {
            let Ok(c) = c_dup(&name, "name", dreq, errmsg) else {
                return ptr::null_mut();
            };
            (*dreq).name = c;
        }
        if (*dreq).expr.is_null() {
            crate::daemon_ffi::daemon_free_request(dreq);
            set_errmsg(errmsg, &MorlocError::Other("Missing 'expr' field in /bind request body".into()));
            return ptr::null_mut();
        }
        return dreq;
    }

    // GET /bindings
    if method == HttpMethod::Get && path == "/bindings" {
        (*dreq).method = DaemonMethod::Bindings;
        return dreq;
    }

    // DELETE /bindings/<name>
    if method == HttpMethod::Delete && path.starts_with("/bindings/") {
        let name = &path[10..];
        if name.is_empty() {
            crate::daemon_ffi::daemon_free_request(dreq);
            set_errmsg(errmsg, &MorlocError::Other("Missing binding name in /bindings/ path".into()));
            return ptr::null_mut();
        }
        (*dreq).method = DaemonMethod::Unbind;
        let Ok(c) = c_dup(&name, "name", dreq, errmsg) else {
            return ptr::null_mut();
        };
        (*dreq).name = c;
        return dreq;
    }

    // POST /call/<command>[?render=<flag>]
    if method == HttpMethod::Post && path.starts_with("/call/") {
        let cmd_name = &path[6..];
        if cmd_name.is_empty() {
            crate::daemon_ffi::daemon_free_request(dreq);
            set_errmsg(errmsg, &MorlocError::Other("Missing command name in /call/ path".into()));
            return ptr::null_mut();
        }
        (*dreq).method = DaemonMethod::Call;
        let Ok(c) = c_dup(cmd_name, "command name", dreq, errmsg) else {
            return ptr::null_mut();
        };
        (*dreq).command = c;
        // `?render=<flag>` selects an output projection; resolved in
        // daemon_dispatch against the command's terminals.
        let query = req.query.as_str();
        match morloc_runtime_types::http_query::render_selection(query) {
            Ok(Some(render)) => {
                let Ok(c) = c_dup(render, "render flag", dreq, errmsg) else {
                    return ptr::null_mut();
                };
                (*dreq).render = c;
            }
            Ok(None) => (*dreq).render_default = true,
            Err(msg) => {
                crate::daemon_ffi::daemon_free_request(dreq);
                set_errmsg(errmsg, &MorlocError::Other(msg.into()));
                return ptr::null_mut();
            }
        }

        let body = &req.body[..];
        match morloc_runtime_types::call_body::args_of(body) {
            Ok(Some(args)) => {
                let Ok(c) = c_dup(args, "arguments", dreq, errmsg) else {
                    return ptr::null_mut();
                };
                (*dreq).args_json = c;
            }
            Ok(None) => {}
            Err(e) => {
                crate::daemon_ffi::daemon_free_request(dreq);
                set_errmsg(errmsg, &MorlocError::Other(e));
                return ptr::null_mut();
            }
        }
        return dreq;
    }

    // OPTIONS preflight is short-circuited to 204 No Content by
    // handle_http_connection before this function is reached. If an
    // OPTIONS request somehow falls through here, treat it as an
    // unknown endpoint (NOT_FOUND below).

    crate::daemon_ffi::daemon_free_request(dreq);
    let method_str = match method {
        HttpMethod::Get => "GET",
        HttpMethod::Post => "POST",
        HttpMethod::Delete => "DELETE",
        HttpMethod::Options => "OPTIONS",
        HttpMethod::Head => "HEAD",
    };
    if !error_kind.is_null() {
        *error_kind = crate::daemon_ffi::DAEMON_ERROR_NOT_FOUND;
    }
    set_errmsg(errmsg, &MorlocError::Other(format!("Unknown HTTP endpoint: {} {}", method_str, path)));
    ptr::null_mut()
}

#[cfg(test)]
mod body_tests {
    #[test]
    fn a_request_head_trickling_in_ends_at_its_deadline() {
        let mut fds = [0; 2];
        assert_eq!(unsafe { libc::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()) }, 0);
        let (reader, writer) = (fds[0], fds[1]);
        let dripper = std::thread::spawn(move || {
            for b in b"POST /call/f HTTP/1.1\r\nHost: x\r\nX-Pad: aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\r\n" {
                if unsafe { libc::send(writer, b as *const u8 as *const libc::c_void, 1, libc::MSG_NOSIGNAL) } != 1 {
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(10));
            }
            unsafe { libc::close(writer) };
        });
        let start = std::time::Instant::now();
        let Err(refusal) = super::parse_request_within(reader, std::time::Duration::from_millis(300)) else {
            panic!("a trickling head was accepted");
        };
        assert!(refusal.why.contains("too long"), "{}", refusal.why);
        assert!(start.elapsed() < std::time::Duration::from_millis(800));
        unsafe { libc::close(reader) };
        dripper.join().unwrap();
    }

    fn parse(fd: i32) -> (String, String) {
        match super::parse_request_within(fd, std::time::Duration::from_secs(5)) {
            Ok(r) => (r.path, String::from_utf8_lossy(&r.body).into_owned()),
            Err(e) => panic!("{}", e.why),
        }
    }

    fn pair() -> (i32, i32) {
        let mut fds = [0; 2];
        assert_eq!(unsafe { libc::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()) }, 0);
        (fds[0], fds[1])
    }

    fn send_all(fd: i32, bytes: &[u8]) {
        let n = unsafe { libc::send(fd, bytes.as_ptr() as *const libc::c_void, bytes.len(), libc::MSG_NOSIGNAL) };
        assert_eq!(n, bytes.len() as isize);
    }

    #[test]
    fn pipelined_requests_on_one_connection_parse_in_turn() {
        let (reader, writer) = pair();
        send_all(
            writer,
            b"POST /call/f HTTP/1.1\r\nContent-Length: 3\r\n\r\n[1]\
              POST /call/g HTTP/1.1\r\ncontent-length: 4\r\n\r\n[22]\
              GET /health HTTP/1.1\r\nHost: x\r\n\r\n",
        );
        assert_eq!(parse(reader), ("/call/f".to_string(), "[1]".to_string()));
        assert_eq!(parse(reader), ("/call/g".to_string(), "[22]".to_string()));
        assert_eq!(parse(reader), ("/health".to_string(), String::new()));
        unsafe {
            libc::close(reader);
            libc::close(writer);
        }
    }

    #[test]
    fn a_long_path_and_query_are_kept_whole() {
        let name = "c".repeat(300);
        let render = "r".repeat(300);
        let (reader, writer) = pair();
        send_all(writer, format!("POST /call/{name}?render={render} HTTP/1.1\r\n\r\n").as_bytes());
        let r = super::parse_request_within(reader, std::time::Duration::from_secs(5)).ok().unwrap();
        assert_eq!(r.path, format!("/call/{name}"));
        assert_eq!(r.query, format!("render={render}"));
        unsafe {
            libc::close(reader);
            libc::close(writer);
        }
    }

    #[test]
    fn the_body_length_comes_from_the_content_length_header_alone() {
        let (reader, writer) = pair();
        send_all(
            writer,
            b"POST /call/f HTTP/1.1\r\nX-Note: content-length: 99\r\nContent-Length: 2\r\n\r\n[]\
              GET /health HTTP/1.1\r\n\r\n",
        );
        assert_eq!(parse(reader), ("/call/f".to_string(), "[]".to_string()));
        assert_eq!(parse(reader), ("/health".to_string(), String::new()));
        unsafe {
            libc::close(reader);
            libc::close(writer);
        }
    }

    #[test]
    fn a_head_whose_end_is_split_across_sends_is_found() {
        let (reader, writer) = pair();
        let sender = std::thread::spawn(move || {
            for part in [&b"POST /call/f HTTP/1.1\r\nContent-Length: 2\r"[..], b"\n\r", b"\n[", b"]GET /x HTTP/1.1\r\n\r\n"] {
                send_all(writer, part);
                std::thread::sleep(std::time::Duration::from_millis(20));
            }
            writer
        });
        assert_eq!(parse(reader), ("/call/f".to_string(), "[]".to_string()));
        assert_eq!(parse(reader), ("/x".to_string(), String::new()));
        let writer = sender.join().unwrap();
        unsafe {
            libc::close(reader);
            libc::close(writer);
        }
    }

    use super::*;
    use std::ffi::CStr;

    fn to_daemon(method: HttpMethod, path: &str, body: &str) -> (*mut DaemonRequest, Option<String>) {
        let (path, query) = path.split_once('?').unwrap_or((path, ""));
        let req = HttpRequest {
            method,
            path: path.to_string(),
            body: body.as_bytes().to_vec(),
            query: query.to_string(),
            authorized: false,
        };
        let mut err: *mut c_char = ptr::null_mut();
        let dreq = unsafe { http_to_daemon_request(&req, &mut err, ptr::null_mut()) };
        let msg = (!err.is_null()).then(|| {
            let m = unsafe { CStr::from_ptr(err) }.to_string_lossy().into_owned();
            unsafe { libc::free(err as *mut std::ffi::c_void) };
            m
        });
        (dreq, msg)
    }

    fn args_of(dreq: *mut DaemonRequest) -> String {
        let s = unsafe { CStr::from_ptr((*dreq).args_json) }.to_string_lossy().into_owned();
        unsafe { crate::daemon_ffi::daemon_free_request(dreq) };
        s
    }

    #[test]
    fn a_body_field_holding_nul_is_refused() {
        let (d, e) = to_daemon(HttpMethod::Post, "/eval", "{\"expr\":\"f\\u0000x\"}");
        assert!(d.is_null());
        assert!(e.is_some_and(|m| m.contains("NUL")));
    }

    #[test]
    fn a_long_name_routes_whole() {
        let (cmd, render, name) = ("c".repeat(300), "r".repeat(300), "b".repeat(300));
        let (d, e) = to_daemon(HttpMethod::Post, &format!("/call/{cmd}?render={render}"), "[]");
        assert_eq!(e, None);
        unsafe {
            assert_eq!(CStr::from_ptr((*d).command).to_str().unwrap(), cmd);
            assert_eq!(CStr::from_ptr((*d).render).to_str().unwrap(), render);
            crate::daemon_ffi::daemon_free_request(d);
        }
        let (d, e) = to_daemon(HttpMethod::Delete, &format!("/bindings/{name}"), "");
        assert_eq!(e, None);
        unsafe {
            assert_eq!(CStr::from_ptr((*d).name).to_str().unwrap(), name);
            crate::daemon_ffi::daemon_free_request(d);
        }
    }

    #[test]
    fn a_daemon_reads_a_request_whose_header_is_not_utf8() {
        let (reader, writer) = pair();
        send_all(writer, b"GET /health HTTP/1.1\r\nUser-Agent: \xe9t\xe9\r\n\r\n");
        assert_eq!(parse(reader), ("/health".to_string(), String::new()));
        unsafe {
            libc::close(reader);
            libc::close(writer);
        }
    }

    // The args of a /call body reach the daemon as the text they arrived
    // in, whether the body is the bare array or wraps it in an object.
    #[test]
    fn call_args_pass_through_as_text() {
        let deep = format!("{}1{}", "[".repeat(400), "]".repeat(400));
        let bare = format!("[18446744073709551617, {deep}]");
        let (d, e) = to_daemon(HttpMethod::Post, "/call/f", &bare);
        assert_eq!(e, None);
        assert_eq!(args_of(d), bare);
        let (d, e) = to_daemon(HttpMethod::Post, "/call/f", &format!(" {{\"args\": {bare} }} "));
        assert_eq!(e, None);
        assert_eq!(args_of(d), bare);
        let (d, e) = to_daemon(HttpMethod::Post, "/call/f", &format!(" {{\"args\": {bare} , \"x\": null}} "));
        assert!(d.is_null() && e.is_some());
        let (d, e) = to_daemon(HttpMethod::Post, "/call/f", "[1,");
        assert!(d.is_null() && e.is_some());
    }

    #[test]
    fn eval_body_that_is_not_json_is_rejected() {
        let (d, e) = to_daemon(HttpMethod::Post, "/eval", "{\"expr\": \"1 + 1\", \"other\": [[[[]]]]}");
        assert_eq!(e, None);
        assert_eq!(unsafe { CStr::from_ptr((*d).expr) }.to_str().unwrap(), "1 + 1");
        unsafe { crate::daemon_ffi::daemon_free_request(d) };
        let (d, e) = to_daemon(HttpMethod::Post, "/eval", "{\"expr\": ");
        assert!(d.is_null());
        assert!(e.unwrap().contains("Malformed JSON in /eval body"));
    }
}
