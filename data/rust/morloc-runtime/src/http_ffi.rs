//! C ABI wrappers for HTTP request/response handling.
//! Replaces http.c.

use std::ffi::{c_char, c_void};
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};

const HTTP_MAX_HEADERS: usize = 8192;
const HTTP_MAX_REQUEST: usize = 4 * 1024 * 1024;

// ── C-compatible types ───────────────────────────────────────────────────────

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HttpMethod {
    Get = 0,
    Post = 1,
    Delete = 2,
    Options = 3,
}

#[repr(C)]
pub struct HttpRequest {
    pub method: HttpMethod,
    pub path: [c_char; 256],
    pub body: *mut c_char,
    pub body_len: usize,
}

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
}

// ── http_parse_request ───────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn http_parse_request(
    fd: i32,
    errmsg: *mut *mut c_char,
) -> *mut HttpRequest {
    clear_errmsg(errmsg);

    // Read headers byte by byte until \r\n\r\n
    let mut header_buf = vec![0u8; HTTP_MAX_HEADERS];
    let mut header_len: usize = 0;
    let mut header_end_pos: Option<usize> = None;

    while header_len < HTTP_MAX_HEADERS - 1 {
        let n = libc::recv(fd, header_buf.as_mut_ptr().add(header_len) as *mut c_void, 1, 0);
        if n <= 0 {
            set_errmsg(errmsg, &MorlocError::Other("Connection closed while reading HTTP headers".into()));
            return ptr::null_mut();
        }
        header_len += 1;

        if header_len >= 4 {
            let tail = &header_buf[header_len - 4..header_len];
            if tail == b"\r\n\r\n" {
                header_end_pos = Some(header_len - 4);
                break;
            }
        }
    }

    let header_end = match header_end_pos {
        Some(p) => p,
        None => {
            set_errmsg(errmsg, &MorlocError::Other("HTTP headers too large or malformed".into()));
            return ptr::null_mut();
        }
    };

    let header_str = std::str::from_utf8(&header_buf[..header_len]).unwrap_or("");

    // Allocate request
    let req = libc::calloc(1, std::mem::size_of::<HttpRequest>()) as *mut HttpRequest;
    if req.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Failed to allocate http_request_t".into()));
        return ptr::null_mut();
    }

    // Parse method
    if header_str.starts_with("GET ") {
        (*req).method = HttpMethod::Get;
    } else if header_str.starts_with("POST ") {
        (*req).method = HttpMethod::Post;
    } else if header_str.starts_with("DELETE ") {
        (*req).method = HttpMethod::Delete;
    } else if header_str.starts_with("OPTIONS ") {
        (*req).method = HttpMethod::Options;
    } else {
        libc::free(req as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Other("Unsupported HTTP method".into()));
        return ptr::null_mut();
    }

    // Parse path
    let first_space = header_str.find(' ').unwrap_or(0) + 1;
    let path_end = header_str[first_space..].find(' ').map(|p| first_space + p).unwrap_or(first_space);
    let path = &header_str[first_space..path_end];
    // Strip query string
    let path = path.split('?').next().unwrap_or(path);
    let path_len = path.len().min(255);
    ptr::copy_nonoverlapping(path.as_ptr(), (*req).path.as_mut_ptr() as *mut u8, path_len);
    (*req).path[path_len] = 0;

    // Find Content-Length
    let mut content_length: usize = 0;
    let header_lower = header_str.to_ascii_lowercase();
    if let Some(pos) = header_lower.find("content-length:") {
        let after = &header_str[pos + 15..];
        let trimmed = after.trim_start();
        if let Some(end) = trimmed.find(|c: char| !c.is_ascii_digit()) {
            content_length = trimmed[..end].parse().unwrap_or(0);
        } else {
            content_length = trimmed.parse().unwrap_or(0);
        }
    }

    // Read body
    if content_length > 0 {
        if content_length > HTTP_MAX_REQUEST {
            libc::free(req as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Other(format!("HTTP body too large: {} bytes", content_length)));
            return ptr::null_mut();
        }

        let body = libc::malloc(content_length + 1) as *mut u8;
        if body.is_null() {
            libc::free(req as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Other("Failed to allocate HTTP body buffer".into()));
            return ptr::null_mut();
        }

        // Some body bytes may be in header_buf after \r\n\r\n
        let after_headers = header_end + 4;
        let already_read = (header_len - after_headers).min(content_length);
        if already_read > 0 {
            ptr::copy_nonoverlapping(header_buf.as_ptr().add(after_headers), body, already_read);
        }

        let mut total = already_read;
        while total < content_length {
            let n = libc::recv(fd, body.add(total) as *mut c_void, content_length - total, 0);
            if n <= 0 {
                libc::free(body as *mut c_void);
                libc::free(req as *mut c_void);
                set_errmsg(errmsg, &MorlocError::Other("Connection closed while reading HTTP body".into()));
                return ptr::null_mut();
            }
            total += n as usize;
        }
        *body.add(content_length) = 0;
        (*req).body = body as *mut c_char;
        (*req).body_len = content_length;
    }

    req
}

#[no_mangle]
pub unsafe extern "C" fn http_free_request(req: *mut HttpRequest) {
    if req.is_null() { return; }
    if !(*req).body.is_null() {
        libc::free((*req).body as *mut c_void);
    }
    libc::free(req as *mut c_void);
}

// ── http_write_response ──────────────────────────────────────────────────────

fn http_status_text(status: i32) -> &'static str {
    match status {
        200 => "OK",
        400 => "Bad Request",
        404 => "Not Found",
        405 => "Method Not Allowed",
        500 => "Internal Server Error",
        _ => "Unknown",
    }
}

#[no_mangle]
pub unsafe extern "C" fn http_write_response(
    fd: i32,
    status: i32,
    content_type: *const c_char,
    body: *const c_char,
    body_len: usize,
) -> bool {
    let ct = if content_type.is_null() {
        "application/json"
    } else {
        std::ffi::CStr::from_ptr(content_type).to_str().unwrap_or("application/json")
    };

    let header = format!(
        "HTTP/1.1 {} {}\r\n\
         Content-Type: {}\r\n\
         Content-Length: {}\r\n\
         Connection: close\r\n\
         Access-Control-Allow-Origin: *\r\n\
         Access-Control-Allow-Methods: GET, POST, OPTIONS\r\n\
         Access-Control-Allow-Headers: Content-Type\r\n\
         \r\n",
        status, http_status_text(status), ct, body_len
    );

    let n = libc::send(fd, header.as_ptr() as *const c_void, header.len(), crate::utility::SEND_NOSIGNAL);
    if n < 0 { return false; }

    if !body.is_null() && body_len > 0 {
        let mut total: usize = 0;
        while total < body_len {
            let n = libc::send(fd, (body as *const u8).add(total) as *const c_void, body_len - total, crate::utility::SEND_NOSIGNAL);
            if n <= 0 { return false; }
            total += n as usize;
        }
    }

    true
}

// ── http_to_daemon_request ───────────────────────────────────────────────────

/// Extract a JSON string value after a key like "expr": "..."
fn extract_json_string(body: &str, key: &str) -> Option<String> {
    let search = format!("\"{}\"", key);
    let pos = body.find(&search)?;
    let after = &body[pos + search.len()..];
    let after = after.trim_start();
    let after = after.strip_prefix(':')?;
    let after = after.trim_start();
    if !after.starts_with('"') { return None; }
    let after = &after[1..]; // skip opening quote
    let mut result = String::new();
    let mut chars = after.chars();
    loop {
        match chars.next() {
            Some('\\') => {
                if let Some(c) = chars.next() {
                    result.push(c);
                }
            }
            Some('"') => break,
            Some(c) => result.push(c),
            None => break,
        }
    }
    Some(result)
}

#[no_mangle]
pub unsafe extern "C" fn http_to_daemon_request(
    req: *mut HttpRequest,
    errmsg: *mut *mut c_char,
) -> *mut DaemonRequest {
    clear_errmsg(errmsg);

    let dreq = libc::calloc(1, std::mem::size_of::<DaemonRequest>()) as *mut DaemonRequest;
    if dreq.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("Failed to allocate daemon_request_t".into()));
        return ptr::null_mut();
    }

    let path = std::ffi::CStr::from_ptr((*req).path.as_ptr())
        .to_str().unwrap_or("");
    let method = (*req).method;

    let body_str = if !(*req).body.is_null() && (*req).body_len > 0 {
        std::str::from_utf8(std::slice::from_raw_parts((*req).body as *const u8, (*req).body_len))
            .unwrap_or("")
    } else {
        ""
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
        if let Some(expr) = extract_json_string(body_str, "expr") {
            (*dreq).expr = libc::strdup(expr.as_ptr() as *const c_char);
            // strdup from Rust string - need null terminated
            let c = std::ffi::CString::new(expr).unwrap_or_default();
            (*dreq).expr = libc::strdup(c.as_ptr());
        }
        if (*dreq).expr.is_null() {
            libc::free(dreq as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Other("Missing 'expr' field in /eval request body".into()));
            return ptr::null_mut();
        }
        return dreq;
    }

    // POST /typecheck
    if method == HttpMethod::Post && path == "/typecheck" {
        (*dreq).method = DaemonMethod::Typecheck;
        if let Some(expr) = extract_json_string(body_str, "expr") {
            let c = std::ffi::CString::new(expr).unwrap_or_default();
            (*dreq).expr = libc::strdup(c.as_ptr());
        }
        if (*dreq).expr.is_null() {
            libc::free(dreq as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Other("Missing 'expr' field in /typecheck request body".into()));
            return ptr::null_mut();
        }
        return dreq;
    }

    // POST /bind
    if method == HttpMethod::Post && path == "/bind" {
        (*dreq).method = DaemonMethod::Bind;
        if let Some(expr) = extract_json_string(body_str, "expr") {
            let c = std::ffi::CString::new(expr).unwrap_or_default();
            (*dreq).expr = libc::strdup(c.as_ptr());
        }
        if let Some(name) = extract_json_string(body_str, "name") {
            let c = std::ffi::CString::new(name).unwrap_or_default();
            (*dreq).name = libc::strdup(c.as_ptr());
        }
        if (*dreq).expr.is_null() {
            libc::free(dreq as *mut c_void);
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
            libc::free(dreq as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Other("Missing binding name in /bindings/ path".into()));
            return ptr::null_mut();
        }
        (*dreq).method = DaemonMethod::Unbind;
        let c = std::ffi::CString::new(name).unwrap_or_default();
        (*dreq).name = libc::strdup(c.as_ptr());
        return dreq;
    }

    // POST /call/<command>
    if method == HttpMethod::Post && path.starts_with("/call/") {
        let cmd_name = &path[6..];
        if cmd_name.is_empty() {
            libc::free(dreq as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Other("Missing command name in /call/ path".into()));
            return ptr::null_mut();
        }
        (*dreq).method = DaemonMethod::Call;
        let c = std::ffi::CString::new(cmd_name).unwrap_or_default();
        (*dreq).command = libc::strdup(c.as_ptr());

        // Parse body
        let trimmed = body_str.trim();
        if trimmed.starts_with('[') {
            let c = std::ffi::CString::new(trimmed).unwrap_or_default();
            (*dreq).args_json = libc::strdup(c.as_ptr());
        } else if trimmed.starts_with('{') {
            // Extract "args" array
            if let Some(args_pos) = trimmed.find("\"args\"") {
                let after = &trimmed[args_pos + 6..];
                let after = after.trim_start().strip_prefix(':').unwrap_or(after).trim_start();
                if after.starts_with('[') {
                    // Find matching ]
                    let mut depth = 0i32;
                    let mut in_string = false;
                    let mut end = 0;
                    for (i, ch) in after.chars().enumerate() {
                        if in_string {
                            if ch == '\\' { continue; }
                            if ch == '"' { in_string = false; }
                        } else {
                            if ch == '"' { in_string = true; }
                            else if ch == '[' { depth += 1; }
                            else if ch == ']' { depth -= 1; if depth == 0 { end = i + 1; break; } }
                        }
                    }
                    if end > 0 {
                        let arr = &after[..end];
                        let c = std::ffi::CString::new(arr).unwrap_or_default();
                        (*dreq).args_json = libc::strdup(c.as_ptr());
                    }
                }
            }
        }
        return dreq;
    }

    // OPTIONS (CORS preflight)
    if method == HttpMethod::Options {
        (*dreq).method = DaemonMethod::Health;
        return dreq;
    }

    libc::free(dreq as *mut c_void);
    let method_str = match method {
        HttpMethod::Get => "GET",
        HttpMethod::Post => "POST",
        HttpMethod::Delete => "DELETE",
        HttpMethod::Options => "OPTIONS",
    };
    set_errmsg(errmsg, &MorlocError::Other(format!("Unknown HTTP endpoint: {} {}", method_str, path)));
    ptr::null_mut()
}
