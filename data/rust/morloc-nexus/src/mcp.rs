//! Native MCP (Model Context Protocol) server over stdio.
//!
//! A compiled morloc program's exported functions become MCP tools. This is a
//! single-stream, newline-delimited JSON-RPC 2.0 loop that reads requests on
//! the protocol *input* fd (fd 0) and writes responses on a private duplicate
//! of the real stdout (`protocol_fd`). It is deliberately NOT built on
//! libmorloc's `daemon_run` (a listener-socket / worker-pool model whose
//! 4-byte length-prefix framing and `accept()` loop do not fit stdio). Only the
//! *execution backend* is reused: each `tools/call` is marshaled into a
//! `daemon_dispatch` FFI call, exactly as the REST/socket daemon does.
//!
//! ## fd discipline (load-bearing)
//!
//! Pools inherit the nexus's fd 0/1/2 unchanged (`process::start_language_server`
//! does no fd hygiene), and a pure command evaluated in-process via `morloc_eval`
//! can write to the nexus's own fd 1. Either would corrupt the JSON-RPC stream.
//! [`rehome_stdout_for_protocol`] therefore runs BEFORE any pool is spawned or
//! any command is evaluated: it saves the real stdout to a private high fd and
//! aliases fd 1 onto fd 2, so every stray `print` / `std::cout` / `cat` lands on
//! stderr. JSON-RPC is written only via raw `write(protocol_fd, ...)`, never
//! `println!`. Commands whose types cannot be safely served (Arrow `Table`,
//! stream handles, `@stdin`) are excluded from the tool surface upstream in
//! [`crate::json_help::build_tool_shapes`].

use std::collections::HashMap;
use std::ffi::{c_char, c_void, CStr, CString};
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::os::unix::io::RawFd;
use std::ptr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use serde_json::{json, Map, Value};

use morloc_manifest::Manifest;
use morloc_runtime_types::daemon_socket::MorlocSocket;

use crate::json_help::{build_tool_shapes, ArgSlot, McpToolShape};
use crate::process::{self, PoolSocket};

/// The MCP protocol revision this server implements. Pinned so we never have to
/// handle request batching (removed in this revision).
const SERVER_PROTOCOL_VERSION: &str = "2025-06-18";

/// Reject any single JSON-RPC line larger than this. Newline framing carries no
/// length prefix, so this is the backstop the length-prefixed pool protocol's
/// 64 MiB cap would otherwise provide.
const MAX_LINE_BYTES: usize = 64 * 1024 * 1024;

/// Mirror of the daemon dispatch error kinds we care about
/// (`daemon_ffi::DAEMON_ERROR_INTERNAL`). A dispatch that fails with this kind
/// may mean a pool process died, so we probe + recover afterward.
const DAEMON_ERROR_INTERNAL: i32 = 5;

// JSON-RPC 2.0 error codes.
const JSONRPC_PARSE_ERROR: i32 = -32700;
const JSONRPC_INVALID_REQUEST: i32 = -32600;
const JSONRPC_METHOD_NOT_FOUND: i32 = -32601;
const JSONRPC_INVALID_PARAMS: i32 = -32602;

/// Layout-compatible view of `daemon_ffi::DaemonResponse`. We only read it.
/// The trailing `result_bytes`/`result_len`/`mime` fields carry a raw-media
/// (`@mime`) return when we requested one via `daemon_set_output_media_bytes`.
#[repr(C)]
struct CDaemonResponse {
    id: *mut c_char,
    success: bool,
    error_kind: i32,
    result_json: *mut c_char,
    error: *mut c_char,
    result_bytes: *mut u8,
    result_len: usize,
    mime: *mut c_char,
}

extern "C" {
    fn parse_manifest(text: *const c_char, errmsg: *mut *mut c_char) -> *mut c_void;
    /// Request the raw-media response form for this thread's next
    /// `daemon_dispatch` (media-typed returns come back as raw bytes + mime).
    /// Returns the previous value. See `daemon_ffi::daemon_set_output_media_bytes`.
    fn daemon_set_output_media_bytes(on: bool) -> bool;
    fn daemon_parse_request(
        json: *const c_char,
        len: usize,
        errmsg: *mut *mut c_char,
    ) -> *mut c_void;
    fn daemon_dispatch(
        manifest: *mut c_void,
        request: *mut c_void,
        sockets: *mut c_void,
        shm_basename: *const c_char,
    ) -> *mut CDaemonResponse;
    fn daemon_free_request(req: *mut c_void);
    fn daemon_free_response(resp: *mut c_void);
}

/// Re-home stdout so the JSON-RPC stream is inviolable, and return the private
/// duplicate of the real stdout to write protocol messages to. MUST be called
/// before any pool fork or in-process eval. Also silences morloc's own
/// prologue/epilogue log lines (they would otherwise clutter stderr).
pub fn rehome_stdout_for_protocol() -> RawFd {
    // Duplicate the real stdout to a fresh high fd (>= 3). F_DUPFD_CLOEXEC so
    // spawned pools do not inherit the protocol fd.
    let protocol_fd = unsafe { libc::fcntl(libc::STDOUT_FILENO, libc::F_DUPFD_CLOEXEC, 3) };
    if protocol_fd < 0 {
        // Extremely unlikely (fd exhaustion at startup). Fall back to the raw
        // stdout fd; the alias below then can't run, so bail loudly.
        eprintln!("morloc mcp: failed to duplicate stdout for the protocol stream");
        std::process::exit(1);
    }
    // Alias fd 1 onto fd 2 so any stray write to "stdout" (pool print, eval
    // std::cout, a library banner) lands on stderr, invisible to JSON-RPC.
    unsafe {
        libc::dup2(libc::STDERR_FILENO, libc::STDOUT_FILENO);
    }
    std::env::set_var("MORLOC_QUIET", "1");
    protocol_fd
}

/// Run the MCP server loop until the client disconnects (fd 0 EOF) or a fatal
/// error. Never returns; exits the process.
pub fn serve(
    sockets: &mut [PoolSocket],
    shm_basename: &str,
    manifest_payload: &str,
    manifest: &Manifest,
    protocol_fd: RawFd,
) -> ! {
    // Parse the manifest into the C-layout struct daemon_dispatch expects.
    let manifest_c = parse_c_manifest(manifest_payload);

    // Build the C socket array (kept alive for the whole session) and the
    // session-constant dispatch context the request handlers share.
    let (mut c_sockets, _keepalive) = build_c_sockets(sockets);
    let ctx = DispatchCtx {
        manifest_c,
        sockets_ptr: c_sockets.as_mut_ptr() as *mut c_void,
        n_pools: c_sockets.len(),
        shm: CString::new(shm_basename).unwrap(),
    };

    // Precompute every servable tool's shape (forward inputSchema + inverse
    // argument mapping). Excluded commands are dropped with a stderr note.
    let shapes: Vec<McpToolShape> = build_tool_shapes(manifest);
    let tools_list: Vec<Value> = shapes.iter().map(|s| s.tool.clone()).collect();
    let by_name: HashMap<&str, &McpToolShape> =
        shapes.iter().map(|s| (s.name.as_str(), s)).collect();

    let server_name = manifest.name.clone();
    let server_version = manifest.build.morloc_version.clone();

    let mut session = Session::new();
    // `StdinLock` is itself a `BufRead`; the lock is process-local, so pools
    // (separate processes sharing fd 0) can't contend on it.
    let reader = std::io::stdin().lock();

    for line in read_lines_capped(reader) {
        let line = match line {
            Ok(l) => l,
            Err(LineError::Eof) => break, // client disconnected -> end session
            Err(LineError::TooLong) => {
                write_message(
                    protocol_fd,
                    &error_response(
                        Value::Null,
                        JSONRPC_INVALID_REQUEST,
                        "request line exceeds the maximum size",
                    ),
                );
                continue;
            }
            Err(LineError::Io(e)) => {
                eprintln!("morloc mcp: input read error: {}", e);
                break;
            }
        };
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let msg: Value = match serde_json::from_str(trimmed) {
            Ok(v) => v,
            Err(_) => {
                write_message(
                    protocol_fd,
                    &error_response(Value::Null, JSONRPC_PARSE_ERROR, "invalid JSON"),
                );
                continue;
            }
        };

        if let Some(response) = handle_message(
            &msg,
            &mut session,
            &tools_list,
            &by_name,
            &ctx,
            &server_name,
            &server_version,
        ) {
            write_message(protocol_fd, &response);
        }
    }

    process::clean_exit(0);
}

/// Per-connection handshake state.
struct Session {
    initialized: bool,
}

impl Session {
    fn new() -> Self {
        Session { initialized: false }
    }
}

/// Dispatch one parsed JSON-RPC message. Returns `Some(response)` for requests
/// (messages carrying an `id`) and `None` for notifications (which never get a
/// reply).
fn handle_message(
    msg: &Value,
    session: &mut Session,
    tools_list: &[Value],
    by_name: &HashMap<&str, &McpToolShape>,
    ctx: &DispatchCtx,
    server_name: &str,
    server_version: &str,
) -> Option<Value> {
    let method = msg.get("method").and_then(|m| m.as_str());
    // A request carries an `id`; a notification does not. `id` may be a string,
    // number, or null and is echoed verbatim.
    let id = msg.get("id").cloned();
    let is_request = id.is_some();

    let method = match method {
        Some(m) => m,
        None => {
            // A message with no method and an id is a malformed request.
            return id.map(|id| {
                error_response(id, JSONRPC_INVALID_REQUEST, "missing 'method'")
            });
        }
    };

    match method {
        "initialize" => {
            // Request-only method: a message with no `id` is a notification and
            // must neither be answered nor acted on. Ignore it.
            let Some(id) = id else { return None };
            // Version negotiation: echo the client's requested version when we
            // support it, else advertise ours.
            let requested = msg
                .get("params")
                .and_then(|p| p.get("protocolVersion"))
                .and_then(|v| v.as_str());
            let version = match requested {
                Some(v) if v == SERVER_PROTOCOL_VERSION => v.to_string(),
                _ => SERVER_PROTOCOL_VERSION.to_string(),
            };
            Some(result_response(
                id,
                json!({
                    "protocolVersion": version,
                    "capabilities": { "tools": { "listChanged": false } },
                    "serverInfo": { "name": server_name, "version": server_version },
                }),
            ))
        }
        "notifications/initialized" => {
            session.initialized = true;
            None
        }
        "ping" => {
            // Answerable at any point in the lifecycle.
            id.map(|id| result_response(id, json!({})))
        }
        "tools/list" => {
            // Request-only (see `initialize`): ignore an id-less notification.
            let Some(id) = id else { return None };
            if !session.initialized {
                return Some(error_response(
                    id,
                    JSONRPC_INVALID_REQUEST,
                    "received tools/list before initialization completed",
                ));
            }
            Some(result_response(id, json!({ "tools": tools_list })))
        }
        "tools/call" => {
            // Request-only (see `initialize`): ignore an id-less notification so
            // a tool is never executed for a message that gets no reply.
            let Some(id) = id else { return None };
            if !session.initialized {
                return Some(error_response(
                    id,
                    JSONRPC_INVALID_REQUEST,
                    "received tools/call before initialization completed",
                ));
            }
            Some(handle_tools_call(id, msg, by_name, ctx))
        }
        other => {
            if is_request {
                Some(error_response(
                    id.unwrap_or(Value::Null),
                    JSONRPC_METHOD_NOT_FOUND,
                    &format!("unknown method '{}'", other),
                ))
            } else {
                // Unknown notification (e.g. notifications/cancelled): ignore.
                None
            }
        }
    }
}

/// Service one `tools/call`: validate arguments against the tool's inputSchema,
/// invert the named `arguments` into the positional array `daemon_dispatch`
/// wants, dispatch, and shape the result. Argument problems become JSON-RPC
/// errors; execution problems become a normal result with `isError: true`.
fn handle_tools_call(
    id: Value,
    msg: &Value,
    by_name: &HashMap<&str, &McpToolShape>,
    ctx: &DispatchCtx,
) -> Value {
    let params = match msg.get("params") {
        Some(p) => p,
        None => return error_response(id, JSONRPC_INVALID_PARAMS, "missing params"),
    };
    let name = match params.get("name").and_then(|n| n.as_str()) {
        Some(n) => n,
        None => return error_response(id, JSONRPC_INVALID_PARAMS, "missing tool 'name'"),
    };
    let shape = match by_name.get(name) {
        Some(s) => *s,
        None => {
            return error_response(
                id,
                JSONRPC_INVALID_PARAMS,
                &format!("unknown tool '{}'", name),
            )
        }
    };

    // `arguments` is optional; absence means an empty object.
    let empty = Map::new();
    let arguments = match params.get("arguments") {
        None | Some(Value::Null) => &empty,
        Some(Value::Object(m)) => m,
        Some(_) => {
            return error_response(
                id,
                JSONRPC_INVALID_PARAMS,
                "'arguments' must be an object",
            )
        }
    };

    // Validate + invert named arguments into the exact-arity positional array.
    let positional = match resolve_arguments(shape, arguments) {
        Ok(p) => p,
        Err(e) => return error_response(id, JSONRPC_INVALID_PARAMS, &e),
    };

    // Select the output projection. Commands with terminals expose a `_render`
    // enum: "raw" (default) dispatches the command itself; a renderer name
    // dispatches that projection's entry command and packages its result by the
    // entry's media type. `_render` is not a pool argument, so `positional` is
    // identical for the command and any of its projections.
    let render_sel = arguments
        .get("_render")
        .and_then(|v| v.as_str())
        .unwrap_or("raw");
    let (dispatch_command, ret_mime, returns_object) = match shape.dispatch_for(render_sel) {
        Ok(target) => target,
        Err(e) => return error_response(id, JSONRPC_INVALID_PARAMS, &e),
    };

    // Dispatch through the shared execution backend. A media-typed projection is
    // fetched as raw bytes (no JSON int-array), then base64'd into an MCP block.
    let want_media = ret_mime.is_some();
    let request_json = json!({
        "method": "call",
        "command": dispatch_command,
        "args": Value::Array(positional),
    });

    match ctx.dispatch(&request_json, want_media) {
        Ok(DispatchOutcome::Media(content)) => {
            result_response(id, media_result_object(content))
        }
        Ok(DispatchOutcome::Json(result_json)) => {
            result_response(id, tool_result_object(&result_json, returns_object))
        }
        Err(message) => {
            // Execution failure (pool error, @throw, recovering, internal):
            // report as a tool result with isError, not a protocol error, so
            // the model can read and react to the failure.
            result_response(
                id,
                json!({
                    "content": [ { "type": "text", "text": message } ],
                    "isError": true,
                }),
            )
        }
    }
}

/// Validate the client's named `arguments` against a tool's inputSchema and
/// invert them into the exact-arity positional array `daemon_dispatch` expects.
/// Pure (no FFI): rejects unknown/missing arguments and resolves each slot.
fn resolve_arguments(
    shape: &McpToolShape,
    arguments: &Map<String, Value>,
) -> Result<Vec<Value>, String> {
    // Reject unknown argument names (inputSchema is additionalProperties:false).
    for key in arguments.keys() {
        if !shape.prop_names.contains(key) {
            return Err(format!("unknown argument '{}'", key));
        }
    }
    // Reject missing required arguments.
    for req in &shape.required {
        if !arguments.contains_key(req) {
            return Err(format!("missing required argument '{}'", req));
        }
    }
    // One resolved value per declared arg slot, in order.
    let mut positional: Vec<Value> = Vec::with_capacity(shape.slots.len());
    for slot in &shape.slots {
        positional.push(resolve_slot(slot, arguments)?);
    }
    Ok(positional)
}

/// Resolve one argument slot against the client's named `arguments`.
fn resolve_slot(slot: &ArgSlot, args: &Map<String, Value>) -> Result<Value, String> {
    match slot {
        // Required-ness was already enforced in `resolve_arguments`, so an
        // absent key here is genuinely optional and falls back to `missing`.
        ArgSlot::Value { key, missing } => {
            Ok(args.get(key).cloned().unwrap_or_else(|| missing.clone()))
        }
        ArgSlot::Flag { key, default } => match args.get(key) {
            Some(Value::Bool(b)) => Ok(Value::Bool(*b)),
            Some(_) => Err(format!("argument '{}' must be a boolean", key)),
            None => Ok(Value::Bool(*default)),
        },
        ArgSlot::Record { group_key, fields } => {
            // Whole-object (group_opt) form: start from the object the client
            // passed under the group key, if any. A non-object value is
            // forwarded as-is for the pool to reject.
            let mut obj = match group_key.as_ref().and_then(|gk| args.get(gk)) {
                Some(Value::Object(m)) => m.clone(),
                Some(other) => return Ok(other.clone()),
                None => Map::new(),
            };
            // Fill every field still absent -- from its own flat argument key
            // (the unrolled form) or its default. So BOTH record forms supply
            // defaults for omitted fields rather than deferring a partial record
            // to a pool error.
            for f in fields {
                if !obj.contains_key(&f.field) {
                    let v = args
                        .get(&f.field)
                        .cloned()
                        .unwrap_or_else(|| f.default.clone());
                    obj.insert(f.field.clone(), v);
                }
            }
            Ok(Value::Object(obj))
        }
    }
}

/// Session-constant execution backend: the C manifest, socket array, pool count,
/// and SHM basename that every `tools/call` dispatches against. Built once in
/// `serve` and shared by reference so the request handlers don't thread four raw
/// FFI pointers apiece.
struct DispatchCtx {
    manifest_c: *mut c_void,
    sockets_ptr: *mut c_void,
    n_pools: usize,
    shm: CString,
}

/// A successful dispatch's payload: either the JSON-serialized return value, or
/// -- when `want_media` was requested for a media-typed (`@mime`) return -- the
/// finished MCP content block built from the raw bytes (skips both the JSON
/// int-array round-trip and an owning copy of the payload).
enum DispatchOutcome {
    Json(String),
    Media(Value),
}

impl DispatchCtx {
    /// Marshal a call-request JSON through `daemon_parse_request` +
    /// `daemon_dispatch` and read the response: `Ok(outcome)` on success,
    /// `Err(message)` on failure. `want_media` asks the backend for the raw
    /// content bytes of a media-typed return (set only when the selected
    /// projection has a `@mime`); otherwise the JSON form is returned. On an
    /// INTERNAL failure a pool may have died, so probe liveness and run crash
    /// recovery (a fast no-op when all pools are alive) before returning, so a
    /// subsequent call can succeed.
    fn dispatch(&self, request_json: &Value, want_media: bool) -> Result<DispatchOutcome, String> {
        let request_str = request_json.to_string();
        let req_c = CString::new(request_str.as_str()).unwrap();

        let mut errmsg: *mut c_char = ptr::null_mut();
        let req = unsafe {
            daemon_parse_request(req_c.as_ptr(), request_str.len(), &mut errmsg)
        };
        if req.is_null() {
            let msg = process::take_c_errmsg(errmsg).unwrap_or_else(|| "unknown error".into());
            return Err(format!("failed to build call request: {}", msg));
        }

        // Request the raw-media response form for this one dispatch (same thread,
        // so the backend's thread-local sees it), then restore.
        let prev = if want_media {
            unsafe { daemon_set_output_media_bytes(true) }
        } else {
            false
        };
        let resp =
            unsafe { daemon_dispatch(self.manifest_c, req, self.sockets_ptr, self.shm.as_ptr()) };
        if want_media {
            unsafe { daemon_set_output_media_bytes(prev) };
        }
        unsafe { daemon_free_request(req) };

        if resp.is_null() {
            return Err("dispatch returned no response".into());
        }

        let outcome = unsafe {
            if (*resp).success {
                // Prefer the raw-media bytes when the backend produced them;
                // fall back to JSON otherwise (untyped return, or want_media off).
                // Build the content block from the borrowed bytes here, before
                // the response is freed below -- no owning copy of the payload.
                if want_media && !(*resp).result_bytes.is_null() {
                    let bytes =
                        std::slice::from_raw_parts((*resp).result_bytes, (*resp).result_len);
                    let mime = cstr_to_string((*resp).mime).unwrap_or_default();
                    Ok(DispatchOutcome::Media(media_content_block(bytes, &mime)))
                } else {
                    Ok(DispatchOutcome::Json(
                        cstr_to_string((*resp).result_json).unwrap_or_else(|| "null".to_string()),
                    ))
                }
            } else {
                let kind = (*resp).error_kind;
                let msg = cstr_to_string((*resp).error)
                    .unwrap_or_else(|| "dispatch failed".to_string());
                if kind == DAEMON_ERROR_INTERNAL {
                    process::mcp_recover_pools(self.n_pools);
                }
                Err(msg)
            }
        };
        unsafe { daemon_free_response(resp as *mut c_void) };
        outcome
    }
}

/// Build the MCP `result` object for a successful non-media call. `result_json`
/// is the serialized return value; `structured` is true only for object returns.
fn tool_result_object(result_json: &str, structured: bool) -> Value {
    // Parse so we can (a) emit compact single-line text and (b) mirror an object
    // return into structuredContent. Fall back to the raw string.
    let parsed: Option<Value> = serde_json::from_str(result_json).ok();

    let text = parsed
        .as_ref()
        .map(|v| v.to_string())
        .unwrap_or_else(|| result_json.to_string());

    let mut result = Map::new();
    result.insert(
        "content".into(),
        json!([ { "type": "text", "text": text } ]),
    );
    if structured {
        if let Some(Value::Object(_)) = parsed {
            result.insert("structuredContent".into(), parsed.unwrap());
        }
    }
    result.insert("isError".into(), Value::Bool(false));
    Value::Object(result)
}

/// Build the MCP `result` object wrapping a media content block (from
/// `DispatchOutcome::Media`, built by `media_content_block`).
fn media_result_object(content: Value) -> Value {
    let mut result = Map::new();
    result.insert("content".into(), json!([content]));
    result.insert("isError".into(), Value::Bool(false));
    Value::Object(result)
}

/// The MCP content block for `bytes` labeled `mime`. MCP carries binary only as
/// base64: `image/*` and `audio/*` become their content block, `text/*` a text
/// block (UTF-8), and any other binary an embedded resource `blob`.
fn media_content_block(bytes: &[u8], mime: &str) -> Value {
    let family = mime.split('/').next().unwrap_or("");
    match family {
        "image" | "audio" => json!({
            "type": family,
            "data": base64_encode(bytes),
            "mimeType": mime,
        }),
        "text" => json!({
            "type": "text",
            "text": String::from_utf8_lossy(bytes).into_owned(),
        }),
        _ => json!({
            "type": "resource",
            "resource": {
                "uri": "morloc://return",
                "mimeType": mime,
                "blob": base64_encode(bytes),
            },
        }),
    }
}

fn base64_encode(bytes: &[u8]) -> String {
    use base64::Engine;
    base64::engine::general_purpose::STANDARD.encode(bytes)
}

// -- JSON-RPC envelope helpers ------------------------------------------------

fn result_response(id: Value, result: Value) -> Value {
    json!({ "jsonrpc": "2.0", "id": id, "result": result })
}

fn error_response(id: Value, code: i32, message: &str) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": id,
        "error": { "code": code, "message": message },
    })
}

// -- FFI + IO plumbing --------------------------------------------------------

fn parse_c_manifest(payload: &str) -> *mut c_void {
    let payload_c = CString::new(payload).unwrap();
    let mut errmsg: *mut c_char = ptr::null_mut();
    let m = unsafe { parse_manifest(payload_c.as_ptr(), &mut errmsg) };
    if m.is_null() {
        let msg = process::take_c_errmsg(errmsg).unwrap_or_else(|| "unknown error".into());
        eprintln!("morloc mcp: failed to parse manifest: {}", msg);
        process::clean_exit(1);
    }
    m
}

/// Build the C `MorlocSocket` array (the canonical shared FFI type) from the
/// nexus's `PoolSocket`s. Returns the array plus the owned `CString`s that back
/// its pointers (kept alive by the caller for the session's lifetime).
///
/// `socket_filename` is stable across pool-crash recovery (recovery changes only
/// the SHM basename, not socket paths), so `daemon_dispatch` keeps working after
/// a recovery without rebuilding this array.
fn build_c_sockets(sockets: &[PoolSocket]) -> (Vec<MorlocSocket>, Vec<Vec<CString>>) {
    let mut c_sockets: Vec<MorlocSocket> = Vec::with_capacity(sockets.len());
    let mut keepalive: Vec<Vec<CString>> = Vec::new();

    for sock in sockets.iter() {
        let lang_c = CString::new(sock.lang.as_str()).unwrap();
        let socket_c = CString::new(sock.socket_path.as_str()).unwrap();

        let mut cmd_ptrs: Vec<*mut c_char> = Vec::new();
        let mut cmd_strs: Vec<CString> = Vec::new();
        for arg in &sock.syscmd {
            let c = CString::new(arg.to_bytes()).unwrap();
            cmd_ptrs.push(c.as_ptr() as *mut c_char);
            cmd_strs.push(c);
        }
        cmd_ptrs.push(ptr::null_mut());

        c_sockets.push(MorlocSocket {
            lang: lang_c.as_ptr() as *mut c_char,
            syscmd: cmd_ptrs.as_ptr() as *mut *mut c_char,
            socket_filename: socket_c.as_ptr() as *mut c_char,
            pid: sock.pid,
        });

        // The syscmd pointer array must also outlive the loop.
        std::mem::forget(cmd_ptrs);
        keepalive.push(cmd_strs);
        keepalive.push(vec![lang_c, socket_c]);
    }

    (c_sockets, keepalive)
}

fn cstr_to_string(p: *const c_char) -> Option<String> {
    if p.is_null() {
        None
    } else {
        Some(unsafe { CStr::from_ptr(p).to_string_lossy().into_owned() })
    }
}

/// Write one JSON-RPC message to the protocol fd, newline-terminated. Never
/// uses `println!` (fd 1 is aliased to stderr). Errors on the protocol fd end
/// the session.
fn write_message(protocol_fd: RawFd, msg: &Value) {
    let mut bytes = msg.to_string().into_bytes();
    bytes.push(b'\n');
    let mut off = 0usize;
    while off < bytes.len() {
        let n = unsafe {
            libc::write(
                protocol_fd,
                bytes.as_ptr().add(off) as *const c_void,
                bytes.len() - off,
            )
        };
        if n < 0 {
            let e = std::io::Error::last_os_error();
            if e.raw_os_error() == Some(libc::EINTR) {
                continue;
            }
            eprintln!("morloc mcp: protocol write failed: {}", e);
            process::clean_exit(1);
        }
        if n == 0 {
            eprintln!("morloc mcp: protocol stream closed");
            process::clean_exit(0);
        }
        off += n as usize;
    }
}

#[derive(Debug)]
enum LineError {
    Eof,
    TooLong,
    Io(std::io::Error),
}

/// Iterator of newline-delimited lines with a hard per-line size cap. When a
/// line exceeds the cap it is discarded up to and including its terminating
/// newline before `TooLong` is yielded, so the next call resumes at a clean
/// line boundary rather than re-parsing the over-long line's tail.
fn read_lines_capped<R: BufRead>(mut reader: R) -> impl Iterator<Item = Result<String, LineError>> {
    std::iter::from_fn(move || {
        let mut buf: Vec<u8> = Vec::new();
        // Set once the line passes the cap: the rest of it (through its newline)
        // is drained and dropped instead of accumulated.
        let mut overflow = false;
        loop {
            let available = match reader.fill_buf() {
                Ok(b) => b,
                Err(ref e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                Err(e) => return Some(Err(LineError::Io(e))),
            };
            if available.is_empty() {
                // EOF. Report an unterminated over-long line, else emit a
                // trailing partial line if any, else stop.
                if overflow {
                    return Some(Err(LineError::TooLong));
                }
                if buf.is_empty() {
                    return Some(Err(LineError::Eof));
                }
                let s = String::from_utf8_lossy(&buf).into_owned();
                return Some(Ok(s));
            }
            if let Some(pos) = available.iter().position(|&b| b == b'\n') {
                if overflow {
                    // End of the over-long line reached; drop it and report.
                    reader.consume(pos + 1);
                    return Some(Err(LineError::TooLong));
                }
                buf.extend_from_slice(&available[..pos]);
                reader.consume(pos + 1);
                if buf.len() > MAX_LINE_BYTES {
                    return Some(Err(LineError::TooLong));
                }
                let s = String::from_utf8_lossy(&buf).into_owned();
                return Some(Ok(s));
            } else {
                let consumed = available.len();
                if !overflow {
                    buf.extend_from_slice(available);
                    if buf.len() > MAX_LINE_BYTES {
                        // Stop accumulating; drain the remainder of this line.
                        overflow = true;
                        buf = Vec::new();
                    }
                }
                reader.consume(consumed);
            }
        }
    })
}

// ===========================================================================
// Streamable-HTTP transport (`mcp --http-port N`)
// ===========================================================================
//
// A second loop around the exact same message core (`handle_message`), tool
// shapes, argument inversion, `_render`/`@mime` media blocks, and
// `daemon_dispatch` backend the stdio server uses -- only the wire framing
// differs. Implements the MCP "Streamable HTTP" transport in its simplest
// spec-compliant form:
//
//   POST /mcp   -> JSON-RPC request in the body; single `application/json`
//                  response (200), or 202 for a notification (no reply).
//   GET  /mcp   -> 405 (this server offers no server-initiated SSE stream,
//                  which the spec explicitly permits).
//   DELETE /mcp -> terminate the session.
//
// Concurrency: connections and request reads run per-thread; a single global
// mutex serializes execution because the language pools are shared,
// single-socket, mutable processes -- two dispatches must never interleave on
// a pool socket. Clients are isolated by `Mcp-Session-Id`. Parallel execution
// (a pool-replication / worker model) is deliberately out of scope here.
//
// Because the endpoint is network-reachable, request reads are bounded (header
// and body size caps + a per-read socket timeout) and the session table is
// bounded (idle TTL + a hard cap), so a slow or abusive client cannot exhaust
// memory or threads.

/// Per-read socket timeout: a connection that sends nothing for this long is
/// dropped, defusing idle/slowloris connections and freeing the thread. Also
/// bounds keep-alive idle time between requests.
const READ_TIMEOUT: Duration = Duration::from_secs(30);

/// Cap on a single request-line / header line. Longer -> the request is
/// rejected and the connection closed.
const MAX_HEADER_LINE_BYTES: u64 = 16 * 1024;

/// Cap on the number of header lines in one request.
const MAX_HEADERS: usize = 128;

/// Idle time after which a session is evicted (bounds the session table when
/// clients disconnect without sending `DELETE /mcp`).
const SESSION_TTL: Duration = Duration::from_secs(3600);

/// Hard cap on live sessions (backstop against initialize floods).
const MAX_SESSIONS: usize = 10_000;

/// One session's state: the handshake flag plus a last-seen stamp for TTL
/// eviction.
struct SessionInfo {
    initialized: bool,
    last_seen: Instant,
}

/// Shared server state guarded by one mutex. The raw C pointers address the
/// leaked (process-lifetime) manifest and socket array; they are only ever
/// dereferenced while the mutex is held, so no dispatch interleaves with
/// another (see `unsafe impl Send`).
struct HttpState {
    manifest_c: *mut c_void,
    sockets_ptr: *mut c_void,
    n_pools: usize,
    shm: CString,
    shapes: Vec<McpToolShape>,
    tools_list: Vec<Value>,
    server_name: String,
    server_version: String,
    /// Session id -> handshake + last-seen state. Bounded by TTL + MAX_SESSIONS.
    sessions: HashMap<String, SessionInfo>,
}

// Safe because every access to the raw pointers happens under the mutex, and
// the pointed-to allocations live for the whole process (leaked in serve_http).
unsafe impl Send for HttpState {}

/// Serve the program's MCP tool surface over HTTP. Never returns.
pub fn serve_http(
    sockets: &mut [PoolSocket],
    shm_basename: &str,
    manifest_payload: &str,
    manifest: &Manifest,
    bind_host: &str,
    auth_token: Option<String>,
    allow_no_auth: bool,
    port: u16,
) -> ! {
    let manifest_c = parse_c_manifest(manifest_payload);

    // The C socket array and its backing CStrings must have stable,
    // process-lifetime addresses: `daemon_dispatch` reads them on every call,
    // from any connection thread. serve_http never returns, so leak both.
    let (c_sockets, keepalive) = build_c_sockets(sockets);
    let mut boxed = c_sockets.into_boxed_slice();
    let n_pools = boxed.len();
    let sockets_ptr = boxed.as_mut_ptr() as *mut c_void;
    std::mem::forget(boxed);
    std::mem::forget(keepalive);

    let shapes: Vec<McpToolShape> = build_tool_shapes(manifest);
    let tools_list: Vec<Value> = shapes.iter().map(|s| s.tool.clone()).collect();

    let state = Arc::new(Mutex::new(HttpState {
        manifest_c,
        sockets_ptr,
        n_pools,
        shm: CString::new(shm_basename).unwrap(),
        shapes,
        tools_list,
        server_name: manifest.name.clone(),
        server_version: manifest.build.morloc_version.clone(),
        sessions: HashMap::new(),
    }));

    let addr = format!("{}:{}", bind_host, port);
    let listener = match TcpListener::bind(&addr) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("morloc mcp: failed to bind {}: {}", addr, e);
            process::clean_exit(1);
        }
    };
    let bound = listener
        .local_addr()
        .map(|a| a.to_string())
        .unwrap_or_else(|_| addr.clone());
    // Fail closed: a non-loopback bind with no token would be an open,
    // unauthenticated endpoint on the network. Refuse unless explicitly
    // overridden (the manager passes --allow-no-auth for a loopback-only
    // publish, where the container nexus binds 0.0.0.0 but only loopback
    // reaches it).
    let is_loopback = matches!(bind_host, "127.0.0.1" | "localhost" | "::1");
    if auth_token.is_none() && !is_loopback && !allow_no_auth {
        eprintln!(
            "morloc mcp: refusing to serve on {} with no auth token (bind is not \
             loopback). Set --auth-token / MORLOC_MCP_TOKEN, or pass --allow-no-auth \
             to override.",
            bound
        );
        process::clean_exit(1);
    }
    eprintln!(
        "morloc mcp: serving \"{}\" over HTTP at http://{}/mcp",
        manifest.name, bound
    );

    let token = Arc::new(auth_token);
    for incoming in listener.incoming() {
        match incoming {
            Ok(stream) => {
                let st = Arc::clone(&state);
                let tk = Arc::clone(&token);
                std::thread::spawn(move || {
                    serve_conn(stream, |req, ka| build_response(req, &st, tk.as_ref().as_deref(), ka))
                });
            }
            Err(e) => eprintln!("morloc mcp: accept error: {}", e),
        }
    }
    process::clean_exit(0);
}

/// One HTTP connection: read requests (keep-alive) and reply. Socket reads run
/// off the mutex; only `build_response`'s dispatch section locks it, so a slow
/// client never blocks others from executing.
/// One keep-alive HTTP connection: read requests until EOF/timeout/`close` and
/// reply, delegating each request to `respond`. Shared by the single-program
/// MCP server and the multi-module front-end, which differ only in how they
/// build the response.
fn serve_conn<F>(stream: TcpStream, mut respond: F)
where
    F: FnMut(&HttpRequest, bool) -> Vec<u8>,
{
    // Drop a connection that stalls mid-request or sits idle past the timeout,
    // so a slow/silent client cannot pin this thread indefinitely.
    let _ = stream.set_read_timeout(Some(READ_TIMEOUT));
    let mut writer = match stream.try_clone() {
        Ok(w) => w,
        Err(_) => return,
    };
    let mut reader = BufReader::new(stream);
    loop {
        let req = match read_http_request(&mut reader) {
            Ok(Some(r)) => r,
            _ => break, // EOF, malformed, or IO error -> close
        };
        let keep_alive = header_get(&req.headers, "connection")
            .map(|v| !v.eq_ignore_ascii_case("close"))
            .unwrap_or(true);
        let resp = respond(&req, keep_alive);
        if writer.write_all(&resp).is_err() {
            break;
        }
        let _ = writer.flush();
        if !keep_alive {
            break;
        }
    }
}

/// A parsed HTTP request. Header names are lowercased for case-insensitive
/// lookup; the body is the exact Content-Length bytes.
struct HttpRequest {
    method: String,
    path: String,
    headers: Vec<(String, String)>,
    body: Vec<u8>,
}

/// Read one request from the buffered connection. `Ok(None)` on a clean EOF, an
/// over-long line/body, or too many headers (the connection is then closed by
/// the caller). Every line is size-capped and the body is read incrementally up
/// to Content-Length, so a hostile Content-Length or header flood cannot force
/// a large up-front allocation.
fn read_http_request<R: BufRead>(reader: &mut R) -> std::io::Result<Option<HttpRequest>> {
    let mut line = String::new();
    match read_capped_line(reader, &mut line)? {
        LineRead::Eof | LineRead::TooLong => return Ok(None),
        LineRead::Ok => {}
    }
    let mut it = line.trim_end().split_whitespace();
    let method = it.next().unwrap_or("").to_string();
    let path = it.next().unwrap_or("").to_string();
    if method.is_empty() {
        return Ok(None);
    }

    let mut headers: Vec<(String, String)> = Vec::new();
    let mut content_length: usize = 0;
    loop {
        if headers.len() >= MAX_HEADERS {
            return Ok(None); // header flood
        }
        let mut h = String::new();
        match read_capped_line(reader, &mut h)? {
            LineRead::Eof | LineRead::TooLong => return Ok(None),
            LineRead::Ok => {}
        }
        let t = h.trim_end_matches(['\r', '\n']);
        if t.is_empty() {
            break; // end of headers
        }
        if let Some(i) = t.find(':') {
            let name = t[..i].trim().to_ascii_lowercase();
            let val = t[i + 1..].trim().to_string();
            if name == "content-length" {
                content_length = val.parse().unwrap_or(0);
            }
            headers.push((name, val));
        }
    }

    // Reject an absurd Content-Length rather than allocate it (same backstop the
    // stdio line cap provides). Read the body incrementally so the buffer grows
    // with the bytes actually received, not the client-declared length.
    if content_length > MAX_LINE_BYTES {
        return Ok(None);
    }
    let mut body = Vec::new();
    if content_length > 0 {
        reader.take(content_length as u64).read_to_end(&mut body)?;
    }
    Ok(Some(HttpRequest {
        method,
        path,
        headers,
        body,
    }))
}

/// Outcome of a size-capped line read.
enum LineRead {
    Ok,
    Eof,
    TooLong,
}

/// Read one line into `buf`, capping the bytes consumed at MAX_HEADER_LINE_BYTES.
/// A line that reaches the cap without a newline yields `TooLong` (the caller
/// closes the connection); this bounds both request-line and header allocation.
fn read_capped_line<R: BufRead>(reader: &mut R, buf: &mut String) -> std::io::Result<LineRead> {
    let n = reader.take(MAX_HEADER_LINE_BYTES).read_line(buf)?;
    if n == 0 {
        return Ok(LineRead::Eof);
    }
    // Hit the cap without terminating the line -> reject.
    if !buf.ends_with('\n') && n as u64 >= MAX_HEADER_LINE_BYTES {
        return Ok(LineRead::TooLong);
    }
    Ok(LineRead::Ok)
}

/// Auth-check, route on method+path, and produce the full HTTP response bytes.
fn build_response(
    req: &HttpRequest,
    state: &Arc<Mutex<HttpState>>,
    token: Option<&str>,
    keep_alive: bool,
) -> Vec<u8> {
    if let Some(tok) = token {
        if !authorized(&req.headers, tok) {
            let extra = vec![("WWW-Authenticate".to_string(), "Bearer".to_string())];
            return http_response(401, br#"{"error":"unauthorized"}"#, keep_alive, &extra);
        }
    }

    // Route on the path only (ignore any query string).
    let path = req.path.split('?').next().unwrap_or("");
    if path != "/mcp" {
        return http_json(404, br#"{"error":"not found"}"#, keep_alive);
    }

    match req.method.as_str() {
        "POST" => handle_http_post(req, state, keep_alive),
        "DELETE" => handle_http_delete(req, state, keep_alive),
        _ => method_not_allowed("POST, DELETE", keep_alive),
    }
}

/// A `POST /mcp`: one JSON-RPC message in, one response out (or 202 for a
/// notification). Session created on `initialize`, required on everything else.
fn handle_http_post(
    req: &HttpRequest,
    state: &Arc<Mutex<HttpState>>,
    keep_alive: bool,
) -> Vec<u8> {
    let msg: Value = match serde_json::from_slice(&req.body) {
        Ok(v) => v,
        Err(_) => {
            let body = error_response(Value::Null, JSONRPC_PARSE_ERROR, "invalid JSON")
                .to_string()
                .into_bytes();
            return http_json(200, &body, keep_alive);
        }
    };
    let method = msg.get("method").and_then(|m| m.as_str()).unwrap_or("");
    let sid_hdr = header_get(&req.headers, "mcp-session-id").map(|s| s.to_string());

    // Recover a poisoned lock so one panicking request cannot wedge the server.
    let mut guard = state.lock().unwrap_or_else(|p| p.into_inner());

    // Resolve (or, on initialize, create) the session.
    let now = Instant::now();
    let (session_id, prior_init) = if method == "initialize" {
        // Evict idle/overflow sessions before minting a new one so the table
        // stays bounded even when clients never send DELETE.
        prune_sessions(&mut guard.sessions, now);
        let id = new_session_id();
        guard.sessions.insert(
            id.clone(),
            SessionInfo { initialized: false, last_seen: now },
        );
        (id, false)
    } else {
        match sid_hdr {
            None => {
                return http_json(
                    400,
                    br#"{"error":"missing Mcp-Session-Id header"}"#,
                    keep_alive,
                );
            }
            Some(id) => match guard.sessions.get(&id) {
                Some(s) => (id, s.initialized),
                None => {
                    return http_json(
                        404,
                        br#"{"error":"unknown or expired session; re-initialize"}"#,
                        keep_alive,
                    );
                }
            },
        }
    };

    // Run the shared message core under the lock (serializes dispatch). The
    // per-request name->shape map borrows the shared shapes (no name copies) and
    // keeps the shapes owned by the state.
    let (resp_opt, new_init) = {
        let ctx = DispatchCtx {
            manifest_c: guard.manifest_c,
            sockets_ptr: guard.sockets_ptr,
            n_pools: guard.n_pools,
            shm: guard.shm.clone(),
        };
        let by_name: HashMap<&str, &McpToolShape> =
            guard.shapes.iter().map(|s| (s.name.as_str(), s)).collect();
        let mut sess = Session {
            initialized: prior_init,
        };
        let r = handle_message(
            &msg,
            &mut sess,
            &guard.tools_list,
            &by_name,
            &ctx,
            &guard.server_name,
            &guard.server_version,
        );
        (r, sess.initialized)
    };
    // Touch the session (refresh last_seen and persist the handshake flag).
    guard.sessions.insert(
        session_id.clone(),
        SessionInfo { initialized: new_init, last_seen: now },
    );
    drop(guard);

    // A newly created session (only `initialize` mints one) is announced via the
    // Mcp-Session-Id header so the client echoes it on subsequent requests.
    let extra: Vec<(String, String)> = if method == "initialize" {
        vec![("Mcp-Session-Id".to_string(), session_id)]
    } else {
        Vec::new()
    };
    match resp_opt {
        Some(response) => http_response(200, &response.to_string().into_bytes(), keep_alive, &extra),
        // Notification (e.g. notifications/initialized): accepted, no JSON-RPC reply.
        None => http_response(202, b"", keep_alive, &extra),
    }
}

/// A `DELETE /mcp`: end the named session.
fn handle_http_delete(
    req: &HttpRequest,
    state: &Arc<Mutex<HttpState>>,
    keep_alive: bool,
) -> Vec<u8> {
    match header_get(&req.headers, "mcp-session-id") {
        Some(id) => {
            let mut g = state.lock().unwrap_or_else(|p| p.into_inner());
            g.sessions.remove(id);
            http_json(200, br#"{"ok":true}"#, keep_alive)
        }
        None => http_json(400, br#"{"error":"missing Mcp-Session-Id header"}"#, keep_alive),
    }
}

/// Case-insensitive header lookup (names were lowercased on parse).
fn header_get<'a>(headers: &'a [(String, String)], name: &str) -> Option<&'a str> {
    headers
        .iter()
        .find(|(k, _)| k == name)
        .map(|(_, v)| v.as_str())
}

/// True when the request carries `Authorization: Bearer <token>` matching.
/// The token comparison is constant-time so a network attacker cannot recover
/// the token byte-by-byte from response timing.
fn authorized(headers: &[(String, String)], token: &str) -> bool {
    match header_get(headers, "authorization") {
        Some(v) => match v.trim().strip_prefix("Bearer ") {
            Some(t) => ct_eq(t.trim().as_bytes(), token.as_bytes()),
            None => false,
        },
        None => false,
    }
}

/// Constant-time byte-slice equality. Length is compared first (the token's
/// length is not the secret); equal-length inputs are compared without an
/// early exit, so timing does not depend on the position of the first
/// mismatch.
fn ct_eq(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    let mut diff: u8 = 0;
    for (x, y) in a.iter().zip(b.iter()) {
        diff |= x ^ y;
    }
    diff == 0
}

/// A fresh, unguessable session id: 16 random bytes hex-encoded (from
/// /dev/urandom), falling back to pid + counter + wall-clock nanos if the
/// random source is unavailable.
fn new_session_id() -> String {
    use std::fmt::Write as _;
    let mut buf = [0u8; 16];
    if let Ok(mut f) = std::fs::File::open("/dev/urandom") {
        if f.read_exact(&mut buf).is_ok() {
            let mut s = String::with_capacity(32);
            for b in &buf {
                let _ = write!(s, "{:02x}", b);
            }
            return s;
        }
    }
    static CTR: AtomicU64 = AtomicU64::new(0);
    let n = CTR.fetch_add(1, Ordering::Relaxed);
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    format!("{}-{}-{}", std::process::id(), n, nanos)
}

/// Bound the session table: drop sessions idle past `SESSION_TTL`, then evict
/// the oldest until there is room for one more below `MAX_SESSIONS`. Called on
/// `initialize` (before minting a new id), so a client that never sends
/// `DELETE /mcp` cannot grow the table without limit.
fn prune_sessions(sessions: &mut HashMap<String, SessionInfo>, now: Instant) {
    sessions.retain(|_, s| now.duration_since(s.last_seen) < SESSION_TTL);
    while sessions.len() >= MAX_SESSIONS {
        let oldest = sessions
            .iter()
            .min_by_key(|(_, s)| s.last_seen)
            .map(|(k, _)| k.clone());
        match oldest {
            Some(k) => {
                sessions.remove(&k);
            }
            None => break,
        }
    }
}

/// Assemble a complete HTTP/1.1 response. The body is always JSON (the only
/// content type this transport emits).
/// A permissive CORS origin header, emitted on every response so browser-based
/// clients can read the API/MCP endpoints cross-origin. The bearer token (not
/// the origin) is the access gate.
const CORS_ALLOW_ORIGIN: &str = "Access-Control-Allow-Origin: *\r\n";

/// Assemble the HTTP response head (status line + standard headers + CORS +
/// any extras + the blank separator line). Shared by the JSON and binary
/// response builders so the header format lives in one place.
fn http_head(
    status: u16,
    content_type: &str,
    content_length: usize,
    keep_alive: bool,
    extra_headers: &[(String, String)],
) -> String {
    let reason = match status {
        200 => "OK",
        202 => "Accepted",
        204 => "No Content",
        400 => "Bad Request",
        401 => "Unauthorized",
        404 => "Not Found",
        405 => "Method Not Allowed",
        _ => "OK",
    };
    let mut head = format!(
        "HTTP/1.1 {} {}\r\nContent-Type: {}\r\nContent-Length: {}\r\nConnection: {}\r\n{}",
        status,
        reason,
        content_type,
        content_length,
        if keep_alive { "keep-alive" } else { "close" },
        CORS_ALLOW_ORIGIN,
    );
    for (k, v) in extra_headers {
        head.push_str(k);
        head.push_str(": ");
        head.push_str(v);
        head.push_str("\r\n");
    }
    head.push_str("\r\n");
    head
}

fn http_response(
    status: u16,
    body: &[u8],
    keep_alive: bool,
    extra_headers: &[(String, String)],
) -> Vec<u8> {
    // A trailing newline is appended to non-empty bodies. HTTP frames by
    // Content-Length, so it is optional for clients (serde ignores trailing
    // whitespace), but it keeps interactive `curl` output off the next shell
    // prompt and mirrors the newline-terminated stdio transport.
    let trailing: usize = if body.is_empty() { 0 } else { 1 };
    let head = http_head(status, "application/json", body.len() + trailing, keep_alive, extra_headers);
    let mut out = head.into_bytes();
    out.extend_from_slice(body);
    if trailing == 1 {
        out.push(b'\n');
    }
    out
}

/// A JSON response with no extra headers -- the common case.
fn http_json(status: u16, body: &[u8], keep_alive: bool) -> Vec<u8> {
    http_response(status, body, keep_alive, &[])
}

/// A 405 with the `Allow` header naming the accepted method(s).
fn method_not_allowed(allow: &str, keep_alive: bool) -> Vec<u8> {
    let extra = vec![("Allow".to_string(), allow.to_string())];
    http_response(405, br#"{"error":"method not allowed"}"#, keep_alive, &extra)
}

// ===========================================================================
// Unified serving front-end (multi-module; forwards to per-module children)
// ===========================================================================
//
// The front-end runs NO morloc computation: it authenticates, tracks MCP
// sessions, resolves a call to (module, command), FORWARDS it to that module's
// child daemon via `router_forward`, and renders the reply. Because it never
// runs a pool/eval itself, a crashing call kills only that module's child (the
// supervisor restarts it), never the front-end. Two adapters share one listener
// and one set of children: `POST /mcp` (MCP) and `/call/<module>/<command>`
// (JSON API).

extern "C" {
    // Forward a parsed daemon request to `program`'s child daemon and return its
    // response; restarts a dead child transparently. See router_ffi::router_forward.
    fn router_forward(
        router: *mut c_void,
        program: *const c_char,
        request: *mut c_void,
        errmsg: *mut *mut c_char,
    ) -> *mut CDaemonResponse;
}

/// Raw supervisor handle shared across connection threads. Mutations to a given
/// program's child are serialized by `Frontend.locks[program]`; distinct
/// programs are distinct memory; the program array is fixed after init.
struct RouterHandle(*mut c_void);
unsafe impl Send for RouterHandle {}
unsafe impl Sync for RouterHandle {}

/// Shared front-end state. Runs no morloc compute; every call is forwarded.
struct Frontend {
    router: RouterHandle,
    /// Per-module forward lock: `router_forward` mutates the child's
    /// RouterProgram on crash-restart, so same-module forwards serialize while
    /// different modules run in parallel. Keys fixed at construction.
    locks: HashMap<String, Mutex<()>>,
    /// MCP tools/list (union across MCP modules; names `<module>__<command>`).
    mcp_tools: Vec<Value>,
    /// Namespaced tool name -> (module, shape). The shape keeps its bare command
    /// name for dispatch/argument resolution.
    mcp_by_name: HashMap<String, (String, McpToolShape)>,
    /// Modules answering the `/call` JSON API.
    api_modules: std::collections::HashSet<String>,
    /// Per-API-module machine-readable help (`GET /discover/<module>`), keyed by
    /// module. Describes positional-argument commands for the `/call` contract.
    api_help: HashMap<String, Value>,
    /// Whether the sandboxed eval capability is exposed (an `eval` MCP tool +
    /// `/eval` route). Off unless the operator exposed it.
    eval_enabled: bool,
    /// eval's sandbox allow-list (`None` => no imports). Passed to the forked
    /// `morloc eval`; independent of the served module sets.
    eval_allow: Option<String>,
    /// CPU-time budget (seconds) for a forked `morloc eval`; <=0 disables it.
    /// Mirrors the daemon's RLIMIT_CPU so a runaway expression can't burn a
    /// front-end thread's CPU indefinitely.
    eval_timeout: i32,
    server_name: String,
    server_version: String,
    sessions: Mutex<HashMap<String, SessionInfo>>,
}
unsafe impl Send for Frontend {}
unsafe impl Sync for Frontend {}

/// A successful forward's payload (owned so the C response is freed at once).
enum ForwardOk {
    Json(String),
    /// A media-typed (`@mime`) return: raw content bytes + media type.
    Media(Vec<u8>, String),
}

impl Frontend {
    /// Forward `request_json` to `module`'s child daemon. Holds the per-module
    /// lock across `router_forward` (whose crash-detect mutates the child's
    /// RouterProgram).
    fn forward(&self, module: &str, request_json: &Value) -> Result<ForwardOk, String> {
        // Fail closed: every served module must have a per-module lock, since
        // router_forward mutates the child's RouterProgram (waitpid + restart).
        // Forwarding without the lock would be a silent unserialized race, so a
        // missing entry is a bug -- error rather than skip the guard.
        let lock = self.locks.get(module).ok_or_else(|| {
            format!("internal error: no forward lock for module '{}'", module)
        })?;
        let request_str = request_json.to_string();
        let req_c = CString::new(request_str.as_str())
            .map_err(|_| "request contains a NUL byte".to_string())?;
        let mut errmsg: *mut c_char = ptr::null_mut();
        let req =
            unsafe { daemon_parse_request(req_c.as_ptr(), request_str.len(), &mut errmsg) };
        if req.is_null() {
            let msg = process::take_c_errmsg(errmsg).unwrap_or_else(|| "bad request".into());
            return Err(format!("failed to build request: {}", msg));
        }
        let module_c = CString::new(module).unwrap_or_default();
        // Keep the guard alive across the forward (named binding, not `_`).
        let _guard = lock.lock().unwrap_or_else(|p| p.into_inner());
        let mut ferr: *mut c_char = ptr::null_mut();
        let resp = unsafe { router_forward(self.router.0, module_c.as_ptr(), req, &mut ferr) };
        unsafe { daemon_free_request(req) };
        if resp.is_null() {
            let msg = process::take_c_errmsg(ferr).unwrap_or_else(|| "forward failed".into());
            return Err(msg);
        }
        let out = unsafe {
            if (*resp).success {
                // A media (`@mime`) return arrives as raw bytes + mime (conveyed
                // as base64 over the socket wire); otherwise a JSON value.
                if !(*resp).mime.is_null() && !(*resp).result_bytes.is_null() {
                    let bytes =
                        std::slice::from_raw_parts((*resp).result_bytes, (*resp).result_len).to_vec();
                    let mime = cstr_to_string((*resp).mime).unwrap_or_default();
                    Ok(ForwardOk::Media(bytes, mime))
                } else {
                    Ok(ForwardOk::Json(
                        cstr_to_string((*resp).result_json).unwrap_or_else(|| "null".to_string()),
                    ))
                }
            } else {
                Err(cstr_to_string((*resp).error)
                    .unwrap_or_else(|| "dispatch failed".to_string()))
            }
        };
        unsafe { daemon_free_response(resp as *mut c_void) };
        out
    }
}

/// Serve a set of modules over one HTTP listener with MCP + API adapters,
/// forwarding to the supervisor's per-module children. Never returns.
pub fn serve_frontend(router: *mut c_void, fdb: &str, config: &crate::dispatch::NexusConfig) -> ! {
    // Load and parse each served program's manifest once (union of adapters);
    // both the MCP tool surface and the API discovery view derive from it.
    let mut manifests: HashMap<String, Manifest> = HashMap::new();
    for m in &config.programs {
        let manifest_path = format!("{}/{}/{}-build/manifest.json", fdb, m, m);
        let payload = match crate::manifest::read_manifest_payload(&manifest_path) {
            Ok(p) => p,
            Err(e) => {
                eprintln!("morloc serve: skipping module '{}': {}", m, e);
                continue;
            }
        };
        match crate::manifest::parse_manifest(&payload) {
            Ok(mf) => {
                manifests.insert(m.clone(), mf);
            }
            Err(e) => eprintln!("morloc serve: skipping module '{}': {}", m, e),
        }
    }

    // Build MCP tool shapes from each MCP module's manifest, namespaced.
    let mut mcp_tools: Vec<Value> = Vec::new();
    let mut mcp_by_name: HashMap<String, (String, McpToolShape)> = HashMap::new();
    let mut server_version = String::new();
    for m in &config.mcp_programs {
        let manifest = match manifests.get(m) {
            Some(mf) => mf,
            None => continue,
        };
        if server_version.is_empty() {
            server_version = manifest.build.morloc_version.clone();
        }
        for s in build_tool_shapes(manifest) {
            let nsname = format!("{}__{}", m, s.name);
            // `__` is the namespace separator, so distinct (module, command)
            // pairs can only collide when a module or command name itself
            // contains `__`. Fail closed: a silent overwrite would make one
            // tool unreachable or route a call to the wrong module.
            if let Some((prev_m, prev_s)) = mcp_by_name.get(&nsname) {
                eprintln!(
                    "morloc serve: MCP tool name collision on '{}': {}::{} vs {}::{}. \
                     Rename a module or command to disambiguate.",
                    nsname, prev_m, prev_s.name, m, s.name
                );
                process::clean_exit(1);
            }
            let mut tool = s.tool.clone();
            tool["name"] = Value::String(nsname.clone());
            mcp_tools.push(tool);
            mcp_by_name.insert(nsname, (m.clone(), s));
        }
    }

    // The eval capability (sandboxed) is a single synthetic MCP tool, exposed
    // only when enabled. It is NOT a module; it forks `morloc eval` per call, so
    // the `morloc` compiler must be on PATH in the serving environment -- warn
    // loudly at startup if it is not, since eval would otherwise fail per call.
    if config.eval_enabled {
        if !morloc_on_path() {
            eprintln!(
                "morloc serve: WARNING: eval is exposed but the `morloc` compiler is \
                 not on PATH; eval calls will fail. Ensure the serving image includes it."
            );
        }
        let allow_desc = config
            .eval_allowed_modules
            .clone()
            .unwrap_or_else(|| "(none)".to_string());
        mcp_tools.push(json!({
            "name": "eval",
            "description": format!(
                "Evaluate an arbitrary morloc expression (sandboxed; IO intrinsics \
                 forbidden; may import: {}).",
                allow_desc
            ),
            "inputSchema": {
                "type": "object",
                "additionalProperties": false,
                "properties": {
                    "expression": {
                        "type": "string",
                        "description": "A morloc expression, optionally with leading `import` lines."
                    }
                },
                "required": ["expression"]
            }
        }));
    }

    let mut locks: HashMap<String, Mutex<()>> = HashMap::new();
    for m in &config.programs {
        locks.insert(m.clone(), Mutex::new(()));
    }
    let api_modules: std::collections::HashSet<String> =
        config.api_programs.iter().cloned().collect();

    // API discovery: per-module machine-readable help (the lossless `--json-help`
    // view). It describes each command's POSITIONAL arguments, matching the
    // `/call/<module>/<command>` positional-array contract. MCP clients discover
    // via the protocol-native `tools/list` (named-argument inputSchema) instead.
    let mut api_help: HashMap<String, Value> = HashMap::new();
    for m in &config.api_programs {
        if let Some(manifest) = manifests.get(m) {
            api_help.insert(m.clone(), crate::json_help::build_json_help(manifest));
        }
    }
    // The parsed manifests have been distilled into `mcp_tools`/`api_help`;
    // `serve_frontend` never returns, so drop them rather than pin every
    // module's full manifest in memory for the process lifetime.
    drop(manifests);

    let bind_host = config
        .mcp_http_host
        .clone()
        .unwrap_or_else(|| "127.0.0.1".to_string());
    let port = config.http_port.unwrap_or(-1);
    if port < 0 {
        eprintln!("morloc serve: --http-port is required for the serving front-end");
        process::clean_exit(1);
    }
    let addr = format!("{}:{}", bind_host, port);
    let listener = match TcpListener::bind(&addr) {
        Ok(l) => l,
        Err(e) => {
            eprintln!("morloc serve: failed to bind {}: {}", addr, e);
            process::clean_exit(1);
        }
    };
    let bound = listener
        .local_addr()
        .map(|a| a.to_string())
        .unwrap_or_else(|_| addr.clone());
    // Fail closed: a non-loopback bind with no token would be an open,
    // unauthenticated endpoint (see the manager's loopback/expose logic).
    let is_loopback = matches!(bind_host.as_str(), "127.0.0.1" | "localhost" | "::1");
    let auth_token = config.mcp_auth_token.clone();
    if auth_token.is_none() && !is_loopback && !config.mcp_allow_no_auth {
        eprintln!(
            "morloc serve: refusing to serve on {} with no auth token (bind is not loopback). \
             Set --auth-token / MORLOC_MCP_TOKEN, or pass --allow-no-auth to override.",
            bound
        );
        process::clean_exit(1);
    }
    eprintln!(
        "morloc serve: MCP at http://{}/mcp ({} tools) | API at http://{}/call/<module>/<command> \
         | discovery at http://{}/discover",
        bound,
        mcp_tools.len(),
        bound,
        bound
    );

    let fe = Arc::new(Frontend {
        router: RouterHandle(router),
        locks,
        mcp_tools,
        mcp_by_name,
        api_modules,
        api_help,
        eval_enabled: config.eval_enabled,
        eval_allow: config.eval_allowed_modules.clone(),
        eval_timeout: config.eval_timeout,
        server_name: "morloc".to_string(),
        server_version: if server_version.is_empty() {
            "unknown".to_string()
        } else {
            server_version
        },
        sessions: Mutex::new(HashMap::new()),
    });
    let token = Arc::new(auth_token);
    for incoming in listener.incoming() {
        match incoming {
            Ok(stream) => {
                let fe = Arc::clone(&fe);
                let tk = Arc::clone(&token);
                std::thread::spawn(move || {
                    serve_conn(stream, |req, ka| {
                        frontend_build_response(req, &fe, tk.as_ref().as_deref(), ka)
                    })
                });
            }
            Err(e) => eprintln!("morloc serve: accept error: {}", e),
        }
    }
    process::clean_exit(0);
}

/// One front-end connection: read requests (keep-alive) and reply. Dispatch runs
/// off any shared lock, so requests to different modules execute in parallel.
/// Auth (both adapters), then route on path: `/mcp` -> MCP, `/call/...` -> API.
fn frontend_build_response(
    req: &HttpRequest,
    fe: &Arc<Frontend>,
    token: Option<&str>,
    keep_alive: bool,
) -> Vec<u8> {
    // A CORS preflight (OPTIONS) carries no Authorization header, so it must be
    // answered before the auth gate; the browser sends the real, authenticated
    // request only if this 204 grants the method/headers.
    if req.method == "OPTIONS" {
        let extra = vec![
            ("Access-Control-Allow-Methods".to_string(), "GET, POST, DELETE, OPTIONS".to_string()),
            (
                "Access-Control-Allow-Headers".to_string(),
                "Authorization, Content-Type, Mcp-Session-Id".to_string(),
            ),
            ("Access-Control-Max-Age".to_string(), "86400".to_string()),
        ];
        return http_response(204, b"", keep_alive, &extra);
    }
    let path = req.path.split('?').next().unwrap_or("");
    // Liveness probe for container orchestrators (the deploy image's
    // HEALTHCHECK). Answered BEFORE the auth gate: it reveals nothing and a
    // token-protected serve must not 401 its own healthcheck (which curls with
    // no token). The front-end is up if it can answer; child health is per-call.
    if path == "/health" && (req.method == "GET" || req.method == "HEAD") {
        return http_json(200, br#"{"status":"ok"}"#, keep_alive);
    }
    if let Some(tok) = token {
        if !authorized(&req.headers, tok) {
            let extra = vec![("WWW-Authenticate".to_string(), "Bearer".to_string())];
            return http_response(401, br#"{"error":"unauthorized"}"#, keep_alive, &extra);
        }
    }
    if path == "/mcp" {
        match req.method.as_str() {
            "POST" => frontend_mcp_post(req, fe, keep_alive),
            "DELETE" => frontend_mcp_delete(req, fe, keep_alive),
            _ => method_not_allowed("POST, DELETE", keep_alive),
        }
    } else if let Some(rest) = path.strip_prefix("/call/") {
        match req.method.as_str() {
            "POST" => frontend_call_api(rest, req, fe, keep_alive),
            _ => method_not_allowed("POST", keep_alive),
        }
    } else if path == "/eval" {
        match req.method.as_str() {
            "POST" => frontend_eval_api(req, fe, keep_alive),
            _ => method_not_allowed("POST", keep_alive),
        }
    } else if path == "/discover" {
        match req.method.as_str() {
            "GET" | "HEAD" => frontend_discover(fe, keep_alive),
            _ => method_not_allowed("GET", keep_alive),
        }
    } else if let Some(module) = path.strip_prefix("/discover/") {
        match req.method.as_str() {
            "GET" | "HEAD" => frontend_discover_module(module, fe, keep_alive),
            _ => method_not_allowed("GET", keep_alive),
        }
    } else {
        http_json(404, br#"{"error":"not found"}"#, keep_alive)
    }
}

/// `GET /discover`: the API index. Lists the JSON-API-exposed modules (with the
/// `/call` URL shape and a link to each module's positional-argument help) and
/// points MCP clients at the protocol-native `tools/list`. This is the API-side
/// counterpart to MCP discovery -- the two adapters branch here because `/call`
/// takes positional args while MCP tools take a named-argument object.
fn frontend_discover(fe: &Arc<Frontend>, keep_alive: bool) -> Vec<u8> {
    let mut modules: Vec<&String> = fe.api_help.keys().collect();
    modules.sort();
    let api_modules: Vec<Value> = modules
        .iter()
        .map(|m| {
            json!({
                "module": m,
                "call": format!("/call/{}/<command>", m),
                "help": format!("/discover/{}", m),
            })
        })
        .collect();
    let doc = json!({
        "api": {
            "modules": api_modules,
            "call": "/call/<module>/<command>",
            "note": "POST positional args as a JSON array (or {\"args\":[...]}). \
                     GET /discover/<module> for each command's ordered arguments.",
        },
        "mcp": {
            "endpoint": "/mcp",
            "tools": fe.mcp_tools.len(),
            "note": "Use the JSON-RPC `tools/list` method for the MCP tool catalog \
                     (tools are named <module>__<command> and take named arguments).",
        },
        "eval": {
            "enabled": fe.eval_enabled,
            "endpoints": ["/eval", "mcp tool 'eval'"],
        },
    });
    http_json(200, doc.to_string().as_bytes(), keep_alive)
}

/// `GET /discover/<module>`: the machine-readable help for one API module (its
/// commands and their positional arguments). 404 unless the module is exposed on
/// the JSON API.
fn frontend_discover_module(module: &str, fe: &Arc<Frontend>, keep_alive: bool) -> Vec<u8> {
    match fe.api_help.get(module) {
        Some(help) => http_json(200, help.to_string().as_bytes(), keep_alive),
        None => http_json(404, br#"{"error":"module not exposed on the API"}"#, keep_alive),
    }
}

/// True if the `morloc` compiler is on PATH (the eval capability forks it).
fn morloc_on_path() -> bool {
    std::env::var_os("PATH")
        .map(|paths| {
            std::env::split_paths(&paths)
                .any(|dir| std::fs::metadata(dir.join("morloc")).map(|m| m.is_file()).unwrap_or(false))
        })
        .unwrap_or(false)
}

/// Run a sandboxed `morloc eval` as a child process (isolated: a crash or hang
/// in eval cannot take down the front-end). `--eval-sandbox` bans IO intrinsics;
/// `--eval-allowed-modules` bounds imports (empty => none). Returns eval's stdout
/// on success, else its stderr.
fn frontend_eval(expr: &str, fe: &Frontend) -> Result<String, String> {
    let allow = fe.eval_allow.clone().unwrap_or_default();
    let mut cmd = std::process::Command::new("morloc");
    cmd.arg("eval")
        .arg("--eval-sandbox")
        .arg("--eval-allowed-modules")
        .arg(&allow)
        .arg("-e")
        .arg(expr);
    // Bound the child's CPU time and address space so a runaway expression
    // (eval-sandbox bans IO, so a hang can only be a compute or memory loop) is
    // killed rather than pinning/exhausting the host. Mirrors the daemon's eval
    // fork (RLIMIT_CPU + a 2 GiB RLIMIT_AS).
    let cpu_secs = fe.eval_timeout;
    if cpu_secs > 0 {
        use std::os::unix::process::CommandExt;
        unsafe {
            cmd.pre_exec(move || {
                let cpu = libc::rlimit {
                    rlim_cur: cpu_secs as libc::rlim_t,
                    rlim_max: (cpu_secs + 5) as libc::rlim_t,
                };
                if libc::setrlimit(libc::RLIMIT_CPU, &cpu) != 0 {
                    return Err(std::io::Error::last_os_error());
                }
                let as_lim = libc::rlimit {
                    rlim_cur: 2 * 1024 * 1024 * 1024,
                    rlim_max: 2 * 1024 * 1024 * 1024,
                };
                if libc::setrlimit(libc::RLIMIT_AS, &as_lim) != 0 {
                    return Err(std::io::Error::last_os_error());
                }
                Ok(())
            });
        }
    }
    let out = cmd
        .output()
        .map_err(|e| {
            format!(
                "failed to run eval: {} (the eval capability requires the `morloc` \
                 compiler on PATH in the serving environment)",
                e
            )
        })?;
    if out.status.success() {
        Ok(String::from_utf8_lossy(&out.stdout).trim_end().to_string())
    } else {
        let err = String::from_utf8_lossy(&out.stderr);
        let err = err.trim();
        Err(if err.is_empty() { "eval failed".to_string() } else { err.to_string() })
    }
}

/// `POST /eval`: the API-side eval capability. Body is `{"expr":"..."}` (or
/// `{"expression":"..."}`, or a bare JSON string). 404 when eval is not exposed.
fn frontend_eval_api(req: &HttpRequest, fe: &Frontend, keep_alive: bool) -> Vec<u8> {
    if !fe.eval_enabled {
        return http_json(404, br#"{"error":"eval is not exposed"}"#, keep_alive);
    }
    let expr: Option<String> = match serde_json::from_slice::<Value>(&req.body) {
        Ok(Value::Object(o)) => o
            .get("expr")
            .or_else(|| o.get("expression"))
            .and_then(|v| v.as_str())
            .map(String::from),
        Ok(Value::String(s)) => Some(s),
        _ => None,
    };
    let expr = match expr {
        Some(e) => e,
        None => return http_json(400, br#"{"error":"expected {\"expr\":\"...\"}"}"#, keep_alive),
    };
    match frontend_eval(&expr, fe) {
        Ok(out) => {
            let body = json!({ "status": "ok", "result": out }).to_string();
            http_json(200, body.as_bytes(), keep_alive)
        }
        Err(message) => {
            let body = json!({ "status": "error", "error": message }).to_string();
            http_json(500, body.as_bytes(), keep_alive)
        }
    }
}

/// `POST /mcp`: one JSON-RPC message. Session resolution locks briefly; the
/// forward (dispatch) runs lock-free so modules execute in parallel.
fn frontend_mcp_post(req: &HttpRequest, fe: &Arc<Frontend>, keep_alive: bool) -> Vec<u8> {
    let msg: Value = match serde_json::from_slice(&req.body) {
        Ok(v) => v,
        Err(_) => {
            let body = error_response(Value::Null, JSONRPC_PARSE_ERROR, "invalid JSON")
                .to_string()
                .into_bytes();
            return http_json(200, &body, keep_alive);
        }
    };
    let method = msg.get("method").and_then(|m| m.as_str()).unwrap_or("");
    let sid_hdr = header_get(&req.headers, "mcp-session-id").map(|s| s.to_string());
    let now = Instant::now();

    let (session_id, prior_init) = {
        let mut sessions = fe.sessions.lock().unwrap_or_else(|p| p.into_inner());
        if method == "initialize" {
            prune_sessions(&mut sessions, now);
            let id = new_session_id();
            sessions.insert(id.clone(), SessionInfo { initialized: false, last_seen: now });
            (id, false)
        } else {
            match sid_hdr {
                None => {
                    return http_json(400, br#"{"error":"missing Mcp-Session-Id header"}"#, keep_alive)
                }
                Some(id) => match sessions.get(&id) {
                    Some(s) => (id, s.initialized),
                    None => {
                        return http_json(
                            404,
                            br#"{"error":"unknown or expired session; re-initialize"}"#,
                            keep_alive,
                        )
                    }
                },
            }
        }
    };

    let mut session = Session { initialized: prior_init };
    let resp_opt = frontend_handle_message(&msg, &mut session, fe);
    {
        // Update in place only if the session still exists. A DELETE or prune
        // that ran while the forward was in flight (lock-free) must not be
        // undone by resurrecting the entry, and re-inserting would bypass the
        // MAX_SESSIONS cap enforced at initialize.
        let mut sessions = fe.sessions.lock().unwrap_or_else(|p| p.into_inner());
        if let Some(s) = sessions.get_mut(&session_id) {
            s.initialized = session.initialized;
            s.last_seen = now;
        }
    }
    let extra: Vec<(String, String)> = if method == "initialize" {
        vec![("Mcp-Session-Id".to_string(), session_id)]
    } else {
        Vec::new()
    };
    match resp_opt {
        Some(response) => {
            http_response(200, &response.to_string().into_bytes(), keep_alive, &extra)
        }
        None => http_response(202, b"", keep_alive, &extra),
    }
}

/// `DELETE /mcp`: end the named session.
fn frontend_mcp_delete(req: &HttpRequest, fe: &Arc<Frontend>, keep_alive: bool) -> Vec<u8> {
    match header_get(&req.headers, "mcp-session-id") {
        Some(id) => {
            let mut sessions = fe.sessions.lock().unwrap_or_else(|p| p.into_inner());
            sessions.remove(id);
            http_json(200, br#"{"ok":true}"#, keep_alive)
        }
        None => http_json(400, br#"{"error":"missing Mcp-Session-Id header"}"#, keep_alive),
    }
}

/// MCP message core for the front-end (mirrors `handle_message`, but tools/call
/// forwards to a per-module child instead of dispatching in-process).
fn frontend_handle_message(msg: &Value, session: &mut Session, fe: &Frontend) -> Option<Value> {
    let method = msg.get("method").and_then(|m| m.as_str());
    let id = msg.get("id").cloned();
    let is_request = id.is_some();
    let method = match method {
        Some(m) => m,
        None => {
            return id.map(|id| error_response(id, JSONRPC_INVALID_REQUEST, "missing 'method'"))
        }
    };
    match method {
        "initialize" => {
            let Some(id) = id else { return None };
            let requested = msg
                .get("params")
                .and_then(|p| p.get("protocolVersion"))
                .and_then(|v| v.as_str());
            let version = match requested {
                Some(v) if v == SERVER_PROTOCOL_VERSION => v.to_string(),
                _ => SERVER_PROTOCOL_VERSION.to_string(),
            };
            Some(result_response(
                id,
                json!({
                    "protocolVersion": version,
                    "capabilities": { "tools": { "listChanged": false } },
                    "serverInfo": { "name": fe.server_name, "version": fe.server_version },
                }),
            ))
        }
        "notifications/initialized" => {
            session.initialized = true;
            None
        }
        "ping" => id.map(|id| result_response(id, json!({}))),
        "tools/list" => {
            let Some(id) = id else { return None };
            if !session.initialized {
                return Some(error_response(
                    id,
                    JSONRPC_INVALID_REQUEST,
                    "received tools/list before initialization completed",
                ));
            }
            Some(result_response(id, json!({ "tools": fe.mcp_tools })))
        }
        "tools/call" => {
            let Some(id) = id else { return None };
            if !session.initialized {
                return Some(error_response(
                    id,
                    JSONRPC_INVALID_REQUEST,
                    "received tools/call before initialization completed",
                ));
            }
            Some(frontend_tools_call(id, msg, fe))
        }
        other => {
            if is_request {
                Some(error_response(
                    id.unwrap_or(Value::Null),
                    JSONRPC_METHOD_NOT_FOUND,
                    &format!("unknown method '{}'", other),
                ))
            } else {
                None
            }
        }
    }
}

/// A front-end `tools/call`: resolve the namespaced tool to (module, command),
/// validate + invert arguments, forward to the module's child, render the reply.
fn frontend_tools_call(id: Value, msg: &Value, fe: &Frontend) -> Value {
    let params = match msg.get("params") {
        Some(p) => p,
        None => return error_response(id, JSONRPC_INVALID_PARAMS, "missing params"),
    };
    let name = match params.get("name").and_then(|n| n.as_str()) {
        Some(n) => n,
        None => return error_response(id, JSONRPC_INVALID_PARAMS, "missing tool 'name'"),
    };
    let empty = Map::new();
    let arguments = match params.get("arguments") {
        None | Some(Value::Null) => &empty,
        Some(Value::Object(m)) => m,
        Some(_) => return error_response(id, JSONRPC_INVALID_PARAMS, "'arguments' must be an object"),
    };
    // The synthetic eval capability (not a module): forks a sandboxed
    // `morloc eval`, keeping the front-end's no-in-process-compute invariant.
    if name == "eval" {
        if !fe.eval_enabled {
            return error_response(id, JSONRPC_INVALID_PARAMS, "unknown tool 'eval'");
        }
        let expr = match arguments.get("expression").and_then(|v| v.as_str()) {
            Some(e) => e,
            None => return error_response(id, JSONRPC_INVALID_PARAMS, "missing 'expression'"),
        };
        return match frontend_eval(expr, fe) {
            Ok(out) => result_response(id, json!({ "content": [ { "type": "text", "text": out } ] })),
            Err(message) => result_response(
                id,
                json!({ "content": [ { "type": "text", "text": message } ], "isError": true }),
            ),
        };
    }
    let (module, shape) = match fe.mcp_by_name.get(name) {
        Some(t) => t,
        None => {
            return error_response(id, JSONRPC_INVALID_PARAMS, &format!("unknown tool '{}'", name))
        }
    };
    let positional = match resolve_arguments(shape, arguments) {
        Ok(p) => p,
        Err(e) => return error_response(id, JSONRPC_INVALID_PARAMS, &e),
    };
    let render_sel = arguments.get("_render").and_then(|v| v.as_str()).unwrap_or("raw");
    // A media (`@mime`) projection comes back from the child as raw bytes+mime
    // (base64 over the wire) and is rendered as an MCP media block below; the
    // JSON projection becomes a text/structured result.
    let (dispatch_command, _ret_mime, returns_object) = match shape.dispatch_for(render_sel) {
        Ok(t) => t,
        Err(e) => return error_response(id, JSONRPC_INVALID_PARAMS, &e),
    };
    let request_json = json!({
        "method": "call",
        "command": dispatch_command,
        "args": Value::Array(positional),
    });
    match fe.forward(module, &request_json) {
        Ok(ForwardOk::Json(result_json)) => {
            result_response(id, tool_result_object(&result_json, returns_object))
        }
        Ok(ForwardOk::Media(bytes, mime)) => {
            result_response(id, media_result_object(media_content_block(&bytes, &mime)))
        }
        Err(message) => result_response(
            id,
            json!({ "content": [ { "type": "text", "text": message } ], "isError": true }),
        ),
    }
}

/// `POST /call/<module>/<command>`: JSON API adapter. Body is a positional-args
/// JSON array or `{"args":[...]}`. Stateless (no session).
fn frontend_call_api(
    rest: &str,
    req: &HttpRequest,
    fe: &Frontend,
    keep_alive: bool,
) -> Vec<u8> {
    let mut it = rest.splitn(2, '/');
    let module = it.next().unwrap_or("");
    let command = it.next().unwrap_or("");
    if module.is_empty() || command.is_empty() {
        return http_json(400, br#"{"error":"expected /call/<module>/<command>"}"#, keep_alive);
    }
    if !fe.api_modules.contains(module) {
        return http_json(404, br#"{"error":"module not exposed on the API"}"#, keep_alive);
    }
    let args: Value = if req.body.is_empty() {
        Value::Array(Vec::new())
    } else {
        match serde_json::from_slice::<Value>(&req.body) {
            Ok(Value::Array(a)) => Value::Array(a),
            Ok(Value::Object(o)) => o.get("args").cloned().unwrap_or(Value::Array(Vec::new())),
            Ok(_) => {
                return http_json(
                    400,
                    br#"{"error":"body must be a JSON array or {\"args\":[...]}"}"#,
                    keep_alive,
                )
            }
            Err(_) => return http_json(400, br#"{"error":"invalid JSON body"}"#, keep_alive),
        }
    };
    let request_json = json!({ "method": "call", "command": command, "args": args });
    match fe.forward(module, &request_json) {
        Ok(ForwardOk::Json(result_json)) => {
            let body = format!("{{\"status\":\"ok\",\"result\":{}}}", result_json);
            http_json(200, body.as_bytes(), keep_alive)
        }
        // A media (`@mime`) return goes out as the raw content bytes with the
        // declared Content-Type (a real image/PDF/...), not a JSON envelope.
        Ok(ForwardOk::Media(bytes, mime)) => http_binary_response(200, &mime, &bytes, keep_alive),
        Err(message) => {
            let body = json!({ "status": "error", "error": message }).to_string();
            http_json(500, body.as_bytes(), keep_alive)
        }
    }
}

/// An HTTP response carrying raw bytes with an explicit Content-Type and no
/// trailing newline (for `@mime` media returns on the API adapter).
fn http_binary_response(status: u16, content_type: &str, body: &[u8], keep_alive: bool) -> Vec<u8> {
    let mut out = http_head(status, content_type, body.len(), keep_alive, &[]).into_bytes();
    out.extend_from_slice(body);
    out
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::json_help::RecordField;
    use std::collections::HashSet;

    fn shape(slots: Vec<ArgSlot>, props: &[&str], required: &[&str], returns_object: bool) -> McpToolShape {
        let prop_names: HashSet<String> = props.iter().map(|s| s.to_string()).collect();
        McpToolShape {
            name: "t".into(),
            tool: json!({ "name": "t" }),
            slots,
            prop_names,
            required: required.iter().map(|s| s.to_string()).collect(),
            returns_object,
            return_mime: None,
            renders: Vec::new(),
        }
    }

    fn args(v: Value) -> Map<String, Value> {
        match v {
            Value::Object(m) => m,
            _ => panic!("test args must be an object"),
        }
    }

    #[test]
    fn optional_omitted_uses_typed_default() {
        let s = shape(
            vec![ArgSlot::Value { key: "count".into(), missing: json!(5) }],
            &["count"],
            &[],
            false,
        );
        assert_eq!(resolve_arguments(&s, &args(json!({}))).unwrap(), vec![json!(5)]);
        assert_eq!(
            resolve_arguments(&s, &args(json!({ "count": 10 }))).unwrap(),
            vec![json!(10)]
        );
    }

    #[test]
    fn missing_required_is_error() {
        let s = shape(
            vec![ArgSlot::Value { key: "n".into(), missing: Value::Null }],
            &["n"],
            &["n"],
            false,
        );
        assert!(resolve_arguments(&s, &args(json!({}))).is_err());
        assert_eq!(resolve_arguments(&s, &args(json!({ "n": 7 }))).unwrap(), vec![json!(7)]);
    }

    #[test]
    fn unknown_argument_is_error() {
        let s = shape(
            vec![ArgSlot::Value { key: "n".into(), missing: Value::Null }],
            &["n"],
            &[],
            false,
        );
        assert!(resolve_arguments(&s, &args(json!({ "bogus": 1 }))).is_err());
    }

    #[test]
    fn flag_default_and_type_check() {
        let s = shape(
            vec![ArgSlot::Flag { key: "v".into(), default: false }],
            &["v"],
            &[],
            false,
        );
        assert_eq!(resolve_arguments(&s, &args(json!({}))).unwrap(), vec![json!(false)]);
        assert_eq!(resolve_arguments(&s, &args(json!({ "v": true }))).unwrap(), vec![json!(true)]);
        // Non-boolean flag value is rejected.
        assert!(resolve_arguments(&s, &args(json!({ "v": "yes" }))).is_err());
    }

    #[test]
    fn record_unrolled_assembles_object_from_fields() {
        let s = shape(
            vec![ArgSlot::Record {
                group_key: None,
                fields: vec![
                    RecordField { field: "a".into(), default: json!(3) },
                    RecordField { field: "b".into(), default: json!("hi") },
                ],
            }],
            &["a", "b"],
            &[],
            false,
        );
        // Provided field overrides its default; omitted field uses it.
        assert_eq!(
            resolve_arguments(&s, &args(json!({ "a": 10 }))).unwrap(),
            vec![json!({ "a": 10, "b": "hi" })]
        );
    }

    #[test]
    fn record_group_opt_accepts_whole_object() {
        let s = shape(
            vec![ArgSlot::Record {
                group_key: Some("cfg".into()),
                fields: vec![RecordField { field: "a".into(), default: json!(3) }],
            }],
            &["cfg"],
            &[],
            false,
        );
        // Whole object passed under the group key is used verbatim.
        assert_eq!(
            resolve_arguments(&s, &args(json!({ "cfg": { "a": 9 } }))).unwrap(),
            vec![json!({ "a": 9 })]
        );
        // Omitted: assembled from field defaults.
        assert_eq!(
            resolve_arguments(&s, &args(json!({}))).unwrap(),
            vec![json!({ "a": 3 })]
        );
    }

    #[test]
    fn scalar_result_has_text_only() {
        let r = tool_result_object("42", false);
        assert_eq!(r["isError"], json!(false));
        assert_eq!(r["content"][0]["type"], "text");
        assert_eq!(r["content"][0]["text"], "42");
        assert!(r.get("structuredContent").is_none());
    }

    #[test]
    fn object_result_has_structured_content() {
        let r = tool_result_object("{\"a\":1}", true);
        assert_eq!(r["structuredContent"], json!({ "a": 1 }));
        assert_eq!(r["content"][0]["text"], "{\"a\":1}");
    }

    #[test]
    fn structured_flag_ignored_for_non_object_result() {
        // Even when the return type is declared object, an array/scalar body
        // must not be placed in structuredContent (it must be a JSON object).
        let r = tool_result_object("[1,2]", true);
        assert!(r.get("structuredContent").is_none());
        assert_eq!(r["content"][0]["text"], "[1,2]");
    }

    #[test]
    fn image_mime_produces_base64_image_block() {
        // A media-typed return arrives as raw bytes (from the backend's raw-media
        // path); an image/* media type turns them into an MCP image block with
        // base64 data -- not a giant JSON number array.
        let r = media_result_object(media_content_block(&[137, 80, 78, 71], "image/png"));
        assert_eq!(r["isError"], json!(false));
        assert_eq!(r["content"][0]["type"], "image");
        assert_eq!(r["content"][0]["mimeType"], "image/png");
        assert_eq!(r["content"][0]["data"], base64_encode(&[137, 80, 78, 71]));
        assert!(r.get("structuredContent").is_none());
    }

    #[test]
    fn text_mime_produces_unquoted_text_block() {
        // A text/* renderer returns `Str`; the raw bytes are surfaced as text.
        let r = media_result_object(media_content_block(b"hi there", "text/plain"));
        assert_eq!(r["content"][0]["type"], "text");
        assert_eq!(r["content"][0]["text"], "hi there");
    }

    #[test]
    fn other_binary_mime_produces_resource_blob() {
        let r = media_result_object(media_content_block(&[1, 2, 3], "application/octet-stream"));
        assert_eq!(r["content"][0]["type"], "resource");
        assert_eq!(
            r["content"][0]["resource"]["mimeType"],
            "application/octet-stream"
        );
        assert_eq!(
            r["content"][0]["resource"]["blob"],
            base64_encode(&[1, 2, 3])
        );
    }

    // -- handshake / dispatch of non-FFI methods -----------------------------

    fn empty_by_name() -> HashMap<&'static str, &'static McpToolShape> {
        HashMap::new()
    }

    fn call(msg: Value, session: &mut Session, tools: &[Value]) -> Option<Value> {
        // These tests exercise only non-FFI methods (initialize / tools/list /
        // ping / unknown), which never call `ctx.dispatch`, so a null-pointer
        // context is never dereferenced.
        let ctx = DispatchCtx {
            manifest_c: ptr::null_mut(),
            sockets_ptr: ptr::null_mut(),
            n_pools: 0,
            shm: CString::new("").unwrap(),
        };
        handle_message(&msg, session, tools, &empty_by_name(), &ctx, "prog", "1.2.3")
    }

    #[test]
    fn initialize_reports_version_and_server_info() {
        let mut s = Session::new();
        let resp = call(
            json!({ "jsonrpc": "2.0", "id": 1, "method": "initialize",
                    "params": { "protocolVersion": SERVER_PROTOCOL_VERSION } }),
            &mut s,
            &[],
        )
        .expect("initialize is a request");
        assert_eq!(resp["id"], json!(1));
        assert_eq!(resp["result"]["protocolVersion"], SERVER_PROTOCOL_VERSION);
        assert_eq!(resp["result"]["serverInfo"]["name"], "prog");
        assert_eq!(resp["result"]["capabilities"]["tools"]["listChanged"], json!(false));
    }

    #[test]
    fn tools_list_gated_on_initialization() {
        let mut s = Session::new();
        let tools = vec![json!({ "name": "a" }), json!({ "name": "b" })];
        // Before the initialized notification: an error.
        let early = call(
            json!({ "jsonrpc": "2.0", "id": 2, "method": "tools/list" }),
            &mut s,
            &tools,
        )
        .expect("request");
        assert!(early.get("error").is_some());

        // The initialized notification carries no id and gets no response.
        assert!(call(
            json!({ "jsonrpc": "2.0", "method": "notifications/initialized" }),
            &mut s,
            &tools,
        )
        .is_none());
        assert!(s.initialized);

        // After: the tools are served.
        let ok = call(
            json!({ "jsonrpc": "2.0", "id": 3, "method": "tools/list" }),
            &mut s,
            &tools,
        )
        .expect("request");
        assert_eq!(ok["result"]["tools"].as_array().unwrap().len(), 2);
    }

    #[test]
    fn ping_answered_before_initialize() {
        let mut s = Session::new();
        let resp = call(
            json!({ "jsonrpc": "2.0", "id": 9, "method": "ping" }),
            &mut s,
            &[],
        )
        .expect("request");
        assert_eq!(resp["result"], json!({}));
    }

    #[test]
    fn unknown_method_request_is_method_not_found() {
        let mut s = Session::new();
        let resp = call(
            json!({ "jsonrpc": "2.0", "id": 4, "method": "does/notExist" }),
            &mut s,
            &[],
        )
        .expect("request");
        assert_eq!(resp["error"]["code"], json!(JSONRPC_METHOD_NOT_FOUND));
    }

    #[test]
    fn unknown_notification_is_ignored() {
        let mut s = Session::new();
        assert!(call(
            json!({ "jsonrpc": "2.0", "method": "notifications/cancelled" }),
            &mut s,
            &[],
        )
        .is_none());
    }

    #[test]
    fn request_methods_sent_as_notifications_are_ignored() {
        // A request-only method with no `id` is a notification: it must get no
        // response, and (for tools/call) must not execute the tool. `call` uses
        // a null DispatchCtx, so a `tools/call` that reached the backend would
        // dereference it -- returning None proves it never does.
        let mut s = Session::new();
        s.initialized = true;
        for method in ["initialize", "tools/list", "tools/call"] {
            assert!(
                call(json!({ "jsonrpc": "2.0", "method": method }), &mut s, &[]).is_none(),
                "{} without an id must be ignored",
                method
            );
        }
    }

    #[test]
    fn read_lines_splits_and_keeps_partial_tail() {
        let input: &[u8] = b"one\ntwo\n\npartial";
        let mut it = read_lines_capped(input);
        assert_eq!(it.next().unwrap().unwrap(), "one");
        assert_eq!(it.next().unwrap().unwrap(), "two");
        assert_eq!(it.next().unwrap().unwrap(), ""); // blank line preserved
        assert_eq!(it.next().unwrap().unwrap(), "partial"); // unterminated tail
        assert!(matches!(it.next(), Some(Err(LineError::Eof))));
    }

    // -- HTTP transport helpers ---------------------------------------------

    fn hdr(pairs: &[(&str, &str)]) -> Vec<(String, String)> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn http_response_frames_status_headers_and_body() {
        let extra = vec![("Mcp-Session-Id".to_string(), "abc".to_string())];
        let bytes = http_response(200, b"{\"ok\":1}", true, &extra);
        let text = String::from_utf8(bytes).unwrap();
        assert!(text.starts_with("HTTP/1.1 200 OK\r\n"));
        assert!(text.contains("Content-Type: application/json\r\n"));
        // Content-Length counts the appended trailing newline (8 + 1).
        assert!(text.contains("Content-Length: 9\r\n"));
        assert!(text.contains("Connection: keep-alive\r\n"));
        assert!(text.contains("Mcp-Session-Id: abc\r\n"));
        assert!(text.ends_with("\r\n\r\n{\"ok\":1}\n"));
    }

    #[test]
    fn http_response_close_when_not_keep_alive() {
        let bytes = http_response(202, b"", false, &[]);
        let text = String::from_utf8(bytes).unwrap();
        assert!(text.contains("Connection: close\r\n"));
        assert!(text.contains("Content-Length: 0\r\n"));
    }

    #[test]
    fn authorized_requires_exact_bearer_token() {
        assert!(authorized(&hdr(&[("authorization", "Bearer s3cret")]), "s3cret"));
        assert!(!authorized(&hdr(&[("authorization", "Bearer nope")]), "s3cret"));
        assert!(!authorized(&hdr(&[("authorization", "Basic s3cret")]), "s3cret"));
        assert!(!authorized(&hdr(&[]), "s3cret"));
    }

    #[test]
    fn header_get_is_case_insensitive_on_lowercased_names() {
        // Parser lowercases names; lookup uses the lowercased key.
        let h = hdr(&[("mcp-session-id", "xyz"), ("content-length", "10")]);
        assert_eq!(header_get(&h, "mcp-session-id"), Some("xyz"));
        assert_eq!(header_get(&h, "missing"), None);
    }

    #[test]
    fn session_ids_are_unique_and_hex_when_random_available() {
        let a = new_session_id();
        let b = new_session_id();
        assert_ne!(a, b);
        // On a normal system /dev/urandom yields a 32-char hex id.
        if a.len() == 32 {
            assert!(a.chars().all(|c| c.is_ascii_hexdigit()));
        }
    }

    #[test]
    fn read_http_request_parses_request_line_headers_and_body() {
        // A localhost listener lets us exercise the real BufReader<TcpStream>
        // path without a client library.
        use std::io::Write as _;
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let addr = listener.local_addr().unwrap();
        let body = br#"{"jsonrpc":"2.0","id":1,"method":"ping"}"#;
        let req = format!(
            "POST /mcp?x=1 HTTP/1.1\r\nHost: localhost\r\nMcp-Session-Id: sid\r\n\
             Content-Length: {}\r\n\r\n",
            body.len()
        );
        let handle = std::thread::spawn(move || {
            let mut c = TcpStream::connect(addr).unwrap();
            c.write_all(req.as_bytes()).unwrap();
            c.write_all(body).unwrap();
            c.flush().unwrap();
            // Keep the connection open briefly so the server-side read completes.
            std::thread::sleep(std::time::Duration::from_millis(50));
        });
        let (stream, _) = listener.accept().unwrap();
        let mut reader = BufReader::new(stream);
        let parsed = read_http_request(&mut reader).unwrap().unwrap();
        assert_eq!(parsed.method, "POST");
        assert_eq!(parsed.path, "/mcp?x=1");
        assert_eq!(header_get(&parsed.headers, "mcp-session-id"), Some("sid"));
        assert_eq!(parsed.body, body);
        handle.join().unwrap();
    }

    // -- request-read hardening (generic over BufRead, exercised via slices) --

    #[test]
    fn read_http_request_rejects_overlong_header_line() {
        // One header line past MAX_HEADER_LINE_BYTES -> connection closed (None).
        let big = "a".repeat(MAX_HEADER_LINE_BYTES as usize + 1024);
        let raw = format!("GET /mcp HTTP/1.1\r\nX: {big}\r\n\r\n");
        let mut slice = raw.as_bytes();
        assert!(read_http_request(&mut slice).unwrap().is_none());
    }

    #[test]
    fn read_http_request_rejects_header_flood() {
        // More than MAX_HEADERS short headers -> rejected before the blank line.
        let mut raw = String::from("GET /mcp HTTP/1.1\r\n");
        for i in 0..(MAX_HEADERS + 2) {
            raw.push_str(&format!("H{i}: v\r\n"));
        }
        raw.push_str("\r\n");
        let mut slice = raw.as_bytes();
        assert!(read_http_request(&mut slice).unwrap().is_none());
    }

    #[test]
    fn read_http_request_body_bounded_to_content_length() {
        // Exactly Content-Length bytes are taken; trailing bytes (a pipelined
        // next request) are left for the following read, not swallowed.
        let raw = b"POST /mcp HTTP/1.1\r\nContent-Length: 5\r\n\r\nHELLOEXTRA";
        let mut slice = &raw[..];
        let parsed = read_http_request(&mut slice).unwrap().unwrap();
        assert_eq!(parsed.body, b"HELLO");
        assert_eq!(slice, b"EXTRA"); // remainder preserved
    }

    #[test]
    fn prune_sessions_evicts_idle_and_enforces_cap() {
        // Use a `now` in the future so the "old" entry is idle past the TTL
        // without depending on wall-clock/uptime (avoids Instant underflow).
        let base = Instant::now();
        let future = base + SESSION_TTL + Duration::from_secs(1);
        let mut m: HashMap<String, SessionInfo> = HashMap::new();
        m.insert("old".into(), SessionInfo { initialized: true, last_seen: base });
        m.insert("fresh".into(), SessionInfo { initialized: false, last_seen: future });
        prune_sessions(&mut m, future);
        assert!(!m.contains_key("old"), "idle session should be evicted");
        assert!(m.contains_key("fresh"), "current session should survive");

        // Hard cap: fill to MAX_SESSIONS (all current), prune makes room for one.
        let mut full: HashMap<String, SessionInfo> = HashMap::new();
        for i in 0..MAX_SESSIONS {
            full.insert(format!("s{i}"), SessionInfo { initialized: false, last_seen: base });
        }
        prune_sessions(&mut full, base);
        assert!(full.len() < MAX_SESSIONS, "cap backstop should evict the oldest");
    }
}
