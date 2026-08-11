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
use std::io::BufRead;
use std::os::unix::io::RawFd;
use std::ptr;

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
    let by_name: HashMap<String, &McpToolShape> =
        shapes.iter().map(|s| (s.name.clone(), s)).collect();

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
    by_name: &HashMap<String, &McpToolShape>,
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
    by_name: &HashMap<String, &McpToolShape>,
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

    fn empty_by_name() -> HashMap<String, &'static McpToolShape> {
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
}
