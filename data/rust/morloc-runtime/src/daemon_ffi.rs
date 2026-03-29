//! C ABI wrappers for daemon subsystems.
//! Replaces daemon.c. Uses serde_json, HashMap, VecDeque, and std::thread.

use std::collections::HashMap;
use std::collections::VecDeque;
use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Arc, Condvar, Mutex};

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::hash;
use crate::http_ffi::{DaemonMethod, DaemonRequest, HttpRequest};

// ── Constants ────────────────────────────────────────────────────────────────

const DEFAULT_XXHASH_SEED: u64 = 0;
const MAX_LP_MESSAGE: u32 = 64 * 1024 * 1024;

// ── Global state ─────────────────────────────────────────────────────────────

static SHUTDOWN_REQUESTED: AtomicBool = AtomicBool::new(false);
static G_EVAL_TIMEOUT: AtomicI32 = AtomicI32::new(30);

// SAFETY: These globals are set once during daemon_run initialization (single-threaded)
// and only read afterwards. The daemon is single-threaded for request dispatch.
static mut G_POOL_ALIVE_FN: Option<unsafe extern "C" fn(usize) -> bool> = None;
static mut G_N_POOLS: usize = 0;
static mut G_BINDING_STORE: *mut BindingStore = ptr::null_mut();

// ── C-compatible types ───────────────────────────────────────────────────────

/// Matches morloc_socket_t from call.h
#[repr(C)]
pub struct MorlocSocket {
    pub lang: *mut c_char,
    pub syscmd: *mut *mut c_char,
    pub socket_filename: *mut c_char,
    pub pid: i32,
}

/// Matches daemon_config_t from daemon.h
#[repr(C)]
pub struct DaemonConfig {
    pub unix_socket_path: *const c_char,
    pub tcp_port: i32,
    pub http_port: i32,
    pub pool_check_fn: Option<unsafe extern "C" fn(*mut MorlocSocket, usize)>,
    pub pool_alive_fn: Option<unsafe extern "C" fn(usize) -> bool>,
    pub n_pools: usize,
    pub eval_timeout: i32,
}

/// Matches daemon_response_t from daemon.h
#[repr(C)]
pub struct DaemonResponse {
    pub id: *mut c_char,
    pub success: bool,
    pub result_json: *mut c_char,
    pub error: *mut c_char,
}

// ── Binding store (replaces linear-probe hash table with HashMap) ────────────

struct BindingEntry {
    hash: u64,
    expr: String,
    #[allow(dead_code)]
    artifact_dir: String,
    type_sig: Option<String>,
    names: Vec<String>,
}

struct BindingStore {
    entries: HashMap<u64, BindingEntry>,
    /// Index from name -> hash for name-based lookup
    name_index: HashMap<String, u64>,
    base_dir: String,
}

impl BindingStore {
    fn new(base_dir: &str) -> Self {
        let _ = std::fs::create_dir_all(base_dir);
        BindingStore {
            entries: HashMap::new(),
            name_index: HashMap::new(),
            base_dir: base_dir.to_string(),
        }
    }

    fn lookup_hash(&self, hash: u64) -> Option<&BindingEntry> {
        self.entries.get(&hash)
    }

    fn lookup_name(&self, name: &str) -> Option<&BindingEntry> {
        let hash = self.name_index.get(name)?;
        self.entries.get(hash)
    }

    fn add_name(&mut self, hash: u64, name: &str) {
        if let Some(entry) = self.entries.get_mut(&hash) {
            if !entry.names.contains(&name.to_string()) {
                entry.names.push(name.to_string());
            }
        }
        self.name_index.insert(name.to_string(), hash);
    }

    fn bind(&mut self, expr: &str, name: Option<&str>, eval_timeout: i32) -> Option<u64> {
        let hv = hash::xxh64_with_seed(expr.as_bytes(), DEFAULT_XXHASH_SEED);

        if self.entries.contains_key(&hv) {
            if let Some(n) = name {
                self.add_name(hv, n);
            }
            return Some(hv);
        }

        let hash_hex = format!("{:016x}", hv);
        let artifact_dir = format!("{}/{}", self.base_dir, hash_hex);

        // Fork morloc eval --save
        unsafe {
            let mut stdout_pipe = [0i32; 2];
            let mut stderr_pipe = [0i32; 2];
            if libc::pipe(stdout_pipe.as_mut_ptr()) != 0
                || libc::pipe(stderr_pipe.as_mut_ptr()) != 0
            {
                return None;
            }

            let pid = libc::fork();
            if pid < 0 {
                libc::close(stdout_pipe[0]);
                libc::close(stdout_pipe[1]);
                libc::close(stderr_pipe[0]);
                libc::close(stderr_pipe[1]);
                return None;
            }

            if pid == 0 {
                // Child
                libc::close(stdout_pipe[0]);
                libc::close(stderr_pipe[0]);
                libc::dup2(stdout_pipe[1], libc::STDOUT_FILENO);
                libc::dup2(stderr_pipe[1], libc::STDERR_FILENO);
                libc::close(stdout_pipe[1]);
                libc::close(stderr_pipe[1]);

                if eval_timeout > 0 {
                    let cpu_limit = libc::rlimit {
                        rlim_cur: eval_timeout as libc::rlim_t,
                        rlim_max: (eval_timeout + 5) as libc::rlim_t,
                    };
                    libc::setrlimit(libc::RLIMIT_CPU, &cpu_limit);
                    let as_limit = libc::rlimit {
                        rlim_cur: 2 * 1024 * 1024 * 1024,
                        rlim_max: 2 * 1024 * 1024 * 1024,
                    };
                    libc::setrlimit(libc::RLIMIT_AS, &as_limit);
                }

                let cmd = CString::new("morloc").unwrap();
                let arg_eval = CString::new("eval").unwrap();
                let arg_save = CString::new("--save").unwrap();
                let arg_hex = CString::new(hash_hex.as_str()).unwrap();
                let arg_expr = CString::new(expr).unwrap();
                libc::execlp(
                    cmd.as_ptr(),
                    cmd.as_ptr(),
                    arg_eval.as_ptr(),
                    arg_save.as_ptr(),
                    arg_hex.as_ptr(),
                    arg_expr.as_ptr(),
                    ptr::null::<c_char>(),
                );
                libc::_exit(127);
            }

            // Parent
            libc::close(stdout_pipe[1]);
            libc::close(stderr_pipe[1]);

            let mut stderr_buf = vec![0u8; 4096];
            let mut stderr_len: usize = 0;
            loop {
                let n = libc::read(
                    stderr_pipe[0],
                    stderr_buf.as_mut_ptr().add(stderr_len) as *mut c_void,
                    stderr_buf.len() - stderr_len - 1,
                );
                if n <= 0 {
                    break;
                }
                stderr_len += n as usize;
            }
            libc::close(stdout_pipe[0]);
            libc::close(stderr_pipe[0]);

            let mut status: i32 = 0;
            libc::waitpid(pid, &mut status, 0);

            if !libc::WIFEXITED(status) || libc::WEXITSTATUS(status) != 0 {
                stderr_buf.truncate(stderr_len);
                let msg = String::from_utf8_lossy(&stderr_buf);
                eprintln!("binding_store_bind: morloc eval --save failed: {}", msg);
                return None;
            }
        }

        let entry = BindingEntry {
            hash: hv,
            expr: expr.to_string(),
            artifact_dir,
            type_sig: None,
            names: Vec::new(),
        };
        self.entries.insert(hv, entry);
        if let Some(n) = name {
            self.add_name(hv, n);
        }

        Some(hv)
    }

    fn list_json(&self) -> String {
        #[derive(serde::Serialize)]
        struct BindingInfo {
            hash: String,
            expr: String,
            #[serde(skip_serializing_if = "Option::is_none")]
            r#type: Option<String>,
            names: Vec<String>,
        }
        #[derive(serde::Serialize)]
        struct BindingsList {
            bindings: Vec<BindingInfo>,
        }
        let bindings: Vec<BindingInfo> = self
            .entries
            .values()
            .map(|e| BindingInfo {
                hash: format!("{:016x}", e.hash),
                expr: e.expr.clone(),
                r#type: e.type_sig.clone(),
                names: e.names.clone(),
            })
            .collect();
        serde_json::to_string(&BindingsList { bindings }).unwrap_or_default()
    }

    fn unbind(&mut self, name: &str) -> bool {
        let hash = match self.name_index.remove(name) {
            Some(h) => h,
            None => return false,
        };
        if let Some(entry) = self.entries.get_mut(&hash) {
            entry.names.retain(|n| n != name);
        }
        true
    }
}

// ── C-exported binding store functions ───────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn binding_store_init(base_dir: *const c_char) -> *mut c_void {
    let dir = CStr::from_ptr(base_dir).to_string_lossy().into_owned();
    let store = Box::new(BindingStore::new(&dir));
    Box::into_raw(store) as *mut c_void
}

#[no_mangle]
pub unsafe extern "C" fn binding_store_free(store: *mut c_void) {
    if !store.is_null() {
        drop(Box::from_raw(store as *mut BindingStore));
    }
}

// ── Request parsing (serde_json) ─────────────────────────────────────────────

#[derive(serde::Deserialize)]
struct JsonRequest {
    id: Option<String>,
    method: Option<String>,
    command: Option<String>,
    args: Option<serde_json::Value>,
    expr: Option<String>,
    name: Option<String>,
}

#[no_mangle]
pub unsafe extern "C" fn daemon_parse_request(
    json: *const c_char,
    len: usize,
    errmsg: *mut *mut c_char,
) -> *mut DaemonRequest {
    clear_errmsg(errmsg);

    let slice = std::slice::from_raw_parts(json as *const u8, len);
    let text = match std::str::from_utf8(slice) {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("Invalid UTF-8 in request".into()));
            return ptr::null_mut();
        }
    };

    let parsed: JsonRequest = match serde_json::from_str(text) {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!("Failed to parse request JSON: {}", e)),
            );
            return ptr::null_mut();
        }
    };

    let req = libc::calloc(1, std::mem::size_of::<DaemonRequest>()) as *mut DaemonRequest;
    if req.is_null() {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to allocate daemon_request_t".into()),
        );
        return ptr::null_mut();
    }

    if let Some(id) = &parsed.id {
        let c = CString::new(id.as_str()).unwrap_or_default();
        (*req).id = libc::strdup(c.as_ptr());
    }

    if let Some(method) = &parsed.method {
        (*req).method = match method.as_str() {
            "call" => DaemonMethod::Call,
            "discover" => DaemonMethod::Discover,
            "health" => DaemonMethod::Health,
            "eval" => DaemonMethod::Eval,
            "typecheck" => DaemonMethod::Typecheck,
            "bind" => DaemonMethod::Bind,
            "bindings" => DaemonMethod::Bindings,
            "unbind" => DaemonMethod::Unbind,
            _ => {
                daemon_free_request(req);
                set_errmsg(
                    errmsg,
                    &MorlocError::Other(format!("Unknown method: {}", method)),
                );
                return ptr::null_mut();
            }
        };
    }

    if let Some(cmd) = &parsed.command {
        let c = CString::new(cmd.as_str()).unwrap_or_default();
        (*req).command = libc::strdup(c.as_ptr());
    }

    if let Some(args) = &parsed.args {
        let args_str = serde_json::to_string(args).unwrap_or_default();
        let c = CString::new(args_str).unwrap_or_default();
        (*req).args_json = libc::strdup(c.as_ptr());
    }

    if let Some(expr) = &parsed.expr {
        let c = CString::new(expr.as_str()).unwrap_or_default();
        (*req).expr = libc::strdup(c.as_ptr());
    }

    if let Some(name) = &parsed.name {
        let c = CString::new(name.as_str()).unwrap_or_default();
        (*req).name = libc::strdup(c.as_ptr());
    }

    req
}

// ── Response parsing (serde_json) ────────────────────────────────────────────

#[derive(serde::Deserialize)]
struct JsonResponse {
    id: Option<String>,
    status: Option<String>,
    result: Option<serde_json::Value>,
    error: Option<String>,
}

#[no_mangle]
pub unsafe extern "C" fn daemon_parse_response(
    json: *const c_char,
    len: usize,
    errmsg: *mut *mut c_char,
) -> *mut DaemonResponse {
    clear_errmsg(errmsg);

    let slice = std::slice::from_raw_parts(json as *const u8, len);
    let text = match std::str::from_utf8(slice) {
        Ok(s) => s,
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("Invalid UTF-8 in response".into()));
            return ptr::null_mut();
        }
    };

    let parsed: JsonResponse = match serde_json::from_str(text) {
        Ok(r) => r,
        Err(e) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!("Failed to parse response JSON: {}", e)),
            );
            return ptr::null_mut();
        }
    };

    let resp = libc::calloc(1, std::mem::size_of::<DaemonResponse>()) as *mut DaemonResponse;
    if resp.is_null() {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to allocate daemon_response_t".into()),
        );
        return ptr::null_mut();
    }

    if let Some(id) = &parsed.id {
        let c = CString::new(id.as_str()).unwrap_or_default();
        (*resp).id = libc::strdup(c.as_ptr());
    }

    (*resp).success = parsed
        .status
        .as_deref()
        .map(|s| s == "ok")
        .unwrap_or(false);

    if let Some(result) = &parsed.result {
        let s = serde_json::to_string(result).unwrap_or_default();
        let c = CString::new(s).unwrap_or_default();
        (*resp).result_json = libc::strdup(c.as_ptr());
    }

    if let Some(error) = &parsed.error {
        let c = CString::new(error.as_str()).unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
    }

    resp
}

// ── Free functions ───────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn daemon_free_request(req: *mut DaemonRequest) {
    if req.is_null() {
        return;
    }
    if !(*req).id.is_null() {
        libc::free((*req).id as *mut c_void);
    }
    if !(*req).command.is_null() {
        libc::free((*req).command as *mut c_void);
    }
    if !(*req).args_json.is_null() {
        libc::free((*req).args_json as *mut c_void);
    }
    if !(*req).expr.is_null() {
        libc::free((*req).expr as *mut c_void);
    }
    if !(*req).name.is_null() {
        libc::free((*req).name as *mut c_void);
    }
    libc::free(req as *mut c_void);
}

#[no_mangle]
pub unsafe extern "C" fn daemon_free_response(resp: *mut DaemonResponse) {
    if resp.is_null() {
        return;
    }
    if !(*resp).id.is_null() {
        libc::free((*resp).id as *mut c_void);
    }
    if !(*resp).result_json.is_null() {
        libc::free((*resp).result_json as *mut c_void);
    }
    if !(*resp).error.is_null() {
        libc::free((*resp).error as *mut c_void);
    }
    libc::free(resp as *mut c_void);
}

// ── Response serialization (serde_json) ──────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn daemon_serialize_response(
    response: *mut DaemonResponse,
    out_len: *mut usize,
) -> *mut c_char {
    let mut map = serde_json::Map::new();

    if !(*response).id.is_null() {
        let id = CStr::from_ptr((*response).id).to_string_lossy();
        map.insert("id".into(), serde_json::Value::String(id.into_owned()));
    }

    map.insert(
        "status".into(),
        serde_json::Value::String(
            if (*response).success { "ok" } else { "error" }.into(),
        ),
    );

    if (*response).success && !(*response).result_json.is_null() {
        let raw = CStr::from_ptr((*response).result_json).to_string_lossy();
        // Try to parse as JSON value; if it fails, store as raw string
        match serde_json::from_str::<serde_json::Value>(&raw) {
            Ok(v) => {
                map.insert("result".into(), v);
            }
            Err(_) => {
                map.insert("result".into(), serde_json::Value::String(raw.into_owned()));
            }
        }
    }

    if !(*response).success && !(*response).error.is_null() {
        let err = CStr::from_ptr((*response).error).to_string_lossy();
        map.insert("error".into(), serde_json::Value::String(err.into_owned()));
    }

    let json_str = serde_json::to_string(&map).unwrap_or_else(|_| "{}".into());
    if !out_len.is_null() {
        *out_len = json_str.len();
    }
    let c = CString::new(json_str).unwrap_or_default();
    libc::strdup(c.as_ptr())
}

// ── Discovery ────────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn daemon_build_discovery(manifest: *mut c_void) -> *mut c_char {
    extern "C" {
        fn manifest_to_discovery_json(manifest: *const c_void) -> *mut c_char;
    }
    manifest_to_discovery_json(manifest)
}

// ── Eval timeout ─────────────────────────────────────────────────────────────

#[no_mangle]
pub extern "C" fn daemon_set_eval_timeout(timeout_sec: i32) {
    let t = if timeout_sec > 0 { timeout_sec } else { 30 };
    G_EVAL_TIMEOUT.store(t, Ordering::Relaxed);
}

// ── Fork-based eval/typecheck ────────────────────────────────────────────────

/// Fork `morloc <subcmd> <expr>`, capture stdout/stderr, return a DaemonResponse.
unsafe fn fork_morloc_command(subcmd: &str, expr: *const c_char) -> *mut DaemonResponse {
    let resp = libc::calloc(1, std::mem::size_of::<DaemonResponse>()) as *mut DaemonResponse;

    let mut stdout_pipe = [0i32; 2];
    let mut stderr_pipe = [0i32; 2];
    if libc::pipe(stdout_pipe.as_mut_ptr()) != 0 || libc::pipe(stderr_pipe.as_mut_ptr()) != 0 {
        (*resp).success = false;
        let c = CString::new(format!("Failed to create pipes for {}", subcmd)).unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
        return resp;
    }

    let pid = libc::fork();
    if pid < 0 {
        (*resp).success = false;
        let c = CString::new(format!("Failed to fork for {}", subcmd)).unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
        libc::close(stdout_pipe[0]);
        libc::close(stdout_pipe[1]);
        libc::close(stderr_pipe[0]);
        libc::close(stderr_pipe[1]);
        return resp;
    }

    if pid == 0 {
        // Child
        libc::close(stdout_pipe[0]);
        libc::close(stderr_pipe[0]);
        libc::dup2(stdout_pipe[1], libc::STDOUT_FILENO);
        libc::dup2(stderr_pipe[1], libc::STDERR_FILENO);
        libc::close(stdout_pipe[1]);
        libc::close(stderr_pipe[1]);

        let timeout = G_EVAL_TIMEOUT.load(Ordering::Relaxed);
        if timeout > 0 {
            let cpu_limit = libc::rlimit {
                rlim_cur: timeout as libc::rlim_t,
                rlim_max: (timeout + 5) as libc::rlim_t,
            };
            libc::setrlimit(libc::RLIMIT_CPU, &cpu_limit);
            let as_limit = libc::rlimit {
                rlim_cur: 2 * 1024 * 1024 * 1024,
                rlim_max: 2 * 1024 * 1024 * 1024,
            };
            libc::setrlimit(libc::RLIMIT_AS, &as_limit);
        }

        let cmd = CString::new("morloc").unwrap();
        let arg_subcmd = CString::new(subcmd).unwrap();
        libc::execlp(
            cmd.as_ptr(),
            cmd.as_ptr(),
            arg_subcmd.as_ptr(),
            expr,
            ptr::null::<c_char>(),
        );
        libc::_exit(127);
    }

    // Parent
    libc::close(stdout_pipe[1]);
    libc::close(stderr_pipe[1]);

    let stdout_buf = read_fd_to_vec(stdout_pipe[0]);
    libc::close(stdout_pipe[0]);
    let stderr_buf = read_fd_to_vec(stderr_pipe[0]);
    libc::close(stderr_pipe[0]);

    let mut status: i32 = 0;
    libc::waitpid(pid, &mut status, 0);

    if libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0 {
        let mut out = String::from_utf8_lossy(&stdout_buf).into_owned();
        // Trim trailing newlines
        while out.ends_with('\n') || out.ends_with('\r') {
            out.pop();
        }
        (*resp).success = true;
        let c = CString::new(out).unwrap_or_default();
        (*resp).result_json = libc::strdup(c.as_ptr());
    } else {
        (*resp).success = false;
        let errmsg = if !stderr_buf.is_empty() {
            String::from_utf8_lossy(&stderr_buf).into_owned()
        } else if libc::WIFSIGNALED(status) {
            format!("morloc {} killed by signal {}", subcmd, libc::WTERMSIG(status))
        } else {
            let code = if libc::WIFEXITED(status) {
                libc::WEXITSTATUS(status)
            } else {
                -1
            };
            format!("morloc {} exited with code {}", subcmd, code)
        };
        let c = CString::new(errmsg).unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
    }

    resp
}

/// Read an fd to completion into a Vec<u8>.
unsafe fn read_fd_to_vec(fd: i32) -> Vec<u8> {
    let mut buf = Vec::with_capacity(65536);
    let mut tmp = [0u8; 8192];
    loop {
        let n = libc::read(fd, tmp.as_mut_ptr() as *mut c_void, tmp.len());
        if n <= 0 {
            break;
        }
        buf.extend_from_slice(&tmp[..n as usize]);
    }
    buf
}

// ── Dispatch ─────────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn daemon_dispatch(
    manifest: *mut c_void,
    request: *mut DaemonRequest,
    sockets: *mut MorlocSocket,
    _shm_basename: *const c_char,
) -> *mut DaemonResponse {
    let resp = libc::calloc(1, std::mem::size_of::<DaemonResponse>()) as *mut DaemonResponse;

    // Echo request id
    if !(*request).id.is_null() {
        (*resp).id = libc::strdup((*request).id);
    }

    match (*request).method {
        DaemonMethod::Health => {
            (*resp).success = true;
            if let Some(alive_fn) = G_POOL_ALIVE_FN {
                let mut arr = Vec::with_capacity(G_N_POOLS);
                for i in 0..G_N_POOLS {
                    arr.push(serde_json::Value::Bool(alive_fn(i)));
                }
                let json = serde_json::to_string(&arr).unwrap_or_default();
                let c = CString::new(json).unwrap_or_default();
                (*resp).result_json = libc::strdup(c.as_ptr());
            }
            return resp;
        }
        DaemonMethod::Discover => {
            (*resp).success = true;
            (*resp).result_json = daemon_build_discovery(manifest);
            return resp;
        }
        DaemonMethod::Eval => {
            if (*request).expr.is_null() {
                (*resp).success = false;
                let c = CString::new("Missing 'expr' field in eval request").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }

            // Check binding store for cached expression
            if !G_BINDING_STORE.is_null() {
                let expr_str = CStr::from_ptr((*request).expr).to_string_lossy();
                let store = &*G_BINDING_STORE;
                let hv = hash::xxh64_with_seed(expr_str.as_bytes(), DEFAULT_XXHASH_SEED);
                let _cached = store
                    .lookup_hash(hv)
                    .or_else(|| store.lookup_name(&expr_str));
                // TODO: direct binary execution for bound functions
            }

            let eval_resp = fork_morloc_command("eval", (*request).expr);
            if !(*request).id.is_null() {
                (*eval_resp).id = libc::strdup((*request).id);
            }
            libc::free(resp as *mut c_void);
            return eval_resp;
        }
        DaemonMethod::Typecheck => {
            if (*request).expr.is_null() {
                (*resp).success = false;
                let c = CString::new("Missing 'expr' field in typecheck request").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
            let tc_resp = fork_morloc_command("typecheck", (*request).expr);
            if !(*request).id.is_null() {
                (*tc_resp).id = libc::strdup((*request).id);
            }
            libc::free(resp as *mut c_void);
            return tc_resp;
        }
        DaemonMethod::Bind => {
            if (*request).expr.is_null() {
                (*resp).success = false;
                let c = CString::new("Missing 'expr' field in bind request").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
            if G_BINDING_STORE.is_null() {
                (*resp).success = false;
                let c = CString::new("Binding store not initialized").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
            let store = &mut *G_BINDING_STORE;
            let expr_str = CStr::from_ptr((*request).expr).to_string_lossy().into_owned();
            let name = if (*request).name.is_null() {
                None
            } else {
                Some(CStr::from_ptr((*request).name).to_string_lossy().into_owned())
            };
            let timeout = G_EVAL_TIMEOUT.load(Ordering::Relaxed);
            match store.bind(&expr_str, name.as_deref(), timeout) {
                Some(hv) => {
                    let mut map = serde_json::Map::new();
                    map.insert(
                        "hash".into(),
                        serde_json::Value::String(format!("{:016x}", hv)),
                    );
                    map.insert("expr".into(), serde_json::Value::String(expr_str));
                    if let Some(n) = &name {
                        map.insert("name".into(), serde_json::Value::String(n.clone()));
                    }
                    if let Some(entry) = store.lookup_hash(hv) {
                        if let Some(ref ts) = entry.type_sig {
                            map.insert("type".into(), serde_json::Value::String(ts.clone()));
                        }
                    }
                    let json = serde_json::to_string(&map).unwrap_or_default();
                    (*resp).success = true;
                    let c = CString::new(json).unwrap_or_default();
                    (*resp).result_json = libc::strdup(c.as_ptr());
                }
                None => {
                    (*resp).success = false;
                    let c =
                        CString::new("Failed to compile and bind expression").unwrap_or_default();
                    (*resp).error = libc::strdup(c.as_ptr());
                }
            }
            return resp;
        }
        DaemonMethod::Bindings => {
            (*resp).success = true;
            if G_BINDING_STORE.is_null() {
                let c = CString::new("{\"bindings\":[]}").unwrap();
                (*resp).result_json = libc::strdup(c.as_ptr());
            } else {
                let store = &*G_BINDING_STORE;
                let json = store.list_json();
                let c = CString::new(json).unwrap_or_default();
                (*resp).result_json = libc::strdup(c.as_ptr());
            }
            return resp;
        }
        DaemonMethod::Unbind => {
            let name_ptr = if !(*request).command.is_null() {
                (*request).command
            } else {
                (*request).name
            };
            if name_ptr.is_null() {
                (*resp).success = false;
                let c = CString::new("Missing binding name").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
            if G_BINDING_STORE.is_null() {
                (*resp).success = false;
                let c = CString::new("Binding store not initialized").unwrap();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
            let store = &mut *G_BINDING_STORE;
            let name = CStr::from_ptr(name_ptr).to_string_lossy();
            if store.unbind(&name) {
                (*resp).success = true;
                let c = CString::new("{\"removed\":true}").unwrap();
                (*resp).result_json = libc::strdup(c.as_ptr());
            } else {
                (*resp).success = false;
                let c = CString::new(format!("Binding not found: {}", name)).unwrap_or_default();
                (*resp).error = libc::strdup(c.as_ptr());
            }
            return resp;
        }
        DaemonMethod::Call => {
            // Fall through to call dispatch below
        }
    }

    // DAEMON_CALL
    if (*request).command.is_null() {
        (*resp).success = false;
        let c = CString::new("Missing 'command' field in call request").unwrap();
        (*resp).error = libc::strdup(c.as_ptr());
        return resp;
    }

    // Delegate to the C functions that handle manifest lookup, arg parsing,
    // schema handling, and pool communication. These are all already ported
    // to Rust in other _ffi modules, so we declare them as extern "C".
    extern "C" {
        fn parse_schema(schema: *const c_char, errmsg: *mut *mut c_char) -> *mut CSchema;
        fn free_schema(schema: *mut CSchema);
        fn initialize_positional(value: *mut c_char) -> *mut c_void;
        fn free_argument_t(arg: *mut c_void);
        fn parse_cli_data_argument(
            dest: *mut u8,
            arg: *const c_void,
            schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn make_call_packet_from_cli(
            dest: *mut u8,
            mid: u32,
            args: *mut *mut c_void,
            arg_schema_strs: *mut *mut c_char,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn send_and_receive_over_socket(
            socket_path: *const c_char,
            packet: *const u8,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn get_morloc_data_packet_error_message(
            data: *const u8,
            errmsg: *mut *mut c_char,
        ) -> *mut c_char;
        fn get_morloc_data_packet_value(
            data: *const u8,
            schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn voidstar_to_json_string(
            data: *const c_void,
            schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut c_char;
        fn morloc_eval(
            expr: *mut c_void,  // actually *mut MorlocExpression
            return_schema: *mut CSchema,
            arg_voidstar: *mut *mut u8,
            arg_schemas: *mut *mut CSchema,
            nargs: usize,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }

    // Access manifest fields through opaque C struct pointers.
    // We do NOT import the Manifest type from manifest_ffi.rs directly;
    // instead we use the same layout as the C code expects.
    //
    // manifest_t layout (from manifest.h):
    //   int version;
    //   char* name;
    //   char* build_dir;
    //   manifest_pool_t* pools;
    //   size_t n_pools;
    //   manifest_command_t* commands;
    //   size_t n_commands;
    //   ...

    // We need to find the command. Since we treat manifest as opaque,
    // we cast to access the fields we need.
    #[repr(C)]
    struct ManifestCommandView {
        name: *mut c_char,
        is_pure: bool,
        mid: u32,
        pool_index: usize,
        needed_pools: *mut usize,
        n_needed_pools: usize,
        arg_schemas: *mut *mut c_char,
        return_schema: *mut c_char,
        desc: *mut *mut c_char,
        return_type: *mut c_char,
        return_desc: *mut *mut c_char,
        args: *mut c_void, // manifest_arg_t*
        n_args: usize,
        expr: *mut c_void, // morloc_expression_t*
        group: *mut c_char,
    }

    // We need to know the size of each manifest_arg entry's kind field
    // to count positional args.
    #[repr(C)]
    #[derive(Clone, Copy, PartialEq)]
    #[allow(dead_code)]
    enum ManifestArgKind {
        Pos = 0,
        Opt = 1,
        Flag = 2,
        Grp = 3,
    }

    #[repr(C)]
    struct ManifestView {
        version: i32,
        name: *mut c_char,
        build_dir: *mut c_char,
        pools: *mut c_void,
        n_pools: usize,
        commands: *mut ManifestCommandView,
        n_commands: usize,
    }

    let mv = manifest as *const ManifestView;
    let command_name = CStr::from_ptr((*request).command);
    let mut cmd: *const ManifestCommandView = ptr::null();
    for i in 0..(*mv).n_commands {
        let c = &*(*mv).commands.add(i);
        if CStr::from_ptr(c.name) == command_name {
            cmd = c;
            break;
        }
    }

    if cmd.is_null() {
        (*resp).success = false;
        let msg = format!(
            "Unknown command: {}",
            command_name.to_string_lossy()
        );
        let c = CString::new(msg).unwrap_or_default();
        (*resp).error = libc::strdup(c.as_ptr());
        return resp;
    }

    let cmd = &*cmd;
    let expected_nargs = cmd.n_args;

    // Parse JSON args into argument_t** array
    let mut err: *mut c_char = ptr::null_mut();
    let args: *mut *mut c_void;

    if !(*request).args_json.is_null() {
        // Parse the JSON array
        let args_str = CStr::from_ptr((*request).args_json).to_string_lossy();
        let parsed_args: Vec<serde_json::Value> = match serde_json::from_str(&args_str) {
            Ok(v) => v,
            Err(e) => {
                (*resp).success = false;
                let c = CString::new(format!("Failed to parse args: {}", e)).unwrap_or_default();
                (*resp).error = libc::strdup(c.as_ptr());
                return resp;
            }
        };

        if parsed_args.len() != expected_nargs {
            (*resp).success = false;
            let c = CString::new(format!(
                "Expected {} arguments, got {}",
                expected_nargs,
                parsed_args.len()
            ))
            .unwrap_or_default();
            (*resp).error = libc::strdup(c.as_ptr());
            return resp;
        }

        args = libc::calloc(expected_nargs + 1, std::mem::size_of::<*mut c_void>())
            as *mut *mut c_void;
        for (i, val) in parsed_args.iter().enumerate() {
            let val_str = match val {
                serde_json::Value::String(s) => format!("\"{}\"", s),
                other => other.to_string(),
            };
            let c = CString::new(val_str).unwrap_or_default();
            let dup = libc::strdup(c.as_ptr());
            *args.add(i) = initialize_positional(dup);
            libc::free(dup as *mut c_void);
        }
        *args.add(expected_nargs) = ptr::null_mut();
    } else {
        if expected_nargs > 0 {
            // Check if any are positional (required)
            // For simplicity, match the C behavior: require args if n_args > 0
            (*resp).success = false;
            let c = CString::new("Missing 'args' field in call request").unwrap();
            (*resp).error = libc::strdup(c.as_ptr());
            return resp;
        }
        args =
            libc::calloc(1, std::mem::size_of::<*mut c_void>()) as *mut *mut c_void;
        *args = ptr::null_mut();
    }

    if cmd.is_pure {
        // Pure command: evaluate expression tree
        let mut nargs: usize = 0;
        while !(*args.add(nargs)).is_null() {
            nargs += 1;
        }

        let arg_schemas_arr =
            libc::calloc(nargs, std::mem::size_of::<*mut CSchema>()) as *mut *mut CSchema;
        let arg_packets =
            libc::calloc(nargs, std::mem::size_of::<*mut u8>()) as *mut *mut u8;
        let arg_voidstars =
            libc::calloc(nargs, std::mem::size_of::<*mut u8>()) as *mut *mut u8;

        let mut cleanup_and_fail = false;

        for i in 0..nargs {
            *arg_schemas_arr.add(i) = parse_schema(*cmd.arg_schemas.add(i), &mut err);
            if !err.is_null() {
                (*resp).success = false;
                (*resp).error = err;
                cleanup_and_fail = true;
                break;
            }

            *arg_packets.add(i) = parse_cli_data_argument(
                ptr::null_mut(),
                *args.add(i),
                *arg_schemas_arr.add(i),
                &mut err,
            );
            if !err.is_null() {
                (*resp).success = false;
                (*resp).error = err;
                cleanup_and_fail = true;
                break;
            }

            *arg_voidstars.add(i) = get_morloc_data_packet_value(
                *arg_packets.add(i),
                *arg_schemas_arr.add(i),
                &mut err,
            );
            if !err.is_null() {
                (*resp).success = false;
                (*resp).error = err;
                cleanup_and_fail = true;
                break;
            }
        }

        if !cleanup_and_fail {
            let return_schema = parse_schema(cmd.return_schema, &mut err);
            if !err.is_null() {
                (*resp).success = false;
                (*resp).error = err;
            } else {
                let result_abs = morloc_eval(
                    cmd.expr,
                    return_schema,
                    arg_voidstars,
                    arg_schemas_arr,
                    nargs,
                    &mut err,
                );
                if !err.is_null() {
                    (*resp).success = false;
                    (*resp).error = err;
                } else {
                    let json = voidstar_to_json_string(
                        result_abs as *const c_void,
                        return_schema as *const CSchema,
                        &mut err,
                    );
                    if !err.is_null() {
                        (*resp).success = false;
                        (*resp).error = err;
                    } else {
                        (*resp).success = true;
                        (*resp).result_json = json;
                    }
                }
                free_schema(return_schema);
            }
        }

        // Cleanup
        for i in 0..nargs {
            let s = *arg_schemas_arr.add(i);
            if !s.is_null() {
                free_schema(s);
            }
            let p = *arg_packets.add(i);
            if !p.is_null() {
                libc::free(p as *mut c_void);
            }
        }
        libc::free(arg_schemas_arr as *mut c_void);
        libc::free(arg_packets as *mut c_void);
        libc::free(arg_voidstars as *mut c_void);
    } else {
        // Remote command: send call packet to pool
        let call_packet = make_call_packet_from_cli(
            ptr::null_mut(),
            cmd.mid,
            args,
            cmd.arg_schemas,
            &mut err,
        );
        if !err.is_null() {
            (*resp).success = false;
            (*resp).error = err;
        } else {
            let socket_path = (*sockets.add(cmd.pool_index)).socket_filename;
            let result_packet =
                send_and_receive_over_socket(socket_path, call_packet, &mut err);
            libc::free(call_packet as *mut c_void);

            if !err.is_null() {
                (*resp).success = false;
                (*resp).error = err;
            } else {
                let packet_error =
                    get_morloc_data_packet_error_message(result_packet, &mut err);
                if !packet_error.is_null() {
                    (*resp).success = false;
                    (*resp).error = libc::strdup(packet_error);
                    libc::free(result_packet as *mut c_void);
                } else if !err.is_null() {
                    (*resp).success = false;
                    (*resp).error = err;
                    libc::free(result_packet as *mut c_void);
                } else {
                    let return_schema = parse_schema(cmd.return_schema, &mut err);
                    if !err.is_null() {
                        (*resp).success = false;
                        (*resp).error = err;
                        libc::free(result_packet as *mut c_void);
                    } else {
                        let packet_value = get_morloc_data_packet_value(
                            result_packet,
                            return_schema as *const CSchema,
                            &mut err,
                        );
                        if !err.is_null() {
                            (*resp).success = false;
                            (*resp).error = err;
                        } else {
                            let json = voidstar_to_json_string(
                                packet_value as *const c_void,
                                return_schema as *const CSchema,
                                &mut err,
                            );
                            if !err.is_null() {
                                (*resp).success = false;
                                (*resp).error = err;
                            } else {
                                (*resp).success = true;
                                (*resp).result_json = json;
                            }
                        }
                        free_schema(return_schema);
                        libc::free(result_packet as *mut c_void);
                    }
                }
            }
        }
    }

    // Free args
    let mut i = 0;
    while !(*args.add(i)).is_null() {
        free_argument_t(*args.add(i));
        i += 1;
    }
    libc::free(args as *mut c_void);

    resp
}

// ── Length-prefixed message protocol ─────────────────────────────────────────

unsafe fn read_lp_message(
    fd: i32,
    out_len: *mut usize,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);

    let mut len_buf = [0u8; 4];
    let n = libc::recv(
        fd,
        len_buf.as_mut_ptr() as *mut c_void,
        4,
        libc::MSG_WAITALL,
    );
    if n != 4 {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to read message length prefix".into()),
        );
        return ptr::null_mut();
    }

    let msg_len = ((len_buf[0] as u32) << 24)
        | ((len_buf[1] as u32) << 16)
        | ((len_buf[2] as u32) << 8)
        | (len_buf[3] as u32);

    if msg_len > MAX_LP_MESSAGE {
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!("Message too large: {} bytes", msg_len)),
        );
        return ptr::null_mut();
    }

    let msg = libc::malloc(msg_len as usize + 1) as *mut c_char;
    if msg.is_null() {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to allocate message buffer".into()),
        );
        return ptr::null_mut();
    }

    let mut total: usize = 0;
    while total < msg_len as usize {
        let n = libc::recv(
            fd,
            msg.add(total) as *mut c_void,
            msg_len as usize - total,
            0,
        );
        if n <= 0 {
            libc::free(msg as *mut c_void);
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!(
                    "Failed to read message body (got {} of {} bytes)",
                    total, msg_len
                )),
            );
            return ptr::null_mut();
        }
        total += n as usize;
    }
    *msg.add(msg_len as usize) = 0;

    if !out_len.is_null() {
        *out_len = msg_len as usize;
    }
    msg
}

unsafe fn write_lp_message(
    fd: i32,
    data: *const c_char,
    len: usize,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);

    let len_buf: [u8; 4] = [
        ((len >> 24) & 0xFF) as u8,
        ((len >> 16) & 0xFF) as u8,
        ((len >> 8) & 0xFF) as u8,
        (len & 0xFF) as u8,
    ];

    let n = libc::send(
        fd,
        len_buf.as_ptr() as *const c_void,
        4,
        libc::MSG_NOSIGNAL,
    );
    if n != 4 {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to write message length prefix".into()),
        );
        return false;
    }

    let mut total: usize = 0;
    while total < len {
        let n = libc::send(
            fd,
            (data as *const u8).add(total) as *const c_void,
            len - total,
            libc::MSG_NOSIGNAL,
        );
        if n <= 0 {
            set_errmsg(
                errmsg,
                &MorlocError::Other("Failed to write message body".into()),
            );
            return false;
        }
        total += n as usize;
    }

    true
}

// ── Connection handlers ──────────────────────────────────────────────────────

unsafe fn handle_lp_connection(
    client_fd: i32,
    manifest: *mut c_void,
    sockets: *mut MorlocSocket,
    shm_basename: *const c_char,
) {
    let mut errmsg: *mut c_char = ptr::null_mut();
    let mut msg_len: usize = 0;

    let msg = read_lp_message(client_fd, &mut msg_len, &mut errmsg);
    if !errmsg.is_null() {
        let err_str = CStr::from_ptr(errmsg).to_string_lossy();
        eprintln!("daemon: read error: {}", err_str);
        libc::free(errmsg as *mut c_void);
        libc::close(client_fd);
        return;
    }

    let req = daemon_parse_request(msg, msg_len, &mut errmsg);
    libc::free(msg as *mut c_void);
    if !errmsg.is_null() {
        let mut err_resp: DaemonResponse = std::mem::zeroed();
        err_resp.success = false;
        err_resp.error = errmsg;
        let mut resp_len: usize = 0;
        let resp_json = daemon_serialize_response(&mut err_resp, &mut resp_len);
        let mut write_err: *mut c_char = ptr::null_mut();
        write_lp_message(client_fd, resp_json, resp_len, &mut write_err);
        libc::free(resp_json as *mut c_void);
        if !write_err.is_null() {
            libc::free(write_err as *mut c_void);
        }
        libc::free(errmsg as *mut c_void);
        libc::close(client_fd);
        return;
    }

    let resp = daemon_dispatch(manifest, req, sockets, shm_basename);

    let mut resp_len: usize = 0;
    let resp_json = daemon_serialize_response(resp, &mut resp_len);

    let mut write_err: *mut c_char = ptr::null_mut();
    write_lp_message(client_fd, resp_json, resp_len, &mut write_err);
    if !write_err.is_null() {
        let err_str = CStr::from_ptr(write_err).to_string_lossy();
        eprintln!("daemon: write error: {}", err_str);
        libc::free(write_err as *mut c_void);
    }

    libc::free(resp_json as *mut c_void);
    daemon_free_request(req);
    daemon_free_response(resp);
    libc::close(client_fd);
}

unsafe fn handle_http_connection(
    client_fd: i32,
    manifest: *mut c_void,
    sockets: *mut MorlocSocket,
    shm_basename: *const c_char,
) {
    extern "C" {
        fn http_parse_request(fd: i32, errmsg: *mut *mut c_char) -> *mut HttpRequest;
        fn http_free_request(req: *mut HttpRequest);
        fn http_write_response(
            fd: i32,
            status: i32,
            content_type: *const c_char,
            body: *const c_char,
            body_len: usize,
        ) -> bool;
        fn http_to_daemon_request(
            req: *mut HttpRequest,
            errmsg: *mut *mut c_char,
        ) -> *mut DaemonRequest;
    }

    let mut errmsg: *mut c_char = ptr::null_mut();
    let http_req = http_parse_request(client_fd, &mut errmsg);
    if !errmsg.is_null() {
        let body = b"{\"status\":\"error\",\"error\":\"Bad request\"}\0";
        let ct = b"application/json\0";
        http_write_response(
            client_fd,
            400,
            ct.as_ptr() as *const c_char,
            body.as_ptr() as *const c_char,
            body.len() - 1,
        );
        libc::free(errmsg as *mut c_void);
        libc::close(client_fd);
        return;
    }

    let req = http_to_daemon_request(http_req, &mut errmsg);
    if !errmsg.is_null() {
        let body = b"{\"status\":\"error\",\"error\":\"Invalid request\"}\0";
        let ct = b"application/json\0";
        http_write_response(
            client_fd,
            400,
            ct.as_ptr() as *const c_char,
            body.as_ptr() as *const c_char,
            body.len() - 1,
        );
        http_free_request(http_req);
        libc::free(errmsg as *mut c_void);
        libc::close(client_fd);
        return;
    }
    http_free_request(http_req);

    let resp = daemon_dispatch(manifest, req, sockets, shm_basename);

    let mut resp_len: usize = 0;
    let resp_json = daemon_serialize_response(resp, &mut resp_len);

    // Append newline for terminal-friendly output
    let resp_body = libc::malloc(resp_len + 2) as *mut u8;
    ptr::copy_nonoverlapping(resp_json as *const u8, resp_body, resp_len);
    *resp_body.add(resp_len) = b'\n';
    *resp_body.add(resp_len + 1) = 0;

    let status = if (*resp).success { 200 } else { 500 };
    let ct = b"application/json\0";
    http_write_response(
        client_fd,
        status,
        ct.as_ptr() as *const c_char,
        resp_body as *const c_char,
        resp_len + 1,
    );

    libc::free(resp_body as *mut c_void);
    libc::free(resp_json as *mut c_void);
    daemon_free_request(req);
    daemon_free_response(resp);
    libc::close(client_fd);
}

// ── Thread pool (VecDeque + Condvar instead of linked list + pthread) ────────

#[derive(Clone, Copy)]
struct DaemonJob {
    client_fd: i32,
    conn_type: i32, // 0 = length-prefixed (unix/tcp), 2 = http
}

struct JobQueue {
    jobs: VecDeque<DaemonJob>,
}

struct WorkerContext {
    queue: Mutex<JobQueue>,
    cond: Condvar,
    manifest: *mut c_void,
    sockets: *mut MorlocSocket,
    shm_basename: *const c_char,
}

// SAFETY: WorkerContext is shared between threads but all raw pointers
// within it point to read-only or thread-safe C data.
unsafe impl Send for WorkerContext {}
unsafe impl Sync for WorkerContext {}

fn set_socket_timeouts(fd: i32, timeout_sec: i32) {
    unsafe {
        let tv = libc::timeval {
            tv_sec: timeout_sec as _,
            tv_usec: 0,
        };
        libc::setsockopt(
            fd,
            libc::SOL_SOCKET,
            libc::SO_RCVTIMEO,
            &tv as *const libc::timeval as *const c_void,
            std::mem::size_of::<libc::timeval>() as libc::socklen_t,
        );
        libc::setsockopt(
            fd,
            libc::SOL_SOCKET,
            libc::SO_SNDTIMEO,
            &tv as *const libc::timeval as *const c_void,
            std::mem::size_of::<libc::timeval>() as libc::socklen_t,
        );
    }
}

// ── Main daemon event loop ───────────────────────────────────────────────────

const MAX_LISTENERS: usize = 3;

#[no_mangle]
pub unsafe extern "C" fn daemon_run(
    config: *mut DaemonConfig,
    manifest: *mut c_void,
    sockets: *mut MorlocSocket,
    n_pools: usize,
    shm_basename: *const c_char,
) {
    // Set globals
    G_POOL_ALIVE_FN = (*config).pool_alive_fn;
    G_N_POOLS = n_pools;
    let timeout = if (*config).eval_timeout > 0 {
        (*config).eval_timeout
    } else {
        30
    };
    G_EVAL_TIMEOUT.store(timeout, Ordering::Relaxed);

    // Initialize binding store
    if G_BINDING_STORE.is_null() {
        let store = Box::new(BindingStore::new("/tmp/morloc-bindings"));
        G_BINDING_STORE = Box::into_raw(store);
    }

    // Install signal handlers
    SHUTDOWN_REQUESTED.store(false, Ordering::Relaxed);
    let handler: libc::sighandler_t =
        std::mem::transmute::<extern "C" fn(i32), libc::sighandler_t>(daemon_signal_handler_fn);
    libc::signal(libc::SIGTERM, handler);
    libc::signal(libc::SIGINT, handler);

    let mut fds = [libc::pollfd {
        fd: -1,
        events: 0,
        revents: 0,
    }; MAX_LISTENERS];
    let mut fd_types = [0i32; MAX_LISTENERS]; // 0=unix, 1=tcp, 2=http
    let mut nfds: usize = 0;

    // Unix socket
    if !(*config).unix_socket_path.is_null() {
        let sock_fd = libc::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
        if sock_fd < 0 {
            eprintln!("daemon: failed to create unix socket");
            return;
        }
        let mut addr: libc::sockaddr_un = std::mem::zeroed();
        addr.sun_family = libc::AF_UNIX as libc::sa_family_t;
        let path_bytes = CStr::from_ptr((*config).unix_socket_path).to_bytes();
        let copy_len = path_bytes.len().min(addr.sun_path.len() - 1);
        ptr::copy_nonoverlapping(
            path_bytes.as_ptr() as *const c_char,
            addr.sun_path.as_mut_ptr(),
            copy_len,
        );
        libc::unlink((*config).unix_socket_path);
        if libc::bind(
            sock_fd,
            &addr as *const libc::sockaddr_un as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_un>() as libc::socklen_t,
        ) < 0
        {
            eprintln!("daemon: failed to bind unix socket");
            libc::close(sock_fd);
            return;
        }
        libc::listen(sock_fd, 64);
        fds[nfds].fd = sock_fd;
        fds[nfds].events = libc::POLLIN as i16;
        fd_types[nfds] = 0;
        nfds += 1;
    }

    // TCP
    if (*config).tcp_port > 0 {
        let tcp_fd = libc::socket(libc::AF_INET, libc::SOCK_STREAM, 0);
        if tcp_fd < 0 {
            eprintln!("daemon: failed to create tcp socket");
            return;
        }
        let opt: i32 = 1;
        libc::setsockopt(
            tcp_fd,
            libc::SOL_SOCKET,
            libc::SO_REUSEADDR,
            &opt as *const i32 as *const c_void,
            std::mem::size_of::<i32>() as libc::socklen_t,
        );
        let mut addr: libc::sockaddr_in = std::mem::zeroed();
        addr.sin_family = libc::AF_INET as libc::sa_family_t;
        addr.sin_addr.s_addr = u32::from_be(0x7f000001); // INADDR_LOOPBACK
        addr.sin_port = ((*config).tcp_port as u16).to_be();
        if libc::bind(
            tcp_fd,
            &addr as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        ) < 0
        {
            eprintln!(
                "daemon: failed to bind tcp port {}",
                (*config).tcp_port
            );
            libc::close(tcp_fd);
            return;
        }
        libc::listen(tcp_fd, 64);
        fds[nfds].fd = tcp_fd;
        fds[nfds].events = libc::POLLIN as i16;
        fd_types[nfds] = 1;
        nfds += 1;
    }

    // HTTP
    if (*config).http_port > 0 {
        let http_fd = libc::socket(libc::AF_INET, libc::SOCK_STREAM, 0);
        if http_fd < 0 {
            eprintln!("daemon: failed to create http socket");
            return;
        }
        let opt: i32 = 1;
        libc::setsockopt(
            http_fd,
            libc::SOL_SOCKET,
            libc::SO_REUSEADDR,
            &opt as *const i32 as *const c_void,
            std::mem::size_of::<i32>() as libc::socklen_t,
        );
        let mut addr: libc::sockaddr_in = std::mem::zeroed();
        addr.sin_family = libc::AF_INET as libc::sa_family_t;
        addr.sin_addr.s_addr = u32::from_be(0x7f000001);
        addr.sin_port = ((*config).http_port as u16).to_be();
        if libc::bind(
            http_fd,
            &addr as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        ) < 0
        {
            eprintln!(
                "daemon: failed to bind http port {}",
                (*config).http_port
            );
            libc::close(http_fd);
            return;
        }
        libc::listen(http_fd, 64);
        fds[nfds].fd = http_fd;
        fds[nfds].events = libc::POLLIN as i16;
        fd_types[nfds] = 2;
        nfds += 1;
    }

    if nfds == 0 {
        eprintln!("daemon: no listeners configured, exiting");
        return;
    }

    // Start worker thread pool
    let ctx = Arc::new(WorkerContext {
        queue: Mutex::new(JobQueue {
            jobs: VecDeque::new(),
        }),
        cond: Condvar::new(),
        manifest,
        sockets,
        shm_basename,
    });

    let n_workers = n_pools.saturating_add(4).clamp(4, 32);
    let mut workers = Vec::with_capacity(n_workers);
    for _ in 0..n_workers {
        let ctx = Arc::clone(&ctx);
        workers.push(std::thread::spawn(move || {
            daemon_worker_fn(ctx);
        }));
    }

    // Main event loop
    while !SHUTDOWN_REQUESTED.load(Ordering::Relaxed) {
        let ready = libc::poll(fds.as_mut_ptr(), nfds as libc::nfds_t, 1000);
        if ready < 0 {
            if crate::utility::errno_val() == libc::EINTR {
                continue;
            }
            eprintln!("daemon: poll error");
            break;
        }

        // Check and restart crashed pools
        if let Some(check_fn) = (*config).pool_check_fn {
            check_fn(sockets, n_pools);
        }

        if ready == 0 {
            continue;
        }

        for i in 0..nfds {
            if fds[i].revents & libc::POLLIN as i16 == 0 {
                continue;
            }
            let client_fd = libc::accept(fds[i].fd, ptr::null_mut(), ptr::null_mut());
            if client_fd < 0 {
                if crate::utility::errno_val() == libc::EINTR
                    || crate::utility::errno_val() == libc::EAGAIN
                {
                    continue;
                }
                eprintln!("daemon: accept error");
                continue;
            }
            set_socket_timeouts(client_fd, 30);

            let job = DaemonJob {
                client_fd,
                conn_type: fd_types[i],
            };
            let mut q = ctx.queue.lock().unwrap();
            q.jobs.push_back(job);
            ctx.cond.notify_one();
        }
    }

    // Wake all workers and join
    ctx.cond.notify_all();
    for w in workers {
        let _ = w.join();
    }

    // Drain remaining jobs
    {
        let mut q = ctx.queue.lock().unwrap();
        while let Some(job) = q.jobs.pop_front() {
            libc::close(job.client_fd);
        }
    }

    // Close listener sockets
    for i in 0..nfds {
        libc::close(fds[i].fd);
    }

    if !(*config).unix_socket_path.is_null() {
        libc::unlink((*config).unix_socket_path);
    }
}

fn daemon_worker_fn(ctx: Arc<WorkerContext>) {
    loop {
        if SHUTDOWN_REQUESTED.load(Ordering::Relaxed) {
            break;
        }

        let job = {
            let mut q = ctx.queue.lock().unwrap();
            loop {
                if let Some(job) = q.jobs.pop_front() {
                    break Some(job);
                }
                if SHUTDOWN_REQUESTED.load(Ordering::Relaxed) {
                    break None;
                }
                // Wait with timeout so we recheck shutdown
                let (guard, _timeout) = ctx
                    .cond
                    .wait_timeout(q, std::time::Duration::from_millis(100))
                    .unwrap();
                q = guard;
            }
        };

        let job = match job {
            Some(j) => j,
            None => continue,
        };

        unsafe {
            if job.conn_type == 2 {
                handle_http_connection(
                    job.client_fd,
                    ctx.manifest,
                    ctx.sockets,
                    ctx.shm_basename,
                );
            } else {
                handle_lp_connection(
                    job.client_fd,
                    ctx.manifest,
                    ctx.sockets,
                    ctx.shm_basename,
                );
            }
        }
    }
}

// Signal handler (must be async-signal-safe)
extern "C" fn daemon_signal_handler_fn(_sig: i32) {
    SHUTDOWN_REQUESTED.store(true, Ordering::Relaxed);
}
