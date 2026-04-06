//! C ABI wrappers for router subsystems.
//! Replaces router.c. Routes requests to per-program daemons.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::daemon_ffi::{
    DaemonConfig, DaemonResponse, MorlocSocket,
};
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::http_ffi::{DaemonMethod, DaemonRequest, HttpMethod, HttpRequest};

// ── Constants ────────────────────────────────────────────────────────────────

/// Max size of sun_path in sockaddr_un (108 on Linux)
const SUN_PATH_LEN: usize = 108;

// ── Global state ─────────────────────────────────────────────────────────────

static ROUTER_SHUTDOWN_REQUESTED: AtomicBool = AtomicBool::new(false);

extern "C" fn router_signal_handler_fn(_sig: i32) {
    ROUTER_SHUTDOWN_REQUESTED.store(true, Ordering::Relaxed);
}

// ── C-compatible types ───────────────────────────────────────────────────────

#[repr(C)]
pub struct RouterProgram {
    pub name: *mut c_char,
    pub manifest_path: *mut c_char,
    pub manifest: *mut c_void, // manifest_t*
    pub daemon_pid: libc::pid_t,
    pub daemon_socket: [c_char; SUN_PATH_LEN],
}

#[repr(C)]
pub struct Router {
    pub programs: *mut RouterProgram,
    pub n_programs: usize,
    pub fdb_path: *mut c_char,
}

// ── router_init ──────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn router_init(
    fdb_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut Router {
    clear_errmsg(errmsg);

    extern "C" {
        fn read_manifest(path: *const c_char, errmsg: *mut *mut c_char) -> *mut c_void;
    }

    let dir = libc::opendir(fdb_path);
    if dir.is_null() {
        let errno_msg = CStr::from_ptr(libc::strerror(crate::utility::errno_val()))
            .to_string_lossy();
        let path_str = CStr::from_ptr(fdb_path).to_string_lossy();
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!(
                "Cannot open fdb directory '{}': {}",
                path_str, errno_msg
            )),
        );
        return ptr::null_mut();
    }

    let router = libc::calloc(1, std::mem::size_of::<Router>()) as *mut Router;
    (*router).fdb_path = libc::strdup(fdb_path);

    let mut cap: usize = 8;
    (*router).programs =
        libc::calloc(cap, std::mem::size_of::<RouterProgram>()) as *mut RouterProgram;
    (*router).n_programs = 0;

    loop {
        let entry = libc::readdir(dir);
        if entry.is_null() {
            break;
        }

        let name = CStr::from_ptr((*entry).d_name.as_ptr());
        let name_str = name.to_string_lossy();

        if name_str.len() < 10 || !name_str.ends_with(".manifest") {
            continue;
        }

        // Grow array if needed
        if (*router).n_programs >= cap {
            cap *= 2;
            (*router).programs = libc::realloc(
                (*router).programs as *mut c_void,
                cap * std::mem::size_of::<RouterProgram>(),
            ) as *mut RouterProgram;
        }

        let prog = &mut *(*router).programs.add((*router).n_programs);
        ptr::write_bytes(prog as *mut RouterProgram, 0, 1);

        // Extract program name (filename without .manifest)
        let prog_name_len = name_str.len() - 9;
        let prog_name = &name_str[..prog_name_len];
        let c_prog_name = CString::new(prog_name).unwrap_or_default();
        prog.name = libc::strdup(c_prog_name.as_ptr());

        // Build full path
        let fdb_str = CStr::from_ptr(fdb_path).to_string_lossy();
        let full_path = format!("{}/{}", fdb_str, name_str);
        let c_path = CString::new(full_path).unwrap_or_default();
        prog.manifest_path = libc::strdup(c_path.as_ptr());

        // Read and parse manifest
        let mut child_err: *mut c_char = ptr::null_mut();
        prog.manifest = read_manifest(prog.manifest_path, &mut child_err);
        if !child_err.is_null() {
            let err_str = CStr::from_ptr(child_err).to_string_lossy();
            let path_str = CStr::from_ptr(prog.manifest_path).to_string_lossy();
            eprintln!("router: warning: failed to parse {}: {}", path_str, err_str);
            libc::free(child_err as *mut c_void);
            libc::free(prog.name as *mut c_void);
            libc::free(prog.manifest_path as *mut c_void);
            continue;
        }

        prog.daemon_pid = 0;
        // Set socket path
        let socket_path = format!("/tmp/morloc-router-{}.sock", prog_name);
        let c_socket = CString::new(socket_path).unwrap_or_default();
        let socket_bytes = c_socket.as_bytes_with_nul();
        let copy_len = socket_bytes.len().min(SUN_PATH_LEN);
        ptr::copy_nonoverlapping(
            socket_bytes.as_ptr() as *const c_char,
            prog.daemon_socket.as_mut_ptr(),
            copy_len,
        );

        (*router).n_programs += 1;
    }

    libc::closedir(dir);

    // Empty fdb is fine — programs can be added while the router is running

    router
}

// ── router_free ──────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn router_free(router: *mut Router) {
    if router.is_null() {
        return;
    }

    extern "C" {
        fn free_manifest(manifest: *mut c_void);
    }

    for i in 0..(*router).n_programs {
        let prog = &mut *(*router).programs.add(i);
        libc::free(prog.name as *mut c_void);
        libc::free(prog.manifest_path as *mut c_void);
        if !prog.manifest.is_null() {
            free_manifest(prog.manifest);
        }
        if prog.daemon_pid > 0 {
            libc::kill(prog.daemon_pid, libc::SIGTERM);
        }
    }
    libc::free((*router).programs as *mut c_void);
    libc::free((*router).fdb_path as *mut c_void);
    libc::free(router as *mut c_void);
}

// ── morloc-nexus path resolution ─────────────────────────────────────────────

/// Locate the morloc-nexus executable.
///
/// Tries, in order:
///   1. `$MORLOC_NEXUS` (explicit override)
///   2. `$MORLOC_HOME/bin/morloc-nexus` (deploy convention)
///   3. `morloc-nexus` on `$PATH`
///   4. `$HOME/.local/bin/morloc-nexus` (bare-metal developer install)
///
/// Returns the path on the first candidate whose `access(_, X_OK)` succeeds,
/// or the list of attempted paths on failure.
unsafe fn find_morloc_nexus() -> Result<String, Vec<String>> {
    fn is_executable(path: &str) -> bool {
        if let Ok(c) = CString::new(path) {
            unsafe { libc::access(c.as_ptr(), libc::X_OK) == 0 }
        } else {
            false
        }
    }

    fn getenv_str(name: &str) -> Option<String> {
        let c_name = CString::new(name).ok()?;
        let p = unsafe { libc::getenv(c_name.as_ptr()) };
        if p.is_null() {
            None
        } else {
            Some(unsafe { CStr::from_ptr(p) }.to_string_lossy().into_owned())
        }
    }

    let mut tried: Vec<String> = Vec::new();

    // 1. $MORLOC_NEXUS
    if let Some(p) = getenv_str("MORLOC_NEXUS") {
        if is_executable(&p) {
            return Ok(p);
        }
        tried.push(format!("$MORLOC_NEXUS={}", p));
    }

    // 2. $MORLOC_HOME/bin/morloc-nexus
    if let Some(h) = getenv_str("MORLOC_HOME") {
        let p = format!("{}/bin/morloc-nexus", h);
        if is_executable(&p) {
            return Ok(p);
        }
        tried.push(p);
    }

    // 3. Search $PATH
    if let Some(path) = getenv_str("PATH") {
        for dir in path.split(':') {
            if dir.is_empty() {
                continue;
            }
            let p = format!("{}/morloc-nexus", dir);
            if is_executable(&p) {
                return Ok(p);
            }
        }
        tried.push(format!("$PATH ({})", path));
    }

    // 4. $HOME/.local/bin/morloc-nexus
    if let Some(h) = getenv_str("HOME") {
        let p = format!("{}/.local/bin/morloc-nexus", h);
        if is_executable(&p) {
            return Ok(p);
        }
        tried.push(p);
    }

    Err(tried)
}

// ── router_start_program ─────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn router_start_program(
    prog: *mut RouterProgram,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);

    let nexus_path = match find_morloc_nexus() {
        Ok(p) => p,
        Err(tried) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!(
                    "morloc-nexus binary not found; tried: {}",
                    tried.join(", ")
                )),
            );
            return false;
        }
    };
    let c_nexus = CString::new(nexus_path.as_str()).unwrap_or_default();

    let pid = libc::fork();
    if pid == 0 {
        // Child: exec morloc-nexus with --daemon
        libc::setpgid(0, 0);
        let arg_nexus = CString::new("morloc-nexus").unwrap();
        let arg_daemon = CString::new("--daemon").unwrap();
        let arg_socket = CString::new("--socket").unwrap();
        let socket_path = CStr::from_ptr((*prog).daemon_socket.as_ptr());
        libc::execl(
            c_nexus.as_ptr(),
            arg_nexus.as_ptr(),
            (*prog).manifest_path,
            arg_daemon.as_ptr(),
            arg_socket.as_ptr(),
            socket_path.as_ptr(),
            ptr::null::<c_char>(),
        );
        // If exec fails
        let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
        let errno_msg = CStr::from_ptr(libc::strerror(crate::utility::errno_val()))
            .to_string_lossy();
        eprintln!(
            "router: failed to exec morloc-nexus for {}: {}",
            prog_name, errno_msg
        );
        libc::_exit(1);
    } else if pid > 0 {
        (*prog).daemon_pid = pid;

        // Wait 200ms for daemon to start
        let ts = libc::timespec {
            tv_sec: 0,
            tv_nsec: 200_000_000,
        };
        libc::nanosleep(&ts, ptr::null_mut());

        // Check child is still alive
        let mut status: i32 = 0;
        let result = libc::waitpid(pid, &mut status, libc::WNOHANG);
        if result == pid {
            (*prog).daemon_pid = 0;
            let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!(
                    "Daemon for '{}' exited immediately (status {})",
                    prog_name, status
                )),
            );
            return false;
        }

        true
    } else {
        let errno_msg = CStr::from_ptr(libc::strerror(crate::utility::errno_val()))
            .to_string_lossy();
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!("fork failed: {}", errno_msg)),
        );
        false
    }
}

// ── router_forward ───────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn router_forward(
    router: *mut Router,
    program: *const c_char,
    request: *mut DaemonRequest,
    errmsg: *mut *mut c_char,
) -> *mut DaemonResponse {
    clear_errmsg(errmsg);

    extern "C" {
        fn daemon_parse_response(
            json: *const c_char,
            len: usize,
            errmsg: *mut *mut c_char,
        ) -> *mut DaemonResponse;
    }

    // Find program
    let program_name = CStr::from_ptr(program);
    let mut prog: *mut RouterProgram = ptr::null_mut();
    for i in 0..(*router).n_programs {
        let p = (*router).programs.add(i);
        if CStr::from_ptr((*p).name) == program_name {
            prog = p;
            break;
        }
    }

    if prog.is_null() {
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!(
                "Unknown program: {}",
                program_name.to_string_lossy()
            )),
        );
        return ptr::null_mut();
    }

    // Start daemon if not running
    if (*prog).daemon_pid <= 0 {
        let mut child_err: *mut c_char = ptr::null_mut();
        if !router_start_program(prog, &mut child_err) {
            if !child_err.is_null() {
                *errmsg = child_err;
            } else {
                set_errmsg(
                    errmsg,
                    &MorlocError::Other("Failed to start program daemon".into()),
                );
            }
            return ptr::null_mut();
        }
    }

    // Serialize request to JSON
    let req_json = serialize_request_to_json(request);
    let c_req = CString::new(req_json.as_str()).unwrap_or_default();
    let req_len = req_json.len();

    // Try to connect, retry once on failure
    let sock = connect_to_daemon(prog, errmsg);
    let sock = if sock < 0 {
        // Try restarting daemon
        (*prog).daemon_pid = 0;
        // Clear previous error
        if !(*errmsg).is_null() {
            libc::free(*errmsg as *mut c_void);
            *errmsg = ptr::null_mut();
        }
        let mut child_err: *mut c_char = ptr::null_mut();
        if !router_start_program(prog, &mut child_err) {
            if !child_err.is_null() {
                *errmsg = child_err;
            }
            return ptr::null_mut();
        }
        let sock2 = connect_to_daemon(prog, errmsg);
        if sock2 < 0 {
            return ptr::null_mut();
        }
        sock2
    } else {
        sock
    };

    // Send length-prefixed message
    let len_buf: [u8; 4] = [
        ((req_len >> 24) & 0xFF) as u8,
        ((req_len >> 16) & 0xFF) as u8,
        ((req_len >> 8) & 0xFF) as u8,
        (req_len & 0xFF) as u8,
    ];

    let n = libc::send(
        sock,
        len_buf.as_ptr() as *const c_void,
        4,
        crate::utility::SEND_NOSIGNAL,
    );
    if n != 4 {
        libc::close(sock);
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to send request length to daemon".into()),
        );
        return ptr::null_mut();
    }

    let mut total_sent: usize = 0;
    while total_sent < req_len {
        let n = libc::send(
            sock,
            c_req.as_ptr().add(total_sent) as *const c_void,
            req_len - total_sent,
            crate::utility::SEND_NOSIGNAL,
        );
        if n <= 0 {
            libc::close(sock);
            set_errmsg(
                errmsg,
                &MorlocError::Other("Failed to send request body to daemon".into()),
            );
            return ptr::null_mut();
        }
        total_sent += n as usize;
    }

    // Read response length
    let mut resp_len_buf = [0u8; 4];
    let n = libc::recv(
        sock,
        resp_len_buf.as_mut_ptr() as *mut c_void,
        4,
        libc::MSG_WAITALL,
    );
    if n != 4 {
        libc::close(sock);
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to read response length from daemon".into()),
        );
        return ptr::null_mut();
    }

    let resp_len = ((resp_len_buf[0] as u32) << 24)
        | ((resp_len_buf[1] as u32) << 16)
        | ((resp_len_buf[2] as u32) << 8)
        | (resp_len_buf[3] as u32);

    let resp_json = libc::malloc(resp_len as usize + 1) as *mut c_char;
    if resp_json.is_null() {
        libc::close(sock);
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to allocate response buffer".into()),
        );
        return ptr::null_mut();
    }

    let mut total_recv: usize = 0;
    while total_recv < resp_len as usize {
        let n = libc::recv(
            sock,
            resp_json.add(total_recv) as *mut c_void,
            resp_len as usize - total_recv,
            0,
        );
        if n <= 0 {
            libc::free(resp_json as *mut c_void);
            libc::close(sock);
            set_errmsg(
                errmsg,
                &MorlocError::Other("Failed to read response body from daemon".into()),
            );
            return ptr::null_mut();
        }
        total_recv += n as usize;
    }
    *resp_json.add(resp_len as usize) = 0;
    libc::close(sock);

    let resp = daemon_parse_response(resp_json, resp_len as usize, errmsg);
    libc::free(resp_json as *mut c_void);
    resp
}

/// Helper: connect to a program daemon's unix socket with 60s timeouts.
unsafe fn connect_to_daemon(
    prog: *mut RouterProgram,
    errmsg: *mut *mut c_char,
) -> i32 {
    let sock = libc::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
    if sock < 0 {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to create socket".into()),
        );
        return -1;
    }
    crate::utility::set_nosigpipe(sock);

    let tv = libc::timeval {
        tv_sec: 60,
        tv_usec: 0,
    };
    libc::setsockopt(
        sock,
        libc::SOL_SOCKET,
        libc::SO_RCVTIMEO,
        &tv as *const libc::timeval as *const c_void,
        std::mem::size_of::<libc::timeval>() as libc::socklen_t,
    );
    libc::setsockopt(
        sock,
        libc::SOL_SOCKET,
        libc::SO_SNDTIMEO,
        &tv as *const libc::timeval as *const c_void,
        std::mem::size_of::<libc::timeval>() as libc::socklen_t,
    );

    let mut addr: libc::sockaddr_un = std::mem::zeroed();
    addr.sun_family = libc::AF_UNIX as libc::sa_family_t;
    let socket_path = (*prog).daemon_socket.as_ptr();
    let path_bytes = CStr::from_ptr(socket_path).to_bytes();
    let copy_len = path_bytes.len().min(addr.sun_path.len() - 1);
    ptr::copy_nonoverlapping(
        path_bytes.as_ptr() as *const c_char,
        addr.sun_path.as_mut_ptr(),
        copy_len,
    );

    if libc::connect(
        sock,
        &addr as *const libc::sockaddr_un as *const libc::sockaddr,
        std::mem::size_of::<libc::sockaddr_un>() as libc::socklen_t,
    ) < 0
    {
        libc::close(sock);
        let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!(
                "Failed to connect to daemon for '{}'",
                prog_name
            )),
        );
        return -1;
    }

    sock
}

/// Serialize a DaemonRequest to JSON using serde_json.
unsafe fn serialize_request_to_json(request: *mut DaemonRequest) -> String {
    let mut map = serde_json::Map::new();

    if !(*request).id.is_null() {
        let id = CStr::from_ptr((*request).id).to_string_lossy();
        map.insert("id".into(), serde_json::Value::String(id.into_owned()));
    }

    let method_str = match (*request).method {
        DaemonMethod::Call => "call",
        DaemonMethod::Discover => "discover",
        DaemonMethod::Health => "health",
        DaemonMethod::Eval => "eval",
        DaemonMethod::Typecheck => "typecheck",
        DaemonMethod::Bind => "bind",
        DaemonMethod::Bindings => "bindings",
        DaemonMethod::Unbind => "unbind",
    };
    map.insert(
        "method".into(),
        serde_json::Value::String(method_str.into()),
    );

    if !(*request).command.is_null() {
        let cmd = CStr::from_ptr((*request).command).to_string_lossy();
        map.insert(
            "command".into(),
            serde_json::Value::String(cmd.into_owned()),
        );
    }

    if !(*request).args_json.is_null() {
        let args_str = CStr::from_ptr((*request).args_json).to_string_lossy();
        // Try to parse as JSON value to embed directly
        if let Ok(v) = serde_json::from_str::<serde_json::Value>(&args_str) {
            map.insert("args".into(), v);
        }
    }

    if !(*request).expr.is_null() {
        let expr = CStr::from_ptr((*request).expr).to_string_lossy();
        map.insert("expr".into(), serde_json::Value::String(expr.into_owned()));
    }

    if !(*request).name.is_null() {
        let name = CStr::from_ptr((*request).name).to_string_lossy();
        map.insert("name".into(), serde_json::Value::String(name.into_owned()));
    }

    serde_json::to_string(&map).unwrap_or_else(|_| "{}".into())
}

// ── router_build_discovery ───────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn router_build_discovery(router: *mut Router) -> *mut c_char {
    // We need to access manifest->n_commands and manifest->commands[i].name etc.
    // Use the same ManifestView approach as daemon_ffi.rs.
    #[repr(C)]
    struct ManifestCommandView {
        name: *mut c_char,
        is_pure: bool,
        _mid: u32,
        _pool_index: usize,
        _needed_pools: *mut usize,
        _n_needed_pools: usize,
        _arg_schemas: *mut *mut c_char,
        _return_schema: *mut c_char,
        _desc: *mut *mut c_char,
        return_type: *mut c_char,
    }

    #[repr(C)]
    struct ManifestView {
        _version: i32,
        _name: *mut c_char,
        _build_dir: *mut c_char,
        _pools: *mut c_void,
        _n_pools: usize,
        commands: *mut ManifestCommandView,
        n_commands: usize,
    }

    #[derive(serde::Serialize)]
    struct CommandInfo {
        name: String,
        r#type: String,
        return_type: String,
    }

    #[derive(serde::Serialize)]
    struct ProgramInfo {
        name: String,
        running: bool,
        #[serde(skip_serializing_if = "Option::is_none")]
        commands: Option<Vec<CommandInfo>>,
    }

    #[derive(serde::Serialize)]
    struct Discovery {
        programs: Vec<ProgramInfo>,
    }

    let mut programs = Vec::with_capacity((*router).n_programs);

    for i in 0..(*router).n_programs {
        let prog = &*(*router).programs.add(i);
        let name = CStr::from_ptr(prog.name).to_string_lossy().into_owned();
        let running =
            prog.daemon_pid > 0 && libc::kill(prog.daemon_pid, 0) == 0;

        let commands = if !prog.manifest.is_null() {
            let mv = prog.manifest as *const ManifestView;
            let mut cmds = Vec::with_capacity((*mv).n_commands);
            for c in 0..(*mv).n_commands {
                let cmd = &*(*mv).commands.add(c);
                let cmd_name = CStr::from_ptr(cmd.name).to_string_lossy().into_owned();
                let cmd_type = if cmd.is_pure { "pure" } else { "remote" };
                let ret_type = if !cmd.return_type.is_null() {
                    CStr::from_ptr(cmd.return_type)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    String::new()
                };
                cmds.push(CommandInfo {
                    name: cmd_name,
                    r#type: cmd_type.into(),
                    return_type: ret_type,
                });
            }
            Some(cmds)
        } else {
            None
        };

        programs.push(ProgramInfo {
            name,
            running,
            commands,
        });
    }

    let disco = Discovery { programs };
    let json = serde_json::to_string(&disco).unwrap_or_else(|_| "{}".into());
    let c = CString::new(json).unwrap_or_default();
    libc::strdup(c.as_ptr())
}

// ── Router HTTP request routing ──────────────────────────────────────────────

/// Route HTTP requests for the router. Sets *out_program to the target program
/// name (caller-owned) for per-program requests, or NULL for router-level requests.
unsafe fn router_http_to_request(
    req: *mut HttpRequest,
    out_program: *mut *mut c_char,
    errmsg: *mut *mut c_char,
) -> *mut DaemonRequest {
    clear_errmsg(errmsg);

    let dreq = libc::calloc(1, std::mem::size_of::<DaemonRequest>()) as *mut DaemonRequest;
    if dreq.is_null() {
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to allocate daemon_request_t".into()),
        );
        return ptr::null_mut();
    }

    *out_program = ptr::null_mut();

    let path = CStr::from_ptr((*req).path.as_ptr())
        .to_str()
        .unwrap_or("");
    let method = (*req).method;

    let body_str = if !(*req).body.is_null() && (*req).body_len > 0 {
        std::str::from_utf8(std::slice::from_raw_parts(
            (*req).body as *const u8,
            (*req).body_len,
        ))
        .unwrap_or("")
    } else {
        ""
    };

    // GET /health
    if method == HttpMethod::Get && path == "/health" {
        (*dreq).method = DaemonMethod::Health;
        return dreq;
    }

    // GET /programs or GET /discover
    if method == HttpMethod::Get && (path == "/programs" || path == "/discover") {
        (*dreq).method = DaemonMethod::Discover;
        return dreq;
    }

    // GET /discover/<program>
    if method == HttpMethod::Get && path.starts_with("/discover/") {
        let prog_name = &path[10..];
        if !prog_name.is_empty() {
            let c = CString::new(prog_name).unwrap_or_default();
            *out_program = libc::strdup(c.as_ptr());
            (*dreq).method = DaemonMethod::Discover;
            return dreq;
        }
    }

    // POST /eval
    if method == HttpMethod::Post && path == "/eval" {
        (*dreq).method = DaemonMethod::Eval;
        if !body_str.is_empty() {
            if let Ok(v) = serde_json::from_str::<serde_json::Value>(body_str) {
                if let Some(expr) = v.get("expr").and_then(|e| e.as_str()) {
                    let c = CString::new(expr).unwrap_or_default();
                    (*dreq).expr = libc::strdup(c.as_ptr());
                }
            }
        }
        if (*dreq).expr.is_null() {
            libc::free(dreq as *mut c_void);
            set_errmsg(
                errmsg,
                &MorlocError::Other("Missing 'expr' field in /eval request body".into()),
            );
            return ptr::null_mut();
        }
        return dreq;
    }

    // POST /call/<program>/<command>
    if method == HttpMethod::Post && path.starts_with("/call/") {
        let rest = &path[6..];
        let slash = rest.find('/');
        match slash {
            Some(pos) if pos + 1 < rest.len() => {
                let prog_name = &rest[..pos];
                let cmd_name = &rest[pos + 1..];
                let c_prog = CString::new(prog_name).unwrap_or_default();
                *out_program = libc::strdup(c_prog.as_ptr());
                (*dreq).method = DaemonMethod::Call;
                let c_cmd = CString::new(cmd_name).unwrap_or_default();
                (*dreq).command = libc::strdup(c_cmd.as_ptr());

                // Parse body for args
                let trimmed = body_str.trim();
                if trimmed.starts_with('[') {
                    let c = CString::new(trimmed).unwrap_or_default();
                    (*dreq).args_json = libc::strdup(c.as_ptr());
                } else if trimmed.starts_with('{') {
                    if let Ok(v) = serde_json::from_str::<serde_json::Value>(trimmed) {
                        if let Some(args) = v.get("args") {
                            let args_str = serde_json::to_string(args).unwrap_or_default();
                            let c = CString::new(args_str).unwrap_or_default();
                            (*dreq).args_json = libc::strdup(c.as_ptr());
                        }
                    }
                }
                return dreq;
            }
            _ => {
                libc::free(dreq as *mut c_void);
                set_errmsg(
                    errmsg,
                    &MorlocError::Other("Expected /call/<program>/<command>".into()),
                );
                return ptr::null_mut();
            }
        }
    }

    // OPTIONS (CORS)
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
    set_errmsg(
        errmsg,
        &MorlocError::Other(format!("Unknown router endpoint: {} {}", method_str, path)),
    );
    ptr::null_mut()
}

// ── Router event loop ────────────────────────────────────────────────────────

const ROUTER_MAX_LISTENERS: usize = 3;

#[no_mangle]
pub unsafe extern "C" fn router_run(config: *mut DaemonConfig, router: *mut Router) {
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
        fn daemon_dispatch(
            manifest: *mut c_void,
            request: *mut DaemonRequest,
            sockets: *mut MorlocSocket,
            shm_basename: *const c_char,
        ) -> *mut DaemonResponse;
        fn daemon_serialize_response(
            response: *mut DaemonResponse,
            out_len: *mut usize,
        ) -> *mut c_char;
        fn daemon_free_request(req: *mut DaemonRequest);
        fn daemon_free_response(resp: *mut DaemonResponse);
        fn daemon_set_eval_timeout(timeout_sec: i32);
        fn manifest_to_discovery_json(manifest: *const c_void) -> *mut c_char;
    }

    daemon_set_eval_timeout((*config).eval_timeout);

    // Install signal handlers
    ROUTER_SHUTDOWN_REQUESTED.store(false, Ordering::Relaxed);
    let handler: libc::sighandler_t =
        std::mem::transmute::<extern "C" fn(i32), libc::sighandler_t>(router_signal_handler_fn);
    libc::signal(libc::SIGTERM, handler);
    libc::signal(libc::SIGINT, handler);

    let mut fds = [libc::pollfd {
        fd: -1,
        events: 0,
        revents: 0,
    }; ROUTER_MAX_LISTENERS];
    let mut nfds: usize = 0;

    let ct = b"application/json\0";

    // HTTP listener
    if (*config).http_port > 0 {
        let http_fd = libc::socket(libc::AF_INET, libc::SOCK_STREAM, 0);
        if http_fd < 0 {
            eprintln!("router: failed to create http socket");
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
        addr.sin_addr.s_addr = libc::INADDR_ANY;
        addr.sin_port = ((*config).http_port as u16).to_be();
        if libc::bind(
            http_fd,
            &addr as *const libc::sockaddr_in as *const libc::sockaddr,
            std::mem::size_of::<libc::sockaddr_in>() as libc::socklen_t,
        ) < 0
        {
            eprintln!(
                "router: failed to bind http port {}",
                (*config).http_port
            );
            libc::close(http_fd);
            return;
        }
        libc::listen(http_fd, 16);
        fds[nfds].fd = http_fd;
        fds[nfds].events = libc::POLLIN as i16;
        nfds += 1;
    }

    // Unix socket
    if !(*config).unix_socket_path.is_null() {
        let sock_fd = libc::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
        if sock_fd < 0 {
            eprintln!("router: failed to create unix socket");
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
            eprintln!("router: failed to bind unix socket");
            libc::close(sock_fd);
            return;
        }
        libc::listen(sock_fd, 16);
        fds[nfds].fd = sock_fd;
        fds[nfds].events = libc::POLLIN as i16;
        nfds += 1;
    }

    if nfds == 0 {
        eprintln!("router: no listeners configured");
        return;
    }

    while !ROUTER_SHUTDOWN_REQUESTED.load(Ordering::Relaxed) {
        let ready = libc::poll(fds.as_mut_ptr(), nfds as libc::nfds_t, 1000);
        if ready < 0 {
            if crate::utility::errno_val() == libc::EINTR {
                continue;
            }
            eprintln!("router: poll error");
            break;
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
                continue;
            }
            crate::utility::set_nosigpipe(client_fd);

            let tv = libc::timeval {
                tv_sec: 30,
                tv_usec: 0,
            };
            libc::setsockopt(
                client_fd,
                libc::SOL_SOCKET,
                libc::SO_RCVTIMEO,
                &tv as *const libc::timeval as *const c_void,
                std::mem::size_of::<libc::timeval>() as libc::socklen_t,
            );
            libc::setsockopt(
                client_fd,
                libc::SOL_SOCKET,
                libc::SO_SNDTIMEO,
                &tv as *const libc::timeval as *const c_void,
                std::mem::size_of::<libc::timeval>() as libc::socklen_t,
            );

            let mut err: *mut c_char = ptr::null_mut();

            let http_req = http_parse_request(client_fd, &mut err);
            if !err.is_null() {
                let body = b"{\"status\":\"error\",\"error\":\"Bad request\"}\0";
                http_write_response(
                    client_fd,
                    400,
                    ct.as_ptr() as *const c_char,
                    body.as_ptr() as *const c_char,
                    body.len() - 1,
                );
                libc::free(err as *mut c_void);
                libc::close(client_fd);
                continue;
            }

            let mut target_program: *mut c_char = ptr::null_mut();
            let dreq = router_http_to_request(http_req, &mut target_program, &mut err);
            http_free_request(http_req);

            if !err.is_null() {
                let err_json = make_error_json(&CStr::from_ptr(err).to_string_lossy());
                let c = CString::new(err_json.as_str()).unwrap_or_default();
                http_write_response(
                    client_fd,
                    404,
                    ct.as_ptr() as *const c_char,
                    c.as_ptr(),
                    err_json.len(),
                );
                libc::free(err as *mut c_void);
                libc::close(client_fd);
                continue;
            }

            // Router-level requests
            if target_program.is_null() {
                if (*dreq).method == DaemonMethod::Health {
                    let body = b"{\"status\":\"ok\"}\0";
                    http_write_response(
                        client_fd,
                        200,
                        ct.as_ptr() as *const c_char,
                        body.as_ptr() as *const c_char,
                        body.len() - 1,
                    );
                } else if (*dreq).method == DaemonMethod::Discover {
                    let disco = router_build_discovery(router);
                    let disco_len = libc::strlen(disco);
                    http_write_response(
                        client_fd,
                        200,
                        ct.as_ptr() as *const c_char,
                        disco,
                        disco_len,
                    );
                    libc::free(disco as *mut c_void);
                } else if (*dreq).method == DaemonMethod::Eval {
                    // daemon_dispatch takes manifest as first arg, NULL is fine for eval
                    let resp = daemon_dispatch(ptr::null_mut(), dreq, ptr::null_mut(), ptr::null());
                    let mut resp_len: usize = 0;
                    let resp_json = daemon_serialize_response(resp, &mut resp_len);
                    let status = if (*resp).success { 200 } else { 500 };
                    http_write_response(
                        client_fd,
                        status,
                        ct.as_ptr() as *const c_char,
                        resp_json,
                        resp_len,
                    );
                    libc::free(resp_json as *mut c_void);
                    daemon_free_response(resp);
                }
                daemon_free_request(dreq);
                libc::close(client_fd);
                continue;
            }

            // Per-program request
            if (*dreq).method == DaemonMethod::Discover {
                let mut found = false;
                for p in 0..(*router).n_programs {
                    let rprog = &*(*router).programs.add(p);
                    if CStr::from_ptr(rprog.name) == CStr::from_ptr(target_program) {
                        if !rprog.manifest.is_null() {
                            let disco = manifest_to_discovery_json(rprog.manifest);
                            let disco_len = libc::strlen(disco);
                            http_write_response(
                                client_fd,
                                200,
                                ct.as_ptr() as *const c_char,
                                disco,
                                disco_len,
                            );
                            libc::free(disco as *mut c_void);
                            found = true;
                        }
                        break;
                    }
                }
                if !found {
                    let body = b"{\"status\":\"error\",\"error\":\"Unknown program\"}\0";
                    http_write_response(
                        client_fd,
                        404,
                        ct.as_ptr() as *const c_char,
                        body.as_ptr() as *const c_char,
                        body.len() - 1,
                    );
                }
            } else {
                // Forward to program daemon
                let resp = router_forward(router, target_program, dreq, &mut err);
                if !err.is_null() {
                    let err_json =
                        make_error_json(&CStr::from_ptr(err).to_string_lossy());
                    let c = CString::new(err_json.as_str()).unwrap_or_default();
                    http_write_response(
                        client_fd,
                        500,
                        ct.as_ptr() as *const c_char,
                        c.as_ptr(),
                        err_json.len(),
                    );
                    libc::free(err as *mut c_void);
                } else {
                    let mut resp_len: usize = 0;
                    let resp_json = daemon_serialize_response(resp, &mut resp_len);
                    let status = if (*resp).success { 200 } else { 500 };
                    http_write_response(
                        client_fd,
                        status,
                        ct.as_ptr() as *const c_char,
                        resp_json,
                        resp_len,
                    );
                    libc::free(resp_json as *mut c_void);
                    daemon_free_response(resp);
                }
            }

            libc::free(target_program as *mut c_void);
            daemon_free_request(dreq);
            libc::close(client_fd);
        }
    }

    // Kill all program daemons
    for i in 0..(*router).n_programs {
        let prog = &*(*router).programs.add(i);
        if prog.daemon_pid > 0 {
            libc::kill(prog.daemon_pid, libc::SIGTERM);
            libc::unlink(prog.daemon_socket.as_ptr());
        }
    }

    // Wait for children
    for i in 0..(*router).n_programs {
        let prog = &*(*router).programs.add(i);
        if prog.daemon_pid > 0 {
            libc::waitpid(prog.daemon_pid, ptr::null_mut(), 0);
        }
    }

    // Close listeners
    for i in 0..nfds {
        libc::close(fds[i].fd);
    }

    if !(*config).unix_socket_path.is_null() {
        libc::unlink((*config).unix_socket_path);
    }
}

/// Build a JSON error response string.
fn make_error_json(error: &str) -> String {
    let map: serde_json::Map<String, serde_json::Value> = [
        ("status".into(), serde_json::Value::String("error".into())),
        ("error".into(), serde_json::Value::String(error.into())),
    ]
    .into_iter()
    .collect();
    serde_json::to_string(&map).unwrap_or_else(|_| "{}".into())
}
