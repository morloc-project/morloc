//! C ABI wrappers for router subsystems.
//! Replaces router.c. Routes requests to per-program daemons.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::daemon_ffi::DaemonResponse;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::http_ffi::{DaemonMethod, DaemonRequest};

// -- Constants ----------------------------------------------------------------

/// Max size of sun_path in sockaddr_un (108 on Linux)
const SUN_PATH_LEN: usize = 108;

// Daemon startup polling (exponential backoff, ~5s total).
// Sum of 100 * 1.25^i for i in 0..16 is ~4650ms.
const DAEMON_POLL_INITIAL_MS: f64 = 100.0;
const DAEMON_POLL_MULTIPLIER: f64 = 1.25;
const DAEMON_POLL_MAX_RETRIES: usize = 16;

// -- C-compatible types -------------------------------------------------------

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

// -- router builder + init ----------------------------------------------------

// Build a Router over an EXPLICIT set of program names under `exe_str`. A named
// program that is missing or whose manifest fails to parse is an ERROR (the
// caller asked for exactly these programs). There is no serve-everything scan:
// which modules are served is always an explicit decision.
unsafe fn router_build(
    fdb_path: *const c_char,
    exe_str: &str,
    names: &[String],
    errmsg: *mut *mut c_char,
) -> *mut Router {
    extern "C" {
        fn read_manifest(path: *const c_char, errmsg: *mut *mut c_char) -> *mut c_void;
    }

    let router = libc::calloc(1, std::mem::size_of::<Router>()) as *mut Router;
    (*router).fdb_path = libc::strdup(fdb_path);
    let cap = names.len().max(1);
    (*router).programs =
        libc::calloc(cap, std::mem::size_of::<RouterProgram>()) as *mut RouterProgram;
    (*router).n_programs = 0;

    for name_str in names {
        // Installed layout: exe/<name>/<name>-build/manifest.json
        // (see Morloc.ProgramBuilder.Paths for the shared convention).
        let full_path = format!("{}/{}/{}-build/manifest.json", exe_str, name_str, name_str);
        if !std::path::Path::new(&full_path).is_file() {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!(
                    "Program '{}' is not installed (no {})",
                    name_str, full_path
                )),
            );
            router_free(router);
            return ptr::null_mut();
        }

        let prog = &mut *(*router).programs.add((*router).n_programs);
        ptr::write_bytes(prog as *mut RouterProgram, 0, 1);

        let c_prog_name = CString::new(name_str.as_str()).unwrap_or_default();
        prog.name = libc::strdup(c_prog_name.as_ptr());

        let c_path = CString::new(full_path.clone()).unwrap_or_default();
        prog.manifest_path = libc::strdup(c_path.as_ptr());

        // Read and parse manifest
        let mut child_err: *mut c_char = ptr::null_mut();
        prog.manifest = read_manifest(prog.manifest_path, &mut child_err);
        if !child_err.is_null() {
            let err_str = CStr::from_ptr(child_err).to_string_lossy().into_owned();
            libc::free(child_err as *mut c_void);
            libc::free(prog.name as *mut c_void);
            libc::free(prog.manifest_path as *mut c_void);
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!("Failed to parse {}: {}", full_path, err_str)),
            );
            router_free(router);
            return ptr::null_mut();
        }

        prog.daemon_pid = 0;
        // Set socket path
        let socket_path = format!("/tmp/morloc-router-{}.sock", name_str);
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

    router
}

// Serve exactly the named programs under `fdb_path`. A named program that is not
// installed is an error. The only serve path: which modules are served is an
// explicit decision, never "whatever happens to be installed".
#[no_mangle]
pub unsafe extern "C" fn router_init_explicit(
    fdb_path: *const c_char,
    names: *const *const c_char,
    n_names: usize,
    errmsg: *mut *mut c_char,
) -> *mut Router {
    clear_errmsg(errmsg);
    let exe_str = CStr::from_ptr(fdb_path).to_string_lossy().into_owned();
    let mut name_vec: Vec<String> = Vec::with_capacity(n_names);
    for i in 0..n_names {
        let p = *names.add(i);
        if !p.is_null() {
            name_vec.push(CStr::from_ptr(p).to_string_lossy().into_owned());
        }
    }
    router_build(fdb_path, &exe_str, &name_vec, errmsg)
}

// SIGTERM every live child daemon so each cleans up its own pools and SHM.
// Async-signal-safe (only `libc::kill`, no allocation/free/stdio), so it is safe
// to call from a signal handler; the serving front-end has no other shutdown
// path (it never returns), so this is how children are told to exit gracefully.
#[no_mangle]
pub unsafe extern "C" fn router_terminate_children(router: *mut Router) {
    if router.is_null() {
        return;
    }
    for i in 0..(*router).n_programs {
        let prog = &*(*router).programs.add(i);
        if prog.daemon_pid > 0 {
            libc::kill(prog.daemon_pid, libc::SIGTERM);
        }
    }
}

// -- router_free --------------------------------------------------------------

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

// -- morloc-nexus path resolution ---------------------------------------------

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

// -- router_start_program -----------------------------------------------------

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
        // Child: exec `morloc-nexus daemon <manifest> --socket <path>`.
        // The post-CLI-overhaul nexus uses explicit subcommands;
        // `daemon` is the only argv shape that brings up a long-
        // lived server. The router relies on this child to bind the
        // Unix socket at `daemon_socket` so subsequent router
        // requests can connect.
        libc::setpgid(0, 0);
        let arg_nexus = CString::new("morloc-nexus").unwrap();
        let arg_daemon = CString::new("daemon").unwrap();
        let arg_socket = CString::new("--socket").unwrap();
        let socket_path = CStr::from_ptr((*prog).daemon_socket.as_ptr());
        libc::execl(
            c_nexus.as_ptr(),
            arg_nexus.as_ptr(),
            arg_daemon.as_ptr(),
            (*prog).manifest_path,
            arg_socket.as_ptr(),
            socket_path.as_ptr(),
            ptr::null::<c_char>(),
        );
        // If exec fails
        let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
        let errno_msg = CStr::from_ptr(libc::strerror(crate::utility::errno_val()))
            .to_string_lossy();
        eprintln!(
            "morloc-router: failed to exec morloc-nexus for {}: {}",
            prog_name, errno_msg
        );
        libc::_exit(1);
    } else if pid > 0 {
        (*prog).daemon_pid = pid;

        // Poll until the daemon socket is connectable (exponential backoff)
        let mut delay_ms = DAEMON_POLL_INITIAL_MS;
        let mut connected = false;
        for _attempt in 0..DAEMON_POLL_MAX_RETRIES {
            let ts = libc::timespec {
                tv_sec: 0,
                tv_nsec: (delay_ms * 1_000_000.0) as i64,
            };
            libc::nanosleep(&ts, ptr::null_mut());

            // Check if child died during startup
            let mut status: i32 = 0;
            let result = libc::waitpid(pid, &mut status, libc::WNOHANG);
            if result == pid {
                (*prog).daemon_pid = 0;
                let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
                set_errmsg(
                    errmsg,
                    &MorlocError::Other(format!(
                        "Daemon for '{}' exited during startup (status {})",
                        prog_name, status
                    )),
                );
                return false;
            }

            // Try connecting to the daemon socket
            let test_sock = libc::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
            if test_sock >= 0 {
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
                let rc = libc::connect(
                    test_sock,
                    &addr as *const libc::sockaddr_un as *const libc::sockaddr,
                    std::mem::size_of::<libc::sockaddr_un>() as libc::socklen_t,
                );
                libc::close(test_sock);
                if rc == 0 {
                    connected = true;
                    break;
                }
            }

            delay_ms *= DAEMON_POLL_MULTIPLIER;
        }

        if !connected {
            // Final check: did the daemon die?
            let mut status: i32 = 0;
            let result = libc::waitpid(pid, &mut status, libc::WNOHANG);
            if result == pid {
                (*prog).daemon_pid = 0;
                let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
                set_errmsg(
                    errmsg,
                    &MorlocError::Other(format!(
                        "Daemon for '{}' exited during startup (status {})",
                        prog_name, status
                    )),
                );
                return false;
            }
            // Daemon alive but socket not yet connectable -- proceed anyway,
            // router_forward() will retry on connect failure.
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

// -- router_forward -----------------------------------------------------------

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

    // Check if a previously-started daemon has exited (crash recovery)
    if (*prog).daemon_pid > 0 {
        let mut status: i32 = 0;
        let result = libc::waitpid((*prog).daemon_pid, &mut status, libc::WNOHANG);
        if result == (*prog).daemon_pid || result < 0 {
            let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
            eprintln!(
                "morloc-router: daemon for '{}' exited (status {}), will restart",
                prog_name, status
            );
            (*prog).daemon_pid = 0;
        }
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

    // A forward is always a serving front-end call: request the raw-media form
    // so an `@mime` return arrives as bytes+mime. Direct length-prefixed clients
    // (which don't go through router_forward) omit this and keep JSON `result`.
    map.insert("media".into(), serde_json::Value::Bool(true));

    serde_json::to_string(&map).unwrap_or_else(|_| "{}".into())
}

// -- router_build_discovery ---------------------------------------------------

#[no_mangle]
pub unsafe extern "C" fn router_build_discovery(router: *mut Router) -> *mut c_char {
    // Walk the canonical Manifest C struct from manifest_ffi.rs. No
    // local mirror -- the in-memory layout is shared.
    use crate::manifest_ffi::Manifest as ManifestC;

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
            let mv = prog.manifest as *const ManifestC;
            let mut cmds = Vec::with_capacity((*mv).n_commands);
            for c in 0..(*mv).n_commands {
                let cmd = &*(*mv).commands.add(c);
                let cmd_name = CStr::from_ptr(cmd.name).to_string_lossy().into_owned();
                let cmd_type = if cmd.is_pure { "pure" } else { "remote" };
                let ret_type = if !cmd.ret.type_desc.is_null() {
                    CStr::from_ptr(cmd.ret.type_desc)
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

