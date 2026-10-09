//! C ABI wrappers for router subsystems.
//! Replaces router.c. Routes requests to per-program daemons.

use std::ffi::{c_char, c_void, CStr, CString};
use std::ptr;

use crate::daemon_ffi::DaemonResponse;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::http_ffi::{DaemonMethod, DaemonRequest};

// -- Constants ----------------------------------------------------------------

use crate::utility::SUN_PATH_LEN;

// Daemon startup polling: backoff from 100 ms by 1.25x, each wait at most
// 1 s, until the start limit.
const DAEMON_POLL_INITIAL_MS: f64 = 100.0;
const DAEMON_POLL_MULTIPLIER: f64 = 1.25;
const DAEMON_POLL_MAX_MS: f64 = 1000.0;
/// How long a daemon has to accept connections before it is stopped.
const DAEMON_START_LIMIT: std::time::Duration =
    std::time::Duration::from_secs(if cfg!(test) { 3 } else { 30 });

// -- daemon-startup diagnostics -----------------------------------------------

/// Read an environment variable as an owned String (None if unset).
unsafe fn env_str(name: &str) -> Option<String> {
    let c_name = CString::new(name).ok()?;
    let p = libc::getenv(c_name.as_ptr());
    if p.is_null() {
        return None;
    }
    Some(CStr::from_ptr(p).to_string_lossy().into_owned())
}

/// Decode a `waitpid` status word into a human-readable phrase so callers see
/// "exited with code 1" / "killed by signal 11" instead of a raw integer.
fn describe_wait_status(status: i32) -> String {
    if libc::WIFEXITED(status) {
        format!("exited with code {}", libc::WEXITSTATUS(status))
    } else if libc::WIFSIGNALED(status) {
        format!("killed by signal {}", libc::WTERMSIG(status))
    } else {
        format!("ended (raw status {})", status)
    }
}

/// Read up to the last `max_bytes` of a file as trimmed, lossy-UTF8 text.
/// Returns "" on any error or if the file is empty. Used to surface a crashed
/// daemon's captured stderr in the router error message.
fn read_file_tail(path: &str, max_bytes: usize) -> String {
    match std::fs::read(path) {
        Ok(bytes) => {
            let start = bytes.len().saturating_sub(max_bytes);
            String::from_utf8_lossy(&bytes[start..]).trim().to_string()
        }
        Err(_) => String::new(),
    }
}

/// Build the router error for a daemon that died during startup: the decoded
/// exit status plus, when available, a tail of its captured stderr.
fn startup_death_msg(prog_name: &str, status: i32, stderr_log: &str) -> String {
    let base = format!(
        "Daemon for '{}' {} during startup",
        prog_name,
        describe_wait_status(status)
    );
    let detail = read_file_tail(stderr_log, 4096);
    if detail.is_empty() {
        base
    } else {
        format!("{base}:\n{detail}")
    }
}

// -- Daemon process groups ----------------------------------------------------

// DAEMON-12: every daemon's group, signalled only through its slot.
static ROUTER_GROUPS: morloc_runtime_types::child_group::ChildGroups =
    morloc_runtime_types::child_group::ChildGroups::new();

static ROUTER_DAEMONS: std::sync::Mutex<Vec<(i32, morloc_runtime_types::child_group::Registered<'static>)>> =
    std::sync::Mutex::new(Vec::new());

fn router_daemons() -> std::sync::MutexGuard<'static, Vec<(i32, morloc_runtime_types::child_group::Registered<'static>)>> {
    // PANIC-4
    ROUTER_DAEMONS.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock())
}

// DAEMON-12
fn signal_daemon(pid: i32, sig: libc::c_int) {
    if let Some((_, group)) = router_daemons().iter().find(|(p, _)| *p == pid) {
        group.signal(sig);
    }
}

/// Grace a stopped daemon has to clean up its pools before it is killed.
const DAEMON_STOP_GRACE: std::time::Duration = std::time::Duration::from_secs(2);

/// Stop and reap the program's daemon `pid`, unless another has replaced it.
// DAEMON-12: no daemon outlives the slot that records it.
unsafe fn stop_daemon(prog: *mut RouterProgram, pid: i32) {
    if pid <= 0 || (*prog).daemon_pid.load(std::sync::atomic::Ordering::SeqCst) != pid {
        return;
    }
    signal_daemon(pid, libc::SIGTERM);
    let until = std::time::Instant::now() + DAEMON_STOP_GRACE;
    while std::time::Instant::now() < until {
        if take_if_exited(&(*prog).daemon_pid, pid).is_some() {
            return;
        }
        std::thread::sleep(std::time::Duration::from_millis(20));
    }
    signal_daemon(pid, libc::SIGKILL);
    while take_if_exited(&(*prog).daemon_pid, pid).is_none() {
        std::thread::sleep(std::time::Duration::from_millis(5));
    }
}

// -- C-compatible types -------------------------------------------------------

#[repr(C)]
pub struct RouterProgram {
    pub name: *mut c_char,
    pub manifest_path: *mut c_char,
    pub manifest: *mut crate::manifest_ffi::Manifest,
    // PANIC-1: read by the panic and signal exits on any thread.
    pub daemon_pid: std::sync::atomic::AtomicI32,
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
    use crate::manifest_ffi::read_manifest;

    // NET-4
    let runtime_dir = match morloc_runtime_types::private_dir::runtime_dir() {
        Ok(dir) => dir,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Other(format!("no private directory for daemon sockets: {e}")));
            return ptr::null_mut();
        }
    };
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

        prog.daemon_pid.store(0, std::sync::atomic::Ordering::SeqCst);
        // Set socket path, refusing a program whose name makes it too long
        // to bind: a truncated path would collide or leave no terminator.
        // NET-4
        let socket_path = format!("{}/router-{}.sock", runtime_dir.display(), name_str);
        if let Err(e) = crate::utility::unix_socket_addr(socket_path.as_bytes()) {
            libc::free(prog.name as *mut c_void);
            libc::free(prog.manifest_path as *mut c_void);
            crate::manifest_ffi::free_manifest(prog.manifest);
            set_errmsg(errmsg, &MorlocError::Other(format!("program '{}': {}", name_str, e)));
            router_free(router);
            return ptr::null_mut();
        }
        ptr::copy_nonoverlapping(
            socket_path.as_ptr() as *const c_char,
            prog.daemon_socket.as_mut_ptr(),
            socket_path.len(),
        );

        (*router).n_programs += 1;
    }

    router
}

// Serve exactly the named programs under `fdb_path`. A named program that is not
// installed is an error. The only serve path: which modules are served is an
// explicit decision, never "whatever happens to be installed".
pub(crate) unsafe fn router_init_explicit(
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
/// `pid`'s wait status if it has exited (or no status when it is not
/// waitable), with `slot` cleared first: the child is reaped only after no
/// signal sent through `slot` can reach its pid, which the kernel does not
/// reissue until the reap.
unsafe fn take_if_exited(slot: &std::sync::atomic::AtomicI32, pid: i32) -> Option<Result<i32, ()>> {
    let mut info: libc::siginfo_t = std::mem::zeroed();
    let rc = loop {
        let rc = libc::waitid(libc::P_PID, pid as libc::id_t, &mut info, libc::WEXITED | libc::WNOHANG | libc::WNOWAIT);
        if rc == 0 || std::io::Error::last_os_error().kind() != std::io::ErrorKind::Interrupted {
            break rc;
        }
    };
    if rc == 0 && info.si_signo != libc::SIGCHLD {
        return None;
    }
    // DAEMON-12: the group's slot is dead before its leader's id is freed.
    ROUTER_GROUPS.leader_exited(pid);
    let _ = slot.compare_exchange(pid, 0, std::sync::atomic::Ordering::SeqCst, std::sync::atomic::Ordering::SeqCst);
    router_daemons().retain(|(p, _)| *p != pid);
    if rc != 0 {
        return Some(Err(()));
    }
    let mut status = 0;
    while libc::waitpid(pid, &mut status, 0) < 0 && std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted {}
    Some(Ok(status))
}

/// Whether child `pid` has exited (or is not this process's child), leaving
/// it unreaped.
fn has_exited(pid: i32) -> bool {
    let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
    let rc = unsafe { libc::waitid(libc::P_PID, pid as libc::id_t, &mut info, libc::WEXITED | libc::WNOHANG | libc::WNOWAIT) };
    rc != 0 || info.si_signo == libc::SIGCHLD
}

pub(crate) unsafe fn router_terminate_children(router: *mut Router) {
    if router.is_null() {
        return;
    }
    // DAEMON-12
    ROUTER_GROUPS.signal_all(libc::SIGTERM);
}

// -- router_free --------------------------------------------------------------

pub(crate) unsafe fn router_free(router: *mut Router) {
    if router.is_null() {
        return;
    }

    use crate::manifest_ffi::free_manifest;

    for i in 0..(*router).n_programs {
        let prog = &mut *(*router).programs.add(i);
        libc::free(prog.name as *mut c_void);
        libc::free(prog.manifest_path as *mut c_void);
        if !prog.manifest.is_null() {
            free_manifest(prog.manifest);
        }
        // DAEMON-12
        signal_daemon(prog.daemon_pid.load(std::sync::atomic::Ordering::SeqCst), libc::SIGTERM);
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

    let mut tried: Vec<String> = Vec::new();

    // 1. $MORLOC_NEXUS
    if let Some(p) = env_str("MORLOC_NEXUS") {
        if is_executable(&p) {
            return Ok(p);
        }
        tried.push(format!("$MORLOC_NEXUS={}", p));
    }

    // 2. $MORLOC_HOME/bin/morloc-nexus
    if let Some(h) = env_str("MORLOC_HOME") {
        let p = format!("{}/bin/morloc-nexus", h);
        if is_executable(&p) {
            return Ok(p);
        }
        tried.push(p);
    }

    // 3. Search $PATH
    if let Some(path) = env_str("PATH") {
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
    if let Some(h) = env_str("HOME") {
        let p = format!("{}/.local/bin/morloc-nexus", h);
        if is_executable(&p) {
            return Ok(p);
        }
        tried.push(p);
    }

    Err(tried)
}

// -- router_start_program -----------------------------------------------------

pub(crate) unsafe fn router_start_program(
    prog: *mut RouterProgram,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);

    match find_morloc_nexus() {
        Ok(nexus_path) => start_program_with(prog, errmsg, &nexus_path),
        Err(tried) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!(
                    "morloc-nexus binary not found; tried: {}",
                    tried.join(", ")
                )),
            );
            false
        }
    }
}

unsafe fn start_program_with(
    prog: *mut RouterProgram,
    errmsg: *mut *mut c_char,
    nexus_path: &str,
) -> bool {
    let c_nexus = CString::new(nexus_path).unwrap_or_default();

    // Capture the daemon's startup stderr to a host-visible file so a crash
    // surfaces the real cause (missing shared library, unreadable config, bad
    // manifest, ...) instead of only an exit status. The file lives under
    // MORLOC_STATE, which is the bind mount in the serve container (so it is also
    // readable from the host).
    let prog_name_str = CStr::from_ptr((*prog).name).to_string_lossy().into_owned();
    // NET-4
    let log_dir = match env_str("MORLOC_STATE").or_else(|| env_str("MORLOC_HOME")) {
        Some(base) => format!("{base}/logs"),
        None => match morloc_runtime_types::private_dir::runtime_dir() {
            Ok(dir) => format!("{}/logs", dir.display()),
            Err(e) => {
                set_errmsg(errmsg, &MorlocError::Other(format!("no private directory for daemon logs: {e}")));
                return false;
            }
        },
    };
    let _ = std::fs::create_dir_all(&log_dir);
    let stderr_log = format!("{log_dir}/{prog_name_str}.err");
    let c_stderr_log = CString::new(stderr_log.as_str()).unwrap_or_default();

    let arg_nexus = CString::new("morloc-nexus").unwrap();
    let arg_daemon = CString::new("daemon").unwrap();
    let arg_socket = CString::new("--socket").unwrap();
    let socket_path = CStr::from_ptr((*prog).daemon_socket.as_ptr());
    let argv = [
        arg_nexus.as_ptr(),
        arg_daemon.as_ptr(),
        (*prog).manifest_path as *const c_char,
        arg_socket.as_ptr(),
        socket_path.as_ptr(),
        ptr::null(),
    ];
    // The daemon ends when this process ends: it adopts our lifeline.
    let lifeline = crate::lifeline::Lifeline::get().ok();
    let env: Vec<CString> = std::env::vars_os()
        .filter(|(k, _)| k != crate::lifeline::ENV)
        .filter_map(|(k, v)| {
            let mut kv = k.into_encoded_bytes();
            kv.push(b'=');
            kv.extend(v.into_encoded_bytes());
            CString::new(kv).ok()
        })
        .chain(lifeline.map(|l| l.env_entry()))
        .collect();
    let envp: Vec<*const c_char> =
        env.iter().map(|e| e.as_ptr()).chain(std::iter::once(ptr::null())).collect();
    let lifeline_fd = lifeline.map_or(-1, |l| l.read_fd());
    let log_fd = if c_stderr_log.as_bytes().is_empty() {
        -1
    } else {
        libc::open(
            c_stderr_log.as_ptr(),
            libc::O_WRONLY | libc::O_CREAT | libc::O_APPEND | libc::O_CLOEXEC,
            0o644,
        )
    };
    let started = (|| {
        let mut spawn = morloc_runtime_types::spawn::Spawn::new()?;
        spawn.new_process_group()?;
        if log_fd >= 0 {
            spawn.dup2(log_fd, libc::STDERR_FILENO)?;
        }
        if lifeline_fd >= 0 {
            spawn.keep_across_exec(lifeline_fd)?;
        }
        spawn.run(&c_nexus, &argv, &envp, false)
    })();
    if log_fd >= 0 {
        libc::close(log_fd);
    }
    let pid = match started {
        Ok(pid) => pid,
        Err(e) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!("cannot start morloc-nexus for {prog_name_str}: {e}")),
            );
            return false;
        }
    };
    // DAEMON-12: the child is unreaped, so its group id is still its own.
    match ROUTER_GROUPS.add(pid) {
        Some(group) => router_daemons().push((pid, group)),
        None => {
            libc::kill(-pid, libc::SIGKILL);
            libc::waitpid(pid, ptr::null_mut(), 0);
            set_errmsg(errmsg, &MorlocError::Other(format!("cannot start a daemon for {prog_name_str}: too many daemons")));
            return false;
        }
    }
    (*prog).daemon_pid.store(pid, std::sync::atomic::Ordering::SeqCst);

    // Poll until the daemon socket is connectable (exponential backoff)
    let mut delay_ms = DAEMON_POLL_INITIAL_MS;
    let mut connected = false;
    let until = std::time::Instant::now() + DAEMON_START_LIMIT;
    while std::time::Instant::now() < until {
        std::thread::sleep(std::time::Duration::from_millis(delay_ms as u64));

        // Check if child died during startup
        if let Some(waited) = take_if_exited(&(*prog).daemon_pid, pid) {
            let status = waited.unwrap_or(0);
            let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
            let msg = startup_death_msg(&prog_name, status, &stderr_log);
            set_errmsg(errmsg, &MorlocError::Other(msg));
            return false;
        }

        // Try connecting to the daemon socket
        let test_sock = morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
        if test_sock >= 0 {
            // The path was checked to fit when the program was registered.
            let addr = crate::utility::unix_socket_addr(
                CStr::from_ptr((*prog).daemon_socket.as_ptr()).to_bytes(),
            )
            .expect("daemon socket path fits");
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

        delay_ms = (delay_ms * DAEMON_POLL_MULTIPLIER).min(DAEMON_POLL_MAX_MS);
    }

    if !connected {
        // Final check: did the daemon die?
        if let Some(waited) = take_if_exited(&(*prog).daemon_pid, pid) {
            let status = waited.unwrap_or(0);
            let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
            let msg = startup_death_msg(&prog_name, status, &stderr_log);
            set_errmsg(errmsg, &MorlocError::Other(msg));
            return false;
        }
        // DAEMON-12
        stop_daemon(prog, pid);
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!("the daemon for {prog_name_str} did not accept connections; it was stopped")),
        );
        return false;
    }

    true
}

// -- router_forward -----------------------------------------------------------

pub(crate) unsafe fn router_forward(
    router: *mut Router,
    program: *const c_char,
    request: *mut DaemonRequest,
    errmsg: *mut *mut c_char,
) -> *mut DaemonResponse {
    clear_errmsg(errmsg);

    use crate::daemon_ffi::daemon_parse_response;

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
    let pid = (*prog).daemon_pid.load(std::sync::atomic::Ordering::SeqCst);
    if pid > 0 {
        if let Some(waited) = take_if_exited(&(*prog).daemon_pid, pid) {
            let prog_name = CStr::from_ptr((*prog).name).to_string_lossy();
            let reason = match waited {
                Ok(status) => describe_wait_status(status),
                // The child was already reaped elsewhere, so there is no
                // status to decode.
                Err(()) => "is no longer waitable".to_string(),
            };
            eprintln!(
                "morloc-router: daemon for '{}' {}, will restart",
                prog_name, reason
            );
        }
    }

    // Start daemon if not running
    if (*prog).daemon_pid.load(std::sync::atomic::Ordering::SeqCst) <= 0 {
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
    let tried = (*prog).daemon_pid.load(std::sync::atomic::Ordering::SeqCst);
    let sock = connect_to_daemon(prog, errmsg);
    let sock = if sock < 0 {
        // DAEMON-12
        stop_daemon(prog, tried);
        // Clear previous error
        if !(*errmsg).is_null() {
            libc::free(*errmsg as *mut c_void);
            *errmsg = ptr::null_mut();
        }
        let mut child_err: *mut c_char = ptr::null_mut();
        // DAEMON-12: a daemon another request started meanwhile is used, not doubled.
        if (*prog).daemon_pid.load(std::sync::atomic::Ordering::SeqCst) <= 0 && !router_start_program(prog, &mut child_err) {
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

    // send_all retries on EAGAIN (a full send buffer surfaces as EWOULDBLOCK on
    // this SO_SNDTIMEO socket) instead of misreading it as a fatal error and
    // truncating the request mid-message.
    if !crate::ipc_ffi::send_all(sock, len_buf.as_ptr(), 4) {
        libc::close(sock);
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to send request length to daemon".into()),
        );
        return ptr::null_mut();
    }

    if !crate::ipc_ffi::send_all(sock, c_req.as_ptr() as *const u8, req_len) {
        libc::close(sock);
        set_errmsg(
            errmsg,
            &MorlocError::Other("Failed to send request body to daemon".into()),
        );
        return ptr::null_mut();
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
    let sock = morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
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

    // The path was checked to fit when the program was registered.
    let addr = crate::utility::unix_socket_addr(CStr::from_ptr((*prog).daemon_socket.as_ptr()).to_bytes())
        .expect("daemon socket path fits");

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

/// The daemon request on the wire. The args are forwarded as the text
/// they arrived in, so a value of any depth and an integer of any width
/// reach the far daemon exactly as the client sent them.
#[derive(serde::Serialize)]
struct ForwardRequest<'a> {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<String>,
    method: &'static str,
    #[serde(skip_serializing_if = "Option::is_none")]
    command: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    args: Option<&'a serde_json::value::RawValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    expr: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,
    media: bool,
}

/// Serialize a DaemonRequest to JSON.
unsafe fn serialize_request_to_json(request: *mut DaemonRequest) -> String {
    let owned = |p: *const c_char| (!p.is_null()).then(|| CStr::from_ptr(p).to_string_lossy().into_owned());
    let args_str = owned((*request).args_json);
    // Args text that is not JSON is dropped, as a request without args.
    let args = args_str
        .as_deref()
        .and_then(|s| serde_json::from_str::<&serde_json::value::RawValue>(s).ok());
    let req = ForwardRequest {
        id: owned((*request).id),
        method: match (*request).method {
            DaemonMethod::Call => "call",
            DaemonMethod::Discover => "discover",
            DaemonMethod::Health => "health",
            DaemonMethod::Eval => "eval",
            DaemonMethod::Typecheck => "typecheck",
            DaemonMethod::Bind => "bind",
            DaemonMethod::Bindings => "bindings",
            DaemonMethod::Unbind => "unbind",
        },
        command: owned((*request).command),
        args,
        expr: owned((*request).expr),
        name: owned((*request).name),
        // A forward is always a serving front-end call: request the
        // raw-media form so an `@mime` return arrives as bytes+mime.
        // Direct length-prefixed clients (which don't go through
        // router_forward) omit this and keep JSON `result`.
        media: true,
    };
    serde_json::to_string(&req).unwrap_or_else(|_| "{}".into())
}

// -- router_build_discovery ---------------------------------------------------

pub(crate) unsafe fn router_build_discovery(router: *mut Router) -> *mut c_char {
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
        let pid = prog.daemon_pid.load(std::sync::atomic::Ordering::SeqCst);
        let running = pid > 0 && !has_exited(pid);

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


#[cfg(test)]
mod forward_tests {
    use super::*;

    #[test]
    fn a_daemon_that_never_accepts_is_stopped_reaped_and_refused() {
        let dir = std::env::temp_dir().join(format!("mlc-router-never-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let script = dir.join("fake-nexus");
        crate::write_test_executable(&script, "#!/bin/sh\nexec sleep 60\n");
        let mut prog: RouterProgram = unsafe { std::mem::zeroed() };
        prog.name = unsafe { libc::strdup(c"never".as_ptr()) };
        prog.manifest_path = unsafe { libc::strdup(c"/nonexistent/manifest.json".as_ptr()) };
        let sock = format!("{}/never.sock", dir.display());
        for (d, b) in prog.daemon_socket.iter_mut().zip(sock.as_bytes()) {
            *d = *b as c_char;
        }
        let mut err: *mut c_char = ptr::null_mut();
        let started = unsafe { start_program_with(&mut prog, &mut err, script.to_str().unwrap()) };
        assert!(!started, "a daemon that never accepted was used");
        assert_eq!(prog.daemon_pid.load(std::sync::atomic::Ordering::SeqCst), 0);
        let msg = unsafe { CStr::from_ptr(err) }.to_string_lossy().into_owned();
        assert!(msg.contains("did not accept"), "{msg}");
        assert!(router_daemons().is_empty(), "the stopped daemon is still registered");
        unsafe {
            libc::free(err as *mut c_void);
            libc::free(prog.name as *mut c_void);
            libc::free(prog.manifest_path as *mut c_void);
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    #[test]
    fn an_exited_daemon_leaves_its_slot_before_it_is_reaped() {
        let child = std::process::Command::new("sh").args(["-c", "exit 3"]).spawn().unwrap();
        let pid = child.id() as i32;
        let slot = std::sync::atomic::AtomicI32::new(pid);
        let waited = loop {
            if let Some(w) = unsafe { take_if_exited(&slot, pid) } {
                break w;
            }
            std::thread::sleep(std::time::Duration::from_millis(5));
        };
        assert_eq!(waited.map(|s| libc::WEXITSTATUS(s)), Ok(3));
        assert_eq!(slot.load(std::sync::atomic::Ordering::SeqCst), 0);
        assert!(unsafe { take_if_exited(&slot, pid) }.is_some_and(|w| w.is_err()));
    }

    #[test]
    fn a_running_daemon_keeps_its_slot() {
        let mut child = std::process::Command::new("sleep").arg("5").spawn().unwrap();
        let pid = child.id() as i32;
        let slot = std::sync::atomic::AtomicI32::new(pid);
        assert!(unsafe { take_if_exited(&slot, pid) }.is_none());
        assert_eq!(slot.load(std::sync::atomic::Ordering::SeqCst), pid);
        let _ = child.kill();
        let _ = child.wait();
    }

    // A forwarded call carries its args as the text they arrived in.
    #[test]
    fn forwarded_args_are_the_text_the_client_sent() {
        let deep = format!("{}1{}", "[".repeat(400), "]".repeat(400));
        let args = format!("[18446744073709551617, {deep}]");
        let mut req: DaemonRequest = unsafe { std::mem::zeroed() };
        req.method = DaemonMethod::Call;
        let cmd = CString::new("f").unwrap();
        let a = CString::new(args.as_str()).unwrap();
        req.command = cmd.as_ptr() as *mut c_char;
        req.args_json = a.as_ptr() as *mut c_char;
        let json = unsafe { serialize_request_to_json(&mut req) };
        assert_eq!(json, format!("{{\"method\":\"call\",\"command\":\"f\",\"args\":{args},\"media\":true}}"));
    }
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub unsafe extern "C" fn router_init_explicit(fdb_path: *const c_char, names: *const *const c_char, n_names: usize, errmsg: *mut *mut c_char) -> *mut Router {
        super::router_init_explicit(fdb_path, names, n_names, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn router_terminate_children(router: *mut Router) {
        super::router_terminate_children(router)
    }

    #[no_mangle]
    pub unsafe extern "C" fn router_free(router: *mut Router) {
        super::router_free(router)
    }

    #[no_mangle]
    pub unsafe extern "C" fn router_start_program(prog: *mut RouterProgram, errmsg: *mut *mut c_char) -> bool {
        super::router_start_program(prog, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn router_forward(router: *mut Router, program: *const c_char, request: *mut DaemonRequest, errmsg: *mut *mut c_char) -> *mut DaemonResponse {
        super::router_forward(router, program, request, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn router_build_discovery(router: *mut Router) -> *mut c_char {
        super::router_build_discovery(router)
    }
}
