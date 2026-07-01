//! Morloc Nexus: CLI dispatcher for multi-language pool orchestration.
//!
//! Replaces data/nexus.c. Entry point for all morloc programs.
//! Reads a .manifest JSON, spawns language pool daemons, and routes
//! function calls to them over Unix sockets.

mod cli;
mod dispatch;
mod file;
mod help;
mod loader;
mod manifest;
mod phase2;
mod process;
mod runlog;
mod schemas;
mod view;

use dispatch::NexusConfig;

/// Resolve the morloc data directory: MORLOC_HOME if set, else ~/.local/share/morloc.
fn morloc_home() -> String {
    std::env::var("MORLOC_HOME").unwrap_or_else(|_| {
        format!(
            "{}/.local/share/morloc",
            std::env::var("HOME").unwrap_or_else(|_| "/root".into())
        )
    })
}

fn main() {
    // Install a panic hook so a Rust panic still runs the run-scope
    // epilogue + summary.json + tee cleanup before the process dies.
    // Without this, panic-unwound exits skip clean_exit entirely and
    // leave operators with a half-written rundir. Restore the
    // default hook for the panic message itself so stack traces keep
    // working under RUST_BACKTRACE=1.
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let msg = format!("nexus panicked: {}", info);
        runlog::record_error(&msg);
        default_hook(info);
        process::clean_exit(101);
    }));

    // Top-level argv parse. [`cli::parse_invocation`] handles the
    // pre-scan for `@` separator (run mode), loads the manifest from
    // the wrapper target, and runs the augmented clap parse with
    // capability flags filtered by the manifest's advertised
    // capabilities. Routes through the static `Nexus` derive for
    // router mode and top-level help.
    let config: NexusConfig;
    let manifest_path: String;
    let user_zone: Vec<String>;

    let invocation = cli::parse_invocation();

    // `file` and `view` need no manifest, no pool spawning, no signal
    // handlers. Dispatch them before the manifest-mode plumbing below.
    // `view` does need SHM because the loader may produce SHM-backed
    // voidstar values.
    match invocation.nexus.cmd {
        cli::Mode::File(ref fargs) => file::run(fargs),
        cli::Mode::View(ref vargs) => {
            process::init_shm();
            view::run(vargs);
        }
        _ => {}
    }

    let mut manifest = match invocation.manifest {
        Some(m) => m,
        None => {
            // Router mode or top-level help: still need to dispatch
            // via the static `Nexus`. The legacy router branch below
            // handles the only non-help case; help paths exited
            // inside parse_invocation.
            match invocation.nexus.cmd {
                cli::Mode::Router(rargs) => {
                    let cfg = cli::router_args_to_config(&rargs);
                    run_router(&cfg);
                    std::process::exit(0);
                }
                _ => {
                    // clap rendered help and the process exited
                    // inside parse_invocation; if we got here, the
                    // user invoked a mode that needs a manifest
                    // without supplying one and we should hand
                    // control back to clap for a proper error.
                    eprintln!("Error: missing required target argument");
                    std::process::exit(2);
                }
            }
        }
    };

    match invocation.nexus.cmd {
        cli::Mode::Router(rargs) => {
            let cfg = cli::router_args_to_config(&rargs);
            run_router(&cfg);
            std::process::exit(0);
        }
        cli::Mode::Run(rargs) => {
            let (mut cfg, _target) = cli::run_args_to_config(&rargs);
            cfg.debug_cache_depth = invocation.capability_values.debug_cache_depth;
            cfg.debug_cache_max = invocation.capability_values.debug_cache_max;
            cfg.debug_recursion_cap = invocation.capability_values.debug_recursion_cap;
            config = cfg;
            manifest_path = invocation.manifest_path.clone();
            user_zone = invocation.user_zone;
        }
        cli::Mode::Daemon(dargs) => {
            let (mut cfg, _target) = cli::daemon_args_to_config(&dargs);
            cfg.debug_cache_depth = invocation.capability_values.debug_cache_depth;
            cfg.debug_cache_max = invocation.capability_values.debug_cache_max;
            cfg.debug_recursion_cap = invocation.capability_values.debug_recursion_cap;
            config = cfg;
            manifest_path = invocation.manifest_path.clone();
            user_zone = Vec::new();
        }
        cli::Mode::File(_) | cli::Mode::View(_) => unreachable!(
            "File/View dispatched before manifest setup"
        ),
    }

    let prog_name = std::path::Path::new(&manifest_path)
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or(&manifest_path)
        .to_string();

    // Re-read the raw manifest payload string from disk. The
    // structured manifest was already loaded and validated inside
    // parse_invocation; the payload is used downstream by
    // `daemon_run` (which parses the JSON via libmorloc.so for its
    // C-layout Manifest type).
    let payload = match manifest::read_manifest_payload(&manifest_path) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Failed to load manifest '{}': {}", manifest_path, e);
            std::process::exit(1);
        }
    };
    // Touch `manifest` to keep the binding live for the rest of
    // main.
    let _ = &mut manifest;

    // Forward the per-program inline-vs-shm routing knobs to libmorloc.
    // Env vars are inherited by every pool process the nexus later
    // execvp's, and libmorloc reads them once on first packet operation
    // (via packet::ensure_config_loaded). Setting both before the
    // shinit call below guarantees the nexus's own libmorloc.so sees
    // them too (its first read of the atomics will trip the Once and
    // pull these values in).
    if let Some(n) = manifest.inline_size {
        std::env::set_var("MORLOC_INLINE_SIZE", n.to_string());
    }
    if manifest.no_shm {
        std::env::set_var("MORLOC_NO_SHM", "1");
    }
    if let Some(ref dir) = manifest.tmpdir {
        std::env::set_var("MORLOC_TMPDIR", dir);
    }

    // Propagate nexus + manifest absolute paths to pools. SLURM remote
    // dispatch (libmorloc.so::remote_call) needs MORLOC_NEXUS_PATH to
    // wrap a sbatch invocation of `<nexus> --call-packet ...`. Both
    // paths are also useful handles for any future per-program tooling
    // that needs to re-enter the same nexus from a worker.
    if let Ok(exe) = std::env::current_exe() {
        std::env::set_var("MORLOC_NEXUS_PATH", &exe);
    }
    if let Ok(abs_manifest) = std::fs::canonicalize(&manifest_path) {
        std::env::set_var("MORLOC_MANIFEST_PATH", &abs_manifest);
    } else {
        std::env::set_var("MORLOC_MANIFEST_PATH", &manifest_path);
    }

    // Publish the run-scope activation env vars NOW (after both option
    // passes) so the runtime sees the final config when morloc_run_init
    // performs its env::var reads. Each flag/var is only set when
    // explicitly requested:
    //
    //   * --log-dir / MORLOC_LOG_DIR -- activates rundir, log tee,
    //     summary.json (under rundir unless --summary overrides).
    //   * --summary / MORLOC_SUMMARY -- writes summary.json to a
    //     specific path; works even without a rundir.
    //   * --quiet  / MORLOC_QUIET   -- suppresses ALL morloc-emitted
    //     log lines (prologue, epilogue, per-label).
    //
    // CLI flags win over env vars. Env vars that pre-existed without
    // a matching flag are left untouched so pools inherit them.
    if let Some(ref d) = config.log_dir {
        std::env::set_var("MORLOC_LOG_DIR", d);
    }
    if let Some(ref p) = config.summary_path {
        std::env::set_var("MORLOC_SUMMARY", p);
    }
    if config.quiet {
        std::env::set_var("MORLOC_QUIET", "1");
    }
    if let Some(n) = config.debug_cache_depth {
        std::env::set_var("MORLOC_DEBUG_CACHE_DEPTH", n.to_string());
    }
    if let Some(n) = config.debug_cache_max {
        std::env::set_var("MORLOC_DEBUG_CACHE_MAX", n.to_string());
    }
    if let Some(n) = config.debug_recursion_cap {
        std::env::set_var("MORLOC_DEBUG_RECURSION_CAP", n.to_string());
    }

    // Resolve the per-run identity now so pools inherit a fully-published
    // env (MORLOC_RUN_DIR / MORLOC_RUN_BASE / MORLOC_RUN_PARENT_PID).
    // No filesystem directory is materialized here -- creation is lazy
    // on the first writer (log tee, future cache or SLURM artifacts).
    // A nested invocation (this nexus's parent is itself a morloc
    // process) detects that via MORLOC_RUN_PARENT_PID matching getppid()
    // and inherits the parent's run dir; otherwise a fresh id is minted.
    extern "C" {
        fn morloc_run_init();
    }
    unsafe { morloc_run_init() };

    // Capture run-scope log templates from the manifest. emit_prologue
    // / emit_epilogue read this; clean_exit triggers emit_epilogue
    // before morloc_run_finalize writes summary.json.
    runlog::install(manifest.run_log.clone());

    // Pool paths in the manifest are absolute, so no chdir is needed.
    // This lets user programs resolve file paths relative to the caller's CWD.
    // Source imports in pools resolve via __file__-relative paths (Python sys.path)
    // or script-relative paths (R .morloc.source) rather than depending on CWD.

    // Validate pool executables exist
    if let Err(e) = process::validate_pools(&manifest.pools) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }

    // Initialize shared memory. The nexus no longer statically links
    // morloc-runtime as an rlib, so the `shinit` FFI symbol resolves at
    // load time via DT_NEEDED against the single process-shared copy in
    // libmorloc.so. The dlsym workaround that used to live here is gone
    // -- it existed only to bypass symbol shadowing by the rlib's static
    // copy of shinit, which is now absent.
    let (tmpdir, shm_basename) = process::init_shm();

    // Become subreaper for orphaned grandchildren
    process::set_child_subreaper();

    // Install signal handlers
    process::install_signal_handlers();

    // Setup sockets
    let mut sockets = process::setup_sockets(&manifest.pools, &tmpdir, &shm_basename);

    // Daemon mode
    if config.daemon_flag {
        let all_indices: Vec<usize> = (0..manifest.pools.len()).collect();
        if let Err(e) = process::start_daemons(&mut sockets, &all_indices) {
            eprintln!("Error: {}", e);
            process::clean_exit(1);
        }

        // Install the recovery context so the pool_check_fn callback can
        // rebuild syscmds with a fresh basename and respawn pools after a
        // pool crash. Done before daemon_run so the callback (invoked from
        // the daemon's main loop) sees a populated context.
        process::install_recovery_context(
            sockets.clone(),
            manifest.pools.clone(),
            tmpdir.clone(),
            shm_basename.clone(),
        );

        // Build DaemonConfig and call daemon_run in libmorloc.so
        run_daemon(&config, &mut sockets, &shm_basename, &payload);
        process::clean_exit(0);
    }

    // Normal CLI mode
    if config.packet_path.is_none() {
        // Scoped to normal CLI dispatch only: call-packet mode already
        // honors output_path internally (writes a sibling .mpk file
        // via write_atomic) and must not have its stdout hijacked.
        process::redirect_stdout_to(config.output_path.as_deref());

        // Route through the manifest-driven parser. `user_zone`
        // holds the post-`@` (or post-target) slice that the
        // top-level parse handed off; clap's auto-help and unknown-
        // flag rejection apply to that slice against the
        // manifest's command surface. parse_run also extracts
        // capability-gated flags (`--debug-*`) into `config`.
        let parsed =
            phase2::parse_run(&manifest, &user_zone, &prog_name);
        let cmd = &manifest.commands[parsed.cmd_index];
        dispatch::dispatch_command_parsed(
            parsed.values,
            &config,
            &manifest,
            cmd,
            &mut sockets,
        );
    } else {
        // Call-packet mode: read a pre-built call packet from file,
        // send to the appropriate pool, write the result packet.
        // Used by SLURM workers on remote compute nodes.
        //
        // Compute-node nexuses come up cold -- no driver-side pool is
        // reachable here -- so each manifest-declared pool whose
        // socket isn't already alive needs to be started. The
        // already-alive check is what makes same-host development /
        // mock-test setups still work: when a parent nexus is the one
        // that fork-exec'd this call-packet nexus (e.g. a local SLURM
        // mock), the parent's pool is at the target socket already
        // and we must not try to bind a second daemon to the same
        // path. Pools we DID start die via PR_SET_PDEATHSIG when
        // clean_exit drops this process.
        let to_start: Vec<usize> = (0..manifest.pools.len())
            .filter(|&i| !socket_is_alive(&sockets[i].socket_path))
            .collect();
        if !to_start.is_empty() {
            if let Err(e) = process::start_daemons(&mut sockets, &to_start) {
                eprintln!("Error: failed to start pools for call-packet dispatch: {}", e);
                process::clean_exit(1);
            }
        }
        run_call_packet(&config, &tmpdir);
    }

    process::clean_exit(0);
}

/// Run the daemon event loop by calling daemon_run in libmorloc.so.
fn run_daemon(
    config: &dispatch::NexusConfig,
    sockets: &mut [process::PoolSocket],
    shm_basename: &str,
    manifest_payload: &str,
) {
    use std::ffi::{c_char, c_void, CString};
    use std::ptr;

    // daemon_run and parse_manifest signatures from libmorloc.so
    extern "C" {
        fn daemon_run(
            config: *mut c_void,        // *mut DaemonConfig
            manifest: *mut c_void,       // *mut Manifest (opaque)
            sockets: *mut c_void,        // *mut MorlocSocket
            n_pools: usize,
            shm_basename: *const c_char,
        );
        fn parse_manifest(text: *const c_char, errmsg: *mut *mut c_char) -> *mut c_void;
    }

    // Build C MorlocSocket array (matches daemon_ffi::MorlocSocket layout)
    #[repr(C)]
    struct CMorlocSocket {
        lang: *mut c_char,
        syscmd: *mut *mut c_char,
        socket_filename: *mut c_char,
        pid: i32,
    }

    let n_pools = sockets.len();
    let mut c_sockets: Vec<CMorlocSocket> = Vec::with_capacity(n_pools);
    // Keep CStrings alive for the duration
    let mut _keepalive: Vec<Vec<CString>> = Vec::new();

    for sock in sockets.iter() {
        let lang_c = CString::new(sock.lang.as_str()).unwrap();
        let socket_c = CString::new(sock.socket_path.as_str()).unwrap();

        // Build NULL-terminated syscmd array
        let mut cmd_ptrs: Vec<*mut c_char> = Vec::new();
        let mut cmd_strs: Vec<CString> = Vec::new();
        for arg in &sock.syscmd {
            let c = CString::new(arg.to_bytes()).unwrap();
            cmd_ptrs.push(c.as_ptr() as *mut c_char);
            cmd_strs.push(c);
        }
        cmd_ptrs.push(ptr::null_mut());

        c_sockets.push(CMorlocSocket {
            lang: lang_c.as_ptr() as *mut c_char,
            syscmd: cmd_ptrs.as_ptr() as *mut *mut c_char,
            socket_filename: socket_c.as_ptr() as *mut c_char,
            pid: sock.pid,
        });

        // Keep everything alive
        _keepalive.push(cmd_strs);
        _keepalive.push(vec![lang_c, socket_c]);
    }

    // Build C DaemonConfig (matches daemon_ffi::DaemonConfig layout).
    // Port sentinel: -1 = listener not configured; 0..=65535 = configured
    // (0 means bind ephemeral). The pre-ephemeral code used 0 as "not
    // configured", which conflicts with the standard "0 = OS picks port"
    // idiom.
    #[repr(C)]
    struct CDaemonConfig {
        unix_socket_path: *const c_char,
        tcp_port: i32,
        http_port: i32,
        port_file_path: *const c_char,
        pool_check_fn: *const c_void,   // Option<fn> as null
        pool_alive_fn: *const c_void,   // Option<fn> as null
        n_pools: usize,
        eval_timeout: i32,
    }

    let unix_socket_cstr = config.unix_socket_path.as_ref()
        .map(|p| CString::new(p.as_str()).unwrap());
    let port_file_cstr = config.port_file_path.as_ref()
        .map(|p| CString::new(p.as_str()).unwrap());

    let mut daemon_config = CDaemonConfig {
        unix_socket_path: unix_socket_cstr.as_ref()
            .map_or(ptr::null(), |c| c.as_ptr()),
        tcp_port: config.tcp_port.unwrap_or(-1),
        http_port: config.http_port.unwrap_or(-1),
        port_file_path: port_file_cstr.as_ref()
            .map_or(ptr::null(), |c| c.as_ptr()),
        pool_check_fn: process::pool_check_and_recover_ptr(),
        pool_alive_fn: process::pool_is_alive_ptr(),
        n_pools,
        eval_timeout: config.eval_timeout,
    };

    // Parse manifest via the C FFI (so daemon_run gets the C-layout manifest).
    // The payload was already extracted from the wrapper script by the main flow.
    let manifest_c_str = CString::new(manifest_payload).unwrap();
    let mut errmsg: *mut c_char = ptr::null_mut();
    let c_manifest = unsafe { parse_manifest(manifest_c_str.as_ptr(), &mut errmsg) };
    if c_manifest.is_null() {
        let msg = if !errmsg.is_null() {
            let s = unsafe { std::ffi::CStr::from_ptr(errmsg) }.to_string_lossy().into_owned();
            unsafe { libc::free(errmsg as *mut c_void) };
            s
        } else {
            "unknown error".into()
        };
        eprintln!("Error: failed to parse manifest for daemon: {}", msg);
        process::clean_exit(1);
    }

    let shm_c = CString::new(shm_basename).unwrap();

    unsafe {
        daemon_run(
            &mut daemon_config as *mut CDaemonConfig as *mut c_void,
            c_manifest,
            c_sockets.as_mut_ptr() as *mut c_void,
            n_pools,
            shm_c.as_ptr(),
        );
    }
}

/// Run the multi-program router daemon.
/// Scans the fdb directory for .manifest files and serves them all via HTTP/TCP/Unix.
fn run_router(config: &dispatch::NexusConfig) {
    use std::ffi::{c_char, c_void, CString};
    use std::ptr;

    extern "C" {
        fn router_init(fdb_path: *const c_char, errmsg: *mut *mut c_char) -> *mut c_void;
        fn router_run(config: *mut c_void, router: *mut c_void);
        fn router_free(router: *mut c_void);
    }

    let fdb_path = config.fdb_path.clone().unwrap_or_else(|| {
        format!("{}/fdb", morloc_home())
    });
    let fdb_c = CString::new(fdb_path.as_str()).unwrap();

    let mut errmsg: *mut c_char = ptr::null_mut();
    let router = unsafe { router_init(fdb_c.as_ptr(), &mut errmsg) };
    if router.is_null() {
        let msg = if !errmsg.is_null() {
            let s = unsafe { std::ffi::CStr::from_ptr(errmsg) }.to_string_lossy().into_owned();
            unsafe { libc::free(errmsg as *mut c_void) };
            s
        } else {
            "unknown error".into()
        };
        eprintln!("Error: failed to initialize router: {}", msg);
        std::process::exit(1);
    }

    // Build DaemonConfig for the router. Port sentinel: -1 = not
    // configured; 0..=65535 = configured (0 means ephemeral).
    #[repr(C)]
    struct CDaemonConfig {
        unix_socket_path: *const c_char,
        tcp_port: i32,
        http_port: i32,
        port_file_path: *const c_char,
        pool_check_fn: *const c_void,
        pool_alive_fn: *const c_void,
        n_pools: usize,
        eval_timeout: i32,
    }

    let unix_cstr = config.unix_socket_path.as_ref()
        .map(|p| CString::new(p.as_str()).unwrap());
    let port_file_cstr = config.port_file_path.as_ref()
        .map(|p| CString::new(p.as_str()).unwrap());

    let mut dc = CDaemonConfig {
        unix_socket_path: unix_cstr.as_ref().map_or(ptr::null(), |c| c.as_ptr()),
        tcp_port: config.tcp_port.unwrap_or(-1),
        http_port: config.http_port.unwrap_or(-1),
        port_file_path: port_file_cstr.as_ref()
            .map_or(ptr::null(), |c| c.as_ptr()),
        pool_check_fn: ptr::null(),
        pool_alive_fn: ptr::null(),
        n_pools: 0,
        eval_timeout: if config.eval_timeout > 0 { config.eval_timeout } else { 30 },
    };

    unsafe {
        router_run(&mut dc as *mut CDaemonConfig as *mut c_void, router);
        router_free(router);
    }
}

/// Quick liveness probe for a pool socket: try to connect; if the
/// connect succeeds the socket file exists and something is listening.
/// Used by call-packet mode to skip starting pools that a parent
/// process (typical in same-host SLURM-mock setups) already has live.
fn socket_is_alive(path: &str) -> bool {
    std::os::unix::net::UnixStream::connect(path).is_ok()
}

/// Run a pre-built call packet on a remote worker node (SLURM mode).
///
/// Reads a call packet from file, sends it to the local pool, and
/// writes the result back to `--output-file`. The output form is the
/// raw morloc packet: a 32-byte header plus `header.offset +
/// header.length` payload bytes. The driver-side `slurm_ffi::remote_call`
/// reads this file with `read_binary_file` and feeds the bytes back up
/// the call chain as the result of the labeled call.
fn run_call_packet(config: &dispatch::NexusConfig, tmpdir: &str) {
    use std::ffi::{c_char, c_void, CString};
    use std::ptr;

    extern "C" {
        fn read_binary_file(
            filename: *const c_char, file_size: *mut usize, errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn send_and_receive_over_socket(
            socket_path: *const c_char, packet: *const u8, errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn morloc_packet_size(
            packet: *const u8, errmsg: *mut *mut c_char,
        ) -> usize;
        fn get_morloc_data_packet_error_message(
            data: *const u8, errmsg: *mut *mut c_char,
        ) -> *mut c_char;
        fn normalize_data_packet_to_fd(
            packet: *const u8, packet_size: usize, compression_level: u8,
            fd: libc::c_int, errmsg: *mut *mut c_char,
        ) -> i64;
    }

    let packet_path = config.packet_path.as_ref().unwrap();
    let socket_base = match &config.socket_base {
        Some(s) => s.clone(),
        None => {
            eprintln!("Error: --socket-base required for call-packet mode");
            process::clean_exit(1);
        }
    };
    let socket_path = format!("{}/{}", tmpdir, socket_base);

    let packet_c = CString::new(packet_path.as_str()).unwrap();
    let socket_c = CString::new(socket_path.as_str()).unwrap();
    let mut errmsg: *mut c_char = ptr::null_mut();

    // Read call packet from file
    let mut packet_size: usize = 0;
    let raw_packet = unsafe { read_binary_file(packet_c.as_ptr(), &mut packet_size, &mut errmsg) };
    if raw_packet.is_null() || !errmsg.is_null() {
        let msg = if !errmsg.is_null() {
            let s = unsafe { std::ffi::CStr::from_ptr(errmsg) }.to_string_lossy().into_owned();
            unsafe { libc::free(errmsg as *mut c_void) };
            s
        } else {
            "unknown error".into()
        };
        eprintln!("Error: failed to read call packet '{}': {}", packet_path, msg);
        process::clean_exit(1);
    }

    // If the on-disk packet was written with -z, transparently
    // decompress before validating + forwarding. decompress_packet_if_needed
    // returns Cow::Borrowed for plain packets, so the common case keeps
    // the original read_binary_file buffer with zero extra copies.
    let (call_packet, packet_size): (*mut u8, usize) = {
        let raw_slice = unsafe { std::slice::from_raw_parts(raw_packet, packet_size) };
        match morloc_runtime_types::compression::decompress_packet_if_needed(raw_slice) {
            Ok(std::borrow::Cow::Borrowed(_)) => (raw_packet, packet_size),
            Ok(std::borrow::Cow::Owned(bytes)) => {
                let buf = unsafe { libc::malloc(bytes.len()) as *mut u8 };
                if buf.is_null() {
                    unsafe { libc::free(raw_packet as *mut c_void) };
                    eprintln!("Error: malloc failed for call packet");
                    process::clean_exit(1);
                }
                unsafe {
                    std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf, bytes.len());
                    libc::free(raw_packet as *mut c_void);
                }
                (buf, bytes.len())
            }
            Err(e) => {
                eprintln!(
                    "Error: failed to decompress call packet '{}': {}",
                    packet_path, e
                );
                unsafe { libc::free(raw_packet as *mut c_void) };
                process::clean_exit(1);
            }
        }
    };

    // Structural check on the bytes we just read from the shared FS. A
    // torn or corrupted call packet would otherwise propagate into the
    // pool dispatch as garbage offsets / lengths and crash the worker.
    {
        let slice = unsafe { std::slice::from_raw_parts(call_packet, packet_size) };
        if let Err(e) = morloc_runtime_types::packet::validate_packet(slice) {
            eprintln!(
                "Error: call packet '{}' is malformed: {}",
                packet_path, e
            );
            unsafe { libc::free(call_packet as *mut c_void) };
            process::clean_exit(1);
        }
    }

    // Send to pool and receive response
    let mut result_packet = unsafe {
        send_and_receive_over_socket(socket_c.as_ptr(), call_packet, &mut errmsg)
    };
    unsafe { libc::free(call_packet as *mut c_void) };
    if result_packet.is_null() || !errmsg.is_null() {
        let msg = if !errmsg.is_null() {
            let s = unsafe { std::ffi::CStr::from_ptr(errmsg) }.to_string_lossy().into_owned();
            unsafe { libc::free(errmsg as *mut c_void) };
            s
        } else {
            "unknown error".into()
        };
        eprintln!("Error: run failed: {}", msg);
        process::clean_exit(1);
    }

    // Check for error in response
    let run_err = unsafe { get_morloc_data_packet_error_message(result_packet, &mut errmsg) };
    if !run_err.is_null() {
        let s = unsafe { std::ffi::CStr::from_ptr(run_err) }.to_string_lossy().into_owned();
        unsafe { libc::free(run_err as *mut c_void) };
        eprintln!("Error: run failed: {}", s);
        process::clean_exit(1);
    }

    // Cross-nexus return egress: rewrite any TAG_HANDLE stream-handle
    // field in the pool's voidstar return payload to TAG_PATH so the
    // parent nexus (which doesn't share our SHM registry) can open it
    // locally. No-op for non-voidstar returns; a null out_ptr signals
    // the packet was left unchanged and we keep our existing buffer.
    {
        extern "C" {
            fn mlc_rewrite_packet_for_persistence(
                packet: *const u8, packet_len: usize,
                out_ptr: *mut *mut u8, out_len: *mut usize,
                errmsg: *mut *mut c_char,
            ) -> i32;
        }
        let packet_size = unsafe { morloc_packet_size(result_packet, &mut errmsg) };
        if errmsg.is_null() && packet_size > 0 {
            let mut rewritten_ptr: *mut u8 = std::ptr::null_mut();
            let mut rewritten_len: usize = 0;
            let mut rerr: *mut c_char = std::ptr::null_mut();
            let rc = unsafe {
                mlc_rewrite_packet_for_persistence(
                    result_packet,
                    packet_size,
                    &mut rewritten_ptr,
                    &mut rewritten_len,
                    &mut rerr,
                )
            };
            if rc != 0 {
                let msg = if !rerr.is_null() {
                    let s = unsafe {
                        std::ffi::CStr::from_ptr(rerr).to_string_lossy().into_owned()
                    };
                    unsafe { libc::free(rerr as *mut c_void) };
                    s
                } else {
                    "unknown error".into()
                };
                eprintln!("Error: return-egress rewrite failed: {}", msg);
                unsafe { libc::free(result_packet as *mut c_void) };
                process::clean_exit(1);
            }
            if !rewritten_ptr.is_null() {
                unsafe { libc::free(result_packet as *mut c_void) };
                result_packet = rewritten_ptr;
            }
        }
        if !errmsg.is_null() {
            unsafe { libc::free(errmsg as *mut c_void) };
            errmsg = std::ptr::null_mut();
        }
    }

    // Write the morloc result packet to --output-file. The driver-side
    // `slurm_ffi::remote_call` (libmorloc.so) reads exactly this file
    // via `read_binary_file` and feeds the bytes back up the call
    // chain. The fd-based normalizer streams the payload straight to
    // disk when the result is large enough; for atomicity we stream
    // into a sibling temp file and rename on success.
    if config.output_format == dispatch::OutputFormat::Packet {
        if let Some(ref output_path) = config.output_path {
            let packet_size = unsafe { morloc_packet_size(result_packet, &mut errmsg) };
            if !errmsg.is_null() {
                let msg = unsafe { std::ffi::CStr::from_ptr(errmsg) }.to_string_lossy().into_owned();
                unsafe { libc::free(errmsg as *mut c_void) };
                eprintln!("Error: morloc_packet_size failed: {}", msg);
                unsafe { libc::free(result_packet as *mut c_void) };
                process::clean_exit(1);
            }

            let output_path_buf = std::path::PathBuf::from(output_path);
            let parent = output_path_buf
                .parent()
                .filter(|p| !p.as_os_str().is_empty())
                .map(std::path::Path::to_path_buf)
                .unwrap_or_else(|| std::path::PathBuf::from("."));
            let basename = output_path_buf
                .file_name()
                .map(|s| s.to_string_lossy().into_owned())
                .unwrap_or_else(|| "out".to_string());
            let tmp_path = parent.join(format!(
                ".{}.tmp.{}",
                basename,
                std::process::id()
            ));

            let tmp_file = match std::fs::File::create(&tmp_path) {
                Ok(f) => f,
                Err(e) => {
                    eprintln!(
                        "Error: failed to open temp file '{}': {}",
                        tmp_path.display(), e
                    );
                    unsafe { libc::free(result_packet as *mut c_void) };
                    process::clean_exit(1);
                }
            };
            let fd = {
                use std::os::unix::io::AsRawFd;
                tmp_file.as_raw_fd()
            };

            let mut nerr: *mut c_char = std::ptr::null_mut();
            let n = unsafe {
                normalize_data_packet_to_fd(
                    result_packet,
                    packet_size,
                    config.compression_level,
                    fd,
                    &mut nerr,
                )
            };
            // sync_all before rename so a crash between rename and
            // fsync doesn't yield a zero-length result file the
            // driver would treat as success.
            let sync_result = tmp_file.sync_all();
            drop(tmp_file);

            if n < 0 || sync_result.is_err() {
                let _ = std::fs::remove_file(&tmp_path);
                let msg = if !nerr.is_null() {
                    let s = unsafe { std::ffi::CStr::from_ptr(nerr) }.to_string_lossy().into_owned();
                    unsafe { libc::free(nerr as *mut c_void) };
                    s
                } else if let Err(e) = sync_result {
                    format!("sync_all failed: {}", e)
                } else {
                    "unknown error".into()
                };
                eprintln!("Error: packet normalization failed: {}", msg);
                unsafe { libc::free(result_packet as *mut c_void) };
                process::clean_exit(1);
            }

            if let Err(e) = std::fs::rename(&tmp_path, &output_path_buf) {
                let _ = std::fs::remove_file(&tmp_path);
                eprintln!(
                    "Error: failed to rename '{}' to '{}': {}",
                    tmp_path.display(), output_path, e
                );
                unsafe { libc::free(result_packet as *mut c_void) };
                process::clean_exit(1);
            }
        }
    }

    unsafe { libc::free(result_packet as *mut c_void) };
}
