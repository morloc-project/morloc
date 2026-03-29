//! Morloc Nexus: CLI dispatcher for multi-language pool orchestration.
//!
//! Replaces data/nexus.c. Entry point for all morloc programs.
//! Reads a .manifest JSON, spawns language pool daemons, and routes
//! function calls to them over Unix sockets.

mod dispatch;
mod help;
mod manifest;
mod process;

use dispatch::NexusConfig;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut config = NexusConfig::default();

    // First pass: parse nexus-level options
    let opt_end = dispatch::parse_nexus_options(&args, &mut config);

    // Handle --router mode (no manifest needed)
    if config.router_flag {
        run_router(&config);
        std::process::exit(0);
    }

    // If -h with no manifest argument, show nexus help
    let prog_name = args.first().map(|s| s.as_str()).unwrap_or("morloc-nexus");
    if config.help_flag && opt_end >= args.len() {
        help::print_nexus_usage(prog_name);
    }

    // Manifest path: either an explicit argument or derived from argv[0].
    // In daemon mode (`./test --daemon`), the manifest is at `<argv[0]>.manifest`.
    // In normal mode (`./test add 1 2`), argv[0] is also the manifest source.
    // An explicit path argument is only needed for multi-command mode.
    let manifest_path = if opt_end < args.len() {
        args[opt_end].clone()
    } else if config.daemon_flag || config.router_flag {
        // Daemon/router: derive from argv[0]
        args[0].clone()
    } else {
        help::print_nexus_usage(prog_name)
    };
    let prog_name = manifest_path.clone();
    let mut arg_cursor = if opt_end < args.len() { opt_end + 1 } else { args.len() };

    // Read and parse manifest
    let payload = match manifest::read_manifest_payload(&manifest_path) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Failed to load manifest '{}': {}", manifest_path, e);
            std::process::exit(1);
        }
    };

    let manifest = match manifest::parse_manifest(&payload) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("Failed to parse manifest '{}': {}", manifest_path, e);
            std::process::exit(1);
        }
    };

    let single_command = manifest.commands.len() == 1 && manifest.groups.is_empty();

    // Second pass: parse options after manifest path (skip in single-command mode)
    let mut remaining_args = args.clone();
    if !single_command {
        arg_cursor = dispatch::parse_nexus_options(&args[opt_end..], &mut config) + opt_end;
    } else {
        // In single-command mode, extract daemon/server long options manually
        dispatch::extract_global_options(&mut remaining_args, &mut config);
    }

    // chdir to build directory
    if let Some(ref build_dir) = manifest.build_dir {
        if std::env::set_current_dir(build_dir).is_err() {
            eprintln!("Cannot chdir to build_dir '{}': {}", build_dir, std::io::Error::last_os_error());
            std::process::exit(1);
        }
    }

    // Validate pool executables exist
    if let Err(e) = process::validate_pools(&manifest.pools) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }

    // Handle help flag with manifest loaded
    if config.help_flag {
        if single_command {
            help::print_command_help_single(&prog_name, &manifest.commands[0]);
        } else {
            help::print_usage(&prog_name, &manifest);
        }
    }

    // Setup tmpdir and SHM
    let tmpdir = match process::make_tmpdir() {
        Ok(t) => t,
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    };
    process::set_tmpdir(tmpdir.clone());

    let job_hash = process::make_job_hash(42);
    let shm_basename = format!("morloc-{}", job_hash);

    // Initialize shared memory via libmorloc.so using dlsym.
    // CRITICAL: We must use dlsym to call the CDYLIB's shinit, not the rlib's.
    // The rlib and cdylib have separate static globals (VOLUMES, ALLOC_MUTEX, etc.).
    // All SHM operations in pool-facing C code go through the cdylib's globals.
    // If we call the rlib's shinit, the cdylib's globals stay uninitialized.
    {
        let _lib = unsafe { libc::dlopen(std::ptr::null(), libc::RTLD_NOW) };
        // RTLD_DEFAULT (NULL handle) searches in order: executable, then loaded libs
        // But the rlib symbols come first. Use RTLD_NEXT-style lookup via the .so path.
        let lib_path = std::ffi::CString::new(
            format!("{}/.local/share/morloc/lib/libmorloc.so",
                    std::env::var("HOME").unwrap_or("/root".into()))
        ).unwrap();
        let lib = unsafe { libc::dlopen(lib_path.as_ptr(), libc::RTLD_NOW | libc::RTLD_GLOBAL) };
        if lib.is_null() {
            let err = unsafe { libc::dlerror() };
            let err_msg = if err.is_null() {
                "unknown error".to_string()
            } else {
                unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy().into_owned()
            };
            eprintln!("Error: failed to load libmorloc.so: {}", err_msg);
            process::clean_exit(1);
        }

        type ShmSetFallbackFn = unsafe extern "C" fn(*const std::ffi::c_char);
        type ShinitFn = unsafe extern "C" fn(*const std::ffi::c_char, usize, usize, *mut *mut std::ffi::c_char) -> *mut std::ffi::c_void;

        let set_fb_sym = std::ffi::CString::new("shm_set_fallback_dir").unwrap();
        let shinit_sym = std::ffi::CString::new("shinit").unwrap();

        let set_fb: ShmSetFallbackFn = unsafe { std::mem::transmute(libc::dlsym(lib, set_fb_sym.as_ptr())) };
        let do_shinit: ShinitFn = unsafe { std::mem::transmute(libc::dlsym(lib, shinit_sym.as_ptr())) };

        let tmpdir_c = std::ffi::CString::new(tmpdir.as_str()).unwrap();
        let basename_c = std::ffi::CString::new(shm_basename.as_str()).unwrap();
        let mut errmsg: *mut std::ffi::c_char = std::ptr::null_mut();
        unsafe {
            set_fb(tmpdir_c.as_ptr());
            let shm = do_shinit(basename_c.as_ptr(), 0, 0xffff, &mut errmsg);
            if shm.is_null() {
                let msg = if !errmsg.is_null() {
                    let s = std::ffi::CStr::from_ptr(errmsg).to_string_lossy().into_owned();
                    libc::free(errmsg as *mut std::ffi::c_void);
                    s
                } else {
                    "unknown error".into()
                };
                eprintln!("Error: failed to initialize shared memory: {}", msg);
                process::clean_exit(1);
            }
        }
        unsafe { libc::dlclose(lib) };
    }

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

        // Build DaemonConfig and call daemon_run in libmorloc.so
        run_daemon(&config, &mut sockets, &shm_basename, &payload);
        process::clean_exit(0);
    }

    // Normal CLI mode
    if config.packet_path.is_none() {
        if single_command {
            // Single-command: dispatch directly to the command, no subcommand lookup
            // Allow optional command name prefix for backward compatibility
            let mut cmd_arg_start = arg_cursor;
            if cmd_arg_start < remaining_args.len()
                && remaining_args[cmd_arg_start] == manifest.commands[0].name
            {
                cmd_arg_start += 1;
            }
            dispatch::dispatch_command(
                &remaining_args,
                cmd_arg_start,
                &config,
                &manifest,
                &manifest.commands[0],
                &mut sockets,
                &prog_name,
            );
        } else {
            if arg_cursor >= remaining_args.len() {
                help::print_usage(&prog_name, &manifest);
            }
            dispatch::dispatch(
                &remaining_args,
                arg_cursor,
                &shm_basename,
                &config,
                &manifest,
                &mut sockets,
                &prog_name,
            );
        }
    } else {
        // Call-packet mode: read a pre-built call packet from file,
        // send to the appropriate pool, write result as MessagePack.
        // Used by SLURM workers on remote compute nodes.
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

    // Build C DaemonConfig (matches daemon_ffi::DaemonConfig layout)
    #[repr(C)]
    struct CDaemonConfig {
        unix_socket_path: *const c_char,
        tcp_port: i32,
        http_port: i32,
        pool_check_fn: *const c_void,   // Option<fn> as null
        pool_alive_fn: *const c_void,   // Option<fn> as null
        n_pools: usize,
        eval_timeout: i32,
    }

    let unix_socket_cstr = config.unix_socket_path.as_ref()
        .map(|p| CString::new(p.as_str()).unwrap());

    let mut daemon_config = CDaemonConfig {
        unix_socket_path: unix_socket_cstr.as_ref()
            .map_or(ptr::null(), |c| c.as_ptr()),
        tcp_port: config.tcp_port.unwrap_or(0),
        http_port: config.http_port.unwrap_or(0),
        pool_check_fn: ptr::null(),
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
        let home = std::env::var("HOME").unwrap_or_else(|_| "/root".into());
        format!("{}/.local/share/morloc/fdb", home)
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

    // Build DaemonConfig for the router
    #[repr(C)]
    struct CDaemonConfig {
        unix_socket_path: *const c_char,
        tcp_port: i32,
        http_port: i32,
        pool_check_fn: *const c_void,
        pool_alive_fn: *const c_void,
        n_pools: usize,
        eval_timeout: i32,
    }

    let unix_cstr = config.unix_socket_path.as_ref()
        .map(|p| CString::new(p.as_str()).unwrap());

    let mut dc = CDaemonConfig {
        unix_socket_path: unix_cstr.as_ref().map_or(ptr::null(), |c| c.as_ptr()),
        tcp_port: config.tcp_port.unwrap_or(0),
        http_port: config.http_port.unwrap_or(0),
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

/// Run a pre-built call packet on a remote worker node (SLURM mode).
/// Reads a call packet from file, sends it to the pool, writes result as MessagePack.
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
        fn get_morloc_data_packet_error_message(
            data: *const u8, errmsg: *mut *mut c_char,
        ) -> *mut c_char;
        fn read_schema_from_packet_meta(
            packet: *const u8, errmsg: *mut *mut c_char,
        ) -> *mut c_char;
        fn parse_schema(
            schema_str: *const c_char, errmsg: *mut *mut c_char,
        ) -> *mut morloc_runtime::cschema::CSchema;
        fn get_morloc_data_packet_value(
            data: *const u8, schema: *const morloc_runtime::cschema::CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
        fn pack_with_schema(
            mlc: *const c_void, schema: *const morloc_runtime::cschema::CSchema,
            mpkptr: *mut *mut c_char, mpk_size: *mut usize, errmsg: *mut *mut c_char,
        ) -> i32;
        fn write_atomic(
            filename: *const c_char, data: *const u8, size: usize, errmsg: *mut *mut c_char,
        ) -> i32;
        fn print_morloc_data_packet(
            packet: *const u8, schema: *const morloc_runtime::cschema::CSchema,
            errmsg: *mut *mut c_char,
        ) -> i32;
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
    let call_packet = unsafe { read_binary_file(packet_c.as_ptr(), &mut packet_size, &mut errmsg) };
    if call_packet.is_null() || !errmsg.is_null() {
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

    // Send to pool and receive response
    let result_packet = unsafe {
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

    // If output-form is "packet", write raw packet to output file
    if config.output_format == dispatch::OutputFormat::Packet {
        if let Some(ref output_path) = config.output_path {
            let schema_str = unsafe { read_schema_from_packet_meta(result_packet, &mut errmsg) };
            let schema = if !schema_str.is_null() {
                unsafe { parse_schema(schema_str, &mut errmsg) }
            } else {
                ptr::null_mut()
            };
            unsafe {
                print_morloc_data_packet(result_packet, schema, &mut errmsg);
            };
            // Also write as msgpack file
            if !schema.is_null() {
                let mlc = unsafe { get_morloc_data_packet_value(result_packet, schema, &mut errmsg) };
                if !mlc.is_null() && errmsg.is_null() {
                    let mut mpk_data: *mut c_char = ptr::null_mut();
                    let mut mpk_size: usize = 0;
                    unsafe { pack_with_schema(mlc as *const c_void, schema, &mut mpk_data, &mut mpk_size, &mut errmsg) };
                    if !mpk_data.is_null() && errmsg.is_null() {
                        let mpk_filename = format!("{}.mpk", output_path);
                        let mpk_c = CString::new(mpk_filename.as_str()).unwrap();
                        unsafe { write_atomic(mpk_c.as_ptr(), mpk_data as *const u8, mpk_size, &mut errmsg) };
                        unsafe { libc::free(mpk_data as *mut c_void) };
                    }
                }
            }
        }
    }

    unsafe { libc::free(result_packet as *mut c_void) };
}
