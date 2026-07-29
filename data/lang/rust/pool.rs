// Rust pool template for the CAbi family (own-pool, Stage A).
//
// The Rust translator (Members/Rust.hs + RustPrinter.hs) splices five
// sections into this file at the section markers below, in order:
//   1. sourced Rust modules  (`source Rust from "..."` -> include!)
//   2. schema table + per-record marshalling impls
//   3. compile-time type-assertion shims (empty in v1)
//   4. manifold function definitions
//   5. local_dispatch / remote_dispatch
//
// The fixed scaffold below owns the C ABI declarations and main(), which
// fills a PoolConfig and hands control to pool_main in libmorloc.so. main()
// mirrors pool_host.cpp: --health probe, PDEATHSIG, panic hook, schema init.
// NOTE: the marker string must not appear anywhere above the first real
// marker (the splicer splits on every occurrence).
#![allow(dead_code, unused_variables, unused_unsafe, unused_mut, non_snake_case, unused_imports, unused_parens)]

use std::ffi::{CString};
use std::os::raw::{c_char, c_int, c_ulong, c_void};
use std::sync::OnceLock;
use rustmorloc::{parse_schema, Schema, ToVoidstar, FromVoidstar, RecurScope, resolve_recur};

// Declared directly (rather than via the `libc` crate) so the generated pool
// has exactly two direct rlib dependencies (rustmorloc, morloc_runtime_types),
// each pinned by path in the bare-rustc build -- avoiding `libc` crate-name
// ambiguity across the many hashed rlibs staged in rust-deps.
extern "C" {
    fn pool_main(argc: c_int, argv: *mut *mut c_char, config: *mut PoolConfig) -> c_int;
    fn fdopen(fd: c_int, mode: *const c_char) -> *mut c_void;
    fn setvbuf(stream: *mut c_void, buf: *mut c_char, mode: c_int, size: usize) -> c_int;
    fn prctl(option: c_int, a2: c_ulong, a3: c_ulong, a4: c_ulong, a5: c_ulong) -> c_int;
}

// glibc constants (stable on Linux): _IOLBF = 1, SIGTERM = 15,
// PR_SET_PDEATHSIG = 1.
const IOLBF: c_int = 1;
const SIGTERM: c_ulong = 15;
const PR_SET_PDEATHSIG: c_int = 1;

#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
enum PoolConcurrency { Threads = 0, Fork = 1, Single = 2 }

type PoolDispatchFn =
    unsafe extern "C" fn(u32, *const *const u8, usize, *mut c_void) -> *mut u8;

#[repr(C)]
struct PoolConfig {
    local_dispatch: PoolDispatchFn,
    remote_dispatch: PoolDispatchFn,
    dispatch_ctx: *mut c_void,
    concurrency: PoolConcurrency,
    initial_workers: i32,
    dynamic_scaling: bool,
    post_fork_child: Option<unsafe extern "C" fn(*mut c_void)>,
}

// <<<BREAK>>>
// <<<BREAK>>>
// <<<BREAK>>>
// <<<BREAK>>>
// <<<BREAK>>>

fn cstdio_stderr() -> *mut c_void {
    unsafe { fdopen(2, b"w\0".as_ptr() as *const c_char) }
}

fn main() {
    unsafe { setvbuf(cstdio_stderr(), std::ptr::null_mut(), IOLBF, 0); }
    #[cfg(target_os = "linux")]
    unsafe { prctl(PR_SET_PDEATHSIG, SIGTERM, 0, 0, 0); }

    let raw: Vec<String> = std::env::args().collect();
    if raw.len() == 2 && raw[1] == "--health" {
        println!("{{\"status\":\"ok\",\"version\":\"__MORLOC_VERSION__\"}}");
        return;
    }

    rustmorloc::install_panic_hook();
    init_schemas();
    // argv is `<socket_path> <tmpdir> <shm_basename>`; record the tmpdir so
    // foreign calls can resolve peer-pool socket paths.
    if raw.len() > 2 {
        rustmorloc::set_tmpdir(&raw[2]);
    }

    let mut argv: Vec<*mut c_char> = raw
        .iter()
        .map(|a| CString::new(a.as_str()).unwrap().into_raw())
        .collect();
    let mut cfg = PoolConfig {
        local_dispatch,
        remote_dispatch,
        dispatch_ctx: std::ptr::null_mut(),
        concurrency: PoolConcurrency::Threads,
        initial_workers: 1,
        dynamic_scaling: true,
        post_fork_child: None,
    };
    let rc = unsafe { pool_main(argv.len() as c_int, argv.as_mut_ptr(), &mut cfg) };
    std::process::exit(rc);
}
