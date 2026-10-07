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
// mirrors pool_host.cpp: --health probe, lifeline, panic hook, schema init.
// NOTE: the marker string must not appear anywhere above the first real
// marker (the splicer splits on every occurrence).
#![allow(dead_code, unused_variables, unused_unsafe, unused_mut, non_snake_case, non_camel_case_types, unused_imports, unused_parens)]

use std::ffi::{CString};
use std::os::raw::{c_char, c_int, c_void};
use std::sync::OnceLock;
use rustmorloc::{parse_schema, Schema, ToVoidstar, FromVoidstar, RecurScope, resolve_recur};
use rustmorloc::{SizeWalk, WriteWalk, ReadWalk, write_variant_nullary, read_variant_tag};
// Function-value traits: a closure is applied as `f.callN(..)` (the trait method
// must be in scope). The blanket impl covers native closures; boxed function
// values (record fields) dispatch through the trait object.
use rustmorloc::{MorlocFn0, MorlocFn1, MorlocFn2, MorlocFn3, MorlocFn4, MorlocFn5, MorlocFn6, MorlocFn7, MorlocFn8};

// Declared directly (rather than via the `libc` crate) so the generated pool
// has exactly two direct rlib dependencies (rustmorloc, morloc_runtime_types),
// each pinned by path in the bare-rustc build -- avoiding `libc` crate-name
// ambiguity across the many hashed rlibs staged in rust-deps.
#[cfg(not(panic = "unwind"))]
compile_error!("morloc needs panic = \"unwind\" (model/panic.md PANIC-8)");

// PANIC-9: a frame of the pool file above the panic machinery, which the
// classifier needs to trust a backtrace's paths; black_box keeps the call
// from becoming a tail call, which would leave no frame here.
#[inline(never)]
extern "C" fn mlc_classify_panic(file: *const u8, len: usize) -> bool {
    std::hint::black_box(rustmorloc::panic_is_runtime(file, len))
}

extern "C" {
    fn morloc_set_panic_classifier(classify: Option<extern "C" fn(*const u8, usize) -> bool>);
    fn pool_main(argc: c_int, argv: *mut *mut c_char, config: *mut PoolConfig) -> c_int;
    fn morloc_lifeline_guard();
}


#[repr(C)]
#[derive(Clone, Copy, PartialEq)]
enum PoolConcurrency { Threads = 0, Single = 1 }

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
    release_dispatch: Option<unsafe extern "C" fn()>,
}

// <<<BREAK>>>
// <<<BREAK>>>
// <<<BREAK>>>
// <<<BREAK>>>
// <<<BREAK>>>

fn main() {
    unsafe { morloc_lifeline_guard(); }

    let raw: Vec<String> = std::env::args().collect();
    if raw.len() == 2 && raw[1] == "--health" {
        println!("{{\"status\":\"ok\",\"version\":\"__MORLOC_VERSION__\"}}");
        return;
    }

    rustmorloc::install_crash_handler();
    rustmorloc::install_panic_hook();
    // PANIC-9
    rustmorloc::register_pool_files(&[file!(), concat!(env!("CARGO_MANIFEST_DIR"), "/", file!())], MLC_USER_FILES, MLC_USER_CALLS);
    unsafe { morloc_set_panic_classifier(Some(mlc_classify_panic)) };
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
    unsafe extern "C" fn release_dispatch() {
        rustmorloc::dispatch_flush();
    }
    let mut cfg = PoolConfig {
        local_dispatch,
        remote_dispatch,
        dispatch_ctx: std::ptr::null_mut(),
        concurrency: PoolConcurrency::Threads,
        initial_workers: 1,
        dynamic_scaling: true,
        release_dispatch: Some(release_dispatch),
    };
    let rc = unsafe { pool_main(argv.len() as c_int, argv.as_mut_ptr(), &mut cfg) };
    std::process::exit(rc);
}
