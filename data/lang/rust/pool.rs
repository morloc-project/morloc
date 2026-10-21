// Rust pool template for the CAbi family (own-pool, Stage A).
//
// The Rust translator (Members/Rust.hs + RustPrinter.hs) splices five
// sections into this file at the section markers below, in order:
//   1. sourced Rust modules  (`source Rust from "..."` -> include!)
//   2. schema table + per-record marshalling impls
//   3. compile-time type-assertion shims (empty in v1)
//   4. manifold function definitions
//   5. __morloc_local_dispatch / __morloc_remote_dispatch
//
// The fixed scaffold below owns the C ABI declarations and main(), which
// fills a __MorlocPoolConfig and hands control to pool_main in libmorloc.so. main()
// mirrors pool_host.cpp: --health probe, panic hook, schema init; pool_main
// watches the nexus's lifeline.
// NOTE: the marker string must not appear anywhere above the first real
// marker (the splicer splits on every occurrence).
#![allow(dead_code, unused_variables, unused_unsafe, unused_mut, non_snake_case, non_camel_case_types, unused_imports, unused_parens)]

// Sourced files are included at the crate root, so this file binds no name a
// user file might: runtime items are written by path, its traits are in scope
// unnamed (for method calls), and its own items start with __morloc.
use ::rustmorloc::{ToVoidstar as _, FromVoidstar as _};

// Declared directly (rather than via the `libc` crate) so the generated pool
// has exactly two direct rlib dependencies (rustmorloc, morloc_runtime_types),
// each pinned by path in the bare-rustc build -- avoiding `libc` crate-name
// ambiguity across the many hashed rlibs staged in rust-deps.
#[cfg(not(panic = "unwind"))]
compile_error!("morloc needs panic = \"unwind\" (model/runtime/panic.md PANIC-8)");

// PANIC-9: a frame of the pool file above the panic machinery, which the
// classifier needs to trust a backtrace's paths; black_box keeps the call
// from becoming a tail call, which would leave no frame here.
#[inline(never)]
extern "C" fn __morloc_classify_panic(file: *const u8, len: usize) -> bool {
    ::std::hint::black_box(::rustmorloc::panic_is_runtime(file, len))
}

extern "C" {
    #[link_name = "morloc_set_panic_classifier"]
    fn __morloc_set_panic_classifier(classify: ::std::option::Option<extern "C" fn(*const u8, usize) -> bool>);
    #[link_name = "pool_main"]
    fn __morloc_pool_main(argc: ::std::os::raw::c_int, argv: *mut *mut ::std::os::raw::c_char, config: *mut __MorlocPoolConfig) -> ::std::os::raw::c_int;
}


type __MorlocPoolDispatchFn =
    unsafe extern "C" fn(u32, *const *const u8, usize, *mut ::std::os::raw::c_void) -> *mut u8;

#[repr(C)]
struct __MorlocPoolConfig {
    local_dispatch: __MorlocPoolDispatchFn,
    remote_dispatch: __MorlocPoolDispatchFn,
    dispatch_ctx: *mut ::std::os::raw::c_void,
    release_dispatch: ::std::option::Option<unsafe extern "C" fn()>,
}

// <<<BREAK>>>
// <<<BREAK>>>
// <<<BREAK>>>
// <<<BREAK>>>
// <<<BREAK>>>

fn main() {
    let raw: ::std::vec::Vec<::std::string::String> = ::std::env::args().collect();
    if raw.len() == 2 && raw[1] == "--health" {
        ::std::println!("{{\"status\":\"ok\",\"version\":\"__MORLOC_VERSION__\"}}");
        return;
    }

    ::rustmorloc::install_crash_handler();
    ::rustmorloc::install_panic_hook();
    // PANIC-9
    ::rustmorloc::register_pool_files(&[::std::file!(), ::std::concat!(::std::env!("CARGO_MANIFEST_DIR"), "/", ::std::file!())], __MORLOC_USER_FILES, __MORLOC_USER_CALLS);
    unsafe { __morloc_set_panic_classifier(::std::option::Option::Some(__morloc_classify_panic)) };
    __morloc_init_schemas();
    // argv is `<socket_path> <tmpdir> <shm_basename>`; record the tmpdir so
    // foreign calls can resolve peer-pool socket paths.
    if raw.len() > 2 {
        ::rustmorloc::set_tmpdir(&raw[2]);
    }

    let mut argv: ::std::vec::Vec<*mut ::std::os::raw::c_char> = raw
        .iter()
        .map(|a| ::std::ffi::CString::new(a.as_str()).unwrap().into_raw())
        .collect();
    unsafe extern "C" fn release_dispatch() {
        ::rustmorloc::dispatch_flush();
    }
    let mut cfg = __MorlocPoolConfig {
        local_dispatch: __morloc_local_dispatch,
        remote_dispatch: __morloc_remote_dispatch,
        dispatch_ctx: ::std::ptr::null_mut(),
        release_dispatch: ::std::option::Option::Some(release_dispatch),
    };
    let rc = unsafe { __morloc_pool_main(argv.len() as ::std::os::raw::c_int, argv.as_mut_ptr(), &mut cfg) };
    ::std::process::exit(rc);
}
