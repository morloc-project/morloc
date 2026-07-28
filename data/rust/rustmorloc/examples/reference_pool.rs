// Reference hand-authored Rust pool -- the exact shape the Rust translator
// (Members/Rust.hs + RustPrinter.hs) must emit. Integrates the rustmorloc
// marshaller against the real libmorloc C ABI. Doubles as the pool.rs template:
// the five sections marked <<<BREAK>>> are what printProgram splices.
#![allow(dead_code, unused_variables, unused_unsafe, non_snake_case, unused_imports)]

use std::ffi::{c_char, c_void, CString};
use std::os::raw::c_int;
use std::sync::OnceLock;
use morloc_runtime_types::schema::{parse_schema, Schema};

extern "C" {
    fn pool_main(argc: c_int, argv: *mut *mut c_char, config: *mut PoolConfig) -> c_int;
}

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

// <<<BREAK>>> section 1: sourced Rust modules (user `source Rust from "..."`).
mod usersrc {
    pub fn add(x: i64, y: i64) -> i64 { x + y }
    pub fn mean_vec(xs: Vec<f64>) -> f64 {
        if xs.is_empty() { return 0.0; }
        xs.iter().sum::<f64>() / xs.len() as f64
    }
    // exercises tuple + option marshalling through the real SHM path
    pub fn minmax(xs: Vec<i64>) -> (i64, i64) {
        let lo = *xs.iter().min().unwrap_or(&0);
        let hi = *xs.iter().max().unwrap_or(&0);
        (lo, hi)
    }
    pub fn safe_head(xs: Vec<i64>) -> Option<i64> {
        xs.first().copied()
    }
}

// <<<BREAK>>> section 2: schema table + per-record marshalling impls.
// Schemas are interned by the translator; index = schema id.
static SCHEMA_STRS: &[&str] = &[
    "i8",   // 0: Int  (add arg0/arg1/result)
    "af8",  // 1: [Real]  (mean_vec arg)
    "f8",   // 2: Real    (mean_vec result)
    "ai8",  // 3: [Int]   (minmax/safe_head arg)
    "t2i8i8", // 4: (Int,Int)  (minmax result)
    "?i8",  // 5: ?Int    (safe_head result)
];

static SCHEMA_TABLE: OnceLock<Vec<Schema>> = OnceLock::new();

fn init_schemas() {
    let table: Vec<Schema> = SCHEMA_STRS
        .iter()
        .map(|s| parse_schema(s).expect("morloc: invalid embedded schema"))
        .collect();
    let _ = SCHEMA_TABLE.set(table);
}

#[inline]
fn schema(id: usize) -> &'static Schema {
    &SCHEMA_TABLE.get().expect("schemas not initialized")[id]
}

// (no records in this reference; per-record `impl ToVoidstar/FromVoidstar`
//  would land here, generated exactly like the rustmorloc LL test.)

// <<<BREAK>>> section 3: signatures / compile-time type-assertion shims (I-E1).
// A mismatch between the sourced fn and the declared morloc type fails here,
// localized, instead of deep in a marshalling instantiation.
const _: fn(i64, i64) -> i64 = usersrc::add;
const _: fn(Vec<f64>) -> f64 = usersrc::mean_vec;
const _: fn(Vec<i64>) -> (i64, i64) = usersrc::minmax;
const _: fn(Vec<i64>) -> Option<i64> = usersrc::safe_head;

// <<<BREAK>>> section 4: manifolds. Each dispatched (serial) manifold takes arg
// packets, deserializes, calls the native body, and serializes the result.
unsafe fn m0(a0: *const u8, a1: *const u8) -> *mut u8 {
    let x: i64 = rustmorloc::get_value(a0, schema(0));
    let y: i64 = rustmorloc::get_value(a1, schema(0));
    let r: i64 = usersrc::add(x, y);
    rustmorloc::put_value(&r, schema(0))
}
unsafe fn m1(a0: *const u8) -> *mut u8 {
    let xs: Vec<f64> = rustmorloc::get_value(a0, schema(1));
    let r: f64 = usersrc::mean_vec(xs);
    rustmorloc::put_value(&r, schema(2))
}
unsafe fn m2(a0: *const u8) -> *mut u8 {
    let xs: Vec<i64> = rustmorloc::get_value(a0, schema(3));
    let r: (i64, i64) = usersrc::minmax(xs);
    rustmorloc::put_value(&r, schema(4))
}
unsafe fn m3(a0: *const u8) -> *mut u8 {
    let xs: Vec<i64> = rustmorloc::get_value(a0, schema(3));
    let r: Option<i64> = usersrc::safe_head(xs);
    rustmorloc::put_value(&r, schema(5))
}

// <<<BREAK>>> section 5: dispatch. Flush deferred SHM (I3) at entry; run each
// arm under the panic->fail-packet guard (I2).
unsafe extern "C" fn local_dispatch(mid: u32, args: *const *const u8, nargs: usize, _ctx: *mut c_void) -> *mut u8 {
    rustmorloc::dispatch_flush();
    let args = std::panic::AssertUnwindSafe(args);
    rustmorloc::dispatch_guard(move || {
        let args = *args;
        let a = |i: usize| -> *const u8 { if i < nargs { *args.add(i) } else { std::ptr::null() } };
        match mid {
            0 => m0(a(0), a(1)),
            1 => m1(a(0)),
            2 => m2(a(0)),
            3 => m3(a(0)),
            _ => rustmorloc::fail_packet(&format!("Invalid local manifold id: {mid}")),
        }
    })
}
unsafe extern "C" fn remote_dispatch(mid: u32, args: *const *const u8, nargs: usize, ctx: *mut c_void) -> *mut u8 {
    local_dispatch(mid, args, nargs, ctx)
}

// Host: main() owns argv parsing + pool_main (own-pool, Stage A -- no separate
// host TU needed). Mirrors pool_host.cpp (--health, PDEATHSIG, usage).
fn main() {
    unsafe { libc::setvbuf(cstdio_stderr(), std::ptr::null_mut(), libc::_IOLBF, 0); }
    #[cfg(target_os = "linux")]
    unsafe { libc::prctl(libc::PR_SET_PDEATHSIG, libc::SIGTERM as libc::c_ulong, 0, 0, 0); }

    let raw: Vec<String> = std::env::args().collect();
    if raw.len() == 2 && raw[1] == "--health" {
        println!("{{\"status\":\"ok\",\"version\":\"__MORLOC_VERSION__\"}}");
        return;
    }

    rustmorloc::install_panic_hook();
    init_schemas();

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

extern "C" {
    fn fdopen(fd: c_int, mode: *const c_char) -> *mut libc::FILE;
}
fn cstdio_stderr() -> *mut libc::FILE {
    unsafe { fdopen(2, b"w\0".as_ptr() as *const c_char) }
}
