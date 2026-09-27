//! The stage of a multi-output run (see `orchestrate`).
//!
//! The stage child runs the parent command once and saves what the
//! terminal actions will read:
//!
//! - `value.pkt`: the parent's result, for a command that returns a value;
//! - `stream.pkt`: everything the parent streams to stdout, for a
//!   `@collect` command, one frame per batch (the pools write batch by
//!   batch under `MORLOC_STDOUT_STAGE`, see the runtime's `open_stdio`);
//! - `arg<N>.pkt`: each parent argument an action refers to as `$N`.
//!
//! With `tee`, the output also goes to stdout exactly as an ordinary run
//! writes it; otherwise stdout gets nothing from the nexus.

use std::sync::OnceLock;

struct Stage {
    dir: String,
    args: Vec<usize>,
    tee: bool,
}

static STAGE: OnceLock<Stage> = OnceLock::new();

/// Enter stage mode. Must run before any pool starts, since the pools read
/// `MORLOC_STDOUT_STAGE` from their environment.
pub fn init(dir: &str, args: &[usize], tee: bool) {
    std::env::set_var("MORLOC_STDOUT_STAGE", "1");
    let _ = STAGE.set(Stage { dir: dir.to_string(), args: args.to_vec(), tee });
}

pub fn active() -> bool {
    STAGE.get().is_some()
}

/// Whether the nexus writes the command's output to stdout. Always, outside
/// a stage.
pub fn stdout_on() -> bool {
    STAGE.get().map(|s| s.tee).unwrap_or(true)
}

pub fn value_path(dir: &str) -> String {
    format!("{}/value.pkt", dir)
}

pub fn stream_path(dir: &str) -> String {
    format!("{}/stream.pkt", dir)
}

pub fn arg_path(dir: &str, n: usize) -> String {
    format!("{}/arg{}.pkt", dir, n)
}

extern "C" {
    fn normalize_data_packet_to_fd(
        packet: *const u8,
        packet_size: usize,
        compression_level: u8,
        fd: libc::c_int,
        errmsg: *mut *mut std::ffi::c_char,
    ) -> i64;
    fn mlc_write_voidstar_data_packet_to_fd(
        data: *const std::ffi::c_void,
        schema: *const morloc_runtime_types::cschema::CSchema,
        level: u8,
        fd: libc::c_int,
        errmsg: *mut *mut std::ffi::c_char,
    ) -> i64;
    fn mlc_rewrite_packet_for_persistence(
        packet: *const u8,
        packet_len: usize,
        out_ptr: *mut *mut u8,
        out_len: *mut usize,
        errmsg: *mut *mut std::ffi::c_char,
    ) -> i32;
}

/// Write a self-contained data packet to `path`: `packet` when there is
/// one, else a packet built from the voidstar `ptr`. A stream handle in the
/// value is rewritten to its path, since the reader is another process.
fn write_packet(
    path: &str,
    packet: &[u8],
    ptr: *const u8,
    schema: *const morloc_runtime_types::cschema::CSchema,
) -> Result<(), String> {
    use std::os::unix::io::AsRawFd;
    let file = std::fs::File::create(path).map_err(|e| format!("{}: {}", path, e))?;
    let fd = file.as_raw_fd();
    let mut err: *mut std::ffi::c_char = std::ptr::null_mut();
    let n = if packet.is_empty() {
        unsafe { mlc_write_voidstar_data_packet_to_fd(ptr as *const _, schema, 0, fd, &mut err) }
    } else {
        let mut out: *mut u8 = std::ptr::null_mut();
        let mut out_len: usize = 0;
        let rc = unsafe {
            mlc_rewrite_packet_for_persistence(
                packet.as_ptr(), packet.len(), &mut out, &mut out_len, &mut err,
            )
        };
        if rc != 0 {
            return Err(crate::process::take_c_errmsg(err).unwrap_or_else(|| "unknown error".into()));
        }
        let n = if out.is_null() {
            unsafe { normalize_data_packet_to_fd(packet.as_ptr(), packet.len(), 0, fd, &mut err) }
        } else {
            let n = unsafe { normalize_data_packet_to_fd(out, out_len, 0, fd, &mut err) };
            unsafe { libc::free(out as *mut std::ffi::c_void) };
            n
        };
        n
    };
    if n < 0 {
        return Err(crate::process::take_c_errmsg(err).unwrap_or_else(|| "unknown error".into()));
    }
    Ok(())
}

/// Save the parent's result for the actions. No-op outside a stage.
pub fn save_value(
    packet: &[u8],
    ptr: *const u8,
    schema: *const morloc_runtime_types::cschema::CSchema,
) {
    if let Some(s) = STAGE.get() {
        if let Err(e) = write_packet(&value_path(&s.dir), packet, ptr, schema) {
            eprintln!("Error: saving the command's result: {}", e);
            crate::process::clean_exit(1);
        }
    }
}

/// Save argument `i` (0-based) of the parent when an action refers to it.
/// No-op outside a stage.
pub fn save_arg(i: usize, packet: *const u8, packet_len: usize) {
    let Some(s) = STAGE.get() else { return };
    if !s.args.contains(&(i + 1)) {
        return;
    }
    let bytes = unsafe { std::slice::from_raw_parts(packet, packet_len) };
    if let Err(e) = write_packet(&arg_path(&s.dir, i + 1), bytes, std::ptr::null(), std::ptr::null()) {
        eprintln!("Error: saving argument {}: {}", i + 1, e);
        crate::process::clean_exit(1);
    }
}
