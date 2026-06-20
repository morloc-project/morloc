//! Shared "path -> typed morloc packet" loader.
//!
//! This is the single Rust-side entry point in the nexus that turns
//! a file path into a typed morloc data packet. Both [`crate::view::run`]
//! and [`crate::file::run`]'s `--validate` path call it, and it wraps
//! the same `initialize_positional` -> `parse_cli_data_argument` ->
//! `free_argument_t` FFI chain that
//! `crate::dispatch::run_remote_command` uses. The C-side
//! `load_morloc_data_file` is the choke point the FFI dispatches into.
//!
//! Callers are expected to have already run `file::classify_path`
//! (or equivalent) and rejected `Classification::Error` inputs;
//! `load_with_schema` itself does no file-size validation and hands
//! the bytes straight to libmorloc.

use std::ffi::{c_char, c_void, CString};
use std::ptr;

use morloc_runtime_types::cschema::CSchema;
use morloc_runtime_types::schema::{parse_schema, Schema};

use crate::process::take_c_errmsg;

extern "C" {
    fn initialize_positional(value: *mut c_char) -> *mut c_void;
    fn parse_cli_data_argument(
        dest: *mut u8,
        arg: *const c_void,
        schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> *mut u8;
    fn free_argument_t(arg: *mut c_void);
    fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize;
}

/// Owned handle to a loaded morloc packet. Frees the packet (`libc::free`)
/// and the temporary `CSchema` when dropped, so callers don't have to
/// remember the per-allocation cleanup.
pub struct LoadedPacket {
    pub packet: *mut u8,
    pub packet_size: usize,
    pub c_schema: *mut CSchema,
}

impl LoadedPacket {
    /// Borrow the packet bytes as a slice. Valid until the handle is
    /// dropped.
    pub fn as_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.packet, self.packet_size) }
    }
}

impl Drop for LoadedPacket {
    fn drop(&mut self) {
        if !self.packet.is_null() {
            unsafe { libc::free(self.packet as *mut c_void) };
            self.packet = ptr::null_mut();
        }
        if !self.c_schema.is_null() {
            unsafe { CSchema::free(self.c_schema) };
            self.c_schema = ptr::null_mut();
        }
    }
}

/// Resolve a schema string (parse via `parse_schema`) and load the
/// file at `path` into a typed morloc packet using the same FFI chain
/// `run` uses. Returns the owned packet on success, or a stringified
/// error message on failure.
pub fn load_with_schema(path: &str, schema_str: &str) -> Result<LoadedPacket, String> {
    let schema = parse_schema(schema_str)
        .map_err(|e| format!("failed to parse schema '{}': {}", schema_str, e))?;
    load_with_parsed_schema(path, schema)
}

/// Same as [`load_with_schema`], but accepts an already-parsed schema
/// (avoids the round-trip when the caller already has one in hand).
pub fn load_with_parsed_schema(path: &str, schema: Schema) -> Result<LoadedPacket, String> {
    let c_schema = CSchema::from_rust(&schema);
    let path_c = CString::new(path)
        .map_err(|_| format!("path contains an interior NUL byte: '{}'", path))?;

    let mut errmsg: *mut c_char = ptr::null_mut();
    let c_arg = unsafe { initialize_positional(path_c.as_ptr() as *mut c_char) };
    let pkt = unsafe { parse_cli_data_argument(ptr::null_mut(), c_arg, c_schema, &mut errmsg) };
    unsafe { free_argument_t(c_arg) };
    drop(path_c);

    if pkt.is_null() {
        let msg = take_c_errmsg(errmsg).unwrap_or_else(|| "unknown error".to_string());
        unsafe { CSchema::free(c_schema) };
        return Err(msg);
    }

    let mut size_err: *mut c_char = ptr::null_mut();
    let packet_size = unsafe { morloc_packet_size(pkt, &mut size_err) };
    if !size_err.is_null() {
        let msg = take_c_errmsg(size_err).unwrap_or_else(|| "unknown error".to_string());
        unsafe {
            libc::free(pkt as *mut c_void);
            CSchema::free(c_schema);
        }
        return Err(format!("morloc_packet_size failed: {}", msg));
    }

    Ok(LoadedPacket {
        packet: pkt,
        packet_size,
        c_schema,
    })
}
