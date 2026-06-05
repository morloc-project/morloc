//! `MorlocSocket` C-ABI struct shared between libmorloc.so and the nexus.
//!
//! Lives in the types crate (not `morloc-runtime::daemon_ffi`) so the
//! nexus can declare callbacks taking `*mut MorlocSocket` without
//! pulling in `daemon_ffi`'s process-global recovery/shutdown atomics.

use std::ffi::c_char;

/// Matches `morloc_socket_t` from call.h.
#[repr(C)]
pub struct MorlocSocket {
    pub lang: *mut c_char,
    pub syscmd: *mut *mut c_char,
    pub socket_filename: *mut c_char,
    pub pid: i32,
}
