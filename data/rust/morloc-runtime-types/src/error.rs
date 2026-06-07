use std::ffi::{CString, c_char};

#[derive(Debug, thiserror::Error)]
pub enum MorlocError {
    #[error("shared memory error: {0}")]
    Shm(String),
    #[error("packet error: {0}")]
    Packet(String),
    #[error("schema error: {0}")]
    Schema(String),
    #[error("serialization error: {0}")]
    Serialization(String),
    #[error("IPC error: {0}")]
    Ipc(String),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("null pointer")]
    NullPointer,
    #[error("{0}")]
    Other(String),
}

/// Write a MorlocError into the C ERRMSG convention.
///
/// # Safety
/// `errmsg` must be a valid, non-dangling pointer to a `*mut c_char` (i.e., `char**`).
/// The caller is responsible for freeing the allocated string via `CString::from_raw`
/// or `libc::free`.
pub unsafe fn set_errmsg(errmsg: *mut *mut c_char, err: &MorlocError) {
    if !errmsg.is_null() {
        if let Ok(cstr) = CString::new(err.to_string()) {
            *errmsg = cstr.into_raw();
        }
    }
}

/// Clear the ERRMSG pointer (must be called at FFI entry).
///
/// # Safety
/// `errmsg` must be a valid pointer to a `*mut c_char`.
pub unsafe fn clear_errmsg(errmsg: *mut *mut c_char) {
    if !errmsg.is_null() {
        *errmsg = std::ptr::null_mut();
    }
}
