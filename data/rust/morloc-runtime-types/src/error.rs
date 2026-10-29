use std::ffi::{CStr, CString, c_char};

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
    /// The downstream reader closed the pipe (PANIC-13).
    #[error("broken pipe")]
    PipeClosed,
    /// A failure `@try` catches (FAIL-4): a throw, or a failed input
    /// check other than of packet bytes, which are `Packet` (PANIC-13).
    #[error("{0}")]
    UserThrow(String),
}

/// A `malloc`ed copy of `s`, which the receiver releases with `free()`.
pub fn malloc_cstr(s: &CStr) -> *mut c_char {
    let bytes = s.to_bytes_with_nul();
    unsafe {
        let p = libc::malloc(bytes.len()) as *mut u8;
        if p.is_null() {
            crate::panic::fatal("out of memory copying a string for the caller")
        }
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), p, bytes.len());
        p as *mut c_char
    }
}

/// Write a MorlocError into the C ERRMSG convention.
///
/// # Safety
/// `errmsg` must be a valid, non-dangling pointer to a `*mut c_char` (i.e., `char**`).
/// The caller frees the string with `libc::free`.
pub unsafe fn set_errmsg(errmsg: *mut *mut c_char, err: &MorlocError) {
    if !errmsg.is_null() {
        let cleaned: String = err.to_string().chars().map(|c| if c == '\0' { '?' } else { c }).collect();
        let cstr = CString::new(cleaned).expect("NUL bytes were substituted");
        *errmsg = malloc_cstr(&cstr);
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

/// The reason a callee wrote to `err` for its failure, taken over from the
/// C allocation.
///
/// # Safety
/// `err` must be null or a `malloc`ed NUL-terminated string.
pub unsafe fn take_reason(err: *mut c_char) -> MorlocError {
    // PANIC-13
    if err.is_null() {
        crate::panic::fatal("a runtime call failed without giving a reason")
    }
    let text = CStr::from_ptr(err).to_string_lossy().into_owned();
    libc::free(err as *mut libc::c_void);
    MorlocError::Other(text)
}
