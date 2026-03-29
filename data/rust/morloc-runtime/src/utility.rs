//! File I/O and string utility functions.
//! Replaces utility.c.

use std::ffi::{c_char, c_void, CStr};
use std::io::Write;
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// ── File operations ────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn file_exists(filename: *const c_char) -> bool {
    if filename.is_null() {
        return false;
    }
    let path = CStr::from_ptr(filename).to_string_lossy();
    std::path::Path::new(path.as_ref()).exists()
}

#[no_mangle]
pub unsafe extern "C" fn mkdir_p(path: *const c_char, errmsg: *mut *mut c_char) -> i32 {
    clear_errmsg(errmsg);
    if path.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("NULL path".into()));
        return -1;
    }
    let p = CStr::from_ptr(path).to_string_lossy();
    match std::fs::create_dir_all(p.as_ref()) {
        Ok(_) => 0,
        Err(e) => {
            set_errmsg(
                errmsg,
                &MorlocError::Io(e),
            );
            -1
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn delete_directory(path: *const c_char) {
    if path.is_null() {
        return;
    }
    let p = CStr::from_ptr(path).to_string_lossy();
    let _ = std::fs::remove_dir_all(p.as_ref());
}

#[no_mangle]
pub unsafe extern "C" fn has_suffix(x: *const c_char, suffix: *const c_char) -> bool {
    if x.is_null() || suffix.is_null() {
        return false;
    }
    let xs = CStr::from_ptr(x).to_string_lossy();
    let ss = CStr::from_ptr(suffix).to_string_lossy();
    xs.ends_with(ss.as_ref())
}

#[no_mangle]
pub unsafe extern "C" fn write_atomic(
    filename: *const c_char,
    data: *const u8,
    size: usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    if filename.is_null() || (data.is_null() && size != 0) {
        set_errmsg(errmsg, &MorlocError::Other("invalid arguments".into()));
        return -1;
    }
    let path_str = CStr::from_ptr(filename).to_string_lossy();
    let path = std::path::Path::new(path_str.as_ref());

    // Get parent directory
    let dir = path.parent().unwrap_or(std::path::Path::new("."));

    // Create temp file in same directory
    let tmp_path = dir.join(format!("morloc-tmp_{}", std::process::id()));

    let result = (|| -> Result<(), std::io::Error> {
        // Write to temp file
        let mut f = std::fs::File::create(&tmp_path)?;
        if size > 0 {
            let bytes = std::slice::from_raw_parts(data, size);
            f.write_all(bytes)?;
        }
        f.sync_all()?;
        drop(f);

        // Atomic rename
        std::fs::rename(&tmp_path, path)?;

        // Sync parent directory
        if let Ok(dir_f) = std::fs::File::open(dir) {
            let _ = dir_f.sync_all();
        }
        Ok(())
    })();

    match result {
        Ok(_) => 0,
        Err(e) => {
            let _ = std::fs::remove_file(&tmp_path);
            set_errmsg(errmsg, &MorlocError::Io(e));
            -1
        }
    }
}

// ── Binary I/O ─────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn read_binary_file(
    filename: *const c_char,
    file_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    if filename.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("NULL filename".into()));
        return ptr::null_mut();
    }
    let path = CStr::from_ptr(filename).to_string_lossy();
    match std::fs::read(path.as_ref()) {
        Ok(data) => {
            *file_size = data.len();
            let buf = libc::malloc(data.len()) as *mut u8;
            if buf.is_null() {
                set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
                return ptr::null_mut();
            }
            std::ptr::copy_nonoverlapping(data.as_ptr(), buf, data.len());
            buf
        }
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Io(e));
            ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn read_binary_fd(
    file: *mut libc::FILE,
    file_size: *mut usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    if file.is_null() {
        set_errmsg(errmsg, &MorlocError::Other("NULL file".into()));
        return ptr::null_mut();
    }

    // Try seek-based size detection
    if libc::fseek(file, 0, libc::SEEK_END) == 0 {
        let size = libc::ftell(file) as usize;
        if size > 0 {
            libc::rewind(file);
            let buf = libc::malloc(size) as *mut u8;
            if buf.is_null() {
                set_errmsg(errmsg, &MorlocError::Other("malloc failed".into()));
                return ptr::null_mut();
            }
            let read = libc::fread(buf as *mut c_void, 1, size, file);
            if read == size {
                *file_size = size;
                return buf;
            }
            libc::free(buf as *mut c_void);
        }
    }

    // Streaming read for non-seekable files
    let chunk_size: usize = 0xffff;
    let mut buf: *mut u8 = ptr::null_mut();
    let mut allocated: usize = 0;

    loop {
        let new_buf = libc::realloc(buf as *mut c_void, allocated + chunk_size) as *mut u8;
        if new_buf.is_null() {
            libc::free(buf as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Other("realloc failed".into()));
            return ptr::null_mut();
        }
        buf = new_buf;
        let read = libc::fread(buf.add(allocated) as *mut c_void, 1, chunk_size, file);
        allocated += read;

        if read < chunk_size {
            if libc::feof(file) != 0 {
                *file_size = allocated;
                return buf;
            }
            if libc::ferror(file) != 0 {
                libc::free(buf as *mut c_void);
                set_errmsg(errmsg, &MorlocError::Other("read error".into()));
                return ptr::null_mut();
            }
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn write_binary_fd(
    fd: i32,
    buf: *const c_char,
    count: usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let mut total: usize = 0;
    while total < count {
        let written = libc::write(fd, buf.add(total) as *const c_void, count - total);
        if written < 0 {
            set_errmsg(
                errmsg,
                &MorlocError::Other(format!("write failed: {}", std::io::Error::last_os_error())),
            );
            return -1;
        }
        total += written as usize;
    }
    0
}

#[no_mangle]
pub unsafe extern "C" fn print_binary(
    buf: *const c_char,
    count: usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    write_binary_fd(libc::STDOUT_FILENO, buf, count, errmsg)
}

// ── Display ────────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn hex(ptr: *const c_void, size: usize) {
    if ptr.is_null() || size == 0 {
        return;
    }
    let bytes = std::slice::from_raw_parts(ptr as *const u8, size);
    for (i, b) in bytes.iter().enumerate() {
        if i > 0 && i % 8 == 0 {
            eprint!(" ");
        }
        eprint!("{:02X}", b);
        if i < size - 1 {
            eprint!(" ");
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn print_hex_dump(
    data: *const u8,
    size: usize,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    if data.is_null() && size > 0 {
        set_errmsg(errmsg, &MorlocError::Other("NULL data".into()));
        return false;
    }
    let bytes = if size > 0 {
        std::slice::from_raw_parts(data, size)
    } else {
        &[]
    };
    for (i, b) in bytes.iter().enumerate() {
        if i > 0 && i % 4 == 0 {
            if i % 24 == 0 {
                println!();
            } else {
                print!(" ");
            }
        }
        print!("{:02X}", b);
    }
    if !bytes.is_empty() {
        println!();
    }
    true
}

// ── xxHash wrapper and mix ─────────────────────────────────────────────────

/// Mix two 64-bit hash values. Matches the C implementation in cache.c.
#[no_mangle]
pub extern "C" fn mix(a: u64, b: u64) -> u64 {
    const PRIME64_1: u64 = 0x9E3779B185EBCA87;
    const PRIME64_2: u64 = 0xC2B2AE3D27D4EB4F;
    let mut a = a ^ b.wrapping_mul(PRIME64_1);
    a = (a << 31) | (a >> 33);
    a.wrapping_mul(PRIME64_2)
}

#[no_mangle]
pub unsafe extern "C" fn morloc_xxh64(
    input: *const c_void,
    length: usize,
    seed: u64,
) -> u64 {
    if input.is_null() || length == 0 {
        return crate::hash::xxh64_with_seed(&[], seed);
    }
    let data = std::slice::from_raw_parts(input as *const u8, length);
    crate::hash::xxh64_with_seed(data, seed)
}

// ── String utilities ───────────────────────────────────────────────────────

/// dirname - returns pointer into the input string (modifies it in-place)
/// Matches the C behavior: returns "." for empty/NULL, strips trailing slashes
#[no_mangle]
pub unsafe extern "C" fn dirname(path: *mut c_char) -> *mut c_char {
    // Return a pointer to the static string "." for empty/null paths and paths with no slash.
    static DOT: [u8; 2] = [b'.', 0];
    let dot_ptr = DOT.as_ptr() as *mut c_char;

    if path.is_null() || *path == 0 {
        return dot_ptr;
    }

    let len = libc::strlen(path);
    let mut end = path.add(len - 1);

    // Remove trailing slashes
    while end > path && *end == b'/' as c_char {
        *end = 0;
        end = end.sub(1);
    }

    // Find last slash
    let last_slash = libc::strrchr(path, b'/' as i32);
    if last_slash.is_null() {
        return dot_ptr;
    }
    if last_slash == path {
        *path.add(1) = 0; // root case "/"
    } else {
        *last_slash = 0;
    }
    path
}
