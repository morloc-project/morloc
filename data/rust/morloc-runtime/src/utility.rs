//! File I/O and string utility functions.
//! Replaces utility.c.

use std::ffi::{c_char, c_void, CStr};
use std::io::Write;
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// ── Cross-platform helpers ─────────────────────────────────────────────────

/// Return the current errno value (cross-platform).
#[cfg(target_os = "linux")]
#[inline]
pub unsafe fn errno_val() -> i32 {
    *libc::__errno_location()
}

#[cfg(target_os = "macos")]
#[inline]
pub unsafe fn errno_val() -> i32 {
    *libc::__error()
}

/// Make a file's data durable: on stable storage, not only handed to the
/// drive. Returns 0, or -1 with errno set.
///
/// Linux: fdatasync. On macOS fsync stops at the drive's cache; only
/// F_FULLFSYNC reaches the medium. A filesystem that does not support it
/// (some network and FUSE mounts) gets fsync.
pub unsafe fn sync_file_data(fd: i32) -> i32 {
    #[cfg(target_os = "linux")]
    {
        libc::fdatasync(fd)
    }
    #[cfg(target_vendor = "apple")]
    {
        if libc::fcntl(fd, libc::F_FULLFSYNC) == 0 {
            return 0;
        }
        libc::fsync(fd)
    }
    #[cfg(not(any(target_os = "linux", target_vendor = "apple")))]
    {
        libc::fsync(fd)
    }
}

/// Bytes in `sockaddr_un.sun_path`: 108 on Linux, 104 on macOS.
pub const SUN_PATH_LEN: usize =
    std::mem::size_of::<libc::sockaddr_un>() - std::mem::offset_of!(libc::sockaddr_un, sun_path);

/// The address of the unix socket at `path`. A path with no room for its
/// terminating NUL is refused: the kernel would bind or connect to a
/// truncated name instead.
pub fn unix_socket_addr(path: &[u8]) -> Result<libc::sockaddr_un, crate::error::MorlocError> {
    if path.len() >= SUN_PATH_LEN || path.contains(&0) {
        return Err(crate::error::MorlocError::Ipc(format!(
            "socket path '{}' is {} bytes; unix sockets on this platform allow at most {}",
            String::from_utf8_lossy(path),
            path.len(),
            SUN_PATH_LEN - 1
        )));
    }
    // SAFETY: zeroed is a valid sockaddr_un; the copy fits with a NUL to spare.
    let mut addr: libc::sockaddr_un = unsafe { std::mem::zeroed() };
    addr.sun_family = libc::AF_UNIX as libc::sa_family_t;
    unsafe {
        std::ptr::copy_nonoverlapping(path.as_ptr() as *const libc::c_char, addr.sun_path.as_mut_ptr(), path.len());
    }
    Ok(addr)
}

/// Suppress SIGPIPE on send(). Linux: per-call flag. macOS: use set_nosigpipe() on the socket.
#[cfg(target_os = "linux")]
pub const SEND_NOSIGNAL: i32 = libc::MSG_NOSIGNAL;
#[cfg(target_os = "macos")]
pub const SEND_NOSIGNAL: i32 = 0;

/// Set SO_NOSIGPIPE on a socket (macOS). No-op on Linux (uses MSG_NOSIGNAL per-call).
#[allow(unused_variables)]
pub unsafe fn set_nosigpipe(fd: i32) {
    #[cfg(target_os = "macos")]
    {
        let val: libc::c_int = 1;
        libc::setsockopt(
            fd,
            libc::SOL_SOCKET,
            libc::SO_NOSIGPIPE,
            &val as *const _ as *const libc::c_void,
            std::mem::size_of::<libc::c_int>() as libc::socklen_t,
        );
    }
}

// ── File operations ────────────────────────────────────────────────────────

pub(crate) unsafe fn file_exists(filename: *const c_char) -> bool {
    if filename.is_null() {
        return false;
    }
    let path = CStr::from_ptr(filename).to_string_lossy();
    std::path::Path::new(path.as_ref()).exists()
}

pub(crate) unsafe fn mkdir_p(path: *const c_char, errmsg: *mut *mut c_char) -> i32 {
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

pub(crate) unsafe fn delete_directory(path: *const c_char) {
    if path.is_null() {
        return;
    }
    let p = CStr::from_ptr(path).to_string_lossy();
    let _ = std::fs::remove_dir_all(p.as_ref());
}

pub(crate) unsafe fn has_suffix(x: *const c_char, suffix: *const c_char) -> bool {
    if x.is_null() || suffix.is_null() {
        return false;
    }
    let xs = CStr::from_ptr(x).to_string_lossy();
    let ss = CStr::from_ptr(suffix).to_string_lossy();
    xs.ends_with(ss.as_ref())
}

/// Best-effort raise of the soft open-file limit (RLIMIT_NOFILE) toward the
/// hard limit, so a process holding many concurrent connections/fds is bounded
/// by the (large) hard limit rather than the common 1024 (Linux) or 256
/// (macOS) soft cap. poll()-based readiness waits already tolerate fds >= 1024,
/// so this only widens headroom. macOS refuses any soft limit above OPEN_MAX
/// (10240) even when the hard limit is unlimited, so smaller targets are tried
/// in turn. Process-global and idempotent; failures (e.g. sandboxes forbidding
/// setrlimit) are ignored. Call once at each process entry point that accepts
/// connections.
pub unsafe fn raise_nofile_limit() {
    let mut rl: libc::rlimit = std::mem::zeroed();
    if libc::getrlimit(libc::RLIMIT_NOFILE, &mut rl) != 0 {
        return;
    }
    for target in [rl.rlim_max, 1 << 20, 65536, 10240] {
        if target <= rl.rlim_cur || target > rl.rlim_max {
            continue;
        }
        let want = libc::rlimit { rlim_cur: target, rlim_max: rl.rlim_max };
        if libc::setrlimit(libc::RLIMIT_NOFILE, &want) == 0 {
            return;
        }
    }
}

/// A file built beside its destination and moved onto it when finished.
///
/// The content is written to `.<basename>.tmp.<pid>.<seq>` in the
/// destination's own directory, which keeps the rename within one
/// filesystem and therefore atomic. A reader of the destination sees
/// either the previous file or the completed one, never a partial build,
/// and a build that does not finish leaves the destination untouched --
/// the temporary is removed on drop. The PID and a monotonic counter in
/// the temporary's name keep concurrent writers to one directory apart.
///
/// Callers that hold the bytes should use `write_atomic_path`. This type
/// exists for a producer that never brings the content into memory, such
/// as a `sendfile` copy, and so needs the descriptor itself.
pub struct AtomicFile {
    tmp: std::path::PathBuf,
    dest: std::path::PathBuf,
    file: Option<std::fs::File>,
}

impl AtomicFile {
    /// Open a temporary beside `dest`. When `dest` already exists its
    /// permissions are carried over, so replacing a file does not
    /// silently widen or narrow who can read it.
    pub fn create(dest: &std::path::Path) -> std::io::Result<Self> {
        use std::os::unix::io::FromRawFd;
        // A symbolic link stays a link: the file it names is replaced.
        let dest = match std::fs::symlink_metadata(dest) {
            Ok(m) if m.file_type().is_symlink() => {
                std::fs::canonicalize(dest).unwrap_or_else(|_| dest.to_path_buf())
            }
            _ => dest.to_path_buf(),
        };
        let (tmp, fd) = create_beside(&dest, 0o666)?;
        let file = unsafe { std::fs::File::from_raw_fd(fd) };
        if let Ok(meta) = std::fs::metadata(&dest) {
            use std::os::unix::fs::PermissionsExt;
            let mode = meta.permissions().mode() & 0o7777;
            let _ = file.set_permissions(std::fs::Permissions::from_mode(mode));
        }
        Ok(AtomicFile { tmp, dest, file: Some(file) })
    }

    /// The descriptor being built. Valid until `commit`.
    pub fn as_raw_fd(&self) -> std::os::unix::io::RawFd {
        use std::os::unix::io::AsRawFd;
        self.file.as_ref().expect("AtomicFile used after commit").as_raw_fd()
    }

    pub fn as_file_mut(&mut self) -> &mut std::fs::File {
        self.file.as_mut().expect("AtomicFile used after commit")
    }

    /// Flush the content to disk and move it onto the destination.
    pub fn commit(mut self) -> std::io::Result<()> {
        let file = self.file.take().expect("AtomicFile committed twice");
        let replaced = file.sync_all().and_then(|()| {
            drop(file);
            let guard = ReplaceGuard::take(&self.dest)?;
            std::fs::rename(&self.tmp, &self.dest).map(|()| drop(guard))
        });
        if let Err(e) = replaced {
            let _ = std::fs::remove_file(&self.tmp);
            return Err(e);
        }
        let dir = self.dest.parent().unwrap_or(std::path::Path::new("."));
        if let Ok(dir_f) = std::fs::File::open(dir) {
            let _ = dir_f.sync_all();
        }
        Ok(())
    }
}

impl Drop for AtomicFile {
    fn drop(&mut self) {
        // Present only when `commit` did not run, so the build failed or
        // was abandoned and the destination must keep what it had.
        if self.file.take().is_some() {
            let _ = std::fs::remove_file(&self.tmp);
        }
    }
}

/// Create a new file beside `target`, named `.<basename>.tmp.<pid>.<seq>`,
/// to be renamed onto it. Every staging file of the process draws from one
/// counter and is created exclusively, so two never share a file, nor take
/// over one a crashed process left behind under a reused pid.
pub fn create_beside(
    target: &std::path::Path,
    mode: libc::mode_t,
) -> std::io::Result<(std::path::PathBuf, libc::c_int)> {
    use std::os::unix::ffi::OsStrExt;
    use std::sync::atomic::{AtomicU64, Ordering};
    static SEQ: AtomicU64 = AtomicU64::new(0);
    let dir = target.parent().unwrap_or(std::path::Path::new("."));
    let basename = target
        .file_name()
        .map(|s| s.to_string_lossy().into_owned())
        .unwrap_or_else(|| String::from("out"));
    loop {
        let seq = SEQ.fetch_add(1, Ordering::Relaxed);
        let tmp = dir.join(format!(".{}.tmp.{}.{}", basename, std::process::id(), seq));
        let c_tmp = std::ffi::CString::new(tmp.as_os_str().as_bytes())
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidInput, e))?;
        let fd = unsafe {
            libc::open(c_tmp.as_ptr(), libc::O_RDWR | libc::O_CREAT | libc::O_EXCL | libc::O_CLOEXEC, mode as libc::c_uint)
        };
        if fd >= 0 {
            return Ok((tmp, fd));
        }
        let e = std::io::Error::last_os_error();
        if e.raw_os_error() != Some(libc::EEXIST) {
            return Err(e);
        }
    }
}

/// The lock of a file about to be replaced, held exclusively across the
/// rename: a rename cannot check what it displaces, so whoever renames over a
/// file must hold its lock. A stream writing the file holds that lock too,
/// and replacing the file under it would leave the writer writing into a file
/// nobody can reach. Replacements of one path (stores of one cache entry,
/// say) therefore take turns: one finding the lock held waits, within
/// `REPLACE_WAIT`, since a replacement holds it only across a rename. A
/// stream holds it for as long as it writes, so the wait runs out and the
/// replacement is refused.
pub struct ReplaceGuard {
    fd: Option<libc::c_int>,
}

const REPLACE_WAIT: std::time::Duration = std::time::Duration::from_secs(2);

impl ReplaceGuard {
    pub fn take(dest: &std::path::Path) -> std::io::Result<Self> {
        use std::os::unix::ffi::OsStrExt;
        let c_path = std::ffi::CString::new(dest.as_os_str().as_bytes())
            .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidInput, e))?;
        let deadline = std::time::Instant::now() + REPLACE_WAIT;
        // Retry until the locked file is still the one `dest` names, so the
        // lock covers the file the rename replaces.
        loop {
            let fd = unsafe { libc::open(c_path.as_ptr(), libc::O_RDONLY | libc::O_CLOEXEC) };
            if fd < 0 {
                let e = std::io::Error::last_os_error();
                // Nothing there yet: no stream can be writing it.
                if matches!(e.raw_os_error(), Some(libc::ENOENT | libc::ENOTDIR)) {
                    return Ok(ReplaceGuard { fd: None });
                }
                return Err(e);
            }
            let refused = match crate::stream::lock_stream_file(fd) {
                Err(why) => {
                    unsafe { libc::close(fd); }
                    format!("'{}' is open for writing as a stream: {}", dest.display(), why)
                }
                Ok(()) if crate::stream::path_names(&c_path, fd) => {
                    return Ok(ReplaceGuard { fd: Some(fd) });
                }
                Ok(()) => {
                    crate::stream::unlock_and_close(fd);
                    format!("'{}' kept being replaced while it was locked", dest.display())
                }
            };
            if std::time::Instant::now() >= deadline {
                return Err(std::io::Error::new(std::io::ErrorKind::ResourceBusy, refused));
            }
            std::thread::sleep(std::time::Duration::from_millis(1));
        }
    }
}

impl Drop for ReplaceGuard {
    fn drop(&mut self) {
        if let Some(fd) = self.fd.take() {
            crate::stream::unlock_and_close(fd);
        }
    }
}

/// Rust-friendly entry point shared by every in-crate caller.
pub fn write_atomic_path(path: &std::path::Path, bytes: &[u8]) -> std::io::Result<()> {
    let mut staged = AtomicFile::create(path)?;
    if !bytes.is_empty() {
        staged.as_file_mut().write_all(bytes)?;
    }
    staged.commit()
}

pub(crate) unsafe fn write_atomic(
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
    let bytes: &[u8] = if size == 0 {
        &[]
    } else {
        std::slice::from_raw_parts(data, size)
    };
    match write_atomic_path(path, bytes) {
        Ok(()) => 0,
        Err(e) => {
            set_errmsg(errmsg, &MorlocError::Io(e));
            -1
        }
    }
}

// ── Binary I/O ─────────────────────────────────────────────────────────────

pub(crate) unsafe fn read_binary_file(
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

pub(crate) unsafe fn read_binary_fd(
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

/// Write all of `bytes` to `fd`, retrying interrupted and partial writes.
pub fn write_all_to_fd(fd: i32, bytes: &[u8]) -> Result<(), MorlocError> {
    let mut rest = bytes;
    while !rest.is_empty() {
        let n = unsafe { libc::write(fd, rest.as_ptr() as *const c_void, rest.len()) };
        if n > 0 {
            rest = &rest[n as usize..];
            continue;
        }
        let e = std::io::Error::last_os_error();
        match (n, e.kind()) {
            (0, _) => return Err(MorlocError::Other("write failed: wrote nothing".into())),
            (_, std::io::ErrorKind::Interrupted) => continue,
            (_, std::io::ErrorKind::BrokenPipe) => return Err(MorlocError::PipeClosed),
            _ => return Err(MorlocError::Other(format!("write failed: {e}"))),
        }
    }
    Ok(())
}

/// Returns 0, or `MLC_RESULT_PIPE_CLOSED` when the reader closed `fd`, or -1;
/// `errmsg` holds the reason for either failure.
pub(crate) unsafe fn write_binary_fd(
    fd: i32,
    buf: *const c_char,
    count: usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    let bytes = if count == 0 { &[][..] } else { std::slice::from_raw_parts(buf as *const u8, count) };
    match write_all_to_fd(fd, bytes) {
        Ok(()) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            if matches!(e, MorlocError::PipeClosed) { morloc_runtime_types::MLC_RESULT_PIPE_CLOSED } else { -1 }
        }
    }
}

pub(crate) unsafe fn print_binary(
    buf: *const c_char,
    count: usize,
    errmsg: *mut *mut c_char,
) -> i32 {
    write_binary_fd(libc::STDOUT_FILENO, buf, count, errmsg)
}

// ── Display ────────────────────────────────────────────────────────────────

pub(crate) unsafe fn hex(ptr: *const c_void, size: usize) {
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

pub(crate) unsafe fn print_hex_dump(
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
pub(crate) fn mix(a: u64, b: u64) -> u64 {
    const PRIME64_1: u64 = 0x9E3779B185EBCA87;
    const PRIME64_2: u64 = 0xC2B2AE3D27D4EB4F;
    let mut a = a ^ b.wrapping_mul(PRIME64_1);
    a = (a << 31) | (a >> 33);
    a.wrapping_mul(PRIME64_2)
}

pub(crate) unsafe fn morloc_xxh64(
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
pub(crate) unsafe fn dirname(path: *mut c_char) -> *mut c_char {
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

#[cfg(test)]
mod socket_addr_tests {
    use super::*;

    #[test]
    fn a_socket_path_must_fit_with_its_terminator() {
        let fits = vec![b'a'; SUN_PATH_LEN - 1];
        let addr = unix_socket_addr(&fits).unwrap();
        assert_eq!(addr.sun_path[SUN_PATH_LEN - 1], 0);
        assert!(unix_socket_addr(&vec![b'a'; SUN_PATH_LEN]).is_err());
        assert!(unix_socket_addr(b"/tmp/a\0b").is_err());
    }

    #[test]
    fn the_open_file_limit_is_raised() {
        unsafe {
            raise_nofile_limit();
            let mut rl: libc::rlimit = std::mem::zeroed();
            assert_eq!(libc::getrlimit(libc::RLIMIT_NOFILE, &mut rl), 0);
            assert!(rl.rlim_cur >= rl.rlim_max.min(10240), "soft limit stayed at {}", rl.rlim_cur);
        }
    }

    #[test]
    fn sun_path_len_matches_the_platform() {
        let addr: libc::sockaddr_un = unsafe { std::mem::zeroed() };
        assert_eq!(SUN_PATH_LEN, addr.sun_path.len());
    }

    fn pipe() -> (i32, i32) {
        let mut fds = [0; 2];
        assert_eq!(unsafe { libc::pipe(fds.as_mut_ptr()) }, 0);
        (fds[0], fds[1])
    }

    #[test]
    fn writing_to_a_closed_pipe_reports_it_apart_from_other_failures() {
        let (r, w) = pipe();
        unsafe { libc::close(r) };
        let mut err: *mut c_char = ptr::null_mut();
        let rc = unsafe { write_binary_fd(w, b"x".as_ptr() as *const c_char, 1, &mut err) };
        assert_eq!(rc, morloc_runtime_types::MLC_RESULT_PIPE_CLOSED);
        assert!(!err.is_null());
        unsafe { libc::free(err as *mut c_void); libc::close(w) };
    }

    #[test]
    fn a_write_larger_than_the_pipe_arrives_whole() {
        let (r, w) = pipe();
        let reader = std::thread::spawn(move || {
            let mut got = Vec::new();
            let mut buf = [0u8; 4096];
            loop {
                std::thread::sleep(std::time::Duration::from_millis(1));
                let n = unsafe { libc::read(r, buf.as_mut_ptr() as *mut c_void, buf.len()) };
                if n <= 0 { break; }
                got.extend_from_slice(&buf[..n as usize]);
            }
            unsafe { libc::close(r) };
            got
        });
        let data: Vec<u8> = (0..1_000_000u32).map(|i| (i % 251) as u8).collect();
        write_all_to_fd(w, &data).unwrap();
        unsafe { libc::close(w) };
        assert_eq!(reader.join().unwrap(), data);
    }
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub unsafe extern "C" fn file_exists(filename: *const c_char) -> bool {
        super::file_exists(filename)
    }

    #[no_mangle]
    pub unsafe extern "C" fn mkdir_p(path: *const c_char, errmsg: *mut *mut c_char) -> i32 {
        super::mkdir_p(path, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn delete_directory(path: *const c_char) {
        super::delete_directory(path)
    }

    #[no_mangle]
    pub unsafe extern "C" fn has_suffix(x: *const c_char, suffix: *const c_char) -> bool {
        super::has_suffix(x, suffix)
    }

    #[no_mangle]
    pub unsafe extern "C" fn write_atomic(filename: *const c_char, data: *const u8, size: usize, errmsg: *mut *mut c_char) -> i32 {
        super::write_atomic(filename, data, size, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn read_binary_file(filename: *const c_char, file_size: *mut usize, errmsg: *mut *mut c_char) -> *mut u8 {
        super::read_binary_file(filename, file_size, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn read_binary_fd(file: *mut libc::FILE, file_size: *mut usize, errmsg: *mut *mut c_char) -> *mut u8 {
        super::read_binary_fd(file, file_size, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn write_binary_fd(fd: i32, buf: *const c_char, count: usize, errmsg: *mut *mut c_char) -> i32 {
        super::write_binary_fd(fd, buf, count, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn print_binary(buf: *const c_char, count: usize, errmsg: *mut *mut c_char) -> i32 {
        super::print_binary(buf, count, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn hex(ptr: *const c_void, size: usize) {
        super::hex(ptr, size)
    }

    #[no_mangle]
    pub unsafe extern "C" fn print_hex_dump(data: *const u8, size: usize, errmsg: *mut *mut c_char) -> bool {
        super::print_hex_dump(data, size, errmsg)
    }

    #[no_mangle]
    pub extern "C" fn mix(a: u64, b: u64) -> u64 {
        super::mix(a, b)
    }

    #[no_mangle]
    pub unsafe extern "C" fn morloc_xxh64(input: *const c_void, length: usize, seed: u64) -> u64 {
        super::morloc_xxh64(input, length, seed)
    }

    #[no_mangle]
    pub unsafe extern "C" fn dirname(path: *mut c_char) -> *mut c_char {
        super::dirname(path)
    }
}
