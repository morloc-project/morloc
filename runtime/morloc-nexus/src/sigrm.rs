//! Directories the run removes when it ends, and a removal that is safe to
//! run from a signal handler.
//!
//! `remove_dir_all` allocates and may take locks, so the SIGINT/SIGTERM
//! handler cannot use it. Directories registered here are removed by
//! [`remove_registered`] using only async-signal-safe calls; `clean_exit`
//! uses the same removal. Pools write files of unknown names
//! into the run tmpdir (their sockets, SHM fallback files), so the removal
//! walks each tree rather than unlinking a known list.

use std::ffi::CString;
use std::os::raw::c_char;
use std::sync::atomic::{AtomicPtr, Ordering};

/// Paths a run removes: its tmpdir, at most one stage directory, and in a
/// multi-output run one temporary file per action.
const MAX_PATHS: usize = 256;

/// A fixed table of paths, readable from a signal handler. Registration
/// fails, rather than dropping the path, when the table is full.
struct Registry<const N: usize> {
    slots: [AtomicPtr<c_char>; N],
}

impl<const N: usize> Registry<N> {
    const fn new() -> Self {
        Registry { slots: [const { AtomicPtr::new(std::ptr::null_mut()) }; N] }
    }

    fn register(&self, path: &str) -> Result<(), String> {
        let raw = CString::new(path)
            .map_err(|_| format!("path contains a NUL byte: {:?}", path))?
            .into_raw();
        let taken = self.slots.iter().any(|slot| {
            slot.compare_exchange(std::ptr::null_mut(), raw, Ordering::AcqRel, Ordering::Acquire)
                .is_ok()
        });
        if taken {
            Ok(())
        } else {
            drop(unsafe { CString::from_raw(raw) });
            Err(format!("more than {} paths to remove at the end of the run", N))
        }
    }

    /// # Safety
    /// Async-signal-safe: reads the fixed slot array and makes only
    /// async-signal-safe system calls.
    unsafe fn remove_all(&self) {
        for slot in self.slots.iter().rev() {
            let p = slot.load(Ordering::Acquire);
            if !p.is_null() {
                remove_tree(p);
            }
        }
    }
}

static PATHS: Registry<MAX_PATHS> = Registry::new();

/// Register a file or directory for removal when the run ends: by
/// `clean_exit`, or by the signal handler. The path is leaked so the handler
/// can read it without allocating. Fails when the table is full, so a caller
/// never goes on believing a path will be cleaned up when it will not.
pub fn register(path: &str) -> Result<(), String> {
    PATHS.register(path)
}

/// Remove every registered path, most recently registered first.
///
/// # Safety
/// Async-signal-safe: reads the fixed slot array and makes only
/// async-signal-safe system calls.
pub unsafe fn remove_registered() {
    PATHS.remove_all();
}

/// Nesting limit for the walk. The run's directories are flat or nearly so;
/// the limit bounds stack use (one 4 KiB buffer per level).
#[cfg(target_os = "linux")]
const MAX_DEPTH: u32 = 8;

#[cfg(target_os = "linux")]
unsafe fn remove_tree(path: *const c_char) {
    remove_at(libc::AT_FDCWD, path, MAX_DEPTH);
}

#[cfg(target_os = "linux")]
unsafe fn remove_at(dirfd: libc::c_int, name: *const c_char, depth: u32) {
    if libc::unlinkat(dirfd, name, 0) == 0 {
        return;
    }
    if depth == 0 {
        return;
    }
    let fd = libc::openat(
        dirfd,
        name,
        libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_CLOEXEC,
    );
    if fd < 0 {
        return;
    }
    // Removing entries while reading a directory may make getdents skip
    // some, so rescan from the start until a pass finds nothing to remove.
    let mut buf = [0u64; 512];
    for _ in 0..16 {
        if libc::lseek(fd, 0, libc::SEEK_SET) < 0 {
            break;
        }
        let mut found = false;
        loop {
            let n = libc::syscall(
                libc::SYS_getdents64,
                fd,
                buf.as_mut_ptr() as *mut libc::c_void,
                std::mem::size_of_val(&buf),
            );
            if n <= 0 {
                break;
            }
            let base = buf.as_ptr() as *const u8;
            let mut off: isize = 0;
            while off < n as isize {
                let d = base.offset(off) as *const libc::dirent64;
                let reclen = (*d).d_reclen as isize;
                let nm = (*d).d_name.as_ptr();
                if !is_dot_entry(nm) {
                    found = true;
                    remove_at(fd, nm, depth - 1);
                }
                off += reclen;
            }
        }
        if !found {
            break;
        }
    }
    libc::close(fd);
    libc::unlinkat(dirfd, name, libc::AT_REMOVEDIR);
}

#[cfg(target_os = "linux")]
unsafe fn is_dot_entry(nm: *const c_char) -> bool {
    let a = *nm;
    a == b'.' as c_char && (*nm.add(1) == 0 || (*nm.add(1) == b'.' as c_char && *nm.add(2) == 0))
}

/// macOS has no async-signal-safe directory reader, so run `rm -rf`. It is
/// started with posix_spawn, not fork: libSystem's fork first takes the
/// malloc and other library locks for its fork handlers, and a handler that
/// interrupted a thread holding one of them would deadlock or trap there.
/// On Darwin posix_spawn with no attributes or file actions is a system call
/// that runs no fork handlers and allocates nothing.
#[cfg(target_os = "macos")]
unsafe fn remove_tree(path: *const c_char) {
    const RM: &[u8] = b"/bin/rm\0";
    const FLAGS: &[u8] = b"-rf\0";
    const DASHES: &[u8] = b"--\0";
    let argv: [*mut c_char; 5] = [
        RM.as_ptr() as *mut c_char,
        FLAGS.as_ptr() as *mut c_char,
        DASHES.as_ptr() as *mut c_char,
        path as *mut c_char,
        std::ptr::null_mut(),
    ];
    let envp: [*mut c_char; 1] = [std::ptr::null_mut()];
    let mut pid: libc::pid_t = 0;
    let rc = libc::posix_spawn(
        &mut pid,
        argv[0],
        std::ptr::null(),
        std::ptr::null(),
        argv.as_ptr(),
        envp.as_ptr(),
    );
    if rc == 0 {
        let mut status: libc::c_int = 0;
        while libc::waitpid(pid, &mut status, 0) < 0
            && *libc::__error() == libc::EINTR
        {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_full_table_refuses_a_path() {
        let table: Registry<2> = Registry::new();
        assert!(table.register("/nonexistent/a").is_ok());
        assert!(table.register("/nonexistent/b").is_ok());
        assert!(table.register("/nonexistent/c").is_err());
    }

    #[test]
    fn removes_nested_tree() {
        let root = std::env::temp_dir().join(format!("sigrm-test-{}", std::process::id()));
        std::fs::create_dir_all(root.join("a/b")).unwrap();
        for i in 0..300 {
            std::fs::write(root.join(format!("f{}", i)), b"x").unwrap();
        }
        std::fs::write(root.join("a/b/deep"), b"x").unwrap();
        let c = CString::new(root.to_str().unwrap()).unwrap();
        unsafe { remove_tree(c.as_ptr()) };
        assert!(!root.exists());
    }
}
