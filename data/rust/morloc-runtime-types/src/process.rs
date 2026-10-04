//! What one process can learn about another: whether it has exited, which
//! process started it, and a start stamp that tells it apart from a later
//! process given the same pid.
//!
//! Everything here answers from the kernel's process table, which on Linux
//! is `/proc/<pid>/stat` and on macOS `proc_pidinfo`. Only `snapshot` differs
//! by platform.

/// One reading of a process's entry in the process table.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Snapshot {
    /// When the process started, in platform units; equal stamps for one pid
    /// mean one process. Never 0.
    pub start: u64,
    /// The process has exited and awaits its parent's reaping. It runs
    /// nothing and holds no descriptors or locks.
    pub exited: bool,
    pub parent: u32,
}

/// The process table's entry for `pid`, or `None` when there is none or it
/// cannot be read.
#[cfg(target_os = "linux")]
pub fn snapshot(pid: u32) -> Option<Snapshot> {
    let stat = std::fs::read_to_string(format!("/proc/{pid}/stat")).ok()?;
    parse_linux_stat(&stat)
}

/// Fields of a `/proc/<pid>/stat` line. The executable name in field two is
/// unquoted and may contain spaces and parentheses, so the fields are counted
/// from the last `)`.
#[cfg(any(target_os = "linux", test))]
fn parse_linux_stat(stat: &str) -> Option<Snapshot> {
    let fields: Vec<&str> = stat.rsplit_once(')')?.1.split_whitespace().collect();
    let state = *fields.first()?;
    let parent = fields.get(1)?.parse().ok()?;
    let threads: u64 = fields.get(17)?.parse().ok()?;
    let start: u64 = fields.get(19)?.parse().ok()?;
    // A leader that exits while its other threads run also reads Z; the
    // process is gone only once no thread remains.
    let exited = state == "Z" && threads <= 1;
    Some(Snapshot { start: start.max(1), exited, parent })
}

#[cfg(target_vendor = "apple")]
pub fn snapshot(pid: u32) -> Option<Snapshot> {
    let mut info: libc::proc_bsdinfo = unsafe { std::mem::zeroed() };
    let size = std::mem::size_of::<libc::proc_bsdinfo>() as libc::c_int;
    // A nonzero arg makes the lookup also find a process awaiting reaping.
    // SAFETY: `info` is a writable buffer of `size` bytes.
    let n = unsafe {
        libc::proc_pidinfo(
            pid as libc::c_int,
            libc::PROC_PIDTBSDINFO,
            1,
            &mut info as *mut libc::proc_bsdinfo as *mut libc::c_void,
            size,
        )
    };
    if n != size {
        return None;
    }
    let start = info.pbi_start_tvsec.wrapping_mul(1_000_000).wrapping_add(info.pbi_start_tvusec);
    Some(Snapshot { start: start.max(1), exited: info.pbi_status == libc::SZOMB, parent: info.pbi_ppid })
}

#[cfg(not(any(target_os = "linux", target_vendor = "apple")))]
pub fn snapshot(_pid: u32) -> Option<Snapshot> {
    None
}

/// `pid`'s start stamp, or 0 when it cannot be read.
pub fn start_time(pid: u32) -> u64 {
    snapshot(pid).map_or(0, |s| s.start)
}

/// The parent of `pid`, or `None` when it cannot be read.
pub fn parent_of(pid: u32) -> Option<u32> {
    snapshot(pid).map(|s| s.parent)
}

/// Whether the process that had start stamp `start` (0 when unknown) may
/// still be running as `pid`. Anything this cannot establish counts as alive:
/// judging a live process dead would hand its resources to another.
pub fn alive(pid: u32, start: u64) -> bool {
    let Ok(p) = libc::pid_t::try_from(pid) else { return false };
    if p <= 0 {
        return false;
    }
    // SAFETY: signal 0 performs only the existence and permission check.
    let gone = unsafe {
        libc::kill(p, 0) != 0
            && std::io::Error::last_os_error().raw_os_error() == Some(libc::ESRCH)
    };
    if gone {
        return false;
    }
    match snapshot(pid) {
        Some(s) => !s.exited && (start == 0 || s.start == start),
        None => true,
    }
}

/// What distinguishes this boot of the machine from every other, or `None`
/// when it cannot be read. Two processes recorded under different boots
/// cannot both be running.
pub fn boot_id() -> Option<String> {
    #[cfg(target_os = "linux")]
    {
        std::fs::read_to_string("/proc/sys/kernel/random/boot_id").ok().map(|s| s.trim().to_string())
    }
    #[cfg(target_vendor = "apple")]
    {
        let mut tv: libc::timeval = unsafe { std::mem::zeroed() };
        let mut len = std::mem::size_of::<libc::timeval>();
        // SAFETY: sysctlbyname writes at most `len` bytes into `tv`.
        let rc = unsafe {
            libc::sysctlbyname(
                b"kern.boottime\0".as_ptr() as *const libc::c_char,
                &mut tv as *mut libc::timeval as *mut libc::c_void,
                &mut len,
                std::ptr::null_mut(),
                0,
            )
        };
        (rc == 0).then(|| format!("{}.{}", tv.tv_sec, tv.tv_usec))
    }
    #[cfg(not(any(target_os = "linux", target_vendor = "apple")))]
    {
        None
    }
}

/// The PID namespace this process sees pids in, or `None` when it cannot be
/// read. A pid recorded in another namespace means nothing here.
pub fn pid_namespace() -> Option<String> {
    #[cfg(target_os = "linux")]
    {
        std::fs::read_link("/proc/self/ns/pid").ok().map(|p| p.to_string_lossy().into_owned())
    }
    #[cfg(not(target_os = "linux"))]
    {
        // No PID namespaces: every process shares one.
        Some("host".to_string())
    }
}

/// This process as one word: pid in the high half, the low bits of its start
/// stamp in the low half, so a reused pid does not pass for it.
pub fn token() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static CACHED: AtomicU64 = AtomicU64::new(0);
    static CACHED_GENERATION: AtomicU64 = AtomicU64::new(u64::MAX);
    // FORK-14: every writer in one generation stores the same token, before
    // the generation that makes it visible; a token without a start stamp is
    // never stored.
    let generation = crate::fork_generation::generation();
    if CACHED_GENERATION.load(Ordering::Acquire) == generation {
        return CACHED.load(Ordering::Relaxed);
    }
    let pid = std::process::id();
    let start = start_time(pid);
    let token = make_token(pid, start);
    if start != 0 {
        CACHED.store(token, Ordering::Relaxed);
        CACHED_GENERATION.store(generation, Ordering::Release);
    }
    token
}

fn make_token(pid: u32, start: u64) -> u64 {
    ((pid as u64) << 32) | (start as u32) as u64
}

/// Whether the process a `token` names may still be running, erring toward
/// alive as `alive` does.
pub fn token_alive(token: u64) -> bool {
    let pid = (token >> 32) as u32;
    let stamp = token as u32;
    if !alive(pid, 0) {
        return false;
    }
    match snapshot(pid) {
        Some(s) => stamp == 0 || s.start as u32 == stamp,
        None => true,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Fork a child that runs `f` and then exits, and return its pid once it
    /// has exited, unreaped.
    fn exited_child(f: impl FnOnce()) -> libc::pid_t {
        unsafe {
            let pid = libc::fork();
            assert!(pid >= 0);
            if pid == 0 {
                f();
                libc::_exit(0);
            }
            let mut info: libc::siginfo_t = std::mem::zeroed();
            libc::waitid(libc::P_PID, pid as libc::id_t, &mut info, libc::WEXITED | libc::WNOWAIT);
            pid
        }
    }

    fn reap(pid: libc::pid_t) {
        unsafe { libc::waitpid(pid, std::ptr::null_mut(), 0) };
    }

    #[test]
    fn this_process_is_alive_and_known() {
        let me = std::process::id();
        let s = snapshot(me).expect("no process-table entry for this process");
        assert!(!s.exited);
        assert_eq!(s.parent, unsafe { libc::getppid() } as u32);
        assert_ne!(s.start, 0);
        assert_eq!(start_time(me), s.start, "the start stamp changed");
        assert!(alive(me, s.start));
        assert!(token_alive(token()));
    }

    #[test]
    fn a_child_names_its_parent() {
        let me = std::process::id();
        let child = exited_child(|| {});
        // Read while unreaped: the entry is still there.
        let s = snapshot(child as u32).expect("no entry for an unreaped child");
        reap(child);
        assert_eq!(s.parent, me);
        assert_eq!(parent_of(me), Some(unsafe { libc::getppid() } as u32));
    }

    #[test]
    fn an_unreaped_exited_process_is_not_alive() {
        let child = exited_child(|| {}) as u32;
        let s = snapshot(child);
        let start = start_time(child);
        let live = alive(child, 0);
        let live_with_start = alive(child, start);
        reap(child as libc::pid_t);
        assert!(s.is_some_and(|s| s.exited), "an exited child does not read as exited: {s:?}");
        assert!(!live && !live_with_start, "an exited, unreaped process reads as alive");
    }

    #[test]
    fn a_reaped_process_is_not_alive() {
        let child = exited_child(|| {});
        reap(child);
        assert!(!alive(child as u32, 0));
        assert_eq!(snapshot(child as u32), None);
    }

    #[test]
    fn another_start_stamp_is_another_process() {
        let me = std::process::id();
        let start = start_time(me);
        assert!(!alive(me, start + 1));
        assert!(!token_alive(make_token(me, start + 1)));
    }

    #[test]
    fn a_child_inherits_no_cached_token() {
        let parent = token();
        let mut fds = [0 as libc::c_int; 2];
        unsafe { assert_eq!(crate::fd::pipe(fds.as_mut_ptr()), 0) };
        let child = exited_child(|| unsafe {
            let t = token().to_le_bytes();
            libc::write(fds[1], t.as_ptr() as *const libc::c_void, 8);
        });
        let mut t = [0u8; 8];
        unsafe { libc::read(fds[0], t.as_mut_ptr() as *mut libc::c_void, 8) };
        reap(child);
        let child_token = u64::from_le_bytes(t);
        assert_eq!(child_token >> 32, child as u64);
        assert_ne!(child_token, parent);
    }

    #[test]
    fn boot_and_namespace_are_known_and_stable() {
        let boot = boot_id().expect("boot id unreadable");
        assert!(!boot.is_empty());
        assert_eq!(boot_id(), Some(boot));
        let ns = pid_namespace().expect("pid namespace unreadable");
        assert_eq!(pid_namespace(), Some(ns));
    }

    #[test]
    fn stat_lines_are_read_from_the_last_parenthesis() {
        let line = "42 (a) b (c) Z 7 1 1 0 -1 4194560 0 0 0 0 0 0 0 0 20 0 1 0 12345 0 0";
        let s = parse_linux_stat(line).unwrap();
        assert_eq!(s, Snapshot { start: 12345, exited: true, parent: 7 });
        let threaded = "42 (a) Z 7 1 1 0 -1 4194560 0 0 0 0 0 0 0 0 20 0 3 0 12345 0 0";
        assert!(!parse_linux_stat(threaded).unwrap().exited);
    }
}
