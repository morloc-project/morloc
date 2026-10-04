//! Ending a nexus's children when the nexus ends, however it ends.
//!
//! The nexus holds the only write end of a pipe; each process it starts
//! (pools, child nexuses) gets the read end. The kernel closes the write end
//! when the nexus exits for any reason, SIGKILL included, so a read in the
//! child then returns end of file. The child answers by ending the process
//! group the nexus gave it, the way the nexus itself would have: SIGTERM,
//! a grace period, then SIGKILL.
//!
//! The nexus passes the read end to a child as `MORLOC_LIFELINE`, naming
//! the descriptor and enough about the pipe and the nexus that an inherited
//! variable whose descriptor is gone or reused, or that belongs to another
//! process tree, is ignored.

use std::ffi::CString;
use std::sync::OnceLock;
use std::time::Duration;

use crate::error::MorlocError;
use morloc_runtime_types::process;

pub const ENV: &str = "MORLOC_LIFELINE";

/// Between SIGTERM and SIGKILL, as the nexus allows its pools.
pub const GRACE: Duration = Duration::from_millis(200);

/// The nexus side: one pipe for every child this process starts.
pub struct Lifeline {
    read_fd: i32,
    /// Never closed: the pipe reaches end of file when this process exits.
    #[cfg_attr(not(test), allow(dead_code))]
    write_fd: i32,
    token: String,
}

static OWN: OnceLock<Result<Lifeline, String>> = OnceLock::new();

impl Lifeline {
    /// This process's lifeline, created on first use.
    pub fn get() -> Result<&'static Lifeline, MorlocError> {
        OWN.get_or_init(|| Self::create().map_err(|e| e.to_string()))
            .as_ref()
            .map_err(|e| MorlocError::Other(format!("cannot create the lifeline pipe: {e}")))
    }

    fn create() -> std::io::Result<Lifeline> {
        let mut fds = [0 as libc::c_int; 2];
        // SAFETY: pipe into a local array.
        unsafe {
            if morloc_runtime_types::fd::pipe(fds.as_mut_ptr()) != 0 {
                return Err(std::io::Error::last_os_error());
            }
        }
        let (dev, ino) = fifo_identity(fds[0])
            .ok_or_else(|| std::io::Error::other("the new pipe is not a FIFO"))?;
        let pid = std::process::id();
        let token = format!(
            "{}:{}:{}:{}:{}:{}",
            fds[0],
            dev,
            ino,
            pid,
            process::start_time(pid),
            // SAFETY: getpgrp cannot fail.
            unsafe { libc::getpgrp() },
        );
        // The write end stays open for the life of the process.
        Ok(Lifeline { read_fd: fds[0], write_fd: fds[1], token })
    }

    pub fn read_fd(&self) -> i32 {
        self.read_fd
    }

    /// `MORLOC_LIFELINE=<token>`, for a child's environment.
    pub fn env_entry(&self) -> CString {
        CString::new(format!("{ENV}={}", self.token)).expect("token holds no NUL")
    }

    pub fn token(&self) -> &str {
        &self.token
    }
}

fn fifo_identity(fd: i32) -> Option<(u64, u64)> {
    // SAFETY: fstat writes only `st`.
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd, &mut st) } != 0 || (st.st_mode & libc::S_IFMT) != libc::S_IFIFO {
        return None;
    }
    Some((st.st_dev as u64, st.st_ino as u64))
}

/// Whether a pipe nobody writes to has lost its last writer.
fn at_end(fd: i32) -> bool {
    let mut p = libc::pollfd { fd, events: libc::POLLIN, revents: 0 };
    // SAFETY: poll on one pollfd owned here.
    unsafe { libc::poll(&mut p, 1, 0) > 0 }
}

/// A lifeline this process received, checked.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Adopted {
    pub fd: i32,
    /// The process group of the nexus that holds the write end.
    pub nexus_pgid: i32,
}

/// Check `token` against this process. The descriptor must be the pipe the
/// token describes, and the nexus it names must be positively identified as
/// an ancestor of this process -- the same pid with the same start stamp --
/// or be gone, with the pipe at end of file, as when a pool starts after its
/// nexus died. A process that cannot be inspected identifies nothing: on
/// macOS that is every process of another user, launchd included, and every
/// process descends from launchd.
fn validate(token: &str) -> Option<Adopted> {
    validate_with(token, process::snapshot)
}

fn validate_with(token: &str, snapshot: impl Fn(u32) -> Option<process::Snapshot>) -> Option<Adopted> {
    let f: Vec<&str> = token.split(':').collect();
    let [fd, dev, ino, pid, start, pgid] = f.as_slice() else { return None };
    let fd: i32 = fd.parse().ok()?;
    let identity = (dev.parse().ok()?, ino.parse().ok()?);
    let pid: i32 = pid.parse().ok()?;
    let start: u64 = start.parse().ok()?;
    let nexus_pgid: i32 = pgid.parse().ok()?;
    if fd < 0 || pid <= 0 || fifo_identity(fd)? != identity {
        return None;
    }
    let ours = match snapshot(pid as u32) {
        Some(s) if s.start == start && !s.exited => crate::run::descends_from(pid),
        // The pid has exited or now names another process.
        Some(_) => at_end(fd),
        None => !process::alive(pid as u32, 0) && at_end(fd),
    };
    ours.then_some(Adopted { fd, nexus_pgid })
}

static ADOPTED: OnceLock<Option<Adopted>> = OnceLock::new();

/// Take up the lifeline this process was started with, if any: validate it
/// and keep it from leaking into programs this process later runs. Returns
/// the read end to watch, or -1. Idempotent.
pub fn adopt() -> i32 {
    let adopted = ADOPTED.get_or_init(|| {
        let a = validate(&std::env::var(ENV).ok()?)?;
        // SAFETY: fcntl on a descriptor validated above.
        unsafe { libc::fcntl(a.fd, libc::F_SETFD, libc::FD_CLOEXEC) };
        Some(a)
    });
    adopted.map_or(-1, |a| a.fd)
}

/// Adopt, and watch the lifeline from a thread that ends this process group
/// when the nexus is gone. For processes with no loop of their own to watch
/// it in. Idempotent.
pub fn guard() {
    static WATCHING: OnceLock<()> = OnceLock::new();
    let Some(a) = ({ adopt(); ADOPTED.get().copied().flatten() }) else { return };
    WATCHING.get_or_init(|| {
        spawn_masked(move || {
            if wait_for_end(a.fd) {
                teardown(a.nexus_pgid, GRACE);
            }
        });
    });
}

/// Block until the lifeline reaches end of file (true), or until it can no
/// longer be read, as when other code closed the descriptor (false).
fn wait_for_end(fd: i32) -> bool {
    let mut b = 0u8;
    loop {
        // SAFETY: reads at most one byte into `b`.
        let n = unsafe { libc::read(fd, &mut b as *mut u8 as *mut libc::c_void, 1) };
        match n {
            0 => return true,
            n if n > 0 => continue,
            _ if std::io::Error::last_os_error().kind() == std::io::ErrorKind::Interrupted => continue,
            _ => return false,
        }
    }
}

/// Run `f` on a thread that receives no asynchronous signals, so signals
/// meant for this process reach the threads that expect them.
fn spawn_masked(f: impl FnOnce() + Send + 'static) {
    // SAFETY: signal-mask calls on sets owned here.
    unsafe {
        let mut all: libc::sigset_t = std::mem::zeroed();
        let mut old: libc::sigset_t = std::mem::zeroed();
        libc::sigfillset(&mut all);
        // Blocking a synchronous fault is undefined.
        for s in [libc::SIGSEGV, libc::SIGBUS, libc::SIGFPE, libc::SIGILL] {
            libc::sigdelset(&mut all, s);
        }
        libc::pthread_sigmask(libc::SIG_BLOCK, &all, &mut old);
        let _ = std::thread::Builder::new()
            .name("morloc-lifeline".into())
            .stack_size(64 * 1024)
            .spawn(f);
        libc::pthread_sigmask(libc::SIG_SETMASK, &old, std::ptr::null_mut());
    }
}

/// End what the nexus would have ended had it exited cleanly: this process
/// group when this process has one apart from the nexus's (as the nexus
/// arranges for pools), otherwise this process alone. A spawned reaper sends
/// SIGTERM, waits `grace`, then sends SIGKILL; it ignores the SIGTERM it
/// sends, so the escalation happens even after this process has exited.
pub fn teardown(nexus_pgid: i32, grace: Duration) {
    // SAFETY: getpgrp and getpid cannot fail.
    let group = unsafe { libc::getpgrp() };
    let target = if group != nexus_pgid { -group } else { unsafe { libc::getpid() } };
    let script = "trap '' TERM; kill -s TERM -- \"$1\"; sleep \"$2\"; kill -s KILL -- \"$1\"";
    let args: Vec<CString> = [
        "sh".to_string(),
        "-c".to_string(),
        script.to_string(),
        "sh".to_string(),
        target.to_string(),
        format!("{}.{:03}", grace.as_secs(), grace.subsec_millis()),
    ]
    .into_iter()
    .map(|a| CString::new(a).unwrap())
    .collect();
    let argv: Vec<*const libc::c_char> =
        args.iter().map(|a| a.as_ptr()).chain(std::iter::once(std::ptr::null())).collect();
    let (_env, envp) = morloc_runtime_types::spawn::current_environment();
    let sh = CString::new("/bin/sh").unwrap();
    let spawned = morloc_runtime_types::spawn::Spawn::new().and_then(|s| s.run(&sh, &argv, &envp, false));
    if spawned.is_err() {
        // SAFETY: kill takes plain integers.
        unsafe { libc::kill(target, libc::SIGTERM) };
    }
}

/// The nexus side, for a child it starts: `MORLOC_LIFELINE=<token>` for the
/// child's environment, with the read end to keep across its exec stored in
/// `read_fd`. Null, and -1, if this process
/// could not make a lifeline.
///
/// # Safety
/// `read_fd` must be writable.
#[no_mangle]
pub unsafe extern "C" fn morloc_lifeline_child_env(read_fd: *mut i32) -> *const std::ffi::c_char {
    static ENTRY: OnceLock<CString> = OnceLock::new();
    match Lifeline::get() {
        Ok(l) => {
            *read_fd = l.read_fd();
            ENTRY.get_or_init(|| l.env_entry()).as_ptr()
        }
        Err(_) => {
            *read_fd = -1;
            std::ptr::null()
        }
    }
}

/// C entry points for the pool scaffolds.
#[no_mangle]
pub extern "C" fn morloc_lifeline_adopt() -> i32 {
    adopt()
}

#[no_mangle]
pub extern "C" fn morloc_lifeline_guard() {
    guard()
}

/// End this process group (see `teardown`) once a lifeline the caller
/// watched itself has reached end of file. Does nothing if none was adopted.
#[no_mangle]
pub extern "C" fn morloc_lifeline_teardown() {
    if let Some(a) = ADOPTED.get().copied().flatten() {
        teardown(a.nexus_pgid, GRACE);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn exit_code(pid: libc::pid_t) -> i32 {
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        if libc::WIFEXITED(status) { libc::WEXITSTATUS(status) } else { -1 }
    }

    /// Fork, run `f` in the child, and return whether it returned true.
    fn in_child(f: impl FnOnce() -> bool) -> bool {
        unsafe {
            let pid = libc::fork();
            assert!(pid >= 0);
            if pid == 0 {
                libc::_exit(if f() { 0 } else { 1 });
            }
            exit_code(pid) == 0
        }
    }

    fn pipe() -> [libc::c_int; 2] {
        let mut p = [0 as libc::c_int; 2];
        assert_eq!(unsafe { morloc_runtime_types::fd::pipe(p.as_mut_ptr()) }, 0);
        p
    }

    fn send(fd: i32, b: &[u8]) {
        unsafe { libc::write(fd, b.as_ptr() as *const libc::c_void, b.len()) };
    }

    /// Read exactly `n` bytes within `secs`, or fewer if the time runs out.
    fn recv(fd: i32, n: usize, secs: u64) -> Vec<u8> {
        let end = std::time::Instant::now() + Duration::from_secs(secs);
        let mut out = Vec::new();
        while out.len() < n && std::time::Instant::now() < end {
            let mut p = libc::pollfd { fd, events: libc::POLLIN, revents: 0 };
            if unsafe { libc::poll(&mut p, 1, 50) } <= 0 {
                continue;
            }
            let mut buf = [0u8; 64];
            let k = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, (n - out.len()).min(64)) };
            if k <= 0 {
                break;
            }
            out.extend_from_slice(&buf[..k as usize]);
        }
        out
    }

    #[test]
    fn a_child_accepts_its_parents_lifeline() {
        // Each lifeline is per process, so the parent here is a fresh child.
        assert!(in_child(|| {
            let token = Lifeline::get().unwrap().token().to_string();
            in_child(move || validate(&token).is_some())
        }));
    }

    #[test]
    fn tokens_that_do_not_describe_this_process_tree_are_refused() {
        assert!(in_child(|| {
            let token = Lifeline::get().unwrap().token().to_string();
            let f: Vec<String> = token.split(':').map(String::from).collect();
            let with = |i: usize, v: String| {
                let mut g = f.clone();
                g[i] = v;
                g.join(":")
            };
            let not_a_fifo = unsafe {
                libc::open(b"/dev/null\0".as_ptr() as *const libc::c_char, libc::O_RDONLY | libc::O_CLOEXEC)
            };
            let refused = [
                String::new(),
                "garbage".to_string(),
                with(0, "1000".into()),               // a closed descriptor
                with(0, not_a_fifo.to_string()),      // not a pipe
                with(0, pipe()[0].to_string()),       // some other pipe
                with(3, "1".into()),                  // a live process that is no ancestor
                with(4, "1".into()),                  // the nexus pid, reused by another process
            ];
            in_child(move || refused.iter().all(|t| validate(t).is_none()) && validate(&token).is_some())
        }));
    }

    /// A nexus that cannot be inspected is not taken on trust: on macOS a
    /// user cannot inspect launchd, the ancestor of every process.
    #[test]
    fn a_nexus_that_cannot_be_inspected_is_refused() {
        assert!(in_child(|| {
            let token = Lifeline::get().unwrap().token().to_string();
            let mut f: Vec<String> = token.split(':').map(String::from).collect();
            f[3] = "1".into();
            let launchd = f.join(":");
            in_child(move || {
                let hidden = |p: u32| if p == 1 { None } else { process::snapshot(p) };
                let blind = |_: u32| None;
                validate_with(&launchd, hidden).is_none()
                    && validate_with(&token, blind).is_none()
                    && validate_with(&token, process::snapshot).is_some()
            })
        }));
    }

    /// A pool that starts only after its nexus died adopts the lifeline and
    /// finds it already at end of file.
    #[test]
    fn a_lifeline_whose_nexus_died_first_is_adopted_at_its_end() {
        let report = pipe();
        assert!(in_child(move || unsafe {
            let l = Lifeline::get().unwrap();
            let (token, read_fd, write_fd) = (l.token().to_string(), l.read_fd(), l.write_fd);
            let nexus = libc::getpid();
            let pool = libc::fork();
            if pool == 0 {
                // exec would have closed this copy of the write end.
                libc::close(write_fd);
                while libc::getppid() == nexus {
                    libc::usleep(1000);
                }
                let ok = validate(&token).is_some_and(|a| a.fd == read_fd) && wait_for_end(read_fd);
                send(report[1], if ok { b"y" } else { b"n" });
                libc::_exit(0);
            }
            true
        }));
        unsafe { libc::close(report[1]) };
        assert_eq!(recv(report[0], 1, 10), b"y");
    }

    /// When the nexus dies, a pool's whole process group ends: the pool by
    /// SIGTERM, and a worker that ignores SIGTERM by the SIGKILL after it.
    #[test]
    fn a_dead_nexus_ends_its_pools_group() {
        let report = pipe();
        assert!(in_child(move || unsafe {
            let l = Lifeline::get().unwrap();
            let token = l.token().to_string();
            let write_fd = l.write_fd;
            let ready = pipe();
            let pool = libc::fork();
            if pool == 0 {
                libc::close(write_fd);
                libc::close(ready[0]);
                libc::setpgid(0, 0);
                let worker = libc::fork();
                if worker == 0 {
                    libc::signal(libc::SIGTERM, libc::SIG_IGN);
                    loop {
                        libc::pause();
                    }
                }
                send(report[1], &worker.to_le_bytes());
                static REPORT: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(-1);
                REPORT.store(report[1], std::sync::atomic::Ordering::SeqCst);
                extern "C" fn on_term_report(_: libc::c_int) {
                    let fd = REPORT.load(std::sync::atomic::Ordering::SeqCst);
                    unsafe {
                        libc::write(fd, b"T".as_ptr() as *const libc::c_void, 1);
                        libc::_exit(0);
                    }
                }
                libc::signal(libc::SIGTERM, on_term_report as *const () as libc::sighandler_t);
                // Not std::env::set_var: its lock may have been held by
                // another test thread when this process was forked.
                let kv = std::ffi::CString::new(token.as_str()).unwrap();
                libc::setenv(b"MORLOC_LIFELINE\0".as_ptr() as *const libc::c_char, kv.as_ptr(), 1);
                guard();
                send(ready[1], b"r");
                loop {
                    libc::pause();
                }
            }
            libc::close(ready[1]);
            recv(ready[0], 1, 10) == b"r"
            // Returning exits this process, the nexus, closing the write end.
        }));
        unsafe { libc::close(report[1]) };
        let got = recv(report[0], 5, 10);
        assert_eq!(got.len(), 5, "the pool never reported: {got:?}");
        let worker = i32::from_le_bytes(got[..4].try_into().unwrap());
        assert_eq!(got[4], b'T', "the pool was not sent SIGTERM");
        let end = std::time::Instant::now() + Duration::from_secs(5);
        while process::alive(worker as u32, 0) && std::time::Instant::now() < end {
            std::thread::sleep(Duration::from_millis(20));
        }
        assert!(!process::alive(worker as u32, 0), "a worker ignoring SIGTERM outlived its nexus");
    }
}
