//! The scheduler of a pool whose workers are forked processes (Python, R).
//!
//! A language supplies how to fork, how a worker loads its sources, how it
//! serves one connection and, if its workers retire, what it releases first;
//! everything else -- the socket, the workers' lifetimes, the busy count that
//! decides when to start another, shutdown -- is here.

use std::collections::HashSet;
use std::ffi::{c_char, c_void, CStr};
use std::ptr;
use std::sync::atomic::{AtomicI32, AtomicPtr, AtomicU32, Ordering};
use std::time::{Duration, Instant};

/// What a language provides to run a fork pool.
#[repr(C)]
pub struct ForkPoolConfig {
    /// The language's name in messages ("py", "R").
    pub lang: *const c_char,
    pub ctx: *mut c_void,
    /// Fork a worker, returning as `morloc_fork_worker` does. `None`: that.
    pub fork: Option<unsafe extern "C" fn(*mut c_void) -> libc::pid_t>,
    /// In a new worker: load the user's sources. Non-zero: the worker ends
    /// with that status.
    pub worker_init: Option<unsafe extern "C" fn(*mut c_void) -> i32>,
    /// Serve one accepted connection and close it. Non-zero: the worker ends
    /// with that status, unretired.
    pub serve: Option<unsafe extern "C" fn(i32, *mut c_void) -> i32>,
    /// `None`: workers never retire. Otherwise run before a surplus idle
    /// worker checks whether it may retire (SHM-8).
    pub before_retire: Option<unsafe extern "C" fn(*mut c_void)>,
    /// Run in a worker just before it exits with the given status: flush
    /// what the language buffers.
    pub worker_exit: Option<unsafe extern "C" fn(*mut c_void, i32)>,
}

/// The words the coordinator and its workers share.
#[repr(C)]
struct Shared {
    shutdown: AtomicU32,
    busy: AtomicI32,
    total: AtomicI32,
}

struct Worker {
    shared: *const Shared,
    wake_w: i32,
}

// The pool this process works for, for the busy count: set in a worker and
// inherited by what the user's code forks from it; null in any other process.
static WORKER: AtomicPtr<Worker> = AtomicPtr::new(ptr::null_mut());

thread_local! {
    // Waits on another pool this thread is inside, so nested ones count once.
    static DEPTH: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
}
// The coordinator's shared words and wake-up pipe, for its signal handlers.
static COORDINATOR: AtomicPtr<Shared> = AtomicPtr::new(ptr::null_mut());
static COORDINATOR_WAKE: AtomicI32 = AtomicI32::new(-1);

/// Held while a process of a fork pool waits on another pool: the pool
/// counts it busy and, once the count reaches its workers, starts another
/// (NET-9).
pub(crate) struct Busy(Option<&'static Shared>);

pub(crate) fn busy() -> Busy {
    let w = WORKER.load(Ordering::Acquire);
    if w.is_null() {
        return Busy(None);
    }
    let outer = DEPTH.with(|d| {
        d.set(d.get() + 1);
        d.get() == 1
    });
    if !outer {
        return Busy(None);
    }
    // SAFETY: a published Worker is never freed.
    let w = unsafe { &*w };
    // SAFETY: the shared words stay mapped for the worker's life.
    let s: &'static Shared = unsafe { &*w.shared };
    let n = s.busy.fetch_add(1, Ordering::AcqRel) + 1;
    if n >= s.total.load(Ordering::Acquire) {
        wake(w.wake_w);
    }
    Busy(Some(s))
}

impl Drop for Busy {
    fn drop(&mut self) {
        if WORKER.load(Ordering::Acquire).is_null() {
            return;
        }
        DEPTH.with(|d| d.set(d.get().saturating_sub(1)));
        if let Some(s) = self.0 {
            s.busy.fetch_sub(1, Ordering::AcqRel);
        }
    }
}

fn wake(fd: i32) {
    if fd >= 0 {
        let b = 1u8;
        // SAFETY: write is async-signal-safe; a full pipe is already awake.
        unsafe { libc::write(fd, &b as *const u8 as *const c_void, 1) };
    }
}

extern "C" fn on_stop(_sig: i32) {
    // PANIC-1
    morloc_runtime_types::panic::signal_frame(|| {
        let s = COORDINATOR.load(Ordering::Relaxed);
        if !s.is_null() {
            // SAFETY: the shared words stay mapped while the handler is installed.
            unsafe { (*s).shutdown.store(1, Ordering::Release) };
        }
        wake(COORDINATOR_WAKE.load(Ordering::Relaxed));
    })
}

extern "C" fn on_child(_sig: i32) {
    // PANIC-1
    morloc_runtime_types::panic::signal_frame(|| wake(COORDINATOR_WAKE.load(Ordering::Relaxed)))
}

unsafe fn handle(sig: i32, f: extern "C" fn(i32)) {
    let mut sa: libc::sigaction = std::mem::zeroed();
    sa.sa_sigaction = f as *const () as usize;
    libc::sigemptyset(&mut sa.sa_mask);
    sa.sa_flags = libc::SA_RESTART;
    libc::sigaction(sig, &sa, ptr::null_mut());
}

unsafe fn default_handlers() {
    for sig in [libc::SIGTERM, libc::SIGINT, libc::SIGCHLD] {
        libc::signal(sig, libc::SIG_DFL);
    }
}

/// Report how a worker ended, as the pool's caller cannot see it: the
/// coordinator lives on and the caller sees only a closed connection.
fn report_end(lang: &str, pid: libc::pid_t, status: i32, shutting_down: bool) {
    if libc::WIFSIGNALED(status) {
        let sig = libc::WTERMSIG(status);
        if sig == libc::SIGTERM || (shutting_down && sig == libc::SIGKILL) {
            return;
        }
        eprintln!("morloc {lang} pool: worker {pid} crashed with signal {sig}");
    } else if libc::WIFEXITED(status) && libc::WEXITSTATUS(status) != 0 {
        eprintln!("morloc {lang} pool: worker {pid} exited with status {}", libc::WEXITSTATUS(status));
    }
}

/// Pids written to the retire pipe so far.
unsafe fn read_retired(fd: i32, into: &mut HashSet<libc::pid_t>) {
    let mut buf = [0u8; 4096];
    loop {
        let n = libc::read(fd, buf.as_mut_ptr() as *mut c_void, buf.len());
        if n <= 0 {
            return;
        }
        for c in buf[..n as usize / 4 * 4].chunks_exact(4) {
            into.insert(libc::pid_t::from_ne_bytes([c[0], c[1], c[2], c[3]]));
        }
    }
}

unsafe fn nonblocking_pipe() -> Option<[i32; 2]> {
    let mut p = [0i32; 2];
    if morloc_runtime_types::fd::pipe(p.as_mut_ptr()) != 0 {
        return None;
    }
    for fd in p {
        libc::fcntl(fd, libc::F_SETFL, libc::fcntl(fd, libc::F_GETFL) | libc::O_NONBLOCK);
    }
    Some(p)
}

struct Fds {
    listen: i32,
    wake: [i32; 2],
    retire: [i32; 2],
    // Only the coordinator holds the write end: a worker sees end of file on
    // the read end once the coordinator is gone.
    life: [i32; 2],
}

/// Run a fork pool until it is told to stop or a worker ends without
/// retiring. Returns the process's exit status.
///
/// # Safety
/// The strings must be valid C strings and `cfg`'s callbacks callable with
/// its context. Call from a process with one thread (FORK-6).
pub unsafe fn run(
    socket_path: *const c_char,
    tmpdir: *const c_char,
    shm_basename: *const c_char,
    cfg: &ForkPoolConfig,
) -> i32 {
    let lang = if cfg.lang.is_null() { "pool".to_string() } else { CStr::from_ptr(cfg.lang).to_string_lossy().into_owned() };
    libc::signal(libc::SIGPIPE, libc::SIG_IGN);
    let lifeline = crate::lifeline::adopt();
    // Workers are forked after this and inherit it.
    crate::ipc_ffi::mlc_set_self_socket(socket_path);

    let mut err: *mut c_char = ptr::null_mut();
    let mut daemon = crate::ipc_ffi::start_daemon(socket_path, tmpdir, shm_basename, 0xffff, &mut err);
    if daemon.is_null() {
        let m = if err.is_null() { "unknown error".into() } else { CStr::from_ptr(err).to_string_lossy().into_owned() };
        eprintln!("morloc {lang} pool: cannot start: {m}");
        if !err.is_null() {
            libc::free(err as *mut c_void);
        }
        return 1;
    }

    let shared = libc::mmap(
        ptr::null_mut(),
        std::mem::size_of::<Shared>(),
        libc::PROT_READ | libc::PROT_WRITE,
        libc::MAP_SHARED | libc::MAP_ANONYMOUS,
        -1,
        0,
    ) as *mut Shared;
    let pipes = (nonblocking_pipe(), nonblocking_pipe(), {
        let mut p = [0i32; 2];
        (morloc_runtime_types::fd::pipe(p.as_mut_ptr()) == 0).then_some(p)
    });
    let (Some(wake), Some(retire), Some(life)) = pipes else {
        eprintln!("morloc {lang} pool: cannot make its pipes: {}", std::io::Error::last_os_error());
        crate::ipc_ffi::close_daemon(&mut daemon);
        return 1;
    };
    if shared as *mut c_void == libc::MAP_FAILED {
        eprintln!("morloc {lang} pool: cannot map its shared words: {}", std::io::Error::last_os_error());
        crate::ipc_ffi::close_daemon(&mut daemon);
        return 1;
    }
    let shared: &'static Shared = &*shared;
    let fds = Fds { listen: (*daemon).server_fd, wake, retire, life };

    COORDINATOR.store(shared as *const Shared as *mut Shared, Ordering::Release);
    COORDINATOR_WAKE.store(fds.wake[1], Ordering::Release);
    handle(libc::SIGTERM, on_stop);
    handle(libc::SIGINT, on_stop);
    handle(libc::SIGCHLD, on_child);

    let mut workers: Vec<libc::pid_t> = Vec::new();
    let mut retired: HashSet<libc::pid_t> = HashSet::new();
    let mut failure: Option<String> = None;
    let mut lifeline_open = lifeline >= 0;

    loop {
        if shared.shutdown.load(Ordering::Acquire) != 0 {
            break;
        }
        // SHM-8: reap first; a worker that ended without retiring ends the pool.
        read_retired(fds.retire[0], &mut retired);
        let mut alive = Vec::with_capacity(workers.len());
        for &pid in &workers {
            let mut status = 0;
            if libc::waitpid(pid, &mut status, libc::WNOHANG) != pid {
                alive.push(pid);
                continue;
            }
            if !retired.contains(&pid) {
                read_retired(fds.retire[0], &mut retired);
            }
            let clean = retired.remove(&pid);
            let stopping = shared.shutdown.load(Ordering::Acquire) != 0;
            report_end(&lang, pid, status, stopping);
            if !clean && !stopping && failure.is_none() {
                failure = Some(format!("worker {pid} ended without retiring"));
            }
        }
        workers = alive;
        shared.total.store(workers.len().max(1) as i32, Ordering::Release);
        if failure.is_some() {
            break;
        }
        if workers.is_empty() || shared.busy.load(Ordering::Acquire) >= shared.total.load(Ordering::Acquire) {
            match spawn(cfg, shared, &fds) {
                Ok(pid) => {
                    workers.push(pid);
                    shared.total.store(workers.len() as i32, Ordering::Release);
                }
                Err(e) => {
                    failure = Some(e);
                    break;
                }
            }
        }

        let mut pfds = [
            libc::pollfd { fd: fds.wake[0], events: libc::POLLIN, revents: 0 },
            libc::pollfd { fd: if lifeline_open { lifeline } else { -1 }, events: libc::POLLIN, revents: 0 },
        ];
        if libc::poll(pfds.as_mut_ptr(), 2, 1000) > 0 {
            let mut buf = [0u8; 256];
            while libc::read(fds.wake[0], buf.as_mut_ptr() as *mut c_void, buf.len()) > 0 {}
            if lifeline_open && pfds[1].revents != 0 {
                let mut b = 0u8;
                let n = libc::read(lifeline, &mut b as *mut u8 as *mut c_void, 1);
                if n == 0 || (n < 0 && !matches!(crate::utility::errno_val(), libc::EAGAIN | libc::EINTR)) {
                    lifeline_open = false;
                    crate::lifeline::morloc_lifeline_teardown();
                }
            }
        }
    }

    // Stop taking calls, then end the workers.
    shared.shutdown.store(1, Ordering::Release);
    crate::ipc_ffi::close_daemon(&mut daemon);
    for &pid in &workers {
        libc::kill(pid, libc::SIGKILL);
    }
    for &pid in &workers {
        let mut status = 0;
        while libc::waitpid(pid, &mut status, 0) < 0 && crate::utility::errno_val() == libc::EINTR {}
        report_end(&lang, pid, status, true);
    }
    COORDINATOR_WAKE.store(-1, Ordering::Release);
    for fd in [fds.wake[0], fds.wake[1], fds.retire[0], fds.retire[1], fds.life[0], fds.life[1]] {
        libc::close(fd);
    }
    match failure {
        Some(why) => {
            // SHM-8: what the process held is recovered only by ending the pool.
            eprintln!("morloc {lang} pool: {why}; ending the pool so its shared memory is recovered");
            1
        }
        None => 0,
    }
}

unsafe fn spawn(cfg: &ForkPoolConfig, shared: &'static Shared, fds: &Fds) -> Result<libc::pid_t, String> {
    let mut threads: libc::c_long = 0;
    let mut errno: libc::c_int = 0;
    let pid = match cfg.fork {
        Some(f) => {
            let p = f(cfg.ctx);
            errno = crate::utility::errno_val();
            p
        }
        None => crate::fork_policy::morloc_fork_worker(&mut threads, &mut errno),
    };
    if pid == 0 {
        worker(cfg, shared, fds);
    }
    if pid == crate::fork_policy::FORK_WORKER_REFUSED {
        return Err(format!("refusing to fork a worker from a process with {threads} threads (FORK-6)"));
    }
    if pid < 0 {
        return Err(format!("cannot fork a worker: {}", std::io::Error::from_raw_os_error(errno)));
    }
    Ok(pid)
}

unsafe fn worker(cfg: &ForkPoolConfig, shared: &'static Shared, fds: &Fds) -> ! {
    default_handlers();
    COORDINATOR.store(ptr::null_mut(), Ordering::Release);
    COORDINATOR_WAKE.store(-1, Ordering::Release);
    libc::close(fds.wake[0]);
    libc::close(fds.retire[0]);
    libc::close(fds.life[1]);
    let born = crate::fork_policy::generation();
    let me = libc::getpid();
    WORKER.store(Box::into_raw(Box::new(Worker { shared, wake_w: fds.wake[1] })), Ordering::Release);
    if let Some(init) = cfg.worker_init {
        let status = init(cfg.ctx);
        if status != 0 {
            leave(cfg, status);
        }
    }
    let lang = if cfg.lang.is_null() { "pool".to_string() } else { CStr::from_ptr(cfg.lang).to_string_lossy().into_owned() };
    let timeout = crate::pool_ffi::worker_idle_timeout();
    let mut idle_since = Instant::now();
    let mut served_since_idle = false;
    let mut warned = false;
    while shared.shutdown.load(Ordering::Acquire) == 0 {
        let mut pfds = [
            libc::pollfd { fd: fds.listen, events: libc::POLLIN, revents: 0 },
            libc::pollfd { fd: fds.life[0], events: libc::POLLIN, revents: 0 },
        ];
        let ready = libc::poll(pfds.as_mut_ptr(), 2, 1000);
        if ready > 0 && pfds[1].revents != 0 {
            break;
        }
        if ready > 0 && pfds[0].revents & libc::POLLIN != 0 {
            // FORK-12: a process forked from a worker never takes a call.
            if crate::fork_policy::generation() != born {
                libc::_exit(0);
            }
            let fd = morloc_runtime_types::fd::accept_nonblocking(fds.listen);
            if fd < 0 {
                if crate::ipc_ffi::out_of_descriptors() {
                    std::thread::sleep(Duration::from_millis(50));
                }
                continue;
            }
            let status = match cfg.serve {
                Some(serve) => serve(fd, cfg.ctx),
                None => {
                    crate::ipc_ffi::close_socket(fd);
                    0
                }
            };
            // FORK-12: a process forked during the call leaves as it returns.
            crate::ipc_ffi::morloc_exit_if_forked();
            if status != 0 {
                leave(cfg, status);
            }
            served_since_idle = true;
            continue;
        }
        // FORK-15: leases of user-forked children that are gone.
        crate::lease::morloc_reclaim_leases();
        // A clock read is a system call on some hosts, so idle time is
        // measured from the first poll that found nothing.
        if served_since_idle {
            served_since_idle = false;
            idle_since = Instant::now();
        }
        let Some(before_retire) = cfg.before_retire else { continue };
        if shared.total.load(Ordering::Acquire) <= 1 || idle_since.elapsed() <= timeout {
            continue;
        }
        before_retire(cfg.ctx);
        // SHM-8: a worker retires only holding no shared memory, and says so.
        let blockers = crate::stream::morloc_retire_blockers();
        if blockers == 0 {
            crate::intrinsics::morloc_remove_own_temps();
            if let Some(exit) = cfg.worker_exit {
                exit(cfg.ctx, 0);
            }
            let token = me.to_ne_bytes();
            libc::write(fds.retire[1], token.as_ptr() as *const c_void, token.len());
            libc::_exit(0);
        }
        if !warned {
            warned = true;
            eprintln!("morloc {lang} worker {me}: idle but holding {blockers} shared-memory references; it cannot retire");
        }
        idle_since = Instant::now();
    }
    leave(cfg, 0)
}

unsafe fn leave(cfg: &ForkPoolConfig, status: i32) -> ! {
    if let Some(exit) = cfg.worker_exit {
        exit(cfg.ctx, status);
    }
    libc::_exit(status)
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub unsafe extern "C" fn morloc_fork_pool_run(
        socket_path: *const c_char,
        tmpdir: *const c_char,
        shm_basename: *const c_char,
        cfg: *const ForkPoolConfig,
    ) -> i32 {
        if cfg.is_null() {
            return 1;
        }
        super::run(socket_path, tmpdir, shm_basename, &*cfg)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU32;

    /// Shared by the test, its pool's coordinator and the pool's workers.
    #[repr(C)]
    struct Ctx {
        calls: AtomicU32,
        hold_first: AtomicU32,
        release: AtomicU32,
        die_on_call: AtomicU32,
    }

    unsafe fn shared_ctx() -> &'static Ctx {
        let p = libc::mmap(
            ptr::null_mut(),
            std::mem::size_of::<Ctx>(),
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED | libc::MAP_ANONYMOUS,
            -1,
            0,
        );
        assert_ne!(p, libc::MAP_FAILED);
        &*(p as *const Ctx)
    }

    unsafe extern "C" fn serve_ping(fd: i32, ctx: *mut c_void) -> i32 {
        let t = &*(ctx as *const Ctx);
        let mut err: *mut c_char = ptr::null_mut();
        let req = crate::ipc_ffi::stream_from_client(fd, &mut err);
        if req.is_null() {
            crate::ipc_ffi::close_socket(fd);
            return 0;
        }
        let n = t.calls.fetch_add(1, Ordering::SeqCst) + 1;
        if n == 1 && t.hold_first.load(Ordering::SeqCst) != 0 {
            let _busy = busy();
            while t.release.load(Ordering::SeqCst) == 0 {
                std::thread::sleep(Duration::from_millis(5));
            }
        }
        if t.die_on_call.load(Ordering::SeqCst) == n {
            return 70;
        }
        let pong = crate::packet_ffi::return_ping(req, &mut err);
        crate::ipc_ffi::send_reply_to_foreign_server(fd, pong, None, &mut err);
        libc::free(pong as *mut c_void);
        libc::free(req as *mut c_void);
        crate::ipc_ffi::close_socket(fd);
        0
    }

    unsafe extern "C" fn nothing_to_release(_: *mut c_void) {}

    struct Pool {
        coordinator: libc::pid_t,
        socket: std::ffi::CString,
        dir: std::path::PathBuf,
    }

    /// Start a pool in a child process of its own.
    fn start(tag: &str, ctx: &'static Ctx, retires: bool) -> Pool {
        let dir = std::env::temp_dir().join(format!("mlc-fp-{}-{tag}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let socket = std::ffi::CString::new(dir.join("s").to_str().unwrap()).unwrap();
        let tmpdir = std::ffi::CString::new(dir.to_str().unwrap()).unwrap();
        let shm = std::ffi::CString::new(format!("/mlc-{}-fp{}", std::process::id(), &tag[..1])).unwrap();
        let lang = std::ffi::CString::new("test").unwrap();
        let coordinator = unsafe { libc::fork() };
        assert!(coordinator >= 0);
        if coordinator == 0 {
            unsafe {
                let _ = crate::shm::shclose();
                if retires {
                    crate::fork_policy::set_test_env("MORLOC_POOL_IDLE_TIMEOUT_MS", Some(std::ffi::OsStr::new("100")));
                }
                let cfg = ForkPoolConfig {
                    lang: lang.as_ptr(),
                    ctx: ctx as *const Ctx as *mut c_void,
                    fork: None,
                    worker_init: None,
                    serve: Some(serve_ping),
                    before_retire: if retires { Some(nothing_to_release) } else { None },
                    worker_exit: None,
                };
                let status = run(socket.as_ptr(), tmpdir.as_ptr(), shm.as_ptr(), &cfg);
                libc::_exit(status);
            }
        }
        let pool = Pool { coordinator, socket, dir };
        for _ in 0..400 {
            if pool.dir.join("s").exists() {
                break;
            }
            std::thread::sleep(Duration::from_millis(10));
        }
        pool
    }

    fn ping(pool: &Pool) -> bool {
        let req = crate::packet::PacketHeader::ping().to_bytes();
        let mut err: *mut c_char = ptr::null_mut();
        let reply = unsafe { crate::ipc_ffi::send_and_receive_over_socket(pool.socket.as_ptr(), req.as_ptr(), &mut err) };
        unsafe {
            if !err.is_null() {
                libc::free(err as *mut c_void);
            }
            if reply.is_null() {
                return false;
            }
            let died = (*(reply as *const crate::packet::PacketHeader)).is_peer_died();
            libc::free(reply as *mut c_void);
            !died
        }
    }

    /// Wait for the coordinator to end, at most `secs`; its exit status, or
    /// `None` after killing it.
    fn finish(pool: &Pool, secs: u64) -> Option<i32> {
        let end = Instant::now() + Duration::from_secs(secs);
        loop {
            let mut status = 0;
            if unsafe { libc::waitpid(pool.coordinator, &mut status, libc::WNOHANG) } == pool.coordinator {
                let _ = std::fs::remove_dir_all(&pool.dir);
                return libc::WIFEXITED(status).then(|| libc::WEXITSTATUS(status));
            }
            if Instant::now() > end {
                crate::signal_test_child(pool.coordinator, libc::SIGKILL);
                unsafe { libc::waitpid(pool.coordinator, ptr::null_mut(), 0) };
                let _ = std::fs::remove_dir_all(&pool.dir);
                return None;
            }
            std::thread::sleep(Duration::from_millis(10));
        }
    }

    // NET-9
    #[test]
    fn a_fork_pool_serves_calls_and_stops_on_sigterm() {
        let ctx = unsafe { shared_ctx() };
        let pool = start("serve", ctx, false);
        let served = (0..3).all(|_| ping(&pool));
        crate::signal_test_child(pool.coordinator, libc::SIGTERM);
        let status = finish(&pool, 10);
        assert!(served, "a call went unanswered");
        assert_eq!(ctx.calls.load(Ordering::SeqCst), 3);
        assert_eq!(status, Some(0), "a stopped pool exits 0");
    }

    // SHM-8
    #[test]
    fn a_worker_that_ends_unretired_ends_the_pool() {
        let ctx = unsafe { shared_ctx() };
        ctx.die_on_call.store(2, Ordering::SeqCst);
        let pool = start("die", ctx, false);
        let first = ping(&pool);
        let second = ping(&pool);
        let status = finish(&pool, 10);
        assert!(first && !second);
        assert_eq!(status, Some(1), "the pool did not end when its worker did");
    }

    // NET-9: a worker waiting on another pool is replaced for the next call.
    #[test]
    fn a_busy_worker_gets_a_second_worker() {
        let ctx = unsafe { shared_ctx() };
        ctx.hold_first.store(1, Ordering::SeqCst);
        let pool = start("busy", ctx, false);
        let socket = pool.socket.clone();
        let first = std::thread::spawn(move || {
            let p = Pool { coordinator: 0, socket, dir: std::path::PathBuf::new() };
            ping(&p)
        });
        while ctx.calls.load(Ordering::SeqCst) == 0 {
            std::thread::sleep(Duration::from_millis(5));
        }
        let second = ping(&pool);
        ctx.release.store(1, Ordering::SeqCst);
        let first = first.join().unwrap();
        crate::signal_test_child(pool.coordinator, libc::SIGTERM);
        let status = finish(&pool, 10);
        assert!(second, "the call behind a busy worker went unanswered");
        assert!(first);
        assert_eq!(status, Some(0));
    }

    fn workers_of(coordinator: libc::pid_t) -> usize {
        let out = std::process::Command::new("pgrep").arg("-P").arg(coordinator.to_string()).output().unwrap();
        String::from_utf8_lossy(&out.stdout).lines().count()
    }

    // SHM-8: a surplus idle worker retires without ending the pool.
    #[test]
    fn a_retiring_worker_leaves_the_pool_running() {
        let ctx = unsafe { shared_ctx() };
        ctx.hold_first.store(1, Ordering::SeqCst);
        let pool = start("retire", ctx, true);
        let socket = pool.socket.clone();
        let first = std::thread::spawn(move || {
            let p = Pool { coordinator: 0, socket, dir: std::path::PathBuf::new() };
            ping(&p)
        });
        while ctx.calls.load(Ordering::SeqCst) == 0 {
            std::thread::sleep(Duration::from_millis(5));
        }
        let second = ping(&pool);
        let both = workers_of(pool.coordinator);
        ctx.release.store(1, Ordering::SeqCst);
        let first = first.join().unwrap();
        // Long enough for the surplus worker to retire.
        std::thread::sleep(Duration::from_millis(2500));
        let one = workers_of(pool.coordinator);
        let after = ping(&pool);
        crate::signal_test_child(pool.coordinator, libc::SIGTERM);
        let status = finish(&pool, 10);
        assert!(first && second && after, "a call went unanswered after a retirement");
        assert_eq!((both, one), (2, 1), "the surplus worker did not retire");
        assert_eq!(status, Some(0), "a retirement ended the pool");
    }

    // NET-9: processes entering and leaving waits at once leave the count
    // where it started.
    #[test]
    fn the_busy_count_stays_balanced_across_processes() {
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| unsafe {
            let shared = libc::mmap(
                ptr::null_mut(),
                std::mem::size_of::<Shared>(),
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *const Shared;
            (*shared).total.store(1000, Ordering::SeqCst);
            WORKER.store(Box::into_raw(Box::new(Worker { shared, wake_w: -1 })), Ordering::SeqCst);
            let mut kids = Vec::new();
            for _ in 0..8 {
                let pid = libc::fork();
                if pid == 0 {
                    for _ in 0..20_000 {
                        let outer = busy();
                        let nested = busy();
                        drop(nested);
                        drop(outer);
                    }
                    libc::_exit(0);
                }
                kids.push(pid);
            }
            for pid in kids {
                libc::waitpid(pid, ptr::null_mut(), 0);
            }
            (*shared).busy.load(Ordering::SeqCst) == 0
        }));
    }
}
