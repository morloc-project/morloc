//! Pool server lifecycle: accept connections, dispatch packets, manage workers.
//! Replaces pool.c. Uses std::thread instead of raw pthreads for thread mode.

use std::ffi::{c_char, c_void};
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use std::sync::{Arc, Mutex, Condvar};

// ── C-compatible types matching pool.h ───────────────────────────────────────

pub type PoolDispatchFn = unsafe extern "C" fn(
    mid: u32, args: *const *const u8, nargs: usize, ctx: *mut c_void,
) -> *mut u8;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PoolConcurrency {
    Threads = 0,
    Fork = 1,
    Single = 2,
}

#[repr(C)]
pub struct PoolConfig {
    pub local_dispatch: PoolDispatchFn,
    pub remote_dispatch: PoolDispatchFn,
    pub dispatch_ctx: *mut c_void,
    pub concurrency: PoolConcurrency,
    pub initial_workers: i32,
    pub dynamic_scaling: bool,
    pub post_fork_child: Option<unsafe extern "C" fn(*mut c_void)>,
}

// SAFETY: PoolConfig contains function pointers and a *mut c_void dispatch_ctx.
// The function pointers are set once at startup and never mutated.
// dispatch_ctx points to language-runtime state that is either thread-local
// (fork mode) or protected by the runtime's own synchronization (thread mode).
// The pool architecture guarantees dispatch_ctx is not concurrently mutated.
unsafe impl Send for PoolConfig {}
unsafe impl Sync for PoolConfig {}

// ── Global state ─────────────────────────────────────────────────────────────

static SHUTTING_DOWN: AtomicBool = AtomicBool::new(false);
static BUSY_COUNT: AtomicI32 = AtomicI32::new(0);
static TOTAL_WORKERS: AtomicI32 = AtomicI32::new(0);

// SAFETY: SHARED_BUSY is set once in pool_main_fork (parent process) before
// forking children. After fork, each process accesses the mmap'd AtomicI32
// via atomic operations only. Reset to null during shutdown.
static mut SHARED_BUSY: *mut AtomicI32 = ptr::null_mut();

#[no_mangle]
pub extern "C" fn pool_mark_busy() {
    // SAFETY: SHARED_BUSY is either null (thread mode, use local atomic) or a valid
    // mmap'd AtomicI32 pointer set during pool_main_fork initialization.
    unsafe {
        if !SHARED_BUSY.is_null() {
            (*SHARED_BUSY).fetch_add(1, Ordering::Relaxed);
        } else {
            BUSY_COUNT.fetch_add(1, Ordering::Relaxed);
        }
    }
}

#[no_mangle]
pub extern "C" fn pool_mark_idle() {
    // SAFETY: Same as pool_mark_busy - SHARED_BUSY is null or a valid mmap'd pointer.
    unsafe {
        if !SHARED_BUSY.is_null() {
            (*SHARED_BUSY).fetch_sub(1, Ordering::Relaxed);
        } else {
            BUSY_COUNT.fetch_sub(1, Ordering::Relaxed);
        }
    }
}

extern "C" fn pool_sigterm_handler(_sig: i32) {
    SHUTTING_DOWN.store(true, Ordering::Relaxed);
}

// ── Packet dispatch ──────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn pool_dispatch_packet(
    packet: *const u8,
    local_dispatch: PoolDispatchFn,
    remote_dispatch: PoolDispatchFn,
    ctx: *mut c_void,
) -> *mut u8 {
    extern "C" {
        fn make_fail_packet(msg: *const c_char) -> *mut u8;
        fn packet_is_ping(packet: *const u8, errmsg: *mut *mut c_char) -> bool;
        fn return_ping(packet: *const u8, errmsg: *mut *mut c_char) -> *mut u8;
        fn packet_is_local_call(packet: *const u8, errmsg: *mut *mut c_char) -> bool;
        fn packet_is_remote_call(packet: *const u8, errmsg: *mut *mut c_char) -> bool;
        fn read_morloc_call_packet(packet: *const u8, errmsg: *mut *mut c_char) -> *mut crate::packet_ffi::MorlocCall;
        fn free_morloc_call(call: *mut crate::packet_ffi::MorlocCall);
    }

    if packet.is_null() {
        return make_fail_packet(b"NULL packet in pool dispatch\0".as_ptr() as *const c_char);
    }

    let mut errmsg: *mut c_char = ptr::null_mut();

    if packet_is_ping(packet, &mut errmsg) {
        if !errmsg.is_null() { return fail_from_errmsg(errmsg); }
        let pong = return_ping(packet, &mut errmsg);
        if !errmsg.is_null() { return fail_from_errmsg(errmsg); }
        return pong;
    }
    if !errmsg.is_null() { return fail_from_errmsg(errmsg); }

    let is_local = packet_is_local_call(packet, &mut errmsg);
    if !errmsg.is_null() { return fail_from_errmsg(errmsg); }
    let is_remote = packet_is_remote_call(packet, &mut errmsg);
    if !errmsg.is_null() { return fail_from_errmsg(errmsg); }

    if is_local || is_remote {
        let call = read_morloc_call_packet(packet, &mut errmsg);
        if !errmsg.is_null() { return fail_from_errmsg(errmsg); }

        let mid = (*call).midx;
        let args = (*call).args as *const *const u8;
        let nargs = (*call).nargs;

        let dispatch_fn = if is_local { local_dispatch } else { remote_dispatch };
        let result = dispatch_fn(mid, args, nargs, ctx);

        free_morloc_call(call);

        if result.is_null() {
            return make_fail_packet(b"dispatch callback returned NULL\0".as_ptr() as *const c_char);
        }
        return result;
    }

    make_fail_packet(b"Unexpected packet type in pool dispatch\0".as_ptr() as *const c_char)
}

unsafe fn fail_from_errmsg(errmsg: *mut c_char) -> *mut u8 {
    extern "C" { fn make_fail_packet(msg: *const c_char) -> *mut u8; }
    let pkt = make_fail_packet(errmsg);
    libc::free(errmsg as *mut c_void);
    pkt
}

// ── Helpers ──────────────────────────────────────────────────────────────────

unsafe fn try_send_fail(client_fd: i32, msg: *const c_char) {
    extern "C" {
        fn make_fail_packet(msg: *const c_char) -> *mut u8;
        fn send_packet_to_foreign_server(fd: i32, packet: *mut u8, errmsg: *mut *mut c_char) -> usize;
    }
    let fail = make_fail_packet(if msg.is_null() { b"Unknown error\0".as_ptr() as *const c_char } else { msg });
    if !fail.is_null() {
        let mut err: *mut c_char = ptr::null_mut();
        send_packet_to_foreign_server(client_fd, fail, &mut err);
        libc::free(fail as *mut c_void);
        if !err.is_null() { libc::free(err as *mut c_void); }
    }
}

// ── Thread mode job queue ────────────────────────────────────────────────────

struct JobQueue {
    jobs: Mutex<Vec<i32>>,
    cond: Condvar,
}

impl JobQueue {
    fn new() -> Self {
        JobQueue { jobs: Mutex::new(Vec::new()), cond: Condvar::new() }
    }

    fn push(&self, fd: i32) {
        let mut jobs = self.jobs.lock().unwrap();
        jobs.push(fd);
        self.cond.notify_one();
    }

    fn pop(&self) -> Option<i32> {
        let mut jobs = self.jobs.lock().unwrap();
        loop {
            if SHUTTING_DOWN.load(Ordering::Relaxed) { return None; }
            if let Some(fd) = jobs.pop() { return Some(fd); }
            let result = self.cond.wait_timeout(jobs, std::time::Duration::from_millis(100)).unwrap();
            jobs = result.0;
        }
    }
}

// ── Worker thread ────────────────────────────────────────────────────────────

unsafe fn worker_loop(queue: &JobQueue, config: &PoolConfig) {
    extern "C" {
        fn stream_from_client(fd: i32, errmsg: *mut *mut c_char) -> *mut u8;
        fn send_packet_to_foreign_server(fd: i32, packet: *mut u8, errmsg: *mut *mut c_char) -> usize;
        fn close_socket(fd: i32);
    }

    while !SHUTTING_DOWN.load(Ordering::Relaxed) {
        let client_fd = match queue.pop() {
            Some(fd) => fd,
            None => break,
        };

        let mut errmsg: *mut c_char = ptr::null_mut();
        let data = stream_from_client(client_fd, &mut errmsg);
        if data.is_null() || !errmsg.is_null() {
            if !errmsg.is_null() {
                try_send_fail(client_fd, errmsg);
                libc::free(errmsg as *mut c_void);
            }
            libc::free(data as *mut c_void);
            close_socket(client_fd);
            continue;
        }

        // Track busy state so the accept loop can spawn new workers if needed
        pool_mark_busy();
        let result = pool_dispatch_packet(data, config.local_dispatch, config.remote_dispatch, config.dispatch_ctx);
        pool_mark_idle();
        libc::free(data as *mut c_void);

        if !result.is_null() {
            send_packet_to_foreign_server(client_fd, result, &mut errmsg);
            libc::free(result as *mut c_void);
            if !errmsg.is_null() { libc::free(errmsg as *mut c_void); }
        }

        libc::fflush(ptr::null_mut()); // flush stdout
        close_socket(client_fd);
    }
}

// ── Pool main: threads mode ──────────────────────────────────────────────────

unsafe fn pool_main_threads(config: &PoolConfig, socket_path: *const c_char, tmpdir: *const c_char, shm_basename: *const c_char) -> i32 {
    extern "C" {
        fn start_daemon(socket_path: *const c_char, tmpdir: *const c_char, shm_basename: *const c_char, size: usize, errmsg: *mut *mut c_char) -> *mut c_void;
        fn close_daemon(daemon: *mut *mut c_void);
        fn wait_for_client_with_timeout(daemon: *mut c_void, timeout_us: i32, errmsg: *mut *mut c_char) -> i32;
    }

    let mut errmsg: *mut c_char = ptr::null_mut();
    let mut daemon = start_daemon(socket_path, tmpdir, shm_basename, 0xffff, &mut errmsg);
    if !errmsg.is_null() {
        libc::fprintf(libc::fdopen(2, b"w\0".as_ptr() as *const c_char),
            b"Failed to start language server:\n%s\n\0".as_ptr() as *const c_char, errmsg);
        libc::free(errmsg as *mut c_void);
        return 1;
    }

    let queue = Arc::new(JobQueue::new());
    let nthreads = config.initial_workers.max(1) as usize;
    TOTAL_WORKERS.store(nthreads as i32, Ordering::Relaxed);

    let mut handles = Vec::with_capacity(nthreads);
    for _ in 0..nthreads {
        let q = Arc::clone(&queue);
        let cfg = ptr::read(config); // Copy config for thread
        handles.push(std::thread::spawn(move || {
            worker_loop(&q, &cfg);
        }));
    }

    while !SHUTTING_DOWN.load(Ordering::Relaxed) {
        let client_fd = wait_for_client_with_timeout(daemon, 10000, &mut errmsg);
        if !errmsg.is_null() { libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
        if client_fd > 0 {
            queue.push(client_fd);
        }

        // Dynamic scaling: spawn a new worker if all are busy
        if config.dynamic_scaling {
            let busy = BUSY_COUNT.load(Ordering::Relaxed);
            let total = TOTAL_WORKERS.load(Ordering::Relaxed);
            if busy >= total {
                let q = Arc::clone(&queue);
                let cfg = ptr::read(config);
                handles.push(std::thread::spawn(move || {
                    worker_loop(&q, &cfg);
                }));
                TOTAL_WORKERS.fetch_add(1, Ordering::Relaxed);
            }
        }
    }

    SHUTTING_DOWN.store(true, Ordering::Relaxed);
    queue.cond.notify_all();

    for h in handles { let _ = h.join(); }

    close_daemon(&mut daemon);
    0
}

// ── Pool main: single mode ───────────────────────────────────────────────────

unsafe fn pool_main_single(config: &PoolConfig, socket_path: *const c_char, tmpdir: *const c_char, shm_basename: *const c_char) -> i32 {
    extern "C" {
        fn start_daemon(socket_path: *const c_char, tmpdir: *const c_char, shm_basename: *const c_char, size: usize, errmsg: *mut *mut c_char) -> *mut c_void;
        fn close_daemon(daemon: *mut *mut c_void);
        fn wait_for_client_with_timeout(daemon: *mut c_void, timeout_us: i32, errmsg: *mut *mut c_char) -> i32;
        fn stream_from_client(fd: i32, errmsg: *mut *mut c_char) -> *mut u8;
        fn send_packet_to_foreign_server(fd: i32, packet: *mut u8, errmsg: *mut *mut c_char) -> usize;
        fn close_socket(fd: i32);
    }

    let mut errmsg: *mut c_char = ptr::null_mut();
    let mut daemon = start_daemon(socket_path, tmpdir, shm_basename, 0xffff, &mut errmsg);
    if !errmsg.is_null() {
        libc::fprintf(libc::fdopen(2, b"w\0".as_ptr() as *const c_char),
            b"Failed to start language server:\n%s\n\0".as_ptr() as *const c_char, errmsg);
        libc::free(errmsg as *mut c_void);
        return 1;
    }

    while !SHUTTING_DOWN.load(Ordering::Relaxed) {
        let client_fd = wait_for_client_with_timeout(daemon, 10000, &mut errmsg);
        if !errmsg.is_null() { libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
        if client_fd <= 0 { continue; }

        let data = stream_from_client(client_fd, &mut errmsg);
        if data.is_null() || !errmsg.is_null() {
            if !errmsg.is_null() { try_send_fail(client_fd, errmsg); libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
            libc::free(data as *mut c_void);
            close_socket(client_fd);
            continue;
        }

        let result = pool_dispatch_packet(data, config.local_dispatch, config.remote_dispatch, config.dispatch_ctx);
        libc::free(data as *mut c_void);

        if !result.is_null() {
            send_packet_to_foreign_server(client_fd, result, &mut errmsg);
            libc::free(result as *mut c_void);
            if !errmsg.is_null() { libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
        }

        libc::fflush(ptr::null_mut());
        close_socket(client_fd);
    }

    close_daemon(&mut daemon);
    0
}

// ── Pool main: fork mode ─────────────────────────────────────────────────────

unsafe fn pool_main_fork(config: &PoolConfig, socket_path: *const c_char, tmpdir: *const c_char, shm_basename: *const c_char) -> i32 {
    extern "C" {
        fn start_daemon(socket_path: *const c_char, tmpdir: *const c_char, shm_basename: *const c_char, size: usize, errmsg: *mut *mut c_char) -> *mut c_void;
        fn close_daemon(daemon: *mut *mut c_void);
        fn wait_for_client_with_timeout(daemon: *mut c_void, timeout_us: i32, errmsg: *mut *mut c_char) -> i32;
        fn stream_from_client(fd: i32, errmsg: *mut *mut c_char) -> *mut u8;
        fn send_packet_to_foreign_server(fd: i32, packet: *mut u8, errmsg: *mut *mut c_char) -> usize;
        fn close_socket(fd: i32);
        fn shinit(basename: *const c_char, volume: usize, size: usize, errmsg: *mut *mut c_char) -> *mut c_void;
    }

    let mut errmsg: *mut c_char = ptr::null_mut();
    let mut daemon = start_daemon(socket_path, tmpdir, shm_basename, 0xffff, &mut errmsg);
    if !errmsg.is_null() {
        libc::fprintf(libc::fdopen(2, b"w\0".as_ptr() as *const c_char),
            b"Failed to start language server:\n%s\n\0".as_ptr() as *const c_char, errmsg);
        libc::free(errmsg as *mut c_void);
        return 1;
    }

    // Create socketpair for fd passing
    let mut sv = [0i32; 2];
    if libc::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, sv.as_mut_ptr()) < 0 {
        close_daemon(&mut daemon);
        return 1;
    }

    // Shared busy counter via mmap
    let shared_counter = libc::mmap(
        ptr::null_mut(), std::mem::size_of::<AtomicI32>(),
        libc::PROT_READ | libc::PROT_WRITE,
        libc::MAP_SHARED | libc::MAP_ANONYMOUS, -1, 0,
    ) as *mut AtomicI32;
    if shared_counter == libc::MAP_FAILED as *mut AtomicI32 {
        libc::close(sv[0]); libc::close(sv[1]);
        close_daemon(&mut daemon);
        return 1;
    }
    (*shared_counter).store(0, Ordering::Relaxed);
    SHARED_BUSY = shared_counter;

    let nworkers = config.initial_workers.max(1);
    let mut child_pids: Vec<i32> = Vec::new();

    for i in 0..nworkers {
        let pid = libc::fork();
        if pid < 0 { break; }
        if pid == 0 {
            // Child
            libc::close(sv[1]); // close write end
            // Get daemon server_fd from opaque pointer and close it
            // (we can't access the struct fields directly since daemon is *mut c_void,
            //  but the child doesn't need to accept connections)
            if let Some(pfk) = config.post_fork_child {
                pfk(config.dispatch_ctx);
            }

            shinit(shm_basename, (i + 1) as usize, 0xffff, &mut errmsg);
            if !errmsg.is_null() { libc::free(errmsg as *mut c_void); libc::_exit(1); }

            // Worker loop: receive fds and process
            loop {
                if SHUTTING_DOWN.load(Ordering::Relaxed) { break; }
                let mut pfd = libc::pollfd { fd: sv[0], events: libc::POLLIN, revents: 0 };
                let ready = libc::poll(&mut pfd, 1, 100);
                if ready <= 0 { continue; }

                let client_fd = recv_fd(sv[0]);
                if client_fd < 0 { break; }

                let data = stream_from_client(client_fd, &mut errmsg);
                if data.is_null() || !errmsg.is_null() {
                    if !errmsg.is_null() { try_send_fail(client_fd, errmsg); libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
                    libc::free(data as *mut c_void);
                    close_socket(client_fd);
                    continue;
                }

                let result = pool_dispatch_packet(data, config.local_dispatch, config.remote_dispatch, config.dispatch_ctx);
                libc::free(data as *mut c_void);

                if !result.is_null() {
                    send_packet_to_foreign_server(client_fd, result, &mut errmsg);
                    libc::free(result as *mut c_void);
                    if !errmsg.is_null() { libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
                }
                libc::fflush(ptr::null_mut());
                close_socket(client_fd);
            }
            libc::close(sv[0]);
            libc::_exit(0);
        }
        child_pids.push(pid);
    }
    TOTAL_WORKERS.store(child_pids.len() as i32, Ordering::Relaxed);

    // Parent: accept loop
    while !SHUTTING_DOWN.load(Ordering::Relaxed) {
        let client_fd = wait_for_client_with_timeout(daemon, 10000, &mut errmsg);
        if !errmsg.is_null() { libc::free(errmsg as *mut c_void); errmsg = ptr::null_mut(); }
        if client_fd > 0 {
            send_fd(sv[1], client_fd);
            close_socket(client_fd);
        }

        // Reap dead children
        for pid in child_pids.iter_mut() {
            if *pid > 0 {
                let mut wstatus: i32 = 0;
                if libc::waitpid(*pid, &mut wstatus, libc::WNOHANG) > 0 {
                    *pid = -1;
                }
            }
        }
    }

    // Shutdown
    for &pid in &child_pids {
        if pid > 0 { libc::kill(pid, libc::SIGTERM); }
    }
    for &pid in &child_pids {
        if pid > 0 { libc::waitpid(pid, ptr::null_mut(), 0); }
    }

    libc::close(sv[0]); libc::close(sv[1]);
    libc::munmap(shared_counter as *mut c_void, std::mem::size_of::<AtomicI32>());
    SHARED_BUSY = ptr::null_mut();

    close_daemon(&mut daemon);
    0
}

// fd-passing helpers
unsafe fn send_fd(sock: i32, fd: i32) -> i32 {
    let mut buf = [0u8; 1];
    let mut iov = libc::iovec { iov_base: buf.as_mut_ptr() as *mut c_void, iov_len: 1 };
    let cmsg_space = libc::CMSG_SPACE(std::mem::size_of::<i32>() as u32) as usize;
    let mut cmsg_buf = vec![0u8; cmsg_space];

    let mut msg: libc::msghdr = std::mem::zeroed();
    msg.msg_iov = &mut iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsg_buf.as_mut_ptr() as *mut c_void;
    msg.msg_controllen = cmsg_space as _;

    let cmsg = libc::CMSG_FIRSTHDR(&msg);
    (*cmsg).cmsg_level = libc::SOL_SOCKET;
    (*cmsg).cmsg_type = libc::SCM_RIGHTS;
    (*cmsg).cmsg_len = libc::CMSG_LEN(std::mem::size_of::<i32>() as u32) as _;
    ptr::copy_nonoverlapping(&fd as *const i32 as *const u8, libc::CMSG_DATA(cmsg), std::mem::size_of::<i32>());

    if libc::sendmsg(sock, &msg, 0) >= 0 { 0 } else { -1 }
}

unsafe fn recv_fd(sock: i32) -> i32 {
    let mut buf = [0u8; 1];
    let mut iov = libc::iovec { iov_base: buf.as_mut_ptr() as *mut c_void, iov_len: 1 };
    let cmsg_space = libc::CMSG_SPACE(std::mem::size_of::<i32>() as u32) as usize;
    let mut cmsg_buf = vec![0u8; cmsg_space];

    let mut msg: libc::msghdr = std::mem::zeroed();
    msg.msg_iov = &mut iov;
    msg.msg_iovlen = 1;
    msg.msg_control = cmsg_buf.as_mut_ptr() as *mut c_void;
    msg.msg_controllen = cmsg_space as _;

    let n = libc::recvmsg(sock, &mut msg, 0);
    if n <= 0 { return -1; }

    let cmsg = libc::CMSG_FIRSTHDR(&msg);
    if cmsg.is_null() || (*cmsg).cmsg_level != libc::SOL_SOCKET || (*cmsg).cmsg_type != libc::SCM_RIGHTS {
        return -1;
    }

    let mut fd: i32 = 0;
    ptr::copy_nonoverlapping(libc::CMSG_DATA(cmsg), &mut fd as *mut i32 as *mut u8, std::mem::size_of::<i32>());
    fd
}

// ── Entry point ──────────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn pool_main(
    argc: i32,
    argv: *mut *mut c_char,
    config: *mut PoolConfig,
) -> i32 {
    if argc != 4 {
        libc::fprintf(libc::fdopen(2, b"w\0".as_ptr() as *const c_char),
            b"Usage: %s <socket_path> <tmpdir> <shm_basename>\n\0".as_ptr() as *const c_char,
            if argc > 0 { *argv } else { b"pool\0".as_ptr() as *const c_char });
        return 1;
    }

    let cfg = &mut *config;
    if cfg.initial_workers <= 0 { cfg.initial_workers = 1; }

    SHUTTING_DOWN.store(false, Ordering::Relaxed);
    BUSY_COUNT.store(0, Ordering::Relaxed);

    // SIGTERM handler
    let mut sa: libc::sigaction = std::mem::zeroed();
    sa.sa_sigaction = pool_sigterm_handler as *const () as usize;
    libc::sigemptyset(&mut sa.sa_mask);
    libc::sigaction(libc::SIGTERM, &sa, ptr::null_mut());

    let socket_path = *argv.add(1);
    let tmpdir = *argv.add(2);
    let shm_basename = *argv.add(3);

    match cfg.concurrency {
        PoolConcurrency::Threads => pool_main_threads(cfg, socket_path, tmpdir, shm_basename),
        PoolConcurrency::Fork => pool_main_fork(cfg, socket_path, tmpdir, shm_basename),
        PoolConcurrency::Single => pool_main_single(cfg, socket_path, tmpdir, shm_basename),
    }
}
