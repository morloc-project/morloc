//! C ABI wrappers for IPC functions.
//! Replaces ipc.c with Rust implementations of Unix domain socket operations.

use std::ffi::{c_char, c_void, CStr};
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// ── C types from call.h ──────────────────────────────────────────────────────

// Mirrors C `language_daemon_t` in morloc.h; layout must stay in sync.
#[repr(C)]
pub struct LanguageDaemon {
    pub socket_path: *mut c_char,
    pub tmpdir: *mut c_char,
    pub shm_basename: *mut c_char,
    pub shm: *mut crate::shm::ShmHeader,
    pub shm_default_size: usize,
    pub server_fd: i32,
    /// The socket file this pool bound, as (device, inode); (0, 0) if unknown.
    pub socket_dev: u64,
    pub socket_ino: u64,
    /// Holds that file so its inode number cannot be reused (-1 if none).
    pub socket_hold: i32,
}

const BUFFER_SIZE: usize = 65536;

// Whether the accept loop still peeks each new connection for a readiness ping.
// True until the first non-ping request is seen (readiness pings precede all
// real calls), after which steady-state accepts skip the extra peek syscall.
static PING_PEEK_ACTIVE: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(true);

// A polled fd is ready to read when data is available or the peer has
// hung up / errored (so the following recv observes EOF or the error).
#[inline]
unsafe fn pfd_ready(pfd: &libc::pollfd) -> bool {
    pfd.revents & (libc::POLLIN | libc::POLLHUP | libc::POLLERR) != 0
}

// Wait on file descriptors with poll(2). Used in preference to ppoll(2) because
// the latter is not bound by the libc crate on Apple targets; plain poll is
// portable and matches the pool/nexus poll loops. Callers handle EINTR and the
// post-wait readiness checks themselves.
//
// The timeout arrives as a nanosecond `timespec` (or null for an unbounded
// wait) and must be converted to poll's millisecond `c_int`:
//   - null       -> -1  (block indefinitely; a naive numeric convert would give
//                        0, an immediate return that busy-loops the caller)
//   - positive   -> round UP with a 1 ms floor, so a sub-millisecond request
//                   never collapses to 0 (which would fire spurious timeouts and
//                   defeat retry loops). Values are clamped to c_int range.
#[inline]
unsafe fn poll_wait(
    fds: *mut libc::pollfd,
    nfds: libc::nfds_t,
    timeout_ptr: *const libc::timespec,
) -> libc::c_int {
    let timeout_ms: libc::c_int = if timeout_ptr.is_null() {
        -1
    } else {
        let ts = &*timeout_ptr;
        let ms = ts.tv_sec as i64 * 1000 + (ts.tv_nsec as i64 + 999_999) / 1_000_000;
        // A non-null (i.e. bounded) timeout must never round down to 0, or poll
        // would return immediately instead of waiting.
        debug_assert!(ms >= 1, "poll_wait: bounded timeout rounded to {} ms", ms);
        ms.clamp(1, libc::c_int::MAX as i64) as libc::c_int
    };
    libc::poll(fds, nfds, timeout_ms)
}

// Ceiling on a zero-progress wait for socket writability in send_all, for a
// peer that cannot be named (a remote client, or a process in another pid
// namespace). A reader that drains at all resets the window.
const SEND_STALL_TIMEOUT_MS: i64 = 120_000;

#[cfg(test)]
thread_local! {
    static SEND_STALL_LIMIT: std::cell::Cell<Option<std::time::Duration>> = const { std::cell::Cell::new(None) };
}

fn send_stall_limit() -> std::time::Duration {
    #[cfg(test)]
    if let Some(d) = SEND_STALL_LIMIT.with(|l| l.get()) {
        return d;
    }
    std::time::Duration::from_millis(SEND_STALL_TIMEOUT_MS as u64)
}

// Send an entire buffer over a (possibly non-blocking) socket, polling for
// writability on EAGAIN/EWOULDBLOCK rather than treating it as failure.
//
// macOS's BSD accept() inherits O_NONBLOCK onto the accepted client fd (Linux
// does not), so a packet whose size exceeds the socket send buffer makes
// send() return -1/EAGAIN part-way through. A bare `bytes_sent <= 0` check
// misreads that as a fatal error, closes the socket mid-packet, and the peer's
// read then fails with "Connection closed early". The recv path already
// retries on EAGAIN; this mirrors it for the send side. Returns true only when
// every byte was sent, false on a closed/hung peer.
pub(crate) unsafe fn send_all(fd: i32, buf: *const u8, len: usize) -> bool {
    let mut parts = [libc::iovec { iov_base: buf as *mut c_void, iov_len: len }];
    send_parts(fd, &mut parts).is_ok()
}

/// Why a send ended before every byte went out.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum SendFail {
    /// The peer's end was closed: no byte sent from here on reached it.
    PeerClosed,
    Other,
}

/// Send `parts` in order as one stream of bytes. The iovecs are consumed.
pub(crate) unsafe fn send_parts(fd: i32, parts: &mut [libc::iovec]) -> Result<(), SendFail> {
    let stall_dur = send_stall_limit();
    let stall = libc::timespec {
        tv_sec: stall_dur.as_secs() as libc::time_t,
        tv_nsec: stall_dur.subsec_nanos() as libc::c_long,
    };
    // Wall-clock deadline for a zero-progress stall, started when a send
    // first stalls and cleared on every byte sent.
    // Bounding on elapsed time (not just per-poll timeout) prevents a signal
    // storm -- e.g. SIGCHLD from exiting pool workers -- from turning
    // send->EAGAIN->poll->EINTR->send into an unbounded busy-loop that never
    // trips the per-poll ceiling.
    let mut stalled_since: Option<std::time::Instant> = None;
    // NET-5: a stalled send to a local peer that can be named waits as long
    // as that peer lives; only one that cannot be named has the bound.
    let mut peer: Option<Option<Peer>> = None;
    let mut first = 0;
    while first < parts.len() {
        if parts[first].iov_len == 0 {
            first += 1;
            continue;
        }
        let mut msg: libc::msghdr = std::mem::zeroed();
        msg.msg_iov = parts[first..].as_mut_ptr();
        msg.msg_iovlen = (parts.len() - first) as _;
        let n = libc::sendmsg(fd, &msg, crate::utility::SEND_NOSIGNAL);
        if n > 0 {
            let mut left = n as usize;
            while left > 0 {
                let take = left.min(parts[first].iov_len);
                parts[first].iov_base = (parts[first].iov_base as *mut u8).add(take) as *mut c_void;
                parts[first].iov_len -= take;
                left -= take;
                if parts[first].iov_len == 0 {
                    first += 1;
                }
            }
            stalled_since = None;
            continue;
        }
        if n == 0 {
            return Err(SendFail::Other);
        }
        let e = crate::utility::errno_val();
        if e == libc::EINTR {
            continue;
        }
        if e == libc::EPIPE || e == libc::ECONNRESET {
            return Err(SendFail::PeerClosed);
        }
        if e == libc::EAGAIN || e == libc::EWOULDBLOCK {
            // Send buffer full on a non-blocking socket. Bail if the peer has
            // accepted zero bytes for the whole stall window (checked on
            // wall-clock so an EINTR-interrupted poll can't reset it), else wait
            // (bounded) for writability and retry the same offset. A dead peer
            // surfaces as POLLHUP/POLLERR -> the next send returns EPIPE.
            let mut pfd = libc::pollfd { fd, events: libc::POLLOUT, revents: 0 };
            if let Some(p) = peer.get_or_insert_with(|| Peer::of(fd)) {
                if poll_wait(&mut pfd, 1, &PEER_CHECK) == 0 && !p.alive() {
                    return Err(SendFail::PeerClosed);
                }
                continue;
            }
            if stalled_since.get_or_insert_with(std::time::Instant::now).elapsed() >= stall_dur {
                return Err(SendFail::Other);
            }
            let _ = poll_wait(&mut pfd, 1, &stall); // EINTR just re-loops (deadline still bounds it)
            continue;
        }
        return Err(SendFail::Other);
    }
    Ok(())
}

// Outcome of peeking a freshly accepted connection for a readiness ping.
enum PingPeek {
    Answered, // it was a ping; pong sent and fd closed -- do not enqueue
    NotPing,  // a full, non-ping header was seen -- hand the fd to a worker
    Unknown,  // header not yet fully arrived (EAGAIN/partial) -- hand off
}

// Answer a readiness ping directly from the accept loop, which is always
// available, instead of handing it to a language worker that may still be
// initializing (e.g. a Python pool importing numpy in a post-fork worker).
//
// The nexus's readiness probe uses a short (10 ms) timeout: if the ping is
// queued behind a not-yet-ready worker it times out, the nexus abandons the
// connection and retries on a fresh one, and the worker later answers into a
// closed socket ("job failed: broken pipe"). Handling the ping here makes the
// readiness signal honest -- a pool that can accept can answer.
//
// Peeks the fixed 32-byte header with MSG_PEEK (which does NOT consume), so a
// non-ping request is left fully intact for the worker to read.
unsafe fn try_answer_ping(fd: i32) -> PingPeek {
    let mut hdr = [0u8; 32];
    let peeked = libc::recv(fd, hdr.as_mut_ptr() as *mut c_void, 32, libc::MSG_PEEK);
    if peeked != 32 {
        return PingPeek::Unknown; // partial/absent header: let a worker handle it
    }
    let mut e: *mut c_char = ptr::null_mut();
    let is_ping = crate::packet_ffi::packet_is_ping(hdr.as_ptr(), &mut e);
    if !e.is_null() {
        libc::free(e as *mut c_void);
        e = ptr::null_mut();
    }
    if !is_ping {
        return PingPeek::NotPing;
    }
    // A genuine ping is EXACTLY the 32-byte header (offset==0, length==0).
    // packet_is_ping only checks magic+command-type, so a malformed 32-byte
    // "ping" with a nonzero length would make return_ping copy 32+offset+length
    // bytes -- reading past our 32-byte stack buffer. Refuse to answer such a
    // packet here; hand it to a worker, which reads the full packet off the
    // socket into a correctly sized heap buffer.
    let size = crate::packet_ffi::morloc_packet_size(hdr.as_ptr(), &mut e);
    if !e.is_null() {
        libc::free(e as *mut c_void);
        e = ptr::null_mut();
    }
    if size != 32 {
        return PingPeek::NotPing;
    }
    // Drain the ping (exactly the 32-byte header) now that it is classified,
    // then echo it back as the pong from this always-ready accept loop. The
    // pong send is best-effort and non-blocking (the fd is O_NONBLOCK): a
    // 32-byte pong on a fresh socket effectively never blocks, but if the send
    // buffer were somehow full we drop it rather than let a not-reading peer
    // head-of-line-block the single accept loop (the nexus retries the probe).
    let mut sink = [0u8; 32];
    let _ = libc::recv(fd, sink.as_mut_ptr() as *mut c_void, 32, 0);
    let pong = crate::packet_ffi::return_ping(hdr.as_ptr(), &mut e);
    if !pong.is_null() {
        let _ = libc::send(fd, pong as *const c_void, 32, crate::utility::SEND_NOSIGNAL);
        libc::free(pong as *mut c_void);
    }
    if !e.is_null() {
        libc::free(e as *mut c_void);
    }
    if trace_close_enabled() {
        eprintln!("[MLC_IPC] pid={} ping-answered fd={}", libc::getpid(), fd);
    }
    close_socket(fd);
    PingPeek::Answered
}

// ── close_socket / close_daemon ──────────────────────────────────────────────

// Diagnostic: when MORLOC_TRACE_CLOSE=1, log every socket close with the pid and
// a backtrace. This localizes the "callee closed cleanly with no trace" macOS
// flake -- the failing job's callee close shows the exact stack that closed it.
// The env var is read once and cached, so a normal (untraced) close is one
// atomic load. Off by default; enabled only by the close-trace diagnostic test.
fn trace_close_enabled() -> bool {
    static T: morloc_runtime_types::publish_once::PublishOnce<bool> = morloc_runtime_types::publish_once::PublishOnce::new();
    *T.get_or_init(|| {
        std::env::var("MORLOC_TRACE_CLOSE").map(|v| v == "1").unwrap_or(false)
    })
}

pub(crate) unsafe fn close_socket(socket_id: i32) {
    if socket_id >= 0 {
        if trace_close_enabled() {
            eprintln!(
                "[MLC_CLOSE] pid={} fd={}\n{}",
                libc::getpid(),
                socket_id,
                std::backtrace::Backtrace::force_capture()
            );
        }
        let _ = REQUEST.try_with(|r| {
            if r.get().0 == socket_id {
                r.set((-1, 0));
            }
        });
        libc::close(socket_id);
    }
}

pub(crate) unsafe fn close_daemon(daemon_ptr: *mut *mut LanguageDaemon) {
    if daemon_ptr.is_null() || (*daemon_ptr).is_null() {
        return;
    }
    let daemon = *daemon_ptr;

    close_socket((*daemon).server_fd);

    if !(*daemon).socket_path.is_null() {
        // DAEMON-10
        unlink_if_ours((*daemon).socket_path, ((*daemon).socket_dev, (*daemon).socket_ino));
        libc::free((*daemon).socket_path as *mut c_void);
    }
    if (*daemon).socket_hold >= 0 {
        libc::close((*daemon).socket_hold);
    }
    if !(*daemon).tmpdir.is_null() {
        libc::free((*daemon).tmpdir as *mut c_void);
    }
    if !(*daemon).shm_basename.is_null() {
        libc::free((*daemon).shm_basename as *mut c_void);
    }

    // DAEMON-5
    let _ = crate::shm::shretire();

    libc::free(daemon as *mut c_void);
    *daemon_ptr = ptr::null_mut();
}

// ── Socket helpers ───────────────────────────────────────────────────────────

/// The (device, inode) of the file at `path`, not following a symlink.
fn socket_file_id(path: *const c_char) -> Option<(u64, u64)> {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    (unsafe { libc::lstat(path, &mut st) } == 0).then(|| (st.st_dev as u64, st.st_ino as u64))
}

/// Hold the socket file at `path` open where the platform allows it, so its
/// inode number is not reused while this pool lives, and return its identity.
fn hold_socket_file(path: *const c_char) -> (i32, Option<(u64, u64)>) {
    #[cfg(target_os = "linux")]
    {
        let fd = unsafe { libc::open(path, libc::O_PATH | libc::O_NOFOLLOW | libc::O_CLOEXEC) };
        if fd >= 0 {
            let mut st: libc::stat = unsafe { std::mem::zeroed() };
            if unsafe { libc::fstat(fd, &mut st) } == 0 {
                return (fd, Some((st.st_dev as u64, st.st_ino as u64)));
            }
            unsafe { libc::close(fd) };
        }
    }
    (-1, socket_file_id(path))
}

/// Remove `path` only while it is still the file with identity `id`.
unsafe fn unlink_if_ours(path: *const c_char, id: (u64, u64)) {
    if id != (0, 0) && socket_file_id(path) == Some(id) {
        libc::unlink(path);
    }
}

unsafe fn new_socket(errmsg: *mut *mut c_char) -> i32 {
    clear_errmsg(errmsg);
    let fd = morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
    if fd < 0 {
        set_errmsg(errmsg, &MorlocError::Ipc("Error creating socket".into()));
        return -1;
    }
    crate::utility::set_nosigpipe(fd);
    fd
}

unsafe fn new_server_addr(socket_path: *const c_char) -> Result<libc::sockaddr_un, MorlocError> {
    crate::utility::unix_socket_addr(CStr::from_ptr(socket_path).to_bytes())
}

unsafe fn new_server(socket_path: *const c_char, errmsg: *mut *mut c_char) -> i32 {
    let server_fd = new_socket(errmsg);
    if server_fd < 0 {
        return -1;
    }

    let addr = match new_server_addr(socket_path) {
        Ok(a) => a,
        Err(e) => {
            close_socket(server_fd);
            set_errmsg(errmsg, &e);
            return -1;
        }
    };

    // Remove any existing socket file
    libc::unlink(socket_path);

    if libc::bind(server_fd, &addr as *const libc::sockaddr_un as *const libc::sockaddr,
                  std::mem::size_of::<libc::sockaddr_un>() as u32) < 0 {
        close_socket(server_fd);
        set_errmsg(errmsg, &MorlocError::Ipc("Error binding socket".into()));
        return -1;
    }

    if libc::listen(server_fd, libc::SOMAXCONN) < 0 {
        close_socket(server_fd);
        set_errmsg(errmsg, &MorlocError::Ipc("Error listening on socket".into()));
        return -1;
    }

    server_fd
}

// ── start_daemon ─────────────────────────────────────────────────────────────

/// Whether the last call failed for want of a descriptor or kernel memory.
pub(crate) fn out_of_descriptors() -> bool {
    matches!(
        std::io::Error::last_os_error().raw_os_error(),
        Some(libc::EMFILE | libc::ENFILE | libc::ENOBUFS | libc::ENOMEM)
    )
}

pub(crate) unsafe fn start_daemon(
    socket_path: *const c_char,
    tmpdir: *const c_char,
    shm_basename: *const c_char,
    shm_default_size: usize,
    errmsg: *mut *mut c_char,
) -> *mut LanguageDaemon {
    morloc_runtime_types::fd::fill_standard_descriptors();
    clear_errmsg(errmsg);

    crate::utility::raise_nofile_limit();

    let daemon = libc::calloc(1, std::mem::size_of::<LanguageDaemon>()) as *mut LanguageDaemon;
    if daemon.is_null() {
        set_errmsg(errmsg, &MorlocError::Ipc("Calloc for language_daemon_t failed".into()));
        return ptr::null_mut();
    }

    (*daemon).server_fd = -1;
    (*daemon).socket_hold = -1;
    (*daemon).socket_path = libc::strdup(socket_path);
    (*daemon).tmpdir = libc::strdup(tmpdir);
    (*daemon).shm_basename = libc::strdup(shm_basename);

    // FORK-16: the run directory, where this process's temp root lives.
    if !tmpdir.is_null() && crate::shm::get_fallback_dir().is_none_or(|d| d.is_empty()) {
        crate::shm::shm_set_fallback_dir(&CStr::from_ptr(tmpdir).to_string_lossy());
    }
    if (*daemon).socket_path.is_null() || (*daemon).tmpdir.is_null() || (*daemon).shm_basename.is_null() {
        close_daemon(&mut (daemon as *mut LanguageDaemon));
        set_errmsg(errmsg, &MorlocError::Ipc("strdup failed in start_daemon".into()));
        return ptr::null_mut();
    }

    (*daemon).shm_default_size = shm_default_size;

    // Set fallback dir for file-backed SHM
    crate::shm::shm_set_fallback_dir(&CStr::from_ptr(tmpdir).to_string_lossy());

    // Init shared memory
    let mut err: *mut c_char = ptr::null_mut();
    let shm = crate::ffi::shinit(shm_basename, shm_default_size, &mut err);
    if !err.is_null() {
        close_daemon(&mut (daemon as *mut LanguageDaemon));
        *errmsg = err;
        return ptr::null_mut();
    }
    (*daemon).shm = shm;

    // Attach the daemon to the shared stream registry. If the nexus
    // already bootstrapped it, this is a fast no-op (the segment is
    // mapped via shopen and the cached state is recorded). If the
    // daemon is the first to call it (e.g. unit-test harness with no
    // nexus), the bootstrap CAS handles allocation.
    let slot_count = crate::ffi::stream_registry_init(&mut err);
    if slot_count == usize::MAX {
        close_daemon(&mut (daemon as *mut LanguageDaemon));
        *errmsg = err;
        return ptr::null_mut();
    }

    // Create server socket
    (*daemon).server_fd = new_server(socket_path, &mut err);
    if !err.is_null() {
        close_daemon(&mut (daemon as *mut LanguageDaemon));
        *errmsg = err;
        return ptr::null_mut();
    }
    // DAEMON-10
    let (hold, id) = hold_socket_file(socket_path);
    (*daemon).socket_hold = hold;
    if let Some((dev, ino)) = id {
        (*daemon).socket_dev = dev;
        (*daemon).socket_ino = ino;
    }

    // Set non-blocking mode
    let flags = libc::fcntl((*daemon).server_fd, libc::F_GETFL);
    if flags == -1 || libc::fcntl((*daemon).server_fd, libc::F_SETFL, flags | libc::O_NONBLOCK) == -1 {
        let errno_msg = std::ffi::CStr::from_ptr(libc::strerror(crate::utility::errno_val()))
            .to_string_lossy().into_owned();
        close_daemon(&mut (daemon as *mut LanguageDaemon));
        set_errmsg(errmsg, &MorlocError::Ipc(format!("Failed to set non-blocking mode: {}", errno_msg)));
        return ptr::null_mut();
    }

    daemon
}

/// The process at the other end of a local socket, as it connected.
pub(crate) enum Peer {
    /// Its pid and start stamp, read when the connection was taken.
    Stamped { pid: u32, start: u64 },
    /// Asked of the socket only when a wait stalls: the kernel keeps the
    /// connecting process's identity with the socket, safe against pid reuse.
    #[cfg(target_os = "linux")]
    Deferred { fd: i32 },
}

impl Peer {
    pub(crate) fn of(fd: i32) -> Option<Peer> {
        Self::deferred(fd).or_else(|| Self::stamped(fd))
    }

    fn stamped(fd: i32) -> Option<Peer> {
        let pid = peer_pid(fd)?;
        Some(Peer::Stamped { pid, start: morloc_runtime_types::process::start_time(pid) })
    }

    #[cfg(target_os = "linux")]
    fn deferred(fd: i32) -> Option<Peer> {
        use std::sync::atomic::{AtomicU8, Ordering};
        const UNKNOWN: u8 = 0;
        const YES: u8 = 1;
        const NO: u8 = 2;
        static SUPPORTED: AtomicU8 = AtomicU8::new(UNKNOWN);
        peer_pid(fd)?;
        match SUPPORTED.load(Ordering::Relaxed) {
            YES => Some(Peer::Deferred { fd }),
            NO => None,
            _ => match peer_pidfd(fd) {
                Err(e) if e.raw_os_error() == Some(libc::ENOPROTOOPT) => {
                    SUPPORTED.store(NO, Ordering::Relaxed);
                    None
                }
                Ok(pidfd) => {
                    unsafe { libc::close(pidfd) };
                    SUPPORTED.store(YES, Ordering::Relaxed);
                    Some(Peer::Deferred { fd })
                }
                Err(_) => None,
            },
        }
    }

    #[cfg(not(target_os = "linux"))]
    fn deferred(_fd: i32) -> Option<Peer> {
        None
    }

    pub(crate) fn alive(&self) -> bool {
        match *self {
            Peer::Stamped { pid, start } => morloc_runtime_types::process::alive(pid, start),
            #[cfg(target_os = "linux")]
            Peer::Deferred { fd } => deferred_alive(fd),
        }
    }

    /// Read up to `len` bytes from `fd`, waiting as long as this peer lives.
    /// `Ok(0)` is end of file.
    // NET-5
    pub(crate) unsafe fn recv(&self, fd: i32, buf: *mut u8, len: usize) -> std::io::Result<usize> {
        loop {
            let n = libc::recv(fd, buf as *mut c_void, len, libc::MSG_DONTWAIT);
            if n >= 0 {
                return Ok(n as usize);
            }
            let e = std::io::Error::last_os_error();
            match e.kind() {
                std::io::ErrorKind::Interrupted => continue,
                std::io::ErrorKind::WouldBlock => {}
                _ => return Err(e),
            }
            let mut pfd = libc::pollfd { fd, events: libc::POLLIN, revents: 0 };
            if poll_wait(&mut pfd, 1, &PEER_CHECK) == 0 && !self.alive() {
                return Err(std::io::Error::new(std::io::ErrorKind::BrokenPipe, "the sending process is gone"));
            }
        }
    }
}

#[cfg(target_os = "linux")]
fn peer_pid(fd: i32) -> Option<u32> {
    let mut cred: libc::ucred = unsafe { std::mem::zeroed() };
    let mut len = std::mem::size_of::<libc::ucred>() as libc::socklen_t;
    let rc = unsafe { libc::getsockopt(fd, libc::SOL_SOCKET, libc::SO_PEERCRED, &mut cred as *mut _ as *mut c_void, &mut len) };
    (rc == 0 && cred.pid > 0).then_some(cred.pid as u32)
}

#[cfg(target_os = "linux")]
const SO_PEERPIDFD: libc::c_int = 77;

/// A pidfd for the process that connected to `fd`. Fails with ENOPROTOOPT
/// on kernels before 6.5, and EINVAL or ESRCH once that process is reaped.
#[cfg(target_os = "linux")]
fn peer_pidfd(fd: i32) -> std::io::Result<i32> {
    let mut pidfd: libc::c_int = -1;
    let mut len = std::mem::size_of::<libc::c_int>() as libc::socklen_t;
    let rc = unsafe { libc::getsockopt(fd, libc::SOL_SOCKET, SO_PEERPIDFD, &mut pidfd as *mut _ as *mut c_void, &mut len) };
    if rc == 0 { Ok(pidfd) } else { Err(std::io::Error::last_os_error()) }
}

/// A pidfd polls readable once its process has exited, reaped or not; a
/// stopped process has not.
#[cfg(target_os = "linux")]
fn deferred_alive(fd: i32) -> bool {
    match peer_pidfd(fd) {
        Ok(pidfd) => {
            let mut pfd = libc::pollfd { fd: pidfd, events: libc::POLLIN, revents: 0 };
            let exited = unsafe { libc::poll(&mut pfd, 1, 0) } == 1;
            unsafe { libc::close(pidfd) };
            !exited
        }
        Err(e) => !matches!(e.raw_os_error(), Some(libc::EINVAL) | Some(libc::ESRCH)),
    }
}

#[cfg(target_os = "macos")]
fn peer_pid(fd: i32) -> Option<u32> {
    let mut pid: libc::pid_t = 0;
    let mut len = std::mem::size_of::<libc::pid_t>() as libc::socklen_t;
    let rc = unsafe { libc::getsockopt(fd, libc::SOL_LOCAL, libc::LOCAL_PEERPID, &mut pid as *mut _ as *mut c_void, &mut len) };
    (rc == 0 && pid > 0).then_some(pid as u32)
}

/// How long a sender whose process cannot be named may stall.
const UNNAMED_SENDER_STALL: libc::timespec = libc::timespec { tv_sec: 30, tv_nsec: 0 };

/// How often a wait on a living sender checks that it still lives.
const PEER_CHECK: libc::timespec = libc::timespec { tv_sec: 1, tv_nsec: 0 };

/// How long a read waits for the sender's next bytes.
enum Wait<'a> {
    /// While this process lives (NET-5).
    Peer(&'a Peer),
    /// Until no byte has arrived for a sender that cannot be named.
    Stall,
    /// Until the sender's end closes.
    Forever,
}

// NET-5: with a peer, no time limit: a wait ends on end of file or when the peer is gone.
unsafe fn read_packet(client_fd: i32, wait: Wait, errmsg: *mut *mut c_char) -> *mut u8 {
    clear_errmsg(errmsg);

    let buffer = libc::calloc(BUFFER_SIZE, 1) as *mut u8;
    if buffer.is_null() {
        set_errmsg(errmsg, &MorlocError::Ipc("calloc failed for buffer".into()));
        return ptr::null_mut();
    }

    // poll() instead of pselect/FD_SET: an fd_set can only hold descriptors
    // below FD_SETSIZE (1024), and FD_SET on a higher fd is out-of-bounds. A
    // pollfd imposes no ceiling on the fd value.
    let mut pfd = libc::pollfd { fd: client_fd, events: libc::POLLIN, revents: 0 };

    let (timeout_ptr, peer) = match wait {
        Wait::Peer(p) => (&PEER_CHECK as *const libc::timespec, Some(p)),
        Wait::Stall => (&UNNAMED_SENDER_STALL as *const libc::timespec, None),
        Wait::Forever => (ptr::null(), None),
    };

    // Initial receive: poll, then recv, retrying on EINTR (poll) and on a
    // spurious EAGAIN/EWOULDBLOCK. A non-blocking socket can report readable via
    // poll yet return EAGAIN on recv; on macOS an accepted fd inherits the listen
    // socket's non-blocking flag, so this is reachable. Looping (rather than
    // falling through with a negative length, which then copied uninitialized
    // buffer bytes) keeps a garbage packet from being returned as success.
    // Bound consecutive poll-readable-but-recv-EAGAIN spins so a peer that keeps
    // the fd poll-readable without ever delivering data cannot defeat the
    // per-iteration timeout and hang the reader indefinitely. The normal path
    // sees zero or one spurious wakeup; this ceiling is orders of magnitude
    // above that.
    const MAX_SPURIOUS_WAKEUPS: u32 = 1024;
    let mut spurious: u32 = 0;
    // The size is read from the header, so the first read lasts until the
    // whole header is in.
    let mut have = 0usize;
    let recv_length = loop {
        let mut ready;
        loop {
            ready = poll_wait(&mut pfd, 1, timeout_ptr);
            if !(ready < 0 && crate::utility::errno_val() == libc::EINTR) {
                break;
            }
        }

        if ready == 0 {
            if peer.is_some_and(Peer::alive) {
                continue;
            }
            libc::free(buffer as *mut c_void);
            let why = if peer.is_some() { "The sending process is gone" } else { "Timeout waiting for initial data" };
            set_errmsg(errmsg, &MorlocError::Ipc(why.into()));
            return ptr::null_mut();
        }
        if ready < 0 {
            libc::free(buffer as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Ipc("poll error".into()));
            return ptr::null_mut();
        }
        if !pfd_ready(&pfd) {
            libc::free(buffer as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Ipc("Bad client file descriptor".into()));
            return ptr::null_mut();
        }

        let n = libc::recv(client_fd, buffer.add(have) as *mut c_void, BUFFER_SIZE - have, 0);
        if n == 0 {
            if trace_close_enabled() {
                eprintln!("[MLC_IPC] pid={} recv EOF (0 bytes) fd={}", libc::getpid(), client_fd);
            }
            libc::free(buffer as *mut c_void);
            let why = if have == 0 { "Connection closed by peer" } else { "Connection closed early" };
            set_errmsg(errmsg, &MorlocError::Ipc(why.into()));
            return ptr::null_mut();
        }
        if n > 0 {
            have += n as usize;
            if have >= morloc_runtime_types::packet::PKT_HEADER_SIZE {
                break have as isize;
            }
            continue;
        }
        let e = crate::utility::errno_val();
        if e == libc::EWOULDBLOCK || e == libc::EAGAIN {
            spurious += 1;
            if spurious > MAX_SPURIOUS_WAKEUPS {
                libc::free(buffer as *mut c_void);
                set_errmsg(errmsg, &MorlocError::Ipc("Timeout waiting for initial data".into()));
                return ptr::null_mut();
            }
            continue; // no data yet despite poll; re-poll and retry
        }
        libc::free(buffer as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Ipc("Recv error".into()));
        return ptr::null_mut();
    };

    // Get packet size from header
    let mut packet_err: *mut c_char = ptr::null_mut();
    let packet_length = crate::packet_ffi::morloc_packet_size(buffer, &mut packet_err);
    if !packet_err.is_null() {
        libc::free(buffer as *mut c_void);
        *errmsg = packet_err;
        return ptr::null_mut();
    }

    let result = libc::calloc(packet_length, 1) as *mut u8;
    if result.is_null() {
        libc::free(buffer as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Ipc("calloc failure".into()));
        return ptr::null_mut();
    }

    let copy_length = (recv_length as usize).min(packet_length);
    ptr::copy_nonoverlapping(buffer, result, copy_length);
    let mut data_ptr = result.add(copy_length);
    libc::free(buffer as *mut c_void);

    while (data_ptr as usize - result as usize) < packet_length {
        let ready = poll_wait(&mut pfd, 1, timeout_ptr);
        if ready == 0 {
            if peer.is_some_and(Peer::alive) {
                continue;
            }
            libc::free(result as *mut c_void);
            let why = if peer.is_some() { "The sending process is gone" } else { "Timeout waiting for remaining data" };
            set_errmsg(errmsg, &MorlocError::Ipc(why.into()));
            return ptr::null_mut();
        }
        if ready < 0 {
            if crate::utility::errno_val() == libc::EINTR {
                continue;
            }
            libc::free(result as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Ipc("poll error".into()));
            return ptr::null_mut();
        }
        if !pfd_ready(&pfd) {
            continue;
        }
        let remaining = packet_length - (data_ptr as usize - result as usize);
        let n = libc::recv(client_fd, data_ptr as *mut c_void, remaining.min(BUFFER_SIZE), 0);
        if n > 0 {
            data_ptr = data_ptr.add(n as usize);
            continue;
        }
        if n == 0 {
            libc::free(result as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Ipc("Connection closed early".into()));
            return ptr::null_mut();
        }
        let e = crate::utility::errno_val();
        if e == libc::EWOULDBLOCK || e == libc::EAGAIN {
            spurious += 1;
            if spurious <= MAX_SPURIOUS_WAKEUPS {
                continue;
            }
            libc::free(result as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Ipc("Timeout waiting for remaining data".into()));
            return ptr::null_mut();
        }
        libc::free(result as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Ipc("Recv error".into()));
        return ptr::null_mut();
    }

    result
}

pub(crate) unsafe fn stream_from_client(
    client_fd: i32,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    // NET-5: a sender that cannot be named has its stalls bounded instead.
    let request = match Peer::of(client_fd) {
        Some(peer) => read_packet(client_fd, Wait::Peer(&peer), errmsg),
        None => read_packet(client_fd, Wait::Stall, errmsg),
    };
    if !request.is_null() {
        REQUEST.with(|r| r.set((client_fd, crate::fork_policy::generation())));
    }
    request
}

thread_local! {
    static REQUEST: std::cell::Cell<(i32, u64)> = const { std::cell::Cell::new((-1, 0)) };
}

// FORK-12
fn refuse_a_reply_from_a_fork(client_fd: i32) {
    let (fd, generation) = REQUEST.with(|r| r.get());
    if fd == client_fd {
        exit_if_forked_since(generation);
    }
}

// FORK-12: called as soon as user code returns, on the thread that read the request.
pub(crate) fn morloc_exit_if_forked() {
    let (fd, generation) = REQUEST.with(|r| r.get());
    if fd >= 0 {
        exit_if_forked_since(generation);
    }
}

pub(crate) fn exit_if_forked_since(generation: u64) {
    if generation != crate::fork_policy::generation() {
        let msg = b"morloc: a process forked during a call returned into the runtime; it exits instead of replying\n";
        unsafe {
            libc::write(2, msg.as_ptr() as *const c_void, msg.len());
            libc::_exit(1);
        }
    }
}

// ── Self-call guard ──────────────────────────────────────────────────────────

/// The socket this process serves, when it is a pool. Set before any worker
/// is forked, so forked workers inherit it.
pub(crate) static SELF_SOCKET: crate::fork_policy::Held<Option<std::path::PathBuf>> = crate::fork_policy::Held::new(13, None);

/// `MORLOC_FORBID_SELF_CALL`: a test guard. A pool sending a call to its own
/// socket is a call between co-located code taking the serial path; with the
/// guard set that call fails instead.
fn forbid_self_call() -> bool {
    static FORBID: morloc_runtime_types::publish_once::PublishOnce<bool> = morloc_runtime_types::publish_once::PublishOnce::new();
    *FORBID.get_or_init(|| std::env::var_os("MORLOC_FORBID_SELF_CALL").is_some())
}

/// Record the socket this pool serves. Called from pool startup.
pub(crate) unsafe fn mlc_set_self_socket(socket_path: *const c_char) {
    if socket_path.is_null() {
        return;
    }
    let path = std::path::PathBuf::from(CStr::from_ptr(socket_path).to_string_lossy().into_owned());
    *SELF_SOCKET.lock() = Some(path);
}

fn is_self_socket(socket_path: *const c_char) -> bool {
    let target = std::path::PathBuf::from(unsafe { CStr::from_ptr(socket_path) }.to_string_lossy().into_owned());
    same_socket(SELF_SOCKET.lock().as_deref(), &target)
}

/// Compared component by component, so `dir/x` and `dir//x` or a trailing
/// separator on the directory still match.
fn same_socket(own: Option<&std::path::Path>, target: &std::path::Path) -> bool {
    own.is_some_and(|own| own.components().eq(target.components()))
}

#[cfg(test)]
mod self_call_tests {
    use super::same_socket;
    use std::path::Path;

    #[test]
    fn a_pool_recognizes_its_own_socket_however_it_is_spelled() {
        let own = Path::new("/tmp/morloc.abc/pipe-cpp");
        assert!(same_socket(Some(own), Path::new("/tmp/morloc.abc/pipe-cpp")));
        assert!(same_socket(Some(own), Path::new("/tmp/morloc.abc//pipe-cpp")));
        assert!(same_socket(Some(own), Path::new("/tmp/morloc.abc/./pipe-cpp")));
    }

    #[test]
    fn another_pool_or_no_registration_is_not_a_self_call() {
        let own = Path::new("/tmp/morloc.abc/pipe-cpp");
        assert!(!same_socket(Some(own), Path::new("/tmp/morloc.abc/pipe-py")));
        assert!(!same_socket(Some(own), Path::new("/tmp/morloc.xyz/pipe-cpp")));
        assert!(!same_socket(None, Path::new("/tmp/morloc.abc/pipe-cpp")));
    }
}

unsafe fn self_call_error(socket_path: *const c_char, packet: *const u8) -> MorlocError {
    let mut err: *mut c_char = ptr::null_mut();
    let header = crate::packet_ffi::read_morloc_packet_header(packet, &mut err);
    let mid = if !header.is_null() && (*header).command_type() == crate::packet::PACKET_TYPE_CALL {
        format!("{}", { (*header).command.call.midx })
    } else {
        "?".into()
    };
    if !err.is_null() {
        libc::free(err as *mut c_void);
    }
    MorlocError::Ipc(format!(
        "MORLOC_FORBID_SELF_CALL: pool called its own socket '{}' (mid {})",
        CStr::from_ptr(socket_path).to_string_lossy(),
        mid
    ))
}

// ── send_and_receive_over_socket ─────────────────────────────────────────────

// NET-7: every caller connection's descriptor, so the fork handler can
// close a child's copies of them without a lock.
const CONN_SLOTS: usize = 256;
static CONN_FDS: [std::sync::atomic::AtomicI32; CONN_SLOTS] =
    [const { std::sync::atomic::AtomicI32::new(-1) }; CONN_SLOTS];

fn track_conn(fd: i32) -> bool {
    use std::sync::atomic::Ordering;
    CONN_FDS.iter().any(|s| s.compare_exchange(-1, fd, Ordering::AcqRel, Ordering::Relaxed).is_ok())
}

fn untrack_conn(fd: i32) {
    use std::sync::atomic::Ordering;
    for s in CONN_FDS.iter() {
        if s.compare_exchange(fd, -1, Ordering::AcqRel, Ordering::Relaxed).is_ok() {
            return;
        }
    }
}

// NET-7: untracked before it is closed, so a child never closes a reused number.
unsafe fn drop_conn(fd: i32) {
    untrack_conn(fd);
    close_socket(fd);
}

// NET-7: a forked child closes its copies of its parent's connections.
pub(crate) fn after_fork_in_child() {
    use std::sync::atomic::Ordering;
    for s in CONN_FDS.iter() {
        let fd = s.swap(-1, Ordering::AcqRel);
        if fd >= 0 {
            unsafe { libc::close(fd) };
        }
    }
}

struct CachedConn {
    path: Vec<u8>,
    fd: i32,
    generation: u64,
}

struct ConnCache(Vec<CachedConn>);

impl Drop for ConnCache {
    fn drop(&mut self) {
        // FORK-14: an entry from before a fork was closed by the fork handler.
        let generation = crate::fork_policy::generation();
        for c in self.0.drain(..) {
            if c.generation == generation {
                unsafe { drop_conn(c.fd) };
            }
        }
    }
}

thread_local! {
    // NET-7
    static CONN_CACHE: std::cell::RefCell<ConnCache> = const { std::cell::RefCell::new(ConnCache(Vec::new())) };
}

fn take_cached(path: &[u8]) -> Option<i32> {
    // FORK-14
    let generation = crate::fork_policy::generation();
    CONN_CACHE
        .try_with(|cache| {
            let mut cache = cache.borrow_mut();
            let i = cache.0.iter().position(|c| c.path == path)?;
            let c = cache.0.swap_remove(i);
            (c.generation == generation).then_some(c.fd)
        })
        .ok()
        .flatten()
}

fn put_cached(path: &[u8], fd: i32) -> bool {
    let generation = crate::fork_policy::generation();
    CONN_CACHE
        .try_with(|cache| cache.borrow_mut().0.push(CachedConn { path: path.to_vec(), fd, generation }))
        .is_ok()
}

/// How many connections this thread holds open for reuse.
#[cfg(test)]
pub(crate) fn cached_connections() -> usize {
    CONN_CACHE.with(|c| c.borrow().0.len())
}

#[cfg(test)]
pub(crate) fn drop_cached_connections() {
    CONN_CACHE.with(|c| {
        for e in c.borrow_mut().0.drain(..) {
            unsafe { drop_conn(e.fd) };
        }
    });
}

/// Connect to `socket_path`, retrying while the pool starts. The descriptor
/// is tracked when there is room, which allows it to be reused.
unsafe fn connect_to(socket_path: *const c_char) -> Result<(i32, bool), MorlocError> {
    let mut err: *mut c_char = ptr::null_mut();
    let client_fd = new_socket(&mut err);
    if client_fd < 0 {
        let msg = CStr::from_ptr(err).to_string_lossy().into_owned();
        libc::free(err as *mut c_void);
        return Err(MorlocError::Ipc(msg));
    }
    let tracked = track_conn(client_fd);
    let fail = |e: MorlocError| {
        drop_conn(client_fd);
        Err(e)
    };
    let addr = match new_server_addr(socket_path) {
        Ok(a) => a,
        Err(e) => return fail(e),
    };
    let mut attempts = 0;
    loop {
        let retcode = libc::connect(client_fd, &addr as *const libc::sockaddr_un as *const libc::sockaddr,
                                    std::mem::size_of::<libc::sockaddr_un>() as u32);
        if retcode == 0 {
            return Ok((client_fd, tracked));
        }
        attempts += 1;
        if crate::daemon_ffi::is_recovering() || crate::daemon_ffi::is_shutting_down() {
            return fail(MorlocError::Ipc(format!(
                "Failed to connect to pipe '{}': the daemon is {}",
                CStr::from_ptr(socket_path).to_string_lossy(),
                if crate::daemon_ffi::is_recovering() { "recovering from a pool crash" } else { "shutting down" }
            )));
        }
        if attempts > 300 { // ~30 seconds with 100ms sleep
            return fail(MorlocError::Ipc(format!(
                "Failed to connect to pipe '{}'",
                CStr::from_ptr(socket_path).to_string_lossy()
            )));
        }
        libc::usleep(100_000); // 100ms
    }
}

/// Send `packet` on `fd`, asking the callee to keep the connection when
/// `keep`. The header goes from a copy, since the packet is the caller's.
unsafe fn send_request(fd: i32, packet: *const u8, size: usize, keep: bool) -> Result<(), SendFail> {
    use morloc_runtime_types::packet::{MODE_KEEP, PKT_HEADER_SIZE, PKT_MODE_OFF};
    if !keep || size < PKT_HEADER_SIZE {
        let mut parts = [libc::iovec { iov_base: packet as *mut c_void, iov_len: size }];
        return send_parts(fd, &mut parts);
    }
    let mut header = [0u8; PKT_HEADER_SIZE];
    ptr::copy_nonoverlapping(packet, header.as_mut_ptr(), PKT_HEADER_SIZE);
    let mode = u16::from_le_bytes([header[PKT_MODE_OFF], header[PKT_MODE_OFF + 1]]) | MODE_KEEP;
    header[PKT_MODE_OFF..PKT_MODE_OFF + 2].copy_from_slice(&mode.to_le_bytes());
    let mut parts = [
        libc::iovec { iov_base: header.as_mut_ptr() as *mut c_void, iov_len: PKT_HEADER_SIZE },
        libc::iovec { iov_base: packet.add(PKT_HEADER_SIZE) as *mut c_void, iov_len: size - PKT_HEADER_SIZE },
    ];
    send_parts(fd, &mut parts)
}

/// Whether `packet`'s mode asks to keep the connection; clears the bit.
pub(crate) unsafe fn take_keep_bit(packet: *mut u8) -> bool {
    use morloc_runtime_types::packet::{MODE_KEEP, PKT_MODE_OFF};
    let at = packet.add(PKT_MODE_OFF);
    let mode = u16::from_le_bytes([*at, *at.add(1)]);
    if mode & MODE_KEEP == 0 {
        return false;
    }
    let cleared = (mode & !MODE_KEEP).to_le_bytes();
    *at = cleared[0];
    *at.add(1) = cleared[1];
    true
}

/// Set the keep bit on a reply the callee is about to send.
pub(crate) unsafe fn set_keep_bit(packet: *mut u8) {
    use morloc_runtime_types::packet::{MODE_KEEP, PKT_MODE_OFF};
    let at = packet.add(PKT_MODE_OFF);
    let mode = (u16::from_le_bytes([*at, *at.add(1)]) | MODE_KEEP).to_le_bytes();
    *at = mode[0];
    *at.add(1) = mode[1];
}

pub(crate) unsafe fn send_and_receive_over_socket(
    socket_path: *const c_char,
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    if forbid_self_call() && is_self_socket(socket_path) {
        set_errmsg(errmsg, &self_call_error(socket_path, packet));
        return ptr::null_mut();
    }

    let mut err: *mut c_char = ptr::null_mut();
    let packet_size = crate::packet_ffi::morloc_packet_size(packet, &mut err);
    if !err.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    // NET-7: a reused connection whose callee is gone fails its send with
    // the peer closed, which delivered nothing; that alone is retried.
    let path = CStr::from_ptr(socket_path).to_bytes();
    let mut cached = take_cached(path);
    let (client_fd, tracked) = loop {
        let reused = cached.is_some();
        let (fd, tracked) = match cached.take() {
            Some(fd) => (fd, true),
            None => match connect_to(socket_path) {
                Ok(c) => c,
                Err(e) => {
                    set_errmsg(errmsg, &e);
                    return ptr::null_mut();
                }
            },
        };
        match send_request(fd, packet, packet_size, tracked) {
            Ok(()) => break (fd, tracked),
            Err(SendFail::PeerClosed) if reused => drop_conn(fd),
            Err(_) => {
                drop_conn(fd);
                set_errmsg(errmsg, &MorlocError::Ipc(format!(
                    "Failed to send data to '{}'",
                    CStr::from_ptr(socket_path).to_string_lossy()
                )));
                return ptr::null_mut();
            }
        }
    };

    if trace_close_enabled() {
        eprintln!("[MLC_IPC] pid={} sent request fd={} bytes={} to '{}'",
            libc::getpid(), client_fd, packet_size,
            CStr::from_ptr(socket_path).to_string_lossy());
    }

    // NET-5: the reply waits as long as the callee's process lives; a callee
    // that cannot be named is waited on until its end closes.
    let result = match Peer::of(client_fd) {
        Some(peer) => read_packet(client_fd, Wait::Peer(&peer), &mut err),
        None => read_packet(client_fd, Wait::Forever, &mut err),
    };
    if !err.is_null() {
        drop_conn(client_fd);
        *errmsg = err;
        return ptr::null_mut();
    }

    let kept = take_keep_bit(result);
    if !(kept && tracked && put_cached(path, client_fd)) {
        drop_conn(client_fd);
    }
    // SHM-8
    crate::packet_ffi::inherit_reply(result);
    result
}

// -- mlc_spawn ----------------------------------------------------------------

/// Start the producer of the channel `handle`: a local call to manifold `mid`
/// in the pool at `socket_path`, made and watched by the nexus. The watcher
/// must outlive the producer, and any pool process -- this one included --
/// may be the one that runs the producer, or be reaped once idle.
pub(crate) unsafe fn mlc_spawn(
    socket_path: *const c_char,
    mid: u32,
    args: *const *const u8,
    nargs: usize,
    handle: i64,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let mut packets: Vec<&[u8]> = Vec::with_capacity(nargs);
    for i in 0..nargs {
        let p = *args.add(i);
        let mut err: *mut c_char = ptr::null_mut();
        let size = crate::packet_ffi::morloc_packet_size(p, &mut err);
        if !err.is_null() {
            *errmsg = err;
            return false;
        }
        packets.push(std::slice::from_raw_parts(p, size));
    }
    let path = CStr::from_ptr(socket_path).to_bytes();
    match crate::stream::nexus_spawn(handle, mid, path, &packets) {
        Ok(()) => true,
        Err(e) => {
            set_errmsg(errmsg, &e);
            false
        }
    }
}

/// Start a local call to manifold `mid` in the pool at `socket_path` without
/// waiting for its reply: the producer of the channel `handle`. A thread of
/// this process waits for the reply instead. The call ends only when the
/// producer does, so a failed producer -- or its pool dying -- is recorded
/// on the channel, where every reader of it (in any pool) will see it after
/// the batches already queued. The call is a dispatch of its own, so the
/// producer runs alongside whatever its pool does next. Run by the nexus.
pub(crate) unsafe fn mlc_spawn_watched(
    socket_path: *const c_char,
    mid: u32,
    args: *const *const u8,
    nargs: usize,
    handle: i64,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();
    let packet = crate::packet_ffi::make_morloc_local_call_packet(mid, args, nargs, &mut err);
    if !err.is_null() {
        *errmsg = err;
        return false;
    }
    let fd = new_socket(&mut err);
    if fd < 0 {
        libc::free(packet as *mut c_void);
        *errmsg = err;
        return false;
    }
    let addr = match new_server_addr(socket_path) {
        Ok(a) => a,
        Err(e) => {
            close_socket(fd);
            libc::free(packet as *mut c_void);
            set_errmsg(errmsg, &e);
            return false;
        }
    };
    let mut attempts = 0;
    while libc::connect(fd, &addr as *const libc::sockaddr_un as *const libc::sockaddr,
                        std::mem::size_of::<libc::sockaddr_un>() as u32) != 0 {
        attempts += 1;
        if attempts > 300 {
            close_socket(fd);
            libc::free(packet as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Ipc(format!(
                "Failed to connect to pipe '{}'", CStr::from_ptr(socket_path).to_string_lossy()
            )));
            return false;
        }
        libc::usleep(100_000);
    }
    let size = crate::packet_ffi::morloc_packet_size(packet, &mut err);
    let sent = err.is_null() && send_all(fd, packet, size);
    libc::free(packet as *mut c_void);
    if !sent {
        close_socket(fd);
        if err.is_null() {
            set_errmsg(errmsg, &MorlocError::Ipc("Failed to send the producer's call".into()));
        } else {
            *errmsg = err;
        }
        return false;
    }
    // DAEMON-1
    let Some(user) = crate::daemon_ffi::enter_shm_user() else {
        close_socket(fd);
        set_errmsg(errmsg, &MorlocError::Ipc("the daemon is recovering from a pool crash".into()));
        return false;
    };
    std::thread::spawn(move || {
        let _user = user;
        let mut err: *mut c_char = ptr::null_mut();
        let result = read_packet(fd, Wait::Forever, &mut err);
        close_socket(fd);
        // SHM-8
        crate::packet_ffi::inherit_reply(result);
        if !err.is_null() {
            libc::free(err as *mut c_void);
            let _ = crate::stream::shared_channel_fail(
                handle, "the process parsing this stream stopped before it finished",
            );
            return;
        }
        let mut perr: *mut c_char = ptr::null_mut();
        let msg = crate::packet_ffi::get_morloc_data_packet_error_message(result, &mut perr);
        if !perr.is_null() {
            libc::free(perr as *mut c_void);
        }
        if msg.is_null() {
            let _ = crate::stream::shared_channel_ended(handle);
        } else {
            let text = CStr::from_ptr(msg).to_string_lossy().into_owned();
            libc::free(msg as *mut c_void);
            let _ = crate::stream::shared_channel_fail(handle, &text);
        }
        crate::packet_ffi::release_received_packet(result);
    });
    true
}

// ── send_packet_to_foreign_server ────────────────────────────────────────────

pub(crate) unsafe fn send_packet_to_foreign_server(
    client_fd: i32,
    packet: *mut u8,
    errmsg: *mut *mut c_char,
) -> usize {
    send_reply_to_foreign_server(client_fd, packet, None, errmsg)
}

/// Send a dispatch's reply, running `release` once the reply holds the
/// caller's own reference to its value and before any of it is sent. A pool
/// passes the release of everything the dispatch still holds: a caller that
/// has its value can ask for the next one at once, and if that request is
/// answered while this dispatch still holds the previous value, both are in
/// memory together. `release` runs whether or not the send succeeds.
pub(crate) unsafe fn send_reply_to_foreign_server(
    client_fd: i32,
    packet: *mut u8,
    release: Option<unsafe extern "C" fn()>,
    errmsg: *mut *mut c_char,
) -> usize {
    refuse_a_reply_from_a_fork(client_fd);
    clear_errmsg(errmsg);

    let mut err: *mut c_char = ptr::null_mut();
    let size = crate::packet_ffi::morloc_packet_size(packet, &mut err);
    if !err.is_null() {
        if let Some(f) = release { f(); }
        *errmsg = err;
        return 0;
    }

    // The result carries its own reference from here, taken before the packet
    // leaves this process. See `donate_packet_reference`.
    let donated = crate::packet_ffi::donate_packet_reference(packet);
    if let Some(f) = release { f(); }
    if let Err(e) = donated {
        set_errmsg(errmsg, &e);
        return 0;
    }

    if !send_all(client_fd, packet, size) {
        // Nobody received the packet, so nobody will release the reference
        // taken for its recipient.
        crate::packet_ffi::revoke_packet_reference(packet);
        set_errmsg(errmsg, &MorlocError::Ipc(format!(
            "Failed to send over client {}", client_fd
        )));
        return 0;
    }

    size
}

// ── wait_for_client ──────────────────────────────────────────────────────────

/// What a wait on a listening socket, and on descriptors watched beside
/// it, saw.
pub(crate) enum Accepted {
    /// A connection to serve.
    Client(i32),
    /// The watched descriptor at this index is readable or has hung up.
    Watched(usize),
    /// Nothing to act on: the wait timed out or was interrupted, or a
    /// readiness ping was answered here.
    Nothing,
}

pub(crate) unsafe fn wait_for_client_with_timeout(
    daemon: *mut LanguageDaemon,
    timeout_us: i32,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);
    match accept_or_watch((*daemon).server_fd, timeout_us, &[]) {
        Ok(Accepted::Client(fd)) => fd,
        Ok(_) => 0,
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1
        }
    }
}

/// Wait up to `timeout_us` (none if 0) for a connection on `server_fd` or
/// for one of `watch` to become readable, and accept the connection.
pub(crate) unsafe fn accept_or_watch(server_fd: i32, timeout_us: i32, watch: &[i32]) -> Result<Accepted, MorlocError> {
    // poll() instead of pselect/FD_SET so any fd value is accepted (an fd_set
    // caps at FD_SETSIZE=1024).
    let mut pfds: Vec<libc::pollfd> = std::iter::once(server_fd)
        .chain(watch.iter().copied())
        .map(|fd| libc::pollfd { fd, events: libc::POLLIN, revents: 0 })
        .collect();

    let mut ts: libc::timespec = std::mem::zeroed();
    let timeout_ptr = if timeout_us > 0 {
        ts.tv_sec = (timeout_us / 1000000) as i64;
        ts.tv_nsec = ((timeout_us % 1000000) * 1000) as i64;
        &ts as *const libc::timespec
    } else {
        ptr::null()
    };

    let ready = poll_wait(pfds.as_mut_ptr(), pfds.len() as libc::nfds_t, timeout_ptr);
    if ready < 0 {
        if crate::utility::errno_val() == libc::EINTR {
            return Ok(Accepted::Nothing);
        }
        return Err(MorlocError::Ipc("poll error".into()));
    }
    if ready == 0 {
        return Ok(Accepted::Nothing);
    }
    if let Some(i) = pfds[1..].iter().position(|p| pfd_ready(p)) {
        return Ok(Accepted::Watched(i));
    }
    if pfds[0].revents & libc::POLLIN == 0 {
        return Ok(Accepted::Nothing);
    }
    let selected_fd = morloc_runtime_types::fd::accept(server_fd, ptr::null_mut(), ptr::null_mut());
    if selected_fd < 0 {
        if out_of_descriptors() {
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
        return Ok(Accepted::Nothing);
    }
    if trace_close_enabled() {
        eprintln!("[MLC_IPC] pid={} accept fd={}", libc::getpid(), selected_fd);
    }
    crate::utility::set_nosigpipe(selected_fd);
    libc::fcntl(selected_fd, libc::F_SETFL, libc::O_NONBLOCK);

    // Short-circuit readiness pings here (see try_answer_ping) so they
    // never wait on a possibly-initializing worker. Peeking costs one
    // extra recv per accept, so only do it during startup: readiness
    // pings always precede the first real call, so once a non-ping
    // request is seen the peek is disabled and steady-state calls pay
    // nothing (a later recovery ping falls through to a now-warm worker,
    // with the pool-side silent-pong-drop as backstop).
    if PING_PEEK_ACTIVE.load(std::sync::atomic::Ordering::Relaxed) {
        match try_answer_ping(selected_fd) {
            PingPeek::Answered => return Ok(Accepted::Nothing),
            PingPeek::NotPing => {
                PING_PEEK_ACTIVE.store(false, std::sync::atomic::Ordering::Relaxed);
            }
            PingPeek::Unknown => {}
        }
    }
    Ok(Accepted::Client(selected_fd))
}

#[cfg(test)]
mod tests {
    use super::*;

    // send_all must deliver every byte of a payload larger than the socket
    // send buffer even when the sending fd is non-blocking -- the exact macOS
    // condition (BSD accept inherits O_NONBLOCK) that made the old bare
    // `bytes_sent <= 0` send loop truncate a packet and close the socket
    // mid-message ("Connection closed early"). A slow reader forces the send
    // buffer to fill, so send() returns EAGAIN part-way and send_all must poll
    // for writability and resume rather than bail.
    #[test]
    fn send_all_completes_across_eagain_on_nonblocking_socket() {
        unsafe {
            let mut fds = [0i32; 2];
            assert_eq!(
                morloc_runtime_types::fd::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()),
                0
            );
            let (send_fd, recv_fd) = (fds[0], fds[1]);

            // Shrink both buffers so a modest payload overflows the pipe and
            // forces EAGAIN, then make the sender non-blocking.
            let small: libc::c_int = 4096;
            libc::setsockopt(
                send_fd,
                libc::SOL_SOCKET,
                libc::SO_SNDBUF,
                &small as *const _ as *const c_void,
                std::mem::size_of::<libc::c_int>() as libc::socklen_t,
            );
            libc::setsockopt(
                recv_fd,
                libc::SOL_SOCKET,
                libc::SO_RCVBUF,
                &small as *const _ as *const c_void,
                std::mem::size_of::<libc::c_int>() as libc::socklen_t,
            );
            let flags = libc::fcntl(send_fd, libc::F_GETFL, 0);
            libc::fcntl(send_fd, libc::F_SETFL, flags | libc::O_NONBLOCK);

            const N: usize = 1 << 20; // 1 MiB, far exceeds the 4 KiB buffer
            let payload: Vec<u8> = (0..N).map(|i| (i & 0xff) as u8).collect();

            // Slow reader on another thread: drains in small chunks with brief
            // pauses so the sender's buffer genuinely fills (yields EAGAIN).
            let reader = std::thread::spawn(move || {
                let mut got = vec![0u8; N];
                let mut total = 0usize;
                while total < N {
                    let n = libc::recv(
                        recv_fd,
                        got.as_mut_ptr().add(total) as *mut c_void,
                        (N - total).min(1024),
                        0,
                    );
                    if n > 0 {
                        total += n as usize;
                    } else if n == 0 {
                        break;
                    }
                    std::thread::yield_now();
                }
                libc::close(recv_fd);
                (got, total)
            });

            let ok = send_all(send_fd, payload.as_ptr(), N);
            libc::close(send_fd);
            let (got, total) = reader.join().unwrap();

            assert!(ok, "send_all reported failure on a non-blocking socket");
            assert_eq!(total, N, "reader did not receive the whole payload");
            assert_eq!(got, payload, "payload corrupted in transit");
        }
    }

    // The accept loop must answer a readiness ping itself (echo the pong) so a
    // still-initializing worker never sees it, and must leave a non-ping request
    // fully intact (MSG_PEEK does not consume) for the worker to read.
    #[test]
    fn try_answer_ping_handles_ping_and_leaves_calls_intact() {
        unsafe {
            // -- ping: answered in-loop, pong echoed back --
            let mut fds = [0i32; 2];
            assert_eq!(
                morloc_runtime_types::fd::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()),
                0
            );
            let (peer, server) = (fds[0], fds[1]);
            let ping = crate::packet_ffi::make_ping_packet();
            assert!(!ping.is_null());
            assert_eq!(send_all(peer, ping, 32), true);

            assert!(
                matches!(try_answer_ping(server), PingPeek::Answered),
                "ping was not answered by the accept loop"
            );

            let mut pong = [0u8; 32];
            let n = libc::recv(peer, pong.as_mut_ptr() as *mut c_void, 32, 0);
            assert_eq!(n, 32, "no pong echoed back");
            let mut e: *mut c_char = ptr::null_mut();
            assert!(crate::packet_ffi::packet_is_ping(pong.as_ptr(), &mut e));
            if !e.is_null() { libc::free(e as *mut c_void); }
            libc::free(ping as *mut c_void);
            libc::close(peer);

            // -- non-ping: not consumed, left for the worker --
            let mut fds2 = [0i32; 2];
            assert_eq!(
                morloc_runtime_types::fd::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds2.as_mut_ptr()),
                0
            );
            let (peer2, server2) = (fds2[0], fds2[1]);
            // 32 bytes that are not a valid morloc ping header.
            let junk = [0x7fu8; 32];
            assert_eq!(send_all(peer2, junk.as_ptr(), 32), true);

            assert!(
                matches!(try_answer_ping(server2), PingPeek::NotPing),
                "non-ping request was wrongly treated as a ping"
            );
            // The bytes must still be readable (MSG_PEEK did not consume them).
            let mut got = [0u8; 32];
            let n2 = libc::recv(server2, got.as_mut_ptr() as *mut c_void, 32, 0);
            assert_eq!(n2, 32, "non-ping payload was consumed by try_answer_ping");
            assert_eq!(got, junk, "non-ping payload was altered");
            libc::close(peer2);
            libc::close(server2);
        }
    }

    // A malformed 32-byte "ping" (valid magic + PING command type but a nonzero
    // length) must NOT be answered from the accept loop: return_ping would read
    // 32+length bytes past the 32-byte peek buffer. It must be classified NotPing
    // and left intact for a worker to read the full packet off the socket.
    #[test]
    fn try_answer_ping_rejects_oversized_ping_header() {
        unsafe {
            let mut fds = [0i32; 2];
            assert_eq!(
                morloc_runtime_types::fd::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()),
                0
            );
            let (peer, server) = (fds[0], fds[1]);
            // Start from a genuine ping header, then forge a nonzero length so
            // morloc_packet_size(hdr) != 32.
            let ping = crate::packet_ffi::make_ping_packet();
            assert!(!ping.is_null());
            let mut hdr = [0u8; 32];
            std::ptr::copy_nonoverlapping(ping, hdr.as_mut_ptr(), 32);
            libc::free(ping as *mut c_void);
            // PacketHeader length is the last 4 bytes (see morloc_packet_size =
            // 32 + offset + length); set a large length to force the over-read.
            hdr[28] = 0x00; hdr[29] = 0x10; hdr[30] = 0x00; hdr[31] = 0x00;
            let mut e: *mut c_char = ptr::null_mut();
            // Only run the assertion if this still parses as a ping by type;
            // otherwise the guard is trivially satisfied via the is_ping path.
            let looks_ping = crate::packet_ffi::packet_is_ping(hdr.as_ptr(), &mut e);
            if !e.is_null() { libc::free(e as *mut c_void); e = ptr::null_mut(); }
            let sz = crate::packet_ffi::morloc_packet_size(hdr.as_ptr(), &mut e);
            if !e.is_null() { libc::free(e as *mut c_void); }
            assert_eq!(send_all(peer, hdr.as_ptr(), 32), true);
            let outcome = try_answer_ping(server);
            if looks_ping && sz != 32 {
                assert!(
                    matches!(outcome, PingPeek::NotPing),
                    "oversized ping header must be handed to a worker, not answered"
                );
            }
            libc::close(peer);
            libc::close(server);
        }
    }
}

#[cfg(test)]
mod dispatch_fork_tests {
    use super::*;

    // A pool's accept loop learns of a watched descriptor's end, such as
    // the nexus's lifeline, without a thread of its own blocked on it.
    #[test]
    fn an_accept_wait_reports_a_watched_descriptor_that_ended() {
        let (server, _path, dir) = listening_socket();
        let mut p = [0 as libc::c_int; 2];
        assert_eq!(unsafe { libc::pipe(p.as_mut_ptr()) }, 0);
        let quiet = unsafe { accept_or_watch(server, 20_000, &[p[0]]) }.unwrap();
        unsafe { libc::close(p[1]) };
        let deadline = std::time::Instant::now() + std::time::Duration::from_secs(5);
        let ended = loop {
            let got = unsafe { accept_or_watch(server, 1_000_000, &[p[0]]) }.unwrap();
            if !matches!(got, Accepted::Nothing) || std::time::Instant::now() > deadline {
                break got;
            }
        };
        unsafe {
            libc::close(p[0]);
            libc::close(server);
        }
        let _ = std::fs::remove_dir_all(&dir);
        assert!(matches!(quiet, Accepted::Nothing), "a live descriptor was reported");
        assert!(matches!(ended, Accepted::Watched(0)), "the ended descriptor was not reported");
    }

    fn listening_socket() -> (i32, std::ffi::CString, std::path::PathBuf) {
        let dir = std::env::temp_dir().join(format!("mlc-peer-{}-{:?}", std::process::id(), std::thread::current().id()));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("s");
        let _ = std::fs::remove_file(&path);
        let c = std::ffi::CString::new(path.to_str().unwrap()).unwrap();
        let fd = unsafe { morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0) };
        let mut addr: libc::sockaddr_un = unsafe { std::mem::zeroed() };
        addr.sun_family = libc::AF_UNIX as libc::sa_family_t;
        for (d, b) in addr.sun_path.iter_mut().zip(c.as_bytes()) {
            *d = *b as c_char;
        }
        let len = std::mem::size_of::<libc::sockaddr_un>() as libc::socklen_t;
        assert_eq!(unsafe { libc::bind(fd, &addr as *const _ as *const libc::sockaddr, len) }, 0);
        assert_eq!(unsafe { libc::listen(fd, 4) }, 0);
        (fd, c, dir)
    }

    // A child that connects `fd`, made by the caller, to `path`, then runs
    // `then`; only async-signal-safe calls follow the fork.
    unsafe fn connecting_child(path: &std::ffi::CStr, fd: i32, then: impl FnOnce(i32)) -> libc::pid_t {
        assert!(fd >= 0);
        let mut addr: libc::sockaddr_un = std::mem::zeroed();
        addr.sun_family = libc::AF_UNIX as libc::sa_family_t;
        for (d, b) in addr.sun_path.iter_mut().zip(path.to_bytes()) {
            *d = *b as c_char;
        }
        let pid = libc::fork();
        if pid == 0 {
            let len = std::mem::size_of::<libc::sockaddr_un>() as libc::socklen_t;
            if libc::connect(fd, &addr as *const _ as *const libc::sockaddr, len) != 0 {
                libc::_exit(2);
            }
            then(fd);
            libc::_exit(0);
        }
        assert!(pid > 0, "fork failed");
        pid
    }

    fn big_buffer() -> Vec<u8> {
        vec![7u8; 8 << 20]
    }

    #[test]
    fn a_reply_to_a_stopped_reader_waits_until_it_resumes() {
        let (listener, path, dir) = listening_socket();
        let client = unsafe { morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0) };
        let want = big_buffer().len();
        let child = unsafe {
            connecting_child(&path, client, |fd| {
                libc::raise(libc::SIGSTOP);
                let mut buf = vec![0u8; 1 << 16];
                let mut got = 0usize;
                loop {
                    let n = libc::read(fd, buf.as_mut_ptr() as *mut c_void, buf.len());
                    if n <= 0 {
                        break;
                    }
                    got += n as usize;
                }
                libc::_exit(if got == want { 0 } else { 3 });
            })
        };
        unsafe { libc::close(client) };
        let conn = unsafe { morloc_runtime_types::fd::accept(listener, ptr::null_mut(), ptr::null_mut()) };
        assert!(conn >= 0);
        unsafe { libc::fcntl(conn, libc::F_SETFL, libc::O_NONBLOCK) };
        let mut status = 0;
        assert_eq!(unsafe { libc::waitpid(child, &mut status, libc::WUNTRACED) }, child);
        let resumer = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(2500));
            unsafe { libc::kill(child, libc::SIGCONT) };
        });
        SEND_STALL_LIMIT.with(|l| l.set(Some(std::time::Duration::from_millis(1000))));
        let buf = big_buffer();
        let sent = unsafe { send_all(conn, buf.as_ptr(), buf.len()) };
        SEND_STALL_LIMIT.with(|l| l.set(None));
        unsafe { libc::close(conn) };
        resumer.join().unwrap();
        assert_eq!(unsafe { libc::waitpid(child, &mut status, 0) }, child);
        unsafe { libc::close(listener) };
        let _ = std::fs::remove_dir_all(dir);
        assert!(sent, "a stopped reader is alive, so the send waits for it");
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "the reader got every byte: {status}");
    }

    #[test]
    fn a_reply_to_a_reader_that_died_ends_though_its_fd_lives_on() {
        let (listener, path, dir) = listening_socket();
        let client = unsafe { morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0) };
        let mut pids = [0 as libc::c_int; 2];
        assert_eq!(unsafe { libc::pipe(pids.as_mut_ptr()) }, 0);
        let pid_w = pids[1];
        let child = unsafe {
            connecting_child(&path, client, move |_| {
                let grandchild = libc::fork();
                if grandchild == 0 {
                    libc::sleep(30);
                    libc::_exit(0);
                }
                libc::write(pid_w, &grandchild as *const libc::pid_t as *const c_void, std::mem::size_of::<libc::pid_t>());
            })
        };
        unsafe { libc::close(client) };
        let mut grandchild: libc::pid_t = 0;
        assert_eq!(
            unsafe { libc::read(pids[0], &mut grandchild as *mut libc::pid_t as *mut c_void, std::mem::size_of::<libc::pid_t>()) },
            std::mem::size_of::<libc::pid_t>() as isize
        );
        let mut status = 0;
        assert_eq!(unsafe { libc::waitpid(child, &mut status, 0) }, child);
        let conn = unsafe { morloc_runtime_types::fd::accept(listener, ptr::null_mut(), ptr::null_mut()) };
        assert!(conn >= 0);
        unsafe { libc::fcntl(conn, libc::F_SETFL, libc::O_NONBLOCK) };
        let started = std::time::Instant::now();
        let buf = big_buffer();
        let sent = unsafe { send_all(conn, buf.as_ptr(), buf.len()) };
        let took = started.elapsed();
        unsafe {
            libc::kill(grandchild, libc::SIGKILL);
            libc::waitpid(grandchild, ptr::null_mut(), 0);
            libc::close(conn);
            libc::close(listener);
            libc::close(pids[0]);
            libc::close(pids[1]);
        }
        let _ = std::fs::remove_dir_all(dir);
        assert!(!sent, "the reader is gone");
        assert!(took < std::time::Duration::from_secs(5), "noticed within the liveness check: {took:?}");
    }

    #[test]
    fn a_request_whose_header_arrives_in_pieces_is_read_whole() {
        let (listener, path, dir) = listening_socket();
        let client = unsafe { morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0) };
        let mut packet = crate::packet::PacketHeader::ping().to_bytes().to_vec();
        let payload = b"0123456789abcdef";
        let len = payload.len() as u64;
        packet[morloc_runtime_types::packet::PKT_HEADER_SIZE - 8..].copy_from_slice(&len.to_le_bytes());
        packet.extend_from_slice(payload);
        let sent = packet.clone();
        let child = unsafe {
            connecting_child(&path, client, move |fd| {
                libc::write(fd, sent.as_ptr() as *const c_void, 10);
                libc::usleep(200_000);
                libc::write(fd, sent[10..].as_ptr() as *const c_void, sent.len() - 10);
                libc::sleep(2);
            })
        };
        unsafe { libc::close(client) };
        let conn = unsafe { morloc_runtime_types::fd::accept(listener, ptr::null_mut(), ptr::null_mut()) };
        assert!(conn >= 0);
        let mut err: *mut c_char = ptr::null_mut();
        let got = unsafe { stream_from_client(conn, &mut err) };
        unsafe {
            libc::kill(child, libc::SIGKILL);
            libc::waitpid(child, ptr::null_mut(), 0);
            libc::close(conn);
            libc::close(listener);
        }
        let _ = std::fs::remove_dir_all(dir);
        assert!(err.is_null(), "{}", unsafe { CStr::from_ptr(err) }.to_string_lossy());
        let size = unsafe { crate::packet_ffi::morloc_packet_size(got, &mut err) };
        let bytes = unsafe { std::slice::from_raw_parts(got, size) }.to_vec();
        unsafe { libc::free(got as *mut c_void) };
        assert_eq!(bytes, packet);
    }

    #[test]
    fn a_caller_whose_callee_died_stops_waiting_though_the_fd_lives_on() {
        let dir = std::env::temp_dir().join(format!("mlc-reply-gone-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("pipe");
        let cpath = std::ffi::CString::new(path.to_str().unwrap()).unwrap();
        let mut ready = [0 as libc::c_int; 2];
        assert_eq!(unsafe { libc::pipe(ready.as_mut_ptr()) }, 0);
        let callee = unsafe { libc::fork() };
        if callee == 0 {
            let l = std::os::unix::net::UnixListener::bind(&path).unwrap();
            unsafe { libc::write(ready[1], b"r".as_ptr() as *const c_void, 1) };
            let (conn, _) = l.accept().unwrap();
            let mut header = [0u8; morloc_runtime_types::packet::PKT_HEADER_SIZE];
            let _ = std::io::Read::read_exact(&mut &conn, &mut header);
            if unsafe { libc::fork() } == 0 {
                unsafe { libc::sleep(30) };
                unsafe { libc::_exit(0) };
            }
            unsafe { libc::_exit(0) };
        }
        let mut b = 0u8;
        assert_eq!(unsafe { libc::read(ready[0], &mut b as *mut u8 as *mut c_void, 1) }, 1);
        let reaper = std::thread::spawn(move || unsafe { libc::waitpid(callee, ptr::null_mut(), 0) });
        let req = crate::packet::PacketHeader::ping().to_bytes();
        let started = std::time::Instant::now();
        let mut err: *mut c_char = ptr::null_mut();
        let reply = unsafe { send_and_receive_over_socket(cpath.as_ptr(), req.as_ptr(), &mut err) };
        let took = started.elapsed();
        reaper.join().unwrap();
        let _ = std::fs::remove_dir_all(&dir);
        unsafe {
            libc::close(ready[0]);
            libc::close(ready[1]);
        }
        assert!(reply.is_null(), "no reply came");
        unsafe { libc::free(err as *mut c_void) };
        assert!(took < std::time::Duration::from_secs(5), "the dead callee was noticed: {took:?}");
    }

    #[test]
    fn a_stopped_sender_keeps_its_request() {
        let (listener, path, dir) = listening_socket();
        let ping = crate::packet::PacketHeader::ping().to_bytes();
        let client = unsafe { morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0) };
        let child = unsafe {
            connecting_child(&path, client, |fd| {
                libc::raise(libc::SIGSTOP);
                libc::write(fd, ping.as_ptr() as *const c_void, ping.len());
            })
        };
        unsafe { libc::close(client) };
        let conn = unsafe { morloc_runtime_types::fd::accept(listener, ptr::null_mut(), ptr::null_mut()) };
        assert!(conn >= 0);
        let resumer = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(2500));
            unsafe { libc::kill(child, libc::SIGCONT) };
        });
        let mut err: *mut c_char = ptr::null_mut();
        let request = unsafe { stream_from_client(conn, &mut err) };
        resumer.join().unwrap();
        let mut status = 0;
        unsafe { libc::waitpid(child, &mut status, 0) };
        assert!(err.is_null(), "{}", unsafe { CStr::from_ptr(err) }.to_string_lossy());
        assert!(!request.is_null());
        unsafe {
            libc::free(request as *mut c_void);
            libc::close(conn);
            libc::close(listener);
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    #[test]
    fn a_sender_counts_as_alive_until_it_exits_in_either_identity() {
        let (listener, path, dir) = listening_socket();
        let client = unsafe { morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0) };
        let child = unsafe {
            connecting_child(&path, client, |_| {
                libc::raise(libc::SIGSTOP);
            })
        };
        let conn = unsafe { morloc_runtime_types::fd::accept(listener, ptr::null_mut(), ptr::null_mut()) };
        assert!(conn >= 0);
        let mut status = 0;
        assert_eq!(unsafe { libc::waitpid(child, &mut status, libc::WUNTRACED) }, child);
        assert!(libc::WIFSTOPPED(status));
        let stamped = Peer::stamped(conn).unwrap();
        let deferred = Peer::deferred(conn);
        assert!(stamped.alive(), "a stopped sender is alive");
        if let Some(d) = &deferred {
            assert!(d.alive(), "a stopped sender is alive");
        }
        unsafe { libc::kill(child, libc::SIGKILL) };
        let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
        assert_eq!(unsafe { libc::waitid(libc::P_PID, child as libc::id_t, &mut info, libc::WEXITED | libc::WNOWAIT) }, 0);
        assert!(!stamped.alive(), "an unreaped sender is gone");
        if let Some(d) = &deferred {
            assert!(!d.alive(), "an unreaped sender is gone");
        }
        assert_eq!(unsafe { libc::waitpid(child, &mut status, 0) }, child);
        assert!(!stamped.alive(), "a reaped sender is gone");
        if let Some(d) = &deferred {
            assert!(!d.alive(), "a reaped sender is gone");
        }
        unsafe {
            libc::close(client);
            libc::close(conn);
            libc::close(listener);
        }
        let _ = std::fs::remove_dir_all(dir);
    }

    #[test]
    fn a_request_whose_sender_is_gone_ends_though_its_fd_lives_on() {
        let (listener, path, dir) = listening_socket();
        let client = unsafe { morloc_runtime_types::fd::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0) };
        let child = unsafe { connecting_child(&path, client, |_| {}) };
        let mut status = 0;
        assert_eq!(unsafe { libc::waitpid(child, &mut status, 0) }, child);
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "the child could not connect: {status}");
        let conn = unsafe { morloc_runtime_types::fd::accept(listener, ptr::null_mut(), ptr::null_mut()) };
        assert!(conn >= 0);
        let (done, finished) = std::sync::mpsc::channel::<()>();
        let watchdog = std::thread::spawn(move || {
            if finished.recv_timeout(std::time::Duration::from_secs(10)).is_err() {
                unsafe { libc::shutdown(client, libc::SHUT_WR) };
            }
        });
        let began = std::time::Instant::now();
        let mut err: *mut c_char = ptr::null_mut();
        let request = unsafe { stream_from_client(conn, &mut err) };
        let waited = began.elapsed();
        let _ = done.send(());
        watchdog.join().unwrap();
        unsafe {
            libc::close(client);
            libc::close(conn);
            libc::close(listener);
        }
        let _ = std::fs::remove_dir_all(dir);
        assert!(request.is_null());
        assert!(!err.is_null());
        unsafe { libc::free(err as *mut c_void) };
        assert!(waited < std::time::Duration::from_secs(5), "waited {waited:?}");
    }

    fn drain(fd: i32) -> Vec<u8> {
        let mut out = Vec::new();
        let mut buf = [0u8; 4096];
        loop {
            let mut pfd = libc::pollfd { fd, events: libc::POLLIN, revents: 0 };
            if unsafe { libc::poll(&mut pfd, 1, 200) } <= 0 {
                return out;
            }
            let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut c_void, buf.len()) };
            if n <= 0 {
                return out;
            }
            out.extend_from_slice(&buf[..n as usize]);
        }
    }

    unsafe fn send(fd: i32, msg: &std::ffi::CStr) {
        let mut err: *mut c_char = ptr::null_mut();
        let packet = crate::packet_ffi::make_fail_packet(msg.as_ptr());
        send_reply_to_foreign_server(fd, packet, None, &mut err);
        libc::free(packet as *mut c_void);
    }

    #[test]
    fn a_child_forked_during_a_dispatch_exits_instead_of_replying() {
        let mut sv = [0 as libc::c_int; 2];
        assert_eq!(unsafe { morloc_runtime_types::fd::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, sv.as_mut_ptr()) }, 0);
        let (server, peer) = (sv[0], sv[1]);
        unsafe { send(peer, c"request") };
        let mut err: *mut c_char = ptr::null_mut();
        let request = unsafe { stream_from_client(server, &mut err) };
        assert!(!request.is_null());
        unsafe { libc::free(request as *mut c_void) };
        let child = unsafe { libc::fork() };
        assert!(child >= 0);
        if child == 0 {
            unsafe { libc::alarm(5) };
            unsafe { send(server, c"from-the-child") };
            unsafe { libc::_exit(0) };
        }
        let mut status = 0;
        unsafe { libc::waitpid(child, &mut status, 0) };
        unsafe { send(server, c"from-the-parent") };
        unsafe { libc::close(server) };
        let got = drain(peer);
        unsafe { libc::close(peer) };
        let has = |needle: &[u8]| got.windows(needle.len()).any(|w| w == needle);
        assert!(!has(b"from-the-child"), "a child forked during a dispatch replied to its parent's request");
        assert!(has(b"from-the-parent"), "the parent's reply was lost");
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 1, "child status {status}");
    }
}

#[cfg(test)]
mod reply_tests {
    //! A pool's release of what a dispatch holds must be done before the
    //! caller can see the reply: a caller that already has its value can ask
    //! the same pool for the next one at once, and if that request is
    //! answered while the previous value is still held, both are in memory
    //! together.
    use super::*;
    use crate::cschema::CSchema;
    use crate::shm::AbsPtr;
    use std::cell::Cell;

    thread_local! {
        static HELD: Cell<AbsPtr> = const { Cell::new(ptr::null_mut()) };
        static PEER: Cell<i32> = const { Cell::new(-1) };
        static SEEN_COUNT: Cell<Option<u32>> = const { Cell::new(None) };
        static PEER_HAD_DATA: Cell<Option<bool>> = const { Cell::new(None) };
    }

    unsafe extern "C" fn release_dispatch() {
        let abs = HELD.with(|h| h.get());
        SEEN_COUNT.with(|c| c.set(crate::shm::reference_count(abs)));
        let peer = PEER.with(|p| p.get());
        if peer >= 0 {
            let mut pfd = libc::pollfd { fd: peer, events: libc::POLLIN, revents: 0 };
            PEER_HAD_DATA.with(|d| d.set(Some(libc::poll(&mut pfd, 1, 0) > 0)));
        }
        crate::shm::shfree(abs).expect("dispatch release");
    }

    unsafe fn rptr_packet(abs: AbsPtr) -> (*mut u8, *mut CSchema) {
        let rel = crate::shm::abs2rel(abs).expect("relptr");
        let schema = crate::schema::Schema::primitive(crate::schema::SerialType::Uint8);
        let cs = CSchema::from_rust(&schema);
        let packet = crate::packet_ffi::make_standard_data_packet(rel, cs);
        assert!(!packet.is_null(), "expected an RPTR packet");
        (packet, cs)
    }

    #[test]
    fn the_dispatch_lets_go_before_the_caller_sees_the_reply() {
        let _shm = crate::own_test_registry();
        unsafe {
            let abs = crate::shm::shmalloc(64).expect("allocate");
            let (packet, cs) = rptr_packet(abs);
            let mut fds = [0i32; 2];
            assert_eq!(morloc_runtime_types::fd::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()), 0);
            HELD.with(|h| h.set(abs));
            PEER.with(|p| p.set(fds[1]));

            let mut err: *mut c_char = ptr::null_mut();
            let sent = send_reply_to_foreign_server(fds[0], packet, Some(release_dispatch), &mut err);
            assert!(err.is_null() && sent > 0, "the reply was not sent");

            assert_eq!(SEEN_COUNT.with(|c| c.get()), Some(2),
                "the release ran before the reply held the caller's reference");
            assert_eq!(PEER_HAD_DATA.with(|d| d.get()), Some(false),
                "the caller could see the reply before the dispatch let go");
            assert_eq!(crate::shm::reference_count(abs), Some(1),
                "the caller should hold the only reference");

            crate::shm::shfree(abs).expect("caller release");
            libc::close(fds[0]);
            libc::close(fds[1]);
            libc::free(packet as *mut c_void);
            CSchema::free(cs);
        }
    }

    #[test]
    fn a_reply_that_reaches_nobody_holds_nothing() {
        let _shm = crate::own_test_registry();
        unsafe {
            let abs = crate::shm::shmalloc(64).expect("allocate");
            let (packet, cs) = rptr_packet(abs);
            let mut fds = [0i32; 2];
            assert_eq!(morloc_runtime_types::fd::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()), 0);
            libc::close(fds[1]);
            crate::utility::set_nosigpipe(fds[0]);
            HELD.with(|h| h.set(abs));
            PEER.with(|p| p.set(-1));

            let mut err: *mut c_char = ptr::null_mut();
            let sent = send_reply_to_foreign_server(fds[0], packet, Some(release_dispatch), &mut err);
            assert_eq!(sent, 0);
            assert!(!err.is_null(), "a failed send must say so");
            libc::free(err as *mut c_void);
            assert_eq!(SEEN_COUNT.with(|c| c.get()), Some(2), "the release did not run");
            assert_eq!(crate::shm::reference_count(abs), Some(0),
                "a reply that reached nobody left a reference behind");

            libc::close(fds[0]);
            libc::free(packet as *mut c_void);
            CSchema::free(cs);
        }
    }
}

mod c_abi {
    use super::*;

    #[no_mangle]
    pub unsafe extern "C" fn close_socket(socket_id: i32) {
        super::close_socket(socket_id)
    }

    #[no_mangle]
    pub unsafe extern "C" fn close_daemon(daemon_ptr: *mut *mut LanguageDaemon) {
        super::close_daemon(daemon_ptr)
    }

    #[no_mangle]
    pub unsafe extern "C" fn start_daemon(socket_path: *const c_char, tmpdir: *const c_char, shm_basename: *const c_char, shm_default_size: usize, errmsg: *mut *mut c_char) -> *mut LanguageDaemon {
        super::start_daemon(socket_path, tmpdir, shm_basename, shm_default_size, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn stream_from_client(client_fd: i32, errmsg: *mut *mut c_char) -> *mut u8 {
        super::stream_from_client(client_fd, errmsg)
    }

    #[no_mangle]
    pub extern "C" fn morloc_exit_if_forked() {
        super::morloc_exit_if_forked()
    }

    #[no_mangle]
    pub unsafe extern "C" fn mlc_set_self_socket(socket_path: *const c_char) {
        super::mlc_set_self_socket(socket_path)
    }

    #[no_mangle]
    pub unsafe extern "C" fn send_and_receive_over_socket(socket_path: *const c_char, packet: *const u8, errmsg: *mut *mut c_char) -> *mut u8 {
        super::send_and_receive_over_socket(socket_path, packet, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn mlc_spawn(socket_path: *const c_char, mid: u32, args: *const *const u8, nargs: usize, handle: i64, errmsg: *mut *mut c_char) -> bool {
        super::mlc_spawn(socket_path, mid, args, nargs, handle, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn mlc_spawn_watched(socket_path: *const c_char, mid: u32, args: *const *const u8, nargs: usize, handle: i64, errmsg: *mut *mut c_char) -> bool {
        super::mlc_spawn_watched(socket_path, mid, args, nargs, handle, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn send_packet_to_foreign_server(client_fd: i32, packet: *mut u8, errmsg: *mut *mut c_char) -> usize {
        super::send_packet_to_foreign_server(client_fd, packet, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn send_reply_to_foreign_server(client_fd: i32, packet: *mut u8, release: Option<unsafe extern "C" fn()>, errmsg: *mut *mut c_char) -> usize {
        super::send_reply_to_foreign_server(client_fd, packet, release, errmsg)
    }

    #[no_mangle]
    pub unsafe extern "C" fn wait_for_client_with_timeout(daemon: *mut LanguageDaemon, timeout_us: i32, errmsg: *mut *mut c_char) -> i32 {
        super::wait_for_client_with_timeout(daemon, timeout_us, errmsg)
    }
}

#[cfg(test)]
mod reuse_tests {
    use super::*;
    use morloc_runtime_types::packet::{PacketHeader, MODE_KEEP, PKT_HEADER_SIZE, PKT_MODE_OFF};
    use std::io::{Read, Write};
    use std::os::unix::net::{UnixListener, UnixStream};
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::{Arc, Mutex};

    #[derive(Clone, Copy, PartialEq)]
    enum Callee {
        Keep,
        Close,
        DieAfterRead,
    }

    struct TestCallee {
        path: std::ffi::CString,
        dir: std::path::PathBuf,
        accepts: Arc<AtomicUsize>,
        runs: Arc<AtomicUsize>,
        eofs: Arc<AtomicUsize>,
        conns: Arc<Mutex<Vec<UnixStream>>>,
    }

    fn mode_of(header: &[u8]) -> u16 {
        u16::from_le_bytes([header[PKT_MODE_OFF], header[PKT_MODE_OFF + 1]])
    }

    fn callee(behaviour: Callee) -> TestCallee {
        let dir = std::env::temp_dir().join(format!("mlc-reuse-{}-{:?}", std::process::id(), std::thread::current().id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let sock = dir.join("pipe");
        let listener = UnixListener::bind(&sock).unwrap();
        let t = TestCallee {
            path: std::ffi::CString::new(sock.to_str().unwrap()).unwrap(),
            dir,
            accepts: Arc::new(AtomicUsize::new(0)),
            runs: Arc::new(AtomicUsize::new(0)),
            eofs: Arc::new(AtomicUsize::new(0)),
            conns: Arc::new(Mutex::new(Vec::new())),
        };
        let (accepts, runs, eofs, conns) = (t.accepts.clone(), t.runs.clone(), t.eofs.clone(), t.conns.clone());
        std::thread::spawn(move || {
            for conn in listener.incoming() {
                let Ok(mut conn) = conn else { return };
                accepts.fetch_add(1, Ordering::SeqCst);
                conns.lock().unwrap().push(conn.try_clone().unwrap());
                let (runs, eofs) = (runs.clone(), eofs.clone());
                std::thread::spawn(move || loop {
                    let mut header = [0u8; PKT_HEADER_SIZE];
                    if conn.read_exact(&mut header).is_err() {
                        eofs.fetch_add(1, Ordering::SeqCst);
                        return;
                    }
                    runs.fetch_add(1, Ordering::SeqCst);
                    if behaviour == Callee::DieAfterRead {
                        let _ = conn.shutdown(std::net::Shutdown::Both);
                        return;
                    }
                    let keep = behaviour == Callee::Keep && mode_of(&header) & MODE_KEEP != 0;
                    let mut reply = PacketHeader::ping().to_bytes();
                    if keep {
                        reply[PKT_MODE_OFF..PKT_MODE_OFF + 2].copy_from_slice(&MODE_KEEP.to_le_bytes());
                    }
                    if conn.write_all(&reply).is_err() || !keep {
                        return;
                    }
                });
            }
        });
        t
    }

    fn call(t: &TestCallee) -> Result<u16, String> {
        let req = PacketHeader::ping().to_bytes();
        let mut err: *mut c_char = ptr::null_mut();
        let reply = unsafe { send_and_receive_over_socket(t.path.as_ptr(), req.as_ptr(), &mut err) };
        if reply.is_null() {
            let m = unsafe { CStr::from_ptr(err) }.to_string_lossy().into_owned();
            unsafe { libc::free(err as *mut c_void) };
            return Err(m);
        }
        let mode = mode_of(unsafe { std::slice::from_raw_parts(reply, PKT_HEADER_SIZE) });
        unsafe { libc::free(reply as *mut c_void) };
        Ok(mode)
    }

    fn eventually(what: impl Fn() -> bool) -> bool {
        for _ in 0..100 {
            if what() {
                return true;
            }
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        what()
    }

    #[test]
    fn a_kept_connection_carries_the_next_calls() {
        let t = callee(Callee::Keep);
        for _ in 0..3 {
            assert_eq!(call(&t), Ok(0), "the keep bit never reaches the caller's caller");
        }
        assert_eq!(t.accepts.load(Ordering::SeqCst), 1);
        assert_eq!(t.runs.load(Ordering::SeqCst), 3);
        assert_eq!(cached_connections(), 1);
        drop_cached_connections();
        assert!(eventually(|| t.eofs.load(Ordering::SeqCst) == 1), "closing the cache ends the callee's wait");
        let _ = std::fs::remove_dir_all(&t.dir);
    }

    #[test]
    fn a_callee_that_does_not_keep_gets_a_connection_per_call() {
        let t = callee(Callee::Close);
        for _ in 0..3 {
            assert_eq!(call(&t), Ok(0));
        }
        assert_eq!(t.accepts.load(Ordering::SeqCst), 3);
        assert_eq!(cached_connections(), 0);
        let _ = std::fs::remove_dir_all(&t.dir);
    }

    #[test]
    fn a_kept_connection_whose_callee_is_gone_is_retried_once_on_a_new_one() {
        let t = callee(Callee::Keep);
        assert_eq!(call(&t), Ok(0));
        for c in t.conns.lock().unwrap().drain(..) {
            c.shutdown(std::net::Shutdown::Both).unwrap();
        }
        assert!(eventually(|| t.eofs.load(Ordering::SeqCst) == 1));
        assert_eq!(call(&t), Ok(0));
        assert_eq!(t.accepts.load(Ordering::SeqCst), 2);
        assert_eq!(t.runs.load(Ordering::SeqCst), 2, "the request reached the callee once");
        drop_cached_connections();
        let _ = std::fs::remove_dir_all(&t.dir);
    }

    #[test]
    fn a_call_whose_callee_dies_after_reading_it_fails_without_a_retry() {
        let t = callee(Callee::DieAfterRead);
        assert!(call(&t).is_err());
        assert_eq!(t.runs.load(Ordering::SeqCst), 1, "the call ran once");
        assert_eq!(t.accepts.load(Ordering::SeqCst), 1, "and was not sent again");
        assert_eq!(cached_connections(), 0);
        let _ = std::fs::remove_dir_all(&t.dir);
    }

    #[test]
    fn a_forked_child_does_not_hold_its_parents_connections_open() {
        let t = callee(Callee::Keep);
        assert_eq!(call(&t), Ok(0));
        let child = unsafe { libc::fork() };
        assert!(child >= 0);
        if child == 0 {
            unsafe {
                libc::sleep(3);
                libc::_exit(0);
            }
        }
        drop_cached_connections();
        let freed = eventually(|| t.eofs.load(Ordering::SeqCst) == 1);
        unsafe {
            libc::kill(child, libc::SIGKILL);
            libc::waitpid(child, ptr::null_mut(), 0);
        }
        assert!(freed, "the callee saw the connection end while the child lived");
        let _ = std::fs::remove_dir_all(&t.dir);
    }

    #[test]
    fn the_keep_bit_is_cleared_from_a_reply() {
        let mut p = PacketHeader::ping().to_bytes();
        p[PKT_MODE_OFF..PKT_MODE_OFF + 2].copy_from_slice(&MODE_KEEP.to_le_bytes());
        assert!(unsafe { take_keep_bit(p.as_mut_ptr()) });
        assert_eq!(mode_of(&p), 0);
        assert!(!unsafe { take_keep_bit(p.as_mut_ptr()) });
    }
}

#[cfg(test)]
mod endpoint_tests {
    use super::*;
    use std::os::unix::net::UnixListener;

    #[test]
    fn a_pool_removes_only_the_socket_file_it_bound() {
        let dir = std::env::temp_dir().join(format!("mlc-endpoint-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("pipe");
        let c = std::ffi::CString::new(path.to_str().unwrap()).unwrap();

        let ours = UnixListener::bind(&path).unwrap();
        let (hold, id) = hold_socket_file(c.as_ptr());
        drop(ours);
        std::fs::remove_file(&path).unwrap();
        let theirs = UnixListener::bind(&path).unwrap();
        unsafe { unlink_if_ours(c.as_ptr(), id.unwrap()) };
        assert!(path.exists(), "a file another pool bound at the path stays");
        if hold >= 0 {
            unsafe { libc::close(hold) };
        }

        let (hold, id) = hold_socket_file(c.as_ptr());
        unsafe { unlink_if_ours(c.as_ptr(), id.unwrap()) };
        assert!(!path.exists(), "the file this pool bound is removed");
        if hold >= 0 {
            unsafe { libc::close(hold) };
        }
        drop(theirs);
        let _ = std::fs::remove_dir_all(&dir);
    }
}
