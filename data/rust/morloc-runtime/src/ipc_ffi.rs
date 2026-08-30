//! C ABI wrappers for IPC functions.
//! Replaces ipc.c with Rust implementations of Unix domain socket operations.

use std::ffi::{c_char, c_void, CStr};
use std::ptr;

use crate::error::{clear_errmsg, set_errmsg, MorlocError};

// ── C types from call.h ──────────────────────────────────────────────────────

#[repr(C)]
pub struct ClientList {
    pub fd: i32,
    pub next: *mut ClientList,
}

// Mirrors C `language_daemon_t` in morloc.h; layout must stay in sync.
#[repr(C)]
pub struct LanguageDaemon {
    pub socket_path: *mut c_char,
    pub tmpdir: *mut c_char,
    pub shm_basename: *mut c_char,
    pub shm: *mut crate::shm::ShmHeader,
    pub shm_default_size: usize,
    pub server_fd: i32,
    pub client_fds: *mut ClientList,
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

// Ceiling on a single zero-progress wait for socket writability in send_all.
// A reader that drains at all wakes the POLLOUT poll immediately and resets
// this window (it bounds only a fully-stalled peer), so a large legitimate send
// to a slow-but-progressing reader is never cut off; a peer that accepts zero
// bytes for this long is treated as hung and the send fails instead of blocking
// the caller (and, for a pool worker, its thread) forever.
const SEND_STALL_TIMEOUT_MS: i64 = 120_000;

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
    let stall = libc::timespec {
        tv_sec: SEND_STALL_TIMEOUT_MS / 1000,
        tv_nsec: (SEND_STALL_TIMEOUT_MS % 1000) * 1_000_000,
    };
    let stall_dur = std::time::Duration::from_millis(SEND_STALL_TIMEOUT_MS as u64);
    // Wall-clock deadline for a zero-progress stall, reset on every byte sent.
    // Bounding on elapsed time (not just per-poll timeout) prevents a signal
    // storm -- e.g. SIGCHLD from exiting pool workers -- from turning
    // send->EAGAIN->poll->EINTR->send into an unbounded busy-loop that never
    // trips the per-poll ceiling.
    let mut last_progress = std::time::Instant::now();
    let mut total: usize = 0;
    while total < len {
        let n = libc::send(
            fd,
            buf.add(total) as *const c_void,
            len - total,
            crate::utility::SEND_NOSIGNAL,
        );
        if n > 0 {
            total += n as usize;
            last_progress = std::time::Instant::now();
            continue;
        }
        if n == 0 {
            return false; // peer closed; nothing more can be sent
        }
        let e = crate::utility::errno_val();
        if e == libc::EINTR {
            continue;
        }
        if e == libc::EAGAIN || e == libc::EWOULDBLOCK {
            // Send buffer full on a non-blocking socket. Bail if the peer has
            // accepted zero bytes for the whole stall window (checked on
            // wall-clock so an EINTR-interrupted poll can't reset it), else wait
            // (bounded) for writability and retry the same offset. A dead peer
            // surfaces as POLLHUP/POLLERR -> the next send returns EPIPE and we
            // bail on the hard-error path below.
            if last_progress.elapsed() >= stall_dur {
                return false;
            }
            let mut pfd = libc::pollfd { fd, events: libc::POLLOUT, revents: 0 };
            let _ = poll_wait(&mut pfd, 1, &stall); // EINTR just re-loops (deadline still bounds it)
            continue;
        }
        return false; // EPIPE / ECONNRESET / other hard error
    }
    true
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
    use std::sync::OnceLock;
    static T: OnceLock<bool> = OnceLock::new();
    *T.get_or_init(|| {
        std::env::var("MORLOC_TRACE_CLOSE").map(|v| v == "1").unwrap_or(false)
    })
}

#[no_mangle]
pub unsafe extern "C" fn close_socket(socket_id: i32) {
    if socket_id >= 0 {
        if trace_close_enabled() {
            eprintln!(
                "[MLC_CLOSE] pid={} fd={}\n{}",
                libc::getpid(),
                socket_id,
                std::backtrace::Backtrace::force_capture()
            );
        }
        libc::close(socket_id);
    }
}

#[no_mangle]
pub unsafe extern "C" fn close_daemon(daemon_ptr: *mut *mut LanguageDaemon) {
    if daemon_ptr.is_null() || (*daemon_ptr).is_null() {
        return;
    }
    let daemon = *daemon_ptr;

    close_socket((*daemon).server_fd);

    // Free client list
    let mut current = (*daemon).client_fds;
    while !current.is_null() {
        let next = (*current).next;
        libc::close((*current).fd);
        libc::free(current as *mut c_void);
        current = next;
    }

    if !(*daemon).socket_path.is_null() {
        libc::unlink((*daemon).socket_path);
        libc::free((*daemon).socket_path as *mut c_void);
    }
    if !(*daemon).tmpdir.is_null() {
        libc::free((*daemon).tmpdir as *mut c_void);
    }
    if !(*daemon).shm_basename.is_null() {
        libc::free((*daemon).shm_basename as *mut c_void);
    }

    // Unlink SHM segments owned by this process.
    // Safe to call even if another process already unlinked (ENOENT is ignored).
    let _ = crate::shm::shclose();

    libc::free(daemon as *mut c_void);
    *daemon_ptr = ptr::null_mut();
}

// ── Socket helpers ───────────────────────────────────────────────────────────

unsafe fn new_socket(errmsg: *mut *mut c_char) -> i32 {
    clear_errmsg(errmsg);
    let fd = libc::socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
    if fd < 0 {
        set_errmsg(errmsg, &MorlocError::Ipc("Error creating socket".into()));
        return -1;
    }
    crate::utility::set_nosigpipe(fd);
    fd
}

unsafe fn new_server_addr(socket_path: *const c_char) -> libc::sockaddr_un {
    let mut addr: libc::sockaddr_un = std::mem::zeroed();
    addr.sun_family = libc::AF_UNIX as libc::sa_family_t;
    let path_bytes = CStr::from_ptr(socket_path).to_bytes();
    let copy_len = path_bytes.len().min(addr.sun_path.len() - 1);
    ptr::copy_nonoverlapping(
        path_bytes.as_ptr() as *const c_char,
        addr.sun_path.as_mut_ptr(),
        copy_len,
    );
    addr
}

unsafe fn new_server(socket_path: *const c_char, errmsg: *mut *mut c_char) -> i32 {
    let server_fd = new_socket(errmsg);
    if server_fd < 0 {
        return -1;
    }

    let addr = new_server_addr(socket_path);

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

#[no_mangle]
pub unsafe extern "C" fn start_daemon(
    socket_path: *const c_char,
    tmpdir: *const c_char,
    shm_basename: *const c_char,
    shm_default_size: usize,
    errmsg: *mut *mut c_char,
) -> *mut LanguageDaemon {
    clear_errmsg(errmsg);

    crate::utility::raise_nofile_limit();

    let daemon = libc::calloc(1, std::mem::size_of::<LanguageDaemon>()) as *mut LanguageDaemon;
    if daemon.is_null() {
        set_errmsg(errmsg, &MorlocError::Ipc("Calloc for language_daemon_t failed".into()));
        return ptr::null_mut();
    }

    (*daemon).server_fd = -1;
    (*daemon).socket_path = libc::strdup(socket_path);
    (*daemon).tmpdir = libc::strdup(tmpdir);
    (*daemon).shm_basename = libc::strdup(shm_basename);

    if (*daemon).socket_path.is_null() || (*daemon).tmpdir.is_null() || (*daemon).shm_basename.is_null() {
        close_daemon(&mut (daemon as *mut LanguageDaemon));
        set_errmsg(errmsg, &MorlocError::Ipc("strdup failed in start_daemon".into()));
        return ptr::null_mut();
    }

    (*daemon).shm_default_size = shm_default_size;
    (*daemon).client_fds = ptr::null_mut();

    // Set fallback dir for file-backed SHM
    crate::shm::shm_set_fallback_dir(&CStr::from_ptr(tmpdir).to_string_lossy());

    // Init shared memory
    let mut err: *mut c_char = ptr::null_mut();
    let shm = crate::ffi::shinit(shm_basename, 0, shm_default_size, &mut err);
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

// ── stream_from_client_wait ──────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn stream_from_client_wait(
    client_fd: i32,
    poll_timeout_us: i32,
    recv_timeout_us: i32,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    if libc::fcntl(client_fd, libc::F_GETFD) == -1 {
        set_errmsg(errmsg, &MorlocError::Ipc("Invalid file descriptor".into()));
        return ptr::null_mut();
    }

    let buffer = libc::calloc(BUFFER_SIZE, 1) as *mut u8;
    if buffer.is_null() {
        set_errmsg(errmsg, &MorlocError::Ipc("calloc failed for buffer".into()));
        return ptr::null_mut();
    }

    // poll() instead of pselect/FD_SET: an fd_set can only hold descriptors
    // below FD_SETSIZE (1024), and FD_SET on a higher fd is out-of-bounds. A
    // pollfd imposes no ceiling on the fd value.
    let mut pfd = libc::pollfd { fd: client_fd, events: libc::POLLIN, revents: 0 };

    // Timeout setup
    let mut ts_loop: libc::timespec = std::mem::zeroed();
    let timeout_ptr = if poll_timeout_us > 0 {
        ts_loop.tv_sec = (poll_timeout_us / 1000000) as i64;
        ts_loop.tv_nsec = ((poll_timeout_us % 1000000) * 1000) as i64;
        &ts_loop as *const libc::timespec
    } else {
        ptr::null()
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
    let recv_length = loop {
        let mut ready;
        loop {
            ready = poll_wait(&mut pfd, 1, timeout_ptr);
            if !(ready < 0 && crate::utility::errno_val() == libc::EINTR) {
                break;
            }
        }

        if ready == 0 {
            libc::free(buffer as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Ipc("Timeout waiting for initial data".into()));
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

        let n = libc::recv(client_fd, buffer as *mut c_void, BUFFER_SIZE, 0);
        if n == 0 {
            if trace_close_enabled() {
                eprintln!("[MLC_IPC] pid={} recv EOF (0 bytes) fd={}", libc::getpid(), client_fd);
            }
            libc::free(buffer as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Ipc("Connection closed by peer".into()));
            return ptr::null_mut();
        }
        if n > 0 {
            break n;
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

    let attempts = 10;
    while (data_ptr as usize - result as usize) < packet_length {
        let mut packet_received = false;
        for attempt in 0..attempts {
            let recv_timeout_ptr = if recv_timeout_us > 0 {
                let total_us = recv_timeout_us as i64 * (attempt as i64 + 1);
                ts_loop.tv_sec = total_us / 1000000;
                ts_loop.tv_nsec = (total_us % 1000000) * 1000;
                &ts_loop as *const libc::timespec
            } else {
                ptr::null()
            };

            let ready = poll_wait(&mut pfd, 1, recv_timeout_ptr);

            if ready == 0 {
                libc::free(result as *mut c_void);
                set_errmsg(errmsg, &MorlocError::Ipc("Timeout waiting for remaining data".into()));
                return ptr::null_mut();
            }
            if ready < 0 && crate::utility::errno_val() != libc::EINTR {
                libc::free(result as *mut c_void);
                set_errmsg(errmsg, &MorlocError::Ipc("poll error".into()));
                return ptr::null_mut();
            }
            if ready <= 0 { continue; }

            if pfd_ready(&pfd) {
                let remaining = packet_length - (data_ptr as usize - result as usize);
                let recv_size = remaining.min(BUFFER_SIZE);
                let n = libc::recv(client_fd, data_ptr as *mut c_void, recv_size, 0);
                if n > 0 {
                    data_ptr = data_ptr.add(n as usize);
                    packet_received = true;
                    break;
                }
                if n == 0 {
                    libc::free(result as *mut c_void);
                    set_errmsg(errmsg, &MorlocError::Ipc("Connection closed early".into()));
                    return ptr::null_mut();
                }
                if n < 0 && crate::utility::errno_val() != libc::EWOULDBLOCK && crate::utility::errno_val() != libc::EAGAIN {
                    libc::free(result as *mut c_void);
                    set_errmsg(errmsg, &MorlocError::Ipc("Recv error".into()));
                    return ptr::null_mut();
                }
            }
        }
        if !packet_received {
            libc::free(result as *mut c_void);
            set_errmsg(errmsg, &MorlocError::Ipc("Failed to retrieve packet".into()));
            return ptr::null_mut();
        }
    }

    result
}

#[no_mangle]
pub unsafe extern "C" fn stream_from_client(
    client_fd: i32,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    stream_from_client_wait(client_fd, 0, 0, errmsg)
}

// ── send_and_receive_over_socket ─────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn send_and_receive_over_socket_wait(
    socket_path: *const c_char,
    packet: *const u8,
    poll_timeout_us: i32,
    recv_timeout_us: i32,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);

    let mut err: *mut c_char = ptr::null_mut();
    let client_fd = new_socket(&mut err);
    if client_fd < 0 {
        *errmsg = err;
        return ptr::null_mut();
    }

    let addr = new_server_addr(socket_path);

    // Connect with retry (matching C WAIT macro behavior)
    let mut retcode;
    let mut attempts = 0;
    loop {
        retcode = libc::connect(client_fd, &addr as *const libc::sockaddr_un as *const libc::sockaddr,
                                std::mem::size_of::<libc::sockaddr_un>() as u32);
        if retcode == 0 { break; }
        attempts += 1;
        if attempts > 300 { // ~30 seconds with 100ms sleep
            close_socket(client_fd);
            set_errmsg(errmsg, &MorlocError::Ipc(format!(
                "Failed to connect to pipe '{}'",
                CStr::from_ptr(socket_path).to_string_lossy()
            )));
            return ptr::null_mut();
        }
        libc::usleep(100_000); // 100ms
    }

    let packet_size = crate::packet_ffi::morloc_packet_size(packet, &mut err);
    if !err.is_null() {
        close_socket(client_fd);
        *errmsg = err;
        return ptr::null_mut();
    }

    // Send packet, retrying on a full send buffer (non-blocking client fd).
    if !send_all(client_fd, packet, packet_size) {
        close_socket(client_fd);
        set_errmsg(errmsg, &MorlocError::Ipc(format!(
            "Failed to send data to '{}'",
            CStr::from_ptr(socket_path).to_string_lossy()
        )));
        return ptr::null_mut();
    }

    if trace_close_enabled() {
        eprintln!("[MLC_IPC] pid={} sent request fd={} bytes={} to '{}'",
            libc::getpid(), client_fd, packet_size,
            CStr::from_ptr(socket_path).to_string_lossy());
    }

    let result = stream_from_client_wait(client_fd, poll_timeout_us, recv_timeout_us, &mut err);
    if !err.is_null() {
        close_socket(client_fd);
        *errmsg = err;
        return ptr::null_mut();
    }

    close_socket(client_fd);
    result
}

#[no_mangle]
pub unsafe extern "C" fn send_and_receive_over_socket(
    socket_path: *const c_char,
    packet: *const u8,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    send_and_receive_over_socket_wait(socket_path, packet, 0, 0, errmsg)
}

// ── send_packet_to_foreign_server ────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn send_packet_to_foreign_server(
    client_fd: i32,
    packet: *mut u8,
    errmsg: *mut *mut c_char,
) -> usize {
    clear_errmsg(errmsg);

    let mut err: *mut c_char = ptr::null_mut();
    let size = crate::packet_ffi::morloc_packet_size(packet, &mut err);
    if !err.is_null() {
        *errmsg = err;
        return 0;
    }

    if !send_all(client_fd, packet, size) {
        set_errmsg(errmsg, &MorlocError::Ipc(format!(
            "Failed to send over client {}", client_fd
        )));
        return 0;
    }

    size
}

// ── wait_for_client ──────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn wait_for_client_with_timeout(
    daemon: *mut LanguageDaemon,
    timeout_us: i32,
    errmsg: *mut *mut c_char,
) -> i32 {
    clear_errmsg(errmsg);

    // poll() instead of pselect/FD_SET so any fd value is accepted (an fd_set
    // caps at FD_SETSIZE=1024). Index 0 is the listening socket; the client fds
    // follow. Only the server slot is acted on (accept), matching the original;
    // clients are included so activity on them still wakes the wait.
    let mut pfds: Vec<libc::pollfd> = Vec::new();
    pfds.push(libc::pollfd { fd: (*daemon).server_fd, events: libc::POLLIN, revents: 0 });
    let mut client = (*daemon).client_fds;
    while !client.is_null() {
        pfds.push(libc::pollfd { fd: (*client).fd, events: libc::POLLIN, revents: 0 });
        client = (*client).next;
    }

    // Timeout
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
            return 0;
        }
        set_errmsg(errmsg, &MorlocError::Ipc("poll error".into()));
        return -1;
    }
    if ready == 0 {
        return 0;
    }

    // Check for new connection
    if pfds[0].revents & libc::POLLIN != 0 {
        let selected_fd = libc::accept((*daemon).server_fd, ptr::null_mut(), ptr::null_mut());
        if selected_fd >= 0 {
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
            let mut handled = false;
            if PING_PEEK_ACTIVE.load(std::sync::atomic::Ordering::Relaxed) {
                match try_answer_ping(selected_fd) {
                    PingPeek::Answered => handled = true,
                    PingPeek::NotPing => {
                        PING_PEEK_ACTIVE.store(false, std::sync::atomic::Ordering::Relaxed);
                    }
                    PingPeek::Unknown => {}
                }
            }
            if !handled {
                let new_client = libc::calloc(1, std::mem::size_of::<ClientList>()) as *mut ClientList;
                (*new_client).fd = selected_fd;
                (*new_client).next = ptr::null_mut();

                if (*daemon).client_fds.is_null() {
                    (*daemon).client_fds = new_client;
                } else {
                    let mut last = (*daemon).client_fds;
                    while !(*last).next.is_null() {
                        last = (*last).next;
                    }
                    (*last).next = new_client;
                }
            }
        }
        // Ignore EAGAIN/EWOULDBLOCK on accept
    }

    if (*daemon).client_fds.is_null() {
        return 0; // spurious wakeup
    }

    // Dequeue first client
    let client_node = (*daemon).client_fds;
    let return_fd = (*client_node).fd;
    (*daemon).client_fds = (*client_node).next;
    libc::free(client_node as *mut c_void);

    if trace_close_enabled() {
        eprintln!("[MLC_IPC] pid={} wait_for_client return fd={}", libc::getpid(), return_fd);
    }
    return_fd
}

#[no_mangle]
pub unsafe extern "C" fn wait_for_client(
    daemon: *mut LanguageDaemon,
    errmsg: *mut *mut c_char,
) -> i32 {
    wait_for_client_with_timeout(daemon, 0, errmsg)
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
                libc::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()),
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
                libc::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()),
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
            libc::close(server);

            // -- non-ping: not consumed, left for the worker --
            let mut fds2 = [0i32; 2];
            assert_eq!(
                libc::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds2.as_mut_ptr()),
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
                libc::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()),
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
