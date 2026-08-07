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

// A polled fd is ready to read when data is available or the peer has
// hung up / errored (so the following recv observes EOF or the error).
#[inline]
unsafe fn pfd_ready(pfd: &libc::pollfd) -> bool {
    pfd.revents & (libc::POLLIN | libc::POLLHUP | libc::POLLERR) != 0
}

// ── close_socket / close_daemon ──────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn close_socket(socket_id: i32) {
    if socket_id >= 0 {
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

    // poll() (via ppoll for the atomic signal mask) instead of pselect/FD_SET:
    // an fd_set can only hold descriptors below FD_SETSIZE (1024), and FD_SET on
    // a higher fd is out-of-bounds. A pollfd imposes no ceiling on the fd value.
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

    // Signal mask setup
    let mut mask: libc::sigset_t = std::mem::zeroed();
    let mut origmask: libc::sigset_t = std::mem::zeroed();
    libc::sigemptyset(&mut mask);
    libc::sigaddset(&mut mask, libc::SIGINT);
    libc::pthread_sigmask(libc::SIG_SETMASK, &mask, &mut origmask);

    // Initial receive with timeout
    let mut ready;
    loop {
        ready = libc::ppoll(&mut pfd, 1, timeout_ptr, &origmask);
        if !(ready < 0 && crate::utility::errno_val() == libc::EINTR) {
            break;
        }
    }
    libc::pthread_sigmask(libc::SIG_SETMASK, &origmask, ptr::null_mut());

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

    let recv_length = libc::recv(client_fd, buffer as *mut c_void, BUFFER_SIZE, 0);
    if recv_length == 0 {
        libc::free(buffer as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Ipc("Connection closed by peer".into()));
        return ptr::null_mut();
    }
    if recv_length < 0 && crate::utility::errno_val() != libc::EWOULDBLOCK && crate::utility::errno_val() != libc::EAGAIN {
        libc::free(buffer as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Ipc("Recv error".into()));
        return ptr::null_mut();
    }

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

            libc::pthread_sigmask(libc::SIG_SETMASK, &mask, ptr::null_mut());
            ready = libc::ppoll(&mut pfd, 1, recv_timeout_ptr, &origmask);
            libc::pthread_sigmask(libc::SIG_SETMASK, &origmask, ptr::null_mut());

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

    // Send packet in loop
    let mut total_sent: usize = 0;
    while total_sent < packet_size {
        let bytes_sent = libc::send(
            client_fd,
            packet.add(total_sent) as *const c_void,
            packet_size - total_sent,
            crate::utility::SEND_NOSIGNAL,
        );
        if bytes_sent <= 0 {
            close_socket(client_fd);
            set_errmsg(errmsg, &MorlocError::Ipc(format!(
                "Failed to send data to '{}'",
                CStr::from_ptr(socket_path).to_string_lossy()
            )));
            return ptr::null_mut();
        }
        total_sent += bytes_sent as usize;
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

    let mut total_sent: usize = 0;
    while total_sent < size {
        let bytes_sent = libc::send(
            client_fd,
            packet.add(total_sent) as *const c_void,
            size - total_sent,
            crate::utility::SEND_NOSIGNAL,
        );
        if bytes_sent <= 0 {
            set_errmsg(errmsg, &MorlocError::Ipc(format!(
                "Failed to send over client {}", client_fd
            )));
            return 0;
        }
        total_sent += bytes_sent as usize;
    }

    total_sent
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

    let mut emptymask: libc::sigset_t = std::mem::zeroed();
    libc::sigemptyset(&mut emptymask);

    let ready = libc::ppoll(pfds.as_mut_ptr(), pfds.len() as libc::nfds_t, timeout_ptr, &emptymask);
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
            crate::utility::set_nosigpipe(selected_fd);
            libc::fcntl(selected_fd, libc::F_SETFL, libc::O_NONBLOCK);

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

    return_fd
}

#[no_mangle]
pub unsafe extern "C" fn wait_for_client(
    daemon: *mut LanguageDaemon,
    errmsg: *mut *mut c_char,
) -> i32 {
    wait_for_client_with_timeout(daemon, 0, errmsg)
}
