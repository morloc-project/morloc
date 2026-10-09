//! How long a remote client may take over a request (model/runtime/network.md NET-2).

use std::io;
use std::time::{Duration, Instant};

/// The request line and headers, or a length prefix, arrive within this of
/// the connection being served.
pub const HEAD_LIMIT: Duration = Duration::from_secs(30);

/// A body arrives within this, plus a second for every `BODY_MIN_RATE` bytes.
pub const BODY_BASE: Duration = Duration::from_secs(30);
pub const BODY_MIN_RATE: u64 = 16 * 1024;

pub fn body_limit(len: usize) -> Duration {
    BODY_BASE + Duration::from_secs(len as u64 / BODY_MIN_RATE)
}

/// Read up to `len` bytes into `buf` from `fd`, giving up at `until`
/// however the bytes trickle in. `Ok(0)` is end of file.
///
/// # Safety
///
/// `buf` must be valid for `len` bytes of writes.
pub unsafe fn recv_by(fd: libc::c_int, buf: *mut u8, len: usize, until: Instant) -> io::Result<usize> {
    loop {
        if Instant::now() >= until {
            return Err(io::Error::new(io::ErrorKind::TimedOut, "the client took too long to send its request"));
        }
        let n = libc::recv(fd, buf as *mut libc::c_void, len, libc::MSG_DONTWAIT);
        if n >= 0 {
            return Ok(n as usize);
        }
        let e = io::Error::last_os_error();
        match e.kind() {
            io::ErrorKind::Interrupted => continue,
            io::ErrorKind::WouldBlock => {}
            _ => return Err(e),
        }
        let left = until.saturating_duration_since(Instant::now());
        let mut pfd = libc::pollfd { fd, events: libc::POLLIN, revents: 0 };
        let ms = left.as_millis().clamp(1, i32::MAX as u128) as libc::c_int;
        if libc::poll(&mut pfd, 1, ms) < 0 {
            let e = io::Error::last_os_error();
            if e.kind() != io::ErrorKind::Interrupted {
                return Err(e);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_request_trickling_in_ends_at_its_deadline() {
        let mut fds = [0; 2];
        assert_eq!(unsafe { libc::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()) }, 0);
        let (reader, writer) = (fds[0], fds[1]);
        let dripper = std::thread::spawn(move || {
            for _ in 0..40 {
                if unsafe { libc::write(writer, b"x".as_ptr() as *const libc::c_void, 1) } != 1 {
                    break;
                }
                std::thread::sleep(Duration::from_millis(25));
            }
            unsafe { libc::close(writer) };
        });
        let start = Instant::now();
        let until = start + Duration::from_millis(300);
        let mut buf = [0u8; 64];
        let mut got = 0;
        let end = loop {
            match unsafe { recv_by(reader, buf.as_mut_ptr(), 1, until) } {
                Ok(0) => break Ok(()),
                Ok(n) => got += n,
                Err(e) => break Err(e),
            }
        };
        assert_eq!(end.unwrap_err().kind(), io::ErrorKind::TimedOut);
        assert!(got > 0 && got < 40, "read {got} bytes");
        assert!(start.elapsed() < Duration::from_millis(600));
        unsafe { libc::close(reader) };
        dripper.join().unwrap();
    }

    #[test]
    fn a_large_body_gets_time_in_proportion() {
        assert_eq!(body_limit(0), BODY_BASE);
        assert_eq!(body_limit(64 * 1024 * 1024), BODY_BASE + Duration::from_secs(4096));
    }
}
