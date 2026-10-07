use libc::c_int;

#[cfg(not(target_os = "linux"))]
unsafe fn set_cloexec(fd: c_int) -> c_int {
    if fd >= 0 && libc::fcntl(fd, libc::F_SETFD, libc::FD_CLOEXEC) != 0 {
        libc::close(fd);
        return -1;
    }
    fd
}

/// # Safety
/// `fds` must point to two writable `c_int`s.
pub unsafe fn pipe(fds: *mut c_int) -> c_int {
    #[cfg(target_os = "linux")]
    {
        libc::pipe2(fds, libc::O_CLOEXEC)
    }
    #[cfg(not(target_os = "linux"))]
    {
        if libc::pipe(fds) != 0 {
            return -1;
        }
        if set_cloexec(*fds) < 0 {
            libc::close(*fds.add(1));
            return -1;
        }
        if set_cloexec(*fds.add(1)) < 0 {
            libc::close(*fds);
            return -1;
        }
        0
    }
}

/// # Safety
/// As `socket(2)`.
pub unsafe fn socket(domain: c_int, ty: c_int, protocol: c_int) -> c_int {
    #[cfg(target_os = "linux")]
    {
        libc::socket(domain, ty | libc::SOCK_CLOEXEC, protocol)
    }
    #[cfg(not(target_os = "linux"))]
    {
        set_cloexec(libc::socket(domain, ty, protocol))
    }
}

/// # Safety
/// `fds` must point to two writable `c_int`s.
pub unsafe fn socketpair(domain: c_int, ty: c_int, protocol: c_int, fds: *mut c_int) -> c_int {
    #[cfg(target_os = "linux")]
    {
        libc::socketpair(domain, ty | libc::SOCK_CLOEXEC, protocol, fds)
    }
    #[cfg(not(target_os = "linux"))]
    {
        if libc::socketpair(domain, ty, protocol, fds) != 0 {
            return -1;
        }
        if set_cloexec(*fds) < 0 {
            libc::close(*fds.add(1));
            return -1;
        }
        if set_cloexec(*fds.add(1)) < 0 {
            libc::close(*fds);
            return -1;
        }
        0
    }
}

/// # Safety
/// As `accept(2)`.
pub unsafe fn accept(fd: c_int, addr: *mut libc::sockaddr, len: *mut libc::socklen_t) -> c_int {
    #[cfg(target_os = "linux")]
    {
        libc::accept4(fd, addr, len, libc::SOCK_CLOEXEC)
    }
    #[cfg(not(target_os = "linux"))]
    {
        set_cloexec(libc::accept(fd, addr, len))
    }
}

/// # Safety
/// As `dup(2)`.
pub unsafe fn dup(fd: c_int) -> c_int {
    libc::fcntl(fd, libc::F_DUPFD_CLOEXEC, 0)
}

/// # Safety
/// As `mkstemp(3)`.
pub unsafe fn mkstemp(template: *mut libc::c_char) -> c_int {
    #[cfg(target_os = "linux")]
    {
        libc::mkostemp(template, libc::O_CLOEXEC)
    }
    #[cfg(not(target_os = "linux"))]
    {
        set_cloexec(libc::mkstemp(template))
    }
}

#[cfg(target_os = "linux")]
unsafe fn with_cloexec_mode(mode: *const libc::c_char) -> std::ffi::CString {
    let mut m = std::ffi::CStr::from_ptr(mode).to_bytes().to_vec();
    m.push(b'e');
    std::ffi::CString::new(m).unwrap()
}

#[cfg(not(target_os = "linux"))]
unsafe fn cloexec_stream(f: *mut libc::FILE) -> *mut libc::FILE {
    if !f.is_null() {
        libc::fcntl(libc::fileno(f), libc::F_SETFD, libc::FD_CLOEXEC);
    }
    f
}

/// # Safety
/// As `fopen(3)`.
pub unsafe fn fopen(path: *const libc::c_char, mode: *const libc::c_char) -> *mut libc::FILE {
    #[cfg(target_os = "linux")]
    {
        libc::fopen(path, with_cloexec_mode(mode).as_ptr())
    }
    #[cfg(not(target_os = "linux"))]
    {
        cloexec_stream(libc::fopen(path, mode))
    }
}

/// # Safety
/// As `popen(3)`.
pub unsafe fn popen(command: *const libc::c_char, mode: *const libc::c_char) -> *mut libc::FILE {
    #[cfg(target_os = "linux")]
    {
        libc::popen(command, with_cloexec_mode(mode).as_ptr())
    }
    #[cfg(not(target_os = "linux"))]
    {
        cloexec_stream(libc::popen(command, mode))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn is_cloexec(fd: c_int) -> bool {
        let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
        flags >= 0 && flags & libc::FD_CLOEXEC != 0
    }

    #[test]
    fn every_constructor_returns_close_on_exec_descriptors() {
        unsafe {
            let mut p = [0 as c_int; 2];
            assert_eq!(pipe(p.as_mut_ptr()), 0);
            assert!(is_cloexec(p[0]) && is_cloexec(p[1]));

            let d = dup(p[0]);
            assert!(d >= 0 && is_cloexec(d));

            let mut sp = [0 as c_int; 2];
            assert_eq!(socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, sp.as_mut_ptr()), 0);
            assert!(is_cloexec(sp[0]) && is_cloexec(sp[1]));

            let s = socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
            assert!(s >= 0 && is_cloexec(s));

            let dir = std::env::temp_dir().join(format!("morloc_fd_test_{}", std::process::id()));
            let _ = std::fs::create_dir_all(&dir);
            let path = dir.join("s");
            let _ = std::fs::remove_file(&path);
            let mut addr: libc::sockaddr_un = std::mem::zeroed();
            addr.sun_family = libc::AF_UNIX as libc::sa_family_t;
            for (i, b) in path.to_str().unwrap().bytes().enumerate() {
                addr.sun_path[i] = b as libc::c_char;
            }
            let alen = std::mem::size_of::<libc::sockaddr_un>() as libc::socklen_t;
            assert_eq!(libc::bind(s, &addr as *const _ as *const libc::sockaddr, alen), 0);
            assert_eq!(libc::listen(s, 1), 0);
            let c = socket(libc::AF_UNIX, libc::SOCK_STREAM, 0);
            assert_eq!(libc::connect(c, &addr as *const _ as *const libc::sockaddr, alen), 0);
            let a = accept(s, std::ptr::null_mut(), std::ptr::null_mut());
            assert!(a >= 0 && is_cloexec(a));

            let mut tmpl = *b"/tmp/morloc_fd_test_XXXXXX\0";
            let t = mkstemp(tmpl.as_mut_ptr() as *mut libc::c_char);
            assert!(t >= 0 && is_cloexec(t));
            let f = fopen(tmpl.as_ptr() as *const libc::c_char, b"r\0".as_ptr() as *const libc::c_char);
            assert!(!f.is_null() && is_cloexec(libc::fileno(f)));
            libc::fclose(f);
            libc::unlink(tmpl.as_ptr() as *const libc::c_char);
            let pf = popen(b"true\0".as_ptr() as *const libc::c_char, b"r\0".as_ptr() as *const libc::c_char);
            assert!(!pf.is_null() && is_cloexec(libc::fileno(pf)));
            libc::pclose(pf);

            for fd in [p[0], p[1], d, sp[0], sp[1], s, c, a, t] {
                libc::close(fd);
            }
            let _ = std::fs::remove_dir_all(&dir);
        }
    }
}

pub fn fill_standard_descriptors() {
    for fd in 0..=2 {
        if unsafe { libc::fcntl(fd, libc::F_GETFD) } < 0 {
            let got = unsafe { libc::open(c"/dev/null".as_ptr(), libc::O_RDWR) };
            if got >= 0 && got != fd {
                unsafe { libc::close(got) };
            }
        }
    }
}
