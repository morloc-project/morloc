use libc::{c_char, c_int};
use std::ffi::{CStr, CString};
use std::io;

#[cfg(target_os = "macos")]
extern "C" {
    fn posix_spawn_file_actions_addinherit_np(actions: *mut libc::posix_spawn_file_actions_t, fd: c_int) -> c_int;
}

fn check(rc: c_int) -> io::Result<()> {
    if rc == 0 {
        Ok(())
    } else {
        Err(io::Error::from_raw_os_error(rc))
    }
}

/// A child process started without fork, so no fork handler runs and the
/// parent's locks and threads are never copied.
pub struct Spawn {
    actions: libc::posix_spawn_file_actions_t,
    attr: libc::posix_spawnattr_t,
    flags: libc::c_short,
}

impl Spawn {
    pub fn new() -> io::Result<Spawn> {
        let mut s = Spawn {
            actions: unsafe { std::mem::zeroed() },
            attr: unsafe { std::mem::zeroed() },
            flags: 0,
        };
        check(unsafe { libc::posix_spawn_file_actions_init(&mut s.actions) })?;
        if let Err(e) = check(unsafe { libc::posix_spawnattr_init(&mut s.attr) }) {
            unsafe { libc::posix_spawn_file_actions_destroy(&mut s.actions) };
            std::mem::forget(s);
            return Err(e);
        }
        Ok(s)
    }

    pub fn new_process_group(&mut self) -> io::Result<()> {
        check(unsafe { libc::posix_spawnattr_setpgroup(&mut self.attr, 0) })?;
        self.flags |= libc::POSIX_SPAWN_SETPGROUP as libc::c_short;
        Ok(())
    }

    pub fn dup2(&mut self, from: c_int, to: c_int) -> io::Result<()> {
        check(unsafe { libc::posix_spawn_file_actions_adddup2(&mut self.actions, from, to) })
    }

    pub fn close(&mut self, fd: c_int) -> io::Result<()> {
        check(unsafe { libc::posix_spawn_file_actions_addclose(&mut self.actions, fd) })
    }

    pub fn keep_across_exec(&mut self, fd: c_int) -> io::Result<()> {
        #[cfg(target_os = "macos")]
        {
            check(unsafe { posix_spawn_file_actions_addinherit_np(&mut self.actions, fd) })
        }
        #[cfg(not(target_os = "macos"))]
        {
            self.dup2(fd, fd)
        }
    }

    /// Start `program` with a NULL-terminated `argv` and `envp`, searching
    /// PATH when `search_path` is set.
    pub fn run(
        mut self,
        program: &CStr,
        argv: &[*const c_char],
        envp: &[*const c_char],
        search_path: bool,
    ) -> io::Result<libc::pid_t> {
        assert!(argv.last() == Some(&std::ptr::null()) && envp.last() == Some(&std::ptr::null()));
        check(unsafe { libc::posix_spawnattr_setflags(&mut self.attr, self.flags) })?;
        let mut pid: libc::pid_t = 0;
        let spawn = if search_path { libc::posix_spawnp } else { libc::posix_spawn };
        check(unsafe {
            spawn(
                &mut pid,
                program.as_ptr(),
                &self.actions,
                &self.attr,
                argv.as_ptr() as *const *mut c_char,
                envp.as_ptr() as *const *mut c_char,
            )
        })?;
        Ok(pid)
    }
}

impl Drop for Spawn {
    fn drop(&mut self) {
        unsafe {
            libc::posix_spawn_file_actions_destroy(&mut self.actions);
            libc::posix_spawnattr_destroy(&mut self.attr);
        }
    }
}

/// This process's environment as owned strings, and a NULL-terminated array
/// of pointers into them for `Spawn::run`.
pub fn current_environment() -> (Vec<CString>, Vec<*const c_char>) {
    let owned: Vec<CString> = std::env::vars_os()
        .filter_map(|(k, v)| {
            let mut kv = k.into_encoded_bytes();
            kv.push(b'=');
            kv.extend(v.into_encoded_bytes());
            CString::new(kv).ok()
        })
        .collect();
    let ptrs = owned.iter().map(|s| s.as_ptr()).chain(std::iter::once(std::ptr::null())).collect();
    (owned, ptrs)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn wait(pid: libc::pid_t) -> i32 {
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        if libc::WIFEXITED(status) { libc::WEXITSTATUS(status) } else { -1 }
    }

    fn sh(script: &str, s: Spawn) -> io::Result<libc::pid_t> {
        let prog = CString::new("/bin/sh").unwrap();
        let args = [CString::new("sh").unwrap(), CString::new("-c").unwrap(), CString::new(script).unwrap()];
        let argv: Vec<*const c_char> = args.iter().map(|a| a.as_ptr()).chain(std::iter::once(std::ptr::null())).collect();
        let (_env, envp) = current_environment();
        s.run(&prog, &argv, &envp, false)
    }

    #[test]
    fn a_spawned_child_writes_through_a_dup2_pipe() {
        let mut p = [0 as c_int; 2];
        assert_eq!(unsafe { crate::fd::pipe(p.as_mut_ptr()) }, 0);
        let mut s = Spawn::new().unwrap();
        s.dup2(p[1], 1).unwrap();
        s.close(p[0]).unwrap();
        let pid = sh("printf ok", s).unwrap();
        unsafe { libc::close(p[1]) };
        let mut buf = [0u8; 8];
        let n = unsafe { libc::read(p[0], buf.as_mut_ptr() as *mut libc::c_void, 8) };
        unsafe { libc::close(p[0]) };
        assert_eq!(wait(pid), 0);
        assert_eq!(&buf[..n as usize], b"ok");
    }

    #[test]
    fn a_descriptor_kept_across_exec_reaches_the_child_and_others_do_not() {
        let mut p = [0 as c_int; 2];
        assert_eq!(unsafe { crate::fd::pipe(p.as_mut_ptr()) }, 0);
        let mut s = Spawn::new().unwrap();
        s.keep_across_exec(p[0]).unwrap();
        let script = format!("test -e /dev/fd/{} && ! test -e /dev/fd/{}", p[0], p[1]);
        let pid = sh(&script, s).unwrap();
        let code = wait(pid);
        unsafe {
            libc::close(p[0]);
            libc::close(p[1]);
        }
        assert_eq!(code, 0);
    }

    #[test]
    fn a_child_can_start_its_own_process_group() {
        let mut s = Spawn::new().unwrap();
        s.new_process_group().unwrap();
        let pid = sh("sleep 1", s).unwrap();
        let group = unsafe { libc::getpgid(pid) };
        unsafe { libc::kill(pid, libc::SIGKILL) };
        wait(pid);
        assert_eq!(group, pid);
    }

    #[test]
    fn a_missing_program_is_an_error_not_a_dead_child() {
        let prog = CString::new("/nonexistent/morloc-no-such-program").unwrap();
        let argv = [prog.as_ptr(), std::ptr::null()];
        let (_env, envp) = current_environment();
        assert!(Spawn::new().unwrap().run(&prog, &argv, &envp, false).is_err());
    }
}
