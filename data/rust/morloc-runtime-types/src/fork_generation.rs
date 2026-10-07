use std::sync::atomic::{AtomicU64, Ordering};

static GENERATION: AtomicU64 = AtomicU64::new(0);

/// How many forks separate this process from the first one that loaded the
/// runtime (FORK-14).
pub fn generation() -> u64 {
    GENERATION.load(Ordering::Relaxed)
}

/// Called by the runtime's fork handler in the child.
pub fn bump_in_child() {
    GENERATION.fetch_add(1, Ordering::Relaxed);
}

#[cfg(test)]
extern "C" fn bump_for_tests() {
    bump_in_child();
}

#[cfg(test)]
extern "C" fn register_for_tests() {
    unsafe { libc::pthread_atfork(None, None, Some(bump_for_tests)) };
}

#[cfg(test)]
#[used]
#[cfg_attr(any(target_os = "linux", target_os = "android"), link_section = ".init_array")]
#[cfg_attr(target_os = "macos", link_section = "__DATA,__mod_init_func")]
static REGISTER_FOR_TESTS: extern "C" fn() = register_for_tests;

#[cfg(test)]
mod tests {
    #[test]
    fn a_forked_child_has_a_new_generation() {
        let parent = super::generation();
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            unsafe { libc::_exit(if super::generation() != parent { 0 } else { 1 }) };
        }
        let mut st = 0;
        unsafe { libc::waitpid(pid, &mut st, 0) };
        assert!(libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 0);
    }
}
