use std::mem::ManuallyDrop;
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicU64, Ordering};

static GENERATION: AtomicU64 = AtomicU64::new(0);

extern "C" fn bump_in_child() {
    GENERATION.fetch_add(1, Ordering::Relaxed);
}

pub(crate) fn register() {
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| unsafe {
        libc::pthread_atfork(None, None, Some(bump_in_child));
    });
}

pub(crate) fn generation() -> u64 {
    GENERATION.load(Ordering::Relaxed)
}

pub(crate) struct ForkLocal<T> {
    generation: u64,
    value: ManuallyDrop<T>,
}

impl<T> ForkLocal<T> {
    pub(crate) fn new(value: T) -> Self {
        register();
        ForkLocal { generation: generation(), value: ManuallyDrop::new(value) }
    }

    pub(crate) fn is_inherited(&self) -> bool {
        self.generation != generation()
    }

    pub(crate) fn into_inner(self) -> Option<T> {
        let mut this = ManuallyDrop::new(self);
        if this.is_inherited() {
            return None;
        }
        Some(unsafe { ManuallyDrop::take(&mut this.value) })
    }
}

impl<T> Deref for ForkLocal<T> {
    type Target = T;
    fn deref(&self) -> &T {
        assert!(!self.is_inherited(), "a forked child used state its parent owns");
        &self.value
    }
}

impl<T> DerefMut for ForkLocal<T> {
    fn deref_mut(&mut self) -> &mut T {
        assert!(!self.is_inherited(), "a forked child used state its parent owns");
        &mut self.value
    }
}

impl<T> Drop for ForkLocal<T> {
    fn drop(&mut self) {
        if !self.is_inherited() {
            unsafe { ManuallyDrop::drop(&mut self.value) }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;

    #[test]
    fn a_value_dropped_in_a_forked_child_is_forgotten() {
        let probe = Arc::new(());
        let local = ForkLocal::new(Arc::clone(&probe));
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            let inherited = local.is_inherited();
            drop(local);
            let forgotten = Arc::strong_count(&probe) == 2;
            unsafe { libc::_exit(if inherited && forgotten { 0 } else { 1 }) }
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0);
        assert!(!local.is_inherited());
        drop(local);
        assert_eq!(Arc::strong_count(&probe), 1);
    }

    #[test]
    fn a_forked_child_gets_nothing_back_from_into_inner() {
        let local = ForkLocal::new(7u32);
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            let none = ForkLocal::into_inner(local).is_none();
            unsafe { libc::_exit(if none { 0 } else { 1 }) }
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0);
        assert_eq!(local.into_inner(), Some(7));
    }
}
