use std::sync::atomic::{AtomicPtr, Ordering};

/// A process-wide value set on first use, safe to fork at any moment
/// (INIT-3, model/runtime/tla/OncePublish.tla). Each first user builds a value
/// outside any lock and publishes it by compare-and-swap; a loser drops its
/// own and takes the published one. Nothing ever waits on another thread, so
/// a forked child is never left waiting on a builder it does not have.
pub struct PublishOnce<T> {
    value: AtomicPtr<T>,
}

// SAFETY: INIT-3: a published value is shared by reference and never freed.
unsafe impl<T: Send + Sync> Sync for PublishOnce<T> {}

impl<T> PublishOnce<T> {
    pub const fn new() -> Self {
        PublishOnce { value: AtomicPtr::new(std::ptr::null_mut()) }
    }

    pub fn get(&self) -> Option<&T> {
        // SAFETY: INIT-3: a published value is never freed.
        unsafe { self.value.load(Ordering::Acquire).as_ref() }
    }

    pub fn get_or_init(&self, build: impl FnOnce() -> T) -> &T {
        self.get_or_init_then(build, |_| ())
    }

    /// As `get_or_init`, running `published` once, on the value that won,
    /// in the thread that published it.
    pub fn get_or_init_then(&self, build: impl FnOnce() -> T, published: impl FnOnce(&T)) -> &T {
        if let Some(v) = self.get() {
            return v;
        }
        let mine = Box::into_raw(Box::new(build()));
        match self.value.compare_exchange(std::ptr::null_mut(), mine, Ordering::AcqRel, Ordering::Acquire) {
            Ok(_) => {
                // SAFETY: INIT-3: published, so never freed.
                let v = unsafe { &*mine };
                published(v);
                v
            }
            Err(theirs) => {
                // SAFETY: INIT-3: the loser frees only its own unpublished value.
                drop(unsafe { Box::from_raw(mine) });
                // SAFETY: INIT-3: a published value is never freed.
                unsafe { &*theirs }
            }
        }
    }
}

impl<T> Default for PublishOnce<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicUsize;
    use std::sync::{Arc, Barrier};

    #[test]
    fn first_uses_on_many_threads_share_one_value_and_one_side_effect() {
        static CELL: PublishOnce<usize> = PublishOnce::new();
        static EFFECTS: AtomicUsize = AtomicUsize::new(0);
        let start = Arc::new(Barrier::new(8));
        let seen: Vec<usize> = (0..8)
            .map(|i| {
                let start = Arc::clone(&start);
                std::thread::spawn(move || {
                    start.wait();
                    *CELL.get_or_init_then(|| i, |_| {
                        EFFECTS.fetch_add(1, Ordering::SeqCst);
                    })
                })
            })
            .collect::<Vec<_>>()
            .into_iter()
            .map(|t| t.join().unwrap())
            .collect();
        assert!(seen.iter().all(|v| *v == seen[0]));
        assert_eq!(EFFECTS.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn a_child_forked_while_another_thread_builds_gets_a_value_at_once() {
        static CELL: PublishOnce<u32> = PublishOnce::new();
        let inside = Arc::new(Barrier::new(2));
        let (release_tx, release_rx) = std::sync::mpsc::channel::<()>();
        let builder = {
            let inside = Arc::clone(&inside);
            std::thread::spawn(move || {
                *CELL.get_or_init(|| {
                    inside.wait();
                    let _ = release_rx.recv();
                    1
                })
            })
        };
        inside.wait();
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            unsafe { libc::alarm(5) };
            let got = *CELL.get_or_init(|| 2);
            unsafe { libc::_exit(if got == 2 { 0 } else { 1 }) };
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        release_tx.send(()).unwrap();
        assert_eq!(builder.join().unwrap(), 1);
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "child status {status}");
    }
}
