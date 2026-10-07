use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};

const SLOTS: usize = 64;
const BUSY: i32 = 1 << 29;
const DEAD: i32 = 1 << 30;
const PGID: i32 = BUSY - 1;

// DAEMON-6: a group's leader stays unreaped while it is registered; every
// signal to the group goes through its slot, and none follows a SIGKILL.
pub struct ChildGroups {
    slots: [AtomicI32; SLOTS],
    stopped: AtomicBool,
}

pub struct Registered<'a> {
    groups: &'a ChildGroups,
    slot: usize,
}

impl ChildGroups {
    pub const fn new() -> Self {
        ChildGroups { slots: [const { AtomicI32::new(0) }; SLOTS], stopped: AtomicBool::new(false) }
    }

    /// `None` when the table is full: the caller stops the group itself.
    pub fn add(&self, pgid: libc::pid_t) -> Option<Registered<'_>> {
        assert!(pgid > 0 && pgid & !PGID == 0, "process group {pgid} out of range");
        let slot = (0..SLOTS).find(|&i| {
            self.slots[i].compare_exchange(0, pgid, Ordering::SeqCst, Ordering::SeqCst).is_ok()
        })?;
        let r = Registered { groups: self, slot };
        // DAEMON-6: a group added as `stop_all` runs is stopped by one of the two.
        if self.stopped.load(Ordering::SeqCst) {
            r.signal(libc::SIGKILL);
        }
        Some(r)
    }

    /// Kill every registered group, and every group added from now on.
    // DAEMON-6: callable from a signal handler.
    pub fn stop_all(&self) {
        self.stopped.store(true, Ordering::SeqCst);
        masked(|_| {
            for i in 0..SLOTS {
                self.signal_slot(i, libc::SIGKILL);
            }
        });
    }

    /// Send `sig` to every registered group that was not killed.
    // DAEMON-12: callable from a signal handler.
    pub fn signal_all(&self, sig: libc::c_int) {
        masked(|_| {
            for i in 0..SLOTS {
                self.signal_slot(i, sig);
            }
        });
    }

    /// The leader `pid` has exited and is about to be reaped: no signal
    /// reaches its group from now on.
    // DAEMON-11: callable from a signal handler; called before the reap, while the id is held.
    pub fn leader_exited(&self, pid: libc::pid_t) {
        masked(|_| {
            for slot in &self.slots {
                loop {
                    let v = slot.load(Ordering::SeqCst);
                    if v & PGID != pid || v & DEAD != 0 {
                        break;
                    }
                    if v & BUSY != 0 {
                        std::hint::spin_loop();
                        continue;
                    }
                    if slot.compare_exchange(v, v | DEAD, Ordering::SeqCst, Ordering::SeqCst).is_ok() {
                        break;
                    }
                }
            }
        });
    }

    pub fn is_stopped(&self) -> bool {
        self.stopped.load(Ordering::SeqCst)
    }

    fn signal_slot(&self, i: usize, sig: libc::c_int) {
        let slot = &self.slots[i];
        loop {
            let v = slot.load(Ordering::SeqCst);
            if v == 0 || v & DEAD != 0 {
                return;
            }
            if v & BUSY != 0 {
                std::hint::spin_loop();
                continue;
            }
            if slot.compare_exchange(v, v | BUSY, Ordering::SeqCst, Ordering::SeqCst).is_ok() {
                unsafe { libc::kill(-(v & PGID), sig) };
                slot.store(if sig == libc::SIGKILL { v | DEAD } else { v }, Ordering::SeqCst);
                return;
            }
        }
    }
}

impl Default for ChildGroups {
    fn default() -> Self {
        Self::new()
    }
}

impl Registered<'_> {
    /// Signal the group unless it was already killed.
    pub fn signal(&self, sig: libc::c_int) {
        masked(|_| self.groups.signal_slot(self.slot, sig));
    }

    /// Run `f` holding the group's slot, so no signal reaches the group
    /// meanwhile; `None`, without running it, once the group was killed.
    /// `f` gets the signal mask to give a process it starts.
    // DAEMON-11: a pool joins its group only while the slot is held.
    pub fn while_held<T>(&self, f: impl FnOnce(&libc::sigset_t) -> T) -> Option<T> {
        let slot = &self.groups.slots[self.slot];
        masked(|old| loop {
            let v = slot.load(Ordering::SeqCst);
            if v & DEAD != 0 {
                return None;
            }
            if v & BUSY != 0 {
                std::hint::spin_loop();
                continue;
            }
            if slot.compare_exchange(v, v | BUSY, Ordering::SeqCst, Ordering::SeqCst).is_ok() {
                let r = f(old);
                slot.store(v, Ordering::SeqCst);
                return Some(r);
            }
        })
    }
}

// DAEMON-6: a slot is held only with signals blocked, so a handler that
// stops the table never spins on its own thread.
fn masked<T>(f: impl FnOnce(&libc::sigset_t) -> T) -> T {
    unsafe {
        let mut all: libc::sigset_t = std::mem::zeroed();
        let mut old: libc::sigset_t = std::mem::zeroed();
        libc::sigfillset(&mut all);
        libc::pthread_sigmask(libc::SIG_SETMASK, &all, &mut old);
        let r = f(&old);
        libc::pthread_sigmask(libc::SIG_SETMASK, &old, std::ptr::null_mut());
        r
    }
}

impl Drop for Registered<'_> {
    fn drop(&mut self) {
        let slot = &self.groups.slots[self.slot];
        loop {
            let v = slot.load(Ordering::SeqCst);
            if v & BUSY != 0 {
                std::hint::spin_loop();
                continue;
            }
            if slot.compare_exchange(v, 0, Ordering::SeqCst, Ordering::SeqCst).is_ok() {
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::spawn::Spawn;
    use std::ffi::CString;

    fn sleeper_group() -> libc::pid_t {
        let mut s = Spawn::new().unwrap();
        s.new_process_group().unwrap();
        let prog = CString::new("sleep").unwrap();
        let arg = CString::new("600").unwrap();
        let argv = [prog.as_ptr(), arg.as_ptr(), std::ptr::null()];
        let envp = [std::ptr::null()];
        s.run(&prog, &argv, &envp, true).unwrap()
    }

    fn killed(pid: libc::pid_t) -> bool {
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        libc::WIFSIGNALED(status) && libc::WTERMSIG(status) == libc::SIGKILL
    }

    #[test]
    fn stopping_all_kills_registered_groups_and_every_group_added_later() {
        let groups = ChildGroups::new();
        let first = sleeper_group();
        let r = groups.add(first).unwrap();
        groups.stop_all();
        assert!(killed(first));
        drop(r);
        let later = sleeper_group();
        let r = groups.add(later).unwrap();
        assert!(killed(later));
        drop(r);
    }

    #[test]
    fn a_killed_group_is_never_signalled_again() {
        let groups = ChildGroups::new();
        let pid = sleeper_group();
        let r = groups.add(pid).unwrap();
        r.signal(libc::SIGKILL);
        assert_eq!(groups.slots[r.slot].load(Ordering::SeqCst), pid | DEAD);
        r.signal(libc::SIGTERM);
        groups.stop_all();
        assert_eq!(groups.slots[r.slot].load(Ordering::SeqCst), pid | DEAD);
        assert!(killed(pid));
        drop(r);
        assert!(groups.slots.iter().all(|s| s.load(Ordering::SeqCst) == 0));
    }

    fn exited(pid: libc::pid_t) -> bool {
        let mut info: libc::siginfo_t = unsafe { std::mem::zeroed() };
        let rc = unsafe {
            libc::waitid(libc::P_PID, pid as libc::id_t, &mut info, libc::WEXITED | libc::WNOHANG | libc::WNOWAIT)
        };
        rc == 0 && unsafe { info.si_pid() } == pid
    }

    #[test]
    fn a_group_whose_leader_exited_is_never_signalled_again() {
        let groups = ChildGroups::new();
        let pid = sleeper_group();
        let r = groups.add(pid).unwrap();
        groups.leader_exited(pid);
        assert_eq!(groups.slots[r.slot].load(Ordering::SeqCst), pid | DEAD);
        r.signal(libc::SIGKILL);
        groups.stop_all();
        assert!(!exited(pid));
        assert!(r.while_held(|_| ()).is_none());
        unsafe { libc::kill(pid, libc::SIGKILL) };
        assert!(killed(pid));
    }

    #[test]
    fn stopping_all_waits_for_a_held_group_then_kills_it() {
        let groups: &'static ChildGroups = Box::leak(Box::new(ChildGroups::new()));
        let pid = sleeper_group();
        let r = groups.add(pid).unwrap();
        let stopper = r
            .while_held(|_| {
                let stopper = std::thread::spawn(|| groups.stop_all());
                std::thread::sleep(std::time::Duration::from_millis(100));
                assert!(!exited(pid), "a signal reached a held group");
                stopper
            })
            .unwrap();
        stopper.join().unwrap();
        assert!(killed(pid));
        assert!(r.while_held(|_| ()).is_none());
    }

    #[test]
    fn signalling_all_reaches_every_live_group_and_no_killed_one() {
        let groups = ChildGroups::new();
        let (a, b) = (sleeper_group(), sleeper_group());
        let (ra, rb) = (groups.add(a).unwrap(), groups.add(b).unwrap());
        rb.signal(libc::SIGKILL);
        assert!(killed(b));
        groups.signal_all(libc::SIGTERM);
        let mut status = 0;
        unsafe { libc::waitpid(a, &mut status, 0) };
        assert!(libc::WIFSIGNALED(status) && libc::WTERMSIG(status) == libc::SIGTERM);
        assert_eq!(groups.slots[rb.slot].load(Ordering::SeqCst), b | DEAD);
        drop((ra, rb));
    }

    #[test]
    fn a_full_table_refuses_a_group() {
        let groups = ChildGroups::new();
        let held: Vec<_> = (0..SLOTS).map(|i| groups.add(1000 + i as i32).unwrap()).collect();
        assert!(groups.add(5000).is_none());
        drop(held);
        assert!(groups.add(5000).is_some());
    }
}
