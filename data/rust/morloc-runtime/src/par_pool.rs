use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicU8, AtomicUsize, Ordering};
use std::sync::Condvar;

use morloc_runtime_types::publish_once::PublishOnce;
use std::time::{Duration, Instant};

use crate::fork_policy::{generation, Reset, ResetGuard};

pub type SpanFn = unsafe extern "C" fn(*mut c_void, usize) -> libc::c_int;
pub type EndFn = unsafe extern "C" fn(*mut c_void);

// WPOOL-6
pub(crate) const MAX_HELPERS: usize = 255;

struct Job {
    maxh: usize,
    seq: u64,
    generation: u64,
    // WPOOL-12: the unclaimed spans [lo, hi), as lo | hi << 32.
    spans: AtomicU64,
    stop: AtomicBool,
    // WPOOL-2: changed only under the pool lock.
    on: AtomicUsize,
    // WPOOL-12: its helpers have been reserved and woken; set under the lock.
    woken: AtomicBool,
    // WPOOL-12: when the timer first saw the job, in nanoseconds of its own
    // clock, 0 for not yet, or SEEN_DUE to wake it when the timer sees it;
    // written under the lock.
    seen: AtomicU64,
    span: SpanFn,
    end: EndFn,
    ctx: *mut c_void,
}

impl Job {
    fn left(&self) -> bool {
        let v = self.spans.load(Ordering::Relaxed);
        (v as u32) < (v >> 32) as u32
    }

    // WPOOL-12: helpers, and the caller once woken, claim from the front.
    fn claim_front(&self) -> Option<usize> {
        let mut v = self.spans.load(Ordering::Relaxed);
        loop {
            if (v as u32) >= (v >> 32) as u32 {
                return None;
            }
            match self.spans.compare_exchange_weak(v, v + 1, Ordering::Relaxed, Ordering::Relaxed) {
                Ok(_) => return Some(v as u32 as usize),
                Err(now) => v = now,
            }
        }
    }

    // WPOOL-12: a caller working alone claims from the back, where a
    // shrinking schedule's spans are smallest.
    fn claim_back(&self) -> Option<usize> {
        let mut v = self.spans.load(Ordering::Relaxed);
        loop {
            let (lo, hi) = (v as u32 as u64, v >> 32);
            if lo >= hi {
                return None;
            }
            match self.spans.compare_exchange_weak(v, lo | ((hi - 1) << 32), Ordering::Relaxed, Ordering::Relaxed) {
                Ok(_) => return Some((hi - 1) as usize),
                Err(now) => v = now,
            }
        }
    }

    fn eligible(&self) -> bool {
        self.woken.load(Ordering::Relaxed)
            && self.left()
            && !self.stop.load(Ordering::Relaxed)
            && self.on.load(Ordering::Relaxed) < self.maxh
    }
}

const SEEN_DUE: u64 = u64::MAX;

struct Posted(*const Job);

// SAFETY: WPOOL-2: a posted job lives until its caller has removed it and no
// helper is on it.
unsafe impl Send for Posted {}

struct Shared {
    work: Condvar,
    done: Condvar,
    timer: Condvar,
    posted: AtomicUsize,
}

#[derive(PartialEq)]
enum Timer {
    Absent,
    Running,
    Failed,
}

pub(crate) struct State {
    jobs: Vec<Posted>,
    seq: u64,
    rr: u64,
    helpers: usize,
    // WPOOL-6: reserved helpers not yet started.
    starting: usize,
    looking: usize,
    sleeping: usize,
    pending: usize,
    retire_after: Duration,
    timer: Timer,
    // WPOOL-12: the timer will look at the jobs again within the delay.
    timer_armed: bool,
    shared: &'static Shared,
}

impl State {
    fn new() -> State {
        State {
            jobs: Vec::new(),
            seq: 0,
            rr: 0,
            helpers: 0,
            starting: 0,
            looking: 0,
            sleeping: 0,
            pending: 0,
            retire_after: crate::pool_ffi::worker_idle_timeout(),
            timer: Timer::Absent,
            timer_armed: false,
            // FORK-8: one per instance; an instance is never freed.
            shared: Box::leak(Box::new(Shared {
                work: Condvar::new(),
                done: Condvar::new(),
                timer: Condvar::new(),
                posted: AtomicUsize::new(0),
            })),
        }
    }

    // WPOOL-7: the first eligible job after the last one picked, in post
    // order, else the first eligible one.
    fn pick(&mut self) -> Option<&'static Job> {
        let mut first: Option<&'static Job> = None;
        let mut next: Option<&'static Job> = None;
        for p in &self.jobs {
            // SAFETY: WPOOL-2: posted, so alive.
            let job: &'static Job = unsafe { &*p.0 };
            if job.eligible() {
                if job.seq > self.rr {
                    next = Some(job);
                    break;
                }
                first.get_or_insert(job);
            }
        }
        let job = next.or(first)?;
        self.rr = job.seq;
        job.on.fetch_add(1, Ordering::Relaxed);
        Some(job)
    }

    // WPOOL-12: wake the job and pass the wake on; true when the caller
    // must start one helper.
    fn wake_job(&mut self, job: &Job) -> bool {
        if job.woken.swap(true, Ordering::AcqRel) {
            return false;
        }
        // WPOOL-7: the fast path counts woken jobs.
        self.shared.posted.fetch_add(1, Ordering::Relaxed);
        self.pass_wake()
    }

    // WPOOL-12: while a woken job has room, wake one sleeper, or reserve one
    // helper for the caller to start; true for the latter.
    fn pass_wake(&mut self) -> bool {
        let mut room = false;
        let mut grow = false;
        for p in &self.jobs {
            // SAFETY: WPOOL-2: posted, so alive while the lock is held.
            let job = unsafe { &*p.0 };
            if job.eligible() {
                room = true;
                grow |= self.helpers < job.maxh;
            }
        }
        if !room {
            return false;
        }
        if self.sleeping > self.pending {
            self.wake(1);
            return false;
        }
        if grow {
            // WPOOL-6: reserved under the lock, started after it.
            self.helpers += 1;
            self.starting += 1;
            self.looking += 1;
            return true;
        }
        false
    }

    // WPOOL-12: after a failed timer start, every job still waiting for it.
    fn wake_waiting(&mut self) -> usize {
        let mut starts = 0;
        let waiting: Vec<&'static Job> = self
            .jobs
            .iter()
            // SAFETY: WPOOL-2: posted, so alive while the lock is held.
            .map(|p| unsafe { &*p.0 })
            .filter(|j| !j.woken.load(Ordering::Relaxed) && j.left() && !j.stop.load(Ordering::Relaxed))
            .collect();
        for job in waiting {
            starts += usize::from(self.wake_job(job));
        }
        starts
    }

    // WPOOL-7: at most `n` sleepers, counting those already woken.
    fn wake(&mut self, n: usize) {
        let k = n.min(self.sleeping.saturating_sub(self.pending));
        self.pending += k;
        for _ in 0..k {
            self.shared.work.notify_one();
        }
    }
}

// WPOOL-5
static POOL: Reset<State> = Reset::new(State::new);

fn lock() -> ResetGuard<'static, State> {
    POOL.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock())
}

// WPOOL-1: MORLOC_PAR_POOL=0 turns the helpers off.
static ENABLED: AtomicU8 = AtomicU8::new(0);

pub(crate) fn enabled() -> bool {
    match ENABLED.load(Ordering::Relaxed) {
        1 => true,
        2 => false,
        _ => {
            let on = std::env::var("MORLOC_PAR_POOL").map_or(true, |v| v.trim() != "0");
            ENABLED.store(if on { 1 } else { 2 }, Ordering::Relaxed);
            on
        }
    }
}

struct SigSet(libc::sigset_t);

// SAFETY: WPOOL-8: plain data, written once.
unsafe impl Sync for SigSet {}
unsafe impl Send for SigSet {}

// WPOOL-8
static USER_MASK: PublishOnce<SigSet> = PublishOnce::new();

pub(crate) fn record_user_mask() {
    // SAFETY: WPOOL-8: reads this thread's mask into a set owned here.
    let mut mask: libc::sigset_t = unsafe { std::mem::zeroed() };
    unsafe { libc::pthread_sigmask(libc::SIG_BLOCK, std::ptr::null(), &mut mask) };
    USER_MASK.get_or_init(|| SigSet(mask));
}

fn user_mask() -> libc::sigset_t {
    match USER_MASK.get() {
        Some(m) => m.0,
        None => {
            // SAFETY: WPOOL-8: an empty set owned here.
            let mut empty: libc::sigset_t = unsafe { std::mem::zeroed() };
            unsafe { libc::sigemptyset(&mut empty) };
            empty
        }
    }
}

#[cfg(test)]
static FAIL_SPAWNS: AtomicUsize = AtomicUsize::new(0);

// WPOOL-8: started with every signal blocked; the helper then takes the
// recorded mask.
fn spawn_helper() -> std::io::Result<()> {
    #[cfg(test)]
    if FAIL_SPAWNS.load(Ordering::SeqCst) > 0 {
        FAIL_SPAWNS.fetch_sub(1, Ordering::SeqCst);
        return Err(std::io::Error::other("refused by the test"));
    }
    spawn_masked("morloc-par", crate::pool_ffi::worker_stack_size(), helper_main)
}

fn spawn_masked(name: &str, stack: usize, f: fn()) -> std::io::Result<()> {
    // SAFETY: WPOOL-8: signal-mask calls on sets owned here.
    unsafe {
        let mut all: libc::sigset_t = std::mem::zeroed();
        let mut old: libc::sigset_t = std::mem::zeroed();
        libc::sigfillset(&mut all);
        for s in [libc::SIGSEGV, libc::SIGBUS, libc::SIGFPE, libc::SIGILL] {
            libc::sigdelset(&mut all, s);
        }
        libc::pthread_sigmask(libc::SIG_SETMASK, &all, &mut old);
        let started = std::thread::Builder::new().name(name.into()).stack_size(stack).spawn(f);
        libc::pthread_sigmask(libc::SIG_SETMASK, &old, std::ptr::null_mut());
        started.map(|_| ())
    }
}

fn helper_main() {
    let mask = user_mask();
    // SAFETY: WPOOL-8: sets this thread's mask from a set owned here.
    unsafe { libc::pthread_sigmask(libc::SIG_SETMASK, &mask, std::ptr::null_mut()) };
    // SAFETY: STATE-7: the alternate stack is this thread's, released as it ends.
    let _altstack = unsafe { crate::pool_ffi::AltStack::install() };
    morloc_runtime_types::panic::outside_scope(helper_loop)
}

// WPOOL-1: spans claimed from the cursor until none is left or the job has
// failed; a helper keeps going only while its job is the only one posted.
fn work(job: &Job, posted: Option<&AtomicUsize>) {
    loop {
        if job.stop.load(Ordering::Acquire) {
            return;
        }
        let Some(i) = job.claim_front() else { return };
        // SAFETY: WPOOL-1: the caller's span callback and context, for an
        // index below nspans.
        let rc = unsafe { (job.span)(job.ctx, i) };
        // WPOOL-5
        crate::ipc_ffi::exit_if_forked_since(job.generation);
        if rc != 0 {
            job.stop.store(true, Ordering::Release);
            return;
        }
        if posted.is_some_and(|p| p.load(Ordering::Relaxed) != 1) {
            return;
        }
    }
}

fn helper_loop() {
    let mut idle_since: Option<Instant> = None;
    let mut kept_closed = false;
    let mut g = lock();
    g.starting -= 1;
    loop {
        if let Some(job) = g.pick() {
            g.looking -= 1;
            let shared = g.shared;
            let start = g.pass_wake();
            drop(g);
            // WPOOL-12: before its span, so the wake goes on at once.
            if start {
                start_helpers(1);
            }
            work(job, Some(&shared.posted));
            // WPOOL-3: before the helper leaves the job.
            // SAFETY: WPOOL-2: the job is alive while this helper is on it.
            unsafe { (job.end)(job.ctx) };
            // WPOOL-3: bounds the debug trace a helper keeps.
            crate::debug::morloc_debug_flush_dispatch();
            g = lock();
            g.looking += 1;
            // WPOOL-3: the job is not read after it is left.
            if job.on.fetch_sub(1, Ordering::Relaxed) == 1 {
                shared.done.notify_all();
            }
            idle_since = None;
            kept_closed = false;
            continue;
        }
        let idle_since = *idle_since.get_or_insert_with(Instant::now);
        let idle = idle_since.elapsed();
        let grace = Duration::from_micros(LINGER_US);
        // WPOOL-10
        let keep = crate::cpus::available_cpus_cached().saturating_sub(1);
        if idle >= g.retire_after || (idle >= grace && g.helpers - g.starting > keep) {
            g.helpers -= 1;
            g.looking -= 1;
            return;
        }
        if idle >= grace && !kept_closed {
            drop(g);
            crate::ipc_ffi::close_kept_connections();
            kept_closed = true;
            g = lock();
            continue;
        }
        g.looking -= 1;
        g.sleeping += 1;
        let wait = if idle < grace { grace - idle } else { g.retire_after.saturating_sub(idle) }.max(Duration::from_micros(100));
        let shared = g.shared;
        g = g.wait_timeout(&shared.work, wait);
        g.sleeping -= 1;
        g.pending = g.pending.saturating_sub(1);
        g.looking += 1;
    }
}

unsafe fn run_inline(nspans: usize, span: SpanFn, ctx: *mut c_void, generation: u64) -> libc::c_int {
    for i in 0..nspans {
        let rc = span(ctx, i);
        // WPOOL-5
        crate::ipc_ffi::exit_if_forked_since(generation);
        if rc != 0 {
            return 1;
        }
    }
    0
}

#[cfg(test)]
static CALLER_WAITS: AtomicUsize = AtomicUsize::new(0);

#[cfg(test)]
static TIMER_START_FAILS_AFTER_MS: AtomicU64 = AtomicU64::new(0);

#[cfg(test)]
static TIMER_LOOKS: AtomicU64 = AtomicU64::new(0);

#[cfg(test)]
static NOT_DUE_ON_SIGHT: AtomicBool = AtomicBool::new(false);

fn start_timer_thread() -> std::io::Result<()> {
    #[cfg(test)]
    if let ms @ 1.. = TIMER_START_FAILS_AFTER_MS.load(Ordering::SeqCst) {
        std::thread::sleep(Duration::from_millis(ms));
        return Err(std::io::Error::other("refused by the test"));
    }
    spawn_masked("morloc-par-time", 256 << 10, timer_main)
}

// WPOOL-12: how long a job runs on its caller alone before the timer wakes
// helpers for it; 0 wakes them at the post.
static WAKE_AFTER_NS: AtomicU64 = AtomicU64::new(WAKE_AFTER_US * 1000);

const WAKE_AFTER_US: u64 = 10;

fn wake_after_ns() -> u64 {
    WAKE_AFTER_NS.load(Ordering::Relaxed)
}

// WPOOL-6: start helpers reserved under the lock; a start that fails gives
// its reservation back.
fn start_helpers(n: usize) {
    for _ in 0..n {
        if spawn_helper().is_err() {
            let mut g = lock();
            g.helpers -= 1;
            g.starting -= 1;
            g.looking -= 1;
        }
    }
}

// WPOOL-12: how long the timer keeps looking every `after` once jobs stop
// coming, so a stream of calls rarely has to wake it.
const LINGER_US: u64 = 1000;

// WPOOL-11: how often the timer reads the available CPUs again.
const REFRESH_US: u64 = 900_000;

// WPOOL-8: the timer runs no user code and keeps every signal blocked.
fn timer_main() {
    // SAFETY: WPOOL-12: asks for timed waits to end on time; no pointers.
    #[cfg(target_os = "linux")]
    unsafe {
        libc::prctl(libc::PR_SET_TIMERSLACK, 1 as libc::c_ulong)
    };
    morloc_runtime_types::panic::outside_scope(timer_loop)
}

// WPOOL-12: wakes every job still posted with spans left a delay after the
// timer first saw it (at once when a post marked it due), and, while no job
// waits, (WPOOL-11) reads the available CPUs again. Only this thread reads a
// clock for the pool.
fn timer_loop() {
    let epoch = Instant::now();
    let refresh = REFRESH_US * 1000;
    let linger = LINGER_US * 1000;
    let mut refreshed: Option<u64> = None;
    let mut linger_until = 0u64;
    let mut had_jobs = false;
    let mut g = lock();
    loop {
        #[cfg(test)]
        TIMER_LOOKS.fetch_add(1, Ordering::Relaxed);
        let now = epoch.elapsed().as_nanos() as u64 + 1;
        let after = wake_after_ns();
        let mut due: Option<&'static Job> = None;
        let mut waiting = false;
        let mut next = u64::MAX;
        for p in &g.jobs {
            // SAFETY: WPOOL-2: posted, so alive while the lock is held.
            let job: &'static Job = unsafe { &*p.0 };
            if job.woken.load(Ordering::Relaxed) || !job.left() || job.stop.load(Ordering::Relaxed) {
                continue;
            }
            waiting = true;
            let seen = job.seen.load(Ordering::Relaxed);
            if seen == SEEN_DUE || (seen != 0 && now - seen >= after) {
                due = Some(job);
                break;
            }
            if seen == 0 {
                job.seen.store(now, Ordering::Relaxed);
                next = next.min(after);
            } else {
                next = next.min(after - (now - seen));
            }
        }
        if let Some(job) = due {
            if g.wake_job(job) {
                drop(g);
                start_helpers(1);
                g = lock();
            }
            continue;
        }
        let has_jobs = !g.jobs.is_empty();
        if had_jobs && !has_jobs {
            linger_until = now + linger;
        }
        had_jobs = has_jobs;
        let lingering = !has_jobs && now < linger_until;
        if !waiting && refreshed.map_or(true, |r| now - r >= refresh) {
            drop(g);
            crate::cpus::refresh_in_background();
            refreshed = Some(epoch.elapsed().as_nanos() as u64 + 1);
            g = lock();
            continue;
        }
        let until_refresh = refresh.saturating_sub(now - refreshed.unwrap_or(now));
        let wait = if waiting {
            next.max(1)
        } else if lingering {
            after.max(1)
        } else {
            until_refresh.max(1)
        };
        g.timer_armed = waiting || lingering;
        let shared = g.shared;
        g = g.wait_timeout(&shared.timer, Duration::from_nanos(wait));
    }
}

// WPOOL-1: the caller claims from the back, where a shrinking schedule's
// spans are smallest.
fn work_caller(job: &Job) {
    loop {
        if job.stop.load(Ordering::Acquire) {
            return;
        }
        let Some(i) = job.claim_back() else { return };
        // SAFETY: WPOOL-1: the caller's span callback and context, for an
        // index below nspans.
        let rc = unsafe { (job.span)(job.ctx, i) };
        // WPOOL-5
        crate::ipc_ffi::exit_if_forked_since(job.generation);
        if rc != 0 {
            job.stop.store(true, Ordering::Release);
            return;
        }
    }
}

// WPOOL-1
pub(crate) unsafe fn run(nspans: usize, workers: usize, span: SpanFn, end: EndFn, ctx: *mut c_void) -> libc::c_int {
    let generation = generation();
    let w = workers.min(nspans);
    if w < 2 || !enabled() || nspans > u32::MAX as usize {
        return run_inline(nspans, span, ctx, generation);
    }
    let maxh = (w - 1).min(MAX_HELPERS);
    let after = wake_after_ns();
    let job: Job;
    let shared;
    let mut starts = 0;
    let mut start_timer = false;
    {
        let mut g = lock();
        g.seq += 1;
        job = Job {
            maxh,
            seq: g.seq,
            generation,
            spans: AtomicU64::new((nspans as u64) << 32),
            stop: AtomicBool::new(false),
            on: AtomicUsize::new(0),
            woken: AtomicBool::new(false),
            seen: AtomicU64::new(0),
            span,
            end,
            ctx,
        };
        g.jobs.push(Posted(&job));
        shared = g.shared;
        // WPOOL-12: the timer wakes the job; a post tells it of the job only
        // when it will not look at the jobs within the delay anyway, and then
        // the timer wakes the job as soon as it sees it.
        if after == 0 || g.timer == Timer::Failed {
            starts = usize::from(g.wake_job(&job));
        } else if g.timer == Timer::Absent {
            g.timer = Timer::Running;
            start_timer = true;
        } else if !g.timer_armed {
            g.timer_armed = true;
            #[cfg(test)]
            let due = !NOT_DUE_ON_SIGHT.load(Ordering::SeqCst);
            #[cfg(not(test))]
            let due = true;
            if due {
                job.seen.store(SEEN_DUE, Ordering::Relaxed);
            }
            shared.timer.notify_one();
        }
    }
    if start_timer && start_timer_thread().is_err() {
        // WPOOL-12: every job a missing timer would have woken.
        let mut g = lock();
        g.timer = Timer::Failed;
        g.timer_armed = false;
        starts = g.wake_waiting();
    }
    start_helpers(starts);
    work_caller(&job);
    let mut g = lock();
    let at = g.jobs.iter().position(|p| std::ptr::eq(p.0, &job));
    if let Some(i) = at {
        g.jobs.remove(i);
    }
    if job.woken.load(Ordering::Relaxed) {
        shared.posted.fetch_sub(1, Ordering::Relaxed);
    }
    while job.on.load(Ordering::Relaxed) > 0 {
        #[cfg(test)]
        CALLER_WAITS.fetch_add(1, Ordering::SeqCst);
        g = g.wait(&shared.done);
    }
    drop(g);
    libc::c_int::from(job.stop.load(Ordering::Acquire))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicI64;
    use std::sync::Mutex;

    static ONE_AT_A_TIME: Mutex<()> = Mutex::new(());

    fn serial() -> std::sync::MutexGuard<'static, ()> {
        let one = ONE_AT_A_TIME.lock().unwrap_or_else(|p| p.into_inner());
        WAKE_AFTER_NS.store(0, Ordering::Relaxed);
        FAIL_SPAWNS.store(0, Ordering::SeqCst);
        one
    }

    fn on_helper() -> bool {
        std::thread::current().name() == Some("morloc-par")
    }

    fn helpers() -> usize {
        counts().helpers
    }

    #[derive(Debug, PartialEq)]
    struct Counts {
        helpers: usize,
        starting: usize,
        pending: usize,
    }

    // WPOOL-1: read under the lock and released before any assertion, so a
    // failing test never poisons the pool the others share.
    fn counts() -> Counts {
        let g = lock();
        Counts { helpers: g.helpers, starting: g.starting, pending: g.pending }
    }

    // WPOOL-12: while this lives every post waits out the delay, however
    // the threads are scheduled; dropping it, also on a panic, restores
    // the wake on sight.
    struct DelayEveryPost;

    impl DelayEveryPost {
        fn new() -> DelayEveryPost {
            NOT_DUE_ON_SIGHT.store(true, Ordering::SeqCst);
            DelayEveryPost
        }
    }

    impl Drop for DelayEveryPost {
        fn drop(&mut self) {
            NOT_DUE_ON_SIGHT.store(false, Ordering::SeqCst);
        }
    }

    #[cfg(target_os = "linux")]
    fn helper_threads() -> usize {
        std::fs::read_dir("/proc/self/task")
            .map(|d| {
                d.filter_map(|t| std::fs::read_to_string(t.ok()?.path().join("comm")).ok())
                    .filter(|c| c.trim() == "morloc-par")
                    .count()
            })
            .unwrap_or(0)
    }

    #[cfg(target_os = "macos")]
    fn helper_threads() -> usize {
        extern "C" {
            fn mach_port_deallocate(task: libc::mach_port_t, name: libc::mach_port_t) -> libc::kern_return_t;
        }
        unsafe {
            #[allow(deprecated)]
            let task = libc::mach_task_self();
            let mut threads: libc::thread_act_array_t = std::ptr::null_mut();
            let mut n: libc::mach_msg_type_number_t = 0;
            if libc::task_threads(task, &mut threads, &mut n) != libc::KERN_SUCCESS {
                return 0;
            }
            let mut count = 0;
            for i in 0..n as usize {
                let port = *threads.add(i);
                let t = libc::pthread_from_mach_thread_np(port);
                let mut name = [0 as libc::c_char; 64];
                if !t.is_null()
                    && libc::pthread_getname_np(t, name.as_mut_ptr(), name.len()) == 0
                    && std::ffi::CStr::from_ptr(name.as_ptr()).to_bytes() == b"morloc-par"
                {
                    count += 1;
                }
                mach_port_deallocate(task, port);
            }
            libc::vm_deallocate(
                task,
                threads as libc::vm_address_t,
                n as libc::vm_size_t * std::mem::size_of::<libc::thread_act_t>() as libc::vm_size_t,
            );
            count
        }
    }

    fn wait_until(what: &str, mut cond: impl FnMut() -> bool) {
        let until = Instant::now() + Duration::from_secs(60);
        while !cond() {
            assert!(Instant::now() < until, "timed out waiting until {what}");
            std::thread::sleep(Duration::from_millis(2));
        }
    }

    fn drain_helpers() {
        lock().retire_after = Duration::ZERO;
        {
            let g = lock();
            g.shared.work.notify_all();
        }
        wait_until("every helper has retired", || helpers() == 0);
        lock().retire_after = Duration::from_secs(60);
    }

    struct Ctx {
        ran: Vec<AtomicUsize>,
        helper_spans: AtomicUsize,
        ends: AtomicUsize,
        fail_at: Option<usize>,
        sleep_us: u64,
        caller_waits_for_helper: bool,
        nested: usize,
        log: Mutex<Vec<(std::thread::ThreadId, &'static str)>>,
        caller_returned: AtomicBool,
        hook_saw_return: AtomicBool,
        hook_sleep_ms: u64,
        grow_to: usize,
        grew: AtomicBool,
        most_started: AtomicUsize,
        helper_ids: Mutex<std::collections::HashSet<std::thread::ThreadId>>,
    }

    impl Ctx {
        fn new(n: usize) -> Ctx {
            Ctx {
                ran: (0..n).map(|_| AtomicUsize::new(0)).collect(),
                helper_spans: AtomicUsize::new(0),
                ends: AtomicUsize::new(0),
                fail_at: None,
                sleep_us: 0,
                caller_waits_for_helper: false,
                nested: 0,
                log: Mutex::new(Vec::new()),
                caller_returned: AtomicBool::new(false),
                hook_saw_return: AtomicBool::new(false),
                hook_sleep_ms: 0,
                grow_to: 0,
                grew: AtomicBool::new(false),
                most_started: AtomicUsize::new(0),
                helper_ids: Mutex::new(std::collections::HashSet::new()),
            }
        }
    }

    unsafe extern "C" fn span(ctx: *mut c_void, i: usize) -> libc::c_int {
        let c = &*(ctx as *const Ctx);
        c.ran[i].fetch_add(1, Ordering::SeqCst);
        c.log.lock().unwrap().push((std::thread::current().id(), "span"));
        if on_helper() {
            c.helper_spans.fetch_add(1, Ordering::SeqCst);
            c.helper_ids.lock().unwrap().insert(std::thread::current().id());
        } else if c.caller_waits_for_helper {
            let until = Instant::now() + Duration::from_secs(5);
            while c.helper_spans.load(Ordering::SeqCst) == 0 && Instant::now() < until {
                std::thread::sleep(Duration::from_millis(1));
            }
        }
        if c.grow_to > 0 {
            let until = Instant::now() + Duration::from_secs(60);
            loop {
                if !on_helper() {
                    c.most_started.fetch_max(counts().helpers, Ordering::SeqCst);
                }
                let grown = c.helper_spans.load(Ordering::SeqCst) >= c.grow_to;
                if grown {
                    c.grew.store(true, Ordering::SeqCst);
                }
                if grown || Instant::now() >= until {
                    break;
                }
                std::thread::sleep(Duration::from_millis(5));
            }
        }
        if c.sleep_us > 0 {
            std::thread::sleep(Duration::from_micros(c.sleep_us));
        }
        if c.nested > 0 {
            let inner = Ctx::new(c.nested);
            if run(c.nested, 3, span, end, &inner as *const Ctx as *mut c_void) != 0 {
                return 1;
            }
            if inner.ran.iter().any(|r| r.load(Ordering::SeqCst) != 1) {
                return 1;
            }
        }
        libc::c_int::from(c.fail_at == Some(i))
    }

    unsafe extern "C" fn end(ctx: *mut c_void) {
        let c = &*(ctx as *const Ctx);
        if c.hook_sleep_ms > 0 {
            std::thread::sleep(Duration::from_millis(c.hook_sleep_ms));
        }
        if c.caller_returned.load(Ordering::SeqCst) {
            c.hook_saw_return.store(true, Ordering::SeqCst);
        }
        c.ends.fetch_add(1, Ordering::SeqCst);
        c.log.lock().unwrap().push((std::thread::current().id(), "end"));
    }

    fn go(c: &Ctx, workers: usize) -> libc::c_int {
        unsafe { run(c.ran.len(), workers, span, end, c as *const Ctx as *mut c_void) }
    }

    #[test]
    fn every_span_runs_once_on_the_caller_and_helpers() {
        let _one = serial();
        for (n, w) in [(1usize, 4usize), (2, 2), (7, 3), (100, 8), (1000, 12)] {
            let mut c = Ctx::new(n);
            c.caller_waits_for_helper = n > 1;
            assert_eq!(go(&c, w), 0);
            assert!(c.ran.iter().all(|r| r.load(Ordering::SeqCst) == 1), "n={n} w={w}");
            if n > 1 {
                assert!(c.helper_spans.load(Ordering::SeqCst) > 0, "no helper ran a span: n={n} w={w}");
            }
        }
        let c = Ctx::new(5);
        assert_eq!(go(&c, 1), 0);
        assert_eq!(c.helper_spans.load(Ordering::SeqCst), 0);
    }

    #[test]
    fn a_failed_span_stops_further_claims() {
        let _one = serial();
        let mut c = Ctx::new(400);
        c.fail_at = Some(3);
        c.sleep_us = 200;
        assert_eq!(go(&c, 4), 1);
        let runs: Vec<usize> = c.ran.iter().map(|r| r.load(Ordering::SeqCst)).collect();
        assert!(runs.iter().all(|&r| r <= 1));
        assert_eq!(runs[3], 1);
        assert!(runs.iter().sum::<usize>() < 400, "every span ran after the failure");
    }

    #[test]
    fn a_caller_whose_helpers_never_picked_returns_without_waiting() {
        let _one = serial();
        let warm = Ctx::new(64);
        assert_eq!(go(&warm, 4), 0);
        for _ in 0..2000 {
            let c = Ctx::new(2);
            let before = CALLER_WAITS.load(Ordering::SeqCst);
            assert_eq!(go(&c, 4), 0);
            let waited = CALLER_WAITS.load(Ordering::SeqCst) - before;
            if c.ends.load(Ordering::SeqCst) == 0 {
                assert_eq!(waited, 0, "the caller waited for helpers that never picked its job");
            }
        }
    }

    #[test]
    fn nested_jobs_from_every_helper_complete() {
        let _one = serial();
        let mut c = Ctx::new(24);
        c.nested = 9;
        c.caller_waits_for_helper = true;
        assert_eq!(go(&c, 4), 0);
        assert!(c.ran.iter().all(|r| r.load(Ordering::SeqCst) == 1));
        unsafe extern "C" fn fails_nested(ctx: *mut c_void, i: usize) -> libc::c_int {
            let c = &*(ctx as *const Ctx);
            c.ran[i].fetch_add(1, Ordering::SeqCst);
            let mut inner = Ctx::new(5);
            inner.fail_at = Some(2);
            run(5, 3, span, end, &inner as *const Ctx as *mut c_void)
        }
        let c = Ctx::new(6);
        let rc = unsafe { run(6, 3, fails_nested, end, &c as *const Ctx as *mut c_void) };
        assert_eq!(rc, 1);
        assert!(c.ran.iter().all(|r| r.load(Ordering::SeqCst) <= 1));
    }

    #[test]
    fn the_hook_runs_before_the_helper_leaves_the_job() {
        let _one = serial();
        let mut c = Ctx::new(8);
        c.caller_waits_for_helper = true;
        c.hook_sleep_ms = 50;
        assert_eq!(go(&c, 3), 0);
        c.caller_returned.store(true, Ordering::SeqCst);
        assert!(c.ends.load(Ordering::SeqCst) > 0);
        let ends = c.ends.load(Ordering::SeqCst);
        std::thread::sleep(Duration::from_millis(120));
        assert_eq!(c.ends.load(Ordering::SeqCst), ends, "a hook ran after the caller returned");
        assert!(!c.hook_saw_return.load(Ordering::SeqCst));
    }

    #[test]
    fn a_helper_runs_the_hook_after_each_pick() {
        let _one = serial();
        let mut c = Ctx::new(64);
        c.caller_waits_for_helper = true;
        c.sleep_us = 100;
        assert_eq!(go(&c, 4), 0);
        let log = c.log.lock().unwrap();
        let caller = std::thread::current().id();
        let mut last: std::collections::HashMap<std::thread::ThreadId, &str> = Default::default();
        for (t, what) in log.iter().filter(|(t, _)| *t != caller) {
            last.insert(*t, what);
        }
        assert!(!last.is_empty());
        assert!(last.values().all(|w| *w == "end"), "a helper left a job without running its hook");
        assert!(log.iter().filter(|(t, w)| *t == caller && *w == "end").count() == 0, "the hook ran on the caller");
    }

    fn wait_child(pid: libc::pid_t) -> i32 {
        let until = Instant::now() + Duration::from_secs(20);
        loop {
            let mut status = 0;
            let r = unsafe { libc::waitpid(pid, &mut status, libc::WNOHANG) };
            if r == pid {
                return status;
            }
            if Instant::now() > until {
                unsafe {
                    libc::kill(pid, libc::SIGKILL);
                    libc::waitpid(pid, &mut status, 0);
                }
                panic!("the forked child hung");
            }
            std::thread::sleep(Duration::from_millis(5));
        }
    }

    #[test]
    fn a_forked_child_gets_a_fresh_pool() {
        let _one = serial();
        let warm = Ctx::new(16);
        assert_eq!(go(&warm, 4), 0);
        let (held_tx, held_rx) = std::sync::mpsc::channel();
        let (go_tx, go_rx) = std::sync::mpsc::channel::<()>();
        let holder = std::thread::spawn(move || {
            let g = lock();
            held_tx.send(()).unwrap();
            go_rx.recv().unwrap();
            drop(g);
        });
        held_rx.recv().unwrap();
        let pid = unsafe { libc::fork() };
        if pid == 0 {
            let c = Ctx::new(32);
            let rc = go(&c, 4);
            let ok = rc == 0 && c.ran.iter().all(|r| r.load(Ordering::SeqCst) == 1);
            unsafe { libc::_exit(if ok { 0 } else { 3 }) };
        }
        let status = wait_child(pid);
        go_tx.send(()).unwrap();
        holder.join().unwrap();
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "child status {status}");
    }

    static CHILD: AtomicI64 = AtomicI64::new(0);

    unsafe extern "C" fn forks_on_a_helper(ctx: *mut c_void, i: usize) -> libc::c_int {
        let c = &*(ctx as *const Ctx);
        c.ran[i].fetch_add(1, Ordering::SeqCst);
        if on_helper() {
            if c.helper_spans.fetch_add(1, Ordering::SeqCst) == 0 {
                let pid = libc::fork();
                if pid == 0 {
                    return 0;
                }
                CHILD.store(pid as i64, Ordering::SeqCst);
            }
        } else {
            let until = Instant::now() + Duration::from_secs(5);
            while c.helper_spans.load(Ordering::SeqCst) == 0 && Instant::now() < until {
                std::thread::sleep(Duration::from_millis(1));
            }
        }
        0
    }

    #[test]
    fn a_span_forking_on_a_helper_exits_in_the_child() {
        let _one = serial();
        let c = Ctx::new(8);
        let parent = unsafe { libc::getpid() };
        let rc = unsafe { run(8, 4, forks_on_a_helper, end, &c as *const Ctx as *mut c_void) };
        assert_eq!(unsafe { libc::getpid() }, parent, "a forked child returned from the call");
        assert_eq!(rc, 0);
        assert!(c.ran.iter().all(|r| r.load(Ordering::SeqCst) == 1));
        let pid = CHILD.swap(0, Ordering::SeqCst) as libc::pid_t;
        assert!(pid > 0, "no span ran on a helper");
        let status = wait_child(pid);
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 1, "child status {status}");
    }

    #[test]
    fn the_helpers_stay_under_the_ceiling() {
        let _one = serial();
        for _ in 0..3 {
            let mut c = Ctx::new(400);
            c.grow_to = MAX_HELPERS;
            let c = std::sync::Arc::new(c);
            let c2 = std::sync::Arc::clone(&c);
            let running = std::thread::spawn(move || go(&c2, 400));
            let mut most = 0;
            while !running.is_finished() {
                most = most.max(helper_threads());
                std::thread::sleep(Duration::from_millis(2));
            }
            assert_eq!(running.join().unwrap(), 0);
            let counted = c.most_started.load(Ordering::SeqCst);
            assert!(c.grew.load(Ordering::SeqCst), "the pool did not reach the ceiling: {counted} helpers");
            assert!(counted <= MAX_HELPERS && most <= MAX_HELPERS, "{counted} counted, {most} threads: past the ceiling");
        }
        let keep = crate::cpus::available_cpus().saturating_sub(1);
        wait_until("the helpers past the CPUs have retired", || helpers() <= keep);
    }

    #[test]
    fn a_post_wakes_sleeping_helpers() {
        let _one = serial();
        let warm = Ctx::new(16);
        assert_eq!(go(&warm, 4), 0);
        wait_until("the helpers sleep", || {
            let g = lock();
            g.sleeping > 0 && g.looking == 0
        });
        let mut c = Ctx::new(8);
        c.caller_waits_for_helper = true;
        assert_eq!(go(&c, 4), 0);
        assert!(c.helper_spans.load(Ordering::SeqCst) > 0, "no sleeping helper was woken for the job");
    }

    #[test]
    fn a_failed_start_gives_its_reservation_back() {
        let _one = serial();
        drain_helpers();
        FAIL_SPAWNS.store(1, Ordering::SeqCst);
        let c = Ctx::new(16);
        assert_eq!(go(&c, 4), 0, "a call whose helper failed to start runs on its caller");
        assert_eq!(FAIL_SPAWNS.load(Ordering::SeqCst), 0);
        assert!(c.ran.iter().all(|r| r.load(Ordering::SeqCst) == 1));
        let now = counts();
        assert_eq!((now.helpers, now.starting), (0, 0), "the failed start kept its reservation");
        let c = Ctx::new(16);
        assert_eq!(go(&c, 4), 0);
        wait_until("a helper started for the next call runs", || helper_threads() >= 1);
    }

    #[test]
    fn two_callers_share_the_helpers_in_turn() {
        let _one = serial();
        drain_helpers();
        let mut big = Ctx::new(300);
        big.sleep_us = 2000;
        let big = std::sync::Arc::new(big);
        let b2 = std::sync::Arc::clone(&big);
        let t = std::thread::spawn(move || unsafe { run(300, 4, span, end, &*b2 as *const Ctx as *mut c_void) });
        wait_until("the big job is running", || big.helper_spans.load(Ordering::SeqCst) >= 3);
        let mut small = Ctx::new(12);
        small.sleep_us = 2000;
        small.caller_waits_for_helper = true;
        assert_eq!(go(&small, 4), 0);
        let left = big.ran.iter().filter(|r| r.load(Ordering::SeqCst) == 0).count();
        assert!(small.helper_spans.load(Ordering::SeqCst) > 0, "no helper served the second job");
        assert!(left > 0, "the big job finished first; the test proves nothing");
        assert_eq!(t.join().unwrap(), 0);
    }

    fn sigblk_of_this_thread() -> String {
        unsafe {
            let mut m: libc::sigset_t = std::mem::zeroed();
            assert_eq!(libc::pthread_sigmask(libc::SIG_BLOCK, std::ptr::null(), &mut m), 0);
            (1..32).map(|s| if libc::sigismember(&m, s) == 1 { '1' } else { '0' }).collect()
        }
    }

    static HELPER_SIGBLK: Mutex<Option<String>> = Mutex::new(None);

    unsafe extern "C" fn reads_mask(ctx: *mut c_void, i: usize) -> libc::c_int {
        let c = &*(ctx as *const Ctx);
        c.ran[i].fetch_add(1, Ordering::SeqCst);
        if on_helper() {
            *HELPER_SIGBLK.lock().unwrap() = Some(sigblk_of_this_thread());
            c.helper_spans.fetch_add(1, Ordering::SeqCst);
        } else {
            let until = Instant::now() + Duration::from_secs(5);
            while c.helper_spans.load(Ordering::SeqCst) == 0 && Instant::now() < until {
                std::thread::sleep(Duration::from_millis(1));
            }
        }
        0
    }

    #[test]
    fn a_helper_gets_the_recorded_signal_mask() {
        let _one = serial();
        let expected = std::thread::spawn(|| {
            let m = user_mask();
            unsafe { libc::pthread_sigmask(libc::SIG_SETMASK, &m, std::ptr::null_mut()) };
            sigblk_of_this_thread()
        })
        .join()
        .unwrap();
        drain_helpers();
        let seen = std::thread::spawn(|| unsafe {
            let mut all: libc::sigset_t = std::mem::zeroed();
            libc::sigfillset(&mut all);
            libc::pthread_sigmask(libc::SIG_SETMASK, &all, std::ptr::null_mut());
            let c = Ctx::new(4);
            assert_eq!(run(4, 2, reads_mask, end, &c as *const Ctx as *mut c_void), 0);
            assert!(c.helper_spans.load(Ordering::SeqCst) > 0);
            HELPER_SIGBLK.lock().unwrap().take().unwrap()
        })
        .join()
        .unwrap();
        assert_eq!(seen, expected);
    }

    #[test]
    fn a_program_exits_with_idle_helpers() {
        let _one = serial();
        let pid = unsafe { libc::fork() };
        if pid == 0 {
            let c = Ctx::new(16);
            let rc = go(&c, 4);
            std::thread::spawn(|| {
                let c = Ctx::new(4);
                unsafe extern "C" fn blocks(_: *mut c_void, _: usize) -> libc::c_int {
                    loop {
                        std::thread::sleep(Duration::from_secs(1));
                    }
                }
                unsafe { run(4, 4, blocks, end, &c as *const Ctx as *mut c_void) };
            });
            std::thread::sleep(Duration::from_millis(50));
            unsafe { libc::exit(if rc == 0 { 0 } else { 3 }) };
        }
        let status = wait_child(pid);
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "child status {status}");
    }

    #[test]
    fn helpers_past_the_available_cpus_retire_when_idle() {
        let _one = serial();
        let keep = crate::cpus::available_cpus().saturating_sub(1);
        let n = (keep + 12).min(MAX_HELPERS + 1);
        let mut c = Ctx::new(n);
        c.grow_to = keep + 1;
        assert_eq!(go(&c, n), 0);
        assert!(c.grew.load(Ordering::SeqCst), "the pool never grew past the {keep} kept helpers");
        wait_until("the helpers past the CPUs have retired", || helpers() <= keep);
        assert!(helpers() > 0, "the helpers under the CPUs retired too");
    }

    fn delay_us(us: u64) {
        WAKE_AFTER_NS.store(us.saturating_mul(1000), Ordering::Relaxed);
        lock().shared.timer.notify_one();
    }

    #[test]
    fn a_call_its_caller_finishes_first_wakes_no_helper() {
        let _one = serial();
        let warm = Ctx::new(16);
        assert_eq!(go(&warm, 4), 0);
        wait_until("the helpers sleep", || {
            let g = lock();
            g.sleeping > 0 && g.looking == 0 && g.pending == 0
        });
        delay_us(1_000_000);
        let armed = DelayEveryPost::new();
        let before = (counts(), CALLER_WAITS.load(Ordering::SeqCst));
        for _ in 0..200 {
            let c = Ctx::new(8);
            assert_eq!(go(&c, 4), 0);
            assert_eq!(c.helper_spans.load(Ordering::SeqCst), 0, "a helper ran a span of an unwoken job");
        }
        let after = (counts(), CALLER_WAITS.load(Ordering::SeqCst));
        drop(armed);
        assert_eq!(after, before);
    }

    #[test]
    fn a_job_still_running_after_the_delay_gets_helpers() {
        let _one = serial();
        delay_us(10);
        let mut c = Ctx::new(32);
        c.sleep_us = 500;
        assert_eq!(go(&c, 4), 0);
        assert!(c.ran.iter().all(|r| r.load(Ordering::SeqCst) == 1));
        assert!(c.helper_spans.load(Ordering::SeqCst) > 0, "no helper was woken after the delay");
    }

    #[repr(C)]
    struct Nesting {
        outer: Ctx,
        inner: Ctx,
        nested: AtomicBool,
        waited_out: AtomicBool,
    }

    unsafe extern "C" fn waits_in_a_nested_call(ctx: *mut c_void, i: usize) -> libc::c_int {
        let n = &*(ctx as *const Nesting);
        n.outer.ran[i].fetch_add(1, Ordering::SeqCst);
        if on_helper() {
            n.outer.helper_spans.fetch_add(1, Ordering::SeqCst);
            return 0;
        }
        if !n.nested.swap(true, Ordering::SeqCst) {
            run(4, 2, waits_for_the_outer_helpers, end, ctx);
        }
        0
    }

    unsafe extern "C" fn waits_for_the_outer_helpers(ctx: *mut c_void, i: usize) -> libc::c_int {
        let n = &*(ctx as *const Nesting);
        n.inner.ran[i].fetch_add(1, Ordering::SeqCst);
        if !on_helper() {
            let until = Instant::now() + Duration::from_secs(5);
            while n.outer.helper_spans.load(Ordering::SeqCst) == 0 && Instant::now() < until {
                std::thread::sleep(Duration::from_millis(1));
            }
            if n.outer.helper_spans.load(Ordering::SeqCst) == 0 {
                n.waited_out.store(true, Ordering::SeqCst);
            }
        }
        0
    }

    #[test]
    fn a_caller_in_a_nested_call_still_gets_helpers() {
        let _one = serial();
        let warm = Ctx::new(16);
        assert_eq!(go(&warm, 4), 0);
        wait_until("the helpers sleep", || {
            let g = lock();
            g.sleeping > 0 && g.looking == 0
        });
        delay_us(10);
        let n = Nesting { outer: Ctx::new(6), inner: Ctx::new(4), nested: AtomicBool::new(false), waited_out: AtomicBool::new(false) };
        let rc = unsafe { run(6, 4, waits_in_a_nested_call, end, &n as *const Nesting as *mut c_void) };
        assert_eq!(rc, 0);
        assert!(n.outer.ran.iter().all(|r| r.load(Ordering::SeqCst) == 1));
        assert!(!n.waited_out.load(Ordering::SeqCst), "the outer job got no helper while its caller waited in a nested call");
    }

    struct Pair {
        started: AtomicUsize,
        waited_out: AtomicBool,
        first_end: Mutex<Option<Instant>>,
        helper_start: Mutex<Option<Instant>>,
    }

    unsafe extern "C" fn waits_for_its_pair(ctx: *mut c_void, _: usize) -> libc::c_int {
        let p = &*(ctx as *const Pair);
        p.started.fetch_add(1, Ordering::SeqCst);
        let until = Instant::now() + Duration::from_secs(5);
        while p.started.load(Ordering::SeqCst) < 2 && Instant::now() < until {
            std::thread::sleep(Duration::from_micros(100));
        }
        if p.started.load(Ordering::SeqCst) < 2 {
            p.waited_out.store(true, Ordering::SeqCst);
        }
        0
    }

    unsafe extern "C" fn no_end(_: *mut c_void) {}

    #[test]
    fn spans_waiting_for_each_other_get_a_helper() {
        let _one = serial();
        delay_us(10);
        for _ in 0..20 {
            let p = Pair { started: AtomicUsize::new(0), waited_out: AtomicBool::new(false), first_end: Mutex::new(None), helper_start: Mutex::new(None) };
            assert_eq!(unsafe { run(2, 2, waits_for_its_pair, no_end, &p as *const Pair as *mut c_void) }, 0);
            assert!(!p.waited_out.load(Ordering::SeqCst), "a span waited for a partner no helper started");
        }
    }

    unsafe extern "C" fn one_long_span(ctx: *mut c_void, _: usize) -> libc::c_int {
        let p = &*(ctx as *const Pair);
        if on_helper() {
            p.helper_start.lock().unwrap().get_or_insert_with(Instant::now);
        }
        std::thread::sleep(Duration::from_millis(if on_helper() { 100 } else { 500 }));
        if !on_helper() {
            p.first_end.lock().unwrap().get_or_insert_with(Instant::now);
        }
        0
    }

    #[test]
    fn a_long_first_span_does_not_hold_back_the_helpers() {
        let _one = serial();
        delay_us(10);
        let warm = Ctx::new(2);
        assert_eq!(go(&warm, 2), 0);
        std::thread::sleep(Duration::from_millis(20));
        let p = Pair { started: AtomicUsize::new(0), waited_out: AtomicBool::new(false), first_end: Mutex::new(None), helper_start: Mutex::new(None) };
        assert_eq!(unsafe { run(4, 4, one_long_span, no_end, &p as *const Pair as *mut c_void) }, 0);
        let helper = p.helper_start.lock().unwrap().expect("no helper ran a span");
        let first = p.first_end.lock().unwrap().expect("the caller ran no span");
        assert!(helper < first, "no helper started before the caller's first span ended");
    }

    #[test]
    fn a_helper_awake_on_another_job_does_not_take_an_unwoken_job() {
        let _one = serial();
        let warm = Ctx::new(16);
        assert_eq!(go(&warm, 4), 0);
        delay_us(1_000_000);
        let armed = DelayEveryPost::new();
        let mut c = Ctx::new(8);
        c.sleep_us = 2000;
        assert_eq!(go(&c, 4), 0);
        drop(armed);
        assert_eq!(c.helper_spans.load(Ordering::SeqCst), 0, "a helper took a job before it was woken");
    }

    #[test]
    fn a_failed_timer_start_wakes_every_waiting_job() {
        let _one = serial();
        // SAFETY: WPOOL-5: the child runs pool calls in a fresh pool and _exits.
        let pid = unsafe { libc::fork() };
        if pid == 0 {
            delay_us(10);
            TIMER_START_FAILS_AFTER_MS.store(50, Ordering::SeqCst);
            let first = std::thread::spawn(|| {
                let mut c = Ctx::new(4);
                c.sleep_us = 100_000;
                go(&c, 2)
            });
            std::thread::sleep(Duration::from_millis(10));
            let p = Pair { started: AtomicUsize::new(0), waited_out: AtomicBool::new(false), first_end: Mutex::new(None), helper_start: Mutex::new(None) };
            let rc = unsafe { run(2, 2, waits_for_its_pair, no_end, &p as *const Pair as *mut c_void) };
            let ok = rc == 0 && !p.waited_out.load(Ordering::SeqCst) && first.join().unwrap() == 0;
            unsafe { libc::_exit(if ok { 0 } else { 3 }) };
        }
        let status = wait_child(pid);
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "child status {status}");
    }

    #[test]
    fn a_woken_job_gets_every_helper_it_may_hold() {
        let _one = serial();
        let held = crate::cpus::available_cpus().saturating_sub(1).min(3);
        assert!(held >= 2, "a wake of every helper needs at least 3 CPUs, so 2 helpers stay when idle");
        let mut warm = Ctx::new(16);
        warm.grow_to = held;
        assert_eq!(go(&warm, 4), 0);
        wait_until("the helpers sleep", || {
            let g = lock();
            g.sleeping >= held && g.looking == 0
        });
        delay_us(10);
        let mut c = Ctx::new(24);
        c.grow_to = held;
        assert_eq!(go(&c, 4), 0);
        let reached = c.helper_ids.lock().unwrap().len();
        assert!(c.grew.load(Ordering::SeqCst), "the wake reached {reached} of the {held} helpers the job may hold");
        assert_eq!(reached, held);
    }

    #[test]
    fn the_timer_sleeps_while_no_job_waits() {
        let _one = serial();
        delay_us(10);
        let mut c = Ctx::new(8);
        c.sleep_us = 50_000;
        let before = TIMER_LOOKS.load(Ordering::Relaxed);
        assert_eq!(go(&c, 8), 0);
        let looks = TIMER_LOOKS.load(Ordering::Relaxed) - before;
        assert!(c.helper_spans.load(Ordering::SeqCst) > 0);
        assert!(looks < 50, "the timer looked {looks} times during a 100 ms call whose job was woken");
    }

    unsafe extern "C" fn keeps_a_connection(ctx: *mut c_void, i: usize) -> libc::c_int {
        let c = &*(ctx as *const Ctx);
        c.ran[i].fetch_add(1, Ordering::SeqCst);
        if on_helper() && c.helper_spans.fetch_add(1, Ordering::SeqCst) == 0 {
            let fd = KEPT_FD.load(Ordering::SeqCst) as i32;
            assert!(crate::ipc_ffi::keep_connection_for_test(b"/kept-by-a-helper", fd));
        } else if !on_helper() {
            let until = Instant::now() + Duration::from_secs(5);
            while c.helper_spans.load(Ordering::SeqCst) == 0 && Instant::now() < until {
                std::thread::sleep(Duration::from_millis(1));
            }
        }
        0
    }

    static KEPT_FD: AtomicI64 = AtomicI64::new(-1);

    #[test]
    fn an_idle_helper_closes_its_kept_connections() {
        let _one = serial();
        let mut fds = [-1i32; 2];
        assert_eq!(unsafe { morloc_runtime_types::fd::socketpair(libc::AF_UNIX, libc::SOCK_STREAM, 0, fds.as_mut_ptr()) }, 0);
        KEPT_FD.store(fds[0] as i64, Ordering::SeqCst);
        let c = Ctx::new(4);
        assert_eq!(unsafe { run(4, 2, keeps_a_connection, end, &c as *const Ctx as *mut c_void) }, 0);
        assert!(c.helper_spans.load(Ordering::SeqCst) > 0);
        let mut pfd = libc::pollfd { fd: fds[1], events: libc::POLLIN, revents: 0 };
        let ready = unsafe { libc::poll(&mut pfd, 1, 5000) };
        let mut b = 0u8;
        let n = unsafe { libc::read(fds[1], &mut b as *mut u8 as *mut c_void, 1) };
        unsafe { libc::close(fds[1]) };
        assert!(ready == 1 && n == 0, "the idle helper kept its connection open");
    }
}

mod c_abi {
    use super::*;

    // WPOOL-1
    #[no_mangle]
    pub unsafe extern "C" fn morloc_par_run(
        nspans: usize,
        workers: usize,
        span: SpanFn,
        end: EndFn,
        job: *mut c_void,
    ) -> libc::c_int {
        super::run(nspans, workers, span, end, job)
    }

    // WPOOL-1
    #[no_mangle]
    pub extern "C" fn morloc_par_pool_enabled() -> libc::c_int {
        libc::c_int::from(super::enabled())
    }
}
