//! Program-wide live and peak byte counts for the SHM allocator.
//!
//! Enabled by `MORLOC_SHM_STATS=<file>`. Every process of the program adds
//! a block's size when it claims the block and subtracts it when the last
//! reference is dropped, on two counters held in a companion segment that
//! all processes map. A block allocated in one process is often released in
//! another, so only a shared count means anything. At exit the program's
//! owner (the process that created the primary volume) writes the peak to
//! `<file>`.
//!
//! Disabled, the cost on the allocation path is one atomic pointer load.

use std::io::Write;
use std::sync::atomic::{AtomicI64, AtomicPtr, Ordering};

use crate::error::MorlocError;
use crate::shm;
use crate::shm_companion::{CompanionSegment, SweepPolicy};

pub const SHM_STATS_ENV: &str = "MORLOC_SHM_STATS";

#[repr(C)]
struct Counters {
    live: AtomicI64,
    peak: AtomicI64,
}

static COUNTERS: AtomicPtr<Counters> = AtomicPtr::new(std::ptr::null_mut());
pub(crate) static SEGMENT: crate::fork_policy::Held<Option<CompanionSegment>> = crate::fork_policy::Held::new(8, None);

/// Map the counters if `MORLOC_SHM_STATS` is set. Called once the program's
/// basename is known; later calls are no-ops.
pub(crate) fn init() -> Result<(), MorlocError> {
    if std::env::var_os(SHM_STATS_ENV).is_none() {
        return Ok(());
    }
    if SEGMENT.lock().is_some() {
        return Ok(());
    }
    // INIT-2: opened without the lock; a fresh segment is zero-filled,
    // which is the counters' initial state.
    let seg = CompanionSegment::open(
        "stats",
        std::mem::size_of::<Counters>(),
        SweepPolicy::SweepOnCrash,
    )?;
    let mut seg = seg;
    let mut seg_slot = SEGMENT.lock();
    if seg_slot.is_some() {
        drop(seg_slot);
        seg.detach();
        return Ok(());
    }
    if let Err(e) = seg.register_for_sweep() {
        drop(seg_slot);
        seg.detach();
        return Err(e);
    }
    COUNTERS.store(seg.base as *mut Counters, Ordering::Release);
    *seg_slot = Some(seg);
    drop(seg_slot);
    shm::register_shclose_hook(teardown);
    Ok(())
}

#[inline]
fn counters() -> Option<&'static Counters> {
    let p = COUNTERS.load(Ordering::Acquire);
    // SAFETY: non-null only while SEGMENT holds the mapping; teardown clears
    // the pointer before unmapping.
    unsafe { p.as_ref() }
}

/// A block of `size` bytes was claimed.
#[inline]
pub(crate) fn on_claim(size: usize) {
    if let Some(c) = counters() {
        let live = c.live.fetch_add(size as i64, Ordering::AcqRel) + size as i64;
        c.peak.fetch_max(live, Ordering::AcqRel);
    }
}

/// The last reference to a block of `size` bytes was dropped.
#[inline]
pub(crate) fn on_release(size: usize) {
    if let Some(c) = counters() {
        c.live.fetch_sub(size as i64, Ordering::AcqRel);
    }
}

/// Current (live, peak) byte counts, if enabled.
pub fn snapshot() -> Option<(i64, i64)> {
    counters().map(|c| (c.live.load(Ordering::Acquire), c.peak.load(Ordering::Acquire)))
}

/// Bytes of shared memory the whole program holds now, or -1 when
/// `MORLOC_SHM_STATS` is unset.
#[no_mangle]
pub extern "C" fn morloc_shm_live_bytes() -> i64 {
    snapshot().map_or(-1, |(live, _)| live)
}

/// Only the owner writes the report and removes the segment. Any other
/// process keeps its mapping until it exits: its worker threads may still be
/// allocating while exit hooks run.
fn teardown() {
    if !shm::owns_program() {
        return;
    }
    if let (Some((_, peak)), Some(path)) = (snapshot(), std::env::var_os(SHM_STATS_ENV)) {
        if let Ok(mut f) = std::fs::File::create(path) {
            let _ = writeln!(f, "{}", peak);
        }
    }
    // FORK-10: the names are removed outside the lock.
    let name = SEGMENT.lock().as_mut().map(|seg| seg.forget_for_unlink());
    if let Some(name) = name {
        crate::shm_companion::remove_names(&name);
    }
}
