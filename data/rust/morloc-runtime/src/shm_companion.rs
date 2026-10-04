//! SHM companion segments: fixed-size shared mappings kept outside
//! the general allocator's `-<idx>` namespace.
//!
//! The general SHM allocator (`shm::shinit` + `find_free_block`) owns
//! volumes named `<basename>-<idx>`. Subsystems that want a fixed
//! shared region (the stream registry, a future trace buffer, a
//! cross-pool `@save` index, ...) name their files
//! `<basename>.<suffix>` and open them through this module. The
//! benefits are:
//!
//! * The allocator never scans a companion for free blocks (silent
//!   corruption of the RegistryHeader that motivated this refactor).
//! * There is no `pick_free_slot` collision risk: companions don't
//!   occupy any `volume_index` slot in the allocator's namespace.
//! * Signal-safe SIGTERM cleanup is uniform: a
//!   `MORLOC_COMPANION_NAMES` array is scanned by the nexus's SIGTERM
//!   handler after the `0..MAX_VOLUME_NUMBER` allocator sweep.
//! * Normal-exit cleanup is uniform: register a
//!   `shm::ShcloseHook` and every existing exit path picks up the
//!   companion teardown for free.
//!
//! Callers should hand off segment ownership to a static (via
//! `mem::forget`) if the companion lives for the process lifetime,
//! or rely on `Drop` for scoped usage.

use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::path::PathBuf;
use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

use crate::error::MorlocError;
use crate::shm;

/// Whether the companion should be `shm_unlink`ed on abnormal exit
/// (SIGTERM / SIGINT / panic). Most companions are `SweepOnCrash`;
/// `Persist` is for authoritative state that a next-nexus recovery is
/// expected to pick up (e.g. an `@save` index).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SweepPolicy {
    SweepOnCrash,
    Persist,
}

/// A shared memory segment held outside the general allocator's
/// `-<idx>` namespace. RAII: `Drop` runs `teardown`.
///
/// Callers that hand the segment off to a static must
/// `mem::forget(seg)` and later call a manual teardown; see
/// `stream::registry_teardown` for an example.
pub struct CompanionSegment {
    pub base:  *mut u8,
    pub size:  usize,
    name:      CString,
    policy:    SweepPolicy,
    swept:     bool,
}

// SAFETY: CompanionSegment holds a raw pointer to an mmap'd region and
// an owned name. The mmap survives across threads; readers/writers of
// the memory synchronise via their own protocol (magic gates, futexes,
// etc.). Marking Send/Sync so callers can put the segment into a
// `Mutex<Option<CompanionSegment>>` or similar.
unsafe impl Send for CompanionSegment {}
unsafe impl Sync for CompanionSegment {}

impl CompanionSegment {
    /// Open (or attach to) `<basename>.<suffix>`. The first caller in a
    /// session creates it, exclusively; later callers attach. An attacher
    /// may arrive before the creator has sized the segment, so a missing
    /// or short segment is retried for a bounded time. Whoever initialises
    /// the contents is still arbitrated by the caller (typically a CAS on
    /// a magic word in the mapped region).
    ///
    /// `size` is the desired byte count; the mapped region may be
    /// larger but never smaller.
    pub fn open(
        suffix: &str,
        size:   usize,
        policy: SweepPolicy,
    ) -> Result<Self, MorlocError> {
        let basename = shm::get_common_basename();
        if basename.is_empty() {
            return Err(MorlocError::Shm(format!(
                "shm_companion::open('{}'): shm not initialised",
                suffix,
            )));
        }
        let name_str = format!("{}.{}", basename, suffix);
        let (base, actual_size) = match shm::create_segment(&name_str, size)? {
            Some(seg) => (seg.ptr, seg.len),
            None => attach(&name_str, size)?,
        };

        let name = CString::new(name_str.as_str()).map_err(|e| {
            MorlocError::Other(format!("companion name contains NUL: {}", e))
        })?;

        COMPANION_TOTAL_BYTES.fetch_add(actual_size, Ordering::Relaxed);

        Ok(Self {
            base,
            size: actual_size,
            name,
            policy,
            swept: false,
        })
    }

    /// Unmap the segment and deregister it from the crash-sweep list. The
    /// segment is removed only by the program's owner (see
    /// `shm::shinit`): other processes may still be using it, and a
    /// removed name would be created afresh, empty, by the next process
    /// to open it. Idempotent: subsequent calls short-circuit on the null
    /// base.
    pub fn teardown(&mut self) {
        if self.base.is_null() {
            return;
        }
        unsafe {
            libc::munmap(self.base as *mut libc::c_void, self.size);
        }
        self.base = std::ptr::null_mut();

        if shm::owns_program() {
            self.remove_name();
        }
        if self.swept {
            deregister_companion(&self.name);
        }
        COMPANION_TOTAL_BYTES.fetch_sub(self.size, Ordering::Relaxed);
    }

    /// Add this segment to the names a crashing nexus removes, if its policy
    /// asks for that. Called once the segment is installed for keeps.
    pub fn register_for_sweep(&mut self) -> Result<(), MorlocError> {
        if self.policy == SweepPolicy::SweepOnCrash && !self.swept {
            register_companion(&self.name)?;
            self.swept = true;
        }
        Ok(())
    }

    /// Unmap this mapping and deregister it, leaving the segment's name for
    /// the mappings that stay.
    pub fn detach(mut self) {
        if self.base.is_null() {
            return;
        }
        unsafe {
            libc::munmap(self.base as *mut libc::c_void, self.size);
        }
        self.base = std::ptr::null_mut();
        if self.swept {
            deregister_companion(&self.name);
        }
        COMPANION_TOTAL_BYTES.fetch_sub(self.size, Ordering::Relaxed);
    }

    /// Remove the segment's name, leaving this process's mapping in place,
    /// for a segment that threads of this process may still touch while it
    /// exits. Only the program's owner should call this.
    pub fn unlink(&mut self) {
        self.remove_name();
        if self.swept {
            deregister_companion(&self.name);
            self.swept = false;
        }
    }

    /// Leave the crash-sweep list and return the name, for a caller that
    /// removes the names itself with [`remove_names`].
    pub fn forget_for_unlink(&mut self) -> CString {
        if self.swept {
            deregister_companion(&self.name);
            self.swept = false;
        }
        self.name.clone()
    }

    fn remove_name(&self) {
        remove_names(&self.name);
    }

    /// Full on-disk name (`<basename>.<suffix>`, including any `/` prefix
    /// per POSIX shm conventions). Borrowed reference; caller must not
    /// outlive the `CompanionSegment`.
    pub fn name(&self) -> &CStr {
        &self.name
    }
}

/// Map the existing segment `name`, waiting up to about five seconds for
/// its creator to give it at least `size` bytes.
fn attach(name: &str, size: usize) -> Result<(*mut u8, usize), MorlocError> {
    const WAIT_MS: u32 = 5000;
    let mut seen = 0;
    for _ in 0..WAIT_MS {
        if let Ok((fd, len)) = shm::open_segment(name)? {
            if len >= size {
                return shm::map_shared(&fd, len)
                    .map(|p| (p, len))
                    .ok_or_else(|| MorlocError::Shm(format!("Cannot mmap companion '{}'", name)));
            }
            seen = len;
        }
        std::thread::sleep(std::time::Duration::from_millis(1));
    }
    Err(MorlocError::Shm(format!(
        "companion '{}' holds {} bytes after {} ms; {} are needed",
        name, seen, WAIT_MS, size
    )))
}

impl Drop for CompanionSegment {
    fn drop(&mut self) {
        self.teardown();
    }
}

/// Remove a segment's names, leaving every mapping of it in place.
pub fn remove_names(name: &CStr) {
    shm::unlink_segment(name);
    if let Some(dir) = shm::get_fallback_dir() {
        let mut path = PathBuf::from(dir);
        path.push(name.to_string_lossy().trim_start_matches('/'));
        let _ = std::fs::remove_file(path);
    }
}

// ── Crash-sweep name registry ────────────────────────────────────────────────

pub const MAX_COMPANIONS: usize = 16;

/// Array of leaked `CString` pointers naming every companion opened
/// with `SweepOnCrash`. The nexus's SIGTERM handler reads this array
/// (via `extern "C"` linkage) and `shm_unlink`s each non-null entry
/// after the allocator sweep.
///
/// `#[no_mangle]` so the nexus's signal handler sees the same static
/// symbol at load time (via DT_NEEDED into `libmorloc.so`).
#[no_mangle]
pub static MORLOC_COMPANION_NAMES:
    [AtomicPtr<c_char>; MAX_COMPANIONS] =
    [const { AtomicPtr::new(std::ptr::null_mut()) }; MAX_COMPANIONS];

fn register_companion(name: &CStr) -> Result<(), MorlocError> {
    let owned = CString::new(name.to_bytes()).map_err(|e| {
        MorlocError::Other(format!("companion name has NUL: {}", e))
    })?;
    let raw = owned.into_raw();
    for slot in MORLOC_COMPANION_NAMES.iter() {
        if slot
            .compare_exchange(
                std::ptr::null_mut(),
                raw,
                Ordering::AcqRel,
                Ordering::Acquire,
            )
            .is_ok()
        {
            return Ok(());
        }
    }
    unsafe { drop(CString::from_raw(raw)); }
    Err(MorlocError::Shm(format!(
        "shm_companion: no free MORLOC_COMPANION_NAMES slot \
         (MAX_COMPANIONS = {})",
        MAX_COMPANIONS,
    )))
}

fn deregister_companion(name: &CStr) {
    for slot in MORLOC_COMPANION_NAMES.iter() {
        let p = slot.load(Ordering::Acquire);
        if p.is_null() {
            continue;
        }
        let same = unsafe { libc::strcmp(p, name.as_ptr()) == 0 };
        if same
            && slot
                .compare_exchange(
                    p,
                    std::ptr::null_mut(),
                    Ordering::AcqRel,
                    Ordering::Acquire,
                )
                .is_ok()
        {
            unsafe { drop(CString::from_raw(p)); }
            return;
        }
    }
}

// ── Size accounting ──────────────────────────────────────────────────────────

static COMPANION_TOTAL_BYTES: AtomicUsize = AtomicUsize::new(0);

/// Sum of every currently-attached companion's mapped size. Fed into
/// `shm::total_shm_size` so diagnostic reports match the on-disk
/// footprint.
pub fn total_companion_bytes() -> usize {
    COMPANION_TOTAL_BYTES.load(Ordering::Relaxed)
}
