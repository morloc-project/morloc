//! SHM companion segments: fixed-size shared mappings kept outside
//! the general allocator's `_<idx>` namespace.
//!
//! The general SHM allocator (`shm::shinit` + `find_free_block`) owns
//! volumes named `<basename>_<idx>`. Subsystems that want a fixed
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

/// Which backing store the companion was opened against. Populated on
/// open, consulted on teardown so we issue exactly one unlink syscall.
#[derive(Debug, Clone)]
enum Backing {
    Tmpfs,
    File(PathBuf),
}

/// A shared memory segment held outside the general allocator's
/// `_<idx>` namespace. RAII: `Drop` runs `teardown`.
///
/// Callers that hand the segment off to a static must
/// `mem::forget(seg)` and later call a manual teardown; see
/// `stream::registry_teardown` for an example.
pub struct CompanionSegment {
    pub base:  *mut u8,
    pub size:  usize,
    name:      CString,
    backing:   Backing,
    policy:    SweepPolicy,
}

// SAFETY: CompanionSegment holds a raw pointer to an mmap'd region and
// an owned name. The mmap survives across threads; readers/writers of
// the memory synchronise via their own protocol (magic gates, futexes,
// etc.). Marking Send/Sync so callers can put the segment into a
// `Mutex<Option<CompanionSegment>>` or similar.
unsafe impl Send for CompanionSegment {}
unsafe impl Sync for CompanionSegment {}

impl CompanionSegment {
    /// Open (or attach to) `<basename>.<suffix>`. First caller in a
    /// session creates the file; later callers attach. Cross-process
    /// arbitration of the "who initialises the header" question is the
    /// caller's job (typically a CAS on a magic word in the mapped
    /// region); this function does not report first-vs-attach because
    /// on a concurrent `shm_open(O_CREAT)` race both peers can observe
    /// `created = true` locally and only the CAS resolves.
    ///
    /// `size` is the desired byte count; the mapped region may be
    /// larger (mmap page-rounds silently) but never smaller.
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

        let (base, actual_size, backing) =
            match shm::try_open_tmpfs(&name_str, size)? {
                Some(opened) => {
                    let base = opened.ptr;
                    unsafe { libc::close(opened.fd); }
                    (base, opened.actual_size, Backing::Tmpfs)
                }
                None => {
                    let opened = shm::try_open_file_backed(&name_str, size)?;
                    let base = opened.ptr;
                    unsafe { libc::close(opened.fd); }
                    (base, opened.actual_size,
                     Backing::File(PathBuf::from(&opened.label)))
                }
            };

        let name = CString::new(name_str.as_str()).map_err(|e| {
            MorlocError::Other(format!("companion name contains NUL: {}", e))
        })?;

        if policy == SweepPolicy::SweepOnCrash {
            register_companion(&name)?;
        }
        COMPANION_TOTAL_BYTES.fetch_add(actual_size, Ordering::Relaxed);

        Ok(Self {
            base,
            size: actual_size,
            name,
            backing,
            policy,
        })
    }

    /// Unlink the backing file (tmpfs OR file, depending on how it
    /// was opened), `munmap` the region, and deregister from the
    /// crash-sweep list. Idempotent: subsequent calls short-circuit
    /// on the null base.
    pub fn teardown(&mut self) {
        if self.base.is_null() {
            return;
        }
        unsafe {
            libc::munmap(self.base as *mut libc::c_void, self.size);
        }
        self.base = std::ptr::null_mut();

        match &self.backing {
            Backing::Tmpfs => unsafe {
                libc::shm_unlink(self.name.as_ptr());
            },
            Backing::File(path) => {
                let _ = std::fs::remove_file(path);
            }
        }

        if self.policy == SweepPolicy::SweepOnCrash {
            deregister_companion(&self.name);
        }
        COMPANION_TOTAL_BYTES.fetch_sub(self.size, Ordering::Relaxed);
    }

    /// Full on-disk name (`<basename>.<suffix>`, including any `/` prefix
    /// per POSIX shm conventions). Borrowed reference; caller must not
    /// outlive the `CompanionSegment`.
    pub fn name(&self) -> &CStr {
        &self.name
    }
}

impl Drop for CompanionSegment {
    fn drop(&mut self) {
        self.teardown();
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
