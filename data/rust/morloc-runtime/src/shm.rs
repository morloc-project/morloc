//! Shared memory management with multi-volume support.
//!
//! Replaces shm.c / memory.h. Uses AtomicU32 + futex for cross-process locking
//! instead of pthread_rwlock_t, providing crash-safety and portability.

use crate::error::MorlocError;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::Mutex;

// Wire-format types and constants live in `morloc-runtime-types::shm_types`
// so they can be shared with the nexus without duplicating process state.
// Re-exported here so existing call sites (`crate::shm::RelPtr`,
// `crate::shm::Array`, etc.) keep working unchanged.
pub use morloc_runtime_types::shm_types::{
    align_up, AbsPtr, Array, BlockHeader, RelPtr, ShmHeader, VolPtr,
    BLK_MAGIC, BLOCK_ALIGN, MAX_FILENAME_SIZE, MAX_PATH_SIZE, MAX_VOLUME_NUMBER,
    RELNULL, SHM_MAGIC, VOLNULL,
};

/// Cross-platform file pre-allocation.
/// Linux: posix_fallocate (allocates disk blocks).
/// macOS: ftruncate (extends file, may be sparse).
#[cfg(target_os = "linux")]
unsafe fn preallocate_fd(fd: i32, size: i64) -> i32 {
    libc::posix_fallocate(fd, 0, size)
}

#[cfg(target_os = "macos")]
unsafe fn preallocate_fd(fd: i32, size: i64) -> i32 {
    if libc::ftruncate(fd, size) == -1 { -1 } else { 0 }
}

// ── Internal lock constants ────────────────────────────────────────────────

const LOCK_UNLOCKED: u32 = 0;
const LOCK_LOCKED: u32 = 1;
const SPIN_LIMIT: u32 = 40;
#[cfg(target_os = "linux")]
const LOCK_TIMEOUT_SECS: u64 = 5;

// ── Send wrapper for raw pointers ──────────────────────────────────────────

#[derive(Clone, Copy)]
struct SendPtr(*mut ShmHeader);
// SAFETY: ShmHeader lives in mmap'd shared memory that outlives all threads.
// Access is serialized via VOLUMES Mutex and per-volume AtomicU32 futex locks.
unsafe impl Send for SendPtr {}
impl SendPtr {
    const fn null() -> Self { SendPtr(std::ptr::null_mut()) }
    fn is_null(&self) -> bool { self.0.is_null() }
    fn ptr(&self) -> *mut ShmHeader { self.0 }
    fn set(&mut self, p: *mut ShmHeader) { self.0 = p; }
}

fn get_cstr_buf(buf: &[u8; MAX_FILENAME_SIZE]) -> &str {
    get_cstr(buf.as_slice())
}

// ── Global state ───────────────────────────────────────────────────────────

static CURRENT_VOLUME: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

static VOLUMES: Mutex<[SendPtr; MAX_VOLUME_NUMBER]> =
    Mutex::new([SendPtr::null(); MAX_VOLUME_NUMBER]);

static ALLOC_MUTEX: Mutex<()> = Mutex::new(());

static COMMON_BASENAME: Mutex<[u8; MAX_FILENAME_SIZE]> = Mutex::new([0u8; MAX_FILENAME_SIZE]);

static FALLBACK_DIR: Mutex<[u8; MAX_FILENAME_SIZE]> = Mutex::new([0u8; MAX_FILENAME_SIZE]);

/// Whether atexit handler has been registered (once per process).
static ATEXIT_REGISTERED: AtomicBool = AtomicBool::new(false);

/// atexit callback: unlink SHM segments on process exit.
/// Catches normal exit() calls that bypass explicit clean_exit().
/// Uses try_lock to avoid panic if mutexes are poisoned (e.g., during panic unwind).
extern "C" fn shclose_atexit() {
    // Best-effort: if locks are poisoned or held, skip cleanup
    // rather than panic inside atexit.
    let vols_guard = match VOLUMES.try_lock() {
        Ok(g) => g,
        Err(_) => return,
    };
    for i in 0..MAX_VOLUME_NUMBER {
        if vols_guard[i].is_null() {
            continue;
        }
        let shm = vols_guard[i].ptr();
        unsafe {
            let name = get_cstr(&(*shm).volume_name).to_string();
            let full_size = (*shm).volume_size + std::mem::size_of::<ShmHeader>();
            libc::munmap(shm as *mut libc::c_void, full_size);
            if name.starts_with('/') {
                let cstr = std::ffi::CString::new(name.as_str()).unwrap_or_default();
                libc::unlink(cstr.as_ptr());
            } else {
                let cstr = std::ffi::CString::new(name.as_str()).unwrap_or_default();
                libc::shm_unlink(cstr.as_ptr());
            }
        }
    }
    drop(vols_guard);
}

fn set_cstr(buf: &mut [u8], s: &str) {
    let bytes = s.as_bytes();
    let len = bytes.len().min(buf.len() - 1);
    buf[..len].copy_from_slice(&bytes[..len]);
    buf[len] = 0;
}

fn get_cstr(buf: &[u8]) -> &str {
    let end = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
    std::str::from_utf8(&buf[..end]).unwrap_or("")
}

// ── Public API ─────────────────────────────────────────────────────────────

/// Set fallback directory for file-backed SHM when /dev/shm is too small.
pub fn shm_set_fallback_dir(dir: &str) {
    let mut fb = FALLBACK_DIR.lock().unwrap();
    set_cstr(&mut *fb, dir);
}

/// Initialize a new SHM volume.
pub fn shinit(
    shm_basename: &str,
    volume_index: usize,
    shm_size: usize,
) -> Result<*mut ShmHeader, MorlocError> {
    // Register atexit handler on first call (once per process).
    // This ensures SHM is unlinked on any normal exit path, even if
    // clean_exit() is not called (e.g., pool processes calling sys.exit()).
    if !ATEXIT_REGISTERED.swap(true, Ordering::SeqCst) {
        unsafe { libc::atexit(shclose_atexit) };
    }

    let full_size = shm_size + std::mem::size_of::<ShmHeader>();
    let shm_name = format!("{}_{}", shm_basename, volume_index);

    // Store common basename
    {
        let mut cb = COMMON_BASENAME.lock().unwrap();
        set_cstr(&mut *cb, shm_basename);
    }

    // Try POSIX shared memory first, fall back to file-backed
    let (fd, created, volume_label, actual_full_size) =
        try_open_shm(&shm_name, full_size)?;

    // SAFETY: mmap with MAP_SHARED on a valid fd obtained from shm_open/open above.
    // The returned pointer is checked against MAP_FAILED before use.
    let ptr = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            actual_full_size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            fd,
            0,
        )
    };
    // SAFETY: fd is a valid file descriptor opened above.
    unsafe { libc::close(fd) };

    if ptr == libc::MAP_FAILED {
        return Err(MorlocError::Shm(format!(
            "Failed to mmap volume '{}' ({} bytes)",
            volume_label, actual_full_size
        )));
    }

    let shm = ptr as *mut ShmHeader;

    // Store in volumes array
    {
        let mut vols = VOLUMES.lock().unwrap();
        vols[volume_index].set(shm);
    }

    let actual_data_size = actual_full_size - std::mem::size_of::<ShmHeader>();

    if created {
        // SAFETY: shm points to the start of our mmap'd region of actual_full_size bytes.
        // We just created it, so we have exclusive access for initialization.
        unsafe {
            (*shm).magic = SHM_MAGIC;
            let mut name_buf = [0u8; MAX_FILENAME_SIZE];
            set_cstr(&mut name_buf, &volume_label);
            (*shm).volume_name = name_buf;
            (*shm).volume_index = volume_index as i32;

            // Calculate relative offset from prior volumes
            let vols = VOLUMES.lock().unwrap();
            let mut rel_offset = 0usize;
            for i in 0..volume_index {
                if !vols[i].is_null() {
                    rel_offset += (*vols[i].ptr()).volume_size;
                }
            }
            (*shm).relative_offset = rel_offset;
            (*shm).volume_size = actual_data_size;
            (*shm).lock = AtomicU32::new(LOCK_UNLOCKED);
            (*shm).cursor = 0;

            // Initialize first block header
            let first_block =
                (shm as *mut u8).add(std::mem::size_of::<ShmHeader>()) as *mut BlockHeader;
            (*first_block).magic = BLK_MAGIC;
            (*first_block).reference_count = AtomicU32::new(0);
            (*first_block).size = actual_data_size - std::mem::size_of::<BlockHeader>();
        }
    }

    Ok(shm)
}

/// Reason `shopen` could not return a mapped volume. Each variant is
/// distinguishable so callers (notably `rel2abs`) can render a precise
/// diagnostic instead of the historical generic "volume not found".
#[derive(Debug)]
pub enum ShopenMiss {
    /// COMMON_BASENAME is empty: this process never called `shinit`.
    /// Post-refactor (rlib removal) this should not be reachable from
    /// normal call paths -- there is one libmorloc.so per process and
    /// the nexus/pool wire shinit before any allocation. Surfacing it
    /// explicitly catches "someone touched a relptr before init" bugs.
    NotInitialized,
    /// Basename is set but neither `/dev/shm/<basename>_<i>` nor the
    /// file-backed fallback at `<fallback_dir>/<basename>_<i>` could be
    /// opened. Common causes: the writer never created this volume,
    /// the writer crashed before sending, basename mismatch between
    /// writer and reader, or another process (or stale-SHM cleanup)
    /// unlinked the segment.
    FileMissing {
        basename: String,
        volume_index: usize,
        fallback_dir: String,
        shm_errno: i32,
    },
}

/// Open an existing SHM volume (or return cached pointer).
/// Wrapper around `shopen_diag` that collapses miss reasons into
/// `Ok(None)` for legacy callers; use `shopen_diag` directly when you
/// need to render a specific diagnostic.
pub fn shopen(volume_index: usize) -> Result<Option<*mut ShmHeader>, MorlocError> {
    match shopen_diag(volume_index)? {
        Ok(shm) => Ok(Some(shm)),
        Err(_) => Ok(None),
    }
}

/// Diagnostic version of `shopen`: distinguishes "not initialized"
/// from "file missing" so `rel2abs` can give the user something
/// actionable instead of the generic "volume not found".
pub fn shopen_diag(
    volume_index: usize,
) -> Result<Result<*mut ShmHeader, ShopenMiss>, MorlocError> {
    {
        let vols = VOLUMES.lock().unwrap();
        if !vols[volume_index].is_null() {
            return Ok(Ok(vols[volume_index].ptr()));
        }
    }

    let basename = {
        let cb = COMMON_BASENAME.lock().unwrap();
        get_cstr_buf(&cb).to_string()
    };
    if basename.is_empty() {
        return Ok(Err(ShopenMiss::NotInitialized));
    }

    let shm_name = format!("{}_{}", basename, volume_index);

    // Try POSIX SHM
    let name_cstr = std::ffi::CString::new(shm_name.as_str()).unwrap();
    // SAFETY: name_cstr is a valid null-terminated CString.
    let fd = unsafe { libc::shm_open(name_cstr.as_ptr(), libc::O_RDWR, 0o666) };
    let shm_open_errno = if fd == -1 { unsafe { crate::utility::errno_val() } } else { 0 };

    let fd = if fd == -1 {
        // Try file-backed fallback
        let fb = FALLBACK_DIR.lock().unwrap();
        let fallback = get_cstr_buf(&fb).to_string();
        drop(fb);
        if fallback.is_empty() {
            return Ok(Err(ShopenMiss::FileMissing {
                basename,
                volume_index,
                fallback_dir: String::new(),
                shm_errno: shm_open_errno,
            }));
        }
        let file_path = format!("{}/{}", fallback, shm_name);
        let path_cstr = std::ffi::CString::new(file_path.as_str()).unwrap();
        let fd2 = unsafe { libc::open(path_cstr.as_ptr(), libc::O_RDWR) };
        if fd2 == -1 {
            return Ok(Err(ShopenMiss::FileMissing {
                basename,
                volume_index,
                fallback_dir: fallback,
                shm_errno: shm_open_errno,
            }));
        }
        fd2
    } else {
        fd
    };

    // SAFETY: zeroed memory is valid for libc::stat. fstat/close on valid fd.
    let mut sb: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd, &mut sb) } == -1 {
        unsafe { libc::close(fd) };
        return Err(MorlocError::Shm(format!(
            "Cannot fstat SHM volume '{}'",
            shm_name
        )));
    }
    let volume_size = sb.st_size as usize;

    // SAFETY: mmap with MAP_SHARED on a valid fd; result checked against MAP_FAILED.
    let ptr = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            volume_size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            fd,
            0,
        )
    };
    // SAFETY: fd is a valid file descriptor opened above.
    unsafe { libc::close(fd) };

    if ptr == libc::MAP_FAILED {
        return Err(MorlocError::Shm(format!(
            "Cannot mmap SHM volume '{}'",
            shm_name
        )));
    }

    let shm = ptr as *mut ShmHeader;
    {
        let mut vols = VOLUMES.lock().unwrap();
        vols[volume_index].set(shm);
    }

    Ok(Ok(shm))
}

/// Close and unlink all SHM volumes.
pub fn shclose() -> Result<(), MorlocError> {
    let _lock = ALLOC_MUTEX.lock().unwrap();
    let mut vols = VOLUMES.lock().unwrap();
    shclose_locked(&mut vols);
    Ok(())
}

/// Drop every SHM volume currently held by this process: munmap, unlink,
/// clear bookkeeping (VOLUMES / CURRENT_VOLUME / COMMON_BASENAME).
///
/// Intended for in-process recovery (pool-crash teardown). After this
/// returns, no SHM is mapped or named on disk for this basename, and the
/// allocator is reset to its pre-`shinit` state. Caller is responsible
/// for calling `shinit` again with a fresh basename and respawning any
/// pools so they shopen the new files.
///
/// Holds `ALLOC_MUTEX` across the entire teardown, so any concurrent
/// `shmalloc` or `shfree` blocks until reset completes. Callers that
/// `shfree` after reset against a stale pointer will hit the
/// "address not inside any mapped volume" guard added to `shfree` and
/// no-op rather than segfault.
pub fn reset_all() -> Result<(), MorlocError> {
    let _lock = ALLOC_MUTEX.lock().unwrap();
    let mut vols = VOLUMES.lock().unwrap();
    shclose_locked(&mut vols);
    CURRENT_VOLUME.store(0, std::sync::atomic::Ordering::Release);
    let mut cb = COMMON_BASENAME.lock().unwrap();
    for b in cb.iter_mut() {
        *b = 0;
    }
    Ok(())
}

/// Returns true if `ptr` falls inside any currently-mapped SHM volume.
/// Used by `shfree` as a safety guard against being called with a
/// stale pointer after `reset_all` (e.g. a worker that was holding an
/// SHM ptr when its request was failed by the recovery quiesce).
fn ptr_is_in_any_volume(ptr: AbsPtr) -> bool {
    let p = ptr as usize;
    let vols = VOLUMES.lock().unwrap();
    for i in 0..MAX_VOLUME_NUMBER {
        let shm = vols[i].ptr();
        if shm.is_null() {
            continue;
        }
        // SAFETY: shm is a valid mmap'd ShmHeader from VOLUMES.
        let vol_size = unsafe { (*shm).volume_size };
        let base = unsafe { (shm as *const u8).add(std::mem::size_of::<ShmHeader>()) } as usize;
        if p >= base && p < base + vol_size {
            return true;
        }
    }
    false
}

/// Internal: do the unlink/munmap walk under already-held locks.
fn shclose_locked(vols: &mut [SendPtr; MAX_VOLUME_NUMBER]) {
    for i in 0..MAX_VOLUME_NUMBER {
        let shm = if !vols[i].is_null() {
            vols[i].ptr()
        } else {
            continue;
        };

        // SAFETY: shm is a valid mmap'd pointer stored in VOLUMES.
        // munmap/unlink on regions we own. Name read from valid ShmHeader.
        unsafe {
            let name = get_cstr(&(*shm).volume_name).to_string();
            let full_size = (*shm).volume_size + std::mem::size_of::<ShmHeader>();
            libc::munmap(shm as *mut libc::c_void, full_size);

            // Unlink: file-backed volumes start with '/', POSIX SHM does not
            if name.starts_with('/') {
                let cstr = std::ffi::CString::new(name.as_str()).unwrap();
                libc::unlink(cstr.as_ptr());
            } else {
                let cstr = std::ffi::CString::new(name.as_str()).unwrap();
                libc::shm_unlink(cstr.as_ptr());
            }
        }
        vols[i] = SendPtr::null();
    }
}

/// Allocate at least `size` bytes from shared memory and return a pointer
/// to the start of the data region.
///
/// **Size contract.** The returned block always has at least `BLOCK_ALIGN`
/// usable bytes; requests for any size in `0..=BLOCK_ALIGN` all yield a
/// `BLOCK_ALIGN`-byte block. Larger requests are rounded up to a multiple
/// of `BLOCK_ALIGN`. Callers that ship "zero bytes of payload" (e.g. nil
/// values, empty arrays whose width comes from `schema.width == 0`) get a
/// real, freeable block back -- there is no zero-byte sentinel. This is
/// load-bearing: the eval pipeline (`morloc_eval_r` shcalloc'ing the
/// top-level wrapper at width 0 for nil), `unpack_with_schema`, and
/// `read_binary` all rely on receiving a non-null pointer for nil-shaped
/// allocations.
///
/// **Lifetime contract.** Every successful return must be paired with
/// either an `shfree` or, when active on the current thread, registration
/// in the per-eval arena (which auto-shfrees at scope drop). The arena
/// hook fires here unconditionally on success; callers outside the arena
/// see no behavioral difference.
pub fn shmalloc(size: usize) -> Result<AbsPtr, MorlocError> {
    let size = if size == 0 { BLOCK_ALIGN } else { align_up(size, BLOCK_ALIGN) };
    let ptr = {
        let _lock = ALLOC_MUTEX.lock().unwrap();
        shmalloc_unlocked(size)?
    };
    crate::eval_arena::record_if_active(ptr);
    Ok(ptr)
}

/// Copy data into a new SHM allocation.
pub fn shmemcpy(src: *const u8, size: usize) -> Result<AbsPtr, MorlocError> {
    let dest = shmalloc(size)?;
    // SAFETY: dest is a freshly allocated SHM block of `size` bytes.
    // Caller guarantees src points to `size` readable bytes.
    unsafe { std::ptr::copy_nonoverlapping(src, dest, size) };
    Ok(dest)
}

/// Allocate and zero-fill.
pub fn shcalloc(nmemb: usize, size: usize) -> Result<AbsPtr, MorlocError> {
    let total = nmemb * size;
    let ptr = shmalloc(total)?;
    // SAFETY: ptr is a freshly allocated SHM block of `total` bytes.
    unsafe { std::ptr::write_bytes(ptr, 0, total) };
    Ok(ptr)
}

/// Free a shared memory block (decrement reference count).
pub fn shfree(ptr: AbsPtr) -> Result<(), MorlocError> {
    // Remove this pointer from any active eval arena before freeing, so
    // guard-drop won't attempt a second free. No-op if no arena is active
    // or if `ptr` was never tracked.
    crate::eval_arena::forget_if_active(ptr);
    let _lock = ALLOC_MUTEX.lock().unwrap();
    // Pool-crash recovery: if `reset_all` has unmapped every volume since
    // this caller obtained `ptr`, dereferencing the (now-unmapped) header
    // would segfault. The recovery sequence is responsible for getting all
    // workers to drop their references; this guard is just defense in
    // depth for the in-flight case where a worker's arena drop race-loses
    // to reset_all's munmap.
    if !ptr.is_null() && !ptr_is_in_any_volume(ptr) {
        return Ok(());
    }
    shfree_unlocked(ptr)
}

/// Increment reference count on a shared memory block.
pub fn shincref(ptr: AbsPtr) -> Result<(), MorlocError> {
    if ptr.is_null() {
        return Err(MorlocError::Shm("Cannot incref NULL pointer".into()));
    }
    // SAFETY: ptr was returned by shmalloc, which places a BlockHeader immediately before
    // the returned data pointer. Magic check below validates the header.
    let blk = unsafe { &*(ptr.sub(std::mem::size_of::<BlockHeader>()) as *const BlockHeader) };
    if blk.magic != BLK_MAGIC {
        return Err(MorlocError::Shm("Corrupted memory - invalid magic".into()));
    }
    blk.reference_count.fetch_add(1, Ordering::AcqRel);
    Ok(())
}

/// Convert relative pointer to absolute pointer.
pub fn rel2abs(ptr: RelPtr) -> Result<AbsPtr, MorlocError> {
    if ptr < 0 {
        return Err(MorlocError::Shm(format!("Illegal relptr value {}", ptr)));
    }

    let mut remaining = ptr as usize;

    // First try with volumes already mapped. Sum their sizes so the
    // error path can report how far into the SHM pool the relptr
    // actually pointed before we ran out of mapped volumes.
    let already_mapped_total: usize = {
        let vols = VOLUMES.lock().unwrap();
        let mut total = 0usize;
        for i in 0..MAX_VOLUME_NUMBER {
            if vols[i].is_null() {
                break; // No more volumes mapped
            }
            let shm = vols[i].ptr();
            // SAFETY: shm is a valid mmap'd ShmHeader pointer from VOLUMES.
            let vol_size = unsafe { (*shm).volume_size };
            if remaining < vol_size {
                // SAFETY: data region starts after ShmHeader; remaining < vol_size
                // guarantees the offset is within the mmap'd region.
                let base = unsafe {
                    (shm as *const u8).add(std::mem::size_of::<ShmHeader>())
                };
                return Ok(unsafe { base.add(remaining) as AbsPtr });
            }
            remaining -= vol_size;
            total += vol_size;
        }
        total
    };

    // If not found, try opening unmapped volumes
    remaining = ptr as usize;
    for i in 0..MAX_VOLUME_NUMBER {
        let shm = match shopen_diag(i)? {
            Ok(s) => s,
            Err(miss) => return Err(rel2abs_miss_error(ptr, i, already_mapped_total, miss)),
        };
        // SAFETY: shm is a valid mmap'd ShmHeader pointer from shopen.
        let vol_size = unsafe { (*shm).volume_size };
        if remaining < vol_size {
            // SAFETY: same as above - offset within mmap'd region.
            let base = unsafe {
                (shm as *const u8).add(std::mem::size_of::<ShmHeader>())
            };
            return Ok(unsafe { base.add(remaining) as AbsPtr });
        }
        remaining -= vol_size;
    }

    // Walked all MAX_VOLUME_NUMBER slots without finding it.
    Err(MorlocError::Shm(format!(
        "relptr {} exceeds total SHM capacity (scanned all {} volume slots; \
         the pool must have grown beyond MAX_VOLUME_NUMBER, or the relptr is corrupt)",
        ptr, MAX_VOLUME_NUMBER
    )))
}

/// Build a user-facing error for an `shopen` miss inside `rel2abs`. Each
/// `ShopenMiss` variant has a different root cause and a different fix,
/// so they get different messages instead of the legacy generic
/// "Failed to find volume". `already_mapped_bytes` is how much of the
/// pool was successfully scanned before the miss, included so the user
/// can correlate the relptr value with the working portion of SHM.
fn rel2abs_miss_error(
    ptr: RelPtr,
    volume_index: usize,
    already_mapped_bytes: usize,
    miss: ShopenMiss,
) -> MorlocError {
    let basename_now = {
        let cb = COMMON_BASENAME.lock().unwrap();
        get_cstr_buf(&cb).to_string()
    };
    match miss {
        ShopenMiss::NotInitialized => MorlocError::Shm(format!(
            "cannot resolve relptr {} -- SHM is not initialized in this process \
             (COMMON_BASENAME is empty). Caller reached rel2abs before any \
             shinit; if you see this after the rlib-removal refactor it most \
             likely means a foreign caller bypassed the dispatch path. \
             {} volume(s) were already mapped totaling {} bytes before the miss.",
            ptr, volume_index, already_mapped_bytes
        )),
        ShopenMiss::FileMissing {
            basename,
            volume_index: vi,
            fallback_dir,
            shm_errno,
        } => {
            let errno_msg = unsafe {
                let s = libc::strerror(shm_errno);
                if s.is_null() {
                    format!("errno={}", shm_errno)
                } else {
                    std::ffi::CStr::from_ptr(s).to_string_lossy().into_owned()
                }
            };
            let basename_note = if basename_now == basename {
                String::new()
            } else {
                format!(
                    " (current COMMON_BASENAME is now '{}'; basename may have \
                     changed after pool-crash recovery)",
                    basename_now
                )
            };
            let fallback_note = if fallback_dir.is_empty() {
                "no file-backed fallback directory was configured".to_string()
            } else {
                format!("file-backed fallback '{}/{}_{}' also missing", fallback_dir, basename, vi)
            };
            MorlocError::Shm(format!(
                "cannot resolve relptr {} -- SHM volume '/dev/shm/{}_{}' does not exist \
                 (shm_open: {}).{} {}. Likely causes: writer never created this volume, \
                 writer crashed before sending, basename mismatch between writer and \
                 reader, or another process (or /dev/shm cleanup) unlinked it. \
                 {} volume(s) were already mapped before the miss.",
                ptr, basename, vi, errno_msg, basename_note, fallback_note, volume_index
            ))
        }
    }
}

/// Convert absolute pointer to relative pointer.
pub fn abs2rel(ptr: AbsPtr) -> Result<RelPtr, MorlocError> {
    let vols = VOLUMES.lock().unwrap();
    for i in 0..MAX_VOLUME_NUMBER {
        let shm = vols[i].ptr();
        if shm.is_null() {
            continue;
        }
        // SAFETY: shm is a valid mmap'd ShmHeader from VOLUMES. We compute
        // data region bounds and check ptr falls within before computing offset.
        unsafe {
            let data_start = (shm as *const u8).add(std::mem::size_of::<ShmHeader>());
            let data_end = data_start.add((*shm).volume_size);
            let p = ptr as *const u8;
            if p >= data_start && p < data_end {
                let offset = p.offset_from(data_start) as usize;
                return Ok(((*shm).relative_offset + offset) as RelPtr);
            }
        }
    }
    Err(MorlocError::Shm(format!(
        "Failed to find absptr {:?} in shared memory",
        ptr
    )))
}

/// Find the ShmHeader for a given absolute pointer.
pub fn abs2shm(ptr: AbsPtr) -> Result<*mut ShmHeader, MorlocError> {
    let vols = VOLUMES.lock().unwrap();
    for i in 0..MAX_VOLUME_NUMBER {
        let shm = vols[i].ptr();
        if shm.is_null() {
            continue;
        }
        // SAFETY: shm is a valid mmap'd ShmHeader from VOLUMES.
        unsafe {
            let data_start = (shm as *const u8).add(std::mem::size_of::<ShmHeader>());
            let data_end = data_start.add((*shm).volume_size);
            let p = ptr as *const u8;
            if p >= data_start && p < data_end {
                return Ok(shm);
            }
        }
    }
    Err(MorlocError::Shm("Failed to find absptr in SHM".into()))
}

/// Total size of all SHM volumes.
pub fn total_shm_size() -> usize {
    let vols = VOLUMES.lock().unwrap();
    let mut total = 0;
    for i in 0..MAX_VOLUME_NUMBER {
        if !vols[i].is_null() {
            // SAFETY: non-null VOLUMES entries are valid mmap'd ShmHeader pointers.
            total += unsafe { (*vols[i].ptr()).volume_size };
        }
    }
    total
}

// ── Internal helpers ───────────────────────────────────────────────────────

fn try_open_shm(
    shm_name: &str,
    full_size: usize,
) -> Result<(i32, bool, String, usize), MorlocError> {
    let name_cstr = std::ffi::CString::new(shm_name).unwrap();

    // Try POSIX SHM
    let fd = unsafe {
        libc::shm_open(
            name_cstr.as_ptr(),
            libc::O_RDWR | libc::O_CREAT,
            0o666,
        )
    };

    if fd >= 0 {
        let mut sb: libc::stat = unsafe { std::mem::zeroed() };
        if unsafe { libc::fstat(fd, &mut sb) } == -1 {
            unsafe { libc::close(fd) };
            return Err(MorlocError::Shm(format!("fstat failed for '{}'", shm_name)));
        }
        let created = sb.st_size == 0;
        if created {
            let err = unsafe { preallocate_fd(fd, full_size as i64) };
            if err == 0 {
                return Ok((fd, true, shm_name.to_string(), full_size));
            }
            // /dev/shm too small, clean up and try file-backed
            unsafe {
                libc::close(fd);
                libc::shm_unlink(name_cstr.as_ptr());
            }
        } else {
            return Ok((fd, false, shm_name.to_string(), sb.st_size as usize));
        }
    }

    // Try file-backed fallback
    let fb = FALLBACK_DIR.lock().unwrap();
    let fallback = get_cstr_buf(&fb);
    if fallback.is_empty() {
        return Err(MorlocError::Shm(format!(
            "Failed to allocate SHM '{}': /dev/shm too small and no fallback directory",
            shm_name
        )));
    }
    let file_path = format!("{}/{}", fallback, shm_name);
    drop(fb);

    let path_cstr = std::ffi::CString::new(file_path.as_str()).unwrap();
    let fd = unsafe { libc::open(path_cstr.as_ptr(), libc::O_RDWR | libc::O_CREAT, 0o666) };
    if fd == -1 {
        return Err(MorlocError::Shm(format!(
            "Failed to create file-backed volume '{}'",
            file_path
        )));
    }

    let mut sb: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd, &mut sb) } == -1 {
        unsafe { libc::close(fd) };
        return Err(MorlocError::Shm(format!("fstat failed for '{}'", file_path)));
    }
    let created = sb.st_size == 0;
    let actual_size = if created {
        let err = unsafe { preallocate_fd(fd, full_size as i64) };
        if err != 0 {
            unsafe {
                libc::close(fd);
                libc::unlink(path_cstr.as_ptr());
            }
            return Err(MorlocError::Shm(format!(
                "Failed to allocate file-backed volume '{}' ({} bytes)",
                file_path, full_size
            )));
        }
        full_size
    } else {
        sb.st_size as usize
    };

    Ok((fd, created, file_path, actual_size))
}

fn shmalloc_unlocked(size: usize) -> Result<AbsPtr, MorlocError> {
    let mut shm: *mut ShmHeader = std::ptr::null_mut();
    let blk = find_free_block(size, &mut shm)?;

    // Split and claim
    let final_blk = split_block(shm, blk, size)?;
    // SAFETY: final_blk is a valid BlockHeader in mmap'd SHM found by find_free_block.
    // The data region starts immediately after the header.
    unsafe {
        (*final_blk).reference_count.store(1, Ordering::Release);
        Ok((final_blk as *mut u8).add(std::mem::size_of::<BlockHeader>()))
    }
}

fn shfree_unlocked(ptr: AbsPtr) -> Result<(), MorlocError> {
    if ptr.is_null() {
        return Err(MorlocError::Shm("Cannot free NULL pointer".into()));
    }
    // SAFETY: ptr was returned by shmalloc, which places a BlockHeader
    // immediately before the data. Magic check validates correctness.
    let blk = unsafe {
        &*(ptr.sub(std::mem::size_of::<BlockHeader>()) as *const BlockHeader)
    };
    if blk.magic != BLK_MAGIC {
        return Err(MorlocError::Shm("Corrupted memory".into()));
    }
    if blk.reference_count.load(Ordering::Acquire) == 0 {
        return Err(MorlocError::Shm("Reference count already 0".into()));
    }
    let prev = blk.reference_count.fetch_sub(1, Ordering::AcqRel);
    if prev == 1 {
        // SAFETY: ptr points to blk.size bytes of SHM data we own (refcount just hit 0).
        unsafe {
            std::ptr::write_bytes(ptr, 0, blk.size);
        }
    }
    Ok(())
}

fn find_free_block(
    size: usize,
    shm_out: &mut *mut ShmHeader,
) -> Result<*mut BlockHeader, MorlocError> {
    let cv = CURRENT_VOLUME.load(Ordering::Relaxed);
    let vols = VOLUMES.lock().unwrap();

    // Try current volume first
    let shm = vols[cv].ptr();
    if !shm.is_null() {
        if let Some(blk) = find_free_block_in_volume(shm, size)? {
            *shm_out = shm;
            return Ok(blk);
        }
    }

    // Search all volumes
    for i in 0..MAX_VOLUME_NUMBER {
        let shm = vols[i].ptr();
        if shm.is_null() {
            // Grow each new volume to K x the previous volume's size, but at
            // least large enough to fit the requesting allocation plus its
            // BlockHeader. Geometric growth means total capacity expands
            // exponentially with volume count, so MAX_VOLUME_NUMBER is not
            // reached after only a handful of fresh volumes.
            const VOLUME_GROWTH_FACTOR: usize = 2;
            let prev_volume_size = if i > 0 && !vols[i - 1].is_null() {
                // SAFETY: vols[i-1] is a valid mmap'd ShmHeader held under the
                // VOLUMES lock for the duration of this read.
                unsafe { (*vols[i - 1].ptr()).volume_size }
            } else {
                0xffff
            };
            let new_size = std::cmp::max(
                size.saturating_add(std::mem::size_of::<BlockHeader>()),
                prev_volume_size.saturating_mul(VOLUME_GROWTH_FACTOR),
            );
            drop(vols);
            let basename = {
                let cb = COMMON_BASENAME.lock().unwrap();
                get_cstr_buf(&cb).to_string()
            };
            let new_shm = shinit(&basename, i, new_size)?;
            CURRENT_VOLUME.store(i, Ordering::Relaxed);
            *shm_out = new_shm;
            let blk = unsafe {
                (new_shm as *mut u8).add(std::mem::size_of::<ShmHeader>()) as *mut BlockHeader
            };
            return Ok(blk);
        }

        if let Some(blk) = find_free_block_in_volume(shm, size)? {
            CURRENT_VOLUME.store(i, Ordering::Relaxed);
            *shm_out = shm;
            return Ok(blk);
        }
    }

    Err(MorlocError::Shm(format!(
        "Could not find suitable block for {} bytes",
        size
    )))
}

fn find_free_block_in_volume(
    shm: *mut ShmHeader,
    size: usize,
) -> Result<Option<*mut BlockHeader>, MorlocError> {
    unsafe {
        let shm_end = (shm as *const u8)
            .add(std::mem::size_of::<ShmHeader>())
            .add((*shm).volume_size);

        shm_lock(&(*shm).lock)?;

        // Try cursor position first
        let cursor = (*shm).cursor;
        if cursor != VOLNULL {
            let blk = vol2abs_raw(cursor, shm);
            let blk = blk as *mut BlockHeader;
            if (*blk).magic == BLK_MAGIC
                && (*blk).reference_count.load(Ordering::Relaxed) == 0
                && (*blk).size >= size
            {
                shm_unlock(&(*shm).lock);
                return Ok(Some(blk));
            }
        }

        // Scan from cursor forward
        let start_blk = if cursor != VOLNULL {
            vol2abs_raw(cursor, shm) as *mut BlockHeader
        } else {
            vol2abs_raw(0, shm) as *mut BlockHeader
        };

        if let Some(blk) = scan_volume(start_blk, size, shm_end as *const u8) {
            shm_unlock(&(*shm).lock);
            return Ok(Some(blk));
        }

        // Wrap around: scan from beginning to cursor
        if cursor > 0 {
            let first_blk = vol2abs_raw(0, shm) as *mut BlockHeader;
            let cursor_end = vol2abs_raw(cursor, shm);
            if let Some(blk) = scan_volume(first_blk, size, cursor_end as *const u8) {
                shm_unlock(&(*shm).lock);
                return Ok(Some(blk));
            }
        }

        shm_unlock(&(*shm).lock);
        Ok(None)
    }
}

/// Scan a volume region for a free block of at least `size` bytes, merging adjacent free blocks.
///
/// # Safety
/// `blk` must point to a valid BlockHeader within an mmap'd SHM volume.
/// `end` must point to the byte past the end of the volume's data region.
unsafe fn scan_volume(
    mut blk: *mut BlockHeader,
    size: usize,
    end: *const u8,
) -> Option<*mut BlockHeader> {
    let hdr_size = std::mem::size_of::<BlockHeader>();
    while (blk as *const u8).add(hdr_size + size) <= end {
        if blk.is_null() || (*blk).magic != BLK_MAGIC {
            return None;
        }

        // Merge adjacent free blocks
        while (*blk).reference_count.load(Ordering::Relaxed) == 0 {
            let next = (blk as *mut u8).add(hdr_size + (*blk).size) as *mut BlockHeader;
            if (next as *const u8) >= end
                || (*next).magic != BLK_MAGIC
                || (*next).reference_count.load(Ordering::Relaxed) != 0
            {
                break;
            }
            (*blk).size += hdr_size + (*next).size;
        }

        if (*blk).reference_count.load(Ordering::Relaxed) == 0 && (*blk).size >= size {
            return Some(blk);
        }

        blk = (blk as *mut u8).add(hdr_size + (*blk).size) as *mut BlockHeader;
    }
    None
}

fn split_block(
    shm: *mut ShmHeader,
    blk: *mut BlockHeader,
    size: usize,
) -> Result<*mut BlockHeader, MorlocError> {
    unsafe {
        if (*blk).size == size {
            return Ok(blk);
        }

        shm_lock(&(*shm).lock)?;

        let remaining = (*blk).size - size;
        (*blk).size = size;

        let hdr_size = std::mem::size_of::<BlockHeader>();
        let new_free = (blk as *mut u8).add(hdr_size + size) as *mut BlockHeader;

        if remaining > hdr_size {
            (*new_free).magic = BLK_MAGIC;
            (*new_free).reference_count = AtomicU32::new(0);
            (*new_free).size = remaining - hdr_size;

            // Update cursor
            let data_start = (shm as *const u8).add(std::mem::size_of::<ShmHeader>());
            (*shm).cursor = (new_free as *const u8).offset_from(data_start) as VolPtr;
        } else {
            (*blk).size += remaining;
            (*shm).cursor = VOLNULL;
        }

        shm_unlock(&(*shm).lock);
        Ok(blk)
    }
}

/// Convert a volume-local offset to an absolute pointer.
///
/// # Safety
/// `shm` must be a valid mmap'd ShmHeader. `ptr` must be within the volume's data region.
#[inline]
unsafe fn vol2abs_raw(ptr: VolPtr, shm: *const ShmHeader) -> *mut u8 {
    (shm as *const u8)
        .add(std::mem::size_of::<ShmHeader>())
        .add(ptr as usize) as *mut u8
}

// ── Futex-based lock ───────────────────────────────────────────────────────

/// Acquire a futex-based cross-process lock on shared memory.
///
/// # Safety
/// `lock` must point to an AtomicU32 in mmap'd shared memory that
/// persists for the duration of the lock. The caller must call shm_unlock
/// on the same lock when done.
pub unsafe fn shm_lock(lock: &AtomicU32) -> Result<(), MorlocError> {
    if lock
        .compare_exchange_weak(LOCK_UNLOCKED, LOCK_LOCKED, Ordering::Acquire, Ordering::Relaxed)
        .is_ok()
    {
        return Ok(());
    }

    for _ in 0..SPIN_LIMIT {
        std::hint::spin_loop();
        if lock
            .compare_exchange_weak(LOCK_UNLOCKED, LOCK_LOCKED, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            return Ok(());
        }
    }

    shm_lock_slow(lock)
}

#[cfg(target_os = "linux")]
unsafe fn shm_lock_slow(lock: &AtomicU32) -> Result<(), MorlocError> {
    let timeout = libc::timespec {
        tv_sec: LOCK_TIMEOUT_SECS as i64,
        tv_nsec: 0,
    };

    loop {
        let ptr = lock as *const AtomicU32 as *const u32;
        libc::syscall(
            libc::SYS_futex, ptr, libc::FUTEX_WAIT, LOCK_LOCKED,
            &timeout as *const libc::timespec, std::ptr::null::<u32>(), 0u32,
        );

        if lock
            .compare_exchange_weak(LOCK_UNLOCKED, LOCK_LOCKED, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            return Ok(());
        }

        if lock.load(Ordering::Relaxed) == LOCK_LOCKED {
            if lock
                .compare_exchange(LOCK_LOCKED, LOCK_UNLOCKED, Ordering::AcqRel, Ordering::Relaxed)
                .is_ok()
            {
                if lock
                    .compare_exchange(LOCK_UNLOCKED, LOCK_LOCKED, Ordering::Acquire, Ordering::Relaxed)
                    .is_ok()
                {
                    return Ok(());
                }
            }
        }
    }
}

/// macOS fallback: spin-yield loop (no futex available).
#[cfg(target_os = "macos")]
unsafe fn shm_lock_slow(lock: &AtomicU32) -> Result<(), MorlocError> {
    loop {
        std::thread::yield_now();
        if lock
            .compare_exchange_weak(LOCK_UNLOCKED, LOCK_LOCKED, Ordering::Acquire, Ordering::Relaxed)
            .is_ok()
        {
            return Ok(());
        }
    }
}

/// Release a futex-based cross-process lock on shared memory.
///
/// # Safety
/// `lock` must be the same AtomicU32 previously acquired via shm_lock.
pub unsafe fn shm_unlock(lock: &AtomicU32) {
    lock.store(LOCK_UNLOCKED, Ordering::Release);
    #[cfg(target_os = "linux")]
    {
        let ptr = lock as *const AtomicU32 as *const u32;
        libc::syscall(
            libc::SYS_futex, ptr, libc::FUTEX_WAKE, 1,
            std::ptr::null::<libc::timespec>(), std::ptr::null::<u32>(), 0u32,
        );
    }
    // macOS: no futex wake needed; spin-yield waiters will see the store.
}

// ── Pointer conversion helpers ─────────────────────────────────────────────

#[inline]
pub fn vol2rel(ptr: VolPtr, shm: &ShmHeader) -> RelPtr {
    shm.relative_offset as RelPtr + ptr
}

/// # Safety
/// `shm` must be a valid mmap'd ShmHeader. `ptr` must be within the volume's data region.
#[inline]
pub unsafe fn vol2abs(ptr: VolPtr, shm: *const ShmHeader) -> AbsPtr {
    vol2abs_raw(ptr, shm)
}

// ── Tests ──────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_block_header_no_padding() {
        assert_eq!(
            std::mem::size_of::<BlockHeader>(),
            4 + 4 + std::mem::size_of::<usize>()
        );
    }

    #[test]
    fn test_align_up() {
        assert_eq!(align_up(0, 8), 0);
        assert_eq!(align_up(1, 8), 8);
        assert_eq!(align_up(7, 8), 8);
        assert_eq!(align_up(8, 8), 8);
        assert_eq!(align_up(9, 8), 16);
    }

    #[test]
    fn test_pointer_constants() {
        assert_eq!(RELNULL, -1);
        assert_eq!(VOLNULL, -1);
    }

    #[test]
    fn test_lock_unlock() {
        let lock = AtomicU32::new(LOCK_UNLOCKED);
        unsafe {
            shm_lock(&lock).unwrap();
            assert_eq!(lock.load(Ordering::Relaxed), LOCK_LOCKED);
            shm_unlock(&lock);
            assert_eq!(lock.load(Ordering::Relaxed), LOCK_UNLOCKED);
        }
    }

    #[test]
    fn test_array_struct_size() {
        assert_eq!(
            std::mem::size_of::<Array>(),
            std::mem::size_of::<usize>() + std::mem::size_of::<RelPtr>()
        );
    }

    #[test]
    fn test_shinit_and_shmalloc() {
        // Use file-backed SHM via tmpdir to avoid /dev/shm permission issues in test
        let tmpdir = std::env::temp_dir();
        let test_dir = tmpdir.join(format!("morloc_test_{}", std::process::id()));
        std::fs::create_dir_all(&test_dir).unwrap();
        shm_set_fallback_dir(test_dir.to_str().unwrap());

        let basename = format!("test_shm_{}", std::process::id());
        let shm = shinit(&basename, 0, 4096).unwrap();
        assert!(!shm.is_null());
        assert_eq!(unsafe { (*shm).magic }, SHM_MAGIC);

        // Allocate some memory
        let ptr1 = shmalloc(64).unwrap();
        assert!(!ptr1.is_null());

        // Write and read back
        unsafe {
            std::ptr::write_bytes(ptr1, 0xAB, 64);
            assert_eq!(*ptr1, 0xAB);
        }

        // Convert to relptr and back
        let rel = abs2rel(ptr1).unwrap();
        assert!(rel >= 0);
        let abs = rel2abs(rel).unwrap();
        assert_eq!(abs, ptr1);

        // Free
        shfree(ptr1).unwrap();

        // Cleanup
        shclose().unwrap();
        let _ = std::fs::remove_dir_all(&test_dir);
    }
}
