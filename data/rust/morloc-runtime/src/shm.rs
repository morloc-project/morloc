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
    align_up, encode_relptr, relptr_is_sentinel, relptr_offset, relptr_volume_index,
    AbsPtr, Array, BlockHeader, MorlocVolEntry, RelPtr, ShmHeader, VolPtr,
    BLK_MAGIC, BLOCK_ALIGN, MAX_FILENAME_SIZE, MAX_PATH_SIZE, MAX_VOLUME_NUMBER,
    OFFSET_MASK, RELNULL, SHM_MAGIC, VOLNULL,
};

/// Cross-platform file pre-allocation, used by the file-backed fallback
/// path. The tmpfs path no longer goes through here -- it uses
/// `ftruncate` + `parallel_reserve_pages` so the slow page-allocate
/// work can run across multiple CPUs.
///
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

/// System page size, cached on first use. Used to align the per-worker
/// sub-ranges in `parallel_madvise_populate_write` -- `madvise`
/// requires page-aligned offsets and lengths.
fn page_size() -> usize {
    static CACHED: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *CACHED.get_or_init(|| {
        let v = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
        if v <= 0 { 4096 } else { v as usize }
    })
}

/// Worker count for parallel page reservation. Reuses the encoder's
/// `frame_workers()` cap so all parallel-CPU work in the runtime sees
/// the same `MORLOC_FRAME_WORKERS` override.
fn fallocate_workers(size: usize) -> usize {
    // Below ~64 MiB there's no headroom for parallel reservation to
    // pay back the thread-spawn cost; stay serial.
    if size < (64 << 20) {
        return 1;
    }
    morloc_runtime_types::compression::frame_workers()
}

/// Linux Option 3 primary: ask the kernel to populate every page in
/// the mapped region writable. After this returns 0, every page is
/// allocated, zero-filled, AND mapped into the calling VMA's page
/// table -- so subsequent writes don't take minor page faults either.
/// Returns 0 on full success; on any per-worker error the first
/// non-zero errno is returned. A return of EINVAL means the kernel is
/// older than Linux 5.14 and the caller should retry with
/// `parallel_posix_fallocate`.
#[cfg(target_os = "linux")]
unsafe fn parallel_madvise_populate_write(ptr: *mut u8, size: usize) -> i32 {
    let workers = fallocate_workers(size);
    // Below the parallel-payback threshold, skip the scope entirely
    // and call madvise inline. Avoids spawning one thread + joining
    // it just to do the same syscall.
    if workers == 1 {
        let r = libc::madvise(
            ptr as *mut libc::c_void,
            size,
            libc::MADV_POPULATE_WRITE,
        );
        return if r == 0 { 0 } else { *libc::__errno_location() };
    }
    let ps = page_size();
    let raw_chunk = size / workers;
    // Each worker's range must start on a page boundary.
    let chunk = if raw_chunk == 0 { ps } else { raw_chunk.div_ceil(ps) * ps };
    let ptr_addr = ptr as usize;
    let first_err = std::sync::atomic::AtomicI32::new(0);
    std::thread::scope(|s| {
        let mut handles = Vec::with_capacity(workers);
        for i in 0..workers {
            let off = i * chunk;
            if off >= size {
                break;
            }
            let len = (size - off).min(chunk);
            let first_err_ref = &first_err;
            let h = s.spawn(move || unsafe {
                let r = libc::madvise(
                    (ptr_addr as *mut libc::c_void).add(off),
                    len,
                    libc::MADV_POPULATE_WRITE,
                );
                if r != 0 {
                    let e = *libc::__errno_location();
                    let _ = first_err_ref.compare_exchange(
                        0,
                        e,
                        std::sync::atomic::Ordering::Relaxed,
                        std::sync::atomic::Ordering::Relaxed,
                    );
                }
            });
            handles.push(h);
        }
        for h in handles {
            let _ = h.join();
        }
    });
    first_err.load(std::sync::atomic::Ordering::Relaxed)
}

/// Linux fallback used when `MADV_POPULATE_WRITE` is unavailable
/// (kernel < 5.14, returns EINVAL). Splits `[0, size)` into N disjoint
/// sub-ranges and runs `posix_fallocate` on each in parallel.
/// `posix_fallocate` returns the error code directly (it does not
/// touch `errno`), so we collect the first non-zero return.
#[cfg(target_os = "linux")]
unsafe fn parallel_posix_fallocate(fd: i32, size: usize) -> i32 {
    let workers = fallocate_workers(size);
    if workers == 1 {
        return libc::posix_fallocate(fd, 0, size as i64);
    }
    let ps = page_size();
    let raw_chunk = size / workers;
    let chunk = if raw_chunk == 0 { ps } else { raw_chunk.div_ceil(ps) * ps };
    let first_err = std::sync::atomic::AtomicI32::new(0);
    std::thread::scope(|s| {
        let mut handles = Vec::with_capacity(workers);
        for i in 0..workers {
            let off = i * chunk;
            if off >= size {
                break;
            }
            let len = (size - off).min(chunk);
            let first_err_ref = &first_err;
            let h = s.spawn(move || unsafe {
                let r = libc::posix_fallocate(fd, off as i64, len as i64);
                if r != 0 {
                    let _ = first_err_ref.compare_exchange(
                        0,
                        r,
                        std::sync::atomic::Ordering::Relaxed,
                        std::sync::atomic::Ordering::Relaxed,
                    );
                }
            });
            handles.push(h);
        }
        for h in handles {
            let _ = h.join();
        }
    });
    first_err.load(std::sync::atomic::Ordering::Relaxed)
}

/// Reserve every page in `[ptr, ptr+size)` -- the same guarantee the
/// old single-threaded `posix_fallocate(fd, 0, size)` gave us, but
/// parallelized. On Linux 5.14+ uses `MADV_POPULATE_WRITE` (also
/// populates page tables, so no minor faults during the subsequent
/// data write); on older kernels falls back to parallel
/// `posix_fallocate` on the fd. Returns 0 on success; non-zero error
/// code (errno from madvise, return code from posix_fallocate) on
/// failure. macOS gets the noop path: tmpfs there is sparse by
/// default and lazy faulting is the convention.
#[cfg(target_os = "linux")]
unsafe fn parallel_reserve_pages(fd: i32, ptr: *mut u8, size: usize) -> i32 {
    let t0 = std::time::Instant::now();
    let workers = fallocate_workers(size);

    // Note: do NOT advise MADV_HUGEPAGE here. We benchmarked it and
    // shmem (MAP_SHARED tmpfs) THP made BOTH populate and the
    // subsequent parallel-write decompress significantly slower:
    //   * populate: 241 ms -> 610 ms (kernel serializes inode-lock
    //     work and may invoke memory compaction per 2 MiB page).
    //   * decompress: 1.18 s -> 1.96 s (40% throughput drop, from
    //     cache-line bouncing on huge pages shared between adjacent
    //     workers whose 16 MiB frames straddle 2 MiB boundaries when
    //     the SHM data region is not 2 MiB-aligned).
    // The read-only phases (adjust_relptrs, output walk+encode) saw
    // no benefit either -- single-threaded TLB pressure on the SHM
    // region is small enough that the savings get lost in noise.
    // If revisited, a hugetlbfs-backed mapping with explicit
    // MAP_HUGE_2MB would dodge the compaction + khugepaged paths but
    // require sysadmin setup. Not worth the complexity here.

    let err = parallel_madvise_populate_write(ptr, size);
    if err == 0 {
        crate::morloc_trace!(
            "[shmalloc] populate via madvise(POPULATE_WRITE), {} workers, {} in {:.2?}",
            workers,
            human_bytes(size),
            t0.elapsed()
        );
        return 0;
    }
    if err != libc::EINVAL {
        // ENOMEM (or anything else) -- propagate so the caller can
        // tear the tmpfs allocation down and fall back to file-backed.
        return err;
    }
    // EINVAL: kernel doesn't recognize MADV_POPULATE_WRITE (pre-5.14).
    // Retry with parallel posix_fallocate, which is functionally
    // identical for tmpfs (allocate + zero pages, mapped on first
    // fault).
    let t1 = std::time::Instant::now();
    let err = parallel_posix_fallocate(fd, size);
    if err == 0 {
        crate::morloc_trace!(
            "[shmalloc] populate via parallel posix_fallocate (madvise unsupported), {} workers, {} in {:.2?}",
            workers,
            human_bytes(size),
            t1.elapsed()
        );
    }
    err
}

#[cfg(target_os = "macos")]
unsafe fn parallel_reserve_pages(_fd: i32, _ptr: *mut u8, _size: usize) -> i32 {
    // macOS tmpfs is sparse-by-default; `ftruncate` already extended
    // the file and the kernel will allocate pages on first write.
    // Lazy faulting is the macOS convention; no upfront reservation
    // needed.
    0
}

// ── Internal lock constants ────────────────────────────────────────────────

const LOCK_UNLOCKED: u32 = 0;
const LOCK_LOCKED: u32 = 1;
const SPIN_LIMIT: u32 = 40;
#[cfg(target_os = "linux")]
const LOCK_TIMEOUT_SECS: u64 = 5;

// ── Slot record (one per volume index) ─────────────────────────────────────

/// One VOLUMES entry. Holds a pointer to the slot's ShmHeader plus a
/// cached copy of `(*header).volume_size`. The cache exists so
/// `rel2abs` can do its bounds check without dereferencing the header
/// on every call -- `volume_size` lives at byte offset 136 of
/// ShmHeader (past the 128-byte name buffer), a separate cache line
/// from the slot pointer that the table walk has already loaded.
///
/// `data_base` is intentionally NOT cached: it is the compile-time
/// constant offset `header + sizeof::<ShmHeader>()`, so reading it
/// from the slot would only save a pointer add, no memory traffic.
///
/// Empty slots have `header.is_null()` and `data_size == 0`.
#[derive(Clone, Copy)]
struct SendPtr {
    header: *mut ShmHeader,
    data_size: usize,
}

// SAFETY: ShmHeader lives in mmap'd shared memory that outlives all threads.
// Access is serialized via VOLUMES Mutex and per-volume AtomicU32 futex locks.
unsafe impl Send for SendPtr {}

impl SendPtr {
    const fn null() -> Self {
        SendPtr { header: std::ptr::null_mut(), data_size: 0 }
    }
    fn is_null(&self) -> bool { self.header.is_null() }
    fn ptr(&self) -> *mut ShmHeader { self.header }
    fn data_size(&self) -> usize { self.data_size }
    fn set(&mut self, header: *mut ShmHeader, data_size: usize) {
        self.header = header;
        self.data_size = data_size;
    }
}

fn get_cstr_buf(buf: &[u8; MAX_FILENAME_SIZE]) -> &str {
    get_cstr(buf.as_slice())
}

// ── Exposed per-process volume table (lock-free, public symbol) ────────────

/// Per-process volume base+size table, exposed as a public symbol so
/// C/C++/Python/R bridges can do `rel2abs` inline without an FFI call.
///
/// Each slot is two atomics with naturally-aligned, naturally-lock-free
/// types. The publication protocol:
///
/// * **Populate** (`shinit`/`shopen_diag`): store `data_size` first
///   (Relaxed -- readers ignore it until the gate flips), then store
///   `data_base` with Release. The Release pairs with the reader's
///   Acquire of `data_base` and makes `data_size` visible.
/// * **Invalidate** (`shclose`/`reset_all`): store `data_base = null`
///   with Release. Subsequent readers see null and fall through to the
///   FFI slow path, which will either find the segment unmapped or
///   trigger a fresh shopen.
/// * **Read** (C inline `resolve_relptr` in morloc.h): Acquire-load
///   `data_base`; if non-null, Relaxed-load `data_size`, bounds-check,
///   return `data_base + offset`. Else fall through to `rel2abs`.
///
/// Sized at MAX_VOLUME_NUMBER (32 768). With 16 bytes per entry that's
/// 512 KiB of process-local static data.
#[no_mangle]
pub static MORLOC_VOL_TABLE: [MorlocVolEntry; MAX_VOLUME_NUMBER] =
    [const { MorlocVolEntry::empty() }; MAX_VOLUME_NUMBER];

/// Publish a volume's mapping to the lock-free table so the C-side
/// `resolve_relptr` inline can resolve relptrs into it without FFI.
#[inline]
fn publish_vol(idx: usize, header: *mut ShmHeader, data_size: usize) {
    if idx >= MAX_VOLUME_NUMBER {
        return;
    }
    let entry = &MORLOC_VOL_TABLE[idx];
    // SAFETY: header is a valid mmap'd ShmHeader pointer; data region
    // starts at header + sizeof::<ShmHeader>().
    let data_base = unsafe {
        (header as *mut u8).add(std::mem::size_of::<ShmHeader>())
    };
    // Store data_size before publishing data_base; readers gate on the
    // Release-store of data_base, so size is visible by the time they
    // observe a non-null base.
    entry.data_size.store(data_size, Ordering::Relaxed);
    entry.data_base.store(data_base, Ordering::Release);
}

/// Withdraw a volume's mapping from the lock-free table. Subsequent
/// `resolve_relptr` inline calls for this slot will fall through to
/// the FFI.
#[inline]
fn unpublish_vol(idx: usize) {
    if idx >= MAX_VOLUME_NUMBER {
        return;
    }
    MORLOC_VOL_TABLE[idx]
        .data_base
        .store(std::ptr::null_mut(), Ordering::Release);
}

// ── Global state ───────────────────────────────────────────────────────────

/// Hint for `find_free_block`: the slot index where the last allocation
/// landed. Not load-bearing; if stale or null the allocator falls back
/// to the USED_VOLUMES walk.
static CURRENT_VOLUME: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

/// Sparse 32 768-entry volume table.
///
/// `slots`: indexed directly by the relptr's volume-index field. Most
/// entries are null; `rel2abs` reads `slots[idx]` in O(1).
///
/// `used`: packed list of currently-occupied slot indices. Walks that
/// historically iterated `0..MAX_VOLUME_NUMBER` (shclose, abs2rel,
/// total_shm_size, etc) iterate `used` instead and visit only the
/// active K, not 32 K nulls. Maintained as a no-order Vec; on free
/// we swap_remove the slot's entry.
struct VolumeTable {
    slots: [SendPtr; MAX_VOLUME_NUMBER],
    used: Vec<u16>,
}

impl VolumeTable {
    fn mark_used(&mut self, idx: usize) {
        // Caller has already verified slots[idx] is non-null. Avoid
        // a duplicate entry if the same slot is re-registered.
        if !self.used.iter().any(|&i| i as usize == idx) {
            self.used.push(idx as u16);
        }
    }
}

static VOLUMES: Mutex<VolumeTable> = Mutex::new(VolumeTable {
    slots: [SendPtr::null(); MAX_VOLUME_NUMBER],
    used: Vec::new(),
});

static ALLOC_MUTEX: Mutex<()> = Mutex::new(());

// ── Thread-local PRNG (for random slot allocation) ─────────────────────────

thread_local! {
    static RNG_STATE: std::cell::Cell<u64> = std::cell::Cell::new(rng_seed());
}

fn rng_seed() -> u64 {
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0);
    // SAFETY: pthread_self always succeeds; the value is opaque but unique
    // per live thread, which is all we need to differentiate seeds.
    let tid = unsafe { libc::pthread_self() as u64 };
    let mut s = nanos
        .wrapping_mul(0x9E37_79B9_7F4A_7C15)
        .wrapping_add(tid.wrapping_mul(0xBF58_476D_1CE4_E5B9));
    if s == 0 {
        s = 0xA5A5_A5A5_A5A5_A5A5;
    }
    s
}

/// xorshift64 -- unbiased enough for random slot picking; not
/// cryptographic. Local to this module; called only inside the
/// allocator path.
fn next_random_u64() -> u64 {
    RNG_STATE.with(|cell| {
        let mut x = cell.get();
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        cell.set(x);
        x
    })
}

/// Pick a random unoccupied slot in `slots`. Tries random uniform
/// picks first (the common case at low occupancy), falls back to a
/// linear probe from a random start (covers the dense case where
/// random picks keep colliding).
fn pick_free_slot(table: &VolumeTable) -> Option<usize> {
    if table.used.len() >= MAX_VOLUME_NUMBER {
        return None;
    }
    for _ in 0..8 {
        let idx = (next_random_u64() as usize) & (MAX_VOLUME_NUMBER - 1);
        if table.slots[idx].is_null() {
            return Some(idx);
        }
    }
    let start = (next_random_u64() as usize) & (MAX_VOLUME_NUMBER - 1);
    for off in 0..MAX_VOLUME_NUMBER {
        let idx = (start + off) & (MAX_VOLUME_NUMBER - 1);
        if table.slots[idx].is_null() {
            return Some(idx);
        }
    }
    None
}

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
    for &slot_idx in &vols_guard.used {
        let shm = vols_guard.slots[slot_idx as usize].ptr();
        if shm.is_null() {
            continue;
        }
        unpublish_vol(slot_idx as usize);
        // SAFETY: shm is a valid mmap'd pointer recorded in VOLUMES under
        // the lock we hold. Name is read from a valid ShmHeader region.
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

    // Try POSIX shared memory first, fall back to file-backed.
    // `try_open_tmpfs` does its own mmap + parallel page reservation
    // (the work that used to be a single-threaded `posix_fallocate`).
    // It returns Ok(None) when the tmpfs path isn't viable -- either
    // `shm_open` failed or the reservation ran out of room. We then
    // fall through to the slower-but-correct file-backed path.
    let opened = match try_open_tmpfs(&shm_name, full_size)? {
        Some(v) => v,
        None => try_open_file_backed(&shm_name, full_size)?,
    };

    let fd = opened.fd;
    let shm = opened.ptr;
    let actual_full_size = opened.actual_size;
    let created = opened.created;
    let volume_label = opened.label;
    // SAFETY: fd is a valid file descriptor opened by try_open_*; we no
    // longer need it after the mapping is established (the mmap'd
    // region survives the close on both Linux and macOS).
    unsafe { libc::close(fd) };

    let actual_data_size = actual_full_size - std::mem::size_of::<ShmHeader>();

    // Store in volumes table and add to USED_VOLUMES walk list.
    {
        let mut vols = VOLUMES.lock().unwrap();
        vols.slots[volume_index].set(shm, actual_data_size);
        vols.mark_used(volume_index);
    }
    publish_vol(volume_index, shm, actual_data_size);

    if created {
        // SAFETY: shm points to the start of our mmap'd region of actual_full_size bytes.
        // We just created it, so we have exclusive access for initialization.
        unsafe {
            (*shm).magic = SHM_MAGIC;
            let mut name_buf = [0u8; MAX_FILENAME_SIZE];
            set_cstr(&mut name_buf, &volume_label);
            (*shm).volume_name = name_buf;
            (*shm).volume_index = volume_index as i32;
            // `relative_offset` is unused under the indexed-relptr encoding;
            // each relptr now carries its own volume index in the high bits.
            // Kept zero for layout compatibility with consumers that still
            // read the field.
            (*shm).relative_offset = 0;
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
        if !vols.slots[volume_index].is_null() {
            return Ok(Ok(vols.slots[volume_index].ptr()));
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
    // SAFETY: shm is a freshly mmap'd ShmHeader; we read volume_size
    // once at registration so the per-call cache in SendPtr is
    // populated for the lifetime of the slot.
    let data_size = unsafe { (*shm).volume_size };
    {
        let mut vols = VOLUMES.lock().unwrap();
        vols.slots[volume_index].set(shm, data_size);
        vols.mark_used(volume_index);
    }
    publish_vol(volume_index, shm, data_size);

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
    for &slot_idx in &vols.used {
        let slot = vols.slots[slot_idx as usize];
        if slot.is_null() {
            continue;
        }
        let base = unsafe {
            (slot.ptr() as *const u8).add(std::mem::size_of::<ShmHeader>())
        } as usize;
        if p >= base && p < base + slot.data_size() {
            return true;
        }
    }
    false
}

/// Internal: do the unlink/munmap walk under already-held locks.
fn shclose_locked(vols: &mut VolumeTable) {
    // Drain `used` once into a local list; `mark_free` would O(N) on
    // each removal otherwise.
    let used: Vec<u16> = std::mem::take(&mut vols.used);
    for slot_idx in used {
        let i = slot_idx as usize;
        let shm = vols.slots[i].ptr();
        if shm.is_null() {
            continue;
        }
        // Withdraw from the lock-free table BEFORE we unmap, so any
        // racing rel2abs inline can't read a stale data_base and
        // dereference an unmapped page. The Release-store ensures the
        // null is visible before munmap is observed.
        unpublish_vol(i);
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
        vols.slots[i] = SendPtr::null();
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

/// Return the allocation size of an SHM block, in O(1), reading the
/// `BlockHeader` placed immediately before `ptr` by `shmalloc`.
///
/// Returns `None` when `ptr` is null, not within any mapped SHM volume,
/// or carries a corrupted/missing block magic -- conditions under which
/// the header bytes cannot be trusted (the pointer may be heap/stack,
/// an interior sub-pointer that doesn't sit at the start of an
/// allocation, or memory from a torn-down volume). Callers can use a
/// `None` return to fall back to a full walk.
///
/// The returned size is the rounded-up allocation size (aligned to
/// `BLOCK_ALIGN`), not the original `shmalloc(size)` request -- callers
/// that need the exact serialized voidstar size still have to walk the
/// structure. The block size is suitable for use as an upper bound in
/// inline-vs-RPTR and streaming-threshold decisions, where the encoder
/// later computed `pledgedSrcSize` from the same `calc_voidstar_size_inner`
/// that this block was sized from.
pub unsafe fn shm_block_size(ptr: AbsPtr) -> Option<usize> {
    if ptr.is_null() {
        return None;
    }
    let header_ptr = ptr.sub(std::mem::size_of::<BlockHeader>());
    if !ptr_is_in_any_volume(header_ptr) {
        return None;
    }
    let header = &*(header_ptr as *const BlockHeader);
    if header.magic != BLK_MAGIC {
        return None;
    }
    Some(header.size)
}

/// Convert relative pointer to absolute pointer. O(1) under the
/// indexed-relptr encoding: a relptr packs `(volume_index, offset)`
/// into a 64-bit word, so we read the index, look up VOLUMES[idx],
/// and add the offset.
///
/// If the volume is not yet mapped in this process (cross-process
/// reader), `shopen_diag(idx)` lazily mmaps `<basename>_<idx>` and
/// records it.
pub fn rel2abs(ptr: RelPtr) -> Result<AbsPtr, MorlocError> {
    if relptr_is_sentinel(ptr) {
        // RELNULL and any future reserved sentinel are not addressable.
        return Err(MorlocError::Shm(format!(
            "rel2abs called on sentinel relptr {}",
            ptr
        )));
    }
    let vol_idx = relptr_volume_index(ptr);
    let offset = relptr_offset(ptr);

    // Fast path: volume already mapped in this process. Read the slot
    // record (header pointer + cached data_size, both in the same
    // SendPtr cache line) under the lock, then drop the lock before
    // computing the address.
    let slot = {
        let vols = VOLUMES.lock().unwrap();
        vols.slots[vol_idx]
    };
    if !slot.is_null() {
        if offset >= slot.data_size() {
            return Err(MorlocError::Shm(format!(
                "rel2abs offset {} exceeds volume {}'s size {}",
                offset, vol_idx, slot.data_size()
            )));
        }
        // SAFETY: slot.header is a valid mmap'd ShmHeader and the
        // bounds check above keeps the add inside the data region.
        unsafe {
            let base = (slot.ptr() as *const u8)
                .add(std::mem::size_of::<ShmHeader>());
            return Ok(base.add(offset) as AbsPtr);
        }
    }

    // Slow path: try to lazily open the producer's volume from disk
    // (POSIX SHM or file-backed fallback). shopen_diag populates the
    // slot's data_size cache as part of its work, so we re-read the
    // slot afterwards rather than dereferencing the header here.
    let _ = match shopen_diag(vol_idx)? {
        Ok(s) => s,
        Err(miss) => return Err(rel2abs_miss_error(ptr, vol_idx, 0, miss)),
    };
    let slot = {
        let vols = VOLUMES.lock().unwrap();
        vols.slots[vol_idx]
    };
    if slot.is_null() {
        return Err(MorlocError::Shm(format!(
            "rel2abs: shopen_diag claimed success for volume {} but slot is null",
            vol_idx
        )));
    }
    if offset >= slot.data_size() {
        return Err(MorlocError::Shm(format!(
            "rel2abs offset {} exceeds volume {}'s size {}",
            offset, vol_idx, slot.data_size()
        )));
    }
    // SAFETY: same as the fast path.
    unsafe {
        let base = (slot.ptr() as *const u8)
            .add(std::mem::size_of::<ShmHeader>());
        Ok(base.add(offset) as AbsPtr)
    }
}

/// Build a user-facing error for an `shopen` miss inside `rel2abs`. Each
/// `ShopenMiss` variant has a different root cause and a different fix,
/// so they get different messages instead of the legacy generic
/// "Failed to find volume". The indexed-relptr encoding makes the
/// decoded `(vol_idx, offset)` pair the actionable piece of information,
/// since `rel2abs` is now O(1) and doesn't iterate over a population
/// of mapped volumes.
fn rel2abs_miss_error(
    ptr: RelPtr,
    volume_index: usize,
    _already_mapped_bytes: usize,
    miss: ShopenMiss,
) -> MorlocError {
    let basename_now = {
        let cb = COMMON_BASENAME.lock().unwrap();
        get_cstr_buf(&cb).to_string()
    };
    let offset = relptr_offset(ptr);
    match miss {
        ShopenMiss::NotInitialized => MorlocError::Shm(format!(
            "cannot resolve relptr {} (decoded as vol_idx={}, offset={}) -- SHM is \
             not initialized in this process (COMMON_BASENAME is empty). Caller \
             reached rel2abs before any shinit; if you see this after the \
             rlib-removal refactor it most likely means a foreign caller bypassed \
             the dispatch path.",
            ptr, volume_index, offset
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
                "cannot resolve relptr {} (decoded as vol_idx={}, offset={}) -- \
                 SHM volume '/dev/shm/{}_{}' does not exist (shm_open: {}).{} {}. \
                 Likely causes: writer never created this volume, writer crashed \
                 before sending, basename mismatch between writer and reader, or \
                 another process (or /dev/shm cleanup) unlinked it.",
                ptr, vi, offset, basename, vi, errno_msg, basename_note, fallback_note
            ))
        }
    }
}

/// Convert absolute pointer to relative pointer. Walks `USED_VOLUMES`
/// (the packed list of K active slot indices) rather than scanning the
/// 32 768-slot sparse `slots` array. Cost is O(K_active).
pub fn abs2rel(ptr: AbsPtr) -> Result<RelPtr, MorlocError> {
    let vols = VOLUMES.lock().unwrap();
    for &slot_idx in &vols.used {
        let i = slot_idx as usize;
        let slot = vols.slots[i];
        if slot.is_null() {
            continue;
        }
        // SAFETY: data_base = header + sizeof::<ShmHeader>(); the slot's
        // cached data_size bounds the search range. ptr is matched
        // against the half-open interval [data_start, data_start +
        // data_size) before any pointer arithmetic returns.
        unsafe {
            let data_start = (slot.ptr() as *const u8)
                .add(std::mem::size_of::<ShmHeader>());
            let data_end = data_start.add(slot.data_size());
            let p = ptr as *const u8;
            if p >= data_start && p < data_end {
                let offset = p.offset_from(data_start) as usize;
                return Ok(encode_relptr(i, offset));
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
    for &slot_idx in &vols.used {
        let slot = vols.slots[slot_idx as usize];
        if slot.is_null() {
            continue;
        }
        // SAFETY: see abs2rel.
        unsafe {
            let data_start = (slot.ptr() as *const u8)
                .add(std::mem::size_of::<ShmHeader>());
            let data_end = data_start.add(slot.data_size());
            let p = ptr as *const u8;
            if p >= data_start && p < data_end {
                return Ok(slot.ptr());
            }
        }
    }
    Err(MorlocError::Shm("Failed to find absptr in SHM".into()))
}

/// Register a pre-mapped ShmHeader-backed region as a volume.
///
/// `slot_hint = Some(i)` requests that index; if `i` is occupied, falls
/// back to a randomly chosen free slot. `slot_hint = None` always picks
/// random.
///
/// The caller owns the mmap'd region for as long as the volume is
/// registered. Layer 2 (packet-as-volume) uses this to map an input
/// file's data section directly into VOLUMES without a memcpy.
///
/// Returns the chosen slot index, or an error if every slot is taken.
/// Total size of all SHM volumes.
pub fn total_shm_size() -> usize {
    let vols = VOLUMES.lock().unwrap();
    let mut total = 0;
    for &slot_idx in &vols.used {
        let slot = vols.slots[slot_idx as usize];
        if !slot.is_null() {
            total += slot.data_size();
        }
    }
    total
}

// ── Internal helpers ───────────────────────────────────────────────────────

/// Format a byte count as "N.N GiB" / "N.N MiB" / "N KiB" / "N B" for
/// user-facing diagnostics. Cutoffs match the obvious thresholds.
fn human_bytes(n: usize) -> String {
    const KIB: usize = 1024;
    const MIB: usize = 1024 * KIB;
    const GIB: usize = 1024 * MIB;
    if n >= GIB {
        format!("{:.1} GiB", n as f64 / GIB as f64)
    } else if n >= MIB {
        format!("{:.1} MiB", n as f64 / MIB as f64)
    } else if n >= KIB {
        format!("{} KiB", n / KIB)
    } else {
        format!("{} B", n)
    }
}

/// Volume successfully opened, sized, and (for newly-created volumes)
/// page-reserved. `fd` is still open; caller is responsible for
/// `libc::close(fd)` after recording the mapping in VOLUMES.
struct OpenedVolume {
    fd: libc::c_int,
    ptr: *mut ShmHeader,
    created: bool,
    label: String,
    actual_size: usize,
}

/// Try to open a tmpfs (POSIX SHM) volume. On Linux 5.14+ this uses
/// `ftruncate + mmap + madvise(POPULATE_WRITE)` across `frame_workers()`
/// threads to reserve pages in parallel; on older kernels it falls
/// back to parallel `posix_fallocate`. Returns `None` when:
///   * `shm_open` fails entirely (e.g. `/dev/shm` doesn't exist), or
///   * the page reservation fails (e.g. `/dev/shm` exists but is too
///     small for `full_size`) -- in which case we have already cleaned
///     up via `shm_unlink` so the caller can immediately fall through
///     to the file-backed path.
/// Returns `Err` only for hard / unexpected failures (`fstat` failing,
/// `mmap` failing) that the caller should propagate.
fn try_open_tmpfs(
    shm_name: &str,
    full_size: usize,
) -> Result<Option<OpenedVolume>, MorlocError> {
    let name_cstr = std::ffi::CString::new(shm_name).unwrap();
    let fd = unsafe {
        libc::shm_open(
            name_cstr.as_ptr(),
            libc::O_RDWR | libc::O_CREAT,
            0o666,
        )
    };
    if fd < 0 {
        return Ok(None);
    }
    let mut sb: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd, &mut sb) } == -1 {
        unsafe { libc::close(fd) };
        return Err(MorlocError::Shm(format!("fstat failed for '{}'", shm_name)));
    }
    let created = sb.st_size == 0;
    let actual_size = if created { full_size } else { sb.st_size as usize };

    if created {
        // Set the file size; pages are NOT allocated yet.
        if unsafe { libc::ftruncate(fd, full_size as i64) } != 0 {
            unsafe {
                libc::close(fd);
                libc::shm_unlink(name_cstr.as_ptr());
            }
            // ftruncate on a freshly-shm_open'd file failing is
            // unusual but not catastrophic; fall through to file-backed.
            return Ok(None);
        }
    }

    let ptr = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            actual_size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            fd,
            0,
        )
    };
    if ptr == libc::MAP_FAILED {
        unsafe {
            libc::close(fd);
            if created {
                libc::shm_unlink(name_cstr.as_ptr());
            }
        }
        return Err(MorlocError::Shm(format!(
            "Failed to mmap tmpfs volume '{}' ({} bytes)",
            shm_name, actual_size
        )));
    }

    if created {
        // Reserve every page upfront (atomically detects /dev/shm OOM
        // before any data write can `SIGBUS`). On Linux this is the
        // parallel madvise/fallocate path; on macOS it's a noop
        // (sparse tmpfs + lazy faulting).
        let err = unsafe {
            parallel_reserve_pages(fd, ptr as *mut u8, full_size)
        };
        if err != 0 {
            unsafe {
                libc::munmap(ptr, actual_size);
                libc::close(fd);
                libc::shm_unlink(name_cstr.as_ptr());
            }
            // Page reservation failed (typically ENOMEM /
            // ENOSPC on a too-small `/dev/shm`). Fall through to
            // file-backed.
            return Ok(None);
        }
    }

    Ok(Some(OpenedVolume {
        fd,
        ptr: ptr as *mut ShmHeader,
        created,
        label: shm_name.to_string(),
        actual_size,
    }))
}

/// Open the file-backed fallback volume. Uses the single-threaded
/// `preallocate_fd` (= `posix_fallocate` on Linux, `ftruncate` on
/// macOS) -- this path is uncommon (only reached when `/dev/shm` is
/// too small) and correctness matters more than speed.
fn try_open_file_backed(
    shm_name: &str,
    full_size: usize,
) -> Result<OpenedVolume, MorlocError> {
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
        // We successfully created a file-backed segment because POSIX
        // SHM couldn't host it. Notify the user once per fallback site
        // -- this is a working but slower path (data lives on whatever
        // backs the fallback dir, often a regular filesystem), and the
        // typical cause in containers is a small /dev/shm. The fallback
        // itself is intentional graceful degradation, not an error.
        eprintln!(
            "morloc warning: /dev/shm is too small for a {} allocation; \
             falling back to file-backed '{}'.",
            human_bytes(full_size),
            file_path,
        );
        full_size
    } else {
        sb.st_size as usize
    };

    let ptr = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            actual_size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            fd,
            0,
        )
    };
    if ptr == libc::MAP_FAILED {
        unsafe {
            libc::close(fd);
            if created {
                libc::unlink(path_cstr.as_ptr());
            }
        }
        return Err(MorlocError::Shm(format!(
            "Failed to mmap file-backed volume '{}' ({} bytes)",
            file_path, actual_size
        )));
    }

    Ok(OpenedVolume {
        fd,
        ptr: ptr as *mut ShmHeader,
        created,
        label: file_path,
        actual_size,
    })
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

    // Try current volume first (allocation hint).
    let shm = vols.slots[cv].ptr();
    if !shm.is_null() {
        if let Some(blk) = find_free_block_in_volume(shm, size)? {
            *shm_out = shm;
            return Ok(blk);
        }
    }

    // Fall back to scanning all currently-occupied volumes.
    for &slot_idx in &vols.used {
        let i = slot_idx as usize;
        if i == cv {
            continue;
        }
        let shm = vols.slots[i].ptr();
        if shm.is_null() {
            continue;
        }
        if let Some(blk) = find_free_block_in_volume(shm, size)? {
            CURRENT_VOLUME.store(i, Ordering::Relaxed);
            *shm_out = shm;
            return Ok(blk);
        }
    }

    // No existing volume has space; grow into a randomly-chosen free
    // slot. Geometric growth (K x previous size) keeps total capacity
    // expanding exponentially so MAX_VOLUME_NUMBER is unreachable in
    // practice.
    const VOLUME_GROWTH_FACTOR: usize = 2;
    let prev_volume_size = {
        let last = vols.slots[cv];
        if !last.is_null() {
            last.data_size()
        } else {
            0xffff
        }
    };
    let new_size = std::cmp::max(
        size.saturating_add(std::mem::size_of::<BlockHeader>()),
        prev_volume_size.saturating_mul(VOLUME_GROWTH_FACTOR),
    );

    let new_idx = match pick_free_slot(&vols) {
        Some(i) => i,
        None => {
            return Err(MorlocError::Shm(format!(
                "Could not find suitable block for {} bytes: all {} \
                 volume slots are occupied",
                size, MAX_VOLUME_NUMBER
            )));
        }
    };
    drop(vols);

    let basename = {
        let cb = COMMON_BASENAME.lock().unwrap();
        get_cstr_buf(&cb).to_string()
    };
    let new_shm = shinit(&basename, new_idx, new_size)?;
    CURRENT_VOLUME.store(new_idx, Ordering::Relaxed);
    *shm_out = new_shm;
    let blk = unsafe {
        (new_shm as *mut u8).add(std::mem::size_of::<ShmHeader>()) as *mut BlockHeader
    };
    Ok(blk)
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
    // Under the indexed-relptr encoding, the volume index lives in the
    // high bits of the relptr and the volume-local offset (ptr) lives
    // in the low 48 bits.
    encode_relptr(shm.volume_index as usize, ptr as usize)
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
    fn vol_table_publication_roundtrip() {
        // shinit / shopen_diag publish a (data_base, data_size) pair to
        // MORLOC_VOL_TABLE so the C-side resolve_relptr inline can do
        // its lookup without crossing FFI. Verify the publish helpers
        // expose the right values to an Acquire reader, and that the
        // unpublish step clears the slot.
        let tmpdir = std::env::temp_dir();
        let test_dir = tmpdir.join(format!("morloc_test_vt_{}", std::process::id()));
        std::fs::create_dir_all(&test_dir).unwrap();
        shm_set_fallback_dir(test_dir.to_str().unwrap());

        let basename = format!("test_shm_vt_{}", std::process::id());
        let shm = shinit(&basename, 0, 4096).unwrap();

        // shinit should have populated slot 0.
        let entry = &MORLOC_VOL_TABLE[0];
        let base = entry.data_base.load(Ordering::Acquire);
        let size = entry.data_size.load(Ordering::Relaxed);
        assert!(!base.is_null(),
            "expected publish_vol to populate slot 0's data_base");
        assert!(size > 0,
            "expected publish_vol to populate slot 0's data_size");
        // data_base must point to the data region right after the header.
        let expected_base = unsafe {
            (shm as *mut u8).add(std::mem::size_of::<ShmHeader>())
        };
        assert_eq!(base, expected_base,
            "data_base must be header + sizeof::<ShmHeader>()");

        shclose().unwrap();

        // shclose drops the slot back to null.
        let base_after = MORLOC_VOL_TABLE[0].data_base.load(Ordering::Acquire);
        assert!(base_after.is_null(),
            "expected shclose to publish a null data_base");

        let _ = std::fs::remove_dir_all(&test_dir);
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
    fn test_indexed_relptr_roundtrip_across_volumes() {
        // Verify that rel2abs/abs2rel commute under the new indexed
        // encoding: every (slot_idx, offset) pair we allocate into can
        // be encoded to a relptr and decoded back to the same absolute
        // address. Stresses the multi-volume case where the old
        // flat-offset encoding required summing prior volume sizes.
        let tmpdir = std::env::temp_dir();
        let test_dir = tmpdir.join(format!("morloc_test_idx_{}", std::process::id()));
        std::fs::create_dir_all(&test_dir).unwrap();
        shm_set_fallback_dir(test_dir.to_str().unwrap());

        let basename = format!("test_shm_idx_{}", std::process::id());
        shinit(&basename, 0, 4096).unwrap();

        // Force grow into several randomly-allocated volumes by
        // allocating much more than the initial 4 KiB volume can hold.
        let mut allocs = Vec::new();
        for i in 0..128 {
            let p = shmalloc(2048).unwrap();
            unsafe {
                std::ptr::write_bytes(p, (i & 0xFF) as u8, 2048);
            }
            allocs.push(p);
        }
        // Confirm we actually exercised the multi-volume path.
        let used_count = VOLUMES.lock().unwrap().used.len();
        assert!(
            used_count >= 2,
            "expected at least 2 volumes after 128 allocs of 2 KiB, got {}",
            used_count
        );

        // Round-trip each: abs -> rel -> abs must yield the original.
        for (i, &abs) in allocs.iter().enumerate() {
            let rel = abs2rel(abs).unwrap();
            assert!(!relptr_is_sentinel(rel), "alloc {}: rel had sentinel bit", i);
            assert!(relptr_volume_index(rel) < MAX_VOLUME_NUMBER);
            let round = rel2abs(rel).unwrap();
            assert_eq!(abs, round, "alloc {} round-trip mismatch", i);
            // Data still readable through the round-tripped pointer.
            unsafe {
                assert_eq!(*round, (i & 0xFF) as u8);
            }
        }

        // RELNULL must come back as an error from rel2abs.
        assert!(rel2abs(RELNULL).is_err());

        // Cleanup.
        for p in allocs {
            shfree(p).unwrap();
        }
        shclose().unwrap();
        let _ = std::fs::remove_dir_all(&test_dir);
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

    #[test]
    fn large_alloc_writes_every_page_without_crashing() {
        // Smoke test for the parallel page-reservation path: allocate
        // a region larger than the 64 MiB serial-fallback cutoff in
        // `fallocate_workers`, then write every page. If the new
        // tmpfs-with-madvise path is correct, every page is backed
        // before our writes; if anything went wrong in `try_open_tmpfs`
        // we'd either crash on write (page-fault SIGBUS) or fall back
        // to the file-backed path (still correct, just slower).
        let tmpdir = std::env::temp_dir();
        let test_dir = tmpdir.join(format!("morloc_test_large_{}", std::process::id()));
        std::fs::create_dir_all(&test_dir).unwrap();
        shm_set_fallback_dir(test_dir.to_str().unwrap());

        let basename = format!("test_shm_large_{}", std::process::id());
        // 128 MiB requested; that's > 64 MiB so `fallocate_workers`
        // returns >= 1 worker, and on Linux 5.14+ this exercises
        // `parallel_madvise_populate_write`.
        let alloc_size = 128 * 1024 * 1024;
        shinit(&basename, 0, alloc_size).unwrap();
        let p = shmalloc(alloc_size).unwrap();
        let ps = page_size();
        let n_pages = alloc_size / ps;
        // Touch one byte per page across the whole region.
        unsafe {
            for i in 0..n_pages {
                std::ptr::write(p.add(i * ps), (i & 0xFF) as u8);
            }
            // Spot-check a few pages.
            for &i in &[0usize, n_pages / 2, n_pages - 1] {
                assert_eq!(*p.add(i * ps), (i & 0xFF) as u8);
            }
        }
        shfree(p).unwrap();
        shclose().unwrap();
        let _ = std::fs::remove_dir_all(&test_dir);
    }
}
