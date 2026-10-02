//! Shared memory management with multi-volume support.
//!
//! Every volume carries a cross-process lock (`ShmLock`) over its block list.

use crate::error::MorlocError;
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
use std::sync::Mutex;
use morloc_runtime_types::shm_lock::{ShmGuard, ShmLock};

// Wire-format types and constants live in `morloc-runtime-types::shm_types`
// so they can be shared with the nexus without duplicating process state.
// Re-exported here so existing call sites (`crate::shm::RelPtr`,
// `crate::shm::Array`, etc.) keep working unchanged.
pub use morloc_runtime_types::shm_types::{
    align_up, encode_relptr, relptr_is_sentinel, relptr_offset, relptr_volume_index,
    AbsPtr, Array, BlockHeader, MorlocVolEntry, RelPtr, ShmHeader, VolPtr,
    BLK_ABSORBED, BLK_MAGIC, BLOCK_ALIGN, MAX_FILENAME_SIZE, MAX_PATH_SIZE, MAX_VOLUME_NUMBER, PRIMARY_VOLUME,
    OFFSET_MASK, RELNULL, SHM_MAGIC, VOLNULL,
};

/// Cross-platform file pre-allocation, used by the file-backed fallback
/// path. The tmpfs path no longer goes through here -- it uses
/// `ftruncate` + `parallel_reserve_pages` so the slow page-allocate
/// work can run across multiple CPUs.
///
/// Disk blocks are reserved up front, so a full disk fails here with an
/// error rather than later with SIGBUS on a write into an unbacked page.
/// Returns 0 or an errno value.
///
/// Linux: posix_fallocate. macOS has none: F_PREALLOCATE reserves the
/// blocks, then ftruncate sets the length.
#[cfg(target_os = "linux")]
unsafe fn preallocate_fd(fd: i32, size: i64) -> i32 {
    libc::posix_fallocate(fd, 0, size)
}

#[cfg(target_os = "macos")]
unsafe fn preallocate_fd(fd: i32, size: i64) -> i32 {
    let mut store = libc::fstore_t {
        fst_flags: libc::F_ALLOCATEALL,
        fst_posmode: libc::F_PEOFPOSMODE,
        fst_offset: 0,
        fst_length: size,
        fst_bytesalloc: 0,
    };
    if libc::fcntl(fd, libc::F_PREALLOCATE, &mut store) == -1
        || libc::ftruncate(fd, size) == -1
    {
        return crate::utility::errno_val();
    }
    0
}

/// System page size, cached on first use. Used to align the per-worker
/// sub-ranges in `parallel_madvise_populate_write` -- `madvise`
/// requires page-aligned offsets and lengths, and by the stream reader to
/// hand back pages it has read.
pub(crate) fn page_size() -> usize {
    static CACHED: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *CACHED.get_or_init(|| {
        let v = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
        if v <= 0 { 4096 } else { v as usize }
    })
}

/// Worker count for parallel page reservation. Reuses the encoder's
/// `frame_workers()` cap so all parallel-CPU work in the runtime sees
/// the same `MORLOC_FRAME_WORKERS` override. Only the Linux
/// page-reservation path uses it (macOS reserves lazily).
#[cfg(target_os = "linux")]
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
    // The read-only phases (relocation, output walk+encode) saw
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

/// Reference-count value marking a block whose last reference has been
/// dropped and whose bytes are being scrubbed. It reads as in-use, so no
/// allocator can claim the block until the scrub completes and publishes
/// zero.
const TEARING_DOWN: u32 = u32::MAX;

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
    // Volume 0 is never mapped; see PRIMARY_VOLUME.
    for _ in 0..8 {
        let idx = (next_random_u64() as usize) & (MAX_VOLUME_NUMBER - 1);
        if idx != 0 && table.slots[idx].is_null() {
            return Some(idx);
        }
    }
    let start = (next_random_u64() as usize) & (MAX_VOLUME_NUMBER - 1);
    for off in 0..MAX_VOLUME_NUMBER {
        let idx = (start + off) & (MAX_VOLUME_NUMBER - 1);
        if idx != 0 && table.slots[idx].is_null() {
            return Some(idx);
        }
    }
    None
}

static COMMON_BASENAME: Mutex<[u8; MAX_FILENAME_SIZE]> = Mutex::new([0u8; MAX_FILENAME_SIZE]);

static FALLBACK_DIR: Mutex<[u8; MAX_FILENAME_SIZE]> = Mutex::new([0u8; MAX_FILENAME_SIZE]);

/// Read the common SHM basename set by the first `shinit` call in
/// this process. Returns an empty string if no `shinit` has been
/// called yet. Used by callers that need to allocate additional
/// volumes (e.g. the stream registry) under the same session.
pub fn get_common_basename() -> String {
    let cb = COMMON_BASENAME.lock().unwrap();
    get_cstr_buf(&cb).to_string()
}

/// Whether atexit handler has been registered (once per process).
static ATEXIT_REGISTERED: AtomicBool = AtomicBool::new(false);

/// Hooks fired by `shclose` (and `shclose_atexit`) before the volumes
/// loop runs. Companion segments (stream registry, future trace
/// buffers, ...) register their teardown here so a caller of
/// `shclose` doesn't need to know which subsystems are alive.
pub type ShcloseHook = fn();

static SHCLOSE_HOOKS: Mutex<Vec<ShcloseHook>> = Mutex::new(Vec::new());

/// Register a function to run when `shclose` is called. Hooks run in
/// registration order, before the allocator volumes are unmapped.
/// Deduped by function-pointer identity, so callers don't need their
/// own "did I already register" guards.
pub fn register_shclose_hook(hook: ShcloseHook) {
    if let Ok(mut hs) = SHCLOSE_HOOKS.lock() {
        let ptr = hook as usize;
        if !hs.iter().any(|h| *h as usize == ptr) {
            hs.push(hook);
        }
    }
}

/// Run all registered `shclose` hooks. Blocking `lock`: normal-exit
/// callers must not silently skip a poisoned mutex.
fn run_shclose_hooks() {
    let hooks: Vec<ShcloseHook> = match SHCLOSE_HOOKS.lock() {
        Ok(hs) => hs.iter().copied().collect(),
        Err(_) => return,
    };
    for h in hooks {
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| h()));
    }
}

/// Best-effort variant for the atexit path: `try_lock` so a
/// panic-poisoned or contended mutex doesn't wedge process shutdown.
fn run_shclose_hooks_atexit() {
    let hooks: Vec<ShcloseHook> = match SHCLOSE_HOOKS.try_lock() {
        Ok(hs) => hs.iter().copied().collect(),
        Err(_) => return,
    };
    for h in hooks {
        let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| h()));
    }
}

/// atexit callback: unmap the volumes, and remove them if this process owns
/// the program (see `OWNER_PID`). Catches normal exit() calls that bypass
/// an explicit shclose. Uses try_lock so a poisoned or held mutex skips the
/// cleanup instead of panicking inside atexit.
extern "C" fn shclose_atexit() {
    // Run companion / subsystem hooks first so their teardown sees a
    // still-live allocator (safe ordering, and required by any hook
    // that itself performs allocator ops on the way out).
    run_shclose_hooks_atexit();
    if let Ok(mut vols) = VOLUMES.try_lock() {
        shclose_locked(&mut vols);
    }
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

/// Read the fallback directory previously set by `shm_set_fallback_dir`.
/// Returns `None` if never set or empty. Used by companion-segment
/// teardown to reach the file-backed path.
pub fn get_fallback_dir() -> Option<String> {
    let fb = FALLBACK_DIR.lock().unwrap();
    let s = get_cstr_buf(&fb).to_string();
    if s.is_empty() { None } else { Some(s) }
}

/// Suffix of the file that records a shared-memory object.
pub const MARKER_SUFFIX: &str = ".shm";

/// The file that records the shared-memory object `name` (with its leading
/// `/`): `<fallback dir>/<name>.shm`. Every process of a program shares the
/// fallback directory, so listing it names every object the program made --
/// macOS offers no other way to enumerate them. `None` when no fallback
/// directory is set.
pub(crate) fn marker_path(name: &str) -> Option<std::ffi::CString> {
    let dir = get_fallback_dir()?;
    std::ffi::CString::new(format!(
        "{}/{}{}",
        dir.trim_end_matches('/'),
        name.trim_start_matches('/'),
        MARKER_SUFFIX
    ))
    .ok()
}

/// Remove the shared-memory object `name`, then its marker. In this order a
/// crash between the two leaves a marker naming nothing, which is harmless,
/// never an object that no listing finds.
pub(crate) fn unlink_segment(name: &std::ffi::CStr) {
    unsafe { libc::shm_unlink(name.as_ptr()) };
    if let Some(m) = marker_path(&name.to_string_lossy()) {
        unsafe { libc::unlink(m.as_ptr()) };
    }
}

/// Map volume `volume_index` of the program named `shm_basename`, creating
/// it with `shm_size` data bytes when it does not exist yet. The process
/// that creates the primary volume owns the program's volumes.
pub fn shinit(
    shm_basename: &str,
    volume_index: usize,
    shm_size: usize,
) -> Result<*mut ShmHeader, MorlocError> {
    if volume_index == 0 || volume_index >= MAX_VOLUME_NUMBER {
        return Err(MorlocError::Shm(format!(
            "shinit: volume index {} is not usable (1..{})", volume_index, MAX_VOLUME_NUMBER
        )));
    }
    // Unmap (and, for the owner, remove) the volumes on any normal exit,
    // even one that bypasses an explicit shclose.
    if !ATEXIT_REGISTERED.swap(true, Ordering::SeqCst) {
        unsafe { libc::atexit(shclose_atexit) };
    }
    if get_common_basename() == shm_basename {
        let mapped = VOLUMES.lock().unwrap().slots[volume_index].ptr();
        if !mapped.is_null() {
            return Ok(mapped);
        }
    }
    {
        let mut cb = COMMON_BASENAME.lock().unwrap();
        set_cstr(&mut *cb, shm_basename);
    }
    let shm_name = volume_name(shm_basename, volume_index);
    if let Some(shm) = create_and_register(&shm_name, volume_index, shm_size)? {
        if volume_index == PRIMARY_VOLUME {
            OWNER_PID.store(std::process::id(), Ordering::SeqCst);
        }
        crate::shm_stats::init()?;
        return Ok(shm);
    }
    match open_and_register(&shm_name, volume_index)? {
        Ok(shm) => {
            crate::shm_stats::init()?;
            Ok(shm)
        }
        Err(miss) => Err(MorlocError::Shm(format!(
            "volume '{}' exists but cannot be opened: {:?}",
            shm_name, miss
        ))),
    }
}

/// `<basename>-<vol:4hex>`. The volume index is < MAX_VOLUME_NUMBER
/// (32768), so 4 hex digits are exact.
fn volume_name(basename: &str, volume_index: usize) -> String {
    format!("{}-{:04x}", basename, volume_index)
}

/// The process that created the program's primary volume, or 0. Only it
/// removes volumes: a volume lives as long as the program, since any
/// process may hold a pointer into one another process created, and a
/// removed name can be created again as a different volume under the
/// same index.
static OWNER_PID: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

pub(crate) fn owns_program() -> bool {
    OWNER_PID.load(Ordering::SeqCst) == std::process::id()
}

/// Create volume `name` with `data_size` data bytes, initialise it, and
/// map it at `volume_index`; `None` when the name is already taken.
fn create_and_register(
    name: &str,
    volume_index: usize,
    data_size: usize,
) -> Result<Option<*mut ShmHeader>, MorlocError> {
    let full_size = data_size
        .checked_add(std::mem::size_of::<ShmHeader>())
        .ok_or_else(|| MorlocError::Shm(format!("volume size {} overflows", data_size)))?;
    let Some(seg) = create_segment(name, full_size)? else {
        return Ok(None);
    };
    let shm = seg.ptr as *mut ShmHeader;
    let data_size = seg.len - std::mem::size_of::<ShmHeader>();
    // SAFETY: this process created the volume and nothing can use it before
    // its magic is stored, which `init_volume` does last.
    unsafe { init_volume(shm, volume_index, &seg.label, data_size)? };
    register_volume(volume_index, shm, data_size);
    Ok(Some(shm))
}

/// Write a new volume's header and its single free block, then publish it
/// by storing the magic.
///
/// # Safety
/// `shm` must be a fresh mapping of `data_size` data bytes that no process
/// can have used, since its magic has never been stored.
unsafe fn init_volume(
    shm: *mut ShmHeader,
    volume_index: usize,
    label: &str,
    data_size: usize,
) -> Result<(), MorlocError> {
    let first_size = data_size
        .checked_sub(std::mem::size_of::<BlockHeader>())
        .ok_or_else(|| MorlocError::Shm(format!("a {} byte volume cannot hold a block", data_size)))?;
    let mut name_buf = [0u8; MAX_FILENAME_SIZE];
    set_cstr(&mut name_buf, label);
    (*shm).volume_name = name_buf;
    (*shm).volume_index = volume_index as i32;
    // `relative_offset` is unused under the indexed-relptr encoding; each
    // relptr carries its own volume index in the high bits.
    (*shm).relative_offset = 0;
    (*shm).volume_size = data_size;
    ShmLock::init(std::ptr::addr_of_mut!((*shm).lock))?;
    (*shm).cursor = 0;
    let first_block = (shm as *mut u8).add(std::mem::size_of::<ShmHeader>()) as *mut BlockHeader;
    (*first_block).magic = BLK_MAGIC;
    (*first_block).reference_count = AtomicU32::new(0);
    (*first_block).size = first_size;
    (*shm).magic.store(SHM_MAGIC, Ordering::Release);
    Ok(())
}

fn register_volume(volume_index: usize, shm: *mut ShmHeader, data_size: usize) {
    {
        let mut vols = VOLUMES.lock().unwrap();
        vols.slots[volume_index].set(shm, data_size);
        vols.mark_used(volume_index);
    }
    publish_vol(volume_index, shm, data_size);
}

/// Map an existing, initialised volume at `volume_index`.
fn open_and_register(
    name: &str,
    volume_index: usize,
) -> Result<Result<*mut ShmHeader, ShopenMiss>, MorlocError> {
    let (shm, data_size) = match open_volume(name)? {
        Ok(v) => v,
        Err(miss) => return Ok(Err(miss)),
    };
    let stored = unsafe { (*shm).volume_index };
    if usize::try_from(stored).ok() != Some(volume_index) {
        // SAFETY: mapped just above with this length.
        unsafe {
            libc::munmap(shm as *mut libc::c_void, data_size + std::mem::size_of::<ShmHeader>());
        }
        return Err(MorlocError::Shm(format!(
            "volume '{}' records index {}, not {}",
            name, stored, volume_index
        )));
    }
    register_volume(volume_index, shm, data_size);
    Ok(Ok(shm))
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
    /// Basename is set but neither `/dev/shm/<basename>-<i>` nor the
    /// file-backed fallback at `<fallback_dir>/<basename>-<i>` could be
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
    open_and_register(&volume_name(&basename, volume_index), volume_index)
}

/// Open the existing segment `name`: the tmpfs object, or, when that is an
/// empty stub or absent, the file of the same name in the fallback
/// directory. Returns the descriptor and the segment's size.
pub(crate) fn open_segment(name: &str) -> Result<Result<(Fd, usize), ShopenMiss>, MorlocError> {
    let name_cstr = std::ffi::CString::new(name)
        .map_err(|_| MorlocError::Shm(format!("segment name '{}' contains NUL", name)))?;
    // SAFETY: name_cstr is a valid null-terminated CString.
    let fd = unsafe { libc::shm_open(name_cstr.as_ptr(), libc::O_RDWR, 0o666) };
    let shm_errno = if fd == -1 { unsafe { crate::utility::errno_val() } } else { 0 };
    if fd != -1 {
        let fd = Fd(fd);
        let size = fd_size(&fd, name)?;
        // An empty tmpfs object is the stub a creator leaves when the
        // segment itself had to go to the fallback directory.
        if size > 0 {
            return Ok(Ok((fd, size)));
        }
    }
    let fallback = get_fallback_dir().unwrap_or_default();
    let missing = |fallback_dir: String| {
        let (basename, volume_index) = split_volume_name(name);
        ShopenMiss::FileMissing { basename, volume_index, fallback_dir, shm_errno }
    };
    if fallback.is_empty() {
        return Ok(Err(missing(fallback)));
    }
    // `name` already carries a leading '/', so append directly.
    let path = std::ffi::CString::new(format!("{}{}", fallback, name))
        .map_err(|_| MorlocError::Shm(format!("segment path for '{}' contains NUL", name)))?;
    let fd = unsafe { libc::open(path.as_ptr(), libc::O_RDWR) };
    if fd == -1 {
        return Ok(Err(missing(fallback)));
    }
    let fd = Fd(fd);
    match fd_size(&fd, name)? {
        0 => Ok(Err(missing(fallback))),
        size => Ok(Ok((fd, size))),
    }
}

fn fd_size(fd: &Fd, name: &str) -> Result<usize, MorlocError> {
    // SAFETY: zeroed memory is valid for libc::stat.
    let mut sb: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd.0, &mut sb) } == -1 {
        return Err(MorlocError::Shm(format!("Cannot fstat SHM segment '{}'", name)));
    }
    Ok(usize::try_from(sb.st_size).unwrap_or(0))
}

/// Map the existing volume `name`. The header is read and checked before
/// anything is mapped, so a truncated or uninitialised file is refused
/// instead of faulting.
fn open_volume(name: &str) -> Result<Result<(*mut ShmHeader, usize), ShopenMiss>, MorlocError> {
    let (fd, file_size) = match open_segment(name)? {
        Ok(v) => v,
        Err(miss) => return Ok(Err(miss)),
    };
    let hdr = std::mem::size_of::<ShmHeader>();
    if file_size < hdr {
        return Err(MorlocError::Shm(format!(
            "SHM volume '{}' is {} bytes, too small to hold its header",
            name, file_size
        )));
    }
    // The header is read through a mapping of its own: macOS shared-memory
    // objects support mmap but not read.
    let head = map_shared(&fd, hdr)
        .ok_or_else(|| MorlocError::Shm(format!("Cannot read the header of SHM volume '{}'", name)))?;
    // SAFETY: `head` maps `hdr` bytes of the file, which holds at least that
    // many, and every bit pattern is a valid header.
    let (magic, data_size) = unsafe {
        let header = &*(head as *const ShmHeader);
        let fields = (header.magic.load(Ordering::Acquire), header.volume_size);
        libc::munmap(head as *mut libc::c_void, hdr);
        fields
    };
    if magic != SHM_MAGIC {
        return Err(MorlocError::Shm(format!(
            "SHM volume '{}' is not initialised (its creator may have died)",
            name
        )));
    }
    let full_size = match data_size.checked_add(hdr) {
        Some(n) if n <= file_size => n,
        _ => {
            return Err(MorlocError::Shm(format!(
                "SHM volume '{}' claims {} data bytes but the file holds {}",
                name, data_size, file_size
            )))
        }
    };
    let ptr = map_shared(&fd, full_size)
        .ok_or_else(|| MorlocError::Shm(format!("Cannot mmap SHM volume '{}'", name)))?;
    Ok(Ok((ptr as *mut ShmHeader, data_size)))
}

/// Map `len` bytes of `fd` shared, read-write.
pub(crate) fn map_shared(fd: &Fd, len: usize) -> Option<*mut u8> {
    // SAFETY: mmap on a valid descriptor; the result is checked.
    let ptr = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            len,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            fd.0,
            0,
        )
    };
    (ptr != libc::MAP_FAILED).then_some(ptr as *mut u8)
}

fn split_volume_name(name: &str) -> (String, usize) {
    match name.rsplit_once('-') {
        Some((b, i)) => (b.to_string(), usize::from_str_radix(i, 16).unwrap_or(0)),
        None => (name.to_string(), 0),
    }
}

/// Unmap every SHM volume, and remove the program's volumes if this process
/// owns them (see `OWNER_PID`). Runs registered `shclose` hooks
/// first (companion segment teardowns) so callers of `shclose` don't
/// have to know which subsystems are alive.
pub fn shclose() -> Result<(), MorlocError> {
    run_shclose_hooks();
    let _lock = ALLOC_MUTEX.lock().unwrap();
    let mut vols = VOLUMES.lock().unwrap();
    shclose_locked(&mut vols);
    Ok(())
}

/// Drop every SHM volume currently held by this process as `shclose` does,
/// and clear bookkeeping (VOLUMES / CURRENT_VOLUME / COMMON_BASENAME).
///
/// Intended for in-process recovery (pool-crash teardown) by the owner.
/// After this returns, no SHM is mapped or named on disk for this basename, and the
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

/// Blocks currently held across every mapped volume, and the bytes they
/// cover. Walks each volume's block chain; a block is held when its
/// reference count is non-zero.
///
/// Volume files only ever grow, so their size records what has been
/// allocated rather than what is still in use. This reports the latter,
/// which is what tells a leak apart from a working set that has settled.
/// `hist` receives a count per power-of-two size class (index i covers
/// sizes in [2^i, 2^(i+1))), which identifies what is being held rather
/// than only how much.
pub fn live_block_stats(hist: &mut [usize]) -> (usize, usize) {
    let hdr_size = std::mem::size_of::<BlockHeader>();
    let mut blocks = 0usize;
    let mut bytes = 0usize;
    let vols = VOLUMES.lock().unwrap();
    for &slot_idx in &vols.used {
        let slot = vols.slots[slot_idx as usize];
        if slot.is_null() {
            continue;
        }
        unsafe {
            let base = (slot.ptr() as *mut u8).add(std::mem::size_of::<ShmHeader>());
            let end = base.add(slot.data_size()) as *const u8;
            let mut blk = base as *mut BlockHeader;
            while (blk as *const u8) < end
                && (end as usize) - (blk as usize) >= hdr_size
            {
                if (*blk).magic != BLK_MAGIC {
                    break;
                }
                let size = (*blk).size;
                if size == 0 || size > slot.data_size() {
                    break;
                }
                if (*blk).reference_count.load(Ordering::Relaxed) != 0 {
                    blocks += 1;
                    bytes += size;
                    if !hist.is_empty() {
                        let cls = (usize::BITS - size.leading_zeros()) as usize;
                        hist[cls.min(hist.len() - 1)] += 1;
                    }
                }
                blk = (blk as *mut u8).add(hdr_size + size) as *mut BlockHeader;
            }
        }
    }
    (blocks, bytes)
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
        // SAFETY: shm is a valid mmap'd pointer stored in VOLUMES, mapped
        // with this length.
        unsafe {
            let full_size = (*shm).volume_size + std::mem::size_of::<ShmHeader>();
            libc::munmap(shm as *mut libc::c_void, full_size);
        }
        vols.slots[i] = SendPtr::null();
    }
    if owns_program() {
        if let Ok(cb) = COMMON_BASENAME.try_lock() {
            let basename = get_cstr_buf(&cb).to_string();
            drop(cb);
            let fallback = FALLBACK_DIR.try_lock().map(|fb| get_cstr_buf(&fb).to_string()).unwrap_or_default();
            remove_program_volumes(&basename, &fallback);
        }
        OWNER_PID.store(0, Ordering::SeqCst);
    }
}

/// Remove every volume named for `basename`, whichever process created it:
/// the shared-memory objects its markers record and the file-backed volumes,
/// both found in the fallback directory.
fn remove_program_volumes(basename: &str, fallback: &str) {
    let stem = basename.trim_start_matches('/');
    if stem.is_empty() {
        return;
    }
    let is_volume = |file: &str| {
        file.strip_prefix(stem)
            .and_then(|r| r.strip_prefix('-'))
            .map_or(false, |hex| hex.len() == 4 && hex.bytes().all(|b| b.is_ascii_hexdigit()))
    };
    if fallback.is_empty() {
        // Nothing records this program's objects: try every index.
        for i in 1..MAX_VOLUME_NUMBER {
            if let Ok(c) = std::ffi::CString::new(volume_name(basename, i)) {
                unsafe { libc::shm_unlink(c.as_ptr()) };
            }
        }
        return;
    }
    let dir = fallback.trim_end_matches('/');
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for file in entries.flatten().filter_map(|e| e.file_name().into_string().ok()) {
        let (object, path) = match file.strip_suffix(MARKER_SUFFIX) {
            Some(seg) if is_volume(seg) => (Some(format!("/{seg}")), format!("{dir}/{file}")),
            None if is_volume(&file) => (None, format!("{dir}/{file}")),
            _ => continue,
        };
        // The object goes before its marker; see `unlink_segment`.
        if let Some(c) = object.and_then(|o| std::ffi::CString::new(o).ok()) {
            unsafe { libc::shm_unlink(c.as_ptr()) };
        }
        if let Ok(c) = std::ffi::CString::new(path) {
            unsafe { libc::unlink(c.as_ptr()) };
        }
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
///
/// # Safety
///
/// `src` must be readable for `size` bytes.
pub unsafe fn shmemcpy(src: *const u8, size: usize) -> Result<AbsPtr, MorlocError> {
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
///
/// # Safety
///
/// `ptr` must be null or a block returned by the SHM allocator.
pub unsafe fn shincref(ptr: AbsPtr) -> Result<(), MorlocError> {
    if ptr.is_null() {
        return Err(MorlocError::Shm("Cannot incref NULL pointer".into()));
    }
    // A pointer into a volume that has since been unmapped (pool-crash
    // recovery calls `reset_all`) must not be dereferenced. `shfree` takes
    // the same guard.
    if !ptr_is_in_any_volume(ptr) {
        return Err(MorlocError::Shm(
            "Cannot incref a pointer outside every mapped volume".into(),
        ));
    }
    // SAFETY: ptr was returned by shmalloc, which places a BlockHeader immediately before
    // the returned data pointer. Magic check below validates the header.
    let blk = unsafe { &*(ptr.sub(std::mem::size_of::<BlockHeader>()) as *const BlockHeader) };
    if blk.magic == BLK_ABSORBED {
        return Err(MorlocError::Shm(
            "Cannot incref a block that was merged into its predecessor \
             (the caller is holding a stale pointer)".into(),
        ));
    }
    if blk.magic != BLK_MAGIC {
        return Err(MorlocError::Shm("Corrupted memory - invalid magic".into()));
    }
    // Refuse the same two states `shfree` refuses. Zero means the block is
    // free and its bytes are gone; the teardown sentinel means another
    // party owns the transition to zero and is scrubbing right now. A
    // reference taken in either state describes nothing, and taking one on
    // the sentinel is destructive: incrementing it wraps to zero, which is
    // the value the allocator reads as free, so the block is handed to a
    // new owner while the previous one is still zeroing it.
    loop {
        let cur = blk.reference_count.load(Ordering::Acquire);
        if cur == 0 {
            return Err(MorlocError::Shm("Cannot incref a free block".into()));
        }
        if cur == TEARING_DOWN {
            return Err(MorlocError::Shm(
                "Cannot incref a block that is being released".into(),
            ));
        }
        if blk
            .reference_count
            .compare_exchange_weak(cur, cur + 1, Ordering::AcqRel, Ordering::Acquire)
            .is_ok()
        {
            return Ok(());
        }
    }
}

/// Current reference count of a shared memory block, or `None` when
/// `ptr` is null or the header magic does not validate. A count of 0
/// means the block is free and its bytes have been scrubbed.
///
/// # Safety
///
/// `ptr` must be null or a block returned by the SHM allocator.
pub unsafe fn reference_count(ptr: AbsPtr) -> Option<u32> {
    if ptr.is_null() {
        return None;
    }
    // SAFETY: as in `shincref` -- shmalloc places a BlockHeader
    // immediately before the returned pointer; the magic check
    // validates that this pointer actually sits at a block start.
    let blk = unsafe {
        &*(ptr.sub(std::mem::size_of::<BlockHeader>()) as *const BlockHeader)
    };
    if blk.magic != BLK_MAGIC {
        return None;
    }
    Some(blk.reference_count.load(Ordering::Acquire))
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
/// reader), `shopen_diag(idx)` lazily mmaps `<basename>-<idx>` and
/// records it.
pub fn rel2abs(ptr: RelPtr) -> Result<AbsPtr, MorlocError> {
    rel2abs_extent(ptr, 0)
}

/// `rel2abs` for a region of `extent` bytes: the whole region, not only its
/// first byte, must lie inside the volume.
pub fn rel2abs_extent(ptr: RelPtr, extent: usize) -> Result<AbsPtr, MorlocError> {
    if relptr_is_sentinel(ptr) {
        // RELNULL and any future reserved sentinel are not addressable.
        return Err(MorlocError::Shm(format!(
            "rel2abs called on sentinel relptr {}",
            ptr
        )));
    }
    let vol_idx = relptr_volume_index(ptr);
    let offset = relptr_offset(ptr);
    if vol_idx == 0 {
        return Err(MorlocError::Shm(format!(
            "rel2abs: relptr {} is in volume 0, which is never mapped: a buffer- or \
             file-relative offset reached shared memory without being rebased",
            ptr
        )));
    }

    // Fast path: the volume is mapped in this process and published to the
    // lock-free table, as the C resolver reads it. No lock is taken.
    let entry = &MORLOC_VOL_TABLE[vol_idx];
    let data_base = entry.data_base.load(Ordering::Acquire);
    if !data_base.is_null() {
        let data_size = entry.data_size.load(Ordering::Relaxed);
        if !region_fits(offset, extent, data_size) {
            return Err(MorlocError::Shm(format!(
                "rel2abs offset {} exceeds volume {}'s size {}",
                offset, vol_idx, data_size
            )));
        }
        // SAFETY: the published base is the mapped data region and the
        // region check keeps the add inside it.
        return Ok(unsafe { data_base.add(offset) });
    }

    // Not published: look the slot up under the lock (it may be mapped but
    // not yet published), then drop the lock before computing the address.
    let slot = {
        let vols = VOLUMES.lock().unwrap();
        vols.slots[vol_idx]
    };
    if !slot.is_null() {
        if !region_fits(offset, extent, slot.data_size()) {
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
    if !region_fits(offset, extent, slot.data_size()) {
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

/// A region of `extent` bytes at `offset` lies inside `size` bytes. An empty
/// region still needs a valid start.
fn region_fits(offset: usize, extent: usize, size: usize) -> bool {
    offset < size && offset.checked_add(extent).is_some_and(|end| end <= size)
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
                format!("file-backed fallback '{}{}-{:04x}' also missing", fallback_dir, basename, vi)
            };
            MorlocError::Shm(format!(
                "cannot resolve relptr {} (decoded as vol_idx={}, offset={}) -- \
                 SHM volume '{}-{:04x}' does not exist (shm_open: {}).{} {}. \
                 Likely causes: writer never created this volume, writer crashed \
                 before sending, basename mismatch between writer and reader, or \
                 another process (or shared-memory cleanup) unlinked it.",
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
/// Bytes across every allocator volume plus every attached companion.
pub fn total_shm_size() -> usize {
    let mut total = 0;
    {
        let vols = VOLUMES.lock().unwrap();
        for &slot_idx in &vols.used {
            let slot = vols.slots[slot_idx as usize];
            if !slot.is_null() {
                total += slot.data_size();
            }
        }
    }
    total + crate::shm_companion::total_companion_bytes()
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

/// A named shared segment this process created and mapped: an allocator
/// volume or a companion. `label` is the tmpfs name, or the file path when
/// the segment lives in the fallback directory.
pub struct Segment {
    pub ptr:   *mut u8,
    pub len:   usize,
    pub label: String,
}

/// An open file descriptor, closed on drop.
pub(crate) struct Fd(libc::c_int);

impl Drop for Fd {
    fn drop(&mut self) {
        // SAFETY: the descriptor is owned and closed once.
        unsafe { libc::close(self.0) };
    }
}

/// Create segment `name` of `full_size` bytes and map it, or `None` when
/// the name is taken. The tmpfs name is the claim on the name: it is
/// created exclusively, and when the segment must live in the fallback
/// directory instead (tmpfs too small), the tmpfs name stays behind,
/// emptied, as a stub that sends readers there (`open_segment`). So every
/// creator contends for the one namespace and at most one wins. Without a
/// usable tmpfs, the fallback file is the only namespace and is created
/// exclusively.
pub(crate) fn create_segment(name: &str, full_size: usize) -> Result<Option<Segment>, MorlocError> {
    let name_cstr = std::ffi::CString::new(name)
        .map_err(|_| MorlocError::Shm(format!("volume name '{}' contains NUL", name)))?;
    // Record the object before making it, so no object exists unrecorded. A
    // marker already present belongs to whoever is creating, or created, the
    // object; the exclusive shm_open below settles who that is.
    let marker = marker_path(name);
    let mut own_marker = false;
    if let Some(m) = &marker {
        let fd = unsafe { libc::open(m.as_ptr(), libc::O_WRONLY | libc::O_CREAT | libc::O_EXCL | libc::O_CLOEXEC, 0o600) };
        if fd >= 0 {
            unsafe { libc::close(fd) };
            own_marker = true;
        } else {
            let e = std::io::Error::last_os_error();
            if e.raw_os_error() != Some(libc::EEXIST) {
                return Err(MorlocError::Shm(format!(
                    "cannot record shared-memory object '{}' at '{}': {}",
                    name,
                    m.to_string_lossy(),
                    e
                )));
            }
        }
    }
    let fd = unsafe {
        libc::shm_open(name_cstr.as_ptr(), libc::O_RDWR | libc::O_CREAT | libc::O_EXCL, 0o666)
    };
    if fd < 0 {
        let e = std::io::Error::last_os_error();
        if e.raw_os_error() == Some(libc::EEXIST) {
            return Ok(None);
        }
        // No object was made, so it needs no record.
        if let (true, Some(m)) = (own_marker, &marker) {
            unsafe { libc::unlink(m.as_ptr()) };
        }
        return create_file_segment(name, full_size, &format!("shm_open: {e}"));
    }
    let size = libc::off_t::try_from(full_size)
        .map_err(|_| MorlocError::Shm(format!("volume size {} overflows", full_size)))?;
    // SAFETY: fd is the tmpfs object just created; every path below closes it.
    unsafe {
        if libc::ftruncate(fd, size) != 0 {
            let why = format!("ftruncate: {}", std::io::Error::last_os_error());
            libc::close(fd);
            return create_file_segment_behind_stub(&name_cstr, name, full_size, &why);
        }
        let ptr = libc::mmap(
            std::ptr::null_mut(),
            full_size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            fd,
            0,
        );
        if ptr == libc::MAP_FAILED {
            let e = std::io::Error::last_os_error();
            libc::close(fd);
            unlink_segment(&name_cstr);
            return Err(MorlocError::Shm(format!(
                "Failed to mmap shared-memory volume '{}' ({} bytes): {}",
                name, full_size, e
            )));
        }
        // Reserve every page now, so a tmpfs too small for the volume is
        // found here rather than by a SIGBUS on some later write.
        if parallel_reserve_pages(fd, ptr as *mut u8, full_size) != 0 {
            libc::munmap(ptr, full_size);
            // Emptying the object releases the pages already reserved and
            // turns it into the stub.
            let emptied = libc::ftruncate(fd, 0) == 0;
            libc::close(fd);
            if !emptied {
                unlink_segment(&name_cstr);
                return Err(MorlocError::Shm(format!(
                    "tmpfs has no room for volume '{}' ({} bytes) and it could not be moved",
                    name, full_size
                )));
            }
            return create_file_segment_behind_stub(&name_cstr, name, full_size, "no room left in tmpfs");
        }
        libc::close(fd);
        Ok(Some(Segment { ptr: ptr as *mut u8, len: full_size, label: name.to_string() }))
    }
}

/// The fallback-directory half of `create_segment` once the empty tmpfs
/// stub holds the name. The stub is removed again if no segment results.
fn create_file_segment_behind_stub(
    stub: &std::ffi::CStr,
    name: &str,
    full_size: usize,
    why: &str,
) -> Result<Option<Segment>, MorlocError> {
    let made = create_file_segment(name, full_size, why);
    if !matches!(made, Ok(Some(_))) {
        unlink_segment(stub);
    }
    made
}

/// Create segment `name` in the fallback directory, exclusively. `why` says
/// why it could not be a shared-memory object.
fn create_file_segment(name: &str, full_size: usize, why: &str) -> Result<Option<Segment>, MorlocError> {
    let fallback = get_fallback_dir().ok_or_else(|| {
        MorlocError::Shm(format!(
            "Failed to allocate SHM '{}' ({} bytes): no shared-memory object ({}) and no fallback directory is set",
            name,
            full_size,
            why
        ))
    })?;
    // `name` already carries a leading '/', so append directly.
    let file_path = format!("{}{}", fallback, name);
    let path_cstr = std::ffi::CString::new(file_path.as_str())
        .map_err(|_| MorlocError::Shm(format!("volume path '{}' contains NUL", file_path)))?;
    let fd = unsafe {
        libc::open(path_cstr.as_ptr(), libc::O_RDWR | libc::O_CREAT | libc::O_EXCL, 0o666)
    };
    if fd == -1 {
        let e = std::io::Error::last_os_error();
        if e.raw_os_error() == Some(libc::EEXIST) {
            return Ok(None);
        }
        return Err(MorlocError::Shm(format!(
            "Failed to create file-backed volume '{}' (no shared-memory object: {}): {}",
            file_path, why, e
        )));
    }
    let size = libc::off_t::try_from(full_size)
        .map_err(|_| MorlocError::Shm(format!("volume size {} overflows", full_size)))?;
    // SAFETY: fd is the file just created; every path below closes it.
    unsafe {
        let rc = preallocate_fd(fd, size);
        if rc != 0 {
            libc::close(fd);
            libc::unlink(path_cstr.as_ptr());
            return Err(MorlocError::Shm(format!(
                "Failed to allocate file-backed volume '{}' ({} bytes): {}",
                file_path,
                full_size,
                std::io::Error::from_raw_os_error(rc)
            )));
        }
        let ptr = libc::mmap(
            std::ptr::null_mut(),
            full_size,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            fd,
            0,
        );
        if ptr == libc::MAP_FAILED {
            libc::close(fd);
            libc::unlink(path_cstr.as_ptr());
            return Err(MorlocError::Shm(format!(
                "Failed to mmap file-backed volume '{}' ({} bytes)",
                file_path, full_size
            )));
        }
        // A working but slower path: data lives on whatever backs the
        // fallback directory. The usual cause is a small /dev/shm in a
        // container.
        eprintln!(
            "morloc warning: no shared-memory object for a {} allocation ({}); \
             falling back to file-backed '{}'.",
            human_bytes(full_size),
            why,
            file_path,
        );
        libc::close(fd);
        Ok(Some(Segment { ptr: ptr as *mut u8, len: full_size, label: file_path }))
    }
}

fn shmalloc_unlocked(size: usize) -> Result<AbsPtr, MorlocError> {
    let blk = find_free_block(size)?;
    // SAFETY: blk is a claimed BlockHeader in mapped SHM; its data starts
    // immediately after the header.
    unsafe { Ok((blk as *mut u8).add(std::mem::size_of::<BlockHeader>())) }
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
    if blk.magic == BLK_ABSORBED {
        return Err(MorlocError::Shm(
            "Cannot free a block that was merged into its predecessor \
             (the caller is holding a stale pointer)".into(),
        ));
    }
    if blk.magic != BLK_MAGIC {
        return Err(MorlocError::Shm("Corrupted memory".into()));
    }
    // Scrub before publishing, not after. A count of zero is precisely what
    // marks a block available, so a scrub that runs after the count drops
    // writes zeros over whatever its next owner has already stored there --
    // and on a machine with few cores that owner gets far enough to notice.
    // Dropping the last reference to a sentinel instead leaves the block
    // reading as in use, so no scanner will take it while the scrub runs, and
    // zero is published only once the bytes are actually gone.
    //
    // The whole transition is a compare-exchange loop because reading the
    // count and then acting on it are two separate steps: between them a
    // concurrent free of the same block can move it, and a decrement issued
    // on the strength of a stale read underflows a counter that everything
    // else treats as "in use".
    loop {
        let cur = blk.reference_count.load(Ordering::Acquire);
        if cur == 0 {
            return Err(MorlocError::Shm("Reference count already 0".into()));
        }
        if cur == TEARING_DOWN {
            // Another party is mid-scrub and owns the transition to zero.
            return Err(MorlocError::Shm(
                "Reference count already 0 (block is being released)".into(),
            ));
        }
        let next = if cur == 1 { TEARING_DOWN } else { cur - 1 };
        if blk
            .reference_count
            .compare_exchange_weak(cur, next, Ordering::AcqRel, Ordering::Acquire)
            .is_err()
        {
            continue;
        }
        if next == TEARING_DOWN {
            // SAFETY: ptr points to blk.size bytes of SHM data. This process
            // held the last reference and has replaced it with a value that
            // reads as in-use, so the block cannot be handed to anyone until
            // the store below.
            unsafe {
                std::ptr::write_bytes(ptr, 0, blk.size);
            }
            crate::shm_stats::on_release(blk.size);
            blk.reference_count.store(0, Ordering::Release);
        }
        return Ok(());
    }
}

fn find_free_block(size: usize) -> Result<*mut BlockHeader, MorlocError> {
    let cv = CURRENT_VOLUME.load(Ordering::Relaxed);
    let vols = VOLUMES.lock().unwrap();

    // Try current volume first (allocation hint).
    let shm = vols.slots[cv].ptr();
    if !shm.is_null() {
        if let Some(blk) = find_free_block_in_volume(shm, size)? {
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

    drop(vols);
    let basename = {
        let cb = COMMON_BASENAME.lock().unwrap();
        get_cstr_buf(&cb).to_string()
    };
    // Another process may have created the index this process picks, since
    // the choice is made from this process's own table. Such an index is
    // mapped (a volume of the program like any other) and tried, and the
    // search moves on.
    const ATTEMPTS: usize = 16;
    for _ in 0..ATTEMPTS {
        let picked = pick_free_slot(&VOLUMES.lock().unwrap());
        let Some(idx) = picked else {
            return Err(MorlocError::Shm(format!(
                "Could not find suitable block for {} bytes: all {} \
                 volume slots are occupied",
                size, MAX_VOLUME_NUMBER
            )));
        };
        let name = volume_name(&basename, idx);
        let shm = match create_and_register(&name, idx, new_size)? {
            Some(shm) => shm,
            None => match open_and_register(&name, idx) {
                Ok(Ok(shm)) => shm,
                _ => continue,
            },
        };
        if let Some(blk) = find_free_block_in_volume(shm, size)? {
            CURRENT_VOLUME.store(idx, Ordering::Relaxed);
            return Ok(blk);
        }
    }
    Err(MorlocError::Shm(format!(
        "Could not create a volume for {} bytes: {} volume indices in a row were taken",
        size, ATTEMPTS
    )))
}

/// Take ownership of a free block, found and sized under the volume lock
/// that `_held` proves is still held.
///
/// A block is "free" precisely when its reference count reads zero, and that
/// count lives in shared memory where every process can see it. Handing a
/// block back to a caller while it still reads zero publishes it as
/// available to every other process for as long as it takes the caller to
/// claim it -- and `ALLOC_MUTEX`, the only thing serialising the steps
/// between, is an ordinary in-process mutex. It keeps this process's own
/// threads apart and says nothing about the pools, which are separate
/// processes allocating from these same volumes. Two of them would be given
/// the same block, then write over each other's data, split the same block
/// in two different ways, and each free it once.
///
/// Only the lock holder moves a count up from zero, and a free never moves
/// one down from zero, so the exchange cannot fail unless the block list
/// is corrupt.
///
/// # Safety
/// `blk` must be a valid BlockHeader in the volume `_held` locks.
#[inline]
unsafe fn claim(_held: &ShmGuard<'_>, blk: *mut BlockHeader) -> Result<*mut BlockHeader, MorlocError> {
    (*blk)
        .reference_count
        .compare_exchange(0, 1, Ordering::AcqRel, Ordering::Relaxed)
        .map(|_| {
            crate::shm_stats::on_claim((*blk).size);
            blk
        })
        .map_err(|n| {
            MorlocError::Shm(format!("the allocator chose a block already in use (count {n})"))
        })
}

/// Find a free block of at least `size` bytes, split off the remainder, and
/// claim it, all in one critical section.
fn find_free_block_in_volume(
    shm: *mut ShmHeader,
    size: usize,
) -> Result<Option<*mut BlockHeader>, MorlocError> {
    unsafe {
        let shm_end = (shm as *const u8)
            .add(std::mem::size_of::<ShmHeader>())
            .add((*shm).volume_size);

        let held = (*shm).lock.lock()?;

        let cursor = (*shm).cursor;
        let found = 'found: {
            // Try cursor position first
            if cursor != VOLNULL {
                let blk = vol2abs_raw(cursor, shm) as *mut BlockHeader;
                if (*blk).magic == BLK_MAGIC
                    && (*blk).reference_count.load(Ordering::Relaxed) == 0
                    && (*blk).size >= size
                {
                    break 'found Some(blk);
                }
            }

            // Scan from cursor forward
            let start_blk = if cursor != VOLNULL {
                vol2abs_raw(cursor, shm) as *mut BlockHeader
            } else {
                vol2abs_raw(0, shm) as *mut BlockHeader
            };
            if let Some(blk) = scan_volume(start_blk, size, shm_end as *const u8) {
                break 'found Some(blk);
            }

            // Wrap around: scan from beginning to cursor
            if cursor > 0 {
                let first_blk = vol2abs_raw(0, shm) as *mut BlockHeader;
                let cursor_end = vol2abs_raw(cursor, shm);
                if let Some(blk) = scan_volume(first_blk, size, cursor_end as *const u8) {
                    break 'found Some(blk);
                }
            }
            None
        };

        match found {
            Some(blk) => {
                split_block(&held, shm, blk, size)?;
                claim(&held, blk).map(Some)
            }
            None => Ok(None),
        }
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
            // The whole header must lie inside the region: starting inside it
            // is not enough, since the next thing done is a 16-byte read.
            if (next as *const u8) >= end
                || (end as usize) - (next as usize) < hdr_size
                || (*next).magic != BLK_MAGIC
                || (*next).reference_count.load(Ordering::Relaxed) != 0
            {
                break;
            }
            // The absorbed header becomes interior bytes of the survivor.
            // Stamp it so a pointer to the absorbed block stops reading as
            // a block: leaving a valid header inside a live allocation is
            // the allocator lying about its own structure, and every
            // validating entry point believes it.
            let next_size = (*next).size;
            (*next).magic = BLK_ABSORBED;
            (*blk).size += hdr_size + next_size;
        }

        if (*blk).reference_count.load(Ordering::Relaxed) == 0 && (*blk).size >= size {
            return Some(blk);
        }

        blk = (blk as *mut u8).add(hdr_size + (*blk).size) as *mut BlockHeader;
    }
    None
}

/// Cut `blk` down to `size` bytes, leaving the remainder as a free block
/// when it is large enough to carry a header.
///
/// # Safety
/// `blk` must be a free BlockHeader in `shm`, the volume `_held` locks.
unsafe fn split_block(
    _held: &ShmGuard<'_>,
    shm: *mut ShmHeader,
    blk: *mut BlockHeader,
    size: usize,
) -> Result<(), MorlocError> {
    if (*blk).size == size {
        return Ok(());
    }
    // A block smaller than the request would underflow the remainder and
    // send the split below to write a header outside the mapping. The
    // search is supposed to return only blocks large enough; refuse rather
    // than trust it.
    if (*blk).size < size {
        return Err(MorlocError::Shm(
            "Block smaller than the request reached the split".into(),
        ));
    }
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
        // Too small to head, so it becomes interior bytes of the block.
        // Stamp whatever header may be sitting there, for the same reason
        // the merge does.
        if remaining >= std::mem::size_of::<u32>() {
            (*new_free).magic = BLK_ABSORBED;
        }
        (*blk).size += remaining;
        (*shm).cursor = VOLNULL;
    }
    Ok(())
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

    // Processes allocating from one volume at once must never be handed the
    // same block. Each forked child stamps every block it holds with its own
    // byte and checks the stamp before freeing; a block given to two owners
    // is overwritten by the other and fails the check.
    #[test]
    fn processes_allocating_together_never_share_a_block() {
        const KIDS: usize = 8;
        const ROUNDS: usize = 200_000;
        const HELD: usize = 8;
        let _shm = crate::own_test_registry();
        unsafe {
            let mut kids = Vec::new();
            for k in 0..KIDS {
                let pid = libc::fork();
                assert!(pid >= 0);
                if pid == 0 {
                    let stamp = k as u8 + 1;
                    let mut rng: u64 = 0x9E37_79B9_7F4A_7C15 ^ (k as u64 + 1);
                    let mut held: Vec<(AbsPtr, usize)> = Vec::new();
                    for _ in 0..ROUNDS {
                        rng ^= rng << 13;
                        rng ^= rng >> 7;
                        rng ^= rng << 17;
                        if held.len() == HELD || (rng & 1 == 1 && !held.is_empty()) {
                            let (p, n) = held.swap_remove((rng as usize >> 1) % held.len());
                            if std::slice::from_raw_parts(p, n).iter().any(|&b| b != stamp) {
                                libc::_exit(3);
                            }
                            if shfree(p).is_err() {
                                libc::_exit(4);
                            }
                        } else {
                            let n = 16 + (rng as usize >> 8) % 2048;
                            let Ok(p) = shmalloc(n) else { libc::_exit(2) };
                            std::ptr::write_bytes(p, stamp, n);
                            held.push((p, n));
                        }
                    }
                    for (p, n) in held {
                        if std::slice::from_raw_parts(p, n).iter().any(|&b| b != stamp) {
                            libc::_exit(3);
                        }
                        let _ = shfree(p);
                    }
                    libc::_exit(0);
                }
                kids.push(pid);
            }
            let statuses: Vec<i32> = kids
                .into_iter()
                .map(|k| {
                    let mut status = 0;
                    libc::waitpid(k, &mut status, 0);
                    status
                })
                .collect();
            for status in statuses {
                assert!(
                    libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0,
                    "child failed with status {status} (3 = a block it held was overwritten)"
                );
            }
        }
    }

    // Threads of one process allocating at once must never be handed the
    // same block.
    #[test]
    fn threads_allocating_together_never_share_a_block() {
        const THREADS: usize = 8;
        const ROUNDS: usize = 100_000;
        const HELD: usize = 8;
        let _shm = crate::own_test_registry();
        let handles: Vec<_> = (0..THREADS)
            .map(|k| {
                std::thread::spawn(move || {
                    let stamp = k as u8 + 1;
                    let mut rng: u64 = 0x9E37_79B9_7F4A_7C15 ^ (k as u64 + 1);
                    let mut held: Vec<(usize, usize)> = Vec::new();
                    for _ in 0..ROUNDS {
                        rng ^= rng << 13;
                        rng ^= rng >> 7;
                        rng ^= rng << 17;
                        if held.len() == HELD || (rng & 1 == 1 && !held.is_empty()) {
                            let (p, n) = held.swap_remove((rng as usize >> 1) % held.len());
                            let p = p as AbsPtr;
                            let bytes = unsafe { std::slice::from_raw_parts(p, n) };
                            assert!(bytes.iter().all(|&b| b == stamp), "a held block was overwritten");
                            shfree(p).unwrap();
                        } else {
                            let n = 16 + (rng as usize >> 8) % 2048;
                            let p = shmalloc(n).unwrap();
                            unsafe { std::ptr::write_bytes(p, stamp, n) };
                            held.push((p as usize, n));
                        }
                    }
                    for (p, _) in held {
                        shfree(p as AbsPtr).unwrap();
                    }
                })
            })
            .collect();
        for h in handles {
            h.join().unwrap();
        }
    }

    // Creating a volume claims its name: of several processes creating one
    // name at once, exactly one succeeds and the rest are told it is taken.
    #[test]
    fn volume_creation_has_one_winner() {
        // Forking runs the stream registry's fork handler, which must not
        // race a test that owns the registry.
        let _arena = crate::own_test_shm();
        const KIDS: usize = 4;
        const NAMES: usize = 40;
        let base = format!("/morloc-{}-test-excl", std::process::id());
        unsafe {
            let wins = libc::mmap(
                std::ptr::null_mut(),
                NAMES * 4,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *const AtomicU32;
            assert_ne!(wins as *mut libc::c_void, libc::MAP_FAILED);
            let mut kids = Vec::new();
            for _ in 0..KIDS {
                let pid = libc::fork();
                assert!(pid >= 0);
                if pid == 0 {
                    for i in 0..NAMES {
                        match create_segment(&format!("{base}-{i:04x}"), 8192) {
                            Ok(Some(v)) => {
                                (*wins.add(i)).fetch_add(1, Ordering::SeqCst);
                                libc::munmap(v.ptr as *mut libc::c_void, v.len);
                            }
                            Ok(None) => {}
                            Err(_) => libc::_exit(2),
                        }
                    }
                    libc::_exit(0);
                }
                kids.push(pid);
            }
            for k in kids {
                let mut status = 0;
                libc::waitpid(k, &mut status, 0);
                assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "child failed: {status}");
            }
            for i in 0..NAMES {
                let name = std::ffi::CString::new(format!("{base}-{i:04x}")).unwrap();
                libc::shm_unlink(name.as_ptr());
            }
            for i in 0..NAMES {
                assert_eq!((*wins.add(i)).load(Ordering::SeqCst), 1, "name {i}");
            }
        }
    }

    /// The shared-memory objects named `<prefix>...` that exist. Markers
    /// say which to look for; on Linux /dev/shm is listed too, so an object
    /// made without a marker fails the comparison.
    fn live_segments(dir: &std::path::Path, prefix: &str) -> Vec<String> {
        let mut live: Vec<String> = crate::marked_segments(dir)
            .into_iter()
            .filter(|n| n[1..].starts_with(prefix))
            .filter(|n| {
                let c = std::ffi::CString::new(n.as_str()).unwrap();
                let fd = unsafe { libc::shm_open(c.as_ptr(), libc::O_RDONLY, 0) };
                if fd >= 0 {
                    unsafe { libc::close(fd) };
                }
                fd >= 0
            })
            .collect();
        live.sort();
        #[cfg(target_os = "linux")]
        {
            let mut listed: Vec<String> = std::fs::read_dir("/dev/shm")
                .map(|d| {
                    d.flatten()
                        .filter_map(|e| e.file_name().into_string().ok())
                        .filter(|n| n.starts_with(prefix))
                        .map(|n| format!("/{n}"))
                        .collect()
                })
                .unwrap_or_default();
            listed.sort();
            assert_eq!(live, listed, "markers disagree with /dev/shm");
        }
        live
    }

    // Volumes live as long as the program, not as long as the process that
    // happened to map or create one: a process that exits while others run
    // unlinks nothing, and the program's owner -- the creator of the primary
    // volume -- removes every volume of the program when it closes.
    #[test]
    fn only_the_owner_unlinks_volumes() {
        let _arena = crate::own_test_shm();
        shclose().unwrap();
        let fallback = crate::ScopedFallback::new("own");
        let test_dir = fallback.path().to_path_buf();
        let base = format!("/morloc-{}-test-own", std::process::id());
        shinit(&base, PRIMARY_VOLUME, 4096).unwrap();
        unsafe {
            let slot = libc::mmap(
                std::ptr::null_mut(),
                8,
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_SHARED | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *mut RelPtr;
            let pid = libc::fork();
            assert!(pid >= 0);
            if pid == 0 {
                // Larger than the primary volume, so this creates another.
                let Ok(p) = shmalloc(1 << 20) else { libc::_exit(2) };
                let Ok(r) = abs2rel(p) else { libc::_exit(3) };
                std::ptr::write_volatile(slot, r);
                let _ = shclose();
                libc::_exit(0);
            }
            let mut status = 0;
            libc::waitpid(pid, &mut status, 0);
            assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0, "child failed: {status}");
            let r = std::ptr::read_volatile(slot);
            assert_ne!(relptr_volume_index(r), PRIMARY_VOLUME);
            let prefix = &base[1..];
            assert_eq!(live_segments(&test_dir, prefix).len(), 2, "a closing non-owner unlinked a volume");
            rel2abs(r).expect("the child's volume outlives the child");
        }
        shclose().unwrap();
        assert_eq!(live_segments(&test_dir, &base[1..]), Vec::<String>::new(), "the owner left volumes behind");
    }

    // A buffer- or file-relative offset reads as volume 0. Copying such a
    // value into SHM without rebasing it must fail at the first dereference,
    // not resolve into whatever this process mapped at volume 0.
    #[test]
    fn an_unrebased_offset_does_not_resolve() {
        let _shm = crate::init_test_shm();
        let block = shmalloc(64).unwrap();
        assert_ne!(relptr_volume_index(abs2rel(block).unwrap()), 0, "SHM never uses volume 0");
        for off in [0isize, 8, 16] {
            let err = rel2abs(off).unwrap_err().to_string();
            assert!(err.contains("volume 0"), "{err}");
        }
        shfree(block).unwrap();
    }

    // Merging a free block into its predecessor turns the absorbed block's
    // header into interior bytes of the survivor. A pointer to the absorbed
    // block must stop validating at that moment: if its header still reads
    // as a block, a stale pointer passes every check and the allocator acts
    // on payload bytes as though they were a header.
    #[test]
    fn coalescing_invalidates_the_absorbed_block_header() {
        let hdr = std::mem::size_of::<BlockHeader>();
        let body = 256usize;

        // A synthetic volume holding two adjacent free blocks, so the merge
        // is exercised without depending on where the live allocator's
        // cursor happens to sit.
        let mut backing = vec![0u64; (2 * (hdr + body)) / 8 + 8];
        let base = backing.as_mut_ptr() as *mut u8;
        let end = unsafe { base.add(2 * (hdr + body)) } as *const u8;

        let first = base as *mut BlockHeader;
        let second = unsafe { base.add(hdr + body) as *mut BlockHeader };
        unsafe {
            (*first).magic = BLK_MAGIC;
            (*first).size = body;
            (*first).reference_count = AtomicU32::new(0);
            (*second).magic = BLK_MAGIC;
            (*second).size = body;
            (*second).reference_count = AtomicU32::new(0);
        }

        // Ask for more than either block alone can serve, forcing the merge.
        let found = unsafe { scan_volume(first, body + 8, end) };
        assert_eq!(found, Some(first), "expected the pair to merge");
        assert!(
            unsafe { (*first).size } >= 2 * body,
            "merged block did not absorb its neighbour",
        );

        assert_eq!(
            unsafe { (*second).magic }, BLK_ABSORBED,
            "absorbed block still reads as a block, so a stale pointer to it \
             passes the header check and is acted on as live payload",
        );

    }

    // The census must see what the allocator has handed out, since the
    // volume files themselves only record what was ever allocated and never
    // shrink -- they cannot distinguish a leak from a settled working set.
    #[test]
    fn live_block_stats_counts_what_is_held() {
        let _shm = crate::own_test_registry();
        let mut hist = [0usize; 40];
        let (base_blocks, base_bytes) = live_block_stats(&mut hist);

        let a = shmalloc(4096).expect("a");
        let b = shmalloc(4096).expect("b");
        let mut hist2 = [0usize; 40];
        let (held, bytes) = live_block_stats(&mut hist2);
        assert_eq!(
            held, base_blocks + 2,
            "census did not see two freshly allocated blocks",
        );
        assert!(
            bytes >= base_bytes + 8192,
            "census undercounted the bytes held: {} vs {}", bytes, base_bytes + 8192,
        );

        shfree(a).expect("free a");
        shfree(b).expect("free b");
        let mut hist3 = [0usize; 40];
        let (after, _) = live_block_stats(&mut hist3);
        assert_eq!(
            after, base_blocks,
            "census still counts blocks that were released",
        );
    }

    // A block whose last reference is being dropped is briefly marked with
    // a sentinel that reads as in-use, so nothing can claim it while its
    // bytes are being scrubbed. Acquiring a reference in that window must
    // be refused: incrementing the sentinel wraps it to zero, which is the
    // value the allocator reads as "free", so the block would be handed to
    // a new owner while the previous owner is still zeroing it.
    #[test]
    fn incref_refuses_a_block_whose_last_reference_is_dropping() {
        let _shm = crate::own_test_registry();
        let p = shmalloc(64).expect("allocate");

        // Reproduce the state shfree publishes before it scrubs.
        unsafe {
            let blk = &*(p.sub(std::mem::size_of::<BlockHeader>())
                as *const BlockHeader);
            blk.reference_count.store(u32::MAX, Ordering::Release);
        }

        let res = unsafe { shincref(p) };
        let rc = unsafe { reference_count(p) };
        assert!(
            res.is_err(),
            "incref accepted a block that was being released (refcount now {:?})",
            rc,
        );
        assert_ne!(
            rc, Some(0),
            "incref wrapped the release sentinel to zero, publishing a block \
             the allocator will hand out while it is still being scrubbed",
        );

        // Leave the block in a state the arena can clean up.
        unsafe {
            let blk = &*(p.sub(std::mem::size_of::<BlockHeader>())
                as *const BlockHeader);
            blk.reference_count.store(1, Ordering::Release);
        }
        let _ = shfree(p);
    }

    // A block that is already free must not be resurrected: its bytes have
    // been scrubbed and the allocator is entitled to hand it to anyone.
    #[test]
    fn incref_refuses_a_free_block() {
        let _shm = crate::own_test_registry();
        let p = shmalloc(64).expect("allocate");
        shfree(p).expect("free");
        assert_eq!(unsafe { reference_count(p) }, Some(0), "block should read as free");
        assert!(
            unsafe { shincref(p) }.is_err(),
            "incref accepted a block that was already free",
        );
    }

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
        // Owns the process-global arena: shinit/shclose here would tear it
        // down under any test allocating in the shared arena.
        let _arena = crate::own_test_shm();
        // shinit / shopen_diag publish a (data_base, data_size) pair to
        // MORLOC_VOL_TABLE so the C-side resolve_relptr inline can do
        // its lookup without crossing FFI. Verify the publish helpers
        // expose the right values to an Acquire reader, and that the
        // unpublish step clears the slot.
        let _fallback = crate::ScopedFallback::new("vt");

        let basename = format!("morloc-{}-test-vt", std::process::id());
        let shm = shinit(&basename, PRIMARY_VOLUME, 4096).unwrap();

        // shinit should have populated the primary volume's slot.
        let entry = &MORLOC_VOL_TABLE[PRIMARY_VOLUME];
        let base = entry.data_base.load(Ordering::Acquire);
        let size = entry.data_size.load(Ordering::Relaxed);
        assert!(!base.is_null(),
            "expected publish_vol to populate the primary slot's data_base");
        assert!(size > 0,
            "expected publish_vol to populate the primary slot's data_size");
        // data_base must point to the data region right after the header.
        let expected_base = unsafe {
            (shm as *mut u8).add(std::mem::size_of::<ShmHeader>())
        };
        assert_eq!(base, expected_base,
            "data_base must be header + sizeof::<ShmHeader>()");

        shclose().unwrap();

        // shclose drops the slot back to null.
        let base_after = MORLOC_VOL_TABLE[PRIMARY_VOLUME].data_base.load(Ordering::Acquire);
        assert!(base_after.is_null(),
            "expected shclose to publish a null data_base");

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
        // Owns the process-global arena: shinit/shclose here would tear it
        // down under any test allocating in the shared arena.
        let _arena = crate::own_test_shm();
        // Verify that rel2abs/abs2rel commute under the new indexed
        // encoding: every (slot_idx, offset) pair we allocate into can
        // be encoded to a relptr and decoded back to the same absolute
        // address. Stresses the multi-volume case where the old
        // flat-offset encoding required summing prior volume sizes.
        let _fallback = crate::ScopedFallback::new("idx");

        let basename = format!("morloc-{}-test-idx", std::process::id());
        shinit(&basename, PRIMARY_VOLUME, 4096).unwrap();

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
    }

    #[test]
    fn test_shinit_and_shmalloc() {
        // Owns the process-global arena: shinit/shclose here would tear it
        // down under any test allocating in the shared arena.
        let _arena = crate::own_test_shm();
        // Use file-backed SHM via tmpdir to avoid /dev/shm permission issues in test
        let _fallback = crate::ScopedFallback::new("plain");

        let basename = format!("morloc-{}-test-shm", std::process::id());
        let shm = shinit(&basename, PRIMARY_VOLUME, 4096).unwrap();
        assert!(!shm.is_null());
        assert_eq!(unsafe { (*shm).magic.load(Ordering::Acquire) }, SHM_MAGIC);

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
    }

    #[test]
    fn large_alloc_writes_every_page_without_crashing() {
        // Owns the process-global arena: shinit/shclose here would tear it
        // down under any test allocating in the shared arena.
        let _arena = crate::own_test_shm();
        // Smoke test for the parallel page-reservation path: allocate
        // a region larger than the 64 MiB serial-fallback cutoff in
        // `fallocate_workers`, then write every page. If the new
        // tmpfs-with-madvise path is correct, every page is backed
        // before our writes; if anything went wrong in `try_open_tmpfs`
        // we'd either crash on write (page-fault SIGBUS) or fall back
        // to the file-backed path (still correct, just slower).
        let _fallback = crate::ScopedFallback::new("large");

        let basename = format!("morloc-{}-test-large", std::process::id());
        // 128 MiB requested; that's > 64 MiB so `fallocate_workers`
        // returns >= 1 worker, and on Linux 5.14+ this exercises
        // `parallel_madvise_populate_write`.
        let alloc_size = 128 * 1024 * 1024;
        shinit(&basename, PRIMARY_VOLUME, alloc_size).unwrap();
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
    }
}
