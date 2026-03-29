//! Shared memory management with multi-volume support.
//!
//! Replaces shm.c / memory.h. Uses AtomicU32 + futex for cross-process locking
//! instead of pthread_rwlock_t, providing crash-safety and portability.

use crate::error::MorlocError;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Mutex;

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

// ── Constants ──────────────────────────────────────────────────────────────

pub const SHM_MAGIC: u32 = 0xFECA_0DF0;
pub const BLK_MAGIC: u32 = 0x0CB1_0DF0;
pub const MAX_VOLUME_NUMBER: usize = 32;
pub const MAX_FILENAME_SIZE: usize = 128;
pub const MAX_PATH_SIZE: usize = 512;

const LOCK_UNLOCKED: u32 = 0;
const LOCK_LOCKED: u32 = 1;
const SPIN_LIMIT: u32 = 40;
const LOCK_TIMEOUT_SECS: u64 = 5;

// ── Pointer types ──────────────────────────────────────────────────────────

/// Relative pointer: index into the multi-volume pool (cross-process safe).
pub type RelPtr = isize;
/// Volume-local pointer: offset within a single volume.
pub type VolPtr = isize;
/// Absolute pointer: virtual address in this process.
pub type AbsPtr = *mut u8;

pub const RELNULL: RelPtr = -1;
pub const VOLNULL: VolPtr = -1;

// ── Block alignment ────────────────────────────────────────────────────────

pub const BLOCK_ALIGN: usize = std::mem::align_of::<BlockHeader>();

#[inline]
pub const fn align_up(x: usize, align: usize) -> usize {
    (x + align - 1) & !(align - 1)
}

// ── Shared memory header (lives in mmap'd region) ──────────────────────────

#[repr(C)]
pub struct ShmHeader {
    pub magic: u32,
    pub volume_name: [u8; MAX_FILENAME_SIZE],
    pub volume_index: i32,
    pub volume_size: usize,
    pub relative_offset: usize,
    pub lock: AtomicU32,
    pub cursor: VolPtr,
}

#[repr(C)]
pub struct BlockHeader {
    pub magic: u32,
    pub reference_count: AtomicU32,
    pub size: usize,
}

const _: () = assert!(
    std::mem::size_of::<BlockHeader>()
        == std::mem::size_of::<u32>()
            + std::mem::size_of::<AtomicU32>()
            + std::mem::size_of::<usize>()
);

// ── Voidstar data structures (used by serialization) ───────────────────────

/// Variable-length array/string representation in SHM.
#[derive(Clone, Copy)]
#[repr(C)]
pub struct Array {
    pub size: usize,
    pub data: RelPtr,
}

/// N-dimensional dense tensor in SHM.
#[repr(C)]
pub struct Tensor {
    pub total_elements: usize,
    pub device_type: u32,
    pub device_id: u32,
    pub data: RelPtr,
    pub shape: RelPtr,
}

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

/// Open an existing SHM volume (or return cached pointer).
pub fn shopen(volume_index: usize) -> Result<Option<*mut ShmHeader>, MorlocError> {
    {
        let vols = VOLUMES.lock().unwrap();
        if !vols[volume_index].is_null() {
            return Ok(Some(vols[volume_index].ptr()));
        }
    }

    let basename = {
        let cb = COMMON_BASENAME.lock().unwrap();
        get_cstr_buf(&cb).to_string()
    };
    if basename.is_empty() {
        return Ok(None);
    }

    let shm_name = format!("{}_{}", basename, volume_index);

    // Try POSIX SHM
    let name_cstr = std::ffi::CString::new(shm_name.as_str()).unwrap();
    // SAFETY: name_cstr is a valid null-terminated CString.
    let fd = unsafe { libc::shm_open(name_cstr.as_ptr(), libc::O_RDWR, 0o666) };

    let fd = if fd == -1 {
        // Try file-backed fallback
        let fb = FALLBACK_DIR.lock().unwrap();
        let fallback = get_cstr_buf(&fb);
        if fallback.is_empty() {
            return Ok(None);
        }
        let file_path = format!("{}/{}", fallback, shm_name);
        let path_cstr = std::ffi::CString::new(file_path.as_str()).unwrap();
        let fd2 = unsafe { libc::open(path_cstr.as_ptr(), libc::O_RDWR) };
        if fd2 == -1 {
            return Ok(None);
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

    Ok(Some(shm))
}

/// Close and unlink all SHM volumes.
pub fn shclose() -> Result<(), MorlocError> {
    let _lock = ALLOC_MUTEX.lock().unwrap();
    let mut vols = VOLUMES.lock().unwrap();

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
    Ok(())
}

/// Allocate `size` bytes from shared memory.
pub fn shmalloc(size: usize) -> Result<AbsPtr, MorlocError> {
    // Allow 0-size: round up to minimum block alignment.
    // Needed for nil type (width=0) in morloc_eval.
    let size = if size == 0 { BLOCK_ALIGN } else { align_up(size, BLOCK_ALIGN) };
    let _lock = ALLOC_MUTEX.lock().unwrap();
    shmalloc_unlocked(size)
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
    let _lock = ALLOC_MUTEX.lock().unwrap();
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

    // First try with volumes already mapped
    {
        let vols = VOLUMES.lock().unwrap();
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
        }
    }

    // If not found, try opening unmapped volumes
    remaining = ptr as usize;
    for i in 0..MAX_VOLUME_NUMBER {
        let shm = match shopen(i)? {
            Some(s) => s,
            None => {
                return Err(MorlocError::Shm(format!(
                    "Failed to find volume for relptr {}", ptr
                )));
            }
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

    Err(MorlocError::Shm(format!(
        "Shared memory pool does not contain index {}", ptr
    )))
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
            // Create a new volume
            drop(vols);
            let new_size = std::cmp::max(size * 2, 0xffff);
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
