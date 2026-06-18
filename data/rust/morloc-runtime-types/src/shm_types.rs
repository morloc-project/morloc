//! Wire-format types and constants for shared memory.
//!
//! These are the type definitions and constants that describe SHM
//! layout. They carry no behaviour and reference no statics, so they
//! are safe to live in `morloc-runtime-types` and be shared between
//! `libmorloc.so` and `morloc-nexus`.
//!
//! The actual SHM state (volume table, allocator mutex, atexit
//! handler, mmap'd region) lives in `morloc-runtime::shm` and is
//! reached only through libmorloc.so's C ABI.

use std::sync::atomic::{AtomicPtr, AtomicU32, AtomicUsize};

// ── Constants ──────────────────────────────────────────────────────────────

pub const SHM_MAGIC: u32 = 0xFECA_0DF0;
pub const BLK_MAGIC: u32 = 0x0CB1_0DF0;
pub const MAX_VOLUME_NUMBER: usize = 32768;
pub const MAX_FILENAME_SIZE: usize = 128;
pub const MAX_PATH_SIZE: usize = 512;

// ── Pointer types ──────────────────────────────────────────────────────────

/// Relative pointer: encodes a (volume index, in-volume offset) pair.
///
/// Layout (64-bit):
///   bit 63        sign / sentinel flag
///   bits 62..48   15-bit volume index   (0..=32767)
///   bits 47..0    48-bit in-volume offset (0..=256 TiB)
///
/// RELNULL = -1 (all bits set) is reserved as the absent-value sentinel.
/// Other negative values are reserved for future sentinel use.
pub type RelPtr = isize;
/// Volume-local pointer: offset within a single volume (no encoding).
pub type VolPtr = isize;
/// Absolute pointer: virtual address in this process.
pub type AbsPtr = *mut u8;

pub const RELNULL: RelPtr = -1;
pub const VOLNULL: VolPtr = -1;

// ── Indexed relptr encoding ────────────────────────────────────────────────

pub const VOL_IDX_SHIFT: u32 = 48;
pub const VOL_IDX_MASK: u64 = 0x7FFF;                          // bits 62..48
pub const OFFSET_MASK: u64  = 0x0000_FFFF_FFFF_FFFF;            // bits 47..0
pub const SENTINEL_MASK: u64 = 0x8000_0000_0000_0000;           // bit 63

/// Pack a (volume_index, offset) pair into a RelPtr.
///
/// `volume_index` must be < `MAX_VOLUME_NUMBER` (15 bits) and `offset`
/// must fit in 48 bits. Producers always satisfy both; debug builds
/// assert it.
#[inline]
pub const fn encode_relptr(volume_index: usize, offset: usize) -> RelPtr {
    debug_assert!(volume_index < MAX_VOLUME_NUMBER);
    debug_assert!(offset <= OFFSET_MASK as usize);
    (((volume_index as u64) << VOL_IDX_SHIFT) | (offset as u64 & OFFSET_MASK)) as RelPtr
}

/// Extract the volume index from a (non-sentinel, non-negative) relptr.
#[inline]
pub const fn relptr_volume_index(ptr: RelPtr) -> usize {
    (((ptr as u64) >> VOL_IDX_SHIFT) & VOL_IDX_MASK) as usize
}

/// Extract the in-volume offset from a (non-sentinel, non-negative) relptr.
#[inline]
pub const fn relptr_offset(ptr: RelPtr) -> usize {
    ((ptr as u64) & OFFSET_MASK) as usize
}

/// True if the relptr carries the sentinel bit (RELNULL or future
/// reserved sentinels). Callers should special-case sentinels before
/// decoding the (volume, offset) pair.
#[inline]
pub const fn relptr_is_sentinel(ptr: RelPtr) -> bool {
    (ptr as u64) & SENTINEL_MASK != 0
}

// ── Block alignment ────────────────────────────────────────────────────────

pub const BLOCK_ALIGN: usize = std::mem::align_of::<BlockHeader>();

#[inline]
pub const fn align_up(x: usize, align: usize) -> usize {
    (x + align - 1) & !(align - 1)
}

// ── Exposed volume table (per-process, lock-free) ──────────────────────────

/// One entry in `MORLOC_VOL_TABLE`. Holds the data-region base address
/// and size of one SHM volume **in this process's address space**. Each
/// loaded volume publishes its entry from `shinit` / `shopen_diag`; the
/// `data_base != null` check serves as the publication gate.
///
/// The struct is exposed via `#[no_mangle]` static and consumed
/// directly from C/C++/Python/R bridges, so the layout is part of the
/// libmorloc.so ABI:
///
/// ```c
/// struct morloc_vol_entry {
///     _Atomic(void*)  data_base;   // 0 if slot empty in this process
///     _Atomic(size_t) data_size;   // valid when data_base != 0
/// };
/// ```
///
/// On x86-64 and ARM64 (the supported targets), `_Atomic(void*)` and
/// `_Atomic(size_t)` are layout-compatible with the plain types and
/// match Rust's `AtomicPtr<u8>` (which is `repr(transparent)` over
/// `*mut u8`) and `AtomicUsize`. The C side asserts the layout via
/// `_Static_assert`.
#[repr(C)]
pub struct MorlocVolEntry {
    pub data_base: AtomicPtr<u8>,
    pub data_size: AtomicUsize,
}

impl MorlocVolEntry {
    pub const fn empty() -> Self {
        MorlocVolEntry {
            data_base: AtomicPtr::new(std::ptr::null_mut()),
            data_size: AtomicUsize::new(0),
        }
    }
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

#[cfg(test)]
mod encoding_tests {
    use super::*;

    #[test]
    fn roundtrip_vol_idx_and_offset() {
        for &(v, o) in &[
            (0usize, 0usize),
            (0, 1),
            (1, 0),
            (1, 0x1234),
            (42, 0xABCDEF),
            (MAX_VOLUME_NUMBER - 1, 1usize << 47),
            (1234, (1usize << 48) - 1),
        ] {
            let p = encode_relptr(v, o);
            assert!(!relptr_is_sentinel(p), "vol={v} off={o}: leaked into sign bit");
            assert_eq!(relptr_volume_index(p), v, "vol roundtrip failed for ({v}, {o})");
            assert_eq!(relptr_offset(p), o, "offset roundtrip failed for ({v}, {o})");
        }
    }

    #[test]
    fn relnull_is_sentinel() {
        assert!(relptr_is_sentinel(RELNULL));
    }

    #[test]
    fn small_values_decode_as_vol0() {
        // Compatibility check for staged migration: a "small positive
        // integer" relptr (the kind the old flat-offset encoding produced
        // for offsets into volume 0) decodes as (volume 0, offset = ptr)
        // under the new encoding. Call sites that haven't yet been
        // updated and happen to operate inside volume 0 keep working.
        for &p in &[0i64, 1, 16, 1024, 0xFFFF] {
            let rp = p as RelPtr;
            assert!(!relptr_is_sentinel(rp));
            assert_eq!(relptr_volume_index(rp), 0);
            assert_eq!(relptr_offset(rp), p as usize);
        }
    }

    #[test]
    fn max_offset_in_bounds() {
        // 48-bit offset field can address exactly 256 TiB - 1 within a
        // single volume; anything larger should be rejected by callers
        // (debug_assert in encode_relptr).
        let off = (1usize << 48) - 1;
        let p = encode_relptr(0, off);
        assert_eq!(relptr_offset(p), off);
    }
}
