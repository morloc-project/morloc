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

use std::sync::atomic::AtomicU32;

// ── Constants ──────────────────────────────────────────────────────────────

pub const SHM_MAGIC: u32 = 0xFECA_0DF0;
pub const BLK_MAGIC: u32 = 0x0CB1_0DF0;
pub const MAX_VOLUME_NUMBER: usize = 32;
pub const MAX_FILENAME_SIZE: usize = 128;
pub const MAX_PATH_SIZE: usize = 512;

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
