//! Stream registry, IFile mmap management, and file-targeting pattern
//! walker support for the streaming I/O system.
//!
//! ## Scope
//!
//! - **Shared SHM registry**: one slot table per nexus invocation in a
//!   reserved SHM volume; all pools attach to the same table. Cross-pool
//!   sharing of compressed bytes is handled by the kernel pagecache (every
//!   pool mmaps the same file); the SHM cache holds decompressed
//!   sub-packets per handle to avoid redundant zstd work.
//! - **Handle layout**: `(generation << 16) | slot`. Generation occupies
//!   47 bits (bit 63 stays clear so negative i64 is reserved for the FFI
//!   error sentinel) and bumps by a salted random step on each close;
//!   double-close, foreign-int collision, and ABA reuse all return a
//!   clean error.
//! - **IFile / IStream / OStream** all implemented against the shared
//!   registry; cross-pool writers/readers share the SHM-resident
//!   sub-packet index under each slot's futex.
//! - **Voidstar-only sub-packets**: open paths reject a stream whose
//!   first sub-packet's format byte is not `PACKET_FORMAT_VOIDSTAR`.
//!
//! ## Per-handle caches
//!
//! - `ProcessLocalSlot::cache` is the per-handle decompressed-sub-packet
//!   LRU; consulted by `cache_get_or_materialize` and survives across
//!   bracket accesses (refcount-shared with caller via `shincref`).
//! - `subpacket_index_local` is built greedily at open time (cheap for
//!   small N, acceptable for the million-sub-packet case at ~8 MiB).

use std::fs::OpenOptions;
use std::os::unix::io::AsRawFd;
use std::path::Path;
use std::sync::Mutex;

use morloc_runtime_types::packet::{
    decode_stream_tail,
    iter_packet_metadata,
    read_schema_from_meta,
    PacketHeader,
    METADATA_TYPE_FOOTER_FINAL, METADATA_TYPE_STREAM_DIAG,
    METADATA_TYPE_SUBPACKET_INDEX,
    MLC_KIND_IFILE, MLC_KIND_ISTREAM, MLC_KIND_OSTREAM,
    PACKET_COMPRESSION_NONE, PACKET_COMPRESSION_ZSTD,
    PACKET_FORMAT_VOIDSTAR,
    StreamDiag, STREAM_TAIL_SIZE,
    handle_kind_name,
    packet_format_name,
};
use morloc_runtime_types::schema::{parse_schema, Schema, SerialType};
use morloc_runtime_types::shm_types::{
    self as shm_types_crate, relptr_is_sentinel, relptr_offset, RelPtr,
};

use crate::error::MorlocError;
use crate::shm::{self, AbsPtr};
use crate::voidstar;

// ── Constants ─────────────────────────────────────────────────────────────

/// Max concurrent open handles per process (low 16 bits of the handle).
/// Aligns with the architectural commitment of a 16-bit slot index.
pub const STREAM_SLOT_COUNT: usize = 65_536;

/// Default slot count for the shared SHM stream registry. Overridable
/// via `MORLOC_REGISTRY_SLOT_COUNT`. 4096 covers realistic workloads
/// (hundreds of concurrent open files at peak); the env override is
/// for niche cases where a single nexus invocation needs more.
pub const STREAM_REGISTRY_DEFAULT_SLOT_COUNT: usize = 4096;

/// Maximum slot count the registry will honour (16-bit slot index).
/// Hard cap mirrors `STREAM_SLOT_COUNT`; the user can request fewer
/// via the env var but never more.
pub const STREAM_REGISTRY_MAX_SLOT_COUNT: usize = STREAM_SLOT_COUNT;

/// Fixed on-disk size of one `RegistrySlot`. The actual struct carries
/// a `#[repr(C, align(64))]` annotation and a `const_assert!` so the
/// layout matches this constant; pinning the size as a constant
/// decouples the volume-size computation from the field-by-field
/// struct layout.
pub const STREAM_ENTRY_SIZE: usize = 512;

/// Default OStream write-buffer capacity in bytes. Each `@write`
/// appends its elements to this SHM-resident per-slot buffer; when
/// the buffer fills, contents are flushed as a single sub-packet.
/// Overridable via `MORLOC_WRITE_BUFFER_BYTES`.
///
/// 16 MiB matches the planfile's FRAME_CHUNK_SIZE: large enough for
/// zstd to build a useful compression dictionary, small enough that
/// a single buffer doesn't dominate per-slot memory. With 4096 slots
/// fully populated the worst-case total is 64 GiB -- normal workloads
/// have only a handful of OStreams open at once.
pub const WRITE_BUFFER_BYTES_DEFAULT: usize = 16 * 1024 * 1024;

/// Initial capacity (number of elements) of the buffer's index
/// section. The index section is preallocated so growth happens by
/// doubling (amortised O(1) per element) rather than by reallocating
/// on every @write. 1024 covers most small @write call sequences
/// without resize; larger workloads pay the doubling cost a handful
/// of times to reach their working set.
pub const WRITE_BUFFER_INDEX_INITIAL_CAP: u64 = 1024;

/// Initial capacity (number of u64 entries) of an OStream slot's
/// SHM-resident sub-packet index. Sub-packet boundaries from every
/// writer pool append here under the slot futex; @close reads it to
/// build the file's final footer. Grows by doubling. 16 covers the
/// common "open + a few flushes + close" shape without resize; larger
/// workloads pay the doubling cost a handful of times.
pub const OSTREAM_SUBPACKET_INDEX_INITIAL_CAP: u64 = 16;

/// Default cache capacity (bytes) for decompressed sub-packets per
/// handle. Overridable via `MORLOC_IFILE_CACHE_BYTES`.
const DEFAULT_IFILE_CACHE_BYTES: u64 = 256 * 1024 * 1024;

// ── Shared SHM stream registry: bootstrap ────────────────────────────────
//
// The registry lives in a dedicated SHM volume (`STREAM_REGISTRY_VOLUME`)
// allocated once per nexus invocation. Layout of the volume's data
// region:
//
//   offset 0:                 RegistryHeader (64 bytes)
//   offset 64:                slot[0]
//   offset 64 + N*512:        slot[N-1]
//
// All slot fields are atomically accessed. The header holds the `magic`
// gate that publishes "init is complete" to attaching processes plus the
// slot count (so attaching processes know the volume bounds without
// re-reading the env).
//
// The volume is created by the FIRST process to call `registry_init()`
// for a given nexus session. Other processes (typically pool daemons)
// call `registry_attach()`, which uses the existing `shopen` machinery
// and then spin-waits on the magic gate.

/// Header at offset 0 of the registry volume's data region. Pinned at
/// 64 bytes (one cache line) so it doesn't share a line with slot[0].
///
/// `magic` is the publication gate: the bootstrap winner writes
/// `STREAM_REGISTRY_MAGIC` with `Release` ordering AFTER zero-init'ing
/// the slot array. Attaching processes Acquire-load `magic` in a spin
/// loop; once the magic is observed, all subsequent reads of slot
/// fields happen-after the winner's zeroing.
#[repr(C, align(64))]
struct RegistryHeader {
    /// Publication gate. Zero until the bootstrap winner completes
    /// initialization, then `STREAM_REGISTRY_MAGIC`.
    magic:        std::sync::atomic::AtomicU64,

    /// Number of slots in the slot array following this header.
    /// Written by the bootstrap winner BEFORE `magic`; readers see
    /// it as part of the happens-before established by the magic.
    slot_count:   u64,

    /// Per-process random salt mixed into the generation increment on
    /// every slot close. Drawn from /dev/urandom by the bootstrap
    /// winner; nonzero so the increment is never trivially zero. All
    /// processes attaching to the registry observe the same salt.
    gen_salt:     u64,

    /// Reserved for future use (e.g. format versioning, sweeper-thread
    /// liveness counter). Zero-initialised.
    _reserved:    [u8; 40],
}

const _: () = {
    assert!(std::mem::size_of::<RegistryHeader>() == 64);
};

/// Compute the volume size required to hold the registry header + N
/// slots. Result is page-aligned by `shinit`, so we just return the
/// logical byte requirement.
///
/// We add `sizeof(BlockHeader)` because `shinit` initialises a single
/// allocator BlockHeader at the start of the data region (regular
/// volumes are managed by the SHM allocator). The registry skips
/// past that BlockHeader -- it doesn't go through shmalloc/shfree
/// -- so we need to reserve that prefix in the volume size.
///
/// The header offset is rounded up to `align_of::<RegistryHeader>()`
/// (= 64) so the header and the slot array following it are correctly
/// aligned. The padding is added here so the volume is large enough
/// to fit the aligned layout.
const fn registry_volume_size(slot_count: usize) -> usize {
    let raw_offset = std::mem::size_of::<shm_types_crate::BlockHeader>();
    let align = std::mem::align_of::<RegistryHeader>();
    let aligned_offset = (raw_offset + align - 1) & !(align - 1);
    aligned_offset
        + std::mem::size_of::<RegistryHeader>()
        + slot_count * STREAM_ENTRY_SIZE
}

/// Read the desired slot count from the `MORLOC_REGISTRY_SLOT_COUNT`
/// env var, defaulting to `STREAM_REGISTRY_DEFAULT_SLOT_COUNT`. Caps
/// at `STREAM_REGISTRY_MAX_SLOT_COUNT`; rejects 0 (use default instead).
fn read_registry_slot_count() -> usize {
    if let Ok(s) = std::env::var("MORLOC_REGISTRY_SLOT_COUNT") {
        if let Ok(n) = s.parse::<usize>() {
            if n == 0 {
                return STREAM_REGISTRY_DEFAULT_SLOT_COUNT;
            }
            return n.min(STREAM_REGISTRY_MAX_SLOT_COUNT);
        }
    }
    STREAM_REGISTRY_DEFAULT_SLOT_COUNT
}

/// Read 8 bytes of entropy from `/dev/urandom` for the per-nexus
/// generation-increment salt. ORs with 1 so the increment is never
/// zero (else a close+open cycle wouldn't bump generation). Falls
/// back to a time-mixed value if /dev/urandom is unavailable.
fn read_gen_salt() -> u64 {
    use std::io::Read;
    if let Ok(mut f) = std::fs::File::open("/dev/urandom") {
        let mut buf = [0u8; 8];
        if f.read_exact(&mut buf).is_ok() {
            return u64::from_le_bytes(buf) | 1;
        }
    }
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0xDEAD_BEEF);
    (now ^ 0xA5A5_A5A5_A5A5_A5A5) | 1
}

/// Process-local cache of the attached registry's base pointer + slot
/// count. Set on first successful `registry_init` or `registry_attach`
/// in this process; stays set for the process lifetime.
///
/// The pointer is into the registry SHM volume mapped at process attach
/// time; subsequent `rel2abs` of slots in this volume goes through
/// `MORLOC_VOL_TABLE` without needing this cache.
static REGISTRY_BASE: std::sync::atomic::AtomicPtr<RegistryHeader> =
    std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());
static REGISTRY_SLOT_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

/// Initialise the shared stream registry for this nexus invocation.
/// Allocates (or attaches to, if already created by an earlier
/// libmorloc.so caller in the same session) the `STREAM_REGISTRY_VOLUME`
/// SHM segment, zero-fills the slot array, and writes the magic gate.
///
/// Called by the nexus startup path before any dispatch fires. Pool
/// processes that join the session later call `registry_attach()`
/// instead.
///
/// Safe to call more than once: subsequent calls observe the cached
/// `REGISTRY_BASE` and short-circuit. Returns the slot count in effect
/// so callers can sanity-check.
pub fn registry_init() -> Result<usize, MorlocError> {
    use std::sync::atomic::Ordering;

    // Fast path: already initialised in this process.
    let cached = REGISTRY_BASE.load(Ordering::Acquire);
    if !cached.is_null() {
        return Ok(REGISTRY_SLOT_COUNT.load(Ordering::Relaxed));
    }

    let slot_count = read_registry_slot_count();
    let volume_bytes = registry_volume_size(slot_count);

    // `shinit` is idempotent on a given (basename, volume_index) pair:
    // the first caller creates the segment and fills the header; later
    // callers attach to the existing one. The `created` distinction is
    // internal to `shinit`; we don't need it here because we use a
    // separate publication gate (the registry's own `magic` word) to
    // serialise initialisation across processes.
    let shm = shm::shinit(
        &shm::get_common_basename(),
        shm_types_crate::STREAM_REGISTRY_VOLUME,
        volume_bytes,
    )?;

    // The data region starts after the ShmHeader + BlockHeader prefix
    // shinit lays down for regular allocator-managed volumes. We skip
    // those because the registry owns a fixed, immutable region. The
    // raw offset (sizeof(ShmHeader) + sizeof(BlockHeader)) is not
    // a multiple of `align_of::<RegistryHeader>()` (= 64), so we
    // round the offset up before reinterpreting as RegistryHeader.
    // Without this rounding, every RegistrySlot following the header
    // sits at the same sub-64-byte offset; codegen that assumes the
    // struct's declared 64-byte alignment (e.g. vectorised batch
    // stores in `release_slot_locked`'s field-reset block) issues
    // aligned-vector instructions that fault on the misaligned slot.
    let raw_offset = std::mem::size_of::<shm_types_crate::ShmHeader>()
        + std::mem::size_of::<shm_types_crate::BlockHeader>();
    let align = std::mem::align_of::<RegistryHeader>();
    let aligned_offset = (raw_offset + align - 1) & !(align - 1);
    let data_base = unsafe {
        (shm as *mut u8).add(aligned_offset)
    } as *mut RegistryHeader;
    let header = unsafe { &*data_base };

    // CAS-arbitrated bootstrap. Exactly one process transitions
    // `magic` from 0 to `STREAM_REGISTRY_MAGIC_INIT`, writes
    // `slot_count` + `gen_salt`, then Release-stores `magic` to
    // `STREAM_REGISTRY_MAGIC`. Other processes observe a non-zero
    // magic and spin-wait until it reaches the final value.
    //
    // The freshly-mmap'd file is zero on first touch (kernel guarantees
    // zero pages for sparse file mappings), so `magic.load() == 0` is
    // the bootstrap-not-started signal.
    let cas = header.magic.compare_exchange(
        0,
        shm_types_crate::STREAM_REGISTRY_MAGIC_INIT,
        Ordering::AcqRel,
        Ordering::Acquire,
    );
    match cas {
        Ok(_) => {
            // We won; fill the header. Plain u64 writes are safe here
            // because no other process can read these fields until we
            // Release-store the final magic below.
            unsafe {
                (*data_base).slot_count = slot_count as u64;
                (*data_base).gen_salt = read_gen_salt();
            }
            header.magic.store(
                shm_types_crate::STREAM_REGISTRY_MAGIC,
                Ordering::Release,
            );
        }
        Err(observed)
            if observed != shm_types_crate::STREAM_REGISTRY_MAGIC
                && observed != shm_types_crate::STREAM_REGISTRY_MAGIC_INIT
        => {
            return Err(MorlocError::Other(format!(
                "stream registry: unexpected magic {:#x} in volume {} \
                 (expected {:#x} or init sentinel); concurrent libmorloc.so \
                 version mismatch?",
                observed,
                shm_types_crate::STREAM_REGISTRY_VOLUME,
                shm_types_crate::STREAM_REGISTRY_MAGIC,
            )));
        }
        Err(_) => {
            // Lost the race; another process is mid-bootstrap or done.
            // Spin below.
        }
    }

    // Spin until magic is the final value. Bounded so a stuck
    // bootstrapper can't hang us indefinitely.
    let mut spins: u32 = 0;
    loop {
        let m = header.magic.load(Ordering::Acquire);
        if m == shm_types_crate::STREAM_REGISTRY_MAGIC {
            break;
        }
        spins += 1;
        if spins > 1_000_000 {
            return Err(MorlocError::Other(format!(
                "stream registry: magic never published in volume {} \
                 (spin-wait exhausted; bootstrapper stalled?)",
                shm_types_crate::STREAM_REGISTRY_VOLUME,
            )));
        }
        std::hint::spin_loop();
    }

    REGISTRY_BASE.store(data_base, Ordering::Release);
    REGISTRY_SLOT_COUNT.store(slot_count, Ordering::Relaxed);

    // Spawn the dedicated sweeper thread now that the registry is
    // published. `sweeper_init` is itself idempotent so the second-
    // and subsequent-attaching processes that pass through here
    // (pool daemons, worker children) get a fast no-op.
    sweeper_init();

    Ok(slot_count)
}

/// Attach to an already-initialised stream registry (the path pool
/// processes take when they join a nexus session that the nexus binary
/// has already bootstrapped). Returns the slot count on success.
///
/// Internally just calls `registry_init`: the `shinit` it triggers
/// is idempotent for an already-created volume (attaches to the
/// existing one), and the publication-gate logic handles the "winner
/// already set the magic" case as a no-op spin that resolves on the
/// first Acquire-load.
pub fn registry_attach() -> Result<usize, MorlocError> {
    registry_init()
}

/// Return a typed pointer to the slot array start, or null if the
/// registry isn't yet attached in this process. Callers MUST call
/// `registry_init` / `registry_attach` first. The returned pointer is
/// process-local but stable for the registry's lifetime (the registry
/// SHM volume isn't remapped after init).
#[inline]
pub(crate) fn registry_slot_array() -> (*mut u8, usize) {
    use std::sync::atomic::Ordering;
    let base = REGISTRY_BASE.load(Ordering::Acquire);
    if base.is_null() {
        return (std::ptr::null_mut(), 0);
    }
    // Slot array starts immediately after the 64-byte header.
    let slots = unsafe {
        (base as *mut u8).add(std::mem::size_of::<RegistryHeader>())
    };
    let count = REGISTRY_SLOT_COUNT.load(Ordering::Relaxed);
    (slots, count)
}

/// Return the registry's per-nexus generation-increment salt. The salt
/// is set by the bootstrap winner and is the same value seen by every
/// attached process. Used by the slot-close path to bump the generation
/// by a salted-random step instead of monotonic +1.
#[inline]
pub(crate) fn registry_gen_salt() -> u64 {
    use std::sync::atomic::Ordering;
    let base = REGISTRY_BASE.load(Ordering::Acquire);
    if base.is_null() {
        return 0;
    }
    unsafe { (*base).gen_salt }
}

// ── Shared SHM stream registry: slot layout ──────────────────────────────
//
// `RegistrySlot` is the SHM-resident per-slot record. The registry's
// data region holds:
//
//   offset 0:                       RegistryHeader (64 bytes)
//   offset 64:                      RegistrySlot[0]
//   offset 64 + N*STREAM_ENTRY_SIZE: RegistrySlot[N-1]
//
// All concurrent access goes through the atomic fields. The publication
// protocol for immutable-after-@open fields (file_path, schema_str,
// kind, final_footer, subpacket_index, body_start) is:
//
//   Writer (in @open): write fields with plain stores, then
//                      generation.store(new_gen, Release).
//   Reader (any pool): generation.load(Acquire) = g0;
//                      read fields;
//                      generation.load(Acquire) = g1;
//                      if g0 != g1, retry from the top.
//
// Mutable-under-futex fields (cursor, element_count, diag) require
// taking `futex` before read or write. Lockfree snapshot reads of
// these are explicitly NOT supported by this protocol.

/// Per-slot state byte. Stored in the `state` AtomicU8. `FREE` is
/// re-allocatable (implicit zero on init); `OPEN_SHARED` marks a slot
/// in active use; `REMOTE_PAUSED` is reserved for cross-nexus IStream
/// pause-on-egress.
pub const SLOT_STATE_FREE: u8 = 0;
pub const SLOT_STATE_OPEN_SHARED: u8 = 1;
pub const SLOT_STATE_REMOTE_PAUSED: u8 = 2;

/// Sentinel `call_id` value: "this slot should NOT be swept at end of
/// any call". Used by the cross-nexus return-serialization path to
/// transfer slot ownership from the remote's call to the caller's
/// scope. The sweeper compares against non-zero `call_id`s only, so
/// slots with this value are always skipped.
pub const CALL_ID_NO_SWEEP: u64 = 0;

/// SHM-resident per-slot record. Layout is pinned at 512 bytes
/// (`STREAM_ENTRY_SIZE`) so the slot array can be addressed by
/// fixed-stride arithmetic without indirection.
///
/// Fields are grouped by mutability + protection:
///   - **Immutable after @open**: `kind`, `opener_pid`,
///     `opener_pid_start_time`, `file_path`, `file_path_len`,
///     `schema_str`, `schema_str_len`, `final_footer`,
///     `compression_level`, `subpacket_index`, `subpacket_index_len`,
///     `body_start`. Readers use the versioned-pointer pattern
///     described above.
///   - **Mutated under `futex`**: `cursor`, `element_count`, `diag`.
///   - **Independently atomic**: `generation`, `call_id`, `state`,
///     `futex`. These are accessed without holding any other lock.
#[repr(C, align(64))]
pub struct RegistrySlot {
    // ── Identity / lifecycle (atomically accessed) ──────────────────
    /// Bumped by salted random increment on every close. Publication-
    /// order: all field writes happen-before the Release-store of this
    /// at @open's end. Cross-pool readers Acquire-load this BEFORE
    /// reading other fields and re-load AFTER; if changed, retry.
    pub generation:           std::sync::atomic::AtomicU64,    // off 0..8

    /// call_id assigned at @open time. `CALL_ID_NO_SWEEP` (= 0) means
    /// "do not sweep" -- used by cross-nexus return-serialization to
    /// transfer ownership.
    pub call_id:              std::sync::atomic::AtomicU64,    // off 8..16

    /// SLOT_STATE_FREE / SLOT_STATE_OPEN_SHARED / SLOT_STATE_REMOTE_PAUSED.
    /// Allocation is a CAS on this field (FREE -> OPEN_SHARED).
    pub state:                std::sync::atomic::AtomicU8,     // off 16
    /// MLC_KIND_IFILE / MLC_KIND_ISTREAM / MLC_KIND_OSTREAM.
    /// Immutable after @open's generation publication.
    pub kind:                 u8,                              // off 17
    _pad0:                    [u8; 6],                         // off 18..24

    /// Opener-pool process start time, read from `/proc/PID/stat`
    /// field 22 (clock ticks since boot). Defends against PID reuse
    /// across pool restarts in the §1.7 PID sweep. Plain u64;
    /// immutable after @open publication.
    pub opener_pid_start_time: u64,                            // off 24..32

    /// OS PID of the pool that opened the slot. Used only by the §1.7
    /// PID sweep on pool exit / crash recovery.
    pub opener_pid:           u32,                             // off 32..36
    _pad1:                    [u8; 4],                         // off 36..40

    /// Per-slot mutation futex. Held for cursor / element_count / diag
    /// updates. NOT held for lockfree reads of immutable-after-open
    /// fields (those use the versioned-pointer pattern against
    /// `generation`).
    pub futex:                std::sync::atomic::AtomicU32,    // off 40..44
    _pad2:                    [u8; 4],                         // off 44..48

    // ── File identity (immutable after publication) ─────────────────
    /// SHM RelPtr to a UTF-8 path string. Allocated from the shared
    /// SHM allocator at @open; freed at @close via shfree. Length is
    /// in `file_path_len`. NOT canonicalised; the planfile commits
    /// to explicit-only multi-writer sharing (no realpath dedup).
    pub file_path:            RelPtr,                          // off 48..56
    pub file_path_len:        u32,                             // off 56..60
    _pad3:                    [u8; 4],                         // off 60..64

    pub schema_str:           RelPtr,                          // off 64..72
    pub schema_str_len:       u32,                             // off 72..76
    _pad4:                    [u8; 4],                         // off 76..80

    // ── Mutable state under `futex` ─────────────────────────────────
    /// IStream/OStream cursor (byte offset). IFile leaves at 0
    /// (random access goes through `subpacket_index` instead).
    pub cursor:               u64,                             // off 80..88
    pub element_count:        u64,                             // off 88..96

    /// IFile only; set at @open from the file's footer. Plain u8 0/1
    /// for cross-process clarity. Immutable after @open publication.
    pub final_footer:         u8,                              // off 96
    pub compression_level:    u8,                              // off 97
    _pad5:                    [u8; 6],                         // off 98..104

    /// IFile only; SHM RelPtr to a `[u64]` array of sub-packet byte
    /// offsets. Length in `subpacket_index_len`. Immutable after @open
    /// publication; freed at @close.
    pub subpacket_index:      RelPtr,                          // off 104..112
    pub subpacket_index_len:  u64,                             // off 112..120

    /// IStream initial cursor (right after stream header). Immutable
    /// after @open publication.
    pub body_start:           u64,                             // off 120..128

    /// Per-write diagnostic / running counters. Updated by OStream
    /// writers under `futex`; readers either hold the futex or accept
    /// momentarily-stale values. ~160 bytes embedded inline.
    pub diag:                 StreamDiag,                      // off 128..288

    // ── OStream write buffer (Part A of the buffering work) ────────
    /// SHM RelPtr to a per-slot write buffer. Allocated at @open
    /// OStream (`WRITE_BUFFER_BYTES_DEFAULT` bytes, env-overridable),
    /// freed at slot release. The buffer's first 16 bytes are
    /// reserved for the Array header (filled at flush); bytes 16..
    /// 16+`index_cap`*elem_width are the inline element index; the
    /// remainder is the variable-data section. Cross-pool writers
    /// append to this same buffer under the slot futex.
    pub write_buffer:           RelPtr,                        // off 288..296

    /// Current index-section capacity in ELEMENTS. Starts at
    /// `WRITE_BUFFER_INDEX_INITIAL_CAP` (1024); doubles when filled
    /// (shifting the data region right). Always >= write_buffer_index_count.
    pub write_buffer_index_cap: u64,                           // off 296..304

    /// Number of elements currently buffered (i.e. inline entries
    /// in the index section). Reset to 0 after each flush.
    pub write_buffer_index_count: u64,                         // off 304..312

    /// Bytes currently used in the data section (variable-length
    /// portion). Reset to 0 after each flush.
    pub write_buffer_data_used: u64,                           // off 312..320

    /// OStream-only: capacity in entries of the SHM-resident sub-packet
    /// index whose RelPtr lives in `subpacket_index`. Grown by doubling
    /// under the slot futex when `subpacket_index_len` reaches it. For
    /// IFile this field is 0 (the index is set once from the parsed
    /// final footer and never grows).
    pub subpacket_index_cap:  u64,                             // off 320..328

    /// Padding to round the slot up to STREAM_ENTRY_SIZE so the next
    /// slot starts on a fresh cache-line-aligned boundary.
    _tail_pad:                [u8; 184],                       // off 328..512
}

const _: () = {
    assert!(std::mem::size_of::<RegistrySlot>() == STREAM_ENTRY_SIZE);
};

/// Resolve a slot index to a typed reference into the SHM-resident
/// slot array. Returns `None` if the registry isn't attached yet or
/// the index is out of range. The reference is valid for the rest of
/// the registry's lifetime (the SHM volume isn't remapped after init).
#[inline]
pub(crate) fn slot_ref(slot_idx: usize) -> Option<&'static RegistrySlot> {
    // Pool processes attach lazily; see allocate_slot_cas for rationale.
    let _ = registry_init();
    let (slots_base, slot_count) = registry_slot_array();
    if slots_base.is_null() || slot_idx >= slot_count {
        return None;
    }
    let ptr = unsafe {
        slots_base.add(slot_idx * STREAM_ENTRY_SIZE) as *const RegistrySlot
    };
    // SAFETY: ptr points into a valid SHM-mapped region of size
    // `slot_count * STREAM_ENTRY_SIZE`; the registry stays mapped for
    // the lifetime of the process (until shclose at exit).
    Some(unsafe { &*ptr })
}

/// Pack a slot's current generation + slot index into a handle int.
/// Layout: 47 bits of generation in bits 16-62, 16 bits of slot
/// index in bits 0-15. Bit 63 is held at 0 so that handles are
/// always non-negative i64 -- callers across the FFI use a
/// negative return value as the error sentinel.
#[inline]
pub(crate) fn pack_handle(generation: u64, slot_idx: usize) -> i64 {
    ((generation << 16) | (slot_idx as u64 & 0xFFFF)) as i64
}

/// Decode a handle int into (generation, slot_idx). Inverse of
/// `pack_handle`. Returns the upper-47-bit generation and the
/// 16-bit slot index.
#[inline]
pub(crate) fn unpack_handle(handle: i64) -> (u64, usize) {
    let h = handle as u64;
    let generation = h >> 16;
    let slot_idx = (h & 0xFFFF) as usize;
    (generation, slot_idx)
}

/// Bit mask for the generation field carried in handle ints. The
/// generation occupies 47 bits so that `pack_handle`'s output --
/// formed by left-shifting the masked generation by 16 -- always
/// has bit 63 clear, keeping handles non-negative. FFI callers
/// treat negative return values as error sentinels, so a packed
/// handle must never collide with that domain.
pub(crate) const GENERATION_MASK: u64 = 0x0000_7FFF_FFFF_FFFF;

// ── Slot futex helpers ────────────────────────────────────────────────────
//
// Each `RegistrySlot` has its own `futex: AtomicU32` word that
// serialises mutations to `cursor`, `element_count`, and `diag`.
// Held briefly; never across mmap / file I/O. We use a simple
// spin-then-yield protocol rather than a real futex syscall because
// hold times are O(memcpy) and contention is rare. The `_yield`
// import keeps us out of the kernel even under heavy contention.

const SLOT_FUTEX_UNLOCKED: u32 = 0;
const SLOT_FUTEX_LOCKED:   u32 = 1;

/// Acquire the slot's mutation lock. Spins with `spin_loop` hints
/// until the CAS succeeds, then a `yield_now` if we've spun more
/// than a few thousand iterations. No timeout (the lock should
/// never be held across blocking I/O).
fn slot_futex_lock(slot: &RegistrySlot) {
    use std::sync::atomic::Ordering;
    let mut spins: u32 = 0;
    loop {
        if slot.futex
            .compare_exchange(
                SLOT_FUTEX_UNLOCKED, SLOT_FUTEX_LOCKED,
                Ordering::Acquire, Ordering::Relaxed,
            )
            .is_ok()
        {
            return;
        }
        spins = spins.wrapping_add(1);
        if spins & 0x3FF == 0 {
            std::thread::yield_now();
        } else {
            std::hint::spin_loop();
        }
    }
}

fn slot_futex_unlock(slot: &RegistrySlot) {
    use std::sync::atomic::Ordering;
    slot.futex.store(SLOT_FUTEX_UNLOCKED, Ordering::Release);
}

/// RAII helper: locks the slot's futex on construction, unlocks on
/// Drop. Use this in code that takes the futex on entry and may
/// have multiple return paths.
struct SlotFutexGuard<'a> {
    slot: &'a RegistrySlot,
}
impl<'a> SlotFutexGuard<'a> {
    fn lock(slot: &'a RegistrySlot) -> Self {
        slot_futex_lock(slot);
        SlotFutexGuard { slot }
    }
}
impl<'a> Drop for SlotFutexGuard<'a> {
    fn drop(&mut self) { slot_futex_unlock(self.slot); }
}

// ── Process-local mmap cache ─────────────────────────────────────────────
//
// The SHM `RegistrySlot` holds the IDENTITY of an open stream (path,
// schema, cursor, etc.). The per-process `ProcessLocalSlot` holds the
// PHYSICAL OS state for that slot in this process: fd, mmap region,
// decompression cache. Splitting them this way lets the SHM slot be
// shared across pools without trying to share fds (which can't be
// safely shared across forks anyway).
//
// Lazy invalidation: every access through `with_process_local_slot`
// re-checks the SHM slot's generation against the cached_generation.
// On mismatch, the local entry is dropped (closing the fd and unmap)
// and the access falls back to attach-by-path-from-SHM.

/// Per-process physical state for an open SHM slot.
pub struct ProcessLocalSlot {
    /// Generation observed at the last validation. If the SHM slot's
    /// current generation differs, this entry is stale and must be
    /// dropped before use.
    pub cached_generation: u64,

    /// PROT_READ mmap of the underlying file. For IFile/IStream this
    /// covers the entire file. For OStream this is null (writes go
    /// through `pwrite` against `fd`).
    pub mmap_ptr:  AbsPtr,
    pub mmap_size: u64,

    /// Underlying file descriptor. For OStream this is the fd that
    /// holds the flock (only the OPENER pool acquires the flock;
    /// non-opener writers use their own non-flock'd fd). For
    /// IFile/IStream this is -1 (we close after mmap).
    pub fd: i32,

    /// Per-handle decompressed-sub-packet LRU. Lives on the heap so
    /// dropping the entry from the HashMap shfree's the SHM blocks
    /// referenced by the cache.
    pub cache: Box<StreamCache>,

    /// Parsed value schema cached from the SHM slot's `schema_str`.
    /// Each pool parses on first attach; the cost is a microsecond-
    /// scale string parse that's done once per pool per handle.
    pub value_schema: Schema,

    /// Parsed element schema (for list-valued kinds, the schema of
    /// one element). For non-list values this equals `value_schema`.
    pub elem_schema: Schema,

    /// IFile only: copy of the SHM slot's sub-packet index. Resident
    /// in process-local memory for fast random access without going
    /// through `rel2abs` per query. Empty for IStream / OStream.
    pub subpacket_index_local: Vec<u64>,

    /// IFile only: cumulative element-count per sub-packet, built
    /// lazily on first random-access query. `cum[i]` is the total
    /// element count of sub-packets `0..i`, so a global element index
    /// `g` lands in the sub-packet `partition_point(g)`. Survives
    /// across bracket accesses on the same handle -- rebuilding on
    /// every access would be O(N sub-packet headers read) per call.
    pub subpacket_elem_cum: Option<Vec<u64>>,

    /// For DATA_PACKET files opened as IFile, the file is a single
    /// "sub-packet" starting at offset 0 and the subpacket_index has
    /// one entry [0]. The walker handles this flag to skip the
    /// stream-header / footer parsing paths.
    pub is_data_packet: bool,
}

impl Drop for ProcessLocalSlot {
    fn drop(&mut self) {
        // Drop the decompression cache first; this shfree's any
        // SHM blocks the cache references. Then unmap the file
        // region. Then close the fd (which releases flock if held).
        for entry in self.cache.entries.drain(..) {
            let _ = crate::shm::shfree(entry.shm_packet);
        }
        if !self.mmap_ptr.is_null() && self.mmap_size > 0 {
            unsafe {
                libc::munmap(self.mmap_ptr as *mut libc::c_void, self.mmap_size as usize);
            }
        }
        if self.fd >= 0 {
            unsafe { libc::close(self.fd); }
        }
    }
}

// SAFETY: ProcessLocalSlot's only !Send field is `mmap_ptr` (a raw
// `*mut u8` returned by mmap) and the AbsPtr fields inside `cache`.
// The mmap region is process-wide (kernel-resident, not thread-bound),
// the fd is a kernel handle with no thread affinity, and the cache's
// SHM blocks are refcounted in SHM. Access is serialised by the
// PROCESS_LOCAL_SLOTS Mutex; we move ownership across threads only
// while the entry is detached from the map.
unsafe impl Send for ProcessLocalSlot {}

/// Per-process map from handle int to physical OS state. Lazily
/// initialised on first access; cleared on `shclose_atexit`-style
/// teardown.
///
/// The Mutex protects the HashMap; per-handle work happens with the
/// Mutex released (the lookup briefly takes the lock to find or
/// install the entry, then operates on the entry via raw pointers
/// to avoid holding the map lock across blocking I/O).
static PROCESS_LOCAL_SLOTS: Mutex<Option<std::collections::HashMap<i64, ProcessLocalSlot>>> =
    Mutex::new(None);

/// Run `f` against the process-local slot for `handle`. On entry the
/// function validates the SHM slot's generation against the cached
/// entry; if stale (or missing), it drops any stale entry and
/// attaches afresh by reading the SHM slot's path + kind.
///
/// The cached entry is removed from the map for the duration of `f`,
/// then re-inserted on completion. This avoids holding the map lock
/// across `f`'s body (which may do file I/O or hold the slot futex).
pub fn with_process_local_slot<R>(
    handle: i64,
    f: impl FnOnce(&mut ProcessLocalSlot, &'static RegistrySlot) -> Result<R, MorlocError>,
) -> Result<R, MorlocError> {
    use std::sync::atomic::Ordering;

    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "stream handle {:#x}: slot index {} out of range; \
         registry not initialised or handle is corrupt",
        handle, slot_idx,
    )))?;

    // Versioned-pointer pre-check: read generation under Acquire so
    // any subsequent reads of immutable-after-open fields see the
    // current publication.
    let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_now != gen_claim {
        return Err(MorlocError::Other(format!(
            "stream handle {:#x}: generation mismatch (claim {}, slot has {}); \
             handle was closed or refers to a recycled slot",
            handle, gen_claim, gen_now,
        )));
    }
    let state = slot.state.load(Ordering::Acquire);
    if state != SLOT_STATE_OPEN_SHARED {
        return Err(MorlocError::Other(format!(
            "stream handle {:#x}: slot is not OPEN (state = {})",
            handle, state,
        )));
    }

    // Take the entry out of the map for the duration of f, so we
    // don't hold the map lock during file I/O.
    let mut taken = take_process_local_slot(handle);

    // Validate cached_generation; drop on mismatch.
    if let Some(existing) = &taken {
        if existing.cached_generation != gen_now {
            taken = None;  // drop triggers Drop::drop on the old entry
        }
    }
    let mut local = match taken {
        Some(s) => s,
        None => attach_process_local_slot(handle, slot, gen_now)?,
    };

    // Re-validate generation AFTER reads of immutable-after-open
    // fields (versioned-pointer pattern epilogue). If the slot was
    // closed and reopened during attach, the second read catches
    // it; we error and let the caller retry rather than papering
    // over the race.
    let gen_after = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_after != gen_now {
        // The freshly attached local slot is now stale; drop it
        // (Drop::drop runs).
        drop(local);
        return Err(MorlocError::Other(format!(
            "stream handle {:#x}: slot was closed mid-attach (generation \
             went from {} to {}); retry",
            handle, gen_now, gen_after,
        )));
    }

    // Run f with the validated local slot.
    let result = f(&mut local, slot);

    // Re-install the local slot in the map for future calls. We
    // always re-install, even on Err, so the cache isn't dropped
    // just because the operation failed.
    install_process_local_slot(handle, local);

    result
}

/// Helper: remove and return the entry for `handle` from the
/// process-local map, leaving the slot to be re-installed by the
/// caller. Lazily creates the map on first use.
fn take_process_local_slot(handle: i64) -> Option<ProcessLocalSlot> {
    let mut guard = PROCESS_LOCAL_SLOTS.lock().unwrap();
    let map = guard.get_or_insert_with(std::collections::HashMap::new);
    map.remove(&handle)
}

/// Helper: insert an entry into the process-local map, replacing any
/// existing entry (the caller already validated generation).
fn install_process_local_slot(handle: i64, slot: ProcessLocalSlot) {
    let mut guard = PROCESS_LOCAL_SLOTS.lock().unwrap();
    let map = guard.get_or_insert_with(std::collections::HashMap::new);
    map.insert(handle, slot);
}

/// Explicitly invalidate (drop) the process-local entry for `handle`
/// without consulting the SHM slot. Used by `shared_close_handle`
/// after it releases the SHM slot, so the next access reattaches
/// (which will then fail the generation check cleanly).
pub fn invalidate_process_local_slot(handle: i64) {
    let mut guard = PROCESS_LOCAL_SLOTS.lock().unwrap();
    if let Some(map) = guard.as_mut() {
        map.remove(&handle);  // Drop::drop runs on the entry
    }
}

/// Open the underlying file and mmap it (for read kinds), producing
/// a fresh `ProcessLocalSlot` for this pool. Reads `file_path` and
/// `kind` from the SHM slot via versioned-pointer reads (the caller
/// has already confirmed the slot is OPEN at this generation).
fn attach_process_local_slot(
    handle: i64,
    slot: &'static RegistrySlot,
    cached_generation: u64,
) -> Result<ProcessLocalSlot, MorlocError> {
    use std::sync::atomic::Ordering;

    // Versioned-pointer read of immutable-after-open fields. Read
    // `kind` and the path's RelPtr + length, then re-verify
    // generation. The caller will re-verify again after we return,
    // so a torn read manifests as a clean retry.
    let gen_before = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_before != cached_generation {
        return Err(MorlocError::Other(format!(
            "stream handle {:#x}: slot raced during attach (gen went from \
             {} to {})",
            handle, cached_generation, gen_before,
        )));
    }
    let kind = slot.kind;
    let path_rel = slot.file_path;
    let path_len = slot.file_path_len as usize;

    if path_rel == shm_types_crate::RELNULL || path_len == 0 {
        return Err(MorlocError::Other(format!(
            "stream handle {:#x}: slot has no file_path (corrupt slot \
             or partially-published @open)",
            handle,
        )));
    }
    let path_abs = crate::shm::rel2abs(path_rel)?;
    // SAFETY: path_abs + path_len are bounded by the SHM block that
    // backs the path string; verified by rel2abs above.
    let path_bytes = unsafe { std::slice::from_raw_parts(path_abs, path_len) };
    let path_str = std::str::from_utf8(path_bytes).map_err(|e| {
        MorlocError::Other(format!(
            "stream handle {:#x}: file_path is not valid UTF-8: {}",
            handle, e,
        ))
    })?.to_string();

    // Open + mmap depending on kind.
    let (fd, mmap_ptr, mmap_size) = match kind {
        x if x == MLC_KIND_IFILE || x == MLC_KIND_ISTREAM => {
            let (mp, sz) = mmap_file_readonly(&path_str)?;
            (-1i32, mp, sz)
        }
        x if x == MLC_KIND_OSTREAM => {
            // Non-opener pools open RDWR but DO NOT acquire flock
            // (the opener holds it for the slot's lifetime). The fd
            // exists only for `pwrite` of sub-packets.
            use std::ffi::CString;
            let c_path = CString::new(path_str.as_bytes()).map_err(|e| {
                MorlocError::Other(format!(
                    "OStream attach: path contains NUL: {}", e,
                ))
            })?;
            let fd = unsafe {
                libc::open(c_path.as_ptr(), libc::O_RDWR | libc::O_CLOEXEC, 0)
            };
            if fd < 0 {
                return Err(MorlocError::Io(std::io::Error::last_os_error()));
            }
            (fd, std::ptr::null_mut(), 0)
        }
        other => {
            return Err(MorlocError::Other(format!(
                "stream handle {:#x}: unknown kind byte {}", handle, other,
            )));
        }
    };

    // Cache the parsed schema. For non-OStream (which always reads
    // the schema from disk on open) we re-parse here.
    let schema_rel = slot.schema_str;
    let schema_len = slot.schema_str_len as usize;
    let schema_str = if schema_rel == shm_types_crate::RELNULL || schema_len == 0 {
        String::new()
    } else {
        let abs = crate::shm::rel2abs(schema_rel)?;
        let bytes = unsafe { std::slice::from_raw_parts(abs, schema_len) };
        std::str::from_utf8(bytes).map_err(|e| {
            MorlocError::Other(format!(
                "stream handle {:#x}: schema_str is not UTF-8: {}", handle, e,
            ))
        })?.to_string()
    };
    let parsed_schema = if schema_str.is_empty() {
        Schema::primitive(SerialType::Nil)
    } else {
        parse_schema(&schema_str).map_err(|e| {
            MorlocError::Other(format!(
                "stream handle {:#x}: unparseable schema '{}': {}",
                handle, schema_str, e,
            ))
        })?
    };
    // value_schema is the top-level type; elem_schema is the element
    // type for list-valued kinds (IFile/IStream/OStream all carry
    // their element schema in their stream header, which is what
    // schema_str holds). For OStream, the value sent over @write is a
    // `[a]` list whose element type matches.
    let (value_schema, elem_schema) = match kind {
        x if x == MLC_KIND_IFILE || x == MLC_KIND_ISTREAM => {
            (array_schema(&parsed_schema), parsed_schema.clone())
        }
        x if x == MLC_KIND_OSTREAM => {
            (array_schema(&parsed_schema), parsed_schema.clone())
        }
        _ => (parsed_schema.clone(), parsed_schema.clone()),
    };

    // Copy IFile sub-packet index from SHM into a process-local Vec.
    // This is a one-time cost at attach; subsequent index lookups
    // hit the local Vec without rel2abs.
    let subpacket_index_local = {
        let idx_rel = slot.subpacket_index;
        let idx_len = slot.subpacket_index_len as usize;
        if idx_rel == shm_types_crate::RELNULL || idx_len == 0 {
            Vec::new()
        } else {
            let abs = crate::shm::rel2abs(idx_rel)?;
            let raw = unsafe {
                std::slice::from_raw_parts(abs as *const u64, idx_len)
            };
            raw.to_vec()
        }
    };

    // Detect DATA_PACKET shape (single sub-packet at offset 0, no
    // stream header). Convention from `parse_stream_file`:
    // is_data_packet => body_start == 0 AND subpacket_index == [0].
    let is_data_packet = slot.body_start == 0
        && subpacket_index_local == [0];

    let cap_bytes = read_cache_cap_env();
    let cache = Box::new(StreamCache::new(cap_bytes));
    Ok(ProcessLocalSlot {
        cached_generation,
        mmap_ptr,
        mmap_size,
        fd,
        cache,
        value_schema,
        elem_schema,
        subpacket_index_local,
        subpacket_elem_cum: None,
        is_data_packet,
    })
}

// ── Slot allocation + lifecycle ──────────────────────────────────────────
//
// `allocate_slot_cas`: random-probe the slot array, CAS the `state`
// byte from FREE to OPEN_SHARED. Returns the slot index and a guard
// reference to fill in. Caller is responsible for writing the
// remaining fields (path, schema, kind, etc.) and Release-storing
// the new generation last.
//
// `release_slot_locked`: caller already holds the slot futex; we
// just zero `state` and bump `generation`. Used by both
// `shared_close_handle` (after finalisation) and the sweeper.

/// Pull 4 bytes of entropy as a slot-probe seed. We don't need
/// cryptographic randomness; the goal is just to spread allocations
/// across the slot space.
fn slot_probe_seed() -> usize {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static SEED: AtomicUsize = AtomicUsize::new(0);
    let prev = SEED.fetch_add(1, Ordering::Relaxed);
    if prev == 0 {
        // First call: mix in /dev/urandom so independent processes
        // start at different positions.
        use std::io::Read;
        let mut buf = [0u8; std::mem::size_of::<usize>()];
        if let Ok(mut f) = std::fs::File::open("/dev/urandom") {
            let _ = f.read_exact(&mut buf);
        }
        let seed = usize::from_le_bytes(buf);
        SEED.store(seed, Ordering::Relaxed);
        return seed;
    }
    prev
}

/// Allocate a free slot via random-probe CAS. On success returns the
/// (slot_idx, slot_ref) pair with `state` set to `OPEN_SHARED` but
/// `generation` NOT YET bumped. The caller must fill in the other
/// fields and Release-store the new generation last to complete
/// publication.
///
/// Returns `Err` if all slots are occupied.
pub(crate) fn allocate_slot_cas() -> Result<(usize, &'static RegistrySlot), MorlocError> {
    use std::sync::atomic::Ordering;
    // Lazily attach to the shared registry. Pool processes (py/r/cpp)
    // only call `shinit` on startup, not `stream_registry_init`; the
    // first stream FFI call from a pool would otherwise see "not
    // initialised". `registry_init` is idempotent and cheap on the
    // fast path (a single Acquire-load of REGISTRY_BASE).
    registry_init()?;
    let (slots_base, slot_count) = registry_slot_array();
    if slots_base.is_null() || slot_count == 0 {
        return Err(MorlocError::Other(
            "stream registry: not initialised (call registry_init first)".into(),
        ));
    }
    let start = slot_probe_seed() % slot_count;
    for off in 0..slot_count {
        let idx = (start + off) % slot_count;
        // SAFETY: idx < slot_count and slots_base points to a valid
        // region of slot_count * STREAM_ENTRY_SIZE bytes.
        let slot = unsafe {
            &*(slots_base.add(idx * STREAM_ENTRY_SIZE) as *const RegistrySlot)
        };
        if slot.state
            .compare_exchange(
                SLOT_STATE_FREE, SLOT_STATE_OPEN_SHARED,
                Ordering::AcqRel, Ordering::Relaxed,
            )
            .is_ok()
        {
            return Ok((idx, slot));
        }
    }
    Err(MorlocError::Other(format!(
        "stream registry: all {} slots are in use; raise \
         MORLOC_REGISTRY_SLOT_COUNT (current cap {}) or close more handles",
        slot_count, STREAM_REGISTRY_MAX_SLOT_COUNT,
    )))
}

/// Free a slot. Caller must hold the slot futex; the slot's state
/// transitions to FREE and the generation bumps by the salted-random
/// increment. After this call, any handle that referenced this slot
/// fails the generation check.
///
/// Also frees the SHM-resident strings (path, schema) and
/// subpacket_index that the slot referenced. The caller must already
/// have done any kind-specific finalisation (e.g. write final footer
/// for OStream).
fn release_slot_locked(slot: &RegistrySlot) {
    use std::sync::atomic::Ordering;

    // Free path / schema / subpacket_index / write_buffer SHM blocks.
    // Best-effort: a leaked block here is bounded by the registry's
    // lifetime (cleaned at nexus shclose), and erroring would obscure
    // the primary `state = FREE` transition.
    let path = slot.file_path;
    if path != shm_types_crate::RELNULL {
        if let Ok(abs) = crate::shm::rel2abs(path) {
            let _ = crate::shm::shfree(abs);
        }
    }
    let schema = slot.schema_str;
    if schema != shm_types_crate::RELNULL {
        if let Ok(abs) = crate::shm::rel2abs(schema) {
            let _ = crate::shm::shfree(abs);
        }
    }
    let idx = slot.subpacket_index;
    if idx != shm_types_crate::RELNULL {
        if let Ok(abs) = crate::shm::rel2abs(idx) {
            let _ = crate::shm::shfree(abs);
        }
    }
    let wbuf = slot.write_buffer;
    if wbuf != shm_types_crate::RELNULL {
        if let Ok(abs) = crate::shm::rel2abs(wbuf) {
            let _ = crate::shm::shfree(abs);
        }
    }

    // Zero out the RelPtr fields so a future allocator sees a clean
    // slot. The `state` and `generation` writes below close the
    // publication window.
    //
    // SAFETY: we hold the slot futex AND `state` is about to become
    // FREE (so no other thread is reading via the versioned-pointer
    // pattern -- they'd fail the state check). The plain stores are
    // visible to future allocators by happens-before via the Release
    // store of `state` below.
    unsafe {
        let mp = slot as *const RegistrySlot as *mut RegistrySlot;
        (*mp).file_path = shm_types_crate::RELNULL;
        (*mp).file_path_len = 0;
        (*mp).schema_str = shm_types_crate::RELNULL;
        (*mp).schema_str_len = 0;
        (*mp).subpacket_index = shm_types_crate::RELNULL;
        (*mp).subpacket_index_len = 0;
        (*mp).subpacket_index_cap = 0;
        (*mp).cursor = 0;
        (*mp).element_count = 0;
        (*mp).final_footer = 0;
        (*mp).compression_level = 0;
        (*mp).body_start = 0;
        (*mp).opener_pid = 0;
        (*mp).opener_pid_start_time = 0;
        (*mp).kind = 0;
        (*mp).write_buffer = shm_types_crate::RELNULL;
        (*mp).write_buffer_index_cap = 0;
        (*mp).write_buffer_index_count = 0;
        (*mp).write_buffer_data_used = 0;
    }

    // Bump generation by the salted random increment. Use fetch_add
    // so concurrent attaches that snapshot the old generation still
    // detect the change.
    let bump = registry_gen_salt() | 1;
    slot.generation.fetch_add(bump, Ordering::AcqRel);

    // Reset call_id to the no-sweep sentinel (which is also the
    // logical "free slot" value -- the sweeper skips it anyway).
    slot.call_id.store(CALL_ID_NO_SWEEP, Ordering::Release);

    // Finally: release the slot. State = FREE is the publication
    // gate that allows other allocators' CAS to succeed.
    slot.state.store(SLOT_STATE_FREE, Ordering::Release);
}

/// Read this process's start time from `/proc/self/stat` (field 22,
/// clock ticks since boot). Used to populate the slot's
/// `opener_pid_start_time` so the §1.7 PID sweep can distinguish a
/// reused PID from the original opener. Falls back to 0 if the read
/// fails (most likely on non-Linux); the PID sweep still works
/// without it, just with a small false-negative window.
fn read_pid_start_time() -> u64 {
    read_pid_start_time_for(std::process::id())
}

/// Read the start time of an arbitrary PID from `/proc/PID/stat`
/// (field 22, clock ticks since boot). Returns 0 if the process no
/// longer exists or the read otherwise fails.
///
/// Used by the nexus to capture pool start times at spawn time so
/// the PID sweep can match the right process even across PID reuse.
pub fn read_pid_start_time_for(pid: u32) -> u64 {
    use std::io::Read;
    let path = format!("/proc/{}/stat", pid);
    let mut f = match std::fs::File::open(&path) {
        Ok(f) => f,
        Err(_) => return 0,
    };
    let mut buf = String::with_capacity(512);
    if f.read_to_string(&mut buf).is_err() {
        return 0;
    }
    let close_paren = match buf.rfind(')') {
        Some(p) => p,
        None => return 0,
    };
    let rest = buf[close_paren + 1..].trim();
    let fields: Vec<&str> = rest.split_whitespace().collect();
    fields.get(19).and_then(|s| s.parse::<u64>().ok()).unwrap_or(0)
}

/// Pull the current dispatch's `call_id` from thread-local storage.
/// Returns `CALL_ID_NO_SWEEP` (= 0) if no call is active, which
/// means the slot will never be swept by the per-call_id sweeper
/// (a deliberate no-op for handles opened outside a dispatch, e.g.
/// from unit tests).
fn current_call_id() -> u64 {
    CURRENT_CALL_ID.with(|c| c.get())
}

thread_local! {
    /// Per-thread current `call_id`. Set by the daemon dispatch loop
    /// at the start of a request; read by `@open` to tag freshly-allocated
    /// slots for the sweeper.
    static CURRENT_CALL_ID: std::cell::Cell<u64> = const { std::cell::Cell::new(CALL_ID_NO_SWEEP) };
}

/// Set the current thread's `call_id`. Called by the daemon dispatch
/// loop before invoking `morloc_eval`. Returns the previous value
/// so callers can restore on dispatch completion (which the dispatch
/// loop does explicitly so a panic during `morloc_eval` doesn't
/// leave the TLS in an inconsistent state).
pub fn set_current_call_id(new: u64) -> u64 {
    CURRENT_CALL_ID.with(|c| {
        let old = c.get();
        c.set(new);
        old
    })
}

/// Allocate an SHM-resident copy of `bytes` and return its RelPtr.
/// Used to publish path / schema strings into the slot's RelPtr
/// fields. The caller frees via `shfree(rel2abs(rel))` at slot close.
fn shm_copy_bytes(bytes: &[u8]) -> Result<RelPtr, MorlocError> {
    let abs = crate::shm::shmemcpy(bytes.as_ptr(), bytes.len())?;
    crate::shm::abs2rel(abs)
}

/// Allocate an SHM-resident copy of a `[u64]` array (used for the
/// IFile sub-packet index). Returns the RelPtr.
fn shm_copy_u64_slice(slice: &[u64]) -> Result<RelPtr, MorlocError> {
    let bytes = unsafe {
        std::slice::from_raw_parts(
            slice.as_ptr() as *const u8,
            slice.len() * std::mem::size_of::<u64>(),
        )
    };
    shm_copy_bytes(bytes)
}

/// Open a file as `IFile` against the shared SHM registry. Allocates
/// a slot, mmaps the file, parses its stream metadata, publishes the
/// path + schema + subpacket_index into SHM, and returns the handle.
///
/// Rejects STREAM files lacking a final footer (the 2026-06-28
/// design contract: random access requires the full index, which
/// only a clean close writes).
pub fn shared_open_ifile(path: &str) -> Result<i64, MorlocError> {
    use std::sync::atomic::Ordering;

    // mmap + parse the file BEFORE we touch the registry, so a bad
    // file doesn't leave a half-initialised slot.
    let (mmap_ptr, mmap_size) = mmap_file_readonly(path)?;
    let parsed = match parse_stream_file(path, mmap_ptr, mmap_size) {
        Ok(p) => p,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };
    // Enforce the IFile-on-clean-footer contract (matches the
    // process-local `open_file_as` check for IFile).
    if !parsed.is_data_packet && !parsed.final_footer {
        unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
        return Err(MorlocError::Other(format!(
            "@open IFile '{}': file is not cleanly closed (no final footer). \
             Open it as IStream to drain forward, or repair it with morloc-nexus.",
            path,
        )));
    }

    // Allocate a slot.
    let (slot_idx, slot) = match allocate_slot_cas() {
        Ok(s) => s,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };
    let _guard = SlotFutexGuard::lock(slot);

    // Publish path + schema + subpacket_index. On any error, release
    // the slot via `release_slot_locked` (which itself shfree's any
    // partials we may have already published).
    let publish_result = (|| -> Result<u64, MorlocError> {
        let path_rel = shm_copy_bytes(path.as_bytes())?;
        let schema_rel = shm_copy_bytes(parsed.schema_str.as_bytes())?;
        let idx_rel = if !parsed.subpacket_index.is_empty() {
            shm_copy_u64_slice(&parsed.subpacket_index)?
        } else {
            shm_types_crate::RELNULL
        };
        // SAFETY: we hold the slot's futex; state is OPEN_SHARED but
        // generation has NOT been bumped to the new value yet, so no
        // cross-pool reader can observe these field writes (the
        // versioned-pointer pattern gates on the new generation).
        unsafe {
            let mp = slot as *const RegistrySlot as *mut RegistrySlot;
            (*mp).kind = MLC_KIND_IFILE;
            (*mp).file_path = path_rel;
            (*mp).file_path_len = path.len() as u32;
            (*mp).schema_str = schema_rel;
            (*mp).schema_str_len = parsed.schema_str.len() as u32;
            (*mp).subpacket_index = idx_rel;
            (*mp).subpacket_index_len = parsed.subpacket_index.len() as u64;
            // IFile's sub-packet index is immutable -- set once from
            // the parsed final footer and never grown. cap = 0 marks
            // "not OStream-growable" so release_slot_locked treats
            // subpacket_index_len, not _cap, as the freed extent.
            (*mp).subpacket_index_cap = 0;
            (*mp).body_start = parsed.body_start;
            (*mp).final_footer = if parsed.final_footer { 1 } else { 0 };
            (*mp).cursor = 0;
            (*mp).element_count = parsed.element_count;
            (*mp).compression_level = 0;
            (*mp).opener_pid = std::process::id();
            (*mp).opener_pid_start_time = read_pid_start_time();
            // IFile/IStream don't write; clear buffer fields so
            // release_slot_locked doesn't attempt an spurious shfree
            // on a freshly-allocated never-released slot whose
            // zero-init bytes look like a real volume-0 relptr.
            (*mp).write_buffer = shm_types_crate::RELNULL;
            (*mp).write_buffer_index_cap = 0;
            (*mp).write_buffer_index_count = 0;
            (*mp).write_buffer_data_used = 0;
            if let Some(d) = parsed.diag.as_ref() {
                (*mp).diag = *d;
            }
        }
        slot.call_id.store(current_call_id(), Ordering::Release);
        // Bump generation by the salted random increment. This is
        // the publication store: cross-pool readers Acquire-load
        // this and observe all prior writes happens-before.
        let bump = registry_gen_salt() | 1;
        let new_gen = (slot.generation.fetch_add(bump, Ordering::AcqRel) + bump) & GENERATION_MASK;
        Ok(new_gen)
    })();
    let new_gen = match publish_result {
        Ok(g) => g,
        Err(e) => {
            // Roll back the slot. release_slot_locked shfree's any
            // RelPtrs we've published; the futex guard releases on
            // drop.
            release_slot_locked(slot);
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };

    // Install the process-local slot with the mmap we already have.
    let cap_bytes = read_cache_cap_env();
    let local = ProcessLocalSlot {
        cached_generation: new_gen,
        mmap_ptr,
        mmap_size,
        fd: -1,
        cache: Box::new(StreamCache::new(cap_bytes)),
        value_schema: parsed.value_schema.clone(),
        elem_schema: parsed.elem_schema.clone(),
        subpacket_index_local: parsed.subpacket_index.clone(),
        subpacket_elem_cum: None,
        is_data_packet: parsed.is_data_packet,
    };
    let handle = pack_handle(new_gen, slot_idx);
    install_process_local_slot(handle, local);
    Ok(handle)
}

/// Open a file as `IStream` against the shared SHM registry. Unlike
/// IFile, IStream walks forward by byte cursor and works on
/// temp-footer files (writer in progress).
pub fn shared_open_istream(path: &str) -> Result<i64, MorlocError> {
    use std::sync::atomic::Ordering;

    let (mmap_ptr, mmap_size) = mmap_file_readonly(path)?;
    let parsed = match parse_stream_file(path, mmap_ptr, mmap_size) {
        Ok(p) => p,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };
    // Forward-only access: kernel readahead is the right hint.
    unsafe {
        libc::madvise(
            mmap_ptr as *mut libc::c_void,
            mmap_size as usize,
            libc::MADV_SEQUENTIAL,
        );
    }

    let (slot_idx, slot) = match allocate_slot_cas() {
        Ok(s) => s,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };
    let _guard = SlotFutexGuard::lock(slot);

    let publish_result = (|| -> Result<u64, MorlocError> {
        let path_rel = shm_copy_bytes(path.as_bytes())?;
        let schema_rel = shm_copy_bytes(parsed.schema_str.as_bytes())?;
        unsafe {
            let mp = slot as *const RegistrySlot as *mut RegistrySlot;
            (*mp).kind = MLC_KIND_ISTREAM;
            (*mp).file_path = path_rel;
            (*mp).file_path_len = path.len() as u32;
            (*mp).schema_str = schema_rel;
            (*mp).schema_str_len = parsed.schema_str.len() as u32;
            (*mp).subpacket_index = shm_types_crate::RELNULL;
            (*mp).subpacket_index_len = 0;
            (*mp).subpacket_index_cap = 0;
            (*mp).body_start = parsed.body_start;
            (*mp).final_footer = if parsed.final_footer { 1 } else { 0 };
            // IStream walks by cursor starting at body_start.
            (*mp).cursor = parsed.body_start;
            (*mp).element_count = parsed.element_count;
            (*mp).compression_level = 0;
            (*mp).opener_pid = std::process::id();
            (*mp).opener_pid_start_time = read_pid_start_time();
            (*mp).write_buffer = shm_types_crate::RELNULL;
            (*mp).write_buffer_index_cap = 0;
            (*mp).write_buffer_index_count = 0;
            (*mp).write_buffer_data_used = 0;
            if let Some(d) = parsed.diag.as_ref() {
                (*mp).diag = *d;
            }
        }
        slot.call_id.store(current_call_id(), Ordering::Release);
        let bump = registry_gen_salt() | 1;
        let new_gen = (slot.generation.fetch_add(bump, Ordering::AcqRel) + bump) & GENERATION_MASK;
        Ok(new_gen)
    })();
    let new_gen = match publish_result {
        Ok(g) => g,
        Err(e) => {
            release_slot_locked(slot);
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };

    let cap_bytes = read_cache_cap_env();
    let local = ProcessLocalSlot {
        cached_generation: new_gen,
        mmap_ptr,
        mmap_size,
        fd: -1,
        cache: Box::new(StreamCache::new(cap_bytes)),
        value_schema: parsed.value_schema.clone(),
        elem_schema: parsed.elem_schema.clone(),
        subpacket_index_local: parsed.subpacket_index.clone(),
        subpacket_elem_cum: None,
        is_data_packet: parsed.is_data_packet,
    };
    let handle = pack_handle(new_gen, slot_idx);
    install_process_local_slot(handle, local);
    Ok(handle)
}

/// Open a file as `OStream` with the element schema known up front.
/// Creates the file (`O_CREAT | O_EXCL`), acquires an exclusive
/// flock, writes the stream header, and allocates a slot.
///
/// Rejects a second `@open` of the same path with EEXIST: the
/// 2026-06-28 design contract requires explicit handle-passing
/// across pools for multi-writer sharing, not transparent
/// path-based dedup.
pub fn shared_open_ostream_with_schema(
    path: &str,
    schema_str: &str,
) -> Result<i64, MorlocError> {
    use std::ffi::CString;
    use std::sync::atomic::Ordering;
    use morloc_runtime_types::schema::SerialType;
    use morloc_runtime_types::packet::make_stream_header_block;

    let c_path = CString::new(path.as_bytes()).map_err(|e| {
        MorlocError::Other(format!("OStream open: path contains NUL: {}", e))
    })?;
    let fd = unsafe {
        libc::open(
            c_path.as_ptr(),
            libc::O_RDWR | libc::O_CREAT | libc::O_EXCL | libc::O_CLOEXEC,
            0o644,
        )
    };
    if fd < 0 {
        let e = std::io::Error::last_os_error();
        // EEXIST is the multi-writer case. We can't tell whether
        // it's an in-nexus existing slot (which the planfile says
        // should error with the existing handle in the message) or
        // an external writer; we surface the OS error and let the
        // caller's diagnostic carry the path.
        return Err(MorlocError::Io(e));
    }
    let lock_rc = unsafe { libc::flock(fd, libc::LOCK_EX | libc::LOCK_NB) };
    if lock_rc != 0 {
        let e = std::io::Error::last_os_error();
        unsafe { libc::close(fd); }
        return Err(MorlocError::Other(format!(
            "@open OStream '{}': could not acquire exclusive flock: {}",
            path, e,
        )));
    }

    // Parse the schema (we need it to write the stream header). An
    // empty schema string is accepted as a placeholder, falling
    // back to Nil; the bridge always supplies a real schema.
    let parsed_schema = if schema_str.is_empty() {
        morloc_runtime_types::schema::Schema::primitive(SerialType::Nil)
    } else {
        match parse_schema(schema_str) {
            Ok(s) => s,
            Err(e) => {
                unsafe { libc::close(fd); libc::unlink(c_path.as_ptr()); }
                return Err(MorlocError::Schema(format!(
                    "OStream open: unparseable schema '{}': {}", schema_str, e,
                )));
            }
        }
    };
    let header_bytes = make_stream_header_block(&parsed_schema);
    if let Err(e) = write_all_fd(fd, &header_bytes) {
        unsafe { libc::close(fd); libc::unlink(c_path.as_ptr()); }
        return Err(e);
    }
    let body_start = header_bytes.len() as u64;

    let (slot_idx, slot) = match allocate_slot_cas() {
        Ok(s) => s,
        Err(e) => {
            unsafe { libc::close(fd); libc::unlink(c_path.as_ptr()); }
            return Err(e);
        }
    };
    let _guard = SlotFutexGuard::lock(slot);

    let publish_result = (|| -> Result<u64, MorlocError> {
        let path_rel = shm_copy_bytes(path.as_bytes())?;
        let schema_rel = shm_copy_bytes(schema_str.as_bytes())?;
        // Allocate the write buffer in SHM. shcalloc zero-fills, which
        // matches `_tail_pad`'s implicit zero start (no leaked bytes
        // from a previous slot use). Sized from MORLOC_WRITE_BUFFER_BYTES.
        let buf_bytes = read_write_buffer_bytes_env();
        let buf_abs = crate::shm::shcalloc(1, buf_bytes)?;
        let buf_rel = crate::shm::abs2rel(buf_abs)?;
        // SHM-resident sub-packet index, shared across all writer pools
        // so the final footer at @close reflects every flush regardless
        // of which pool emitted it. Initial cap is small and grows
        // exponentially under the slot futex in emit_subpacket_to_disk.
        let idx_cap_initial: u64 = OSTREAM_SUBPACKET_INDEX_INITIAL_CAP;
        let idx_buf_bytes = (idx_cap_initial as usize) * std::mem::size_of::<u64>();
        let idx_buf_abs = crate::shm::shcalloc(1, idx_buf_bytes)?;
        let idx_buf_rel = crate::shm::abs2rel(idx_buf_abs)?;
        unsafe {
            let mp = slot as *const RegistrySlot as *mut RegistrySlot;
            (*mp).kind = MLC_KIND_OSTREAM;
            (*mp).file_path = path_rel;
            (*mp).file_path_len = path.len() as u32;
            (*mp).schema_str = schema_rel;
            (*mp).schema_str_len = schema_str.len() as u32;
            (*mp).subpacket_index = idx_buf_rel;
            (*mp).subpacket_index_len = 0;
            (*mp).subpacket_index_cap = idx_cap_initial;
            (*mp).body_start = body_start;
            (*mp).final_footer = 0;
            (*mp).cursor = body_start;
            (*mp).element_count = 0;
            (*mp).compression_level = 0;
            (*mp).opener_pid = std::process::id();
            (*mp).opener_pid_start_time = read_pid_start_time();
            (*mp).diag = StreamDiag::new();
            // Write buffer fields. index_cap is set lazily on first
            // @write -- elem_width isn't known until then since the
            // schema-string parse happens below.
            (*mp).write_buffer = buf_rel;
            (*mp).write_buffer_index_cap = 0;
            (*mp).write_buffer_index_count = 0;
            (*mp).write_buffer_data_used = 0;
        }
        slot.call_id.store(current_call_id(), Ordering::Release);
        let bump = registry_gen_salt() | 1;
        let new_gen = (slot.generation.fetch_add(bump, Ordering::AcqRel) + bump) & GENERATION_MASK;
        Ok(new_gen)
    })();
    let new_gen = match publish_result {
        Ok(g) => g,
        Err(e) => {
            release_slot_locked(slot);
            unsafe { libc::close(fd); libc::unlink(c_path.as_ptr()); }
            return Err(e);
        }
    };

    // OStream: parse the schema once for caching. `array_schema`
    // wraps it as `[a]` so @write payloads of type `[a]` match.
    let elem_schema_parsed = if schema_str.is_empty() {
        morloc_runtime_types::schema::Schema::primitive(
            morloc_runtime_types::schema::SerialType::Nil,
        )
    } else {
        parse_schema(schema_str).unwrap_or_else(|_| {
            morloc_runtime_types::schema::Schema::primitive(
                morloc_runtime_types::schema::SerialType::Nil,
            )
        })
    };
    let local = ProcessLocalSlot {
        cached_generation: new_gen,
        mmap_ptr: std::ptr::null_mut(),
        mmap_size: 0,
        fd,                       // opener holds flock for slot lifetime
        cache: Box::new(StreamCache::new(0)),
        value_schema: array_schema(&elem_schema_parsed),
        elem_schema: elem_schema_parsed,
        subpacket_index_local: Vec::new(),
        subpacket_elem_cum: None,
        is_data_packet: false,
    };
    let handle = pack_handle(new_gen, slot_idx);
    install_process_local_slot(handle, local);
    Ok(handle)
}

/// Explicit `@close`: writes final footer + fdatasync for OStream,
/// then releases the slot. Caller-visible failures (pwrite, fsync)
/// propagate to the user; the slot stays OPEN on error so a retry
/// can succeed.
pub fn shared_close_handle(handle: i64) -> Result<(), MorlocError> {
    use std::sync::atomic::Ordering;

    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "shared_close_handle: slot index {} out of range", slot_idx,
    )))?;

    // Snapshot the kind under a versioned read; if mismatch, abort.
    let gen_pre = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_pre != gen_claim {
        return Err(MorlocError::Other(format!(
            "shared_close_handle: generation mismatch (claim {}, slot {})",
            gen_claim, gen_pre,
        )));
    }
    if slot.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED {
        return Err(MorlocError::Other(
            "shared_close_handle: slot is not OPEN".into(),
        ));
    }
    let kind = slot.kind;

    if kind == MLC_KIND_OSTREAM {
        // OStream close uses with_process_local_slot so the buffer
        // flush (which appends to local.subpacket_index_local) and
        // the subsequent final-footer write share one futex-held
        // critical section. The flush + footer write together can
        // be sizable (one buffered sub-packet + a final footer)
        // but cross-pool contention during close is rare; the
        // shared-buffer story trumps minimising hold time here.
        with_process_local_slot(handle, |local, slot| {
            let _guard = SlotFutexGuard::lock(slot);
            let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
            if gen_now != gen_claim {
                return Err(MorlocError::Other(
                    "shared_close_handle: slot generation changed under us".into(),
                ));
            }
            // Flush any buffered elements as a final sub-packet so
            // they're durably on disk before the footer.
            flush_write_buffer(slot, local)?;
            // Build + write the final footer from the SHM-resident
            // shared sub-packet index. Every flush across every pool
            // that has written through this slot appended to it under
            // the slot futex (see append_shared_subpacket_index), so
            // the resulting final footer's random-access index covers
            // the file completely -- subsequent `@open IFile` works
            // regardless of which pool wrote which sub-packet.
            let cursor = slot.cursor;
            let diag = slot.diag;
            let shared_index = read_shared_subpacket_index(slot)?;
            let footer = morloc_runtime_types::packet::make_final_footer_packet(
                &diag, &shared_index,
            );
            pwrite_all_fd(local.fd, &footer, cursor)?;
            let rc = unsafe { libc::fdatasync(local.fd) };
            if rc != 0 {
                return Err(MorlocError::Io(std::io::Error::last_os_error()));
            }
            release_slot_locked(slot);
            drop(_guard);
            Ok(())
        })?;
    } else {
        // IFile / IStream: no finalisation; just release.
        let _guard = SlotFutexGuard::lock(slot);
        let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
        if gen_now != gen_claim {
            return Err(MorlocError::Other(
                "shared_close_handle: slot generation changed under us".into(),
            ));
        }
        release_slot_locked(slot);
        drop(_guard);
    }

    invalidate_process_local_slot(handle);
    Ok(())
}

/// Sweep-path close: releases the slot WITHOUT writing a final footer.
/// Used by the per-call_id sweeper and the PID-based pool-crash sweep.
/// Leaves the on-disk OStream in temp-footer state (the honest "writer
/// didn't finish" signal — a downstream reader sees a temp footer iff the
/// writer never reached an explicit close).
pub fn shared_discard_handle(handle: i64) -> Result<(), MorlocError> {
    use std::sync::atomic::Ordering;

    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "shared_discard_handle: slot index {} out of range", slot_idx,
    )))?;
    let _guard = SlotFutexGuard::lock(slot);
    let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_now != gen_claim {
        return Err(MorlocError::Other(format!(
            "shared_discard_handle: generation mismatch (handle {}, slot {})",
            gen_claim, gen_now,
        )));
    }
    release_slot_locked(slot);
    drop(_guard);
    invalidate_process_local_slot(handle);
    Ok(())
}

/// Same as `shared_discard_handle` but assumes the caller already
/// holds the slot's futex. Used by the per-call_id sweeper, which
/// takes the futex to re-check `state` + `call_id` and then
/// proceeds to the discard while still holding it.
pub fn shared_discard_handle_locked(
    slot: &RegistrySlot,
    slot_idx: usize,
) -> Result<(), MorlocError> {
    use std::sync::atomic::Ordering;
    let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    let handle = pack_handle(gen_now, slot_idx);
    release_slot_locked(slot);
    invalidate_process_local_slot(handle);
    Ok(())
}

// Shared op functions run against the SHM slot + process-local mmap cache.
// They coordinate cursor + sub-packet-index updates via the slot futex so
// concurrent pools writing to or reading from the same handle stay
// consistent.

/// Emit one sub-packet from a payload byte slice, write it at the slot's
/// current cursor, and update slot bookkeeping (cursor, diag, temp footer,
/// shared sub-packet index). Caller MUST hold the slot futex.
///
/// `payload_bytes` is the uncompressed `[a]` voidstar payload (Array
/// header + inline + variable). If `level > 0` it gets zstd-compressed
/// here and the FRAME_INDEX metadata entry is added.
fn emit_subpacket_to_disk(
    slot: &RegistrySlot,
    local: &mut ProcessLocalSlot,
    payload_bytes: &[u8],
    level: u8,
) -> Result<(), MorlocError> {
    use morloc_runtime_types::packet::{
        PacketHeader, METADATA_TYPE_SCHEMA_STRING, METADATA_TYPE_FRAME_INDEX,
        METADATA_BLOCK_ALIGNMENT, PACKET_COMPRESSION_NONE, PACKET_COMPRESSION_ZSTD,
        make_temp_footer_packet,
    };
    use morloc_runtime_types::schema::schema_to_string;

    let clvl = crate::compression::CompressionLevel::from_u8(level)?;
    let (final_payload, compression_byte, frame_index_body): (Vec<u8>, u8, Option<Vec<u8>>) =
        if clvl.is_none() {
            (payload_bytes.to_vec(), PACKET_COMPRESSION_NONE, None)
        } else {
            let (bytes, frames) =
                crate::compression::compress_payload_zstd(payload_bytes, clvl)?;
            let body = crate::packet::encode_frame_index_entry(&frames);
            (bytes, PACKET_COMPRESSION_ZSTD, Some(body))
        };
    let value_schema_str = schema_to_string(&local.value_schema);
    let mut schema_body = value_schema_str.into_bytes();
    schema_body.push(0);
    let base_meta = crate::packet::append_metadata_entry(
        &[], METADATA_TYPE_SCHEMA_STRING, &schema_body,
    );
    let meta_unpadded = match &frame_index_body {
        Some(body) => crate::packet::append_metadata_entry(
            &base_meta, METADATA_TYPE_FRAME_INDEX, body,
        ),
        None => base_meta,
    };
    let padded_meta_len = meta_unpadded.len()
        .div_ceil(METADATA_BLOCK_ALIGNMENT)
        * METADATA_BLOCK_ALIGNMENT;
    let mut meta = meta_unpadded;
    meta.resize(padded_meta_len, 0);
    let mut hdr = PacketHeader::data_rptr(
        morloc_runtime_types::packet::PACKET_FORMAT_VOIDSTAR,
        final_payload.len() as u64,
    );
    hdr.offset = padded_meta_len as u32;
    let mut hdr_bytes = hdr.to_bytes();
    hdr_bytes[15] = compression_byte;

    let mut packet = Vec::with_capacity(
        hdr_bytes.len() + meta.len() + final_payload.len(),
    );
    packet.extend_from_slice(&hdr_bytes);
    packet.extend_from_slice(&meta);
    packet.extend_from_slice(&final_payload);

    let cursor = slot.cursor;
    pwrite_all_fd(local.fd, &packet, cursor)?;
    let subpacket_end = cursor + packet.len() as u64;

    unsafe {
        let mp = slot as *const RegistrySlot as *mut RegistrySlot;
        (*mp).cursor = subpacket_end;
        let d = &mut (*mp).diag;
        d.subpacket_count += 1;
        d.bytes_compressed_total += final_payload.len() as u64;
        let now_us = unix_micros_now();
        if d.first_flush_time == 0 { d.first_flush_time = now_us; }
        d.last_flush_time = now_us;
        push_tail_window(d, cursor);
    }
    // Publish the sub-packet boundary into the SHM-resident shared
    // index. Caller (shared_write_subpacket / shared_flush_buffer /
    // shared_close_handle) already holds the slot futex, so the
    // append is serialised across every writer pool. The opener's
    // @close then reads this single index to build the final footer
    // -- every pool's flushes appear regardless of who emitted them.
    append_shared_subpacket_index(slot, cursor)?;

    let footer = make_temp_footer_packet(&slot.diag);
    pwrite_all_fd(local.fd, &footer, subpacket_end)?;
    Ok(())
}

/// Snapshot the SHM-resident shared sub-packet index into a heap
/// `Vec<u64>` suitable for `make_final_footer_packet`. Caller MUST
/// hold the slot futex so the index can't grow underneath us. Returns
/// an empty vec when the slot has no index allocated (IFile/IStream
/// slots, or an OStream with no flushes yet).
fn read_shared_subpacket_index(
    slot: &RegistrySlot,
) -> Result<Vec<u64>, MorlocError> {
    let len = slot.subpacket_index_len as usize;
    if len == 0 {
        return Ok(Vec::new());
    }
    let idx_rel = slot.subpacket_index;
    if idx_rel == shm_types_crate::RELNULL {
        return Ok(Vec::new());
    }
    let idx_abs = crate::shm::rel2abs(idx_rel)? as *const u64;
    let mut out = Vec::with_capacity(len);
    unsafe {
        out.set_len(len);
        std::ptr::copy_nonoverlapping(idx_abs, out.as_mut_ptr(), len);
    }
    Ok(out)
}

/// Append a sub-packet starting offset to the SHM-resident shared
/// sub-packet index. Caller MUST hold the slot futex. Doubles the
/// capacity (and reallocates the SHM block) when full. The relptr in
/// `slot.subpacket_index` is updated to the new block before the old
/// one is freed -- readers under the same futex see a single
/// publication step.
fn append_shared_subpacket_index(
    slot: &RegistrySlot,
    offset: u64,
) -> Result<(), MorlocError> {
    let cap = slot.subpacket_index_cap;
    if cap == 0 {
        // OStream slots seed this to >= OSTREAM_SUBPACKET_INDEX_INITIAL_CAP
        // at open. A zero cap here means this slot isn't OStream-shaped
        // (IFile/IStream readers don't emit sub-packets); the emit path
        // shouldn't be called against them.
        return Err(MorlocError::Other(
            "append_shared_subpacket_index: slot has no growable index \
             (kind is not OStream or open path forgot to allocate)".into(),
        ));
    }
    let len = slot.subpacket_index_len;
    let idx_rel = slot.subpacket_index;
    let idx_abs = crate::shm::rel2abs(idx_rel)? as *mut u64;
    if len < cap {
        unsafe { *idx_abs.add(len as usize) = offset; }
        unsafe {
            let mp = slot as *const RegistrySlot as *mut RegistrySlot;
            (*mp).subpacket_index_len = len + 1;
        }
        return Ok(());
    }
    // Grow by doubling.
    let new_cap = cap.checked_mul(2).ok_or_else(|| MorlocError::Other(
        "append_shared_subpacket_index: capacity overflow".into(),
    ))?;
    let new_bytes = (new_cap as usize) * std::mem::size_of::<u64>();
    let new_abs = crate::shm::shcalloc(1, new_bytes)? as *mut u64;
    unsafe {
        std::ptr::copy_nonoverlapping(idx_abs, new_abs, len as usize);
        *new_abs.add(len as usize) = offset;
    }
    let new_rel = crate::shm::abs2rel(new_abs as *mut u8)?;
    unsafe {
        let mp = slot as *const RegistrySlot as *mut RegistrySlot;
        (*mp).subpacket_index = new_rel;
        (*mp).subpacket_index_len = len + 1;
        (*mp).subpacket_index_cap = new_cap;
    }
    // Free the old block AFTER the publication. Readers under the
    // futex see the new relptr; no one is holding a pointer to the
    // old block.
    let _ = crate::shm::shfree(idx_abs as *mut u8);
    Ok(())
}

/// Double the write buffer's index-section capacity. Caller MUST hold
/// the slot futex and have already verified that the new capacity
/// (plus current data_used) still fits in the buffer. Memmoves the
/// data region right by `(new_cap - old_cap) * elem_width` and
/// shifts every inline element's relptrs by the same amount.
fn grow_index_capacity(
    slot: &RegistrySlot,
    local: &ProcessLocalSlot,
    new_cap: u64,
) -> Result<(), MorlocError> {
    let w = local.elem_schema.width;
    let old_cap = slot.write_buffer_index_cap;
    let data_used = slot.write_buffer_data_used as usize;
    let n = slot.write_buffer_index_count;

    let old_data_offset = 16 + (old_cap as usize) * w;
    let new_data_offset = 16 + (new_cap as usize) * w;
    let shift: isize = (new_data_offset - old_data_offset) as isize;

    let buf_abs = crate::shm::rel2abs(slot.write_buffer)?;
    if data_used > 0 {
        unsafe {
            std::ptr::copy(
                buf_abs.add(old_data_offset),
                buf_abs.add(new_data_offset),
                data_used,
            );
        }
        for i in 0..n {
            let elem_ptr = unsafe { buf_abs.add(16 + (i as usize) * w) };
            crate::voidstar::adjust_relptrs(elem_ptr, &local.elem_schema, shift)?;
        }
    }
    unsafe {
        let mp = slot as *const RegistrySlot as *mut RegistrySlot;
        (*mp).write_buffer_index_cap = new_cap;
    }
    Ok(())
}

/// Compact the write buffer (remove wasted index space) and emit its
/// contents as one sub-packet at the slot's cursor. Caller MUST hold
/// the slot futex. No-op when the buffer is empty. Resets buffer
/// counters after a successful flush.
fn flush_write_buffer(
    slot: &RegistrySlot,
    local: &mut ProcessLocalSlot,
) -> Result<(), MorlocError> {
    let n = slot.write_buffer_index_count;
    if n == 0 {
        return Ok(());
    }
    let w = local.elem_schema.width;
    let index_cap = slot.write_buffer_index_cap as usize;
    let data_used = slot.write_buffer_data_used as usize;

    let buf_abs = crate::shm::rel2abs(slot.write_buffer)?;

    // Compact: remove the wasted index space between the used inline
    // elements and the data region. Shift data region left, adjust
    // relptrs in inline elements by -wasted.
    let wasted = (index_cap - n as usize) * w;
    if wasted > 0 && data_used > 0 {
        let old_data_offset = 16 + index_cap * w;
        let new_data_offset = 16 + (n as usize) * w;
        unsafe {
            std::ptr::copy(
                buf_abs.add(old_data_offset),
                buf_abs.add(new_data_offset),
                data_used,
            );
        }
        for i in 0..n {
            let elem_ptr = unsafe { buf_abs.add(16 + (i as usize) * w) };
            crate::voidstar::adjust_relptrs(
                elem_ptr, &local.elem_schema, -(wasted as isize),
            )?;
        }
    }

    // Write the Array header at buffer[0..16]: {size: n, data: 16}.
    unsafe {
        let hdr_ptr = buf_abs as *mut shm_types_crate::Array;
        (*hdr_ptr).size = n as usize;
        (*hdr_ptr).data = 16 as RelPtr;
    }

    let payload_len = 16 + (n as usize) * w + data_used;
    let payload_slice = unsafe { std::slice::from_raw_parts(buf_abs, payload_len) };

    let level = slot.compression_level;
    emit_subpacket_to_disk(slot, local, payload_slice, level)?;

    // Reset buffer counters. The buffer bytes don't need to be cleared;
    // the next @write overwrites them.
    unsafe {
        let mp = slot as *const RegistrySlot as *mut RegistrySlot;
        (*mp).write_buffer_index_count = 0;
        (*mp).write_buffer_data_used = 0;
    }
    Ok(())
}

/// Try to append one element from `elem_src` into the write buffer.
/// Returns Ok(()) on success (whether the element went into the
/// buffer or was emitted directly as an oversize sub-packet). Caller
/// MUST hold the slot futex.
fn append_one_element(
    slot: &RegistrySlot,
    local: &mut ProcessLocalSlot,
    elem_src: AbsPtr,
) -> Result<(), MorlocError> {
    let w = local.elem_schema.width;
    let buf_size = read_write_buffer_bytes_env();

    // Flatten the single element to a self-contained blob:
    //   blob[0..w]: inline (with buffer-relative relptrs into blob[w..])
    //   blob[w..]:  variable bytes (sub-allocations)
    let elem_blob = crate::voidstar::flatten_to_buffer(elem_src, &local.elem_schema)?;
    let variable_size = elem_blob.len().saturating_sub(w);

    // Initialise the index cap on first write into this slot. Clamp
    // to what the buffer can actually hold: with a small env-overridden
    // buf_size (e.g. 4 KiB) and the default INITIAL_CAP (1024) at
    // 8-byte elements, a literal INITIAL_CAP-sized index section would
    // be 8 KiB on its own and the buffer would have no room left for
    // a single element.
    if slot.write_buffer_index_cap == 0 {
        let max_index_cap = (buf_size.saturating_sub(16)) / w.max(1);
        let initial_cap = (WRITE_BUFFER_INDEX_INITIAL_CAP as usize)
            .min(max_index_cap)
            .max(1) as u64;
        unsafe {
            let mp = slot as *const RegistrySlot as *mut RegistrySlot;
            (*mp).write_buffer_index_cap = initial_cap;
        }
    }

    // Single oversize element: doesn't fit even in a fully-empty buffer
    // with the smallest possible index (one slot). The buffer's layout
    // is header (16) + index (>= 1 element) + variable data. If even
    // that minimum exceeds buf_size, the element is truly oversize.
    let min_required = 16 + w + variable_size;
    if min_required > buf_size {
        // Even on an empty buffer, this element wouldn't fit. Flush
        // and emit oversize. The oversize packet is a one-element
        // Array<a> built from elem_blob.
        flush_write_buffer(slot, local)?;
        // Build oversize payload: Array{size=1, data=16} + elem_blob,
        // with elem_blob's inline relptrs shifted by +16 to account
        // for the Array header.
        let oversize_len = 16 + elem_blob.len();
        let mut oversize_payload = Vec::with_capacity(oversize_len);
        // Array header
        let arr_hdr = shm_types_crate::Array { size: 1, data: 16 as RelPtr };
        oversize_payload.extend_from_slice(unsafe {
            std::slice::from_raw_parts(
                &arr_hdr as *const _ as *const u8,
                16,
            )
        });
        // Element blob
        oversize_payload.extend_from_slice(&elem_blob);
        // Shift relptrs in the appended element by +16.
        let elem_in_payload = unsafe {
            oversize_payload.as_mut_ptr().add(16)
        };
        crate::voidstar::adjust_relptrs(
            elem_in_payload, &local.elem_schema, 16isize,
        )?;
        let level = slot.compression_level;
        emit_subpacket_to_disk(slot, local, &oversize_payload, level)?;
        return Ok(());
    }

    // Try to fit one more element. If neither growing the index nor
    // the current data region's remaining space is enough, flush
    // first then retry (next iteration's empty buffer makes room).
    loop {
        let need_index_grow =
            slot.write_buffer_index_count + 1 > slot.write_buffer_index_cap;
        let candidate_cap = if need_index_grow {
            slot.write_buffer_index_cap.saturating_mul(2)
        } else {
            slot.write_buffer_index_cap
        };
        let candidate_index_bytes = (candidate_cap as usize) * w;
        let required = 16 + candidate_index_bytes
            + (slot.write_buffer_data_used as usize)
            + variable_size;
        if required <= buf_size {
            if need_index_grow {
                grow_index_capacity(slot, local, candidate_cap)?;
            }
            break;
        }
        // Doesn't fit. If buffer is empty we've already established
        // the element doesn't fit even minimally (handled above);
        // anything else here is an unexpected edge case.
        if slot.write_buffer_index_count == 0 {
            return Err(MorlocError::Other(
                "@write: internal error -- empty buffer can't fit element \
                 that passed the oversize check".into(),
            ));
        }
        flush_write_buffer(slot, local)?;
        // Loop: buffer is empty now, try again.
    }

    // Copy inline + variable into the buffer and rebase relptrs.
    let buf_abs = crate::shm::rel2abs(slot.write_buffer)?;
    let index_offset = 16 + (slot.write_buffer_index_count as usize) * w;
    let data_region_start = 16 + (slot.write_buffer_index_cap as usize) * w;
    let data_offset = data_region_start + (slot.write_buffer_data_used as usize);

    unsafe {
        std::ptr::copy_nonoverlapping(
            elem_blob.as_ptr(), buf_abs.add(index_offset), w,
        );
        if variable_size > 0 {
            std::ptr::copy_nonoverlapping(
                elem_blob.as_ptr().add(w),
                buf_abs.add(data_offset),
                variable_size,
            );
        }
    }
    // Shift the copied element's relptrs from "blob-relative" (where
    // sub-allocations start at offset w) to "buffer-relative" (where
    // they now start at `data_offset`).
    let shift: isize = (data_offset as isize) - (w as isize);
    if shift != 0 {
        let elem_ptr = unsafe { buf_abs.add(index_offset) };
        crate::voidstar::adjust_relptrs(elem_ptr, &local.elem_schema, shift)?;
    }

    unsafe {
        let mp = slot as *const RegistrySlot as *mut RegistrySlot;
        (*mp).write_buffer_index_count += 1;
        (*mp).write_buffer_data_used += variable_size as u64;
    }
    Ok(())
}

/// `@write level value handle` on an OStream slot. Appends each
/// element of the incoming `[a]` to the slot's SHM write buffer;
/// flushes a sub-packet when the buffer fills. Oversize single
/// elements (>buffer-size) get their own sub-packet.
///
/// Element atomicity is preserved: a single element is never split
/// across sub-packets. Multi-element overflow (a list that partly
/// fits and partly doesn't) is handled per-element -- elements that
/// fit are appended, then the buffer flushes, then the remaining
/// elements continue accumulating in the fresh buffer.
pub fn shared_write_subpacket(
    handle: i64,
    level: u8,
    payload_voidstar: AbsPtr,
) -> Result<(), MorlocError> {
    if payload_voidstar.is_null() {
        return Err(MorlocError::Other("@write: null payload".into()));
    }
    // Validate the level before any I/O.
    let _ = crate::compression::CompressionLevel::from_u8(level)?;

    with_process_local_slot(handle, |local, slot| {
        if slot.kind != MLC_KIND_OSTREAM {
            return Err(MorlocError::Other(format!(
                "@write on non-OStream handle (kind = {})",
                handle_kind_name(slot.kind),
            )));
        }

        let arr = unsafe { &*(payload_voidstar as *const shm_types_crate::Array) };
        let n_elements = arr.size as u64;
        let w = local.elem_schema.width;
        let elem_data_base = if n_elements == 0 {
            std::ptr::null_mut()
        } else {
            crate::shm::rel2abs(arr.data)?
        };

        let _guard = SlotFutexGuard::lock(slot);

        // Pin compression level on first @write into this slot
        // (across all pools); subsequent writes must match.
        if slot.element_count == 0 && slot.write_buffer_index_count == 0 {
            unsafe {
                let mp = slot as *const RegistrySlot as *mut RegistrySlot;
                (*mp).compression_level = level;
            }
        } else if slot.compression_level != level {
            return Err(MorlocError::Other(format!(
                "@write level mismatch: stream was opened/written at level {} \
                 but this call passed {}. All sub-packets must share a level.",
                slot.compression_level, level,
            )));
        }

        // Walk the elements and append each. element_count updates
        // here (not at flush) so @flen reflects buffered elements too.
        for i in 0..n_elements {
            let elem_src = unsafe { elem_data_base.add((i as usize) * w) };
            append_one_element(slot, local, elem_src)?;
            unsafe {
                let mp = slot as *const RegistrySlot as *mut RegistrySlot;
                (*mp).element_count += 1;
                let d = &mut (*mp).diag;
                d.element_count += 1;
            }
        }
        Ok(())
    })
}

/// `@flush handle`: force any buffered elements to be written as a
/// sub-packet immediately, without closing the stream. Useful for
/// tests that need predictable packet boundaries and for user code
/// that wants to commit progress visibly to readers.
pub fn shared_flush_buffer(handle: i64) -> Result<(), MorlocError> {
    with_process_local_slot(handle, |local, slot| {
        if slot.kind != MLC_KIND_OSTREAM {
            return Err(MorlocError::Other(format!(
                "@flush on non-OStream handle (kind = {})",
                handle_kind_name(slot.kind),
            )));
        }
        let _guard = SlotFutexGuard::lock(slot);
        flush_write_buffer(slot, local)
    })
}

/// `@next handle` on an IStream slot. Reads the next sub-packet's
/// header at the slot's current cursor (via this pool's mmap),
/// materialises it, advances the cursor, and returns an SHM
/// `Array<a>` AbsPtr.
///
/// The cursor advance is under the slot futex, so concurrent
/// readers from multiple pools each pull a distinct sub-packet
/// (the multi-reader IStream work-queue pattern). On EOF (cursor
/// at or past the file's footer / end-of-data) returns an empty
/// `Array<a>`.
pub fn shared_next_subpacket(handle: i64) -> Result<AbsPtr, MorlocError> {
    with_process_local_slot(handle, |local, slot| {
        if slot.kind != MLC_KIND_ISTREAM {
            return Err(MorlocError::Other(format!(
                "@next on non-IStream handle (kind = {})",
                handle_kind_name(slot.kind),
            )));
        }

        // Claim the next sub-packet under the futex. The lock window
        // is just header-read + cursor-advance; the actual
        // decompression / deep-copy happens with the lock dropped.
        let (claim_cursor, on_disk_size, header_is_data) = {
            let _guard = SlotFutexGuard::lock(slot);
            let cursor = slot.cursor;
            if cursor >= local.mmap_size || cursor + 32 > local.mmap_size {
                // EOF: leave cursor where it is, return empty Array.
                return empty_shm_array();
            }
            let hdr_bytes = unsafe {
                std::slice::from_raw_parts(
                    (local.mmap_ptr as *const u8).add(cursor as usize),
                    32,
                )
            };
            let header = morloc_runtime_types::packet::PacketHeader::from_bytes(
                hdr_bytes.try_into().unwrap(),
            )?;
            if !header.is_data() {
                // Footer encountered: end-of-stream. Return empty.
                return empty_shm_array();
            }
            let size = 32 + header.offset as u64 + header.length;
            // Advance the cursor BEFORE we drop the futex so concurrent
            // @next on this slot from another pool reads from
            // cursor+size and claims a DIFFERENT sub-packet.
            unsafe {
                let mp = slot as *const RegistrySlot as *mut RegistrySlot;
                (*mp).cursor = cursor + size;
            }
            (cursor, size, true)
        };
        let _ = (on_disk_size, header_is_data); // (currently only on_disk_size feeds cursor advance)

        // Materialise the claimed sub-packet without holding the
        // futex. Other pools can advance through subsequent
        // sub-packets in parallel.
        materialize_and_finalise_subpacket(local, slot, claim_cursor)
    })
}

/// Read the sub-packet at the given byte offset in this pool's mmap
/// and produce a fresh SHM `Array<a>` to return to the caller.
/// Decompresses if needed, walks relptrs, deep-copies.
fn materialize_and_finalise_subpacket(
    local: &ProcessLocalSlot,
    _slot: &RegistrySlot,
    subpacket_off: u64,
) -> Result<AbsPtr, MorlocError> {
    let (src, _on_disk_size) =
        materialize_subpacket_at_offset(local, subpacket_off)?;
    let arr_base = src.arr_base();
    let arr = unsafe { &*(arr_base as *const shm_types_crate::Array) };
    let arr_size = arr.size as u64;
    let elem_width = local.elem_schema.width;

    match src {
        SubpacketSrc::File { payload_base, payload_len, vol_idx_hint, .. } => {
            let resolver = make_file_resolver(payload_base, payload_len);
            let arr_data = resolver(arr.data)?;
            let arr_ptr = shm::shcalloc(1, std::mem::size_of::<shm_types_crate::Array>())?;
            if arr_size == 0 {
                let a = unsafe { &mut *(arr_ptr as *mut shm_types_crate::Array) };
                a.size = 0;
                a.data = shm::RELNULL;
                Ok(arr_ptr)
            } else {
                match slice_bulk_copy_contiguous(
                    0, arr_size as usize, arr_size, arr_data,
                    payload_base, payload_len, vol_idx_hint,
                    &local.elem_schema, elem_width, arr_ptr,
                ) {
                    Ok(()) => Ok(arr_ptr),
                    Err(e) => {
                        let _ = shm::shfree(arr_ptr);
                        Err(e)
                    }
                }
            }
        }
        SubpacketSrc::Shm { arr_base } => Ok(arr_base),
    }
}

/// Allocate and return a SHM `Array` of size 0, the EOF return value
/// of `shared_next_subpacket`. Mirrors `empty_array()` in the
/// process-local path.
fn empty_shm_array() -> Result<AbsPtr, MorlocError> {
    let arr_ptr = shm::shcalloc(1, std::mem::size_of::<shm_types_crate::Array>())?;
    let arr = unsafe { &mut *(arr_ptr as *mut shm_types_crate::Array) };
    arr.size = 0;
    arr.data = shm::RELNULL;
    Ok(arr_ptr)
}

/// `@flen handle`: return the element_count from the slot. Works on
/// IFile (count comes from the final footer's StreamDiag at @open)
/// and IStream (count comes from whichever footer was present).
pub fn shared_handle_length(handle: i64) -> Result<u64, MorlocError> {
    use std::sync::atomic::Ordering;

    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "shared_handle_length: slot index {} out of range", slot_idx,
    )))?;
    // Versioned-pointer read; element_count is futex-protected for
    // OStream writes but we accept a momentarily-stale snapshot
    // for IFile/IStream readers. For OStream callers (which would
    // be unusual for @flen) take the futex.
    let gen_before = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_before != gen_claim {
        return Err(MorlocError::Other(format!(
            "shared_handle_length: generation mismatch (claim {}, slot {})",
            gen_claim, gen_before,
        )));
    }
    if slot.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED {
        return Err(MorlocError::Other(
            "shared_handle_length: slot is not OPEN".into(),
        ));
    }
    if slot.kind != MLC_KIND_IFILE && slot.kind != MLC_KIND_ISTREAM {
        return Err(MorlocError::Other(format!(
            "@flen is only defined on IFile / IStream handles (got kind = {})",
            handle_kind_name(slot.kind),
        )));
    }
    let count = if slot.kind == MLC_KIND_OSTREAM {
        let _guard = SlotFutexGuard::lock(slot);
        slot.element_count
    } else {
        slot.element_count
    };
    let gen_after = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_after != gen_before {
        return Err(MorlocError::Other(
            "shared_handle_length: slot was closed mid-read; retry".into(),
        ));
    }
    Ok(count)
}

/// Versioned-pointer read of `kind` from a shared slot. Used by the
/// cross-pool wire codec to know which `open_dispatch` arm to call on
/// the receiving side.
pub fn shared_handle_kind(handle: i64) -> Result<u8, MorlocError> {
    use std::sync::atomic::Ordering;
    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "shared_handle_kind: slot index {} out of range", slot_idx,
    )))?;
    let gen_before = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_before != gen_claim {
        return Err(MorlocError::Other(format!(
            "shared_handle_kind: generation mismatch (claim {}, slot {})",
            gen_claim, gen_before,
        )));
    }
    let kind = slot.kind;
    let gen_after = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_after != gen_before {
        return Err(MorlocError::Other(
            "shared_handle_kind: slot was closed mid-read; retry".into(),
        ));
    }
    Ok(kind)
}

/// Versioned-pointer read of the file path bound to an open handle.
/// Used by the cross-pool wire codec (`mlc_handle_pack_path` /
/// `mlc_handle_path_len`).
pub fn shared_handle_path(handle: i64) -> Result<String, MorlocError> {
    use std::sync::atomic::Ordering;
    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "shared_handle_path: slot index {} out of range", slot_idx,
    )))?;
    loop {
        let gen_before = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
        if gen_before != gen_claim {
            return Err(MorlocError::Other(format!(
                "shared_handle_path: generation mismatch (claim {}, slot {})",
                gen_claim, gen_before,
            )));
        }
        if slot.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED {
            return Err(MorlocError::Other(
                "shared_handle_path: slot is not OPEN".into(),
            ));
        }
        let path_rel = slot.file_path;
        let path_len = slot.file_path_len as usize;
        if path_rel == shm_types_crate::RELNULL || path_len == 0 {
            return Err(MorlocError::Other(
                "shared_handle_path: slot has empty file_path".into(),
            ));
        }
        let path_abs = crate::shm::rel2abs(path_rel)?;
        // Snapshot bytes into an owned String, then re-verify
        // generation. If a close raced, retry.
        let path_bytes = unsafe {
            std::slice::from_raw_parts(path_abs, path_len)
        };
        let path_string = match std::str::from_utf8(path_bytes) {
            Ok(s) => s.to_string(),
            Err(_) => {
                // Could be a torn read; check generation before
                // surfacing the UTF-8 error.
                let gen_after = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
                if gen_after != gen_before {
                    continue;  // retry
                }
                return Err(MorlocError::Other(
                    "shared_handle_path: file_path is not valid UTF-8".into(),
                ));
            }
        };
        let gen_after = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
        if gen_after == gen_before {
            return Ok(path_string);
        }
        // Raced; retry from the top.
    }
}

/// `@stream :: IFile a -> <IO> IStream a`: open a fresh IStream slot
/// at the same path as the given IFile handle. The two handles have
/// independent cursors (the new IStream walks from `body_start`).
pub fn shared_derive_istream(ifile_handle: i64) -> Result<i64, MorlocError> {
    let kind = shared_handle_kind(ifile_handle)?;
    if kind != MLC_KIND_IFILE {
        return Err(MorlocError::Other(format!(
            "@stream expects an IFile handle (got kind = {})",
            handle_kind_name(kind),
        )));
    }
    let path = shared_handle_path(ifile_handle)?;
    shared_open_istream(&path)
}

/// Batched length lookup over a slice of shared-registry handles.
/// Each handle is resolved via the versioned-pointer pattern; per-
/// handle results are written into `out_lens` if `Some`. Returns the
/// sum of all lengths.
pub fn shared_handles_path_lens(
    handles: &[i64],
    mut out_lens: Option<&mut [i64]>,
) -> Result<u64, MorlocError> {
    if let Some(ref outs) = out_lens {
        debug_assert_eq!(handles.len(), outs.len());
    }
    let mut sum: u64 = 0;
    for (i, &h) in handles.iter().enumerate() {
        let p = shared_handle_path(h).map_err(|e| {
            MorlocError::Other(format!(
                "handle at index {}: {}", i, e
            ))
        })?;
        let len = p.len() as i64;
        if let Some(ref mut outs) = out_lens {
            outs[i] = len;
        }
        sum += len as u64;
    }
    Ok(sum)
}

/// Batched voidstar write for an `[IFile a]` pack pass. Per-handle
/// resolution goes through the versioned-pointer pattern; the path
/// bytes are then memcpy'd into the destination buffer at the running
/// cursor position. `cursor` is advanced past the concatenated bytes.
pub fn shared_write_handles_voidstar(
    handles: &[i64],
    dest_base: *mut u8,
    elem_stride: usize,
    cursor: &mut *mut u8,
) -> Result<(), MorlocError> {
    use crate::shm_types::Array;
    for (i, &h) in handles.iter().enumerate() {
        let path = shared_handle_path(h).map_err(|e| {
            MorlocError::Other(format!("handle at index {}: {}", i, e))
        })?;
        let bytes = path.as_bytes();
        let slot = unsafe { dest_base.add(i * elem_stride) as *mut Array };
        unsafe { (*slot).size = bytes.len(); }
        if bytes.is_empty() {
            unsafe { (*slot).data = shm::RELNULL; }
            continue;
        }
        let cur = *cursor;
        let rel = shm::abs2rel(cur)?;
        unsafe {
            (*slot).data = rel;
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), cur, bytes.len());
            *cursor = cur.add(bytes.len());
        }
    }
    Ok(())
}

/// `@open IFile path` + pattern walk on the shared registry. Dispatches
/// to root-bracket fast paths or the general field walker, mirroring
/// the existing process-local `ifile_walk` but reading from the
/// SHM slot's process-local cache.
pub fn shared_ifile_walk(
    handle: i64,
    path: &str,
    args: &[crate::intrinsics::IFileWalkArg],
) -> Result<AbsPtr, MorlocError> {
    // Root-only `.[]` (single-index): shared bracket-index.
    if path == ".[]" {
        if args.len() != 1 {
            return Err(MorlocError::Other(format!(
                "ifile_walk: \".[]\" expects 1 runtime arg, got {}", args.len()
            )));
        }
        if args[0].has == 0 {
            return Err(MorlocError::Other(
                "ifile_walk: \".[]\" requires a present index (got None)".into(),
            ));
        }
        return shared_ifile_bracket_index(handle, args[0].value);
    }
    // Root-only `.[:]` (slice) with optional field/key tail.
    if let Some(rest) = path.strip_prefix(".[:]") {
        if args.len() != 3 {
            return Err(MorlocError::Other(format!(
                "ifile_walk: \".[:]\" expects 3 runtime args, got {}", args.len()
            )));
        }
        let opt = |a: &crate::intrinsics::IFileWalkArg| {
            if a.has != 0 { Some(a.value) } else { None }
        };
        let tail_steps = if rest.is_empty() {
            Vec::new()
        } else {
            let parsed = parse_field_only_tail(rest)?;
            if parsed.is_none() {
                return shared_ifile_general(handle, path, args);
            }
            parsed.unwrap()
        };
        return shared_ifile_bracket_slice_with_tail(
            handle, opt(&args[0]), opt(&args[1]), opt(&args[2]), &tail_steps,
        );
    }
    // General field-walk path.
    shared_ifile_general(handle, path, args)
}

fn shared_ifile_general(
    handle: i64,
    path: &str,
    args: &[crate::intrinsics::IFileWalkArg],
) -> Result<AbsPtr, MorlocError> {
    let steps = parse_walk_path(path)?;
    with_process_local_slot(handle, |local, slot| {
        if slot.kind != MLC_KIND_IFILE {
            return Err(MorlocError::Other(format!(
                "field access on non-IFile handle (kind = {})",
                handle_kind_name(slot.kind),
            )));
        }
        if local.subpacket_index_local.is_empty() {
            return Err(MorlocError::Other(
                "IFile has no sub-packets (empty file?)".into(),
            ));
        }
        let src = materialize_subpacket(local, 0)?;
        let r = walk_into_fresh(&local.value_schema, &src, &steps, args);
        src.release();
        r
    })
}

fn shared_ifile_bracket_index(handle: i64, index: i64) -> Result<AbsPtr, MorlocError> {
    with_process_local_slot(handle, |local, slot| {
        ifile_bracket_index_against_slot(local, slot, index)
    })
}

fn shared_ifile_bracket_slice_with_tail(
    handle: i64,
    start: Option<i64>,
    stop: Option<i64>,
    step: Option<i64>,
    tail_steps: &[WalkStep],
) -> Result<AbsPtr, MorlocError> {
    with_process_local_slot(handle, |local, slot| {
        ifile_bracket_slice_against_slot(
            local, slot, start, stop, step, tail_steps,
        )
    })
}

/// `@append schema_str path`: open an existing stream file for append
/// and allocate a fresh OSTREAM slot in the shared registry. The
/// schema must match the file's stored schema; mismatches error
/// before any bytes are written. Trailing partial bytes (orphaned
/// temp footer from the prior writer, partial flush) are truncated
/// at the last complete sub-packet boundary.
pub fn shared_append_to_path(
    path: &str,
    expected_schema_str: &str,
) -> Result<i64, MorlocError> {
    use std::ffi::CString;
    use std::sync::atomic::Ordering;

    // Step 1: mmap read-only to find the resume offset and validate
    // the schema. Mmap is unmapped before reopening RW.
    let (mmap_ptr, mmap_size) = mmap_file_readonly(path)?;
    let parsed = match parse_stream_file(path, mmap_ptr, mmap_size) {
        Ok(p) => p,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };
    if parsed.schema_str != expected_schema_str {
        unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
        return Err(MorlocError::Other(format!(
            "@append: schema mismatch on '{}': file has '{}', open requested '{}'",
            path, parsed.schema_str, expected_schema_str
        )));
    }
    let stream_hdr = match parse_stream_header(mmap_ptr, mmap_size) {
        Ok(h) => h,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };
    let resume_off = if let Some(&last_off) = parsed.subpacket_index.last() {
        match read_subpacket_size(mmap_ptr, mmap_size, last_off) {
            Ok(sz) => last_off + sz,
            Err(e) => {
                unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
                return Err(e);
            }
        }
    } else {
        stream_hdr.body_start
    };
    let element_count_at_resume = parsed.element_count;
    let value_schema_clone = parsed.value_schema.clone();
    let elem_schema_clone = parsed.elem_schema.clone();
    let subpacket_index_clone = parsed.subpacket_index.clone();
    let schema_str_clone = parsed.schema_str.clone();
    unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }

    // Step 2: reopen RW, flock, truncate at resume offset.
    let c_path = CString::new(path).map_err(|e| {
        MorlocError::Other(format!("@append: path contains NUL: {}", e))
    })?;
    let fd = unsafe {
        libc::open(c_path.as_ptr(), libc::O_RDWR | libc::O_CLOEXEC, 0)
    };
    if fd < 0 {
        return Err(MorlocError::Io(std::io::Error::last_os_error()));
    }
    let lock_rc = unsafe { libc::flock(fd, libc::LOCK_EX | libc::LOCK_NB) };
    if lock_rc != 0 {
        let e = std::io::Error::last_os_error();
        unsafe { libc::close(fd); }
        return Err(MorlocError::Other(format!(
            "@append: failed to flock '{}': {}", path, e
        )));
    }
    let trunc_rc = unsafe { libc::ftruncate(fd, resume_off as libc::off_t) };
    if trunc_rc != 0 {
        let e = std::io::Error::last_os_error();
        unsafe { libc::close(fd); }
        return Err(MorlocError::Io(e));
    }

    // Step 3: allocate a fresh OSTREAM slot in the shared registry,
    // pre-seeded with the resume cursor + element_count.
    let (slot_idx, slot) = match allocate_slot_cas() {
        Ok(s) => s,
        Err(e) => {
            unsafe { libc::close(fd); }
            return Err(e);
        }
    };
    let _guard = SlotFutexGuard::lock(slot);
    let publish_result = (|| -> Result<u64, MorlocError> {
        let path_rel = shm_copy_bytes(path.as_bytes())?;
        let schema_rel = shm_copy_bytes(schema_str_clone.as_bytes())?;
        let buf_bytes = read_write_buffer_bytes_env();
        let buf_abs = crate::shm::shcalloc(1, buf_bytes)?;
        let buf_rel = crate::shm::abs2rel(buf_abs)?;
        let mut diag = StreamDiag::new();
        diag.subpacket_count = subpacket_index_clone.len() as u64;
        diag.element_count = element_count_at_resume;
        // SHM-resident sub-packet index pre-seeded with the on-disk
        // offsets discovered during forward-scan recovery. Subsequent
        // flushes from any pool append under the slot futex. Initial
        // capacity is at least the current length so we don't grow
        // during the first append.
        let preseed_len = subpacket_index_clone.len();
        let idx_cap_initial: u64 = std::cmp::max(
            OSTREAM_SUBPACKET_INDEX_INITIAL_CAP,
            (preseed_len as u64).next_power_of_two().max(1),
        );
        let idx_buf_bytes = (idx_cap_initial as usize) * std::mem::size_of::<u64>();
        let idx_buf_abs = crate::shm::shcalloc(1, idx_buf_bytes)?;
        let idx_buf_rel = crate::shm::abs2rel(idx_buf_abs)?;
        if preseed_len > 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(
                    subpacket_index_clone.as_ptr(),
                    idx_buf_abs as *mut u64,
                    preseed_len,
                );
            }
        }
        unsafe {
            let mp = slot as *const RegistrySlot as *mut RegistrySlot;
            (*mp).kind = MLC_KIND_OSTREAM;
            (*mp).file_path = path_rel;
            (*mp).file_path_len = path.len() as u32;
            (*mp).schema_str = schema_rel;
            (*mp).schema_str_len = schema_str_clone.len() as u32;
            (*mp).subpacket_index = idx_buf_rel;
            (*mp).subpacket_index_len = preseed_len as u64;
            (*mp).subpacket_index_cap = idx_cap_initial;
            (*mp).body_start = stream_hdr.body_start;
            (*mp).final_footer = 0;
            (*mp).cursor = resume_off;
            (*mp).element_count = element_count_at_resume;
            (*mp).compression_level = 0;
            (*mp).opener_pid = std::process::id();
            (*mp).opener_pid_start_time = read_pid_start_time();
            (*mp).diag = diag;
            (*mp).write_buffer = buf_rel;
            (*mp).write_buffer_index_cap = 0;
            (*mp).write_buffer_index_count = 0;
            (*mp).write_buffer_data_used = 0;
        }
        slot.call_id.store(current_call_id(), Ordering::Release);
        let bump = registry_gen_salt() | 1;
        let new_gen = (slot.generation.fetch_add(bump, Ordering::AcqRel) + bump) & GENERATION_MASK;
        Ok(new_gen)
    })();
    let new_gen = match publish_result {
        Ok(g) => g,
        Err(e) => {
            release_slot_locked(slot);
            unsafe { libc::close(fd); }
            return Err(e);
        }
    };

    // Install the process-local cache. The OPENER's sub-packet index is
    // handed to this fresh slot so subsequent @close can rebuild the final
    // footer correctly. In the multi-pool case, the `@append`er is the
    // canonical "opener" of the new slot and owns the final-footer write.
    let local = ProcessLocalSlot {
        cached_generation: new_gen,
        mmap_ptr: std::ptr::null_mut(),
        mmap_size: 0,
        fd,
        cache: Box::new(StreamCache::new(0)),
        value_schema: value_schema_clone,
        elem_schema: elem_schema_clone,
        subpacket_index_local: subpacket_index_clone,
        subpacket_elem_cum: None,
        is_data_packet: false,
    };
    let handle = pack_handle(new_gen, slot_idx);
    install_process_local_slot(handle, local);
    Ok(handle)
}

// ── Off-worker-thread sweeper ────────────────────────────────────────────
//
// Daemon workers tag every `@open` with the current dispatch's `call_id`
// (held in TLS, set at start of the dispatch). After the daemon sends its
// response back to the nexus, it enqueues a per-call sweep request and
// clears its TLS. A single dedicated sweeper thread drains the queue and
// walks the registry, discarding any slots whose `call_id` matches.
//
// The sweep is OFF the worker thread (so a slow sweep doesn't block the
// next dispatch on the same worker) and confirms `state` + `call_id`
// UNDER the slot futex before discarding (so a fresh allocation that
// landed in the same slot index between pre-filter and discard isn't
// accidentally swept).
//
// Per-PID sweeps (for crashed pools) flow through the same thread and
// queue.

/// A request enqueued to the sweeper thread.
#[derive(Debug, Clone, Copy)]
pub enum SweepRequest {
    /// Discard all slots whose `call_id` field matches.
    PerCall(u64),
    /// Discard all slots whose (`opener_pid`, `opener_pid_start_time`)
    /// matches a (PID, start_time) pair. Used when a pool is detected
    /// to have crashed.
    PerPid(u32, u64),
}

/// Sender half of the sweeper queue. Cloned to every daemon worker
/// that needs to enqueue a sweep. Initialised by `sweeper_init`.
static SWEEPER_TX: Mutex<Option<std::sync::mpsc::Sender<SweepRequest>>> =
    Mutex::new(None);

/// Spawn the dedicated sweeper thread and install its `Sender` in
/// `SWEEPER_TX`. Idempotent: subsequent calls observe the existing
/// sender and return immediately. Called from the daemon startup
/// path (or any other place that wants per-call cleanup).
///
/// The thread is detached: it lives for the lifetime of the
/// process. On process exit, the OS reclaims it. No clean-shutdown
/// handshake is needed because the sweep is best-effort (the registry
/// SHM volume is unlinked at shclose anyway, freeing any straggler
/// slots).
pub fn sweeper_init() {
    let mut guard = SWEEPER_TX.lock().unwrap();
    if guard.is_some() {
        return;
    }
    let (tx, rx) = std::sync::mpsc::channel::<SweepRequest>();
    *guard = Some(tx);
    drop(guard);
    std::thread::Builder::new()
        .name("morloc-stream-sweeper".into())
        .spawn(move || sweeper_main(rx))
        .expect("morloc-stream-sweeper: thread spawn failed");
}

/// Enqueue a per-call sweep request. Non-blocking. Returns silently
/// if the sweeper isn't initialised (which would be a runtime bug:
/// the daemon path is supposed to call `sweeper_init` at startup).
///
/// `CALL_ID_NO_SWEEP` (0) is filtered here: enqueueing a sweep for
/// the sentinel value would needlessly walk the registry without
/// matching anything.
pub fn sweeper_enqueue_call(call_id: u64) {
    if call_id == CALL_ID_NO_SWEEP {
        return;
    }
    let guard = SWEEPER_TX.lock().unwrap();
    if let Some(tx) = guard.as_ref() {
        // Errors mean the receiver has been dropped (process is
        // shutting down). Discard silently.
        let _ = tx.send(SweepRequest::PerCall(call_id));
    }
}

/// Enqueue a per-PID sweep request. Called when a pool crash is detected
/// (the pool's PID + start_time uniquely identify the dead pool's slots).
pub fn sweeper_enqueue_pid(pid: u32, start_time: u64) {
    let guard = SWEEPER_TX.lock().unwrap();
    if let Some(tx) = guard.as_ref() {
        let _ = tx.send(SweepRequest::PerPid(pid, start_time));
    }
}

/// Sweeper thread main loop. Drains the queue forever; exits when
/// the `Sender` half is dropped (only happens at clean shutdown if
/// somebody calls `sweeper_shutdown`).
fn sweeper_main(rx: std::sync::mpsc::Receiver<SweepRequest>) {
    while let Ok(req) = rx.recv() {
        match req {
            SweepRequest::PerCall(call_id) => sweep_per_call(call_id),
            SweepRequest::PerPid(pid, start_time) => {
                sweep_per_pid(pid, start_time);
            }
        }
    }
}

/// Walk the registry and discard any OPEN slot whose `call_id`
/// matches. Two-phase: lockfree pre-filter (`state` + `call_id`
/// Acquire-loads), then take the slot futex and re-confirm before
/// calling `shared_discard_handle_locked`.
///
/// The re-confirm protects against the race where another dispatch
/// (on a different worker) reallocates the slot in the window
/// between the pre-filter and the discard.
fn sweep_per_call(call_id: u64) {
    use std::sync::atomic::Ordering;
    let (slots_base, slot_count) = registry_slot_array();
    if slots_base.is_null() || slot_count == 0 {
        return;
    }
    for idx in 0..slot_count {
        // SAFETY: idx < slot_count and the array is mapped in SHM.
        let slot = unsafe {
            &*(slots_base.add(idx * STREAM_ENTRY_SIZE) as *const RegistrySlot)
        };
        if slot.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED {
            continue;
        }
        if slot.call_id.load(Ordering::Acquire) != call_id {
            continue;
        }
        // Confirm under the slot futex.
        slot_futex_lock(slot);
        let still_open = slot.state.load(Ordering::Acquire) == SLOT_STATE_OPEN_SHARED;
        let still_matches = slot.call_id.load(Ordering::Acquire) == call_id;
        if still_open && still_matches {
            // shared_discard_handle_locked frees the slot in place
            // without taking the futex again (we hold it).
            let _ = shared_discard_handle_locked(slot, idx);
        }
        slot_futex_unlock(slot);
    }
}

/// Walk the registry and discard any OPEN slot whose
/// (`opener_pid`, `opener_pid_start_time`) matches. The start-time
/// disambiguates PID reuse: a slot owned by the original PID
/// has the start time matching that process's `/proc/PID/stat`
/// field 22; a new process inheriting the same PID after the
/// original exits has a different start time.
fn sweep_per_pid(pid: u32, start_time: u64) {
    use std::sync::atomic::Ordering;
    let (slots_base, slot_count) = registry_slot_array();
    if slots_base.is_null() || slot_count == 0 {
        return;
    }
    for idx in 0..slot_count {
        let slot = unsafe {
            &*(slots_base.add(idx * STREAM_ENTRY_SIZE) as *const RegistrySlot)
        };
        if slot.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED {
            continue;
        }
        // `opener_pid` and `opener_pid_start_time` are immutable
        // after @open publication, so a lockfree read (under the
        // versioned-pointer pattern) is safe. Read generation
        // before and after, retry on mismatch.
        let gen_before = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
        let read_pid = slot.opener_pid;
        let read_start = slot.opener_pid_start_time;
        let gen_after = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
        if gen_before != gen_after {
            continue;  // raced; next sweep iteration may catch it
        }
        if read_pid != pid || read_start != start_time {
            continue;
        }
        slot_futex_lock(slot);
        // Re-confirm under the futex (state could have changed).
        if slot.state.load(Ordering::Acquire) == SLOT_STATE_OPEN_SHARED
            && slot.opener_pid == pid
            && slot.opener_pid_start_time == start_time
        {
            let _ = shared_discard_handle_locked(slot, idx);
        }
        slot_futex_unlock(slot);
    }
}

/// Read the configured write-buffer capacity in bytes from the
/// `MORLOC_WRITE_BUFFER_BYTES` env var, defaulting to
/// `WRITE_BUFFER_BYTES_DEFAULT` (16 MiB). Tests use this to lower
/// the threshold and exercise flush logic without writing megabytes.
/// Minimum is 4 KiB so the Array header + a few elements always fit.
pub fn read_write_buffer_bytes_env() -> usize {
    const MIN: usize = 4096;
    if let Ok(s) = std::env::var("MORLOC_WRITE_BUFFER_BYTES") {
        if let Ok(n) = s.parse::<usize>() {
            return n.max(MIN);
        }
    }
    WRITE_BUFFER_BYTES_DEFAULT
}

/// Generate a fresh `call_id`. Reads 8 bytes from `/dev/urandom` and
/// ORs with 1 to ensure the value is never `CALL_ID_NO_SWEEP` (0).
/// Reusing a `call_id` is statistically negligible (2^-63 per call)
/// and would only matter for a stale sweep entry; the under-futex
/// re-check rejects.
pub fn generate_call_id() -> u64 {
    use std::io::Read;
    let mut buf = [0u8; 8];
    if let Ok(mut f) = std::fs::File::open("/dev/urandom") {
        if f.read_exact(&mut buf).is_ok() {
            return u64::from_le_bytes(buf) | 1;
        }
    }
    // Fallback: time + counter. Process-private, monotonic enough.
    use std::sync::atomic::{AtomicU64, Ordering};
    static FALLBACK_COUNTER: AtomicU64 = AtomicU64::new(0);
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0xDEAD_BEEF);
    let n = FALLBACK_COUNTER.fetch_add(1, Ordering::Relaxed);
    (now.wrapping_add(n) ^ 0xA5A5_A5A5_A5A5_A5A5) | 1
}

// ── Types ─────────────────────────────────────────────────────────────────

/// Per-handle cache of decompressed (and relptr-adjusted) sub-packets in
/// SHM. Approximate clock-hand LRU; eviction is `shfree` on the entry's
/// `shm_packet` block — the `BlockHeader` refcount makes any still-in-
/// flight reader safe via the existing `shincref` mechanism.
#[derive(Debug)]
pub struct StreamCache {
    pub capacity_bytes: u64,
    pub current_bytes: u64,
    pub entries: Vec<CacheEntry>,
    /// Index of the next entry the clock hand will scan on eviction.
    pub clock_hand: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct CacheEntry {
    pub subpacket_idx: u64,
    pub shm_packet: AbsPtr,
    pub size_bytes: u64,
    /// 0 = candidate for eviction on next clock pass; 1 = recently used.
    /// Cleared by the clock hand during a sweep; set on every hit.
    pub clock_bit: u8,
}

impl StreamCache {
    fn new(capacity_bytes: u64) -> Self {
        Self {
            capacity_bytes,
            current_bytes: 0,
            entries: Vec::new(),
            clock_hand: 0,
        }
    }
}

/// Locates one sub-packet's voidstar Array within the IFile, telling
/// the walker how to resolve in-payload relptrs.
///
/// `File` is the zero-copy path: the Array struct and the variable-
/// length tails the walker chases all live in the mmap'd file region.
/// File-internal relptrs are plain offsets relative to `payload_base`.
///
/// `Shm` is the path for compressed sub-packets, where we must
/// materialise the decompressed bytes into SHM (decompression has to
/// write somewhere). After materialisation the relptrs are SHM-
/// relative and the standard `shm::rel2abs` resolver applies. `Shm`
/// blocks are refcounted; the caller drops their reference with
/// `release()` once the walk is done.
enum SubpacketSrc {
    File {
        arr_base: AbsPtr,
        payload_base: AbsPtr,
        payload_len: u64,
        /// Producer's Layer-3 `vol_idx` hint (high 15 bits of every
        /// emitted relptr). The file resolver ignores these for live
        /// dereferences, but the bulk-copy slice path needs the hint
        /// to compute a single `delta` for `voidstar::adjust_relptrs`
        /// rather than walking the schema itself.
        vol_idx_hint: u16,
    },
    Shm {
        /// AbsPtr to the SHM-resident Array struct (refcount = 1
        /// owned by the caller of `cache_get_or_materialize`).
        arr_base: AbsPtr,
    },
}

impl SubpacketSrc {
    fn arr_base(&self) -> AbsPtr {
        match *self {
            SubpacketSrc::File { arr_base, .. } => arr_base,
            SubpacketSrc::Shm { arr_base } => arr_base,
        }
    }
    /// Drop the caller's reference to the source. For `File`, this is
    /// a no-op (the mmap is owned by the ProcessLocalSlot). For `Shm`, we
    /// `shfree` the SHM block, decrementing the refcount; the cache's
    /// own reference keeps the block alive for future hits.
    fn release(self) {
        if let SubpacketSrc::Shm { arr_base } = self {
            let _ = shm::shfree(arr_base);
        }
    }
}

/// Bounds-checked resolver for relptrs embedded in a file-backed
/// sub-packet's payload. The producer may have encoded a vol_idx
/// hint in the high bits (writers sometimes pre-bias relptrs toward
/// the volume they expect the reader to allocate); the resolver
/// IGNORES the vol_idx and extracts only the 48-bit offset, since
/// for file-backed reads we don't go through the SHM volume table.
///
/// Rejects sentinel relptrs (RELNULL or future reserved values) and
/// offsets past the payload end.
fn make_file_resolver(
    payload_base: AbsPtr,
    payload_len: u64,
) -> impl Fn(RelPtr) -> Result<AbsPtr, MorlocError> {
    move |relptr: RelPtr| {
        if relptr_is_sentinel(relptr) || relptr < 0 {
            return Err(MorlocError::Other(
                "file-resident relptr is a sentinel value (corrupt payload?)".into(),
            ));
        }
        let offset = relptr_offset(relptr) as u64;
        if offset > payload_len {
            return Err(MorlocError::Other(format!(
                "file-resident relptr offset {} exceeds payload length {}",
                offset, payload_len,
            )));
        }
        // SAFETY: bounds-checked offset within the payload region the
        // caller passed; payload_base is a stable mmap'd pointer.
        Ok(unsafe { (payload_base as *const u8).add(offset as usize) as AbsPtr })
    }
}

fn read_cache_cap_env() -> u64 {
    if let Ok(s) = std::env::var("MORLOC_IFILE_CACHE_BYTES") {
        if let Ok(n) = s.parse::<u64>() {
            return n;
        }
    }
    DEFAULT_IFILE_CACHE_BYTES
}

// ── Public API: open / close / fschema ────────────────────────────────────

/// Open a stream/data packet file as an IFile (random access). Thin
/// delegation to `shared_open_ifile`; kept so the in-file test suite
/// can drive the runtime without naming the shared variant.
pub fn open_ifile(path: &str) -> Result<i64, MorlocError> {
    shared_open_ifile(path)
}

/// Open a stream file as an IStream (forward-only). Delegation to
/// `shared_open_istream`.
pub fn open_istream(path: &str) -> Result<i64, MorlocError> {
    shared_open_istream(path)
}

/// Open a fresh OStream with the empty-schema placeholder; the typed
/// path is `open_ostream_with_schema` which the codegen wires to
/// `mlc_open_ostream`. Delegation to `shared_open_ostream_with_schema`.
pub fn open_ostream(path: &str) -> Result<i64, MorlocError> {
    shared_open_ostream_with_schema(path, "")
}

/// Open an OStream with the element schema known up-front. Delegation
/// to `shared_open_ostream_with_schema`.
pub fn open_ostream_with_schema(
    path: &str,
    schema_str: &str,
) -> Result<i64, MorlocError> {
    shared_open_ostream_with_schema(path, schema_str)
}

/// Parsed form of a stream/data packet file ready to populate a
/// `ProcessLocalSlot` + SHM `RegistrySlot`. Shared by `shared_open_ifile`
/// and `shared_open_istream` since the on-disk format is identical for
/// both kinds; only post-open access semantics differ.
struct ParsedStreamFile {
    schema_str: String,
    value_schema: Schema,
    elem_schema: Schema,
    subpacket_index: Vec<u64>,
    element_count: u64,
    diag: Option<StreamDiag>,
    /// True iff the file is a STREAM_PACKET that carries a
    /// `METADATA_TYPE_FOOTER_FINAL` block. False for STREAM_PACKET files
    /// that only have a temp footer (or none at all), and false for
    /// DATA_PACKET files (which inherently have no footer concept; see
    /// `is_data_packet` for that signal).
    final_footer: bool,
    /// True iff the file is a single DATA_PACKET (the `@save` shape),
    /// false for STREAM_PACKET. DATA-packet files are self-contained
    /// and inherently complete: they have no header / footer concept,
    /// and IFile can always open them for random access.
    is_data_packet: bool,
    /// Byte offset of the first sub-packet header (i.e. end of the
    /// stream header). For DATA-packet files (single-packet shape) this
    /// is 0: the whole file IS the sub-packet. IStream uses this as the
    /// initial cursor position; IFile ignores it.
    body_start: u64,
}

/// Parse a mmap'd stream or data packet file into a `ParsedStreamFile`.
/// Caller is responsible for munmap on error.
fn parse_stream_file(
    path: &str,
    mmap_ptr: AbsPtr,
    mmap_size: u64,
) -> Result<ParsedStreamFile, MorlocError> {
    if mmap_size < 32 {
        return Err(MorlocError::Packet(
            "file too short for a packet header".into(),
        ));
    }
    let hdr_bytes = unsafe {
        std::slice::from_raw_parts(mmap_ptr as *const u8, 32)
    };
    let outer_header = PacketHeader::from_bytes(hdr_bytes.try_into().unwrap())?;

    let is_data_packet = outer_header.is_data();
    let (schema_str, subpacket_index, element_count, diag, final_footer, body_start):
        (String, Vec<u64>, u64, Option<StreamDiag>, bool, u64) = if is_data_packet {
        let (schema, index, count) = open_data_packet(path, mmap_ptr, mmap_size)?;
        // DATA-packet files have no stream header; the whole file is a
        // single sub-packet that starts at offset 0. IStream's forward
        // walker reads this as one sub-packet, then sees EOF. There is
        // no StreamDiag and no footer of either kind, so diag = None
        // and final_footer = false; the IFile gate uses is_data_packet
        // separately to know this branch is still random-access safe.
        (schema, index, count, None, false, 0u64)
    } else if outer_header.is_stream() {
        let StreamHeader { schema: schema_str, body_start } =
            parse_stream_header(mmap_ptr, mmap_size)?;
        if body_start < mmap_size {
            let fmt = read_subpacket_format(mmap_ptr, mmap_size, body_start)?;
            if fmt != PACKET_FORMAT_VOIDSTAR {
                return Err(MorlocError::Other(format!(
                    "file '{}' has {}-format sub-packets; only voidstar is supported",
                    path, packet_format_name(fmt)
                )));
            }
        }
        // IStream walks forward from body_start without needing an
        // index, so an empty subpacket_index on temp-footer files is
        // fine here; IFile's open path enforces final_footer separately.
        let (subpacket_index, element_count, diag, final_footer) =
            match try_read_footer(mmap_ptr, mmap_size) {
                Ok(Some(parsed)) => (
                    parsed.subpacket_index,
                    parsed.element_count,
                    parsed.diag,
                    parsed.final_footer,
                ),
                Ok(None) | Err(_) => {
                    // No footer at all (writer crashed mid-write or
                    // before the first temp-footer pwrite). Scan the
                    // headers to recover counts; IFile open will refuse
                    // this kind of file too.
                    let scanned = forward_scan_subpackets(mmap_ptr, mmap_size, body_start)?;
                    (scanned.subpacket_index, scanned.element_count, None, false)
                }
            };
        (schema_str, subpacket_index, element_count, diag, final_footer, body_start)
    } else {
        return Err(MorlocError::Packet(format!(
            "file '{}' is neither a STREAM_PACKET nor a DATA_PACKET (cmd_type = {})",
            path,
            unsafe { outer_header.command.cmd_type.cmd_type }
        )));
    };

    let parsed_schema = parse_schema(&schema_str).map_err(|e| {
        MorlocError::Schema(format!(
            "file '{}' has unparseable schema '{}': {}", path, schema_str, e
        ))
    })?;
    let (value_schema, elem_schema) = if is_data_packet {
        let elem = if parsed_schema.serial_type == SerialType::Array
            && !parsed_schema.parameters.is_empty()
        {
            parsed_schema.parameters[0].clone()
        } else {
            parsed_schema.clone()
        };
        (parsed_schema.clone(), elem)
    } else {
        (array_schema(&parsed_schema), parsed_schema.clone())
    };

    Ok(ParsedStreamFile {
        schema_str,
        value_schema,
        elem_schema,
        subpacket_index,
        element_count,
        diag,
        final_footer,
        is_data_packet,
        body_start,
    })
}

/// Close any open handle. Bumps the slot's generation; subsequent
/// operations on the same Int return a clean generation-mismatch error.
///
/// For OStream, this is the **explicit-close** path: it runs
/// `finalise_ostream` which pwrites the final footer and fdatasyncs
/// before releasing the slot. A file closed this way is "cleanly
/// closed" -- the final footer carries the full sub-packet index, and
/// IFile can open it for random access.
///
/// For arena-drop unwinding (writer crashed, exception, manifold scope
/// exit without explicit `@close`), use `discard_handle` instead: that
/// path leaves the temp footer in place so the file's on-disk state
/// honestly reflects "writer did not finish".
pub fn close_handle(handle: i64) -> Result<(), MorlocError> {
    shared_close_handle(handle)
}

/// Release a handle without writing a final footer. Used by the
/// eval_arena Drop path: when an OStream goes out of scope without an
/// explicit `@close`, we deliberately leave the temp footer in place
/// so downstream tooling can distinguish "writer crashed / never
/// finished" from "writer completed cleanly". The fd is closed (which
/// also releases the flock) and the slot is freed; subsequent ops on
/// the handle return generation-mismatch errors.
///
/// IFile / IStream entries take the same release path as `close_handle`
/// (they have nothing to finalise either way).
pub fn discard_handle(handle: i64) -> Result<(), MorlocError> {
    shared_discard_handle(handle)
}

/// Read the schema string from a stream/data file without opening it as
/// a typed handle. Used by `@fschema`.
pub fn read_schema_from_file(path: &str) -> Result<String, MorlocError> {
    // We need only the first ~4 KiB of the file to parse the header and
    // its metadata block. Use pread rather than full mmap to keep
    // fschema cheap (we don't need the rest of the file).
    use std::io::Read;
    let mut f = std::fs::File::open(path)
        .map_err(|e| MorlocError::Io(e))?;
    let mut buf = vec![0u8; 4096];
    let n = f.read(&mut buf)
        .map_err(|e| MorlocError::Io(e))?;
    if n < 32 {
        return Err(MorlocError::Packet(format!(
            "file '{}' too short to contain a packet header ({} bytes)",
            path, n
        )));
    }
    buf.truncate(n);
    let header = PacketHeader::from_bytes(buf[..32].try_into().unwrap())?;
    if !header.is_stream() && !header.is_data() {
        return Err(MorlocError::Packet(format!(
            "file '{}' is not a stream or data packet", path
        )));
    }
    // Parse the metadata block in-place.
    let meta_end = 32usize.checked_add(header.offset as usize)
        .ok_or_else(|| MorlocError::Packet("offset overflow".into()))?;
    if meta_end > buf.len() {
        // Schema is past our pread window; read more.
        let mut more = vec![0u8; meta_end];
        more[..buf.len()].copy_from_slice(&buf);
        f.read_exact(&mut more[buf.len()..])
            .map_err(|e| MorlocError::Io(e))?;
        buf = more;
    }
    match read_schema_from_meta(&buf)? {
        Some(s) => Ok(s),
        None => Err(MorlocError::Packet(format!(
            "file '{}' has no schema metadata block", path
        ))),
    }
}

// ── mmap helpers ──────────────────────────────────────────────────────────

fn mmap_file_readonly(path: &str) -> Result<(AbsPtr, u64), MorlocError> {
    let f = OpenOptions::new()
        .read(true)
        .open(Path::new(path))
        .map_err(|e| MorlocError::Io(e))?;
    let fd = f.as_raw_fd();
    let size = f.metadata()
        .map_err(|e| MorlocError::Io(e))?
        .len();
    if size == 0 {
        return Err(MorlocError::Packet(format!(
            "file '{}' is empty (cannot be a stream packet)", path
        )));
    }

    // SAFETY: fd is open; PROT_READ + MAP_PRIVATE is the standard
    // read-only mapping. We hold the file handle until mmap returns.
    let ptr = unsafe {
        libc::mmap(
            std::ptr::null_mut(),
            size as usize,
            libc::PROT_READ,
            libc::MAP_PRIVATE,
            fd,
            0,
        )
    };
    if ptr == libc::MAP_FAILED {
        return Err(MorlocError::Other(format!(
            "mmap failed for '{}': {}", path, std::io::Error::last_os_error()
        )));
    }

    // No explicit MADV_RANDOM here: in practice IFile traffic is a mix
    // of bulk slices (sequential access through the records section
    // and the string tail -- benefits from default ~128 KB readahead)
    // and single-element index lookups (.[k] f). MADV_RANDOM disables
    // readahead entirely and turns a 200K-element slice into 200K
    // single-page synchronous reads, dominating the walker cost.
    // Default kernel heuristics handle both patterns acceptably; the
    // slice walker also issues MADV_WILLNEED over the projected
    // sub-packet range below to prefault the bulk-read section.

    // `f` (and thus fd) is dropped when this function returns; mmap
    // pins the underlying inode regardless of fd lifetime.
    Ok((ptr as AbsPtr, size))
}

// ── Stream header parsing ─────────────────────────────────────────────────

#[derive(Debug)]
struct StreamHeader {
    /// Schema string of the stream's element type.
    schema: String,
    /// Byte offset where the first sub-packet starts (immediately after
    /// the stream header's 32-byte header + metadata block).
    body_start: u64,
}

/// Open a single DATA_PACKET file as a one-sub-packet IFile.
/// Returns `(value_schema_str, subpacket_index, element_count)`.
/// `value_schema_str` is the file's full payload schema string.
///
/// For files whose payload is `[a]` (a list), element_count is the
/// array's length and bracket access on the IFile is valid. For
/// files whose payload is anything else (tuple, record, primitive),
/// element_count is 0 and only PatternStruct access is valid.
///
/// Rejects compressed DATA_PACKET files: decompressing the whole
/// payload defeats IFile's purpose. The user can rewrite the file
/// as a STREAM_PACKET (per-sub-packet compression) or use `@load` to
/// materialise the whole thing.
fn open_data_packet(
    path: &str,
    mmap_ptr: AbsPtr,
    mmap_size: u64,
) -> Result<(String, Vec<u64>, u64), MorlocError> {
    if mmap_size < 32 {
        return Err(MorlocError::Packet("file too short for a packet header".into()));
    }
    // SAFETY: bounds verified.
    let hdr_bytes = unsafe {
        std::slice::from_raw_parts(mmap_ptr as *const u8, 32)
    };
    let header = PacketHeader::from_bytes(hdr_bytes.try_into().unwrap())?;
    if !header.is_data() {
        return Err(MorlocError::Packet(
            "open_data_packet called on non-DATA file".into(),
        ));
    }
    // SAFETY: is_data() implies the data variant of the command union.
    let data = unsafe { header.command.data };
    if data.format != PACKET_FORMAT_VOIDSTAR {
        return Err(MorlocError::Packet(format!(
            "file '{}' is {}-format; only voidstar is supported for IFile (use @load instead)",
            path, packet_format_name(data.format)
        )));
    }
    if data.compression != PACKET_COMPRESSION_NONE {
        return Err(MorlocError::Packet(format!(
            "file '{}' is a compressed DATA_PACKET; IFile cannot \
             random-access compressed monolithic payloads. Either rewrite \
             as a STREAM_PACKET (compression then applies per sub-packet) \
             or use `@load path` to materialise the whole file.",
            path
        )));
    }
    let meta_end = 32u64.checked_add(header.offset as u64)
        .ok_or_else(|| MorlocError::Packet("DATA header offset overflow".into()))?;
    if meta_end > mmap_size {
        return Err(MorlocError::Packet(
            "DATA metadata block extends past file end".into(),
        ));
    }
    let payload_off = meta_end;
    let payload_len = header.length as u64;
    if payload_off.checked_add(payload_len)
        .map(|end| end > mmap_size)
        .unwrap_or(true)
    {
        return Err(MorlocError::Packet(
            "DATA payload extends past file end".into(),
        ));
    }
    // Schema string from the metadata block.
    // SAFETY: meta_end <= mmap_size.
    let prefix = unsafe {
        std::slice::from_raw_parts(mmap_ptr as *const u8, meta_end as usize)
    };
    let value_schema_str = read_schema_from_meta(prefix)?
        .ok_or_else(|| MorlocError::Packet(format!(
            "file '{}' is a DATA packet without a SCHEMA_STRING metadata block. \
             This file was either produced by a pre-Stage-2 morloc version (which did \
             not embed schemas in @save output) or was hand-crafted. Regenerate it \
             with the current `@save` to embed the schema, or use `@load` instead of \
             `@open` (load does not require a self-describing schema).",
            path,
        )))?;
    let value_schema = parse_schema(&value_schema_str).map_err(|e| {
        MorlocError::Schema(format!(
            "file '{}' has unparseable schema '{}': {}",
            path, value_schema_str, e
        ))
    })?;
    // element_count is meaningful only when the file's value is a
    // list -- it's the array's size. For non-list values it's 0 (no
    // "length" concept).
    let element_count: u64 =
        if value_schema.serial_type == SerialType::Array {
            if payload_len < std::mem::size_of::<shm_types_crate::Array>() as u64 {
                return Err(MorlocError::Packet(
                    "DATA payload too short for Array header".into(),
                ));
            }
            // SAFETY: bounds verified.
            let size_bytes = unsafe {
                std::slice::from_raw_parts(
                    (mmap_ptr as *const u8).add(payload_off as usize),
                    8,
                )
            };
            u64::from_le_bytes(size_bytes.try_into().unwrap())
        } else {
            0
        };
    // subpacket_index = [0]: the "sub-packet" is the whole file
    // packet, whose header begins at byte 0.
    Ok((value_schema_str, vec![0], element_count))
}

fn parse_stream_header(mmap_ptr: AbsPtr, size: u64) -> Result<StreamHeader, MorlocError> {
    if size < 32 {
        return Err(MorlocError::Packet(
            "file too short for a stream header".into(),
        ));
    }
    // SAFETY: mmap_ptr points to size bytes of readable memory.
    let bytes = unsafe {
        std::slice::from_raw_parts(mmap_ptr as *const u8, 32)
    };
    let header = PacketHeader::from_bytes(bytes.try_into().unwrap())?;
    if !header.is_stream() {
        return Err(MorlocError::Packet(format!(
            "file is not a stream packet (cmd_type = {})",
            unsafe { header.command.cmd_type.cmd_type }
        )));
    }
    let meta_end = 32u64.checked_add(header.offset as u64)
        .ok_or_else(|| MorlocError::Packet("stream offset overflow".into()))?;
    if meta_end > size {
        return Err(MorlocError::Packet(
            "stream metadata block extends past file end".into(),
        ));
    }
    // Read schema from the stream-header metadata block.
    // SAFETY: meta_end <= size.
    let stream_prefix = unsafe {
        std::slice::from_raw_parts(mmap_ptr as *const u8, meta_end as usize)
    };
    let schema = read_schema_from_meta(stream_prefix)?
        .ok_or_else(|| MorlocError::Packet(
            "stream header missing schema metadata block".into(),
        ))?;
    Ok(StreamHeader { schema, body_start: meta_end })
}

/// Read the format byte of a sub-packet whose header begins at
/// `off` within the mmap'd region.
fn read_subpacket_format(
    mmap_ptr: AbsPtr,
    size: u64,
    off: u64,
) -> Result<u8, MorlocError> {
    if off + 32 > size {
        return Err(MorlocError::Packet(
            "sub-packet header extends past file end".into(),
        ));
    }
    // SAFETY: mmap_ptr + off points to at least 32 bytes (validated above).
    let bytes = unsafe {
        std::slice::from_raw_parts(
            (mmap_ptr as *const u8).add(off as usize),
            32,
        )
    };
    let header = PacketHeader::from_bytes(bytes.try_into().unwrap())?;
    if !header.is_data() {
        return Err(MorlocError::Packet(format!(
            "sub-packet at offset {} is not a DATA packet", off
        )));
    }
    // SAFETY: header is_data() implies the data variant of the command.
    Ok(unsafe { header.command.data.format })
}

// ── Footer parsing ────────────────────────────────────────────────────────

#[derive(Debug)]
struct ParsedFooter {
    subpacket_index: Vec<u64>,
    element_count: u64,
    diag: Option<StreamDiag>,
    final_footer: bool,
}

/// Try to read the footer at EOF; returns `Ok(None)` if no footer tail
/// magic is present (writer crashed mid-stream, or live tail). Returns
/// `Err` on corrupt footer.
fn try_read_footer(
    mmap_ptr: AbsPtr,
    size: u64,
) -> Result<Option<ParsedFooter>, MorlocError> {
    if size < (STREAM_TAIL_SIZE as u64) {
        return Ok(None);
    }
    // SAFETY: size >= STREAM_TAIL_SIZE; read the last 8 bytes.
    let tail_bytes = unsafe {
        std::slice::from_raw_parts(
            (mmap_ptr as *const u8)
                .add((size - STREAM_TAIL_SIZE as u64) as usize),
            STREAM_TAIL_SIZE,
        )
    };
    let tail_arr: [u8; STREAM_TAIL_SIZE] = tail_bytes.try_into().unwrap();
    let footer_len = match decode_stream_tail(&tail_arr) {
        Some(n) => n as u64,
        None => return Ok(None),
    };
    let footer_start = size
        .checked_sub(STREAM_TAIL_SIZE as u64)
        .and_then(|x| x.checked_sub(footer_len))
        .ok_or_else(|| MorlocError::Packet(
            "footer length tail is past file start".into(),
        ))?;
    if footer_start + 32 > size {
        return Err(MorlocError::Packet(
            "footer header extends past file end".into(),
        ));
    }
    // SAFETY: footer_start + footer_len + STREAM_TAIL_SIZE <= size.
    let footer_slice = unsafe {
        std::slice::from_raw_parts(
            (mmap_ptr as *const u8).add(footer_start as usize),
            footer_len as usize,
        )
    };
    let footer_hdr = PacketHeader::from_bytes(
        footer_slice[..32].try_into().unwrap(),
    )?;
    if !footer_hdr.is_footer() {
        // Tail-magic matched but the packet header isn't a footer; treat
        // as no footer (defensive: tail magic could collide with random
        // data on a truncated write).
        return Ok(None);
    }

    let mut subpacket_index = Vec::new();
    let mut diag: Option<StreamDiag> = None;
    let mut final_footer = false;
    for (kind, body) in iter_packet_metadata(footer_slice)? {
        match kind {
            METADATA_TYPE_FOOTER_FINAL => { final_footer = true; }
            METADATA_TYPE_STREAM_DIAG => {
                diag = Some(StreamDiag::from_bytes(body)?);
            }
            METADATA_TYPE_SUBPACKET_INDEX => {
                subpacket_index =
                    morloc_runtime_types::packet::decode_subpacket_index(body)?;
            }
            _ => {}  // unknown blocks are tolerated
        }
    }

    // Derive element_count from the diag if present; otherwise leave
    // zero (caller may fall back to scanning sub-packet headers).
    let element_count = diag.as_ref()
        .map(|d| { let n = d.element_count; n })
        .unwrap_or(0);

    Ok(Some(ParsedFooter {
        subpacket_index,
        element_count,
        diag,
        final_footer,
    }))
}

// ── Forward-scan recovery ─────────────────────────────────────────────────

#[derive(Debug)]
struct ForwardScan {
    subpacket_index: Vec<u64>,
    element_count: u64,
}

/// Walk every sub-packet header from `body_start` to EOF (or the first
/// non-DATA packet — typically the footer, if any). Used when the
/// footer is absent or unusable.
fn forward_scan_subpackets(
    mmap_ptr: AbsPtr,
    size: u64,
    body_start: u64,
) -> Result<ForwardScan, MorlocError> {
    let mut subpacket_index = Vec::new();
    let element_count: u64 = 0;
    let mut cur = body_start;

    while cur + 32 <= size {
        // SAFETY: cur + 32 <= size.
        let hdr_bytes = unsafe {
            std::slice::from_raw_parts(
                (mmap_ptr as *const u8).add(cur as usize),
                32,
            )
        };
        let header = match PacketHeader::from_bytes(
            hdr_bytes.try_into().unwrap(),
        ) {
            Ok(h) => h,
            Err(_) => break,  // corrupt header: stop here
        };
        if header.is_footer() {
            break;
        }
        if !header.is_data() {
            // Unknown or stale data; stop scanning.
            break;
        }
        let offset = header.offset as u64;
        let length = header.length as u64;
        let total = 32u64
            .checked_add(offset)
            .and_then(|x| x.checked_add(length))
            .ok_or_else(|| MorlocError::Packet(
                "sub-packet length overflow during scan".into(),
            ))?;
        if cur + total > size {
            // Partial last sub-packet (crashed mid-write). Stop cleanly.
            break;
        }
        subpacket_index.push(cur);
        // Count elements in this sub-packet if its metadata carries it
        // (currently we don't write per-sub-packet counts; element_count
        // remains 0 for files without a final footer and the user must
        // walk).
        let _ = (offset, length, &header);

        // Track element-count if a per-sub-packet ELEMENT_COUNT metadata
        // block is present. Today the writer does NOT emit this; reserved
        // for future evolution. The simpler path is to leave this 0 when
        // the final footer is absent and document `length f` as
        // expensive in that case.
        let _ = element_count;

        cur += total;
    }
    Ok(ForwardScan { subpacket_index, element_count })
}

// ── Validation helpers used by other modules ──────────────────────────────

/// Look up the kind of an open handle. Useful for error messages from
/// IStream/OStream-specific intrinsics that receive an IFile handle.
pub fn handle_kind(handle: i64) -> Result<u8, MorlocError> {
    // Legacy entry point; delegates to the shared-SHM-registry impl.
    // The old process-local registry is never populated by the new
    // `shared_open_*` path, so a direct `with_entry` lookup would
    // always report "slot is free". See `handle_path` for the same
    // pattern.
    shared_handle_kind(handle)
}

/// Read the file path bound to an open handle. The cross-pool wire
/// codec for IFile values ships this path so the receiving pool can
/// `open_dispatch(path, kind)` and bind a fresh local handle of its
/// own; each pool keeps an independent fd + mmap + slot, and the
/// receiver's own `eval_arena` is what closes the new handle on scope
/// exit. Symmetric path on the receive side: `open_dispatch` after
/// reading `(kind, path)` off the wire.
pub fn handle_path(handle: i64) -> Result<String, MorlocError> {
    shared_handle_path(handle)
}

/// Batched length lookup for an [IFile a] sizing pass: one registry
/// acquire, N path-length reads. Returns the sum. When `out_lens` is
/// `Some`, the per-handle lengths are written there too; callers that
/// only need the sum pass `None`. No String clone -- we only need
/// `.len()`.
pub fn handles_path_lens(
    handles: &[i64],
    out_lens: Option<&mut [i64]>,
) -> Result<u64, MorlocError> {
    shared_handles_path_lens(handles, out_lens)
}

/// Batched voidstar write for an [IFile a] pack pass. One registry
/// acquire, N path memcpys. `dest_base` points at the first Array slot
/// in the destination buffer; successive slots are `elem_stride` bytes
/// apart (= sizeof(Array) for a packed array). `cursor` is advanced
/// past the concatenated path bytes.
pub fn write_handles_voidstar(
    handles: &[i64],
    dest_base: *mut u8,
    elem_stride: usize,
    cursor: &mut *mut u8,
) -> Result<(), MorlocError> {
    shared_write_handles_voidstar(handles, dest_base, elem_stride, cursor)
}

/// Read the total element count for an IFile handle.
///
/// Errors when the file's value type is not a list: `length` on a
/// tuple/record IFile is undefined, and silently returning 0 hides
/// type-mismatch user errors. The wire schema is checked at open time
/// and stored on the entry; this is a cheap branch.
///
/// For list-typed IFiles whose final footer is absent (the writer
/// crashed before close), the count is 0 because forward-scan recovery
/// does not currently re-tally elements. That is a known limitation
/// of the recovery path, NOT a non-list signal.
pub fn handle_length(handle: i64) -> Result<u64, MorlocError> {
    shared_handle_length(handle)
}

/// Unified IFile pattern walker. Parses the path string and runs the
/// general walker. The C ABI (`mlc_ifile_walk`) is the single public
/// entry point and the codegen surface above the C ABI is also
/// single-call.
///
/// Path encoding mirrors `Morloc.CodeGenerator.IFile.walkStepsToPath`:
///
/// | Path                | Args                  | Dispatch                  |
/// |---------------------|-----------------------|---------------------------|
/// | `".[]"`             | `[idx]`               | root bracket-index (fast) |
/// | `".[:]"`            | `[start, stop, step]` | root bracket-slice (fast) |
/// | `".1.foo"` etc.     | `[]`                  | general walker            |
/// | `".(.x;.y)"`        | `[]`                  | general walker (group)    |
/// | `".(.0.[];.1)"`     | `[idx]`               | general walker (bracket-  |
/// |                     |                       | in-group; codegen does    |
/// |                     |                       | not yet emit this, but    |
/// |                     |                       | the design supports it)   |
///
/// Args flow in DFS order across the whole walk: every bracket step
/// consumes 1 (index) or 3 (slice) args from the front of the list.
pub fn ifile_walk(
    handle: i64,
    path: &str,
    args: &[crate::intrinsics::IFileWalkArg],
) -> Result<AbsPtr, MorlocError> {
    shared_ifile_walk(handle, path, args)
}

// ── IFile pattern walker (BracketIndex / BracketSlice) ────────────────────
//
// The walker treats an IFile as a logical sequence of `a`-valued
// elements distributed across sub-packets in the file. Each sub-packet
// holds one voidstar Array (`[a]`) whose `size` field is its element
// count and whose `data` relptr points to consecutive element slots.
//
// A pattern access (`.[i] f` or `.[i:j] f`) requires:
//   1. Mapping a global element index to a (sub-packet, local index)
//      pair. The cumulative element-count index supports a binary
//      search; it is built lazily on first random-access query.
//   2. Materializing the sub-packet's payload into SHM. Today this is
//      a fresh shmemcpy + adjust_relptrs per access; the per-handle
//      LRU cache hook is in place but not yet populated.
//   3. Walking to the local element and `deep_copy`ing it (or each
//      slice element) into a fresh result SHM block. The result has
//      `elem_schema.width` bytes for one element, or
//      `n_out * elem_width + sub_block_allocs` for a slice.

/// Container schema = Array(elem_schema). Constructed at runtime since
/// each sub-packet's payload is `[a]` even though the stream's header
/// stores the bare `a` schema.
fn array_schema(elem: &Schema) -> Schema {
    Schema {
        serial_type: SerialType::Array,
        size: 1,
        width: std::mem::size_of::<shm_types_crate::Array>(),
        offsets: Vec::new(),
        hint: None,
        parameters: vec![elem.clone()],
        keys: Vec::new(),
        name: None,
    }
}

/// Build the cumulative element-count index for this entry, if not
/// already cached. Idempotent.
fn ensure_elem_index(local: &mut ProcessLocalSlot) -> Result<(), MorlocError> {
    if local.subpacket_elem_cum.is_some() {
        return Ok(());
    }
    let n = local.subpacket_index_local.len();
    let mut cum = Vec::with_capacity(n + 1);
    cum.push(0u64);
    for &subpacket_off in &local.subpacket_index_local {
        let sz = read_subpacket_element_count(
            local.mmap_ptr,
            local.mmap_size,
            subpacket_off,
            &local.elem_schema,
        )?;
        let last = *cum.last().unwrap();
        cum.push(last.saturating_add(sz));
    }
    local.subpacket_elem_cum = Some(cum);
    Ok(())
}

/// Read the element count from a sub-packet's payload header. The
/// payload of each sub-packet is a voidstar Array; the first 16 bytes
/// are `{ size: usize, data: RelPtr }`. For compressed sub-packets we
/// decompress the whole sub-packet just to read those 16 bytes — the
/// element-count index is built once at open time so the per-sub-packet
/// cost is amortised against many subsequent random-access reads.
fn read_subpacket_element_count(
    mmap_ptr: AbsPtr,
    file_size: u64,
    subpacket_off: u64,
    elem_schema: &Schema,
) -> Result<u64, MorlocError> {
    if subpacket_off + 32 > file_size {
        return Err(MorlocError::Packet(
            "sub-packet header past file end during element-count scan".into(),
        ));
    }
    // SAFETY: bounds checked.
    let hdr_bytes = unsafe {
        std::slice::from_raw_parts(
            (mmap_ptr as *const u8).add(subpacket_off as usize),
            32,
        )
    };
    let header = PacketHeader::from_bytes(hdr_bytes.try_into().unwrap())?;
    // SAFETY: header.is_data() implies CommandData variant.
    let data = unsafe { header.command.data };
    if data.compression != PACKET_COMPRESSION_NONE
        && data.compression != PACKET_COMPRESSION_ZSTD
    {
        return Err(MorlocError::Packet(format!(
            "sub-packet at {} has unknown compression byte {}",
            subpacket_off, data.compression,
        )));
    }
    let payload_off = subpacket_off + 32 + header.offset as u64;
    let payload_len = header.length as u64;
    if payload_off + payload_len > file_size {
        return Err(MorlocError::Packet(
            "sub-packet payload past file end".into(),
        ));
    }
    // SAFETY: payload region bounded.
    let payload = unsafe {
        std::slice::from_raw_parts(
            (mmap_ptr as *const u8).add(payload_off as usize),
            payload_len as usize,
        )
    };
    let payload_bytes: std::borrow::Cow<'_, [u8]> =
        if data.compression == PACKET_COMPRESSION_ZSTD {
            // Decompress to read the 16-byte Array header.
            // `decompress_packet` expects a full packet (header + meta +
            // payload), not a payload-only slice, so reconstruct it.
            let mut full = Vec::with_capacity(32 + header.offset as usize + payload_len as usize);
            full.extend_from_slice(hdr_bytes);
            // Include metadata block.
            let meta_off = subpacket_off + 32;
            if meta_off + header.offset as u64 > file_size {
                return Err(MorlocError::Packet(
                    "sub-packet metadata block past file end".into(),
                ));
            }
            // SAFETY: bounds checked.
            let meta = unsafe {
                std::slice::from_raw_parts(
                    (mmap_ptr as *const u8).add(meta_off as usize),
                    header.offset as usize,
                )
            };
            full.extend_from_slice(meta);
            full.extend_from_slice(payload);
            let decompressed =
                morloc_runtime_types::compression::decompress_packet(&full)?;
            // Find the payload region in the decompressed packet.
            let dec_hdr = PacketHeader::from_bytes(
                decompressed[..32].try_into().unwrap(),
            )?;
            let dec_payload_start = 32 + dec_hdr.offset as usize;
            let dec_payload_end = dec_payload_start + dec_hdr.length as usize;
            std::borrow::Cow::Owned(
                decompressed[dec_payload_start..dec_payload_end].to_vec(),
            )
        } else {
            std::borrow::Cow::Borrowed(payload)
        };

    // The payload starts with the Array struct: { size: usize, data: RelPtr }.
    let _ = elem_schema; // unused here but documents the contract
    if payload_bytes.len() < std::mem::size_of::<shm_types_crate::Array>() {
        return Err(MorlocError::Packet(
            "sub-packet payload too short for Array header".into(),
        ));
    }
    // Read size (usize, little-endian on supported platforms).
    let size_bytes: [u8; 8] = payload_bytes[..8].try_into().unwrap();
    let size = u64::from_le_bytes(size_bytes);
    Ok(size)
}

/// Parse a sub-packet's header at `subpacket_off` and return the
/// payload byte-region offsets, the header struct, and the raw header
/// bytes (the latter needed to reconstruct a full packet for the
/// compressed-decompress path).
fn read_subpacket_header(
    local: &ProcessLocalSlot,
    subpacket_off: u64,
) -> Result<(PacketHeader, u64 /*payload_off*/, u64 /*payload_len*/, u64 /*meta_off*/), MorlocError> {
    if subpacket_off + 32 > local.mmap_size {
        return Err(MorlocError::Packet(
            "sub-packet header past EOF".into(),
        ));
    }
    // SAFETY: bounds verified.
    let hdr_bytes = unsafe {
        std::slice::from_raw_parts(
            (local.mmap_ptr as *const u8).add(subpacket_off as usize),
            32,
        )
    };
    let header = PacketHeader::from_bytes(hdr_bytes.try_into().unwrap())?;
    let data = unsafe { header.command.data };
    if data.format != PACKET_FORMAT_VOIDSTAR {
        return Err(MorlocError::Packet(format!(
            "sub-packet at {} is {}-format; IFile requires voidstar",
            subpacket_off,
            packet_format_name(data.format),
        )));
    }
    let meta_off = subpacket_off + 32;
    let payload_off = meta_off + header.offset as u64;
    let payload_len = header.length as u64;
    if payload_off + payload_len > local.mmap_size {
        return Err(MorlocError::Packet(
            "sub-packet payload past EOF".into(),
        ));
    }
    Ok((header, payload_off, payload_len, meta_off))
}

/// Locate a sub-packet's source. For uncompressed sub-packets this is
/// zero-copy: returns a `File` descriptor pointing into the mmap.
/// For compressed sub-packets, decompresses the payload into a fresh
/// SHM block, rebases its relptrs to be SHM-relative, and returns a
/// `Shm` descriptor (refcount = 1, owned by the caller).
fn materialize_subpacket(
    local: &ProcessLocalSlot,
    sub_k: usize,
) -> Result<SubpacketSrc, MorlocError> {
    if sub_k >= local.subpacket_index_local.len() {
        return Err(MorlocError::Other(format!(
            "sub-packet index {} out of range (have {})",
            sub_k, local.subpacket_index_local.len(),
        )));
    }
    let subpacket_off = local.subpacket_index_local[sub_k];
    let (src, _on_disk_size) = materialize_subpacket_at_offset(local, subpacket_off)?;
    Ok(src)
}

/// Materialise the sub-packet whose header starts at `subpacket_off`
/// (a byte offset into the mmap'd file). Returns the materialised
/// source AND the sub-packet's full on-disk size (header + metadata +
/// payload), so the caller can advance a byte cursor past it.
///
/// Used by IStream's forward walker (cursor-driven, no index needed)
/// and by `materialize_subpacket` (index-driven, used by IFile).
fn materialize_subpacket_at_offset(
    local: &ProcessLocalSlot,
    subpacket_off: u64,
) -> Result<(SubpacketSrc, u64), MorlocError> {
    let (header, payload_off, payload_len, meta_off) =
        read_subpacket_header(local, subpacket_off)?;
    let on_disk_size = 32 + header.offset as u64 + header.length;
    let data = unsafe { header.command.data };

    // Fast path: uncompressed. The walker reads directly from the
    // mmap'd region; no SHM allocation, no copy. The kernel page-
    // cache shares pages across pools opening the same path.
    if data.compression == PACKET_COMPRESSION_NONE {
        let arr_base = unsafe {
            (local.mmap_ptr as *const u8).add(payload_off as usize) as AbsPtr
        };
        // Parse the producer's Layer-3 vol_idx hint from the header +
        // metadata bytes (zero-allocation; the slices view directly
        // into the mmap). Hint is only used by the contiguous-slice
        // bulk-copy path; other walks ignore it via the file resolver.
        let hint_bytes_len = 32 + header.offset as usize;
        let mut hint_buf = Vec::with_capacity(hint_bytes_len);
        unsafe {
            hint_buf.extend_from_slice(std::slice::from_raw_parts(
                (local.mmap_ptr as *const u8).add(subpacket_off as usize),
                hint_bytes_len,
            ));
        }
        let vol_idx_hint = morloc_runtime_types::packet::read_vol_index_from_meta(&hint_buf)
            .ok()
            .flatten()
            .unwrap_or(0);
        return Ok((SubpacketSrc::File {
            arr_base,
            payload_base: arr_base,
            payload_len,
            vol_idx_hint,
        }, on_disk_size));
    }

    // Slow path: compressed. Decompress the sub-packet's full bytes
    // (header + metadata + payload), then materialise just the
    // decompressed payload region into SHM and rebase its relptrs.
    if data.compression != PACKET_COMPRESSION_ZSTD {
        return Err(MorlocError::Packet(format!(
            "sub-packet at {} has unknown compression byte {}",
            subpacket_off, data.compression,
        )));
    }
    // SAFETY: all bounds verified by read_subpacket_header.
    let hdr_bytes = unsafe {
        std::slice::from_raw_parts(
            (local.mmap_ptr as *const u8).add(subpacket_off as usize),
            32,
        )
    };
    let meta = unsafe {
        std::slice::from_raw_parts(
            (local.mmap_ptr as *const u8).add(meta_off as usize),
            header.offset as usize,
        )
    };
    let payload = unsafe {
        std::slice::from_raw_parts(
            (local.mmap_ptr as *const u8).add(payload_off as usize),
            payload_len as usize,
        )
    };
    let mut full = Vec::with_capacity(
        32 + header.offset as usize + payload.len(),
    );
    full.extend_from_slice(hdr_bytes);
    full.extend_from_slice(meta);
    full.extend_from_slice(payload);
    let decompressed =
        morloc_runtime_types::compression::decompress_packet(&full)?;
    let dec_hdr = PacketHeader::from_bytes(
        decompressed[..32].try_into().unwrap(),
    )?;
    let dec_payload_start = 32 + dec_hdr.offset as usize;
    let dec_payload_end = dec_payload_start + dec_hdr.length as usize;
    let dec_payload = &decompressed[dec_payload_start..dec_payload_end];

    let base = shm::shmalloc(dec_payload.len())?;
    // SAFETY: base owns dec_payload.len() bytes.
    unsafe {
        std::ptr::copy_nonoverlapping(
            dec_payload.as_ptr(),
            base,
            dec_payload.len(),
        );
    }
    // From here on, every error path must shfree(base) -- the block has
    // been allocated but its refcount is held only by the local `base`
    // until we return Ok. abs2rel or adjust_relptrs failures (e.g. a
    // corrupt relptr in the decompressed payload) would otherwise leak
    // the SHM block.
    let base_rel = match shm::abs2rel(base) {
        Ok(r) => r,
        Err(e) => { let _ = shm::shfree(base); return Err(e); }
    };
    let arr_schema = array_schema(&local.elem_schema);
    if let Err(e) = voidstar::adjust_relptrs(base, &arr_schema, base_rel) {
        let _ = shm::shfree(base);
        return Err(e);
    }
    Ok((SubpacketSrc::Shm { arr_base: base }, on_disk_size))
}

/// Resolve a global element index, normalising negatives Python-style.
/// Returns `(sub_packet_k, local_idx)`.
fn resolve_global_index(
    local: &mut ProcessLocalSlot,
    requested: i64,
) -> Result<(usize, u64), MorlocError> {
    ensure_elem_index(local)?;
    let cum = local
        .subpacket_elem_cum
        .as_ref()
        .expect("ensure_elem_index just populated subpacket_elem_cum");
    let total = *cum.last().unwrap_or(&0u64) as i64;
    let idx = if requested < 0 { requested + total } else { requested };
    if idx < 0 || idx >= total {
        return Err(MorlocError::Other(format!(
            "IFile bracket index {} out of bounds (have {} elements)",
            requested, total,
        )));
    }
    let idx_u = idx as u64;
    // partition_point returns the index of the first cum[k] > idx_u;
    // sub-packet K contains elements [cum[K], cum[K+1]).
    let upper = cum.partition_point(|&c| c <= idx_u);
    let k = upper - 1;
    let local_idx = idx_u - cum[k];
    Ok((k, local_idx))
}

/// `@next` on an IStream handle: materialise the current sub-packet as
/// `[a]` into a fresh SHM block, advance the cursor, return the AbsPtr
/// to the materialised Array. At EOF the returned Array has size 0 and
/// `data = RELNULL` -- the user-visible empty list.
pub fn next_subpacket(handle: i64) -> Result<AbsPtr, MorlocError> {
    shared_next_subpacket(handle)
}

/// `@write level value handle` on an OStream: write one sub-packet
/// whose payload is `value` (already materialised by the bridge into
/// a SHM voidstar Array<T> via `to_voidstar<vector<T>>`). The user
/// chooses sub-packet granularity at the morloc level by batching
/// elements into the list passed to `@write`.
///
/// First-call semantics: `level` is locked in for the file's lifetime
/// so downstream tooling can read a uniform compression level. Mixed
/// levels on subsequent writes are an error.
pub fn write_subpacket(
    handle: i64,
    level: u8,
    payload_voidstar: AbsPtr,
) -> Result<(), MorlocError> {
    shared_write_subpacket(handle, level, payload_voidstar)
}

/// Push a sub-packet offset into the diag's tail-window. The window is
/// length-prefixed; once full, slide forward by overwriting the oldest.
///
/// Manipulates the packed struct via local copies because `StreamDiag`
/// is `#[repr(C, packed)]` -- direct field references are unaligned.
fn push_tail_window(d: &mut StreamDiag, offset: u64) {
    let cap = morloc_runtime_types::packet::STREAM_DIAG_TAIL_MAX as u32;
    let len = d.tail_len;
    if len < cap {
        let mut tail = d.tail;
        tail[len as usize] = offset;
        d.tail = tail;
        d.tail_len = len + 1;
    } else {
        let mut tail = d.tail;
        for i in 1..cap as usize {
            tail[i - 1] = tail[i];
        }
        tail[cap as usize - 1] = offset;
        d.tail = tail;
    }
}

fn unix_micros_now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_micros() as u64)
        .unwrap_or(0)
}

/// `@append :: Str -> <IO> (OStream a)`: open an existing stream file
/// for append. Forward-scans to find the last complete sub-packet,
/// truncates any partial trailing bytes, reopens RW, flock-acquires,
/// and registers a fresh OSTREAM slot whose cursor sits at the resume
/// offset. Schema must match the existing file's schema.
pub fn append_to_path(
    path: &str,
    expected_schema_str: &str,
) -> Result<i64, MorlocError> {
    shared_append_to_path(path, expected_schema_str)
}

/// Read the on-disk byte size of a sub-packet at `offset`. Used by
/// `@append` to advance past the last complete sub-packet to the
/// resume cursor.
fn read_subpacket_size(
    mmap_ptr: AbsPtr,
    mmap_size: u64,
    offset: u64,
) -> Result<u64, MorlocError> {
    if offset + 32 > mmap_size {
        return Err(MorlocError::Packet(
            "sub-packet header past EOF in @append".into(),
        ));
    }
    let hdr_bytes = unsafe {
        std::slice::from_raw_parts(
            (mmap_ptr as *const u8).add(offset as usize), 32,
        )
    };
    let hdr = PacketHeader::from_bytes(hdr_bytes.try_into().unwrap())?;
    Ok(32 + hdr.offset as u64 + hdr.length as u64)
}

/// `@concat :: [Str] -> Str -> <IO> ()`: byte-level concat of N stream
/// files into one. Exploits the stream-packet concat invariant: take
/// src[0] from its stream header through its last sub-packet, then
/// each subsequent src[i] from its FIRST sub-packet through its LAST
/// sub-packet (dropping headers and footers). Finally write one final
/// footer with the merged subpacket index over the dest's tail.
pub fn concat_files(paths: &[&str], dest: &str) -> Result<(), MorlocError> {
    use std::ffi::CString;
    if paths.is_empty() {
        return Err(MorlocError::Other("@concat: paths list is empty".into()));
    }

    let c_dest = CString::new(dest).map_err(|e| {
        MorlocError::Other(format!("@concat: dest path contains NUL: {}", e))
    })?;
    let dest_fd = unsafe {
        libc::open(
            c_dest.as_ptr(),
            libc::O_RDWR | libc::O_CREAT | libc::O_EXCL | libc::O_CLOEXEC,
            0o644,
        )
    };
    if dest_fd < 0 {
        return Err(MorlocError::Io(std::io::Error::last_os_error()));
    }
    let mut dest_cursor: u64 = 0;
    let mut merged_index: Vec<u64> = Vec::new();
    let mut total_element_count: u64 = 0;
    let mut reference_schema: Option<String> = None;

    for (i, &p) in paths.iter().enumerate() {
        // Parse via mmap (cheap: we only touch the header + footer +
        // sub-packet index). The bulk byte copy below uses sendfile()
        // against a separate fd so the kernel moves the data without
        // crossing into userspace.
        let (mmap_ptr, mmap_size) = match mmap_file_readonly(p) {
            Ok(t) => t,
            Err(e) => {
                unsafe { libc::close(dest_fd); }
                let _ = unsafe { libc::unlink(c_dest.as_ptr()) };
                return Err(e);
            }
        };
        let parsed = match parse_stream_file(p, mmap_ptr, mmap_size) {
            Ok(p2) => p2,
            Err(e) => {
                unsafe {
                    libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize);
                    libc::close(dest_fd);
                    libc::unlink(c_dest.as_ptr());
                }
                return Err(e);
            }
        };
        match &reference_schema {
            None => {
                reference_schema = Some(parsed.schema_str.clone());
            }
            Some(ref_str) if *ref_str != parsed.schema_str => {
                unsafe {
                    libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize);
                    libc::close(dest_fd);
                    libc::unlink(c_dest.as_ptr());
                }
                return Err(MorlocError::Other(format!(
                    "@concat: schema mismatch -- '{}' has '{}', earlier had '{}'",
                    p, parsed.schema_str, ref_str
                )));
            }
            _ => {}
        }
        let hdr = match parse_stream_header(mmap_ptr, mmap_size) {
            Ok(h) => h,
            Err(e) => {
                unsafe {
                    libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize);
                    libc::close(dest_fd);
                    libc::unlink(c_dest.as_ptr());
                }
                return Err(e);
            }
        };

        // First sub-packet's body start = stream header end.
        let body_start = hdr.body_start;
        // Last sub-packet's end: take the largest start offset from the
        // index and read its size, OR if no index, body_start (empty
        // source contributes nothing).
        let body_end = if let Some(&last_off) = parsed.subpacket_index.last() {
            match read_subpacket_size(mmap_ptr, mmap_size, last_off) {
                Ok(sz) => last_off + sz,
                Err(e) => {
                    unsafe {
                        libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize);
                        libc::close(dest_fd);
                        libc::unlink(c_dest.as_ptr());
                    }
                    return Err(e);
                }
            }
        } else {
            body_start
        };

        // Done with mmap (parsing complete); release it before sendfile.
        unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }

        // Reopen the source RDONLY for sendfile. The previous fd that
        // mmap was built on was dropped in mmap_file_readonly; sendfile
        // needs its own fd anyway.
        let c_src = match CString::new(p) {
            Ok(c) => c,
            Err(e) => {
                unsafe { libc::close(dest_fd); libc::unlink(c_dest.as_ptr()); }
                return Err(MorlocError::Other(format!(
                    "@concat: source path '{}' contains NUL: {}", p, e
                )));
            }
        };
        let src_fd = unsafe {
            libc::open(c_src.as_ptr(), libc::O_RDONLY | libc::O_CLOEXEC, 0)
        };
        if src_fd < 0 {
            let e = std::io::Error::last_os_error();
            unsafe { libc::close(dest_fd); libc::unlink(c_dest.as_ptr()); }
            return Err(MorlocError::Io(e));
        }

        if i == 0 {
            // Preserve the source's stream header verbatim so the
            // merged file's schema metadata block matches.
            if let Err(e) = sendfile_range(dest_fd, src_fd, 0, body_start, dest_cursor) {
                unsafe { libc::close(src_fd); libc::close(dest_fd); libc::unlink(c_dest.as_ptr()); }
                return Err(e);
            }
            dest_cursor += body_start;
        }

        // Record sub-packet offsets remapped to the dest cursor space.
        for &src_off in &parsed.subpacket_index {
            let delta = src_off - body_start;
            merged_index.push(dest_cursor + delta);
        }
        if body_end > body_start {
            let body_len = body_end - body_start;
            if let Err(e) = sendfile_range(dest_fd, src_fd, body_start, body_len, dest_cursor) {
                unsafe { libc::close(src_fd); libc::close(dest_fd); libc::unlink(c_dest.as_ptr()); }
                return Err(e);
            }
            dest_cursor += body_len;
        }
        unsafe { libc::close(src_fd); }
        total_element_count += parsed.element_count;
    }

    // Write the merged final footer (small, fine to pwrite from userspace).
    let mut diag = morloc_runtime_types::packet::StreamDiag::new();
    diag.subpacket_count = merged_index.len() as u64;
    diag.element_count = total_element_count;
    let footer = morloc_runtime_types::packet::make_final_footer_packet(
        &diag, &merged_index,
    );
    if let Err(e) = pwrite_all_fd(dest_fd, &footer, dest_cursor) {
        unsafe {
            libc::close(dest_fd);
            libc::unlink(c_dest.as_ptr());
        }
        return Err(e);
    }
    let rc = unsafe { libc::fdatasync(dest_fd) };
    if rc != 0 {
        let e = std::io::Error::last_os_error();
        unsafe { libc::close(dest_fd); }
        return Err(MorlocError::Io(e));
    }
    unsafe { libc::close(dest_fd); }
    Ok(())
}

/// Finalise an OStream on close: replace the temp footer with a final
/// footer carrying the full sub-packet index, the StreamDiag block,
/// and the FOOTER_FINAL marker. fdatasync before returning so the
/// on-disk file is consistent before the fd is closed.
///
/// `@stream :: IFile a -> <IO> IStream a`: open a fresh ISTREAM handle
/// bound to the same path as the source IFile. Independent fd + mmap +
/// cursor so the two handles can be walked concurrently.
pub fn derive_istream(ifile_handle: i64) -> Result<i64, MorlocError> {
    shared_derive_istream(ifile_handle)
}

/// Implementation of `.[i] f` on an IFile handle. Returns an AbsPtr to
/// a freshly-allocated SHM block of `elem_schema.width` bytes holding
/// the materialized element (with any sub-allocations also in SHM).
pub fn ifile_bracket_index(handle: i64, index: i64) -> Result<AbsPtr, MorlocError> {
    shared_ifile_bracket_index(handle, index)
}

/// Inner body of `ifile_bracket_index`. Operates on the SHM slot's
/// process-local cache directly; `local.cache` and
/// `local.subpacket_elem_cum` persist across bracket accesses so the
/// decompression LRU and the cumulative element-count index survive.
fn ifile_bracket_index_against_slot(
    local: &mut ProcessLocalSlot,
    slot: &RegistrySlot,
    index: i64,
) -> Result<AbsPtr, MorlocError> {
    if slot.kind != MLC_KIND_IFILE {
        return Err(MorlocError::Other(format!(
            "bracket index on non-IFile handle (kind = {})",
            handle_kind_name(slot.kind),
        )));
    }
    let (sub_k, local_idx) = resolve_global_index(local, index)?;
    let src = cache_get_or_materialize(local, sub_k)?;
    let result = ifile_extract_element(&local.elem_schema, &src, local_idx);
    src.release();
    result
}

/// Acquire a source descriptor for sub-packet `sub_k`. Uncompressed
/// sub-packets are zero-copy (`File` variant); compressed sub-packets
/// are decompressed into SHM and cached (`Shm` variant) with the
/// `shincref` discipline so eviction of an entry that another caller
/// still holds is benign.
///
/// Caller drops their source descriptor via `SubpacketSrc::release()`.
fn cache_get_or_materialize(
    local: &mut ProcessLocalSlot,
    sub_k: usize,
) -> Result<SubpacketSrc, MorlocError> {
    // Hit path (compressed only -- uncompressed sub-packets bypass
    // the cache since the mmap'd region serves the walker directly).
    for ce in local.cache.entries.iter_mut() {
        if ce.subpacket_idx == sub_k as u64 {
            ce.clock_bit = 1;
            shm::shincref(ce.shm_packet)?;
            return Ok(SubpacketSrc::Shm { arr_base: ce.shm_packet });
        }
    }
    // Miss: materialise. For uncompressed sub-packets this returns a
    // File descriptor pointing at the mmap (no SHM allocation); for
    // compressed sub-packets a fresh SHM block (refcount = 1).
    let src = materialize_subpacket(local, sub_k)?;

    // Cache compressed (Shm) sub-packets; uncompressed (File) ones
    // don't need our cache -- the kernel pagecache handles them.
    if let SubpacketSrc::Shm { arr_base } = src {
        let size_bytes = unsafe { shm::shm_block_size(arr_base).unwrap_or(0) } as u64;
        cache_make_room_for(&mut local.cache, size_bytes);
        // Install: shincref so the cache owns one ref and the caller
        // owns the other. Both decrement independently on shfree.
        shm::shincref(arr_base)?;
        local.cache.entries.push(CacheEntry {
            subpacket_idx: sub_k as u64,
            shm_packet: arr_base,
            size_bytes,
            clock_bit: 1,
        });
        local.cache.current_bytes = local.cache.current_bytes.saturating_add(size_bytes);
    }
    Ok(src)
}

/// Make room in the cache for `needed` bytes via the clock-hand
/// approximate-LRU rule. Entries with `clock_bit == 0` are evictable;
/// the hand sweeps and clears clock bits as it goes.
fn cache_make_room_for(cache: &mut StreamCache, needed: u64) {
    if cache.capacity_bytes == 0 {
        // Cache disabled (MORLOC_IFILE_CACHE_BYTES=0): refuse to keep
        // anything. Caller still gets the materialised block but the
        // cache vector stays empty.
        // (Eviction loop is a no-op since we never insert when cap=0,
        //  but bail early to avoid spinning over `entries`.)
        return;
    }
    // Bound the eviction loop to two full passes so the clock-hand
    // sweep is guaranteed to terminate.
    let mut passes_remaining = 2 * cache.entries.len().max(1);
    while cache.current_bytes.saturating_add(needed) > cache.capacity_bytes
        && !cache.entries.is_empty()
        && passes_remaining > 0
    {
        passes_remaining -= 1;
        if cache.clock_hand >= cache.entries.len() {
            cache.clock_hand = 0;
        }
        let i = cache.clock_hand;
        if cache.entries[i].clock_bit == 0 {
            // Evict.
            let entry = cache.entries.swap_remove(i);
            cache.current_bytes = cache.current_bytes.saturating_sub(entry.size_bytes);
            let _ = shm::shfree(entry.shm_packet);
            // `swap_remove` filled slot `i` with the last entry; keep
            // the hand pointing at `i` so the next pass starts there.
        } else {
            // Second-chance: clear and advance.
            cache.entries[i].clock_bit = 0;
            cache.clock_hand += 1;
        }
    }
}

/// Given an SHM-resident Array, copy element `local_idx` into a fresh
/// SHM block sized to `elem_schema.width`. Sub-allocations (Array
/// data, String data, BigInt limbs, Optional inners) are deep-copied
/// into their own fresh SHM blocks.
fn ifile_extract_element(
    elem_schema: &Schema,
    src: &SubpacketSrc,
    local_idx: u64,
) -> Result<AbsPtr, MorlocError> {
    // The arr_base points at a `morloc::Array { size, data }` struct.
    // For File variant this points into the mmap'd region; for Shm
    // variant it points into the decompressed SHM copy.
    let arr_base = src.arr_base();
    let arr = unsafe { &*(arr_base as *const shm_types_crate::Array) };
    if local_idx >= arr.size as u64 {
        return Err(MorlocError::Other(format!(
            "local element index {} out of bounds for sub-packet of size {}",
            local_idx, arr.size,
        )));
    }
    let elem_width = elem_schema.width;
    let dst = shm::shcalloc(1, elem_width)?;
    // SAFETY: dst is a freshly-allocated SHM block of `elem_width`
    // bytes. The source-side resolver dispatches on the variant:
    // file-resident relptrs are plain payload-relative offsets;
    // SHM-resident relptrs go through shm::rel2abs.
    match *src {
        SubpacketSrc::File { payload_base, payload_len, .. } => {
            let resolver = make_file_resolver(payload_base, payload_len);
            let arr_data = resolver(arr.data)?;
            let elem_src = unsafe {
                (arr_data as *const u8).add(local_idx as usize * elem_width)
            };
            unsafe {
                voidstar::deep_copy_with(elem_src, dst, elem_schema, &resolver)?;
            }
        }
        SubpacketSrc::Shm { .. } => {
            let arr_data = shm::rel2abs(arr.data)?;
            let elem_src = unsafe {
                (arr_data as *const u8).add(local_idx as usize * elem_width)
            };
            unsafe {
                voidstar::deep_copy(elem_src, dst, elem_schema)?;
            }
        }
    }
    Ok(dst)
}

/// Implementation of `.[s:e:p] f` on an IFile handle. Each bound is
/// optional with Python semantics:
///   - step defaults to 1
///   - step > 0: start defaults to 0, stop defaults to len
///   - step < 0: start defaults to len-1, stop defaults to -1
///   - step == 0 is a runtime error
/// Negative bounds wrap from the end.
/// Returns an AbsPtr to a freshly-allocated SHM `Array {size, data}`
/// holding the materialized slice; `data` is a relptr to a fresh SHM
/// block of `n_out * elem_schema.width` bytes.
pub fn ifile_bracket_slice(
    handle: i64,
    start: Option<i64>,
    stop: Option<i64>,
    step: Option<i64>,
) -> Result<AbsPtr, MorlocError> {
    shared_ifile_bracket_slice_with_tail(handle, start, stop, step, &[])
}

/// Inner body of `ifile_bracket_slice_with_tail`. Operates on the
/// SHM slot's process-local cache directly; the decompression LRU
/// and cumulative element-count index live on `ProcessLocalSlot`
/// so they survive across slice accesses.
fn ifile_bracket_slice_against_slot(
    local: &mut ProcessLocalSlot,
    slot: &RegistrySlot,
    start: Option<i64>,
    stop: Option<i64>,
    step: Option<i64>,
    tail_steps: &[WalkStep],
) -> Result<AbsPtr, MorlocError> {
    let step_val = step.unwrap_or(1);
    if step_val == 0 {
        return Err(MorlocError::Other("Bracket slice step cannot be 0".into()));
    }

    struct SliceWork {
        // The static projection inside each element. (0, elem_schema)
        // when the tail is empty; (offset, target_schema) when a tail
        // is present.
        proj_offset: usize,
        proj_schema: Schema,
        // Source element width on disk -- always the full record's
        // width, since the slice plan addresses element slots in the
        // sub-packet array, not the projected field.
        elem_width: usize,
        // For each output position, which sub-packet and local index
        // it sources from.
        plan: Vec<(usize /*sub_k*/, u64 /*local_idx*/)>,
        // Located sub-packet sources, keyed by sub_k. Held for the
        // duration of the slice walk so a slice spanning many output
        // elements within the same sub-packet locates once. Released
        // (no-op for File, shfree for Shm) once the walk completes.
        materialised: std::collections::BTreeMap<usize, SubpacketSrc>,
    }

    let mut work: SliceWork = {
        if slot.kind != MLC_KIND_IFILE {
            return Err(MorlocError::Other(format!(
                "bracket slice on non-IFile handle (kind = {})",
                handle_kind_name(slot.kind),
            )));
        }
        ensure_elem_index(local)?;
        let cum = local
            .subpacket_elem_cum
            .as_ref()
            .expect("ensure_elem_index populates subpacket_elem_cum");
        let n: i64 = *cum.last().unwrap_or(&0u64) as i64;

        // Normalise bounds Python-style.
        let normalize = |v: i64| if v < 0 { v + n } else { v };
        let clamp = |v: i64| -> i64 {
            if step_val > 0 {
                v.max(0).min(n)
            } else {
                v.max(-1).min(n - 1)
            }
        };
        let start_norm: i64 = match start {
            Some(v) => clamp(normalize(v)),
            None => if step_val > 0 { 0 } else { n - 1 },
        };
        let stop_norm: i64 = match stop {
            Some(v) => clamp(normalize(v)),
            None => if step_val > 0 { n } else { -1 },
        };

        let mut plan: Vec<(usize, u64)> = Vec::new();
        let mut i = start_norm;
        if step_val > 0 {
            while i < stop_norm {
                let idx_u = i as u64;
                let upper = cum.partition_point(|&c| c <= idx_u);
                let sub_k = upper - 1;
                let local = idx_u - cum[sub_k];
                plan.push((sub_k, local));
                i += step_val;
            }
        } else {
            while i > stop_norm {
                let idx_u = i as u64;
                let upper = cum.partition_point(|&c| c <= idx_u);
                let sub_k = upper - 1;
                let local = idx_u - cum[sub_k];
                plan.push((sub_k, local));
                i += step_val;
            }
        }
        let (proj_offset, proj_schema) =
            navigate_static_field_offset(&local.elem_schema, tail_steps)?;
        SliceWork {
            proj_offset,
            proj_schema,
            elem_width: local.elem_schema.width,
            plan,
            materialised: std::collections::BTreeMap::new(),
        }
    };

    let n_out = work.plan.len();

    // Allocate the output Array structure (16 B) + the element-data buffer
    // (n_out * elem_width bytes). The Array struct is a separate SHM block;
    // its `data` relptr points to the buffer.
    let arr_ptr = shm::shcalloc(1, std::mem::size_of::<shm_types_crate::Array>())?;
    // Output element width is the PROJECTED schema's width (just the
    // tail field's width when chain-fused; the full record width when
    // the tail is empty). Source element width is the full record's
    // width on disk -- we offset into each record by proj_offset to
    // find the projected sub-field.
    let out_elem_width = work.proj_schema.width;
    if n_out == 0 {
        // Empty slice: data relptr = RELNULL.
        let arr = unsafe { &mut *(arr_ptr as *mut shm_types_crate::Array) };
        arr.size = 0;
        arr.data = morloc_runtime_types::shm_types::RELNULL;
        return Ok(arr_ptr);
    }
    // Contiguous bulk-copy fast path for the empty-tail case (`.[i:j]`).
    // When the slice is one contiguous run (step=1), lives entirely in
    // one File-variant sub-packet, and there is no per-element
    // projection, the slice collapses to three operations:
    //
    //   1) one shmalloc for the whole output
    //   2) one memcpy for the records range
    //   3) one memcpy for the sub-allocation range, with one
    //      `adjust_relptrs` pass to rebase the relptrs in the copied
    //      records.
    //
    // The structural invariant (voidstar emits sub-allocations in
    // element-DFS order, so the variable bytes for `records[i..j]` are
    // themselves contiguous) is what lets us replace the per-element
    // `deep_copy_with` loop -- and its N * sub-fields shmalloc traffic
    // -- with bulk byte copies.
    if tail_steps.is_empty()
        && work.plan.len() >= 2
        && step_val == 1
        && work.plan.iter().enumerate().all(|(k, &(sk, li))| {
            sk == work.plan[0].0 && li == (work.plan[0].1 + k as u64)
        })
    {
        let sub_k = work.plan[0].0;
        let i_local = work.plan[0].1 as usize;
        let j_local = i_local + n_out;

        // Materialise the sub-packet source once for the bulk copy.
        let src = cache_get_or_materialize(local, sub_k)?;
        let arr_base = src.arr_base();
        let arr = unsafe { &*(arr_base as *const shm_types_crate::Array) };
        let arr_size = arr.size as u64;
        if let SubpacketSrc::File { payload_base, payload_len, vol_idx_hint, .. } = src {
            let resolver = make_file_resolver(payload_base, payload_len);
            let arr_data = resolver(arr.data)?;
            let elem_schema = local.elem_schema.clone();
            let r = slice_bulk_copy_contiguous(
                i_local, j_local, arr_size, arr_data,
                payload_base, payload_len, vol_idx_hint,
                &elem_schema, work.elem_width, arr_ptr,
            );
            // `src` is a File variant: release is a no-op.
            src.release();
            if let Err(e) = r {
                let _ = shm::shfree(arr_ptr);
                return Err(e);
            }
            return Ok(arr_ptr);
        }
        // Shm variant (compressed sub-packet): fall through to the
        // generic per-element path below. The decompressed bytes
        // already live in SHM so per-element deep_copy is cheap
        // relative to the decompression we already paid for.
        src.release();
    }

    // Fast path: when the projected element is a String, do ONE
    // shmalloc for the whole output (n element headers + concatenated
    // string bytes) and cursor-pack each element. Replaces N
    // per-element shmemcpy calls with one shmalloc + N memcpys --
    // eliminates the ALLOC_MUTEX bottleneck for slice patterns like
    // `.[i:j].field-of-Str` that the chain-fusion path produces. For
    // anything else we keep the per-element deep_copy_with route
    // below, which already handles the full schema zoo.
    if work.proj_schema.serial_type == SerialType::String {
        let r = slice_bulk_pack_str(
            local, &mut work.plan, &mut work.materialised,
            work.elem_width, work.proj_offset, arr_ptr,
        );
        for (_, s) in std::mem::take(&mut work.materialised) {
            s.release();
        }
        if let Err(e) = r {
            let _ = shm::shfree(arr_ptr);
            return Err(e);
        }
        return Ok(arr_ptr);
    }
    let buf_ptr: AbsPtr = shm::shcalloc(n_out, out_elem_width)?;

    // For each output slot, ensure the source sub-packet is located, then
    // deep_copy the projected sub-field into the output buffer slot.
    // Sub-packets are cached by sub_k for the duration of the walk so a
    // slice spanning many elements within one sub-packet pays only one
    // location cost.
    for (out_i, &(sub_k, local_idx)) in work.plan.iter().enumerate() {
        if !work.materialised.contains_key(&sub_k) {
            let src = cache_get_or_materialize(local, sub_k)?;
            work.materialised.insert(sub_k, src);
        }
        let src = work.materialised.get(&sub_k).unwrap();
        let arr_base = src.arr_base();
        let arr = unsafe { &*(arr_base as *const shm_types_crate::Array) };
        if local_idx >= arr.size as u64 {
            // Defensive — should be unreachable given the
            // cum-element-count plan.
            for (_, s) in std::mem::take(&mut work.materialised) {
                s.release();
            }
            let _ = shm::shfree(buf_ptr);
            let _ = shm::shfree(arr_ptr);
            return Err(MorlocError::Other(format!(
                "slice plan: local index {} out of bounds for sub-packet {} (size {})",
                local_idx, sub_k, arr.size,
            )));
        }
        let dst = unsafe {
            (buf_ptr as *mut u8).add(out_i * out_elem_width)
        };
        // Dispatch on the source variant so the deep_copy uses the
        // right resolver. The hot path (uncompressed) reads source
        // bytes directly from the mmap; the compressed path goes
        // through the SHM-resident decompressed copy. The
        // `proj_offset` byte add hops over any record fields the
        // chain fusion is skipping.
        match *src {
            SubpacketSrc::File { payload_base, payload_len, .. } => {
                let resolver = make_file_resolver(payload_base, payload_len);
                let arr_data = resolver(arr.data)?;
                let elem_src = unsafe {
                    (arr_data as *const u8)
                        .add(local_idx as usize * work.elem_width)
                        .add(work.proj_offset)
                };
                unsafe {
                    voidstar::deep_copy_with(elem_src, dst, &work.proj_schema, &resolver)?;
                }
            }
            SubpacketSrc::Shm { .. } => {
                let arr_data = shm::rel2abs(arr.data)?;
                let elem_src = unsafe {
                    (arr_data as *const u8)
                        .add(local_idx as usize * work.elem_width)
                        .add(work.proj_offset)
                };
                unsafe {
                    voidstar::deep_copy(elem_src, dst, &work.proj_schema)?;
                }
            }
        }
    }

    // Release source descriptors and fill in the output Array struct.
    // The cache (for Shm variants) keeps its own refs; File variants are
    // no-op releases.
    for (_, s) in std::mem::take(&mut work.materialised) {
        s.release();
    }
    let buf_relptr = shm::abs2rel(buf_ptr)?;
    let arr = unsafe { &mut *(arr_ptr as *mut shm_types_crate::Array) };
    arr.size = n_out;
    arr.data = buf_relptr;
    Ok(arr_ptr)
}

/// Bulk-copy a contiguous slice on a single (uncompressed) sub-packet.
///
/// The voidstar layout of `[T]` places `N` fixed-stride records first,
/// followed by their sub-allocations in element-DFS order. For a
/// contiguous slice `[i, j)` that means:
///
///   * the records we keep are one contiguous block
///     `records[i..j] = arr_data + i*elem_width .. arr_data + j*elem_width`;
///   * the sub-allocations they reference are also one contiguous block
///     `[first_suballoc(records[i]) .. first_suballoc(records[j]))`
///     (or `.. payload_len` when `j == arr.size`).
///
/// So the slice is **one shmalloc + two memcpys + one linear relptr
/// rewrite**, with no per-element allocator traffic. For 200 K records
/// of `(Str, [u8], [u8])` this replaces ~600 K `shmemcpy` calls (each
/// taking `ALLOC_MUTEX`) with three calls total.
fn slice_bulk_copy_contiguous(
    i: usize,
    j: usize,
    arr_size: u64,
    arr_data: AbsPtr,
    payload_base: AbsPtr,
    payload_len: u64,
    vol_idx_hint: u16,
    elem_schema: &Schema,
    elem_width: usize,
    arr_ptr: AbsPtr,
) -> Result<(), MorlocError> {
    let n_out = j - i;
    let records_src = unsafe { (arr_data as *const u8).add(i * elem_width) };

    // Fixed-width records (no relptrs anywhere): one memcpy, no rebase.
    let var_start_offset = match first_suballoc_offset(records_src as AbsPtr, elem_schema)? {
        Some(o) => o,
        None => {
            let buf_ptr = shm::shmalloc(n_out * elem_width)?;
            unsafe {
                std::ptr::copy_nonoverlapping(records_src, buf_ptr, n_out * elem_width);
            }
            let buf_relptr = shm::abs2rel(buf_ptr)?;
            let arr_out = unsafe { &mut *(arr_ptr as *mut shm_types_crate::Array) };
            arr_out.size = n_out;
            arr_out.data = buf_relptr;
            return Ok(());
        }
    };

    // Variable range end: records[j]'s first sub-allocation if present,
    // else the payload end. Voidstar DFS layout guarantees records[i..j]'s
    // sub-allocations all lie in [var_start_offset, var_end_offset).
    let var_end_offset = if (j as u64) < arr_size {
        let records_j = unsafe { (arr_data as *const u8).add(j * elem_width) };
        match first_suballoc_offset(records_j as AbsPtr, elem_schema)? {
            Some(o) => o,
            None => payload_len as usize,
        }
    } else {
        payload_len as usize
    };
    if var_end_offset < var_start_offset {
        return Err(MorlocError::Other(format!(
            "slice bulk copy: var_end_offset {} < var_start_offset {} \
             (voidstar DFS invariant broken or schema mismatch)",
            var_end_offset, var_start_offset,
        )));
    }
    let var_size = var_end_offset - var_start_offset;
    let records_size = n_out * elem_width;
    let total_size = records_size + var_size;

    // Only hint the kernel on slices larger than a few pages -- madvise
    // is a syscall and rounds to page boundaries, so it's net-negative
    // on tiny slices.
    const MADV_THRESHOLD: usize = 64 * 1024;
    unsafe {
        if records_size >= MADV_THRESHOLD {
            libc::madvise(
                records_src as *mut libc::c_void,
                records_size,
                libc::MADV_WILLNEED,
            );
        }
        if var_size >= MADV_THRESHOLD {
            libc::madvise(
                (payload_base as *mut u8).add(var_start_offset) as *mut libc::c_void,
                var_size,
                libc::MADV_WILLNEED,
            );
        }
    }

    // shmalloc (not shcalloc): the two memcpys below cover every byte.
    let buf_ptr = shm::shmalloc(total_size)?;
    unsafe {
        std::ptr::copy_nonoverlapping(records_src, buf_ptr, records_size);
        let var_src = (payload_base as *const u8).add(var_start_offset);
        let var_dst = (buf_ptr as *mut u8).add(records_size);
        std::ptr::copy_nonoverlapping(var_src, var_dst, var_size);
    }

    // Producer wrote relptrs as `(vol_idx_hint << 48) | payload_offset`.
    // The same delta trick the load path uses (cli.rs:rebase_voidstar_in_shm)
    // cancels the hint and rebases the offset in one `adjust_relptrs` walk.
    let target_var_start_rel = shm::abs2rel(
        unsafe { (buf_ptr as *mut u8).add(records_size) } as AbsPtr,
    )?;
    let producer_base = morloc_runtime_types::shm_types::encode_relptr(
        vol_idx_hint as usize,
        var_start_offset,
    );
    let delta = (target_var_start_rel as i64).wrapping_sub(producer_base as i64) as RelPtr;
    for k in 0..n_out {
        let rec_ptr = unsafe { (buf_ptr as *mut u8).add(k * elem_width) as AbsPtr };
        voidstar::adjust_relptrs(rec_ptr, elem_schema, delta)?;
    }

    let buf_relptr = shm::abs2rel(buf_ptr)?;
    let arr_out = unsafe { &mut *(arr_ptr as *mut shm_types_crate::Array) };
    arr_out.size = n_out;
    arr_out.data = buf_relptr;
    Ok(())
}

/// Bulk-pack a String-valued slice into one SHM block.
///
/// Layout in the output buffer:
///   [Array{size,data} * n_out] [concatenated u8 bytes]
///
/// Each Array's `data` relptr addresses into the byte tail of the
/// same buffer. One shmalloc for the whole slice, regardless of N.
/// Replaces the per-element shmemcpy in `deep_copy_with`'s String
/// arm under the ALLOC_MUTEX -- the dominant cost for record slices
/// that the chain-fusion path projects down to a String field.
///
/// Caller is responsible for releasing `work.materialised` and
/// freeing `arr_ptr` on the error path.
fn slice_bulk_pack_str(
    local: &mut ProcessLocalSlot,
    plan: &mut Vec<(usize, u64)>,
    materialised: &mut std::collections::BTreeMap<usize, SubpacketSrc>,
    elem_width: usize,
    proj_offset: usize,
    arr_ptr: AbsPtr,
) -> Result<(), MorlocError> {
    let n_out = plan.len();
    let hdr_size = std::mem::size_of::<shm_types_crate::Array>();

    // Cache the resolved sub-packet context across consecutive elements
    // sharing the same `sub_k`. For a contiguous slice this collapses
    // 200 K resolver/array lookups to a single resolve at sub-packet
    // boundaries. For a fragmented slice (rare, requires step > 1
    // across sub-packet boundaries) the cache misses fall back to the
    // recompute path.
    struct SrcCtx {
        sub_k: Option<usize>,
        // For File: payload_base of the sub-packet's mmap region and
        // pre-resolved pointer to the sub-packet's element-data array.
        // For Shm: just the pre-resolved arr_data pointer.
        arr_data: AbsPtr,
        arr_size: u64,
        // File-only: payload_base + payload_len for relptr resolution
        // on the per-element string-data lookups.
        file_payload: Option<(AbsPtr, u64)>,
    }
    let mut ctx = SrcCtx {
        sub_k: None,
        arr_data: std::ptr::null::<u8>() as AbsPtr,
        arr_size: 0,
        file_payload: None,
    };
    // Pass 1: walk the plan, resolve every source Array<u8> (the Str
    // wire form), record (src_data_ptr, len) for each element, and
    // sum up the total tail size.
    let mut srcs: Vec<(AbsPtr, usize)> = Vec::with_capacity(n_out);
    let mut total_tail: usize = 0;
    for &(sub_k, local_idx) in plan.iter() {
        if ctx.sub_k != Some(sub_k) {
            if !materialised.contains_key(&sub_k) {
                let src = cache_get_or_materialize(local, sub_k)?;
                materialised.insert(sub_k, src);
            }
            let src = materialised.get(&sub_k).unwrap();
            let arr_base = src.arr_base();
            let arr = unsafe { &*(arr_base as *const shm_types_crate::Array) };
            let (arr_data, file_payload) = match *src {
                SubpacketSrc::File { payload_base, payload_len, .. } => {
                    let resolver = make_file_resolver(payload_base, payload_len);
                    let arr_data = resolver(arr.data)?;
                    // Tell the kernel to prefault this sub-packet's
                    // payload range. For a single-pass walk that
                    // touches the records section + the string tail,
                    // turning on bulk readahead here cuts the cost
                    // from N synchronous single-page faults to
                    // payload_len / readahead-window pages.
                    unsafe {
                        libc::madvise(
                            payload_base as *mut libc::c_void,
                            payload_len as usize,
                            libc::MADV_WILLNEED,
                        );
                    }
                    (arr_data, Some((payload_base, payload_len)))
                }
                SubpacketSrc::Shm { .. } => {
                    let arr_data = shm::rel2abs(arr.data)?;
                    (arr_data, None)
                }
            };
            ctx = SrcCtx {
                sub_k: Some(sub_k),
                arr_data,
                arr_size: arr.size as u64,
                file_payload,
            };
        }
        if local_idx >= ctx.arr_size {
            return Err(MorlocError::Other(format!(
                "slice plan: local index {} out of bounds for sub-packet {} (size {})",
                local_idx, sub_k, ctx.arr_size,
            )));
        }
        // Land on slot 0 (or whatever proj_offset says) of the element.
        let elem_src = unsafe {
            (ctx.arr_data as *const u8)
                .add(local_idx as usize * elem_width)
                .add(proj_offset)
        };
        let str_arr = unsafe { &*(elem_src as *const shm_types_crate::Array) };
        let str_len = str_arr.size;
        let src_str_data: AbsPtr = if str_len == 0 {
            std::ptr::null::<u8>() as AbsPtr
        } else {
            match ctx.file_payload {
                Some((payload_base, payload_len)) => {
                    let resolver = make_file_resolver(payload_base, payload_len);
                    resolver(str_arr.data)?
                }
                None => shm::rel2abs(str_arr.data)?,
            }
        };
        srcs.push((src_str_data, str_len));
        total_tail += str_len;
    }

    // One bare shmalloc for the whole slice -- Pass 2 writes every
    // byte (headers in front, string tail behind), so a zero-fill
    // would be 100 % wasted work.
    let buf_size = n_out * hdr_size + total_tail;
    let buf_ptr = shm::shmalloc(buf_size)?;
    let hdr_start = buf_ptr;
    let mut tail_cursor = unsafe { (buf_ptr as *mut u8).add(n_out * hdr_size) };
    // abs2rel adds a constant (volume index + base) to a pointer
    // offset; the offset between two same-buffer pointers is just
    // `ptr.offset_from`. Compute the cursor's relptr once via abs2rel
    // and bump by string length per element so we skip 200 K
    // abs2rel calls.
    let mut tail_rel = shm::abs2rel(tail_cursor)?;
    for (k, &(src_data, len)) in srcs.iter().enumerate() {
        let hdr_slot = unsafe { (hdr_start as *mut shm_types_crate::Array).add(k) };
        if len == 0 {
            unsafe {
                (*hdr_slot).size = 0;
                (*hdr_slot).data = morloc_runtime_types::shm_types::RELNULL;
            }
            continue;
        }
        unsafe {
            (*hdr_slot).size = len;
            (*hdr_slot).data = tail_rel;
            std::ptr::copy_nonoverlapping(src_data, tail_cursor, len);
            tail_cursor = tail_cursor.add(len);
        }
        tail_rel += len as RelPtr;
    }
    let buf_relptr = shm::abs2rel(buf_ptr)?;
    let arr = unsafe { &mut *(arr_ptr as *mut shm_types_crate::Array) };
    arr.size = n_out;
    arr.data = buf_relptr;
    Ok(())
}

/// Parse a `.<step>.<step>...` suffix consisting purely of
/// Field/Key steps. Returns `None` if the suffix contains any
/// bracket, group, or other non-field step -- the caller falls back
/// to the general walker in that case. Used to detect chain-fusable
/// tails after a root-level `.[:]`.
fn parse_field_only_tail(suffix: &str) -> Result<Option<Vec<WalkStep>>, MorlocError> {
    let bytes = suffix.as_bytes();
    let mut out = Vec::new();
    let mut pos = 0;
    while pos < bytes.len() {
        if bytes[pos] != b'.' {
            return Err(MorlocError::Other(format!(
                "tail walk: expected '.' at byte {}, found {:?}",
                pos, bytes[pos] as char
            )));
        }
        pos += 1;
        if pos >= bytes.len() {
            return Err(MorlocError::Other(
                "tail walk: trailing '.' with no step".into(),
            ));
        }
        // Anything that isn't a digit or an identifier byte means a
        // structural step (group / bracket) -- signal fall-through.
        match bytes[pos] {
            b'0'..=b'9' => {
                let start = pos;
                while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                    pos += 1;
                }
                let n: usize = std::str::from_utf8(&bytes[start..pos])
                    .unwrap()
                    .parse()
                    .map_err(|e: std::num::ParseIntError| MorlocError::Other(format!(
                        "tail walk: bad field index '{}': {}",
                        std::str::from_utf8(&bytes[start..pos]).unwrap_or("?"), e
                    )))?;
                out.push(WalkStep::Field(FieldStep::Index(n)));
            }
            b'_' | b'A'..=b'Z' | b'a'..=b'z' => {
                let start = pos;
                while pos < bytes.len()
                    && (bytes[pos] == b'_'
                        || bytes[pos].is_ascii_alphanumeric())
                {
                    pos += 1;
                }
                let name = std::str::from_utf8(&bytes[start..pos])
                    .map_err(|_| MorlocError::Other(
                        "tail walk: non-UTF-8 key name".into()
                    ))?
                    .to_string();
                out.push(WalkStep::Field(FieldStep::Key(name)));
            }
            _ => return Ok(None),
        }
    }
    Ok(Some(out))
}

/// Find the offset of the first sub-allocation referenced by a single
/// element, in the source payload's relptr coordinate space. Walks
/// the element's schema in DFS order and returns the first
/// variable-length leaf's `data` relptr offset. Returns `None` when
/// the element has no sub-allocations (pure fixed-width primitive).
///
/// Used by the contiguous-slice bulk copy to identify the start of
/// the variable-bytes region that the slice's records reference.
/// Voidstar layout guarantees that sub-allocations are written in
/// element-DFS order, so the first relptr of `records[i]` is also
/// the lowest offset touched by `records[i]`.
fn first_suballoc_offset(
    elem_ptr: AbsPtr,
    elem_schema: &Schema,
) -> Result<Option<usize>, MorlocError> {
    unsafe {
        match elem_schema.serial_type {
            SerialType::String | SerialType::IFile | SerialType::Array => {
                let arr = &*(elem_ptr as *const shm_types_crate::Array);
                if arr.size == 0 || arr.data == shm::RELNULL {
                    return Ok(None);
                }
                Ok(Some(relptr_offset(arr.data)))
            }
            SerialType::Tuple | SerialType::Map => {
                for i in 0..elem_schema.parameters.len() {
                    let field_ptr = (elem_ptr as *const u8).add(elem_schema.offsets[i]) as AbsPtr;
                    if let Some(o) = first_suballoc_offset(field_ptr, &elem_schema.parameters[i])? {
                        return Ok(Some(o));
                    }
                }
                Ok(None)
            }
            SerialType::Optional => {
                let relptr = *(elem_ptr as *const RelPtr);
                if relptr == shm::RELNULL || schema_is_absent_optional(elem_schema) {
                    Ok(None)
                } else {
                    Ok(Some(relptr_offset(relptr)))
                }
            }
            SerialType::Int => {
                // Inline BigInt: [size, value_or_relptr]. Only > 1 limb
                // makes the second field a relptr.
                let size = *(elem_ptr as *const usize);
                if size > 1 {
                    let relptr = *(elem_ptr.add(std::mem::size_of::<usize>()) as *const RelPtr);
                    Ok(Some(relptr_offset(relptr)))
                } else {
                    Ok(None)
                }
            }
            // Fixed-width primitives, Nil, Recur (treated as absent
            // for this estimator), Table (not slice-relevant): no
            // sub-allocations to chase.
            _ => Ok(None),
        }
    }
}

#[inline]
fn schema_is_absent_optional(_s: &Schema) -> bool {
    // Placeholder for any future "phantom Optional" marker. Currently
    // Optional schemas always carry an inner parameter.
    false
}

/// Walk a sequence of Field/Key steps statically on the schema and
/// return the cumulative byte offset and the schema at the end of the
/// chain. Used by chain-fused bracket-slice to compute, once per
/// slice, where inside each record the projected sub-field lives.
///
/// Rejects any non-Field tail step. BracketIndex/BracketSlice in a
/// tail would need runtime args + per-element walking; we route those
/// through the general walker (`ifile_general`) instead.
fn navigate_static_field_offset(
    start: &Schema,
    steps: &[WalkStep],
) -> Result<(usize, Schema), MorlocError> {
    let mut off = 0usize;
    let mut cur = start.clone();
    for (i, step) in steps.iter().enumerate() {
        let f = match step {
            WalkStep::Field(f) => f,
            other => return Err(MorlocError::Other(format!(
                "navigate_static_field_offset: only Field/Key tail steps are supported, \
                 got {:?} at step {}", other, i
            ))),
        };
        let field_idx = match f {
            FieldStep::Index(i) => *i,
            FieldStep::Key(k) => cur.keys.iter().position(|sk| sk == k).ok_or_else(|| {
                MorlocError::Other(format!(
                    "static-field walk: key '{}' not found at step {} (keys = {:?})",
                    k, i, cur.keys
                ))
            })?,
        };
        if field_idx >= cur.parameters.len() {
            return Err(MorlocError::Other(format!(
                "static-field walk: field index {} out of range at step {} (schema has {} params)",
                field_idx, i, cur.parameters.len()
            )));
        }
        if field_idx >= cur.offsets.len() {
            return Err(MorlocError::Other(format!(
                "static-field walk: schema offsets[{}] missing at step {}",
                field_idx, i
            )));
        }
        off += cur.offsets[field_idx];
        cur = cur.parameters[field_idx].clone();
    }
    Ok((off, cur))
}

#[derive(Debug, Clone)]
enum FieldStep {
    Index(usize),
    Key(String),
}

#[derive(Debug)]
enum WalkStep {
    Field(FieldStep),
    /// Bracket-index step: consumes 1 runtime arg from the DFS-ordered
    /// args list. Result schema is the element type of the surrounding
    /// Array. Legal inside group children and as a leaf in any chain.
    BracketIndex,
    /// Bracket-slice step: consumes 3 runtime args (start, stop, step).
    /// Result schema is the surrounding Array type (length may change).
    /// Bracket-slice steps are TERMINAL within a chain: the path
    /// "...[:]X" with anything after is rejected.
    BracketSlice,
    /// Multi-field group: each child is its own sub-walk that
    /// operates at the same position. Result is a fresh tuple of the
    /// sibling values, in source order. Nested groups are supported
    /// (child chains may themselves contain Group steps). Empty
    /// groups (`.()`) materialise a Nil/unit value at this position.
    Group(Vec<Vec<WalkStep>>),
}

/// Parse a walk path into a chain of `WalkStep`s.
///
/// Grammar:
/// ```text
///   path        ::= step path | ε
///   step        ::= "." segment
///   segment     ::= int | name | "[]" | "[:]" | group
///   group       ::= "(" path { ";" path } ")"
/// ```
///
/// Invariants enforced here:
///
/// * A group is terminal in its parent chain -- nothing may follow.
///   `.(.x;.y).0` is rejected.
/// * `BracketSlice` is terminal in its chain (it returns a list; a
///   structural step after a list of values is morloc's `IntrMap`
///   territory, not a single walker call).
/// * A group with exactly one child is rejected -- a single-sibling
///   group is the non-grouped chain and the encoder normalises it.
/// * Empty groups `.()` are permitted and materialise a Nil/unit value
///   (an empty tuple) at the current position.
/// * Empty *child chains* (e.g. `.(.x;)`) are still rejected -- they
///   are unambiguously malformed and not the same as an empty group.
fn parse_walk_path(path: &str) -> Result<Vec<WalkStep>, MorlocError> {
    let bytes = path.as_bytes();
    let (steps, end) = parse_walk_seq(bytes, 0, /*depth=*/0)?;
    if end != bytes.len() {
        return Err(MorlocError::Other(format!(
            "walk path '{}' has trailing input at byte {}", path, end
        )));
    }
    if steps.is_empty() {
        return Err(MorlocError::Other(format!(
            "walk path '{}' contains no steps", path
        )));
    }
    Ok(steps)
}

/// Parse zero or more walk steps starting at byte `pos`. Stops at
/// end-of-input or at the first ';' / ')' belonging to an enclosing
/// group. Enforces "Group is terminal" and "BracketSlice is terminal"
/// within the produced chain.
fn parse_walk_seq(
    bytes: &[u8],
    mut pos: usize,
    depth: usize,
) -> Result<(Vec<WalkStep>, usize), MorlocError> {
    // Hard depth cap: protects against pathological deeply-nested
    // paths constructed by a misbehaving codegen.
    const MAX_DEPTH: usize = 64;
    if depth > MAX_DEPTH {
        return Err(MorlocError::Other(
            "walk path too deeply nested".into(),
        ));
    }
    let mut out: Vec<WalkStep> = Vec::new();
    while pos < bytes.len() && bytes[pos] != b';' && bytes[pos] != b')' {
        // Terminal-step enforcement: nothing may follow a Group or a
        // BracketSlice within the same chain. (BracketIndex is NOT
        // terminal -- a chain may continue navigating into the
        // materialised element.)
        if let Some(last) = out.last() {
            match last {
                WalkStep::Group(_) => return Err(MorlocError::Other(
                    "walk path: groups are terminal -- nothing may follow `.(.x;.y)`".into()
                )),
                WalkStep::BracketSlice => return Err(MorlocError::Other(
                    "walk path: bracket slices are terminal -- nothing may follow `.[:]`".into()
                )),
                _ => {}
            }
        }
        if bytes[pos] != b'.' {
            return Err(MorlocError::Other(format!(
                "walk path: expected '.' at byte {}, found {:?}",
                pos, bytes[pos] as char
            )));
        }
        pos += 1;
        if pos >= bytes.len() {
            return Err(MorlocError::Other(
                "walk path: trailing '.' with no step".into(),
            ));
        }
        match bytes[pos] {
            b'(' => {
                pos += 1;
                let mut chains: Vec<Vec<WalkStep>> = Vec::new();
                // Special-case the empty group ".()": consume the
                // closing ')' immediately and emit Group with zero
                // children. This materialises as a Nil/unit value.
                if pos < bytes.len() && bytes[pos] == b')' {
                    pos += 1;
                    out.push(WalkStep::Group(chains));
                    continue;
                }
                loop {
                    let (chain, next) = parse_walk_seq(bytes, pos, depth + 1)?;
                    if chain.is_empty() {
                        return Err(MorlocError::Other(
                            "walk path: empty child chain in group (e.g. '.(.x;)'); for an \
                             empty tuple use '.()' instead".into(),
                        ));
                    }
                    chains.push(chain);
                    pos = next;
                    if pos >= bytes.len() {
                        return Err(MorlocError::Other(
                            "walk path: unclosed group".into(),
                        ));
                    }
                    match bytes[pos] {
                        b';' => { pos += 1; continue; }
                        b')' => { pos += 1; break; }
                        c => return Err(MorlocError::Other(format!(
                            "walk path: expected ';' or ')' inside group, got {:?}",
                            c as char
                        ))),
                    }
                }
                if chains.len() == 1 {
                    // Single-child groups are illegal: morloc has no
                    // 1-tuple, and the encoder collapses single-child
                    // groups into the non-grouped chain before
                    // emission. Anything reaching us here is a
                    // malformed encoding.
                    return Err(MorlocError::Other(
                        "walk path: single-child groups are illegal -- there is no 1-tuple; \
                         the encoder should have normalised `.(.x)` to `.x`".into(),
                    ));
                }
                out.push(WalkStep::Group(chains));
            }
            b'[' => {
                pos += 1;
                // Distinguish "[]" (bracket-index) from "[:]" (bracket-slice).
                match bytes.get(pos).copied() {
                    Some(b']') => {
                        pos += 1;
                        out.push(WalkStep::BracketIndex);
                    }
                    Some(b':') => {
                        pos += 1;
                        match bytes.get(pos).copied() {
                            Some(b']') => {
                                pos += 1;
                                out.push(WalkStep::BracketSlice);
                            }
                            other => return Err(MorlocError::Other(format!(
                                "walk path: expected ']' after '[:', got {:?}",
                                other.map(|c| c as char)
                            ))),
                        }
                    }
                    other => return Err(MorlocError::Other(format!(
                        "walk path: expected ']' or ':' after '[', got {:?}",
                        other.map(|c| c as char)
                    ))),
                }
            }
            _ => {
                // Field step: read a maximal run of [A-Za-z0-9_].
                let start = pos;
                while pos < bytes.len() {
                    let c = bytes[pos];
                    if c == b'.' || c == b';' || c == b')' { break; }
                    if !(c.is_ascii_alphanumeric() || c == b'_') {
                        return Err(MorlocError::Other(format!(
                            "walk path: illegal character {:?} at byte {}",
                            c as char, pos
                        )));
                    }
                    pos += 1;
                }
                if pos == start {
                    return Err(MorlocError::Other(format!(
                        "walk path: empty step at byte {}", start
                    )));
                }
                let seg = std::str::from_utf8(&bytes[start..pos])
                    .map_err(|e| MorlocError::Other(format!(
                        "walk path: non-UTF8 segment ({})", e
                    )))?;
                if let Ok(i) = seg.parse::<usize>() {
                    out.push(WalkStep::Field(FieldStep::Index(i)));
                } else {
                    out.push(WalkStep::Field(FieldStep::Key(seg.to_string())));
                }
            }
        }
    }
    Ok((out, pos))
}

/// Stateful cursor over the DFS-ordered runtime args list. Each
/// bracket step consumes 1 (index) or 3 (slice) args from the front;
/// every other step consumes none.
struct ArgsCursor<'a> {
    args: &'a [crate::intrinsics::IFileWalkArg],
    pos: usize,
}

impl<'a> ArgsCursor<'a> {
    fn new(args: &'a [crate::intrinsics::IFileWalkArg]) -> Self {
        Self { args, pos: 0 }
    }
    fn next_one(&mut self) -> Result<&'a crate::intrinsics::IFileWalkArg, MorlocError> {
        if self.pos >= self.args.len() {
            return Err(MorlocError::Other(
                "walk: ran out of runtime args (bracket step expected an index/bound)".into(),
            ));
        }
        let r = &self.args[self.pos];
        self.pos += 1;
        Ok(r)
    }
    fn next_three(&mut self) -> Result<
        (Option<i64>, Option<i64>, Option<i64>),
        MorlocError,
    > {
        let a = *self.next_one()?;
        let b = *self.next_one()?;
        let c = *self.next_one()?;
        let opt = |x: &crate::intrinsics::IFileWalkArg| {
            if x.has != 0 { Some(x.value) } else { None }
        };
        Ok((opt(&a), opt(&b), opt(&c)))
    }
    fn remaining(&self) -> usize { self.args.len() - self.pos }
}

/// Walk steps from the start of the value and deep-copy the resulting
/// value into a fresh SHM block. Entry point for non-bracket-only
/// paths (the BracketIndexOnly / BracketSliceOnly fast paths bypass
/// this).
fn walk_into_fresh(
    value_schema: &Schema,
    src: &SubpacketSrc,
    steps: &[WalkStep],
    args: &[crate::intrinsics::IFileWalkArg],
) -> Result<AbsPtr, MorlocError> {
    // Infer the result schema for the whole walk so we can allocate
    // the output once. infer_chain_schema is a pure static walk over
    // the input schema -- no disk reads.
    let result_schema = infer_chain_schema(value_schema, steps)?;
    let dst = shm::shcalloc(1, result_schema.width)?;
    let mut cursor = ArgsCursor::new(args);
    if let Err(e) = walk_into(src, src.arr_base(), value_schema,
                              steps, &mut cursor, dst, &result_schema) {
        let _ = shm::shfree(dst);
        return Err(e);
    }
    if cursor.remaining() != 0 {
        let _ = shm::shfree(dst);
        return Err(MorlocError::Other(format!(
            "walk: {} unconsumed runtime arg(s) -- pattern/args arity mismatch",
            cursor.remaining()
        )));
    }
    Ok(dst)
}

/// Walk `steps` from the current source position into a pre-allocated
/// output slot. Used both at the top level (`walk_into_fresh`
/// allocates and calls in) and recursively for group children
/// (`materialize_group` lays out the tuple and recurses per-child).
fn walk_into(
    src: &SubpacketSrc,
    cur_ptr: AbsPtr,
    cur_schema: &Schema,
    steps: &[WalkStep],
    args: &mut ArgsCursor<'_>,
    out: AbsPtr,
    out_schema: &Schema,
) -> Result<(), MorlocError> {
    // Pre-walk through any leading Field steps -- they are pure
    // pointer arithmetic and never consume args.
    let (mut cur_ptr, mut cur_schema_ref, mut tail) =
        navigate_field_prefix(cur_schema, cur_ptr, steps)?;

    loop {
        match tail.first() {
            None => {
                // Reached the end of the chain. Deep-copy current
                // value into out.
                debug_assert_eq!(out_schema.width, cur_schema_ref.width);
                return deep_copy_one(src, cur_ptr, cur_schema_ref, out);
            }
            Some(WalkStep::Field(_)) => unreachable!("navigate_field_prefix consumed all fields"),
            Some(WalkStep::BracketIndex) => {
                let idx_arg = args.next_one()?;
                if idx_arg.has == 0 {
                    return Err(MorlocError::Other(
                        "walk: bracket-index requires a present index (got None)".into(),
                    ));
                }
                // Read array header, bounds-check, advance cursor to
                // the element's source position, recurse on remainder.
                if cur_schema_ref.serial_type != SerialType::Array {
                    return Err(MorlocError::Other(format!(
                        "walk: bracket-index requires an Array, got {:?}",
                        cur_schema_ref.serial_type
                    )));
                }
                let (elem_ptr, elem_schema) =
                    locate_array_element(src, cur_ptr, cur_schema_ref, idx_arg.value)?;
                tail = &tail[1..];
                // Field-prefix-walk over any further field steps in
                // the chain; if `tail` is now Group/BracketSlice/...
                // the outer match handles it.
                let (np, ns, nt) = navigate_field_prefix(elem_schema, elem_ptr, tail)?;
                cur_ptr = np;
                cur_schema_ref = ns;
                tail = nt;
            }
            Some(WalkStep::BracketSlice) => {
                // BracketSlice is terminal by parse-time check; any
                // tail after it would have been rejected. The result
                // here writes directly into `out`.
                let (s, e, p) = args.next_three()?;
                if cur_schema_ref.serial_type != SerialType::Array {
                    return Err(MorlocError::Other(format!(
                        "walk: bracket-slice requires an Array, got {:?}",
                        cur_schema_ref.serial_type
                    )));
                }
                debug_assert_eq!(tail.len(), 1, "BracketSlice should be terminal");
                return inline_bracket_slice(src, cur_ptr, cur_schema_ref, s, e, p, out);
            }
            Some(WalkStep::Group(chains)) => {
                // Group is terminal by parse-time check; result writes
                // directly into `out`.
                debug_assert_eq!(tail.len(), 1, "Group should be terminal");
                return materialize_group(src, cur_ptr, cur_schema_ref,
                                          chains, args, out, out_schema);
            }
        }
    }
}

/// Consume Field steps from the front of `steps`. Stops at end-of-
/// list or at the first non-Field step. Pure pointer arithmetic; no
/// disk reads, no args.
fn navigate_field_prefix<'a>(
    start_schema: &'a Schema,
    start_ptr: AbsPtr,
    steps: &'a [WalkStep],
) -> Result<(AbsPtr, &'a Schema, &'a [WalkStep]), MorlocError> {
    let mut cur_ptr = start_ptr;
    let mut cur_schema = start_schema;
    let mut i = 0;
    while i < steps.len() {
        match &steps[i] {
            WalkStep::Field(f) => {
                let (np, ns) = step_field(cur_ptr, cur_schema, f, i)?;
                cur_ptr = np;
                cur_schema = ns;
                i += 1;
            }
            _ => break,
        }
    }
    Ok((cur_ptr, cur_schema, &steps[i..]))
}

fn step_field<'a>(
    cur_ptr: AbsPtr,
    cur_schema: &'a Schema,
    f: &FieldStep,
    step_i: usize,
) -> Result<(AbsPtr, &'a Schema), MorlocError> {
    let field_idx = match f {
        FieldStep::Index(i) => *i,
        FieldStep::Key(k) => cur_schema.keys.iter().position(|sk| sk == k).ok_or_else(|| {
            MorlocError::Other(format!(
                "field '{}' not found at step {} (schema keys = {:?})",
                k, step_i, cur_schema.keys
            ))
        })?,
    };
    if field_idx >= cur_schema.parameters.len() {
        return Err(MorlocError::Other(format!(
            "field index {} out of range at step {} (schema has {} params)",
            field_idx, step_i, cur_schema.parameters.len()
        )));
    }
    if field_idx >= cur_schema.offsets.len() {
        return Err(MorlocError::Other(format!(
            "schema offsets[{}] missing at step {}", field_idx, step_i
        )));
    }
    let off = cur_schema.offsets[field_idx];
    let next_ptr = unsafe { (cur_ptr as *const u8).add(off) as AbsPtr };
    Ok((next_ptr, &cur_schema.parameters[field_idx]))
}

/// Resolve `arr_ptr` as an Array struct, bounds-check `idx` against
/// `arr.size` (with Python negative-index semantics), and return the
/// element's source pointer + schema. Source-side only -- no copy.
fn locate_array_element<'a>(
    src: &SubpacketSrc,
    arr_ptr: AbsPtr,
    arr_schema: &'a Schema,
    idx: i64,
) -> Result<(AbsPtr, &'a Schema), MorlocError> {
    debug_assert_eq!(arr_schema.serial_type, SerialType::Array);
    if arr_schema.parameters.is_empty() {
        return Err(MorlocError::Other("walk: Array schema missing element type".into()));
    }
    let arr = unsafe { &*(arr_ptr as *const shm_types_crate::Array) };
    let n = arr.size as i64;
    let actual = if idx < 0 { idx + n } else { idx };
    if actual < 0 || actual >= n {
        return Err(MorlocError::Other(format!(
            "walk: bracket-index {} out of bounds (size {})", idx, n
        )));
    }
    let elem_schema = &arr_schema.parameters[0];
    let data_abs = match src {
        SubpacketSrc::File { payload_base, payload_len, .. } => {
            let resolver = make_file_resolver(*payload_base, *payload_len);
            resolver(arr.data)?
        }
        SubpacketSrc::Shm { .. } => shm::rel2abs(arr.data)?,
    };
    let elem_ptr = unsafe {
        (data_abs as *const u8).add(actual as usize * elem_schema.width) as AbsPtr
    };
    Ok((elem_ptr, elem_schema))
}

/// Apply BracketSlice on an in-file Array. Writes the resulting
/// Array { size, RelPtr data } header into `out`; allocates a fresh
/// SHM block for the element bank and deep-copies each selected
/// element into it. Mirrors the semantics of `ifile_bracket_slice`
/// for the file's root array, but operates at an arbitrary
/// (ptr, schema) -- used inside group children whose chain ends in
/// `.[:]`.
fn inline_bracket_slice(
    src: &SubpacketSrc,
    arr_ptr: AbsPtr,
    arr_schema: &Schema,
    start: Option<i64>,
    stop: Option<i64>,
    step: Option<i64>,
    out: AbsPtr,
) -> Result<(), MorlocError> {
    debug_assert_eq!(arr_schema.serial_type, SerialType::Array);
    if arr_schema.parameters.is_empty() {
        return Err(MorlocError::Other("walk: Array schema missing element type".into()));
    }
    let arr = unsafe { &*(arr_ptr as *const shm_types_crate::Array) };
    let n = arr.size as i64;
    let elem_schema = &arr_schema.parameters[0];
    let elem_w = elem_schema.width;
    let indices = python_slice_indices(n, start, stop, step)?;
    let n_out = indices.len();
    // Allocate the element bank. shcalloc handles n_out == 0 by
    // returning a sentinel; we still need to size the output header.
    let buf_ptr = if n_out == 0 {
        std::ptr::null::<u8>() as AbsPtr
    } else {
        shm::shcalloc(n_out, elem_w)?
    };
    let data_abs = match src {
        SubpacketSrc::File { payload_base, payload_len, .. } => {
            let resolver = make_file_resolver(*payload_base, *payload_len);
            resolver(arr.data)?
        }
        SubpacketSrc::Shm { .. } => shm::rel2abs(arr.data)?,
    };
    for (k, &i) in indices.iter().enumerate() {
        let elem_src = unsafe { (data_abs as *const u8).add(i as usize * elem_w) as AbsPtr };
        let elem_dst = unsafe { (buf_ptr as *mut u8).add(k * elem_w) as AbsPtr };
        if let Err(e) = deep_copy_one(src, elem_src, elem_schema, elem_dst) {
            if !buf_ptr.is_null() { let _ = shm::shfree(buf_ptr); }
            return Err(e);
        }
    }
    // Write the Array { size, RelPtr data } header into out.
    let data_rel = if buf_ptr.is_null() {
        // Empty slice: write a relptr that resolves to a 0-byte
        // region. RELNULL is conventionally used to encode "no data".
        shm::RELNULL
    } else {
        shm::abs2rel(buf_ptr)?
    };
    let header = shm_types_crate::Array { size: n_out, data: data_rel };
    unsafe {
        std::ptr::copy_nonoverlapping(
            &header as *const shm_types_crate::Array as *const u8,
            out as *mut u8,
            std::mem::size_of::<shm_types_crate::Array>(),
        );
    }
    Ok(())
}

/// Python slice index expansion: produces the actual list of source
/// indices for a (start, stop, step) triple against length n.
/// Mirrors the semantics used by `ifile_bracket_slice` so element
/// selection is consistent between root-level and inline bracket
/// slices.
fn python_slice_indices(
    n: i64,
    start: Option<i64>,
    stop: Option<i64>,
    step: Option<i64>,
) -> Result<Vec<i64>, MorlocError> {
    let step = step.unwrap_or(1);
    if step == 0 {
        return Err(MorlocError::Other("walk: slice step must be non-zero".into()));
    }
    let (lo_default, hi_default) =
        if step > 0 { (0, n) } else { (n - 1, -1) };
    let clamp = |v: i64| -> i64 {
        let v = if v < 0 { v + n } else { v };
        if step > 0 {
            v.clamp(0, n)
        } else {
            v.clamp(-1, n - 1)
        }
    };
    let lo = match start { Some(s) => clamp(s), None => lo_default };
    let hi = match stop  { Some(s) => clamp(s), None => hi_default };
    let mut out = Vec::new();
    if step > 0 {
        let mut i = lo;
        while i < hi {
            out.push(i);
            i += step;
        }
    } else {
        let mut i = lo;
        while i > hi {
            out.push(i);
            i += step;
        }
    }
    Ok(out)
}

/// Materialise a group of sibling sub-walks into a tuple at `out`.
/// Each child writes directly into its slot.
fn materialize_group(
    src: &SubpacketSrc,
    cur_ptr: AbsPtr,
    cur_schema: &Schema,
    chains: &[Vec<WalkStep>],
    args: &mut ArgsCursor<'_>,
    out: AbsPtr,
    out_schema: &Schema,
) -> Result<(), MorlocError> {
    // Empty group: nothing to do. The output slot is `out_schema`'s
    // width (zero for Nil/empty-tuple); the caller's shcalloc has
    // already zeroed it.
    if chains.is_empty() {
        return Ok(());
    }
    debug_assert_eq!(out_schema.serial_type, SerialType::Tuple);
    debug_assert_eq!(out_schema.parameters.len(), chains.len());
    debug_assert_eq!(out_schema.offsets.len(), chains.len());
    for (i, chain) in chains.iter().enumerate() {
        let slot_off = out_schema.offsets[i];
        let slot_schema = &out_schema.parameters[i];
        let slot_dst = unsafe { (out as *mut u8).add(slot_off) as AbsPtr };
        walk_into(src, cur_ptr, cur_schema, chain, args, slot_dst, slot_schema)?;
    }
    Ok(())
}

/// Compute the static schema produced by walking `chain` from
/// `start`. Pure static walk -- no disk reads. Used to size the
/// output buffer up front and to lay out tuple slots for groups.
fn infer_chain_schema(
    start: &Schema,
    chain: &[WalkStep],
) -> Result<Schema, MorlocError> {
    let mut cur = start.clone();
    for (step_i, step) in chain.iter().enumerate() {
        match step {
            WalkStep::Field(f) => {
                let field_idx = match f {
                    FieldStep::Index(i) => *i,
                    FieldStep::Key(k) => cur.keys.iter().position(|sk| sk == k).ok_or_else(|| {
                        MorlocError::Other(format!(
                            "field '{}' not found at step {} during schema inference",
                            k, step_i
                        ))
                    })?,
                };
                if field_idx >= cur.parameters.len() {
                    return Err(MorlocError::Other(format!(
                        "field index {} out of range at step {} during schema inference",
                        field_idx, step_i
                    )));
                }
                cur = cur.parameters[field_idx].clone();
            }
            WalkStep::BracketIndex => {
                if cur.serial_type != SerialType::Array {
                    return Err(MorlocError::Other(format!(
                        "bracket-index at step {} requires an Array, got {:?}",
                        step_i, cur.serial_type
                    )));
                }
                if cur.parameters.is_empty() {
                    return Err(MorlocError::Other(
                        "Array schema missing element type".into(),
                    ));
                }
                cur = cur.parameters[0].clone();
            }
            WalkStep::BracketSlice => {
                if cur.serial_type != SerialType::Array {
                    return Err(MorlocError::Other(format!(
                        "bracket-slice at step {} requires an Array, got {:?}",
                        step_i, cur.serial_type
                    )));
                }
                // Slice preserves the Array shape; the length is a
                // runtime fact carried by the result's Array header.
                // cur stays unchanged.
            }
            WalkStep::Group(inner_chains) => {
                if inner_chains.is_empty() {
                    // Empty group -> unit / Nil schema.
                    return Ok(Schema::primitive(SerialType::Nil));
                }
                let mut child_schemas = Vec::with_capacity(inner_chains.len());
                for c in inner_chains {
                    child_schemas.push(infer_chain_schema(&cur, c)?);
                }
                let (width, offsets) = tuple_layout(&child_schemas);
                return Ok(Schema {
                    serial_type: SerialType::Tuple,
                    size: child_schemas.len(),
                    width,
                    offsets,
                    hint: None,
                    parameters: child_schemas,
                    keys: Vec::new(),
                    name: None,
                });
            }
        }
    }
    Ok(cur)
}

/// Voidstar tuple layout: each field sits at the natural alignment of
/// its type, total width is rounded up to the max field alignment.
/// Mirrors `morloc_runtime_types::schema::calculate_tuple_layout`
/// (which is private to that crate).
fn tuple_layout(params: &[Schema]) -> (usize, Vec<usize>) {
    let mut offsets = Vec::with_capacity(params.len());
    let mut offset: usize = 0;
    let mut max_align: usize = 1;
    for p in params {
        let a = p.alignment();
        if a > max_align { max_align = a; }
        offset = (offset + a - 1) & !(a - 1);
        offsets.push(offset);
        offset += p.width;
    }
    let width = (offset + max_align - 1) & !(max_align - 1);
    (width, offsets)
}

/// Run `deep_copy_with` (or the SHM-resolver `deep_copy` for cached
/// Shm sources) with the right resolver for the source variant.
fn deep_copy_one(
    src: &SubpacketSrc,
    cur_ptr: AbsPtr,
    cur_schema: &Schema,
    dst: AbsPtr,
) -> Result<(), MorlocError> {
    match *src {
        SubpacketSrc::File { payload_base, payload_len, .. } => {
            let resolver = make_file_resolver(payload_base, payload_len);
            unsafe { voidstar::deep_copy_with(cur_ptr, dst, cur_schema, &resolver) }
        }
        SubpacketSrc::Shm { .. } => {
            unsafe { voidstar::deep_copy(cur_ptr, dst, cur_schema) }
        }
    }
}

/// `write_all` for a raw fd: loops past EINTR / partial writes.
fn write_all_fd(fd: i32, mut buf: &[u8]) -> Result<(), MorlocError> {
    while !buf.is_empty() {
        let n = unsafe {
            libc::write(fd, buf.as_ptr() as *const libc::c_void, buf.len())
        };
        if n < 0 {
            let e = std::io::Error::last_os_error();
            if e.kind() == std::io::ErrorKind::Interrupted { continue; }
            return Err(MorlocError::Io(e));
        }
        if n == 0 {
            return Err(MorlocError::Other("write_all_fd: zero-byte write".into()));
        }
        buf = &buf[n as usize..];
    }
    Ok(())
}

/// `pwrite_all`: positional write that loops past EINTR / partials.
fn pwrite_all_fd(fd: i32, mut buf: &[u8], mut offset: u64) -> Result<(), MorlocError> {
    while !buf.is_empty() {
        let n = unsafe {
            libc::pwrite(
                fd,
                buf.as_ptr() as *const libc::c_void,
                buf.len(),
                offset as libc::off_t,
            )
        };
        if n < 0 {
            let e = std::io::Error::last_os_error();
            if e.kind() == std::io::ErrorKind::Interrupted { continue; }
            return Err(MorlocError::Io(e));
        }
        if n == 0 {
            return Err(MorlocError::Other("pwrite_all_fd: zero-byte write".into()));
        }
        buf = &buf[n as usize..];
        offset += n as u64;
    }
    Ok(())
}

/// Copy `count` bytes from `src_fd[src_off..src_off+count]` to
/// `dest_fd[dest_off..]` using `sendfile()` for zero-copy via the
/// kernel pagecache. Loops past EINTR + partial transfers; advances
/// dest_fd's file offset by the bytes written.
///
/// Used by `@concat` to glue stream files together without crossing
/// the data through userspace.
fn sendfile_range(
    dest_fd: i32, src_fd: i32,
    src_off: u64, count: u64, dest_off: u64,
) -> Result<(), MorlocError> {
    // sendfile writes at dest_fd's current file offset; seek to where
    // we want this slice to land. lseek is also needed when the caller
    // mixed pwrite and sendfile on the same fd, since pwrite leaves the
    // file offset untouched.
    let s = unsafe {
        libc::lseek(dest_fd, dest_off as libc::off_t, libc::SEEK_SET)
    };
    if s < 0 {
        return Err(MorlocError::Io(std::io::Error::last_os_error()));
    }
    let mut offset = src_off as libc::off_t;
    let mut remaining = count;
    while remaining > 0 {
        let n = unsafe {
            libc::sendfile(dest_fd, src_fd, &mut offset, remaining as libc::size_t)
        };
        if n < 0 {
            let e = std::io::Error::last_os_error();
            if e.kind() == std::io::ErrorKind::Interrupted { continue; }
            return Err(MorlocError::Io(e));
        }
        if n == 0 {
            return Err(MorlocError::Other(
                "sendfile_range: zero-byte transfer (source truncated?)".into(),
            ));
        }
        remaining = remaining.saturating_sub(n as u64);
    }
    Ok(())
}

/// Dispatch entry point used by the FFI shim in `intrinsics.rs`.
///
/// OSTREAM is intentionally NOT dispatched here -- the writer needs the
/// element schema in hand at open time, which the typed entry point
/// `mlc_open_ostream(schema_str, path)` provides. Routing OSTREAM
/// through `mlc_open(path, kind)` would create a Nil-schema header on
/// disk, breaking the receiving IStream/IFile reader's schema parse.
pub fn open_dispatch(path: &str, kind: u8) -> Result<i64, MorlocError> {
    match kind {
        MLC_KIND_IFILE => shared_open_ifile(path),
        MLC_KIND_ISTREAM => shared_open_istream(path),
        MLC_KIND_OSTREAM => Err(MorlocError::Other(
            "mlc_open: OSTREAM requires the typed entry point \
             mlc_open_ostream(schema_str, path) -- the codegen wires \
             this automatically for `@open :: <IO> (OStream T)`".into(),
        )),
        _ => Err(MorlocError::Other(format!(
            "mlc_open: unknown handle kind {} ({})",
            kind, handle_kind_name(kind),
        ))),
    }
}

// ── Tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Verify the walk-path parser accepts every shape the Haskell
    /// encoder can produce (and the design's full grammar including
    /// brackets-inside-groups, which the codegen does not yet emit
    /// but the runtime must support per the design).
    #[test]
    fn parse_walk_path_shapes() {
        let cases: &[(&str, usize)] = &[
            (".1",                1),
            (".foo",              1),
            (".1.2.3",            3),
            (".foo.bar",          2),
            (".[]",               1),  // top-level bracket-index (parser accepts; dispatch
                                       // still routes the bare ".[]" through the fast path)
            (".[:]",              1),  // top-level bracket-slice
            (".1.[]",             2),  // field prefix + bracket-index
            (".1.[:]",            2),  // field prefix + bracket-slice
            (".[].x",             2),  // bracket-index NOT terminal: chain continues
            (".(.x;.y)",          1),  // top-level group, 2 siblings
            (".0.(.x;.y)",        2),  // prefix + group
            (".0.(.x;.y;.z)",     2),  // group with 3 siblings
            (".(.0.x;.1.y)",      1),  // group children with their own prefixes
            (".(.0;.(.x;.y))",    1),  // nested group
            (".(.0.[];.1)",       1),  // bracket-index inside a group child
            (".(.0.[:];.1.[])",   1),  // mixed: slice in one child, index in another
            (".()",               1),  // empty group -> unit
        ];
        for (path, expected_len) in cases {
            let steps = parse_walk_path(path)
                .unwrap_or_else(|e| panic!("parse_walk_path({:?}) failed: {:?}", path, e));
            assert_eq!(steps.len(), *expected_len,
                       "step count mismatch for {:?}: got {:?}", path, steps);
        }
    }

    #[test]
    fn parse_walk_path_rejects_malformed() {
        let bad: &[&str] = &[
            "",                  // empty path
            ".",                 // trailing dot
            ".1.",               // trailing dot after step
            ".(.x)",             // single-child group (illegal: no 1-tuple)
            ".(.x;)",            // empty trailing child in group
            ".(.x;.y",           // unclosed group
            ".(.x;.y)x",         // missing dot after group close
            ".(.x;.y).0",        // step after group (groups are terminal)
            ".[:].x",            // step after slice (slices are terminal)
            ".[a]",              // bracket with non-empty contents
            ".[",                // unclosed bracket
        ];
        for path in bad {
            assert!(parse_walk_path(path).is_err(),
                    "expected parse failure for {:?}", path);
        }
    }

    #[test]
    fn python_slice_indices_basic() {
        // start=None, stop=None, step=None -> full sequence
        assert_eq!(python_slice_indices(5, None, None, None).unwrap(),
                   vec![0,1,2,3,4]);
        // explicit forward slice
        assert_eq!(python_slice_indices(5, Some(1), Some(4), None).unwrap(),
                   vec![1,2,3]);
        // step > 1
        assert_eq!(python_slice_indices(10, Some(0), Some(10), Some(2)).unwrap(),
                   vec![0,2,4,6,8]);
        // negative step (reverse)
        assert_eq!(python_slice_indices(5, None, None, Some(-1)).unwrap(),
                   vec![4,3,2,1,0]);
        // step 0 is an error
        assert!(python_slice_indices(5, None, None, Some(0)).is_err());
        // negative start clamped
        assert_eq!(python_slice_indices(5, Some(-2), None, None).unwrap(),
                   vec![3,4]);
    }

    /// Build a minimal valid STREAM_PACKET file (no sub-packets) and
    /// confirm open_ifile + close_handle round-trip. Sub-packet walking
    /// + cache + pattern eval are exercised in task #13's integration
    /// tests.
    #[test]
    fn open_close_empty_stream_file() {
        crate::init_test_shm();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("empty.idx");

        // Build a stream file consisting of only the stream header
        // block (no sub-packets, no footer). Minimal schema = uint32;
        // the actual element type doesn't matter for this open-test
        // since we never walk any sub-packet.
        let schema = crate::schema::Schema::primitive(
            crate::schema::SerialType::Uint32,
        );
        let header = morloc_runtime_types::packet::make_stream_header_block(&schema);
        std::fs::write(&path, &header).unwrap();

        let handle = shared_open_ifile(path.to_str().unwrap()).unwrap();
        assert!(handle > 0);
        let kind = shared_handle_kind(handle).unwrap();
        assert_eq!(kind, MLC_KIND_IFILE);
        shared_close_handle(handle).unwrap();

        // Re-close is an error.
        assert!(shared_close_handle(handle).is_err());

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn handle_encoding_roundtrip() {
        // pack_handle's generation field is 47 bits (bit 63 stays clear so
        // the i64 sentinel domain is reserved for errors), so the test
        // value must fit that bound.
        let g_in: u64 = 0x7EAD_BEEF_CAFE;
        let s_in: usize = 0x1234;
        let h = pack_handle(g_in, s_in);
        let (g, s) = unpack_handle(h);
        assert_eq!(g, g_in);
        assert_eq!(s, s_in);
    }

    #[test]
    fn unknown_kind_is_rejected() {
        let res = open_dispatch("/dev/null", 99);
        assert!(res.is_err());
    }

    // ── End-to-end: build a STREAM_PACKET file with two voidstar sub-
    // packets of [Int64] and exercise the full bracket-index path. This
    // verifies the wire format, registry, sub-packet locator, element-
    // count indexing, mmap walker, and SHM materialisation all work
    // together end-to-end without an OStream writer.

    use morloc_runtime_types::packet::{
        make_stream_header_block, make_final_footer_packet,
        StreamDiag as TStreamDiag,
        METADATA_HEADER_MAGIC, METADATA_TYPE_SCHEMA_STRING,
        PACKET_FORMAT_VOIDSTAR as VOIDSTAR,
    };
    use morloc_runtime_types::schema::{
        schema_to_string, Schema as TSchema, SerialType as TSerialType,
    };
    use morloc_runtime_types::shm_types::{encode_relptr, Array as ShmArray};

    /// Build a single voidstar DATA sub-packet for `[i64]` containing
    /// `values`. The payload layout is:
    ///   - 16 B Array { size: usize, data: RelPtr -> offset 16 }
    ///   - 8 * len bytes of contiguous i64 element data
    /// Metadata block carries the element-type schema string "ai8"
    /// (Array of Sint64).
    fn build_int_voidstar_subpacket(values: &[i64]) -> Vec<u8> {
        // Element schema = Array Sint64. The metadata's schema string
        // describes the sub-packet's payload type, which is [a].
        let mut elem_inner = TSchema::primitive(TSerialType::Sint64);
        elem_inner.width = 8;
        let array_schema = TSchema {
            serial_type: TSerialType::Array,
            size: 1,
            width: std::mem::size_of::<ShmArray>(),
            offsets: Vec::new(),
            hint: None,
            parameters: vec![elem_inner],
            keys: Vec::new(),
            name: None,
        };
        let schema_str = schema_to_string(&array_schema);
        let schema_bytes = schema_str.as_bytes();
        let schema_len = schema_bytes.len() + 1; // null terminator

        let meta_header_size = 8usize; // mmh+type+size
        let raw_meta_len = meta_header_size + schema_len;
        let padded_meta_len = (raw_meta_len + 31) / 32 * 32;

        // Payload: 16 B Array struct + 8 B * N elements
        let payload_size = 16 + 8 * values.len();
        let mut payload = vec![0u8; payload_size];

        // size (usize / u64 on 64-bit)
        payload[0..8].copy_from_slice(&(values.len() as u64).to_le_bytes());
        // data: buffer-relative relptr pointing at offset 16
        let data_relptr = encode_relptr(0, 16);
        payload[8..16].copy_from_slice(&(data_relptr as i64).to_le_bytes());
        // element data
        for (i, &v) in values.iter().enumerate() {
            let off = 16 + 8 * i;
            payload[off..off + 8].copy_from_slice(&v.to_le_bytes());
        }

        // Header: DATA + MESG + VOIDSTAR. The IFile walker rejects
        // non-voidstar sub-packets, so set format explicitly.
        let mut hdr = PacketHeader::data_mesg(VOIDSTAR, payload_size as u64);
        hdr.offset = padded_meta_len as u32;
        let mut packet = hdr.to_bytes().to_vec();

        // Metadata block: schema string.
        let mut meta = vec![0u8; padded_meta_len];
        meta[0..3].copy_from_slice(&METADATA_HEADER_MAGIC);
        meta[3] = METADATA_TYPE_SCHEMA_STRING;
        meta[4..8].copy_from_slice(&(schema_len as u32).to_le_bytes());
        meta[8..8 + schema_bytes.len()].copy_from_slice(schema_bytes);
        // null terminator at meta[8 + schema_bytes.len()] is already 0.

        packet.extend_from_slice(&meta);
        packet.extend_from_slice(&payload);
        packet
    }

    fn build_stream_file(
        elem_schema: &TSchema,
        sub_values: &[&[i64]],
    ) -> Vec<u8> {
        let mut out = make_stream_header_block(elem_schema);
        let mut subpacket_offsets: Vec<u64> = Vec::with_capacity(sub_values.len());
        let mut element_count: u64 = 0;
        for values in sub_values {
            subpacket_offsets.push(out.len() as u64);
            let sub = build_int_voidstar_subpacket(values);
            out.extend_from_slice(&sub);
            element_count += values.len() as u64;
        }
        // Build a final footer carrying StreamDiag (with element_count
        // for `length f`) and the full sub-packet index.
        let mut diag = TStreamDiag::new();
        diag.subpacket_count = sub_values.len() as u64;
        diag.element_count = element_count;
        let footer = make_final_footer_packet(&diag, &subpacket_offsets);
        out.extend_from_slice(&footer);
        out
    }

    #[test]
    fn ifile_bracket_index_end_to_end() {
        crate::init_test_shm();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("ints.idx");

        // Stream-level schema is the ELEMENT type a = Sint64.
        let elem_schema = TSchema::primitive(TSerialType::Sint64);
        let sub_a: &[i64] = &[10, 20, 30];
        let sub_b: &[i64] = &[40, 50];
        let file_bytes = build_stream_file(&elem_schema, &[sub_a, sub_b]);
        std::fs::write(&path, &file_bytes).unwrap();

        let handle = open_ifile(path.to_str().unwrap()).unwrap();
        assert!(handle > 0);
        assert_eq!(handle_kind(handle).unwrap(), MLC_KIND_IFILE);
        // Footer carries the element count, so `length f` is free.
        assert_eq!(handle_length(handle).unwrap(), 5);

        // Verify random access into both sub-packets.
        let cases: &[(i64, i64)] = &[
            (0, 10),
            (1, 20),
            (2, 30),  // last element of sub-packet 0
            (3, 40),  // first element of sub-packet 1 (crosses boundary)
            (4, 50),
            (-1, 50), // python-style wraparound
            (-5, 10),
        ];
        for &(idx, expected) in cases {
            let ptr = ifile_bracket_index(handle, idx)
                .expect(&format!("bracket_index({}) failed", idx));
            // SAFETY: ptr is an SHM block of sizeof(i64) bytes
            // holding the materialized element.
            let value = unsafe { *(ptr as *const i64) };
            assert_eq!(value, expected, "index {} gave {}, want {}", idx, value, expected);
            shm::shfree(ptr).unwrap();
        }

        // Out-of-bounds error cleanly (no SIGBUS, no panic).
        assert!(ifile_bracket_index(handle, 5).is_err());
        assert!(ifile_bracket_index(handle, -6).is_err());

        close_handle(handle).unwrap();
        // Double-close is a clean error.
        assert!(close_handle(handle).is_err());

        let _ = std::fs::remove_file(&path);
    }

    /// Verify bracket-slice over a single sub-packet, spanning sub-
    /// packets, negative indices, and step.
    #[test]
    fn ifile_bracket_slice_end_to_end() {
        crate::init_test_shm();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}_slice", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("ints-slice.idx");

        let elem_schema = TSchema::primitive(TSerialType::Sint64);
        let sub_a: &[i64] = &[10, 20, 30, 40];
        let sub_b: &[i64] = &[50, 60, 70];
        let file_bytes = build_stream_file(&elem_schema, &[sub_a, sub_b]);
        std::fs::write(&path, &file_bytes).unwrap();

        let handle = open_ifile(path.to_str().unwrap()).unwrap();
        assert_eq!(handle_length(handle).unwrap(), 7);

        // Read a slice and verify its contents.
        fn read_slice(handle: i64, start: Option<i64>, stop: Option<i64>, step: Option<i64>)
            -> Vec<i64>
        {
            let ptr = ifile_bracket_slice(handle, start, stop, step).unwrap();
            let arr = unsafe { &*(ptr as *const shm_types_crate::Array) };
            let size = arr.size;
            let out = if size == 0 {
                Vec::new()
            } else {
                let data_abs = shm::rel2abs(arr.data).unwrap();
                let mut v = Vec::with_capacity(size);
                for i in 0..size {
                    let p = unsafe { (data_abs as *const i64).add(i) };
                    v.push(unsafe { *p });
                }
                // Free the element-data block and the Array struct
                // (both fresh allocations from ifile_bracket_slice).
                shm::shfree(data_abs).unwrap();
                v
            };
            shm::shfree(ptr).unwrap();
            out
        }

        // Pure within sub-packet 0: [10, 20, 30, 40][1:3] = [20, 30]
        assert_eq!(read_slice(handle, Some(1), Some(3), None), vec![20, 30]);
        // Spans sub-packet boundary: [10..70][2:6] = [30, 40, 50, 60]
        assert_eq!(read_slice(handle, Some(2), Some(6), None), vec![30, 40, 50, 60]);
        // Full slice w/ defaults: [10, 20, ..., 70]
        assert_eq!(read_slice(handle, None, None, None),
                   vec![10, 20, 30, 40, 50, 60, 70]);
        // Step > 1: every other element
        assert_eq!(read_slice(handle, Some(0), Some(7), Some(2)),
                   vec![10, 30, 50, 70]);
        // Negative step: reverse
        assert_eq!(read_slice(handle, None, None, Some(-1)),
                   vec![70, 60, 50, 40, 30, 20, 10]);
        // Negative bounds: [-3:] = last 3
        assert_eq!(read_slice(handle, Some(-3), None, None), vec![50, 60, 70]);
        // Empty slice: stop <= start with positive step
        assert_eq!(read_slice(handle, Some(3), Some(3), None), Vec::<i64>::new());

        close_handle(handle).unwrap();
        let _ = std::fs::remove_file(&path);
    }

    /// Step zero is a clean runtime error, not a panic.
    #[test]
    fn ifile_bracket_slice_step_zero_is_error() {
        crate::init_test_shm();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}_step0", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("ints-step0.idx");
        let elem_schema = TSchema::primitive(TSerialType::Sint64);
        let bytes = build_stream_file(&elem_schema, &[&[1i64, 2, 3]]);
        std::fs::write(&path, &bytes).unwrap();
        let handle = open_ifile(path.to_str().unwrap()).unwrap();
        assert!(ifile_bracket_slice(handle, None, None, Some(0)).is_err());
        close_handle(handle).unwrap();
        let _ = std::fs::remove_file(&path);
    }

    /// Hammer the cache: many repeated reads should keep cache size
    /// bounded and produce correct values. With a tiny capacity, the
    /// clock-hand eviction is exercised.
    #[test]
    fn ifile_cache_eviction_correct() {
        // Override the cache cap for this test by writing to the env
        // BEFORE the registry is lazy-initialised. (The registry uses
        // a once-init pattern keyed on the static REGISTRY; we run
        // each test in its own process, so this is safe-ish but
        // best-effort -- the lazy init may have already run.)
        std::env::set_var("MORLOC_IFILE_CACHE_BYTES", "256");
        crate::init_test_shm();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}_cache", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("ints-cache.idx");
        let elem_schema = TSchema::primitive(TSerialType::Sint64);
        // 4 sub-packets of 4 elements each. With a 256-byte cap and
        // ~80 bytes per cached sub-packet, only a few fit at a time.
        let sub_a: Vec<i64> = (0..4).collect();
        let sub_b: Vec<i64> = (4..8).collect();
        let sub_c: Vec<i64> = (8..12).collect();
        let sub_d: Vec<i64> = (12..16).collect();
        let bytes = build_stream_file(
            &elem_schema,
            &[&sub_a, &sub_b, &sub_c, &sub_d],
        );
        std::fs::write(&path, &bytes).unwrap();
        let handle = open_ifile(path.to_str().unwrap()).unwrap();
        assert_eq!(handle_length(handle).unwrap(), 16);

        // Read every element twice; cache hits + misses should both
        // produce the right values.
        for round in 0..2 {
            for i in 0..16i64 {
                let ptr = ifile_bracket_index(handle, i).unwrap();
                let v = unsafe { *(ptr as *const i64) };
                assert_eq!(v, i, "round {} idx {} got {}", round, i, v);
                shm::shfree(ptr).unwrap();
            }
        }
        close_handle(handle).unwrap();
        let _ = std::fs::remove_file(&path);
        std::env::remove_var("MORLOC_IFILE_CACHE_BYTES");
    }

    /// DATA_PACKET file: a single voidstar packet (no STREAM header,
    /// no footer). The IFile dispatch treats the whole file as one
    /// sub-packet and exercises bracket index + slice + length over
    /// the file's payload via the file resolver (zero-copy).
    #[test]
    fn ifile_data_packet_zero_copy() {
        crate::init_test_shm();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}_data", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("ints-data.idx");

        // build_int_voidstar_subpacket produces a self-contained
        // DATA_PACKET (header + metadata + voidstar [Int64] payload).
        // Writing it directly to disk yields a valid DATA_PACKET file.
        let values: Vec<i64> = (100..110).collect();
        let bytes = build_int_voidstar_subpacket(&values);
        std::fs::write(&path, &bytes).unwrap();

        let handle = open_ifile(path.to_str().unwrap()).unwrap();
        assert_eq!(handle_length(handle).unwrap(), values.len() as u64);

        // Bracket index across the whole array.
        for (i, &expected) in values.iter().enumerate() {
            let ptr = ifile_bracket_index(handle, i as i64).unwrap();
            let v = unsafe { *(ptr as *const i64) };
            assert_eq!(v, expected, "DATA_PACKET .[{}] = {} expected {}", i, v, expected);
            shm::shfree(ptr).unwrap();
        }
        // Negative index wraps.
        let ptr = ifile_bracket_index(handle, -1).unwrap();
        assert_eq!(unsafe { *(ptr as *const i64) }, 109);
        shm::shfree(ptr).unwrap();

        // Slice within the file.
        let ptr = ifile_bracket_slice(handle, Some(2), Some(5), None).unwrap();
        let arr = unsafe { &*(ptr as *const shm_types_crate::Array) };
        assert_eq!(arr.size, 3);
        let data = shm::rel2abs(arr.data).unwrap();
        for i in 0..3 {
            let v = unsafe { *((data as *const i64).add(i)) };
            assert_eq!(v, 102 + i as i64);
        }
        shm::shfree(data).unwrap();
        shm::shfree(ptr).unwrap();

        // Out-of-bounds errors cleanly.
        assert!(ifile_bracket_index(handle, 20).is_err());

        close_handle(handle).unwrap();
        let _ = std::fs::remove_file(&path);
    }

    /// Compressed DATA_PACKET files are rejected at open time: we
    /// cannot serve random access without decompressing the whole
    /// payload, which defeats IFile's purpose.
    #[test]
    fn ifile_compressed_data_packet_rejected() {
        crate::init_test_shm();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}_zstd", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("ints-zstd.idx");

        // Build an uncompressed DATA_PACKET and then flip the
        // compression byte to PACKET_COMPRESSION_ZSTD. The file is
        // structurally syntactic (the open path only checks the
        // header's compression byte before rejecting); we don't need
        // to actually compress the payload.
        let mut bytes = build_int_voidstar_subpacket(&[1, 2, 3]);
        // Bytes 0..32 are the header. CommandData.compression is at
        // offset 8 + 3 = 11 (command_union starts at byte 8, then
        // cmd_type:u8, source:u8, format:u8, compression:u8).
        bytes[11] = PACKET_COMPRESSION_ZSTD;
        std::fs::write(&path, &bytes).unwrap();

        let err = open_ifile(path.to_str().unwrap()).unwrap_err();
        let msg = format!("{:?}", err);
        assert!(msg.contains("compressed"), "error should mention compression: {}", msg);
        let _ = std::fs::remove_file(&path);
    }

    /// Same as the end-to-end test, but the file lacks a final footer
    /// (writer crashed). Verifies forward-scan recovery rebuilds the
    /// sub-packet index. `length f` returns 0 in this path today
    /// (element-count only sourced from the StreamDiag); bracket_index
    /// still works because we read each sub-packet's Array.size during
    /// the lazy element-count build.
    #[test]
    fn ifile_recovery_without_footer() {
        crate::init_test_shm();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}_norec", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("ints-no-footer.idx");

        let elem_schema = TSchema::primitive(TSerialType::Sint64);
        let mut bytes = make_stream_header_block(&elem_schema);
        let sub_a: &[i64] = &[7, 8, 9];
        let sub_b: &[i64] = &[11];
        for vs in &[sub_a, sub_b] {
            bytes.extend_from_slice(&build_int_voidstar_subpacket(vs));
        }
        // No footer, no EOF tail -- simulates a crashed writer.
        std::fs::write(&path, &bytes).unwrap();

        let handle = open_ifile(path.to_str().unwrap()).unwrap();
        // length f from StreamDiag is unavailable -> 0. The walker
        // still works because ensure_elem_index reads each sub-packet
        // payload's Array.size on first random-access query.
        assert_eq!(handle_length(handle).unwrap(), 0);

        let cases: &[(i64, i64)] = &[(0, 7), (2, 9), (3, 11)];
        for &(idx, expected) in cases {
            let ptr = ifile_bracket_index(handle, idx)
                .expect(&format!("bracket_index({}) failed", idx));
            let value = unsafe { *(ptr as *const i64) };
            assert_eq!(value, expected);
            shm::shfree(ptr).unwrap();
        }

        close_handle(handle).unwrap();
        let _ = std::fs::remove_file(&path);
    }
}
