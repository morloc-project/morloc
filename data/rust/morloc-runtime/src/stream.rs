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
//!   error sentinel) and changes on each close;
//!   double-close, foreign-int collision, and ABA reuse all return a
//!   clean error.
//! - **IFile / IStream / OStream** all implemented against the shared
//!   registry; cross-pool writers/readers share the SHM-resident
//!   sub-packet index under each slot's lock.
//! - **Voidstar-only sub-packets**: open paths reject a stream whose
//!   first sub-packet's format byte is not `PACKET_FORMAT_VOIDSTAR`.
//!
//! ## Per-handle caches
//!
//! - `ProcessLocalSlot::cache` is the per-handle decompressed-sub-packet
//!   LRU; consulted by `cache_get_or_materialize` and survives across
//!   bracket accesses (refcount-shared with caller via `shincref`).
//! - `subpacket_entries_local` is built greedily at open time (cheap for
//!   small N, acceptable for the million-sub-packet case at ~8 MiB).

use std::fs::OpenOptions;
use std::os::unix::io::AsRawFd;
use std::path::Path;
#[cfg(test)]
use std::sync::Mutex;
use crate::fork_policy::Held;

use morloc_runtime_types::packet::{
    decode_stream_tail,
    iter_packet_metadata,
    read_schema_from_meta,
    PacketHeader,
    METADATA_TYPE_FOOTER_FINAL, METADATA_TYPE_FOOTER_STATUS,
    METADATA_TYPE_STREAM_DIAG, METADATA_TYPE_SUBPACKET_INDEX,
    MLC_KIND_CHANNEL, MLC_KIND_IFILE, MLC_KIND_ISTREAM, MLC_KIND_OSTREAM,
    PACKET_COMPRESSION_NONE, PACKET_COMPRESSION_ZSTD,
    PACKET_FORMAT_VOIDSTAR,
    StreamDiag, STREAM_TAIL_SIZE,
    handle_kind_name,
    packet_format_name,
};
use morloc_runtime_types::recoverable_lock::RecoverableLock;
use morloc_runtime_types::schema::{parse_schema, Schema, SerialType};
use morloc_runtime_types::shm_types::{
    self as shm_types_crate, RelPtr,
};

use crate::error::MorlocError;
use crate::shm::{self, AbsPtr};
use crate::voidstar::{self, Space};
use morloc_runtime_types::{slice, width};

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
/// writer pool append here under the slot lock; @close reads it to
/// build the file's final footer. Grows by doubling. 16 covers the
/// common "open + a few flushes + close" shape without resize; larger
/// workloads pay the doubling cost a handful of times.
pub const OSTREAM_SUBPACKET_INDEX_INITIAL_CAP: u64 = 16;

/// Default cache capacity (bytes) for decompressed sub-packets per
/// handle. Overridable via `MORLOC_IFILE_CACHE_BYTES`.
const DEFAULT_IFILE_CACHE_BYTES: u64 = 256 * 1024 * 1024;

// ── Shared SHM stream registry: bootstrap ────────────────────────────────
//
// The registry is a `CompanionSegment` -- a dedicated shared mapping
// named `<basename>.reg` that lives outside the general allocator's
// `-<idx>` namespace. Layout:
//
//   offset 0:                 RegistryHeader (64 bytes)
//   offset 64:                slot[0]
//   offset 64 + N*512:        slot[N-1]
//
// All slot fields are atomically accessed. The header holds the `magic`
// gate that publishes "init is complete" to attaching processes plus the
// slot count.
//
// `registry_bootstrap()` opens (or attaches to) the segment; the CAS
// bootstrap on the magic gate arbitrates first-writer vs. attacher.
// `registry_teardown()` unmaps and unlinks; registered as a shclose hook
// so it runs on every normal-exit path.

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

    /// Singleton claim slots for the three stdio kinds. Sentinel
    /// `STDIO_UNCLAIMED = -1` means "no owner." Set via CAS at
    /// `@stdin` / `@stdout` / `@stderr` open; cleared via store at
    /// close. Second open of the same kind observes the winning
    /// handle and errors. Coordinates across every pool.
    stdio_slot_stdin:   std::sync::atomic::AtomicI64,
    stdio_slot_stdout:  std::sync::atomic::AtomicI64,
    stdio_slot_stderr:  std::sync::atomic::AtomicI64,

    /// Bumped, with a wake, whenever a stream is left ENDING for its
    /// opener to release; every process's release service waits on it.
    release_doorbell:   std::sync::atomic::AtomicU32,

    /// Reserved for future use.
    _reserved:    [u8; 12],
}

const _: () = {
    assert!(std::mem::size_of::<RegistryHeader>() == 64);
};

/// Compute the byte count of the registry companion segment.
///
/// The layout is `RegistryHeader` at offset 0 (`#[repr(C, align(64))]`
/// so alignment is guaranteed) followed by `slot_count` slots of
/// `STREAM_ENTRY_SIZE` each. `mmap` will page-round this up silently;
/// the actual mapped size may be larger.
const fn registry_volume_size(slot_count: usize) -> usize {
    std::mem::size_of::<RegistryHeader>() + slot_count * STREAM_ENTRY_SIZE
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

/// Lock-free fast-path check + slot count, populated on successful
/// bootstrap and cleared on teardown.
static REGISTRY_BASE: std::sync::atomic::AtomicPtr<RegistryHeader> =
    std::sync::atomic::AtomicPtr::new(std::ptr::null_mut());
static REGISTRY_SLOT_COUNT: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

pub(crate) static REGISTRY_SEGMENT: crate::fork_policy::Held<Option<crate::shm_companion::CompanionSegment>> =
    crate::fork_policy::Held::new(7, None);

// DAEMON-5
static REGISTRY_TORN_DOWN: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

// DAEMON-5
pub(crate) fn registry_reopen() {
    REGISTRY_TORN_DOWN.store(false, std::sync::atomic::Ordering::Release);
}

fn registry_closed() -> MorlocError {
    MorlocError::Other("the stream registry was torn down; no stream can be opened now".into())
}

/// Initialise the shared stream registry for this session. Wraps
/// `registry_bootstrap`; kept as the public entry point for the FFI
/// (`stream_registry_init` in ffi.rs).
pub fn registry_init() -> Result<usize, MorlocError> {
    registry_bootstrap()
}

/// Open (or attach to) the registry, run the CAS-arbitrated magic-gate
/// bootstrap, and register `registry_teardown` as an `shclose` hook so
/// normal-exit paths reach it automatically.
#[cfg(test)]
static BOOTSTRAP_GAP_HOOK: Mutex<Option<fn()>> = Mutex::new(None);

pub fn registry_bootstrap() -> Result<usize, MorlocError> {
    use std::sync::atomic::Ordering;

    let cached = REGISTRY_BASE.load(Ordering::Acquire);
    if !cached.is_null() {
        return Ok(REGISTRY_SLOT_COUNT.load(Ordering::Relaxed));
    }
    if REGISTRY_TORN_DOWN.load(Ordering::Acquire) {
        return Err(registry_closed());
    }
    #[cfg(test)]
    {
        let hook = *BOOTSTRAP_GAP_HOOK.lock().unwrap();
        if let Some(hook) = hook {
            hook();
        }
    }

    let slot_count = read_registry_slot_count();
    let volume_bytes = registry_volume_size(slot_count);

    // INIT-2: opened and published without the lock.
    let seg = crate::shm_companion::CompanionSegment::open(
        "reg",
        volume_bytes,
        crate::shm_companion::SweepPolicy::SweepOnCrash,
    )?;
    let base = seg.base as *mut RegistryHeader;
    if let Err(e) = publish_registry_header(base, slot_count) {
        seg.detach();
        return Err(e);
    }

    let mut seg = seg;
    let mut segment = REGISTRY_SEGMENT.lock();
    if REGISTRY_TORN_DOWN.load(Ordering::Acquire) {
        drop(segment);
        if shm::owns_program() {
            seg.unlink();
        }
        seg.detach();
        return Err(registry_closed());
    }
    if !REGISTRY_BASE.load(Ordering::Acquire).is_null() {
        drop(segment);
        seg.detach();
        return Ok(REGISTRY_SLOT_COUNT.load(Ordering::Relaxed));
    }
    if let Err(e) = seg.register_for_sweep() {
        drop(segment);
        seg.detach();
        return Err(e);
    }
    *segment = Some(seg);
    REGISTRY_SLOT_COUNT.store(slot_count, Ordering::Relaxed);
    REGISTRY_BASE.store(base, Ordering::Release);
    drop(segment);

    // Register normal-exit teardown once per process. `register_shclose_hook`
    // runs on both nexus (via `clean_exit -> shclose`) and pool (via
    // DAEMON-1, SLOT-13: stream writers stop before the registry and the
    // volumes they read are unmapped.
    shm::register_shclose_hook(crate::custody::stop_before_unmap);
    shm::register_shclose_hook(registry_teardown);

    sweeper_want();

    Ok(slot_count)
}

fn publish_registry_header(base: *mut RegistryHeader, slot_count: usize) -> Result<(), MorlocError> {
    use std::sync::atomic::Ordering;
    let header = unsafe { &*base };

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
                (*base).slot_count = slot_count as u64;
                (*base).gen_salt = read_gen_salt();
                (*base).stdio_slot_stdin.store(STDIO_UNCLAIMED, Ordering::Relaxed);
                (*base).stdio_slot_stdout.store(STDIO_UNCLAIMED, Ordering::Relaxed);
                (*base).stdio_slot_stderr.store(STDIO_UNCLAIMED, Ordering::Relaxed);
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
                "stream registry: unexpected magic {:#x} (expected {:#x} \
                 or init sentinel); concurrent libmorloc.so version mismatch?",
                observed,
                shm_types_crate::STREAM_REGISTRY_MAGIC,
            )));
        }
        Err(_) => {
            // Lost the race; another process is mid-bootstrap or done.
        }
    }

    let began = std::time::Instant::now();
    loop {
        if header.magic.load(Ordering::Acquire) == shm_types_crate::STREAM_REGISTRY_MAGIC {
            break;
        }
        if began.elapsed() > std::time::Duration::from_secs(5) {
            return Err(MorlocError::Other(
                "stream registry: magic never published within 5 s \
                 (bootstrapper stalled?)"
                    .into(),
            ));
        }
        std::thread::yield_now();
    }
    Ok(())
}

/// Reverse of `registry_bootstrap`. Ordering: stop sweeper (its reads
/// depend on `REGISTRY_BASE`), invalidate the base, then drop the
/// segment (its `Drop` runs `munmap` + `shm_unlink` + sweep-list
/// deregister). Idempotent.
pub fn registry_teardown() {
    use std::sync::atomic::Ordering;

    sweeper_shutdown();
    // DAEMON-1: after the sweeps, which finish streams through their writers.
    crate::custody::stop_before_unmap();

    let mut held = REGISTRY_SEGMENT.lock();
    REGISTRY_TORN_DOWN.store(true, Ordering::Release);
    if REGISTRY_BASE.swap(std::ptr::null_mut(), Ordering::AcqRel).is_null() {
        return;
    }
    REGISTRY_SLOT_COUNT.store(0, Ordering::Relaxed);
    let segment = held.take();
    drop(held);
    match segment {
        // DAEMON-5: every exit keeps the mapping.
        Some(mut seg) if shm::exiting() => {
            if shm::owns_program() {
                seg.unlink();
            }
            std::mem::forget(seg);
        }
        other => drop(other),
    }
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

/// Return the atomic claim slot for a given stdio kind. Callers CAS
/// this from `STDIO_UNCLAIMED` to the winning handle at open, and
/// store `STDIO_UNCLAIMED` back at close. Returns None if the
/// registry isn't attached yet.
#[inline]
pub(crate) fn stdio_claim_slot(
    stdio_kind: u8,
) -> Option<&'static std::sync::atomic::AtomicI64> {
    use std::sync::atomic::Ordering;
    let base = REGISTRY_BASE.load(Ordering::Acquire);
    if base.is_null() {
        return None;
    }
    // SAFETY: base is a stable pointer into the registry SHM region
    // for the lifetime of the process. The atomics live inside the
    // header struct at fixed offsets.
    let hdr = unsafe { &*base };
    Some(match stdio_kind {
        STDIO_KIND_STDIN  => &hdr.stdio_slot_stdin,
        STDIO_KIND_STDOUT => &hdr.stdio_slot_stdout,
        STDIO_KIND_STDERR => &hdr.stdio_slot_stderr,
        _ => return None,
    })
}

/// Element count of the process's @stdout OStream (its cumulative
/// `element_count`), or 0 if no @stdout is open. Read by @tell so the
/// offset-aware `with:`/`render:` synthesis can pass the per-batch element
/// offset to a handler. The count is already maintained for the stream
/// footers, so this is a plain field read.
pub fn stdout_element_count() -> u64 {
    use std::sync::atomic::Ordering;
    let handle = match stdio_claim_slot(STDIO_KIND_STDOUT) {
        Some(a) => a.load(Ordering::Acquire),
        None => return 0,
    };
    if handle <= 0 {
        return 0;
    }
    with_process_local_slot(handle, |_local, slot| Ok(slot.element_count.get())).unwrap_or(0)
}

/// True when this process runs under a staging nexus, which captures the
/// run's stdout stream batch by batch for replay (`MORLOC_STDOUT_STAGE`).
fn stdout_staged() -> bool {
    static STAGED: morloc_runtime_types::publish_once::PublishOnce<bool> = morloc_runtime_types::publish_once::PublishOnce::new();
    *STAGED.get_or_init(|| std::env::var_os("MORLOC_STDOUT_STAGE").is_some())
}

/// Return the registry's per-nexus generation-increment salt. The salt
/// is set by the bootstrap winner and is the same value seen by every
/// attached process.
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
// SLOT-8: fields fixed while the slot is open (file_path, schema_str,
// kind, final_footer, body_start, and subpacket_entries for IFile and
// IStream) are read through versioned_read. An OStream's
// subpacket_entries and compression_level change under the lock.
//
// Mutable-under-lock fields (cursor, element_count, diag) require
// taking `lock` before read or write. Lockfree snapshot reads of
// these are explicitly NOT supported by this protocol.

/// Per-slot state byte. Stored in the `state` AtomicU8. `FREE` is
/// re-allocatable (implicit zero on init); `OPEN_SHARED` marks a slot
/// in active use; `REMOTE_PAUSED` is reserved for cross-nexus IStream
/// pause-on-egress.
pub const SLOT_STATE_FREE: u8 = 0;
pub const SLOT_STATE_OPEN_SHARED: u8 = 1;
pub const SLOT_STATE_REMOTE_PAUSED: u8 = 2;
/// An OStream ended by a process other than its opener: its footer is
/// written and its handle is dead, but the file is still locked by the
/// opener's descriptor, so the opener releases the slot.
pub const SLOT_STATE_ENDING: u8 = 3;

/// Sentinel `call_id` value: "this slot should NOT be swept at end of
/// any call". Used by the cross-nexus return-serialization path to
/// transfer slot ownership from the remote's call to the caller's
/// scope. The sweeper compares against non-zero `call_id`s only, so
/// slots with this value are always skipped.
pub const CALL_ID_NO_SWEEP: u64 = 0;

/// Sentinel value stored in `stdio_slot_{stdin,stdout,stderr}` when no
/// process holds that stdio kind. CAS from this to a real handle at open.
pub const STDIO_UNCLAIMED: i64 = -1;

pub use morloc_runtime_types::stdio_proto::{
    STDIO_KIND_STDIN, STDIO_KIND_STDOUT, STDIO_KIND_STDERR,
};

/// SHM-resident per-slot record. Layout is pinned at 512 bytes
/// (`STREAM_ENTRY_SIZE`) so the slot array can be addressed by
/// fixed-stride arithmetic without indirection.
///
/// Fields are grouped by mutability + protection:
///   - **Immutable after @open**: `kind`, `opener_pid`,
///     `opener_pid_start_time`, `file_path`, `file_path_len`,
///     `schema_str`, `schema_str_len`, `final_footer`,
///     `compression_level`, `subpacket_entries`, `subpacket_entries_len`,
///     `body_start`. Readers use the versioned-pointer pattern
///     described above.
///   - **Mutated under `lock`**: `cursor`, `element_count`, `diag`.
///   - **Independently atomic**: `generation`, `call_id`, `state`,
///     `lock`. These are accessed without holding any other lock.
/// SLOT-8: a slot field, written under the slot's lock and read under it
/// or behind a generation check, so a read may race a write.
pub(crate) trait SlotField {
    type Value;
    fn get(&self) -> Self::Value;
    fn set(&self, v: Self::Value);
}

macro_rules! slot_field {
    ($atomic:ty, $value:ty) => {
        impl SlotField for $atomic {
            type Value = $value;
            #[inline]
            fn get(&self) -> $value {
                self.load(std::sync::atomic::Ordering::Relaxed)
            }
            #[inline]
            fn set(&self, v: $value) {
                self.store(v, std::sync::atomic::Ordering::Relaxed)
            }
        }
    };
}
slot_field!(std::sync::atomic::AtomicU8, u8);
slot_field!(std::sync::atomic::AtomicU32, u32);
slot_field!(std::sync::atomic::AtomicU64, u64);
slot_field!(std::sync::atomic::AtomicIsize, isize);

#[repr(C, align(64))]
pub struct RegistrySlot {
    // ── Identity / lifecycle (atomically accessed) ──────────────────
    /// Bumped on every close. Publication-
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
    pub kind:                 std::sync::atomic::AtomicU8,                              // off 17
    _pad0:                    [u8; 6],                         // off 18..24

    /// Opener-pool process start stamp (`process::start_time`). Defends against PID reuse
    /// across pool restarts in the §1.7 PID sweep. Plain u64;
    /// immutable after @open publication.
    pub opener_pid_start_time: std::sync::atomic::AtomicU64,                            // off 24..32

    /// OS PID of the pool that opened the slot. Used only by the §1.7
    /// PID sweep on pool exit / crash recovery.
    pub opener_pid:           std::sync::atomic::AtomicU32,                             // off 32..36
    _pad1:                    [u8; 4],                         // off 36..40

    /// IStream only: the end of the last sub-packet when the stream was
    /// opened. A reader never reads at or past it: a later @append cuts
    /// and rewrites the bytes beyond, and every process sharing the
    /// stream must agree where it ends.
    pub data_end:             std::sync::atomic::AtomicU64,                             // off 40..48

    // ── File identity (immutable after publication) ─────────────────
    /// SHM RelPtr to a UTF-8 path string. Allocated from the shared
    /// SHM allocator at @open; freed at @close via shfree. Length is
    /// in `file_path_len`. NOT canonicalised; the planfile commits
    /// to explicit-only multi-writer sharing (no realpath dedup).
    pub file_path:            std::sync::atomic::AtomicIsize,                          // off 48..56
    pub file_path_len:        std::sync::atomic::AtomicU32,                             // off 56..60
    _pad3:                    [u8; 4],                         // off 60..64

    pub schema_str:           std::sync::atomic::AtomicIsize,                          // off 64..72
    pub schema_str_len:       std::sync::atomic::AtomicU32,                             // off 72..76
    _pad4:                    [u8; 4],                         // off 76..80

    // -- Mutable state under `lock` --
    /// IStream/OStream cursor (byte offset). IFile leaves at 0
    /// (random access goes through `subpacket_entries` instead).
    pub cursor:               std::sync::atomic::AtomicU64,                             // off 80..88
    pub element_count:        std::sync::atomic::AtomicU64,                             // off 88..96

    /// IFile only; set at @open from the file's footer. Plain u8 0/1
    /// for cross-process clarity. Immutable after @open publication.
    pub final_footer:         std::sync::atomic::AtomicU8,                              // off 96
    pub compression_level:    std::sync::atomic::AtomicU8,                              // off 97
    _pad5:                    [u8; 6],                         // off 98..104

    /// IFile only; SHM RelPtr to a `[SubpacketEntry]` array of
    /// (offset, elem_count) pairs, one per sub-packet. Length in
    /// `subpacket_entries_len`. Immutable after @open publication;
    /// freed at @close.
    pub subpacket_entries:    std::sync::atomic::AtomicIsize,                          // off 104..112
    pub subpacket_entries_len:std::sync::atomic::AtomicU64,                             // off 112..120

    /// IStream initial cursor (right after stream header). Immutable
    /// after @open publication.
    pub body_start:           std::sync::atomic::AtomicU64,                             // off 120..128

    /// Per-write diagnostic / running counters. Updated by OStream
    /// writers under `lock`; readers either hold the lock or accept
    /// momentarily-stale values. ~160 bytes embedded inline.
    pub diag:                 std::cell::UnsafeCell<StreamDiag>,                      // off 128..288

    // ── OStream write buffer (Part A of the buffering work) ────────
    /// SHM RelPtr to a per-slot write buffer. Allocated at @open
    /// OStream (`WRITE_BUFFER_BYTES_DEFAULT` bytes, env-overridable),
    /// freed at slot release. The buffer's first 16 bytes are
    /// reserved for the Array header (filled at flush); bytes 16..
    /// 16+`index_cap`*elem_width are the inline element index; the
    /// remainder is the variable-data section. Cross-pool writers
    /// append to this same buffer under the slot lock.
    pub write_buffer:           std::sync::atomic::AtomicIsize,                        // off 288..296

    /// Current index-section capacity in ELEMENTS. Starts at
    /// `WRITE_BUFFER_INDEX_INITIAL_CAP` (1024); doubles when filled
    /// (shifting the data region right). Always >= write_buffer_index_count.
    pub write_buffer_index_cap: std::sync::atomic::AtomicU64,                           // off 296..304

    /// Number of elements currently buffered (i.e. inline entries
    /// in the index section). Reset to 0 after each flush.
    pub write_buffer_index_count: std::sync::atomic::AtomicU64,                         // off 304..312

    /// Bytes currently used in the data section (variable-length
    /// portion). Reset to 0 after each flush.
    pub write_buffer_data_used: std::sync::atomic::AtomicU64,                           // off 312..320

    /// OStream-only: capacity in entries of the SHM-resident sub-packet
    /// entry array whose RelPtr lives in `subpacket_entries`. Grown by
    /// doubling under the slot lock when `subpacket_entries_len` reaches
    /// it. For IFile this field is 0 (the array is set once from the
    /// parsed final footer and never grows).
    pub subpacket_entries_cap:std::sync::atomic::AtomicU64,                             // off 320..328

    /// Non-zero when this slot is bound to stdin/stdout/stderr rather
    /// than a real file. Immutable after publication.
    pub is_stdio:             std::sync::atomic::AtomicU8,                              // off 328
    /// When `is_stdio` is set, the specific stdio kind: 0=stdin,
    /// 1=stdout, 2=stderr. Immutable after publication.
    pub stdio_kind:           std::sync::atomic::AtomicU8,                              // off 329
    /// Non-zero on a staged stdout (see `stdout_staged`): each `@write`
    /// is emitted as exactly one sub-packet, never split or merged, an
    /// empty batch included, so the stream keeps the producer's batch
    /// boundaries. Immutable after publication.
    pub staged:               std::sync::atomic::AtomicU8,                              // off 330
    _stdio_pad:               [u8; 5],                         // off 331..336

    _retired_wb:              [u8; 10],                        // off 336..346
    /// Non-zero once a process died holding `lock`: what it protects may
    /// be half-updated, so every operation but releasing the slot fails.
    pub poisoned:             std::sync::atomic::AtomicU8,                              // off 346
    _wb_pad:                  [u8; 5],                         // off 347..352
    /// A written stream's queue to its custodian (`custody::CustodyQueue`).
    pub custody:              std::sync::atomic::AtomicIsize,                          // off 352

    /// Device and inode of an OStream's file, from its locked descriptor,
    /// so a reopen blocked by the lock can find the stream holding it.
    pub file_dev:             std::sync::atomic::AtomicU64,                             // off 360
    pub file_ino:             std::sync::atomic::AtomicU64,                             // off 368
    _retired_end:             [u8; 8],                         // off 376

    /// Per-slot mutation lock. Held for cursor / element_count / diag
    /// updates. NOT held for lockfree reads of immutable-after-open
    /// fields (those use the versioned-pointer pattern against
    /// `generation`). Survives a holder's death; see `SlotGuard`.
    pub lock:                 RecoverableLock,                 // off 384

    /// Padding to round the slot up to STREAM_ENTRY_SIZE so the next
    /// slot starts on a fresh cache-line-aligned boundary.
    _tail_pad:                [u8; 128 - std::mem::size_of::<RecoverableLock>()],
}

// SLOT-8: `diag` is read and written only under the slot's lock.
unsafe impl Sync for RegistrySlot {}

const _: () = {
    assert!(std::mem::size_of::<RegistrySlot>() == STREAM_ENTRY_SIZE);
    assert!(std::mem::offset_of!(RegistrySlot, lock) == 384);
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

// -- Slot lock --
//
// Each `RegistrySlot` has its own `lock`, which serialises mutations to
// `cursor`, `element_count`, `diag` and the write buffer. It is held across
// sub-packet writes, fdatasync, synchronous compression and stdio RPCs, so
// waiters block rather than spin. A process killed while holding it (a
// signal, the OOM killer) poisons the slot: its state may be half-updated,
// so operations on the stream fail from then on, and releasing the slot
// leaks its SHM blocks rather than free pointers that may be mid-swap.

fn died_inside() -> MorlocError {
    MorlocError::Other(
        "a process died while using this stream; it is incomplete and can \
         no longer be read or written".into(),
    )
}

/// Holds a slot's lock until dropped. A panic while it is held poisons the
/// slot, as a death would: unwinding releases the lock mid-update.
pub(crate) struct SlotGuard<'a> {
    slot: &'a RegistrySlot,
    _held: morloc_runtime_types::recoverable_lock::RecoverableGuard<'a>,
}

impl Drop for SlotGuard<'_> {
    fn drop(&mut self) {
        if std::thread::panicking() {
            self.slot.poisoned.set(1);
        }
    }
}

impl<'a> SlotGuard<'a> {
    /// Take the slot's lock to operate on its stream. Fails if a process
    /// died inside the slot.
    fn lock(slot: &'a RegistrySlot) -> Result<Self, MorlocError> {
        let guard = Self::lock_any(slot)?;
        if slot.poisoned.get() != 0 {
            return Err(died_inside());
        }
        Ok(guard)
    }

    /// Take the slot's lock whatever its state, to end or reclaim it.
    fn lock_any(slot: &'a RegistrySlot) -> Result<Self, MorlocError> {
        let acquired = slot.lock.lock()?;
        if acquired.holder_died {
            slot.poisoned.set(1);
        }
        Ok(SlotGuard { slot, _held: acquired.guard })
    }
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

    /// How much of the start of the mapping has been handed back to the
    /// kernel ('drop_read_pages'): an IStream reads its file forward, once.
    pub pages_dropped: u64,

    /// IStream only: the file the mapping was made from, kept open so that
    /// 'drop_read_pages' can map read pages afresh.
    pub map_file: Option<std::fs::File>,

    /// Underlying file descriptor. For OStream this is the fd that
    /// holds the flock (only the OPENER pool acquires the flock;
    /// non-opener writers use their own non-flock'd fd). For
    /// IFile/IStream this is -1 (we close after mmap).
    pub fd: i32,

    /// Per-handle decompressed-sub-packet LRU. Lives on the heap so
    /// dropping the entry from the HashMap shfree's the SHM blocks
    /// referenced by the cache.
    pub(crate) cache: crate::fork_policy::ForkLocal<Box<StreamCache>>,

    /// Parsed value schema cached from the SHM slot's `schema_str`.
    /// Each pool parses on first attach; the cost is a microsecond-
    /// scale string parse that's done once per pool per handle.
    pub value_schema: Schema,

    /// Parsed element schema (for list-valued kinds, the schema of
    /// one element). For non-list values this equals `value_schema`.
    pub elem_schema: Schema,

    /// IFile only: copy of the SHM slot's sub-packet entry array.
    /// Resident in process-local memory for fast random access without
    /// going through `rel2abs` per query. Empty for IStream / OStream.
    pub subpacket_entries_local: Vec<morloc_runtime_types::packet::SubpacketEntry>,

    /// IFile only: cumulative element-count per sub-packet, built
    /// lazily on first random-access query from `subpacket_entries_local`.
    /// `cum[i]` is the total element count of sub-packets `0..i`, so a
    /// global element index `g` lands in the sub-packet
    /// `partition_point(g)`. Survives across bracket accesses on the
    /// same handle.
    pub subpacket_elem_cum: Option<Vec<u64>>,

    /// For DATA_PACKET files opened as IFile, the file is a single
    /// "sub-packet" starting at offset 0 and the entry array has one
    /// entry (offset=0, elem_count=<Array size>). The walker handles
    /// this flag to skip the stream-header / footer parsing paths.
    pub is_data_packet: bool,

    /// `fd` holds the stream file's lock: this process opened the stream.
    pub(crate) holds_lock: bool,

    /// `FORK_EPOCH` when this slot was made. A slot from before a fork that
    /// the child reaches is its parent's: its descriptor was closed.
    pub(crate) fork_epoch: u64,
}

fn fork_epoch() -> u64 {
    crate::fork_policy::generation()
}

impl Drop for ProcessLocalSlot {
    fn drop(&mut self) {
        // Drop the decompression cache first; this shfree's any
        // SHM blocks the cache references. Then unmap the file
        // region. Then close the fd (which releases flock if held).
        if !self.cache.is_inherited() {
            for entry in self.cache.entries.drain(..) {
                let _ = crate::shm::shfree(entry.shm_packet);
            }
        }
        if !self.mmap_ptr.is_null() && self.mmap_size > 0 {
            unsafe {
                libc::munmap(self.mmap_ptr as *mut libc::c_void, self.mmap_size as usize);
            }
        }
        if self.fd >= 0 {
            if self.holds_lock {
                LOCKED_FDS.lock().retain(|&f| f != self.fd);
            }
            unsafe { libc::close(self.fd); }
        }
    }
}


fn file_identity_of(f: &std::fs::File) -> (u64, u64) {
    use std::os::unix::fs::MetadataExt;
    f.metadata().map_or((0, 0), |m| (m.dev(), m.ino()))
}

/// A process joining a stream opens its file by path; refuse it unless that
/// is still the file the stream was opened on. Another file renamed over
/// the path would be read through the original's index, or written instead.
fn check_identity(
    handle: i64,
    expected: (u64, u64),
    path: &str,
    found: (u64, u64),
) -> Result<(), MorlocError> {
    if expected.1 != 0 && expected != found {
        return Err(MorlocError::Other(format!(
            "stream handle {:#x}: the file '{}' was replaced after the stream was opened",
            handle, path,
        )));
    }
    Ok(())
}

/// The device and inode of an open file, or zeros if they cannot be read.
fn file_identity(fd: libc::c_int) -> (u64, u64) {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd, &mut st) } != 0 {
        return (0, 0);
    }
    (st.st_dev as u64, st.st_ino as u64)
}

/// Close a descriptor this process locked for a stream that never
/// opened, releasing the lock for any child forked in between.
pub(crate) fn unlock_and_close(fd: libc::c_int) {
    LOCKED_FDS.lock().retain(|&f| f != fd);
    unsafe {
        libc::flock(fd, libc::LOCK_UN);
        libc::close(fd);
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

/// A process's entry for one handle.
pub(crate) enum LocalEntry {
    Idle(ProcessLocalSlot),
    /// A thread has the slot out for an operation; others wait for it.
    /// A forked child inherits the mark without the thread, so a mark
    /// from another pid is disregarded.
    InUse { generation: u64, thread: std::thread::ThreadId },
}

/// Per-process map from handle int to physical OS state. Lazily
/// initialised on first access; cleared on `shclose_atexit`-style
/// teardown.
///
/// A process has at most one slot per written handle. A second would hold
/// its own descriptor and sealed batches: whichever was dropped would take
/// the opener's file lock or unwritten elements with it. So an operation
/// on a written handle takes the slot out of the map and leaves an `InUse`
/// mark, and other threads wait for it rather than attaching another. A
/// read handle's slot holds only a mapping and a cache, so threads reading
/// at once each use their own. The map lock is never held across an
/// operation's I/O.
pub(crate) static PROCESS_LOCAL_SLOTS: Held<Option<std::collections::HashMap<i64, LocalEntry>>> =
    Held::new(2, None);

// SHM-8: slots that hold a stream's file lock, which only this process releases.
pub fn held_stream_locks() -> usize {
    let guard = PROCESS_LOCAL_SLOTS.lock();
    guard.as_ref().map_or(0, |map| {
        map.values()
            .filter(|e| match e {
                LocalEntry::Idle(local) => local.holds_lock,
                LocalEntry::InUse { .. } => true,
            })
            .count()
    })
}

// SHM-8: what keeps a worker from retiring.
pub(crate) fn morloc_retire_blockers() -> i64 {
    drop_ended_unlocked_slots();
    crate::shm::held_references() + held_stream_locks() as i64
}

/// Signalled whenever an `InUse` mark is replaced or removed.
static PROCESS_LOCAL_RETURNED: std::sync::Condvar = std::sync::Condvar::new();

/// This thread's use of a handle's slot. Dropping an exclusive claim
/// without `give_back` removes the `InUse` mark, so waiting threads attach
/// anew.
struct LocalClaim {
    handle: i64,
    exclusive: bool,
    returned: bool,
}

impl LocalClaim {
    fn give_back(mut self, local: ProcessLocalSlot) {
        self.returned = true;
        if self.exclusive {
            install_process_local_slot(self.handle, local);
            return;
        }
        let mut guard = PROCESS_LOCAL_SLOTS.lock();
        let map = guard.get_or_insert_with(std::collections::HashMap::new);
        let spare = match map.entry(self.handle) {
            std::collections::hash_map::Entry::Vacant(v) => {
                v.insert(LocalEntry::Idle(local));
                None
            }
            // Another reader returned first; keep one.
            std::collections::hash_map::Entry::Occupied(_) => Some(local),
        };
        drop(guard);
        drop(spare);
    }
}

impl Drop for LocalClaim {
    fn drop(&mut self) {
        if self.returned || !self.exclusive {
            return;
        }
        let mut guard = PROCESS_LOCAL_SLOTS.lock();
        if let Some(map) = guard.as_mut() {
            if let Some(LocalEntry::InUse { generation, thread }) = map.get(&self.handle) {
                if *generation == crate::fork_policy::generation() && *thread == std::thread::current().id() {
                    map.remove(&self.handle);
                }
            }
        }
        drop(guard);
        PROCESS_LOCAL_RETURNED.notify_all();
    }
}

/// How an operation uses a handle's slot.
#[derive(Clone, Copy, PartialEq)]
enum ClaimMode {
    /// Reads only: take the idle slot if there is one, else attach another.
    Shared,
    /// Exclusive, waiting while another thread of this process uses it.
    Wait,
    /// Exclusive, or `None` while another thread uses it.
    NoWait,
}

/// Claim `handle`'s slot for one operation, taking its idle slot if there
/// is one. An exclusive claim marks the handle in use by this thread.
fn claim_process_local_slot(
    handle: i64,
    mode: ClaimMode,
) -> Result<Option<(LocalClaim, Option<ProcessLocalSlot>)>, MorlocError> {
    // FORK-14
    let generation = crate::fork_policy::generation();
    let thread = std::thread::current().id();
    let mut guard = PROCESS_LOCAL_SLOTS.lock();
    if mode == ClaimMode::Shared {
        let map = guard.get_or_insert_with(std::collections::HashMap::new);
        let local = match map.remove(&handle) {
            Some(LocalEntry::Idle(l)) => Some(l),
            Some(mark) => {
                map.insert(handle, mark);
                None
            }
            None => None,
        };
        return Ok(Some((LocalClaim { handle, exclusive: false, returned: false }, local)));
    }
    loop {
        let map = guard.get_or_insert_with(std::collections::HashMap::new);
        if let Some(LocalEntry::InUse { generation: g, thread: t }) = map.get(&handle) {
            if *g == generation {
                if *t == thread {
                    return Err(MorlocError::Other(format!(
                        "stream handle {:#x} used again by an operation already using it",
                        handle,
                    )));
                }
                if mode == ClaimMode::NoWait {
                    return Ok(None);
                }
                guard = guard.wait(&PROCESS_LOCAL_RETURNED);
                continue;
            }
        }
        let local = match map.insert(handle, LocalEntry::InUse { generation, thread }) {
            Some(LocalEntry::Idle(l)) => Some(l),
            _ => None,
        };
        return Ok(Some((LocalClaim { handle, exclusive: true, returned: false }, local)));
    }
}

/// Run `f` against the process-local slot for `handle`. On entry the
/// function validates the SHM slot's generation against the cached
/// entry; if stale (or missing), it drops any stale entry and
/// attaches afresh by reading the SHM slot's path + kind.
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
    // Stdio slots pin to the opener PID: a forked child that inherits
    // the process-local slot map (and would otherwise sail through
    // the local cache) has its writes routed to a nexus RPC that
    // interleaves bytes on the same fd with the parent. Catch the
    // misuse at the pool-side entry point.
    if slot.is_stdio.get() != 0 && !is_this_process(slot.opener_pid.get(), slot.opener_pid_start_time.get()) {
        return Err(MorlocError::Other(format!(
            "stdio stream cannot cross a fork boundary: slot opened by \
             PID {}, current process is PID {}. Re-open the stream in \
             this process, or route the read/write through the opener.",
            slot.opener_pid.get(), std::process::id(),
        )));
    }

    let mode = if slot.kind.get() == MLC_KIND_IFILE || slot.kind.get() == MLC_KIND_ISTREAM {
        ClaimMode::Shared
    } else {
        ClaimMode::Wait
    };
    let (claim, mut taken) = claim_process_local_slot(handle, mode)?
        .expect("a waiting claim always succeeds");

    // Validate cached_generation; dispose on mismatch. The stream it
    // belonged to has ended.
    if taken.as_ref().is_some_and(|l| l.cached_generation != gen_now) {
        finish_ended(handle, taken.take().expect("checked above"));
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
    let gen_after = generation_after_read(slot) & GENERATION_MASK;
    if gen_after != gen_now {
        // The local slot is now stale; dispose of it.
        drop(claim);
        finish_ended(handle, local);
        return Err(MorlocError::Other(format!(
            "stream handle {:#x}: slot was closed mid-attach (generation \
             went from {} to {}); retry",
            handle, gen_now, gen_after,
        )));
    }

    if local.cache.is_inherited() {
        local.cache = crate::fork_policy::ForkLocal::new(Box::new(StreamCache::new(read_cache_cap_env())));
    }

    // Run f with the validated local slot. A panic must not lose the slot:
    // it may hold the file lock that only this process can release.
    let result = match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        f(&mut local, slot)
    })) {
        Ok(r) => r,
        Err(panic) => {
            if has_ended(handle, &local) {
                drop(claim);
                finish_ended(handle, local);
            } else {
                claim.give_back(local);
            }
            std::panic::resume_unwind(panic);
        }
    };

    // Re-install the local slot in the map for future calls. We
    // always re-install, even on Err, so the cache isn't dropped
    // just because the operation failed -- unless another process ended
    // the stream meanwhile and left its file locked here, or this is a
    // forked child holding its parent's slot.
    if local.fork_epoch != fork_epoch() {
        // FORK-4: a holder's descriptor was closed by the fork handler; the
        // child closes its own copy of any other.
        if local.holds_lock {
            local.fd = -1;
            local.holds_lock = false;
        }
        drop(claim);
        drop(local);
    } else if has_ended(handle, &local) {
        drop(claim);
        finish_ended(handle, local);
    } else {
        claim.give_back(local);
    }

    result
}

/// Helper: insert an entry into the process-local map, replacing any
/// existing entry (the caller already validated generation).
fn install_process_local_slot(handle: i64, slot: ProcessLocalSlot) {
    let mut guard = PROCESS_LOCAL_SLOTS.lock();
    let map = guard.get_or_insert_with(std::collections::HashMap::new);
    let replaced = map.insert(handle, LocalEntry::Idle(slot));
    drop(guard);
    PROCESS_LOCAL_RETURNED.notify_all();
    if let Some(LocalEntry::Idle(old)) = replaced {
        finish_ended(handle, old);
    }
}

/// Descriptors of this process that hold a stream file's lock. A forked
/// child closes its copies: the lock is the opener's, and a copy in a child
/// would keep the file locked after the opener ended the stream or died.
pub(crate) static LOCKED_FDS: Held<Vec<libc::c_int>> = Held::new(3, Vec::new());

fn note_locked_fd(fd: libc::c_int) {
    LOCKED_FDS.lock().push(fd);
}

// FORK-4: the child closes its copies; its slots reattach on next use.
pub(crate) fn after_fork_in_child(
    map: &mut Option<std::collections::HashMap<i64, LocalEntry>>,
    locked_fds: &mut Vec<libc::c_int>,
) {
    for fd in locked_fds.drain(..) {
        unsafe { libc::close(fd); }
    }
    if let Some(map) = map.as_mut() {
        for entry in map.values_mut() {
            if let LocalEntry::Idle(local) = entry {
                if local.holds_lock {
                    local.fd = -1;
                    local.holds_lock = false;
                    local.cached_generation = u64::MAX;
                }
            }
        }
    }
}

/// Explicitly invalidate (drop) the process-local entry for `handle`
/// without consulting the SHM slot, once its stream has ended. Used by
/// `shared_close_handle` after it releases the SHM slot, so the next
/// access reattaches (which will then fail the generation check cleanly).
pub fn invalidate_process_local_slot(handle: i64) {
    let mut guard = PROCESS_LOCAL_SLOTS.lock();
    let removed = guard.as_mut().and_then(|map| map.remove(&handle));
    drop(guard);
    match removed {
        Some(LocalEntry::Idle(local)) => finish_ended(handle, local),
        Some(LocalEntry::InUse { .. }) => PROCESS_LOCAL_RETURNED.notify_all(),
        None => {}
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
    struct View {
        is_stdio: u8,
        stdio_kind: u8,
        kind: u8,
        path: Vec<u8>,
        schema: Vec<u8>,
        entries: Vec<morloc_runtime_types::packet::SubpacketEntry>,
        body_start: u64,
        identity: (u64, u64),
    }
    let view = versioned_read(slot, cached_generation, |s| {
        // SLOT-8: an open OStream's entry array grows under its lock.
        let entries = if s.kind.get() == MLC_KIND_IFILE || s.kind.get() == MLC_KIND_ISTREAM {
            let width = std::mem::size_of::<morloc_runtime_types::packet::SubpacketEntry>();
            let extent = (s.subpacket_entries_len.get() as usize).checked_mul(width).ok_or_else(|| {
                MorlocError::Other(format!("stream handle {:#x}: sub-packet index length overflows", handle))
            })?;
            copy_slot_bytes(s.subpacket_entries.get(), extent)?
                .chunks_exact(width)
                .map(|e| unsafe {
                    std::ptr::read_unaligned(e.as_ptr() as *const morloc_runtime_types::packet::SubpacketEntry)
                })
                .collect()
        } else {
            Vec::new()
        };
        Ok(View {
            is_stdio: s.is_stdio.get(),
            stdio_kind: s.stdio_kind.get(),
            kind: s.kind.get(),
            path: copy_slot_bytes(s.file_path.get(), s.file_path_len.get() as usize)?,
            schema: copy_slot_bytes(s.schema_str.get(), s.schema_str_len.get() as usize)?,
            entries,
            body_start: s.body_start.get(),
            identity: (s.file_dev.get(), s.file_ino.get()),
        })
    })?
    .ok_or_else(|| MorlocError::Other(format!(
        "stream handle {:#x}: slot raced during attach (the handle names generation {})",
        handle, cached_generation,
    )))?;
    // Stdio slots must be routed through the nexus RPC by every op
    // (write/next/flush/close). If we get here, an op forgot its
    // stdio short-circuit; fail loudly rather than trying to open
    // the sentinel path ("-", "-2") as a real file.
    if view.is_stdio != 0 {
        return Err(MorlocError::Other(format!(
            "stream handle {:#x}: attach_process_local_slot called on a \
             stdio slot (kind byte {}); the caller is missing its stdio \
             RPC short-circuit",
            handle, view.stdio_kind,
        )));
    }
    let kind = view.kind;
    // A channel has no file: this process needs only its schemas.
    if kind == MLC_KIND_CHANNEL {
        let schema_str = std::str::from_utf8(&view.schema).map_err(|e| MorlocError::Other(format!(
            "stream handle {:#x}: channel schema is not valid UTF-8: {}", handle, e,
        )))?;
        let schema = parse_schema(schema_str).map_err(|e| MorlocError::Schema(format!(
            "stream handle {:#x}: channel schema '{}': {}", handle, schema_str, e,
        )))?;
        return Ok(channel_local(cached_generation, &schema));
    }
    if view.path.is_empty() {
        return Err(MorlocError::Other(format!(
            "stream handle {:#x}: slot has no file_path (corrupt slot \
             or partially-published @open)",
            handle,
        )));
    }
    let path_str = String::from_utf8(view.path).map_err(|e| {
        MorlocError::Other(format!(
            "stream handle {:#x}: file_path is not valid UTF-8: {}",
            handle, e,
        ))
    })?;

    // Open + mmap depending on kind.
    let (fd, map_file, mmap_ptr, mmap_size) = match kind {
        x if x == MLC_KIND_IFILE || x == MLC_KIND_ISTREAM => {
            let (f, mp, sz) = mmap_file_readonly_keep(&path_str)?;
            if let Err(e) = check_identity(handle, view.identity, &path_str, file_identity_of(&f)) {
                unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize); }
                return Err(e);
            }
            let keep = if x == MLC_KIND_ISTREAM { Some(f) } else { None };
            (-1i32, keep, mp, sz)
        }
        // SLOT-10: a writer needs no descriptor; the custodian has the file.
        x if x == MLC_KIND_OSTREAM => (-1i32, None, std::ptr::null_mut(), 0),
        other => {
            return Err(MorlocError::Other(format!(
                "stream handle {:#x}: unknown kind byte {}", handle, other,
            )));
        }
    };

    let schema_str = String::from_utf8(view.schema).map_err(|e| {
        MorlocError::Other(format!(
            "stream handle {:#x}: schema_str is not UTF-8: {}", handle, e,
        ))
    })?;
    let parsed_schema = if schema_str.is_empty() {
        Schema::primitive(SerialType::Nil)
    } else {
        let s = parse_schema(&schema_str).map_err(|e| {
            MorlocError::Other(format!(
                "stream handle {:#x}: unparseable schema '{}': {}",
                handle, schema_str, e,
            ))
        })?;
        // Streams (IStream / OStream) are list-shaped; IFile is a
        // single-value container that may hold any shape (like a
        // `Maybe a`). Only reject non-list on stream-kind attaches.
        if kind == MLC_KIND_ISTREAM || kind == MLC_KIND_OSTREAM {
            let target = format!("handle {:#x}", handle);
            reject_non_list_stream_schema(&s, "stream slot attach", &target)?;
        }
        s
    };
    let subpacket_entries_local = view.entries;

    // Detect DATA_PACKET shape (single sub-packet at offset 0, no
    // stream header). Convention from `parse_stream_file`:
    // is_data_packet => body_start == 0 AND the entry array is a
    // single (offset=0, elem_count=<Array size>) pair.
    let is_data_packet = view.body_start == 0
        && subpacket_entries_local.len() == 1
        && subpacket_entries_local[0].offset == 0;

    let (value_schema, elem_schema) = derive_stream_schemas(&parsed_schema);

    let cap_bytes = read_cache_cap_env();
    let cache = crate::fork_policy::ForkLocal::new(Box::new(StreamCache::new(cap_bytes)));
    Ok(ProcessLocalSlot {
        cached_generation,
        mmap_ptr,
        mmap_size,
        pages_dropped: 0,
        map_file,
        fd,
        cache,
        value_schema,
        elem_schema,
        subpacket_entries_local,
        subpacket_elem_cum: None,
        is_data_packet,
        holds_lock: false,
        fork_epoch: fork_epoch(),
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
// `release_slot_locked`: caller already holds the slot lock; we
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

pub(crate) fn allocate_slot_cas(
) -> Result<(usize, &'static RegistrySlot, SlotGuard<'static>), MorlocError> {
    allocate_slot_cas_for(std::process::id(), read_pid_start_time(), current_call_id())
}

/// Allocate a slot for a stream opened by process `(pid, start)` in call
/// `call_id`.
pub(crate) fn allocate_slot_cas_for(
    pid: u32,
    start: u64,
    call_id: u64,
) -> Result<(usize, &'static RegistrySlot, SlotGuard<'static>), MorlocError> {
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
    let start_idx = slot_probe_seed() % slot_count;
    for off in 0..slot_count {
        let idx = (start_idx + off) % slot_count;
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
            let guard = match SlotGuard::lock_any(slot) {
                Ok(g) => g,
                Err(e) => {
                    slot.state.store(SLOT_STATE_FREE, Ordering::Release);
                    return Err(e);
                }
            };
            // A process that died inside the slot before it was freed
            // left the mark; the stream about to be published owns every
            // field afresh. The opener is recorded first, so the crash
            // sweeps can reclaim the slot if it dies before publishing it.
            slot.poisoned.set(0);
            slot.opener_pid.set(pid);
            slot.opener_pid_start_time.set(start);

            slot.call_id.store(call_id, Ordering::Release);
            return Ok((idx, slot, guard));
        }
    }
    Err(MorlocError::Other(format!(
        "stream registry: all {} slots are in use; raise \
         MORLOC_REGISTRY_SLOT_COUNT (current cap {}) or close more handles",
        slot_count, STREAM_REGISTRY_MAX_SLOT_COUNT,
    )))
}

/// The slot's custody queue, ready for a new stream: the one its index
/// already has, or a new one the slot keeps from now on.
// SLOT-10
fn ready_slot_queue(slot: &RegistrySlot) -> Result<&'static crate::custody::CustodyQueue, MorlocError> {
    let depth = crate::write_behind::depth().max(1);
    if let Ok(q) = slot_queue(slot) {
        if !q.has_consumer() {
            q.reset_for_open(depth);
            return Ok(q);
        }
        // A writer that has not yet seen its slot released may still pop
        // the old queue; the new stream gets its own, and the old block is
        // left to it.
    }
    let rel = crate::custody::new_queue()?;
    slot.custody.set(slot_owns(rel));
    slot_queue(slot)
}

/// Transfer a freshly allocated block to the registry. The slot owns it
/// from here: its lifetime is the slot's, which spans dispatches and can
/// be shared across processes, so it must not be released when the
/// allocating eval scope exits. Returns the relptr for assignment.
// SHM-8: the slot, not this process, holds the reference from here.
/// Blocks allocated for a slot being published, until the slot holds them:
/// freed if publishing fails first.
#[derive(Default)]
struct Unpublished(Vec<RelPtr>);

impl Unpublished {
    fn hold(&mut self, rel: RelPtr) -> RelPtr {
        if rel != shm_types_crate::RELNULL {
            self.0.push(rel);
        }
        rel
    }

    /// `rel`, now held by the slot.
    fn own(&mut self, rel: RelPtr) -> RelPtr {
        self.0.retain(|r| *r != rel);
        slot_owns(rel)
    }
}

impl Drop for Unpublished {
    fn drop(&mut self) {
        for rel in self.0.drain(..) {
            if let Ok(abs) = crate::shm::rel2abs(rel) {
                crate::eval_arena::forget_if_active(abs);
                let _ = crate::shm::shfree(abs);
            }
        }
    }
}

fn slot_owns(rel: RelPtr) -> RelPtr {
    if rel != shm_types_crate::RELNULL {
        if let Ok(abs) = crate::shm::rel2abs(rel) {
            crate::eval_arena::forget_if_active(abs);
            crate::shm::hand_on_reference();
        }
    }
    rel
}

// SHM-8: release a block no process counts: a slot's, or one another process allocated.
fn free_uncounted(abs: crate::shm::AbsPtr) {
    crate::shm::free_uncounted(abs);
}

#[cfg(test)]
static RELEASE_GAP_HOOK: Mutex<Option<fn(&RegistrySlot)>> = Mutex::new(None);
#[cfg(test)]
static READ_GAP_HOOK: Mutex<Option<fn(&RegistrySlot)>> = Mutex::new(None);

// SLOT-8: the fields read since the first load are ordered before this one.
fn generation_after_read(slot: &RegistrySlot) -> u64 {
    std::sync::atomic::fence(std::sync::atomic::Ordering::Acquire);
    slot.generation.load(std::sync::atomic::Ordering::Acquire)
}

// SLOT-8: `copy` only copies; its result is used only once the slot is
// known to have held `claim` throughout. `None` means the handle is stale.
fn versioned_read<T>(
    slot: &RegistrySlot,
    claim: u64,
    copy: impl FnOnce(&RegistrySlot) -> Result<T, MorlocError>,
) -> Result<Option<T>, MorlocError> {
    let before = slot.generation.load(std::sync::atomic::Ordering::Acquire) & GENERATION_MASK;
    if before != claim {
        return Ok(None);
    }
    let copied = copy(slot);
    #[cfg(test)]
    {
        let hook = *READ_GAP_HOOK.lock().unwrap();
        if let Some(hook) = hook {
            hook(slot);
        }
    }
    if generation_after_read(slot) & GENERATION_MASK != before {
        return Ok(None);
    }
    copied.map(Some)
}

// SLOT-8: bounds-checked against the volume, so a torn pointer and
// length pair is an error rather than a read past the mapping.
fn copy_slot_bytes(rel: RelPtr, len: usize) -> Result<Vec<u8>, MorlocError> {
    if rel == shm_types_crate::RELNULL || len == 0 {
        return Ok(Vec::new());
    }
    let abs = crate::shm::rel2abs_extent(rel, len)?;
    Ok(unsafe { std::slice::from_raw_parts(abs, len) }.to_vec())
}

fn release_slot_locked(slot: &RegistrySlot) {
    use std::sync::atomic::Ordering;

    // Release the stdio claim if it names this slot. Must happen before
    // the slot's fields are zeroed so the claim kind is still readable. A
    // slot that lost the race to claim its kind, or one released a second
    // time after its holder died, must not clear another slot's claim.
    if slot.is_stdio.get() != 0 {
        if let Some(claim) = stdio_claim_slot(slot.stdio_kind.get()) {
            let _ = claim.compare_exchange(
                slot_handle(slot), STDIO_UNCLAIMED, Ordering::AcqRel, Ordering::Acquire,
            );
        }
    }

    // SLOT-8: the generation moves before any field or block changes.
    let bump = registry_gen_salt() | 1;
    slot.generation.fetch_add(bump, Ordering::AcqRel);
    std::sync::atomic::fence(Ordering::Release);

    // Free path / schema / subpacket_entries / write_buffer SHM blocks.
    // Best-effort: a leaked block here is bounded by the registry's
    // lifetime (cleaned at nexus shclose), and erroring would obscure
    // the primary `state = FREE` transition. A poisoned slot's pointers
    // may be mid-swap, so its blocks are leaked rather than freed.
    if slot.poisoned.get() == 0 {
        free_slot_blocks(slot);
    }

    clear_slot_fields(slot);
    #[cfg(test)]
    {
        let hook = *RELEASE_GAP_HOOK.lock().unwrap();
        if let Some(hook) = hook {
            hook(slot);
        }
    }

    // Reset call_id to the no-sweep sentinel (which is also the
    // logical "free slot" value -- the sweeper skips it anyway).
    slot.call_id.store(CALL_ID_NO_SWEEP, Ordering::Release);

    // Finally: release the slot. State = FREE is the publication
    // gate that allows other allocators' CAS to succeed.
    slot.state.store(SLOT_STATE_FREE, Ordering::Release);
    // SLOT-9: counted, not woken: each process drops its slot for the stream
    // at its next dispatch end or release service tick.
    if let Some(bell) = release_doorbell() {
        bell.fetch_add(1, Ordering::Release);
    }
}

fn free_slot_blocks(slot: &RegistrySlot) {
    let path = slot.file_path.get();
    if path != shm_types_crate::RELNULL {
        if let Ok(abs) = crate::shm::rel2abs(path) {
            free_uncounted(abs);
        }
    }
    let schema = slot.schema_str.get();
    if schema != shm_types_crate::RELNULL {
        if let Ok(abs) = crate::shm::rel2abs(schema) {
            free_uncounted(abs);
        }
    }
    if slot.kind.get() == MLC_KIND_CHANNEL {
        channel_free_queue(slot);
    }
    let idx = slot.subpacket_entries.get();
    if idx != shm_types_crate::RELNULL {
        if let Ok(abs) = crate::shm::rel2abs(idx) {
            free_uncounted(abs);
        }
    }
    let wbuf = slot.write_buffer.get();
    if wbuf != shm_types_crate::RELNULL {
        if let Ok(abs) = crate::shm::rel2abs(wbuf) {
            free_uncounted(abs);
        }
    }
    // SLOT-10: the queue itself stays with the slot index, for its next stream.
    if let Ok(q) = slot_queue(slot) {
        for spare in q.drain_spares() {
            if let Ok(abs) = crate::shm::rel2abs(spare) {
                free_uncounted(abs);
            }
        }
    }
}

/// End the stream `(slot_idx, gen_claim)` names with a final footer of
/// `status` (or none, for `custody::STATUS_DISCARD`), and release its slot.
/// Waits, unlocked, for its custodian to write everything queued. A stream
/// another process is already ending is refused.
// SLOT-12, SLOT-14, SLOT-15
pub(crate) fn finish_stream(slot_idx: usize, gen_claim: u64, status: u32) -> Result<(), MorlocError> {
    use std::sync::atomic::Ordering;
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "stream slot index {} out of range", slot_idx,
    )))?;
    let waiting = {
        let _guard = SlotGuard::lock_any(slot)?;
        if !slot_generation_is(slot, gen_claim) || slot.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED {
            return Err(MorlocError::Other("the stream was closed".into()));
        }
        let q = if slot.kind.get() == MLC_KIND_OSTREAM {
            slot_queue(slot).ok().filter(|q| q.hosted() && !q.is_stopped())
        } else {
            None
        };
        if let Some(q) = q {
            let poisoned = slot.poisoned.get() != 0;
            let tail = if poisoned || status == crate::custody::STATUS_DISCARD {
                Ok(())
            } else {
                queue_buffer(slot)
            };
            let status = if poisoned || tail.is_err() {
                morloc_runtime_types::packet::FOOTER_STATUS_FAILED as u32
            } else {
                status
            };
            match q.push_close(status) {
                Ok(seq) => Some((q, seq, q.epoch(), tail, poisoned)),
                // SLOT-10: a writer that is gone reads the slot no more.
                Err(_) if q.is_stopped() => {
                    release_slot_locked(slot);
                    None
                }
                Err(e) => return Err(e),
            }
        } else {
            // No writer took the stream, or it has stopped: nothing reads it.
            release_slot_locked(slot);
            None
        }
    };
    invalidate_process_local_slot(pack_handle(gen_claim, slot_idx));
    let Some((q, seq, epoch, tail, poisoned)) = waiting else { return Ok(()) };
    let done = q.wait_done(seq, epoch);
    release_closed_slot(slot_idx, gen_claim);
    if poisoned {
        return Err(died_inside());
    }
    tail?;
    done
}

/// Release the slot of a stream whose close has been written.
pub(crate) fn release_closed_slot(slot_idx: usize, gen_claim: u64) {
    use std::sync::atomic::Ordering;
    let Some(slot) = slot_ref(slot_idx) else { return };
    let Ok(_guard) = SlotGuard::lock_any(slot) else { return };
    if slot_generation_is(slot, gen_claim) && slot.state.load(Ordering::Acquire) == SLOT_STATE_OPEN_SHARED {
        release_slot_locked(slot);
    }
}

pub(crate) fn slot_generation_is_pub(slot: &RegistrySlot, gen_claim: u64) -> bool {
    slot_generation_is(slot, gen_claim)
}

fn clear_slot_fields(slot: &RegistrySlot) {
    slot.file_path.set(shm_types_crate::RELNULL);
    slot.file_path_len.set(0);
    slot.schema_str.set(shm_types_crate::RELNULL);
    slot.schema_str_len.set(0);
    slot.subpacket_entries.set(shm_types_crate::RELNULL);
    slot.subpacket_entries_len.set(0);
    slot.subpacket_entries_cap.set(0);
    slot.cursor.set(0);
    slot.element_count.set(0);
    slot.final_footer.set(0);
    slot.compression_level.set(0);
    slot.body_start.set(0);
    slot.opener_pid.set(0);
    slot.opener_pid_start_time.set(0);
    slot.kind.set(0);
    slot.is_stdio.set(0);
    slot.stdio_kind.set(0);
    slot.staged.set(0);
    slot.write_buffer.set(shm_types_crate::RELNULL);
    slot.write_buffer_index_cap.set(0);
    slot.write_buffer_index_count.set(0);
    slot.write_buffer_data_used.set(0);
    slot.poisoned.set(0);
    slot.data_end.set(0);
    slot.file_dev.set(0);
    slot.file_ino.set(0);

}

/// Whether `local` holds the file lock of a stream that has since ended.
// SLOT-9: a slot for a stream that has ended, whether or not it holds the
// file lock, is disposed of rather than kept.
fn has_ended(handle: i64, local: &ProcessLocalSlot) -> bool {
    use std::sync::atomic::Ordering;
    let (_, idx) = unpack_handle(handle);
    match slot_ref(idx) {
        Some(slot) => {
            slot.generation.load(Ordering::Acquire) & GENERATION_MASK != local.cached_generation
        }
        None => true,
    }
}

/// Dispose of this process's slot for a stream that has ended.
fn finish_ended(_handle: i64, local: ProcessLocalSlot) {
    drop(local);
}

/// Held across a release pass, which frees SHM blocks, so a fork never
/// copies the allocator's lock mid-pass into a child without the thread.
pub(crate) static RELEASE_PASS: Held<()> = Held::new(1, ());

// SLOT-9: the doorbell's count when this process last released ended slots.
static RELEASES_SEEN: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);

// SLOT-9: run at dispatch ends; a pass only when some stream was released.
pub(crate) fn release_ended_if_rung() {
    use std::sync::atomic::Ordering;
    let Some(bell) = release_doorbell() else { return };
    let now = bell.load(Ordering::Acquire);
    let seen = RELEASES_SEEN.swap(now, Ordering::AcqRel);
    if seen != now && !drop_ended_unlocked_slots() {
        // A pass already runs; a later dispatch end tries again.
        let _ = RELEASES_SEEN.compare_exchange(now, seen, Ordering::AcqRel, Ordering::Relaxed);
    }
}

// SLOT-9: never waits: a slot holding a file lock is left to the release
// service, which every holder runs, and a running pass is left alone.
// Returns false if a pass was running.
pub(crate) fn drop_ended_unlocked_slots() -> bool {
    let Some(_pass) = RELEASE_PASS.try_lock() else { return false };
    let ended: Vec<ProcessLocalSlot> = {
        let mut guard = PROCESS_LOCAL_SLOTS.lock();
        let Some(map) = guard.as_mut() else { return true };
        let handles: Vec<i64> = map
            .iter()
            .filter_map(|(h, e)| match e {
                LocalEntry::Idle(l) if !l.holds_lock && has_ended(*h, l) => Some(*h),
                _ => None,
            })
            .collect();
        handles
            .into_iter()
            .filter_map(|h| match map.remove(&h) {
                Some(LocalEntry::Idle(l)) => Some(l),
                Some(mark) => {
                    map.insert(h, mark);
                    None
                }
                None => None,
            })
            .collect()
    };
    drop(ended);
    true
}

fn release_doorbell() -> Option<&'static std::sync::atomic::AtomicU32> {
    use std::sync::atomic::Ordering;
    let base = REGISTRY_BASE.load(Ordering::Acquire);
    if base.is_null() {
        return None;
    }
    // SAFETY: the registry stays mapped until `registry_teardown`, which
    // stops the release service first.
    Some(unsafe { &(*base).release_doorbell })
}

/// The sub-packet index and element count of a stream with no final
/// footer, from its complete sub-packets up to `data_end`.
fn index_unclosed_stream(
    mmap_ptr: AbsPtr,
    mmap_size: u64,
    body_start: u64,
    data_end: u64,
) -> Result<(Vec<morloc_runtime_types::packet::SubpacketEntry>, u64), MorlocError> {
    let scan = forward_scan_subpackets(mmap_ptr, data_end.min(mmap_size), body_start)?;
    // SAFETY: the mapping covers mmap_size bytes; reads stop at data_end.
    let file = unsafe { std::slice::from_raw_parts(mmap_ptr as *const u8, data_end.min(mmap_size) as usize) };
    let mut total = 0u64;
    let mut entries = Vec::with_capacity(scan.subpacket_offsets.len());
    for offset in scan.subpacket_offsets {
        let elem_count = morloc_runtime_types::compression::subpacket_elem_count(file, offset)?;
        total += elem_count;
        entries.push(morloc_runtime_types::packet::SubpacketEntry { offset, elem_count });
    }
    Ok((entries, total))
}

/// Why a stream file could not be opened for writing.
enum WriteOpenError {
    Open(std::io::Error),
    Lock(String),
}

/// Open `path` to write a stream and take the file's lock, retrying until
/// the locked file is still the one the path names. A file renamed over the
/// path between the open and the lock would otherwise be locked and written
/// in its place, out of reach.
fn open_locked_for_writing(path: &str) -> Result<libc::c_int, WriteOpenError> {
    let c_path = std::ffi::CString::new(path.as_bytes()).map_err(|e| {
        WriteOpenError::Open(std::io::Error::new(std::io::ErrorKind::InvalidInput, e))
    })?;
    for _ in 0..8 {
        let fd = unsafe {
            libc::open(c_path.as_ptr(), libc::O_RDWR | libc::O_CREAT | libc::O_CLOEXEC, 0o644)
        };
        if fd < 0 {
            return Err(WriteOpenError::Open(std::io::Error::last_os_error()));
        }
        #[cfg(test)]
        {
            let mut armed = BEFORE_STREAM_LOCK.lock().unwrap();
            if armed.as_ref().is_some_and(|(p, _)| p == path) {
                let (_, hook) = armed.take().expect("checked above");
                drop(armed);
                hook();
            }
        }
        if let Err(why) = lock_stream_file(fd) {
            unsafe { libc::close(fd); }
            return Err(WriteOpenError::Lock(why));
        }
        if path_names(&c_path, fd) {
            return Ok(fd);
        }
        unlock_and_close(fd);
    }
    Err(WriteOpenError::Lock("the file kept being replaced while it was opened".into()))
}

/// Whether `path` still names the file open on `fd`.
pub(crate) fn path_names(path: &std::ffi::CStr, fd: libc::c_int) -> bool {
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::stat(path.as_ptr(), &mut st) } != 0 {
        return false;
    }
    (st.st_dev as u64, st.st_ino as u64) == file_identity(fd)
}

/// Put a fresh, locked, empty file where `path` names the non-empty file
/// locked on `old_fd`, and release the old one. A file being rewritten may
/// be mapped by readers; truncating it in place would pull pages from
/// under them (SIGBUS), while a new file leaves them the one they opened.
/// A symbolic link stays a link: the file it names is the one replaced.
fn replace_with_fresh_file(old_fd: libc::c_int, path: &str) -> Result<libc::c_int, MorlocError> {
    use std::os::unix::ffi::OsStrExt;
    let fail = |e: std::io::Error| {
        unlock_and_close(old_fd);
        MorlocError::Io(e)
    };
    let invalid = |e| std::io::Error::new(std::io::ErrorKind::InvalidInput, e);
    let target = std::fs::canonicalize(path).map_err(fail)?;
    let c_target = std::ffi::CString::new(target.as_os_str().as_bytes()).map_err(|e| fail(invalid(e)))?;
    // A link re-pointed since the lock was taken would name a file this
    // process never locked.
    if !path_names(&c_target, old_fd) {
        unlock_and_close(old_fd);
        return Err(MorlocError::Other(format!(
            "@open OStream '{}': the file changed while it was being opened", path,
        )));
    }
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(old_fd, &mut st) } != 0 {
        return Err(fail(std::io::Error::last_os_error()));
    }
    // The path cannot take a new file (a directory this process may not
    // write, a file mounted on its own): rewrite in place if no stream of
    // this program is reading the file.
    let identity = (st.st_dev as u64, st.st_ino as u64);
    let in_place = |e: &std::io::Error| {
        matches!(e.raw_os_error(), Some(libc::EBUSY | libc::EXDEV | libc::EACCES | libc::EPERM | libc::EROFS))
            && !file_is_being_read(identity)
    };
    let (tmp, fd) = match crate::utility::create_beside(&target, 0o600) {
        Ok(created) => created,
        Err(e) if in_place(&e) => return Ok(old_fd),
        Err(e) => return Err(fail(e)),
    };
    let c_tmp = std::ffi::CString::new(tmp.as_os_str().as_bytes()).map_err(|e| fail(invalid(e)))?;
    let locked = unsafe {
        libc::fchmod(fd, (st.st_mode & 0o7777) as libc::mode_t) == 0
            && libc::flock(fd, libc::LOCK_EX | libc::LOCK_NB) == 0
    };
    if !locked {
        let e = std::io::Error::last_os_error();
        unsafe {
            libc::unlink(c_tmp.as_ptr());
            libc::close(fd);
        }
        return Err(fail(e));
    }
    note_locked_fd(fd);
    if unsafe { libc::rename(c_tmp.as_ptr(), c_target.as_ptr()) } != 0 {
        let e = std::io::Error::last_os_error();
        unsafe { libc::unlink(c_tmp.as_ptr()); }
        unlock_and_close(fd);
        return if in_place(&e) { Ok(old_fd) } else { Err(fail(e)) };
    }
    if let Some(dir) = target.parent() {
        if let Ok(d) = std::fs::File::open(dir) {
            let _ = d.sync_all();
        }
    }
    unlock_and_close(old_fd);
    Ok(fd)
}

/// Whether an input stream of this program has the file `identity` open.
fn file_is_being_read(identity: (u64, u64)) -> bool {
    use std::sync::atomic::Ordering;
    let (slots_base, slot_count) = registry_slot_array();
    if slots_base.is_null() {
        return false;
    }
    (0..slot_count).any(|idx| {
        let slot = unsafe { &*(slots_base.add(idx * STREAM_ENTRY_SIZE) as *const RegistrySlot) };
        slot.state.load(Ordering::Acquire) == SLOT_STATE_OPEN_SHARED
            && (slot.kind.get() == MLC_KIND_IFILE || slot.kind.get() == MLC_KIND_ISTREAM)
            && (slot.file_dev.get(), slot.file_ino.get()) == identity
    })
}

/// Run between opening the named stream file for writing and locking it.
#[cfg(test)]
pub(crate) static BEFORE_STREAM_LOCK: Mutex<Option<(String, Box<dyn Fn() + Send>)>> = Mutex::new(None);

/// Take the exclusive lock on a stream file opened for writing. Returns
/// the reason the lock was refused.
pub(crate) fn lock_stream_file(fd: libc::c_int) -> Result<(), String> {
    if unsafe { libc::flock(fd, libc::LOCK_EX | libc::LOCK_NB) } == 0 {
        note_locked_fd(fd);
        return Ok(());
    }
    Err(std::io::Error::last_os_error().to_string())
}

/// This process's start stamp: with the pid, it names this process even
/// after the pid is reused. 0 when unknown.
fn read_pid_start_time() -> u64 {
    morloc_runtime_types::process::start_time(std::process::id())
}

// FORK-14: a pid alone names another process once reused, here or in another pid namespace.
fn is_this_process(pid: u32, start: u64) -> bool {
    pid == std::process::id() && (start == 0 || start as u32 == morloc_runtime_types::process::token() as u32)
}

/// Pull the current dispatch's `call_id` from thread-local storage.
/// Returns `CALL_ID_NO_SWEEP` (= 0) if no call is active, which
/// means the slot will never be swept by the per-call_id sweeper
/// (a deliberate no-op for handles opened outside a dispatch, e.g.
/// from unit tests).
pub(crate) fn current_call_id_for_open() -> u64 {
    current_call_id()
}

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
    // SAFETY: bytes is a live slice.
    let abs = unsafe { crate::shm::shmemcpy(bytes.as_ptr(), bytes.len()) }?;
    crate::shm::abs2rel(abs)
}

/// Allocate an SHM-resident copy of a `[SubpacketEntry]` array (used
/// for the IFile sub-packet entry array). Returns the RelPtr.
fn shm_copy_entries_slice(
    slice: &[morloc_runtime_types::packet::SubpacketEntry],
) -> Result<RelPtr, MorlocError> {
    let bytes = unsafe {
        std::slice::from_raw_parts(
            slice.as_ptr() as *const u8,
            slice.len() * std::mem::size_of::<morloc_runtime_types::packet::SubpacketEntry>(),
        )
    };
    shm_copy_bytes(bytes)
}

/// Open a file as `IFile` against the shared SHM registry. Allocates
/// a slot, mmaps the file, parses its stream metadata, publishes the
/// path + schema + subpacket_entries into SHM, and returns the handle.
///
/// Rejects STREAM files lacking a final footer (the 2026-06-28
/// design contract: random access requires the full index, which
/// only a clean close writes).
pub fn shared_open_ifile(path: &str) -> Result<i64, MorlocError> {
    use std::sync::atomic::Ordering;

    reject_dev_stdio_path(path)?;

    // mmap + parse the file BEFORE we touch the registry, so a bad
    // file doesn't leave a half-initialised slot.
    let (map_file, mmap_ptr, mmap_size) = mmap_file_readonly_keep(path)?;
    let (file_dev, file_ino) = file_identity_of(&map_file);
    drop(map_file);
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
    let (slot_idx, slot, _guard) = match allocate_slot_cas() {
        Ok(s) => s,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };

    // Publish path + schema + subpacket_entries. On any error, release
    // the slot via `release_slot_locked` (which itself shfree's any
    // partials we may have already published).
    let publish_result = (|| -> Result<u64, MorlocError> {
        let mut pending = Unpublished::default();
        let path_rel = pending.hold(shm_copy_bytes(path.as_bytes())?);
        let schema_rel = pending.hold(shm_copy_bytes(parsed.schema_str.as_bytes())?);
        let idx_rel = if !parsed.subpacket_entries.is_empty() {
            pending.hold(shm_copy_entries_slice(&parsed.subpacket_entries)?)
        } else {
            shm_types_crate::RELNULL
        };
        // SAFETY: we hold the slot's lock; state is OPEN_SHARED but
        // generation has NOT been bumped to the new value yet, so no
        // cross-pool reader can observe these field writes (the
        // versioned-pointer pattern gates on the new generation).
        unsafe {
            slot.kind.set(MLC_KIND_IFILE);
            slot.file_dev.set(file_dev);
            slot.file_ino.set(file_ino);
            slot.file_path.set(pending.own(path_rel));
            slot.file_path_len.set(path.len() as u32);
            slot.schema_str.set(pending.own(schema_rel));
            slot.schema_str_len.set(parsed.schema_str.len() as u32);
            slot.subpacket_entries.set(pending.own(idx_rel));
            slot.subpacket_entries_len.set(parsed.subpacket_entries.len() as u64);
            // IFile's sub-packet entry array is immutable -- set once
            // from the parsed final footer and never grown. cap = 0
            // marks "not OStream-growable" so release_slot_locked treats
            // subpacket_entries_len, not _cap, as the freed extent.
            slot.subpacket_entries_cap.set(0);
            slot.body_start.set(parsed.body_start);
            slot.final_footer.set(if parsed.final_footer { 1 } else { 0 });
            slot.cursor.set(0);
            slot.element_count.set(parsed.element_count);
            slot.compression_level.set(0);
            // IFile/IStream don't write; clear buffer fields so
            // release_slot_locked doesn't attempt an spurious shfree
            // on a freshly-allocated never-released slot whose
            // zero-init bytes look like a real volume-0 relptr.
            slot.write_buffer.set(shm_types_crate::RELNULL);
            slot.write_buffer_index_cap.set(0);
            slot.write_buffer_index_count.set(0);
            slot.write_buffer_data_used.set(0);
            if let Some(d) = parsed.diag.as_ref() {
                *slot.diag.get() = *d;
            }
        }
        // Bump generation by the salted random increment. This is
        // the publication store: cross-pool readers Acquire-load
        // this and observe all prior writes happens-before.
        let bump = registry_gen_salt() | 1;
        // wrapping_add: generation is a wrapping counter masked to
        // GENERATION_MASK; a large random salt overflows u64 (debug panic).
        let new_gen = slot.generation.fetch_add(bump, Ordering::AcqRel).wrapping_add(bump) & GENERATION_MASK;
        Ok(new_gen)
    })();
    let new_gen = match publish_result {
        Ok(g) => g,
        Err(e) => {
            // Roll back the slot. release_slot_locked shfree's any
            // RelPtrs we've published; the lock guard releases on
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
        pages_dropped: 0,
        map_file: None,
        fd: -1,
        cache: crate::fork_policy::ForkLocal::new(Box::new(StreamCache::new(cap_bytes))),
        value_schema: parsed.value_schema.clone(),
        elem_schema: parsed.elem_schema.clone(),
        subpacket_entries_local: parsed.subpacket_entries.clone(),
        subpacket_elem_cum: None,
        is_data_packet: parsed.is_data_packet,
        holds_lock: false,
        fork_epoch: fork_epoch(),
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

    reject_dev_stdio_path(path)?;

    let (map_file, mmap_ptr, mmap_size) = mmap_file_readonly_keep(path)?;
    let (file_dev, file_ino) = file_identity_of(&map_file);
    let parsed = match parse_stream_file(path, mmap_ptr, mmap_size) {
        Ok(p) => p,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };
    // IStream reads element-by-element, so the file's value must be
    // list-shaped. STREAM_PACKET files always are (writer invariant);
    // a DATA_PACKET holding a scalar value cannot be read as IStream.
    if let Err(e) = reject_non_list_stream_schema(
        &parsed.value_schema, "IStream open", path,
    ) {
        unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
        return Err(e);
    }
    // Forward-only access: kernel readahead is the right hint.
    unsafe {
        libc::madvise(
            mmap_ptr as *mut libc::c_void,
            mmap_size as usize,
            libc::MADV_SEQUENTIAL,
        );
    }

    let (slot_idx, slot, _guard) = match allocate_slot_cas() {
        Ok(s) => s,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };

    let publish_result = (|| -> Result<u64, MorlocError> {
        let mut pending = Unpublished::default();
        let path_rel = pending.hold(shm_copy_bytes(path.as_bytes())?);
        let schema_rel = pending.hold(shm_copy_bytes(parsed.schema_str.as_bytes())?);
        unsafe {
            slot.kind.set(MLC_KIND_ISTREAM);
            slot.file_dev.set(file_dev);
            slot.file_ino.set(file_ino);
            slot.file_path.set(pending.own(path_rel));
            slot.file_path_len.set(path.len() as u32);
            slot.schema_str.set(pending.own(schema_rel));
            slot.schema_str_len.set(parsed.schema_str.len() as u32);
            slot.subpacket_entries.set(shm_types_crate::RELNULL);
            slot.subpacket_entries_len.set(0);
            slot.subpacket_entries_cap.set(0);
            slot.body_start.set(parsed.body_start);
            slot.final_footer.set(if parsed.final_footer { 1 } else { 0 });
            // IStream walks by cursor starting at body_start.
            slot.cursor.set(parsed.body_start);
            slot.data_end.set(parsed.data_end);
            slot.element_count.set(parsed.element_count);
            slot.compression_level.set(0);
            slot.write_buffer.set(shm_types_crate::RELNULL);
            slot.write_buffer_index_cap.set(0);
            slot.write_buffer_index_count.set(0);
            slot.write_buffer_data_used.set(0);
            if let Some(d) = parsed.diag.as_ref() {
                *slot.diag.get() = *d;
            }
        }
        let bump = registry_gen_salt() | 1;
        // wrapping_add: generation is a wrapping counter masked to
        // GENERATION_MASK; a large random salt overflows u64 (debug panic).
        let new_gen = slot.generation.fetch_add(bump, Ordering::AcqRel).wrapping_add(bump) & GENERATION_MASK;
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
        pages_dropped: 0,
        map_file: Some(map_file),
        fd: -1,
        cache: crate::fork_policy::ForkLocal::new(Box::new(StreamCache::new(cap_bytes))),
        value_schema: parsed.value_schema.clone(),
        elem_schema: parsed.elem_schema.clone(),
        subpacket_entries_local: parsed.subpacket_entries.clone(),
        subpacket_elem_cum: None,
        is_data_packet: parsed.is_data_packet,
        holds_lock: false,
        fork_epoch: fork_epoch(),
    };
    let handle = pack_handle(new_gen, slot_idx);
    install_process_local_slot(handle, local);
    Ok(handle)
}

/// Whether the process that opened a claim, `pid` with start stamp
/// `opener_start_time` (0 when unknown), may still be running. Errs toward
/// alive, so a live owner is never reclaimed.
fn stdio_owner_is_alive(pid: u32, opener_start_time: u64) -> bool {
    morloc_runtime_types::process::alive(pid, opener_start_time)
}

/// The registry's online self-heal for a stale/corrupt stdio claim. If the
/// set claim `existing` is unrecoverable garbage (its handle unpacks to an
/// out-of-range/freed/non-stdio slot) or its owning process is gone, clear
/// it so a fresh open can proceed, and return true. A claim held by THIS
/// process, or by a live other process, is left intact so a genuine
/// double-open still errors.
///
/// Without this a leaked or corrupt claim would wedge every @stdout open
/// until the owning process dies (so the nexus poll's `sweep_per_pid`
/// fires) or the daemon restarts.
fn try_reclaim_stale_stdio_claim(
    claim: &std::sync::atomic::AtomicI64,
    existing: i64,
) -> bool {
    use std::sync::atomic::Ordering;
    let (gen_claim, slot_idx) = unpack_handle(existing);
    let slot = match slot_ref(slot_idx) {
        Some(s) => s,
        // Handle unpacks to an out-of-range slot: it cannot correspond to
        // any live owner. Clear it directly.
        None => {
            return claim
                .compare_exchange(existing, STDIO_UNCLAIMED, Ordering::AcqRel, Ordering::Acquire)
                .is_ok();
        }
    };
    let dead_owner = {
        let Ok(_guard) = SlotGuard::lock_any(slot) else { return false };
        if claim.load(Ordering::Acquire) != existing {
            // Someone else changed the claim under us; the caller re-loads.
            return false;
        }
        let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
        let slot_ok = slot.state.load(Ordering::Acquire) == SLOT_STATE_OPEN_SHARED
            && slot.is_stdio.get() != 0
            && gen_now == gen_claim;
        if !slot_ok {
            // Claim points at a freed / reused / non-stdio slot: garbage.
            return claim
                .compare_exchange(existing, STDIO_UNCLAIMED, Ordering::AcqRel, Ordering::Acquire)
                .is_ok();
        }
        !is_this_process(slot.opener_pid.get(), slot.opener_pid_start_time.get())
            && !stdio_owner_is_alive(slot.opener_pid.get(), slot.opener_pid_start_time.get())
    };
    // A dead owner's stream is finished, which clears the claim.
    dead_owner
        && finish_stream(slot_idx, gen_claim, morloc_runtime_types::packet::FOOTER_STATUS_PAUSED as u32).is_ok()
}

/// Open one of stdin/stdout/stderr as a stream handle. The nexus is
/// the sole owner of fd 0/1/2; this call registers a slot that routes
/// `mlc_next` / `mlc_write` through the pool-nexus socket rather than
/// opening any fd of its own.
///
/// `kind` must be `MLC_KIND_ISTREAM` for `STDIO_KIND_STDIN`, or
/// `MLC_KIND_OSTREAM` for `STDIO_KIND_STDOUT` / `STDIO_KIND_STDERR`.
/// Any other pairing is a caller bug.
///
/// Uniqueness across every pool attached to this nexus: the shared
/// header carries three `AtomicI64` claim slots. Second open of the
/// same stdio kind returns a generation-mismatch-shaped error
/// pointing at "already opened."
pub fn open_stdio(kind: u8, stdio_kind: u8, schema_str: &str)
    -> Result<i64, MorlocError>
{
    use std::sync::atomic::Ordering;

    // The declared schema is published to the nexus, which compares it
    // against the schema on each incoming packet. A pool passes the
    // compiler's hint-bearing string and the wire never carries hints,
    // so store the canonical form.
    let schema_owned =
        morloc_runtime_types::schema::canonicalize_schema_str(schema_str);
    let staged = kind == MLC_KIND_OSTREAM
        && stdio_kind == STDIO_KIND_STDOUT
        && stdout_staged();
    let schema_str: &str = &schema_owned;

    // Pool processes attach lazily; without this, the first stdio open
    // from a pool sees a null REGISTRY_BASE and stdio_claim_slot
    // returns None. Idempotent + cheap on the fast path.
    registry_init()?;

    let claim = stdio_claim_slot(stdio_kind).ok_or_else(|| {
        MorlocError::Other(format!(
            "open_stdio: unknown stdio_kind {}", stdio_kind,
        ))
    })?;

    // Best-effort pre-check so the common "already open" case avoids
    // burning a slot allocation. The real gate is the CAS below. If the
    // claim is set but stale/corrupt (dead owner, or a garbage handle),
    // reclaim it so a fresh open can proceed instead of wedging forever.
    let mut existing = claim.load(Ordering::Acquire);
    if existing != STDIO_UNCLAIMED && try_reclaim_stale_stdio_claim(claim, existing) {
        existing = claim.load(Ordering::Acquire);
    }
    if existing != STDIO_UNCLAIMED {
        // Not reclaimed => a live owner (this process, or another). Report
        // the owner pid so a genuine wedge is diagnosable in the field.
        let (_g, owner_idx) = unpack_handle(existing);
        let owner_pid = slot_ref(owner_idx).map(|s| s.opener_pid.get()).unwrap_or(0);
        return Err(MorlocError::Other(format!(
            "@{} already open in this nexus (handle {:#x}, owner pid {}); \
             at most one open per stdio kind is allowed",
            stdio_kind_name(stdio_kind), existing, owner_pid,
        )));
    }

    // Pre-parse schema so the pool and the nexus (which parses the same
    // string in mlc_stdio_build_stream_header) agree on the stream
    // header's byte length. That length seeds body_start / cursor so
    // the subpacket_entries we accumulate carry meaningful stream-
    // position offsets (matches on-disk offsets for a naive `> file`
    // redirect; still meaningful as a stream position on pipes).
    // The schema string is the value schema `[a]` -- streams are
    // list-shaped and the on-disk metadata must match the wire data.
    let value_schema_parsed = if schema_str.is_empty() {
        morloc_runtime_types::schema::Schema::primitive(
            morloc_runtime_types::schema::SerialType::Nil,
        )
    } else if kind == MLC_KIND_OSTREAM {
        parse_schema(schema_str).map_err(|e| MorlocError::Schema(format!(
            "open_stdio: unparseable schema '{}': {}", schema_str, e,
        )))?
    } else {
        parse_schema(schema_str).unwrap_or_else(|_| {
            morloc_runtime_types::schema::Schema::primitive(
                morloc_runtime_types::schema::SerialType::Nil,
            )
        })
    };
    if kind == MLC_KIND_OSTREAM && !schema_str.is_empty() {
        reject_non_list_stream_schema(
            &value_schema_parsed, "open_stdio", stdio_kind_name(stdio_kind),
        )?;
    }
    let stdio_body_start: u64 = if kind == MLC_KIND_OSTREAM {
        morloc_runtime_types::packet::make_stream_header_block(
            &value_schema_parsed,
        ).len() as u64
    } else {
        0
    };

    // Mint a call_id if the caller has not set one, before the slot is
    // allocated and tagged with it, so the post-dispatch stdio reclaim
    // (pool_reclaim_stdio_after_dispatch) can match this slot. The nexus
    // dispatch always pre-sets a call_id, so this only fires in pool
    // processes, and only when a stdio handle is opened.
    if current_call_id() == CALL_ID_NO_SWEEP {
        set_current_call_id(generate_call_id());
    }
    let (slot_idx, slot, _guard) = allocate_slot_cas()?;

    // OStream stdio slots buffer writes through the same pipeline as
    // file-backed OStreams; they need a real write_buffer. IStream
    // stdio slots pull sub-packets straight from the RPC and don't
    // need one.
    let want_write_buffer = kind == MLC_KIND_OSTREAM;

    let publish_result = (|| -> Result<u64, MorlocError> {
        let mut pending = Unpublished::default();
        let sentinel = match stdio_kind {
            STDIO_KIND_STDERR => STDIO_SENTINEL_ERR,
            _ => STDIO_SENTINEL_STD,
        };
        let path_rel = pending.hold(shm_copy_bytes(sentinel.as_bytes())?);
        let schema_rel = pending.hold(shm_copy_bytes(schema_str.as_bytes())?);
        let (buf_rel, buf_size) = if want_write_buffer {
            let buf_bytes = read_write_buffer_bytes_env();
            let buf_abs = crate::shm::shcalloc(1, buf_bytes)?;
            (pending.hold(crate::shm::abs2rel(buf_abs)?), buf_bytes)
        } else {
            (shm_types_crate::RELNULL, 0)
        };
        let _ = buf_size;
        // OStream stdio slots grow a shared subpacket_entries array
        // just like file-backed OStreams, so `@close`'s final footer
        // can carry a real index. Offsets are stream-position, not fd
        // position; they're accurate whenever the consumer starts
        // reading from the beginning of the emitted output (the
        // `> file` case).
        if kind == MLC_KIND_OSTREAM {
            ready_slot_queue(slot)?;
        }
        let (idx_rel, idx_cap) = (shm_types_crate::RELNULL, 0);
        unsafe {
            slot.kind.set(kind);
            slot.is_stdio.set(1);
            slot.stdio_kind.set(stdio_kind);
            slot.staged.set(staged as u8);
            slot.file_path.set(pending.own(path_rel));
            slot.file_path_len.set(sentinel.len() as u32);
            slot.schema_str.set(pending.own(schema_rel));
            slot.schema_str_len.set(schema_str.len() as u32);
            slot.subpacket_entries.set(pending.own(idx_rel));
            slot.subpacket_entries_len.set(0);
            slot.subpacket_entries_cap.set(idx_cap);
            slot.body_start.set(stdio_body_start);
            slot.final_footer.set(0);
            slot.cursor.set(stdio_body_start);
            slot.element_count.set(0);
            slot.compression_level.set(0);
            *slot.diag.get() = StreamDiag::new();
            slot.write_buffer.set(pending.own(buf_rel));
            slot.write_buffer_index_cap.set(0);
            slot.write_buffer_index_count.set(0);
            slot.write_buffer_data_used.set(0);
        }
        let bump = registry_gen_salt() | 1;
        // wrapping_add: the generation is a wrapping counter masked to
        // GENERATION_MASK; a large random salt can overflow u64, which
        // panics in debug builds without the wrapping form.
        let new_gen = slot.generation.fetch_add(bump, Ordering::AcqRel)
            .wrapping_add(bump)
            & GENERATION_MASK;
        Ok(new_gen)
    })();

    let new_gen = match publish_result {
        Ok(g) => g,
        Err(e) => { release_slot_locked(slot); return Err(e); }
    };
    let handle = pack_handle(new_gen, slot_idx);

    // Real uniqueness gate. If we lose the race, roll back the slot.
    if claim.compare_exchange(
        STDIO_UNCLAIMED, handle, Ordering::AcqRel, Ordering::Acquire,
    ).is_err() {
        release_slot_locked(slot);
        return Err(MorlocError::Other(format!(
            "@{} already open in this nexus; at most one open per \
             stdio kind is allowed", stdio_kind_name(stdio_kind),
        )));
    }
    drop(_guard);
    // SLOT-16
    if kind == MLC_KIND_OSTREAM {
        if let Err(e) = crate::custody::adopt(handle) {
            if let Ok(_g) = SlotGuard::lock_any(slot) {
                if slot_generation_is(slot, new_gen) {
                    release_slot_locked(slot);
                }
            }
            return Err(e);
        }
    }

    // Install a process-local slot so the buffered write path
    // (`with_process_local_slot` -> `append_one_element`) can flow
    // straight through the same machinery as file-backed OStreams.
    // For OStream, value_schema is `[a]` and elem_schema is
    // parameters[0]. For IStream, both fields hold the placeholder --
    // IStream stdio reads discover the real element type from
    // incoming packet headers.
    let (value_schema_cached, elem_schema_cached) =
        if kind == MLC_KIND_OSTREAM && !schema_str.is_empty() {
            let elem = value_schema_parsed.parameters[0].clone();
            (value_schema_parsed, elem)
        } else {
            let value = value_schema_parsed.clone();
            (value, value_schema_parsed)
        };
    let local = ProcessLocalSlot {
        cached_generation: new_gen,
        mmap_ptr: std::ptr::null_mut(),
        mmap_size: 0,
        pages_dropped: 0,
        map_file: None,
        fd: -1,                    // stdio writes go through RPC, not fd
        cache: crate::fork_policy::ForkLocal::new(Box::new(StreamCache::new(0))),
        value_schema: value_schema_cached,
        elem_schema: elem_schema_cached,
        subpacket_entries_local: Vec::new(),
        subpacket_elem_cum: None,
        is_data_packet: false,
        holds_lock: false,
        fork_epoch: fork_epoch(),
    };
    install_process_local_slot(handle, local);
    Ok(handle)
}

fn stdio_kind_name(k: u8) -> &'static str {
    match k {
        STDIO_KIND_STDIN  => "stdin",
        STDIO_KIND_STDOUT => "stdout",
        STDIO_KIND_STDERR => "stderr",
        _ => "?stdio",
    }
}

/// Return the stdio kind of `handle`, or `Ok(None)` if the handle
/// refers to a file-backed slot. Verifies the generation only; does
/// NOT check opener PID -- both pool-side ops and the nexus's RPC
/// dispatch call this to route by kind, and the nexus is never the
/// opener. Callers on the pool side that need the fork-boundary gate
/// must additionally call `verify_stdio_opener_pid`.
pub fn shared_handle_stdio_kind(handle: i64) -> Result<Option<u8>, MorlocError> {
    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "shared_handle_stdio_kind: slot index {} out of range", slot_idx,
    )))?;
    let (is_stdio, kind) = versioned_read(slot, gen_claim, |s| Ok((s.is_stdio.get(), s.stdio_kind.get())))?
        .ok_or_else(|| MorlocError::Other(format!(
            "shared_handle_stdio_kind: handle {:#x} names a closed stream", handle,
        )))?;
    Ok((is_stdio != 0).then_some(kind))
}

/// Enforce the pool-side fork-boundary invariant: the caller's PID
/// must match the slot's opener_pid. A forked child that inherits
/// the nexus socket cannot write to the parent's stdio slot -- its
/// bytes would interleave with the parent's on the same fd in an
/// order the user has no way to reason about.
pub fn verify_stdio_opener_pid(handle: i64) -> Result<(), MorlocError> {
    use std::sync::atomic::Ordering;
    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "verify_stdio_opener_pid: slot index {} out of range", slot_idx,
    )))?;
    let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_now != gen_claim {
        return Err(MorlocError::Other(format!(
            "verify_stdio_opener_pid: generation mismatch (claim {}, slot {})",
            gen_claim, gen_now,
        )));
    }
    let opener = slot.opener_pid.get();
    let me = std::process::id();
    if !is_this_process(opener, slot.opener_pid_start_time.get()) {
        return Err(MorlocError::Other(format!(
            "stdio stream cannot cross a fork boundary: slot opened by \
             PID {}, current process is PID {}. Re-open the stream in \
             this process, or route the read/write through the opener.",
            opener, me,
        )));
    }
    Ok(())
}

// ── Stdio RPC client (pool side) ─────────────────────────────────────────
//
// Pool-side of the stdio protocol described in
// `morloc-nexus/src/stdio_server.rs`. Connects to the socket exported
// via `MORLOC_NEXUS_STDIO_SOCK`, sends a request, waits for the
// response synchronously. The connection is opened lazily and cached
// in thread-local storage so tight `@next` loops don't pay the
// connect cost per call.

use morloc_runtime_types::stdio_proto::{
    OP_NEXT_STDIO, STATUS_OK, STATUS_ERR, STATUS_EOF,
};

thread_local! {
    // FORK-14
    static STDIO_SOCK: std::cell::RefCell<Option<(u64, std::os::unix::net::UnixStream)>> =
        std::cell::RefCell::new(None);
}

fn stdio_sock_connect() -> Result<std::os::unix::net::UnixStream, MorlocError> {
    let set = std::env::var("MORLOC_NEXUS_STDIO_SOCK").ok();
    #[cfg(test)]
    let set = set.or_else(|| crate::custody::TEST_SOCK.get().cloned());
    let path = set.ok_or_else(|| MorlocError::Other(
        "@stdin/@stdout/@stderr: MORLOC_NEXUS_STDIO_SOCK is not set. The \
         pool was not started by a morloc nexus, or the nexus failed to \
         bind its stdio server.".into(),
    ))?;
    std::os::unix::net::UnixStream::connect(&path).map_err(|e| MorlocError::Other(
        format!("@stdio: connect({}): {}", path, e),
    ))
}

pub(crate) fn with_stdio_sock<R>(
    f: impl FnOnce(&mut std::os::unix::net::UnixStream) -> Result<R, MorlocError>,
) -> Result<R, MorlocError> {
    // FORK-14
    let generation = crate::fork_policy::generation();
    let mut f = Some(f);
    let cached = STDIO_SOCK.try_with(|cell| {
        let mut opt = cell.borrow_mut();
        if opt.as_ref().map_or(true, |(owner, _)| *owner != generation) {
            *opt = Some((generation, stdio_sock_connect()?));
        }
        let (_, s) = opt.as_mut().expect("populated above");
        let result = (f.take().expect("called once"))(s);
        if result.is_err() {
            *opt = None;
        }
        result
    });
    match cached {
        Ok(r) => r,
        // FORK-5: prepare's drain can run from a thread-local destructor.
        Err(_) => (f.take().expect("not called"))(&mut stdio_sock_connect()?),
    }
}

pub(crate) fn read_error_message(stream: &mut std::os::unix::net::UnixStream) -> String {
    use std::io::Read;
    let mut len_buf = [0u8; 4];
    if stream.read_exact(&mut len_buf).is_err() {
        return "nexus stdio server closed unexpectedly".into();
    }
    let len = u32::from_le_bytes(len_buf) as usize;
    let mut buf = vec![0u8; len];
    if stream.read_exact(&mut buf).is_err() {
        return "nexus stdio server closed while sending error body".into();
    }
    String::from_utf8_lossy(&buf).into_owned()
}

/// Ask the nexus to start and watch a channel's producer; see `mlc_spawn`.
pub fn nexus_spawn(handle: i64, mid: u32, socket_path: &[u8], packets: &[&[u8]])
    -> Result<(), MorlocError>
{
    use std::io::{Read, Write};
    use morloc_runtime_types::stdio_proto::OP_SPAWN;
    let mut req: Vec<u8> = Vec::new();
    req.push(OP_SPAWN);
    req.extend_from_slice(&handle.to_le_bytes());
    req.extend_from_slice(&mid.to_le_bytes());
    req.extend_from_slice(&(socket_path.len() as u32).to_le_bytes());
    req.extend_from_slice(socket_path);
    req.extend_from_slice(&(packets.len() as u32).to_le_bytes());
    for p in packets {
        req.extend_from_slice(&(p.len() as u64).to_le_bytes());
        req.extend_from_slice(p);
    }
    with_stdio_sock(|s| {
        s.write_all(&req).map_err(|e| MorlocError::Other(format!("@spawn: send: {}", e)))?;
        let mut status = [0u8; 1];
        s.read_exact(&mut status).map_err(|e| MorlocError::Other(
            format!("@spawn: recv status: {}", e),
        ))?;
        match status[0] {
            STATUS_OK => Ok(()),
            STATUS_ERR => Err(MorlocError::Other(format!("@spawn: {}", read_error_message(s)))),
            other => Err(MorlocError::Other(format!("@spawn: unknown status byte {}", other))),
        }
    })
}

/// `@next` on a stdio-bound IStream. Round-trips one sub-packet
/// through the nexus. The returned pointer is a fresh SHM block
/// holding the sub-packet's `MORLOC_DATA_PACKET` bytes; the caller's
/// deserializer materializes it into `[a]`.
fn stdio_next_via_rpc(handle: i64, stdio_kind: u8)
    -> Result<AbsPtr, MorlocError>
{
    if stdio_kind != STDIO_KIND_STDIN {
        return Err(MorlocError::Other(format!(
            "@next requires an @stdin handle, got {}",
            stdio_kind_name(stdio_kind),
        )));
    }
    use std::io::{Read, Write};
    with_stdio_sock(|s| {
        let mut req = [0u8; 9];
        req[0] = OP_NEXT_STDIO;
        req[1..9].copy_from_slice(&handle.to_le_bytes());
        s.write_all(&req).map_err(|e| MorlocError::Other(
            format!("@next: send: {}", e),
        ))?;
        let mut status = [0u8; 1];
        s.read_exact(&mut status).map_err(|e| MorlocError::Other(
            format!("@next: recv status: {}", e),
        ))?;
        match status[0] {
            STATUS_OK => {
                let mut resp = [0u8; 16];
                s.read_exact(&mut resp).map_err(|e| MorlocError::Other(
                    format!("@next: recv body: {}", e),
                ))?;
                let relptr = i64::from_le_bytes(resp[0..8].try_into().unwrap());
                let _size = u64::from_le_bytes(resp[8..16].try_into().unwrap());
                let packet_abs = crate::shm::rel2abs(relptr as crate::shm::RelPtr)?;
                // The nexus wrote a self-contained data packet
                // (header + metadata + payload) into SHM. Decode into
                // an SHM Array<a> using the slot's cached element
                // schema, then free the packet buffer.
                let result = stdio_decode_packet(handle, packet_abs);
                free_uncounted(packet_abs);
                result
            }
            STATUS_EOF => empty_shm_array(),
            STATUS_ERR => Err(MorlocError::Other(format!(
                "@next: {}", read_error_message(s),
            ))),
            other => Err(MorlocError::Other(format!(
                "@next: unknown status byte {}", other,
            ))),
        }
    })
}

/// Decode an SHM-resident data packet into an `Array<a>` voidstar
/// using the slot's cached element schema. The packet was written by
/// the nexus on receipt from stdin; we route it through the same
/// `get_morloc_data_packet_value` reader every file-backed packet
/// goes through.
fn stdio_decode_packet(handle: i64, packet_abs: crate::shm::AbsPtr)
    -> Result<crate::shm::AbsPtr, MorlocError>
{
    // The slot's schema is the full value schema `[a]` -- streams are
    // list-shaped -- so the reader materializes `[a]` directly.
    let value_schema_str = shared_handle_schema_str(handle)?;
    let c_schema = std::ffi::CString::new(value_schema_str.as_bytes())
        .map_err(|_| MorlocError::Other(
            "@next: schema string contains NUL".into(),
        ))?;
    use crate::ffi::parse_schema;
    use crate::ffi::free_schema;
    use crate::packet_ffi::get_morloc_data_packet_value;
    unsafe {
        let mut err: *mut std::os::raw::c_char = std::ptr::null_mut();
        let schema = parse_schema(c_schema.as_ptr(), &mut err);
        if schema.is_null() {
            let msg = if !err.is_null() {
                let m = std::ffi::CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut std::ffi::c_void);
                m
            } else {
                "@next: parse_schema failed".to_string()
            };
            return Err(MorlocError::Other(msg));
        }
        let voidstar = get_morloc_data_packet_value(
            packet_abs as *const u8, schema, &mut err,
        );
        free_schema(schema);
        if voidstar.is_null() {
            let msg = if !err.is_null() {
                let m = std::ffi::CStr::from_ptr(err).to_string_lossy().into_owned();
                libc::free(err as *mut std::ffi::c_void);
                m
            } else {
                "@next: get_morloc_data_packet_value returned NULL".to_string()
            };
            return Err(MorlocError::Other(msg));
        }
        Ok(voidstar as crate::shm::AbsPtr)
    }
}

/// True for the stdin fd-0 aliases. `@open` on stdin routes an IStream
/// through the nexus RPC channel (the pool does not own fd 0) and rejects
/// IFile (a pipe is not seekable). The nexus only ever emits the
/// `/dev/stdin` sentinel, but a user may type any alias, so recognize all.
fn is_stdin_device(path: &str) -> bool {
    matches!(path, "/dev/stdin" | "/dev/fd/0" | "/proc/self/fd/0")
}

/// Refuse `@open` on the `/dev/std{in,out,err}` paths. Opening them
/// as a regular file would race the nexus's own reads/writes on
/// fd 0/1/2. The user must route through `@stdin` / `@stdout` /
/// `@stderr` intrinsics instead.
fn reject_dev_stdio_path(path: &str) -> Result<(), MorlocError> {
    match path {
        "/dev/stdin" | "/dev/stdout" | "/dev/stderr" => Err(MorlocError::Other(
            format!(
                "@open '{}' is refused: opening stdio via a file path \
                 would race the nexus's own I/O on fd 0/1/2. Use the \
                 `@stdin` / `@stdout` / `@stderr` intrinsics instead.",
                path,
            ),
        )),
        _ => Ok(()),
    }
}

pub fn shared_open_ostream_with_schema(
    path: &str,
    schema_str: &str,
) -> Result<i64, MorlocError> {
    reject_dev_stdio_path(path)?;
    // Parse schema first so a malformed spec never destroys prior content.
    ostream_schema(schema_str, "OStream open", path)?;
    crate::custody::open(
        morloc_runtime_types::stdio_proto::OPEN_CREATE,
        &absolute_path(path)?,
        schema_str,
    )
}

/// `path` resolved against this process's working directory, which the
/// custodian does not share.
fn absolute_path(path: &str) -> Result<String, MorlocError> {
    let abs = std::path::absolute(path).map_err(MorlocError::Io)?;
    abs.into_os_string().into_string().map_err(|_| MorlocError::Other(format!(
        "stream path '{}' is not valid UTF-8", path,
    )))
}

/// The value schema `[a]` of a written stream; empty is the bridge's
/// placeholder.
fn ostream_schema(schema_str: &str, what: &str, path: &str) -> Result<Schema, MorlocError> {
    let parsed = if schema_str.is_empty() {
        Schema::primitive(SerialType::Nil)
    } else {
        parse_schema(schema_str).map_err(|e| MorlocError::Schema(format!(
            "{}: unparseable schema '{}': {}", what, schema_str, e,
        )))?
    };
    reject_non_list_stream_schema(&parsed, what, path)?;
    Ok(parsed)
}

/// Where a custodian starts writing a file it has locked.
struct Resume {
    cursor: u64,
    body_start: u64,
    entries: Vec<morloc_runtime_types::packet::SubpacketEntry>,
    element_count: u64,
}

/// Open a file `OStream` in the custodian, for the process `(pid, start)`
/// in call `call_id`. `mode` is `stdio_proto::OPEN_CREATE` or `OPEN_APPEND`.
// SLOT-10
pub(crate) fn host_open(
    mode: u8,
    path: &str,
    schema_str: &str,
    pid: u32,
    start: u64,
    call_id: u64,
) -> Result<i64, MorlocError> {
    use morloc_runtime_types::stdio_proto::{OPEN_APPEND, OPEN_CREATE};
    reject_dev_stdio_path(path)?;
    let opener = (pid, start, call_id);
    let mut retried = false;
    loop {
        let res = match mode {
            OPEN_CREATE => host_create(path, schema_str, opener),
            OPEN_APPEND => host_append(path, schema_str, opener),
            other => return Err(MorlocError::Other(format!("unknown stream open mode {other}"))),
        };
        match res {
            Err(HostOpenError::Locked(e)) if !retried && finish_dead_openers_of(path) => {
                retried = true;
                let _ = e;
            }
            Err(HostOpenError::Locked(e)) | Err(HostOpenError::Other(e)) => return Err(e),
            Ok(h) => return Ok(h),
        }
    }
}

enum HostOpenError {
    Locked(MorlocError),
    Other(MorlocError),
}

impl From<MorlocError> for HostOpenError {
    fn from(e: MorlocError) -> Self {
        HostOpenError::Other(e)
    }
}

fn open_for_custody(path: &str, what: &str) -> Result<libc::c_int, HostOpenError> {
    match open_locked_for_writing(path) {
        Ok(fd) => Ok(fd),
        Err(WriteOpenError::Open(e)) => Err(HostOpenError::Other(MorlocError::Io(e))),
        Err(WriteOpenError::Lock(why)) => Err(HostOpenError::Locked(MorlocError::Other(format!(
            "{} '{}': could not acquire exclusive flock: {}", what, path, why,
        )))),
    }
}

/// Finish the streams this custodian writes on `path`'s file whose opener
/// has died, as the kernel would have dropped a dead opener's lock. Returns
/// whether it finished any.
fn finish_dead_openers_of(path: &str) -> bool {
    use std::sync::atomic::Ordering;
    let Ok(meta) = std::fs::metadata(path) else { return false };
    let identity = {
        use std::os::unix::fs::MetadataExt;
        (meta.dev(), meta.ino())
    };
    let mut any = false;
    for (idx, gen) in crate::custody::hosted_slots() {
        let Some(slot) = slot_ref(idx) else { continue };
        if slot.generation.load(Ordering::Acquire) & GENERATION_MASK != gen {
            continue;
        }
        let seen = (slot.file_dev.get(), slot.file_ino.get(), slot.opener_pid.get(), slot.opener_pid_start_time.get());
        if generation_after_read(slot) & GENERATION_MASK != gen || (seen.0, seen.1) != identity {
            continue;
        }
        if !morloc_runtime_types::process::alive(seen.2, seen.3)
            && finish_stream(idx, gen, morloc_runtime_types::packet::FOOTER_STATUS_PAUSED as u32).is_ok()
        {
            any = true;
        }
    }
    any
}

fn host_create(path: &str, schema_str: &str, opener: (u32, u64, u64)) -> Result<i64, HostOpenError> {
    let parsed_schema = ostream_schema(schema_str, "OStream open", path)?;
    let header_bytes = morloc_runtime_types::packet::make_stream_header_block(&parsed_schema);
    let fd = open_for_custody(path, "@open OStream")?;
    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    let fd = if unsafe { libc::fstat(fd, &mut st) } == 0 && st.st_size == 0 {
        fd
    } else {
        replace_with_fresh_file(fd, path)?
    };
    Ok(start_fresh(fd, path, schema_str, parsed_schema, header_bytes, opener)?)
}

fn start_fresh(
    fd: libc::c_int,
    path: &str,
    schema_str: &str,
    parsed_schema: Schema,
    header_bytes: Vec<u8>,
    opener: (u32, u64, u64),
) -> Result<i64, MorlocError> {
    if unsafe { libc::ftruncate(fd, 0) } != 0 {
        let e = std::io::Error::last_os_error();
        unlock_and_close(fd);
        return Err(MorlocError::Io(e));
    }
    if let Err(e) = write_all_fd(fd, &header_bytes) {
        unlock_and_close(fd);
        return Err(e);
    }
    let body_start = header_bytes.len() as u64;
    let resume = Resume { cursor: body_start, body_start, entries: Vec::new(), element_count: 0 };
    start_custody(fd, path, schema_str, parsed_schema, resume, opener)
}

/// Publish a slot for the locked file `fd` and start its custodian. `fd` is
/// closed on error.
fn start_custody(
    fd: libc::c_int,
    path: &str,
    schema_str: &str,
    parsed_schema: Schema,
    resume: Resume,
    opener: (u32, u64, u64),
) -> Result<i64, MorlocError> {
    use std::sync::atomic::Ordering;
    let (slot_idx, slot, guard) = match allocate_slot_cas_for(opener.0, opener.1, opener.2) {
        Ok(s) => s,
        Err(e) => {
            unlock_and_close(fd);
            return Err(e);
        }
    };
    let publish_result = (|| -> Result<u64, MorlocError> {
        let mut pending = Unpublished::default();
        let path_rel = pending.hold(shm_copy_bytes(path.as_bytes())?);
        let schema_rel = pending.hold(shm_copy_bytes(schema_str.as_bytes())?);
        let buf_bytes = read_write_buffer_bytes_env();
        let buf_abs = crate::shm::shcalloc(1, buf_bytes)?;
        let buf_rel = pending.hold(crate::shm::abs2rel(buf_abs)?);
        crate::custody::take_custody(ready_slot_queue(slot)?);
        unsafe {
            slot.kind.set(MLC_KIND_OSTREAM);
            let (dev, ino) = file_identity(fd);
            slot.file_dev.set(dev);
            slot.file_ino.set(ino);
            slot.file_path.set(pending.own(path_rel));
            slot.file_path_len.set(path.len() as u32);
            slot.schema_str.set(pending.own(schema_rel));
            slot.schema_str_len.set(schema_str.len() as u32);
            slot.subpacket_entries.set(shm_types_crate::RELNULL);
            slot.subpacket_entries_len.set(0);
            slot.subpacket_entries_cap.set(0);
            slot.body_start.set(resume.body_start);
            slot.final_footer.set(0);
            slot.cursor.set(resume.cursor);
            slot.element_count.set(resume.element_count);
            slot.compression_level.set(0);
            *slot.diag.get() = StreamDiag::new();
            slot.write_buffer.set(pending.own(buf_rel));
            slot.write_buffer_index_cap.set(0);
            slot.write_buffer_index_count.set(0);
            slot.write_buffer_data_used.set(0);
        }
        let bump = registry_gen_salt() | 1;
        // wrapping_add: generation is a wrapping counter masked to
        // GENERATION_MASK; a large random salt overflows u64 (debug panic).
        Ok(slot.generation.fetch_add(bump, Ordering::AcqRel).wrapping_add(bump) & GENERATION_MASK)
    })();
    let new_gen = match publish_result {
        Ok(g) => g,
        Err(e) => {
            release_slot_locked(slot);
            unlock_and_close(fd);
            return Err(e);
        }
    };
    let (value_schema, elem_schema) = derive_stream_schemas(&parsed_schema);
    let mut diag = StreamDiag::new();
    diag.subpacket_count = resume.entries.len() as u64;
    diag.element_count = resume.element_count;
    let writer = crate::custody::Writer::new(crate::custody::WriterInit {
        q: slot_queue(slot)?,
        slot,
        slot_idx,
        gen: new_gen,
        out: crate::custody::Out::File(fd),
        value_schema,
        elem_schema,
        cursor: resume.cursor,
        entries: resume.entries,
        diag,
    });
    if let Err(e) = crate::custody::host_spawn(writer) {
        release_slot_locked(slot);
        drop(guard);
        unlock_and_close(fd);
        return Err(e);
    }
    drop(guard);
    Ok(pack_handle(new_gen, slot_idx))
}

/// Start the custodian of a stdout or stderr stream this process or a pool
/// published.
// SLOT-16
pub(crate) fn host_adopt(handle: i64) -> Result<(), MorlocError> {
    use std::sync::atomic::Ordering;
    let (gen, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "stream handle {:#x}: slot index out of range", handle,
    )))?;
    let (schema, body_start) = versioned_read(slot, gen, |s| {
        if s.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED
            || s.kind.get() != MLC_KIND_OSTREAM
            || s.is_stdio.get() == 0
        {
            return Err(MorlocError::Other(format!("stream handle {:#x} is not an open stdout or stderr stream", handle)));
        }
        Ok((copy_slot_bytes(s.schema_str.get(), s.schema_str_len.get() as usize)?, s.body_start.get()))
    })?
    .ok_or_else(|| MorlocError::Other(format!("stream handle {:#x} was closed", handle)))?;
    let schema_str = String::from_utf8(schema).map_err(|_| MorlocError::Other("stream schema is not UTF-8".into()))?;
    let parsed = if schema_str.is_empty() {
        Schema::primitive(SerialType::Nil)
    } else {
        parse_schema(&schema_str).map_err(|e| MorlocError::Schema(e.to_string()))?
    };
    let (value_schema, elem_schema) = derive_stream_schemas(&parsed);
    let q = {
        let _guard = SlotGuard::lock(slot)?;
        let q = slot_queue(slot)?;
        if !slot_generation_is(slot, gen) || slot.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED {
            return Err(MorlocError::Other(format!("stream handle {:#x} was closed", handle)));
        }
        if q.hosted() {
            return Err(MorlocError::Other(format!("stream handle {:#x} already has a writer", handle)));
        }
        crate::custody::take_custody(q);
        q
    };
    let writer = crate::custody::Writer::new(crate::custody::WriterInit {
        q,
        slot,
        slot_idx,
        gen,
        out: crate::custody::Out::Stdio,
        value_schema,
        elem_schema,
        cursor: body_start,
        entries: Vec::new(),
        diag: StreamDiag::new(),
    });
    crate::custody::host_spawn(writer)
}


/// Explicit `@close`: writes final footer (status = CLOSED) + fdatasync
/// for OStream, then releases the slot. Caller-visible failures (pwrite,
/// fsync) propagate to the user; the slot stays OPEN on error so a retry
/// can succeed.
pub fn shared_close_handle(handle: i64) -> Result<(), MorlocError> {
    shared_close_handle_with_status(
        handle,
        morloc_runtime_types::packet::FOOTER_STATUS_CLOSED,
    )
}

/// Same as `shared_close_handle` but records `status` in the final
/// footer's `METADATA_TYPE_FOOTER_STATUS` block. Used by the per-call_id
/// sweeper to mark OStream slots auto-closed at remote-child dispatch
/// exit with `FOOTER_STATUS_PAUSED` so the parent can distinguish a
/// hand-off from a `@close` that ran to completion.
pub fn shared_close_handle_with_status(
    handle: i64,
    status: u8,
) -> Result<(), MorlocError> {
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
    match close_open_stream(handle, slot, gen_claim, status) {
        Ok(()) => Ok(()),
        Err(e) => Err(release_poisoned(handle, slot, gen_claim).unwrap_or(e)),
    }
}

/// End a stream a process died inside, recording the failure in its footer.
/// Returns the error to report, or `None` if the stream was not poisoned.
fn release_poisoned(handle: i64, slot: &RegistrySlot, gen_claim: u64) -> Option<MorlocError> {
    use std::sync::atomic::Ordering;
    if slot.poisoned.get() == 0
        || !slot_generation_is(slot, gen_claim)
        || slot.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED
    {
        return None;
    }
    let (_, idx) = unpack_handle(handle);
    let _ = finish_stream(idx, gen_claim, morloc_runtime_types::packet::FOOTER_STATUS_FAILED as u32);
    Some(died_inside())
}

fn close_open_stream(
    handle: i64,
    slot: &RegistrySlot,
    gen_claim: u64,
    status: u8,
) -> Result<(), MorlocError> {
    use std::sync::atomic::Ordering;
    let kind = slot.kind.get();

    // Closing a channel finishes it: what is buffered is queued and readers
    // see the end after it. The slot stays until the channel is settled.
    if kind == MLC_KIND_CHANNEL {
        with_process_local_slot(handle, |local, slot| {
            let _guard = SlotGuard::lock(slot)?;
            if !slot_generation_is(slot, gen_claim) {
                return Err(MorlocError::Other("the reader of this stream has stopped".into()));
            }
            flush_write_buffer(slot, local)?;
            channel_finish(slot)
        })?;
        invalidate_process_local_slot(handle);
        return Ok(());
    }

    if kind == MLC_KIND_OSTREAM {
        if slot.poisoned.get() != 0 {
            return Err(died_inside());
        }
        let (_, idx) = unpack_handle(handle);
        return finish_stream(idx, gen_claim, status as u32);
    }

    // IFile / IStream: no finalisation; just release.
    let _guard = SlotGuard::lock(slot)?;
    let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_now != gen_claim {
        return Err(MorlocError::Other(
            "shared_close_handle: slot generation changed under us".into(),
        ));
    }
    release_slot_locked(slot);
    drop(_guard);
    invalidate_process_local_slot(handle);
    Ok(())
}

/// End a stream without completing it: what was queued is written, the
/// unflushed buffer is dropped, and an `OStream`'s file keeps its temporary
/// footer (the "writer did not finish" signal). Used by the sweeps of
/// abandoned handles and by cleanup after errors.
pub fn shared_discard_handle(handle: i64) -> Result<(), MorlocError> {
    let (gen_claim, slot_idx) = unpack_handle(handle);
    finish_stream(slot_idx, gen_claim, crate::custody::STATUS_DISCARD)
}

// Shared op functions run against the SHM slot + process-local mmap cache.
// They coordinate cursor + sub-packet-index updates via the slot lock so
// concurrent pools writing to or reading from the same handle stay
// consistent.

/// A sub-packet ready to write: its header and metadata block, and its
/// payload kept where it already is.
///
/// The payload is the whole sub-packet but for a few hundred bytes, and at
/// level 0 it is the caller's buffer unchanged, so it is carried by
/// reference. Assembling one contiguous `Vec` instead would copy it -- and
/// the old shape copied it twice, once to own it and once to append it.
pub(crate) struct SubpacketBytes<'a> {
    pub(crate) head: Vec<u8>,
    pub(crate) payload: std::borrow::Cow<'a, [u8]>,
    /// Zero bytes written after an uncompressed payload, counted in the
    /// header's length, so the next sub-packet -- and so its payload,
    /// whose metadata block is padded too -- starts 8-byte aligned and
    /// a reader can use the mapped bytes in place.
    pub(crate) pad: usize,
    /// On-disk payload region size (post-compression if applicable).
    /// Tracked separately so diag counters see the true bytes written
    /// rather than the assembled packet length (which also carries
    /// the header + metadata block).
    pub(crate) compressed_payload_len: usize,
    /// The payload's size before compression.
    pub(crate) uncompressed_len: usize,
}

impl SubpacketBytes<'_> {
    pub(crate) fn len(&self) -> usize {
        self.head.len() + self.payload.len() + self.pad
    }
}

/// A sub-packet payload ready to frame: the bytes to write and, when they
/// are zstd frames, their index.
pub(crate) struct PreparedPayload<'a> {
    pub(crate) bytes: std::borrow::Cow<'a, [u8]>,
    pub(crate) frames: Option<Vec<morloc_runtime_types::packet::FrameEntry>>,
    pub(crate) uncompressed_len: usize,
}

/// Assemble a `MORLOC_DATA_PACKET` around a prepared payload: build the
/// SCHEMA_STRING (+ FRAME_INDEX when compressed) metadata block and
/// prepend the 32-byte header. Returns the wire bytes ready to write to
/// any transport (disk pwrite, RPC send-into-SHM). Format-only work -- no
/// cursor, no diag, no I/O.
pub(crate) fn build_subpacket_bytes<'a>(
    value_schema: &morloc_runtime_types::schema::Schema,
    prepared: PreparedPayload<'a>,
) -> Result<SubpacketBytes<'a>, MorlocError> {
    use morloc_runtime_types::packet::{
        PacketHeader, METADATA_TYPE_SCHEMA_STRING, METADATA_TYPE_FRAME_INDEX,
        METADATA_BLOCK_ALIGNMENT, PACKET_COMPRESSION_NONE, PACKET_COMPRESSION_ZSTD,
    };
    use morloc_runtime_types::schema::schema_to_string;

    let uncompressed_len = prepared.uncompressed_len;
    let final_payload = prepared.bytes;
    let (compression_byte, frame_index_body) = match &prepared.frames {
        None => (PACKET_COMPRESSION_NONE, None),
        Some(frames) => (
            PACKET_COMPRESSION_ZSTD,
            Some(crate::packet::encode_frame_index_entry(frames)),
        ),
    };
    let value_schema_str = schema_to_string(value_schema);
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
    // SOURCE_MESG: the packet body is the voidstar bytes themselves.
    // The file-OStream reader ignores the source byte and dispatches
    // on the payload directly; the stdio reader routes through the
    // generic `get_morloc_data_packet_value`, which reads a leading
    // relptr on SOURCE_RPTR -- catastrophic when the payload is raw
    // voidstar bytes rather than an SHM pointer.
    let pad = if compression_byte == PACKET_COMPRESSION_NONE {
        final_payload.len().next_multiple_of(8) - final_payload.len()
    } else {
        0
    };
    let mut hdr = PacketHeader::data_mesg(
        morloc_runtime_types::packet::PACKET_FORMAT_VOIDSTAR,
        (final_payload.len() + pad) as u64,
    );
    hdr.offset = padded_meta_len as u32;
    let mut hdr_bytes = hdr.to_bytes();
    hdr_bytes[15] = compression_byte;

    let compressed_payload_len = final_payload.len();
    let mut head = Vec::with_capacity(hdr_bytes.len() + meta.len());
    head.extend_from_slice(&hdr_bytes);
    head.extend_from_slice(&meta);
    Ok(SubpacketBytes { head, payload: final_payload, pad, compressed_payload_len, uncompressed_len })
}

/// Record a completed sub-packet flush into a slot's `StreamDiag`.
/// Reads the packed struct into an aligned stack copy, updates every
/// counter in one place, then writes it back -- one `read_unaligned`
/// plus one `write_unaligned` per flush regardless of how many fields
/// change. Pass `tail_offset` when the sub-packet has a stable on-disk
/// position to record in the tail window (disk writers); pass `None`
/// for transports without one (RPC).
pub(crate) unsafe fn record_subpacket_flush(
    diag_ptr: *mut StreamDiag,
    uncompressed: u64,
    compressed: u64,
    tail_offset: Option<u64>,
) {
    let mut d = std::ptr::read_unaligned(diag_ptr);
    d.subpacket_count += 1;
    d.bytes_compressed_total += compressed;
    d.bytes_uncompressed_total += uncompressed;
    if uncompressed > d.largest_packet_uncompressed {
        d.largest_packet_uncompressed = uncompressed;
        d.largest_packet_idx = d.subpacket_count - 1;
    }
    let now_us = unix_micros_now();
    if d.first_flush_time == 0 { d.first_flush_time = now_us; }
    d.last_flush_time = now_us;
    if let Some(off) = tail_offset {
        push_tail_window(&mut d, off);
    }
    std::ptr::write_unaligned(diag_ptr, d);
}

/// A sub-packet payload in portable form: every stream-handle field already
/// names its file by path (`TAG_PATH`), never by a slot of this nexus's
/// registry. Every payload the writer builds is portable -- elements are
/// flattened with [`crate::voidstar::flatten_into_portable`], and a
/// fixed-width run cannot hold a stream field -- so emitting one needs no
/// scan or rewrite. Constructed only where such a payload is built.
struct PortablePayload<'a>(&'a [u8]);

impl<'a> PortablePayload<'a> {
    fn new(bytes: &'a [u8], schema: &Schema) -> Result<Self, MorlocError> {
        if cfg!(debug_assertions) {
            let fields = crate::handle_scan::collect_stream_fields(bytes, schema)?;
            assert!(
                fields.iter().all(|f| unsafe {
                    morloc_runtime_types::stream_handle::read_tag(bytes.as_ptr().add(f.offset))
                        != morloc_runtime_types::stream_handle::TAG_HANDLE
                }),
                "a sub-packet payload holds a registry handle",
            );
        }
        Ok(PortablePayload(bytes))
    }
}

/// Read the current handle for a slot under its lock. Both fields are
/// derived from immutable-after-@open state, so this is a plain load.
fn slot_handle(slot: &RegistrySlot) -> i64 {
    use std::sync::atomic::Ordering;
    // Registry stores generation in the slot's atomic; the slot_idx is
    // implicit in the slot's SHM offset -- callers with only a slot ref
    // recover it via pointer arithmetic against slots_base.
    let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    let (slots_base, _) = registry_slot_array();
    let base = slots_base as usize;
    let this = slot as *const RegistrySlot as usize;
    let slot_idx = (this - base) / STREAM_ENTRY_SIZE;
    pack_handle(gen_now, slot_idx)
}

/// Debug-only guard: the writer's declared element count must equal
/// the count already stamped into the payload's Array header
/// (first 8 bytes = `Array.size`). Divergence would desync the
/// footer's per-sub-packet counts from the on-disk sub-packet payloads.
/// No-op in release builds.
#[inline]
pub(crate) fn debug_assert_payload_elem_count(elem_count: u64, payload: &[u8], site: &str) {
    if cfg!(debug_assertions) {
        let payload_count = if payload.len() < 8 {
            0
        } else {
            u64::from_le_bytes(payload[..8].try_into().unwrap())
        };
        assert_eq!(
            elem_count, payload_count,
            "{}: caller elem_count = {} but payload Array.size = {}",
            site, elem_count, payload_count,
        );
    }
}

/// Double the write buffer's index-section capacity. Caller MUST hold
/// the slot lock and have already verified that the new capacity
/// (plus current data_used) still fits in the buffer. Memmoves the
/// data region right by `(new_cap - old_cap) * elem_width` and
/// shifts every inline element's relptrs by the same amount.
fn grow_index_capacity(
    slot: &RegistrySlot,
    local: &ProcessLocalSlot,
    new_cap: u64,
) -> Result<(), MorlocError> {
    let w = local.elem_schema.width;
    let old_cap = slot.write_buffer_index_cap.get();
    let data_used = slot.write_buffer_data_used.get() as usize;
    let n = slot.write_buffer_index_count.get();

    let old_data_offset = 16 + (old_cap as usize) * w;
    let new_data_offset = 16 + (new_cap as usize) * w;
    let shift: isize = (new_data_offset - old_data_offset) as isize;

    let buf_abs = crate::shm::rel2abs(slot.write_buffer.get())?;
    if data_used > 0 {
        unsafe {
            std::ptr::copy(
                buf_abs.add(old_data_offset),
                buf_abs.add(new_data_offset),
                data_used,
            );
        }
        let res = crate::recur::Resolver::new(&local.elem_schema);
        for i in 0..n {
            let inline_off = 16 + (i as usize) * w;
            unsafe {
                crate::voidstar::shift_buffer_relptrs_with(
                    buf_abs, new_data_offset + data_used, inline_off,
                    &local.elem_schema, shift, &res,
                )?;
            }
        }
    }
    slot.write_buffer_index_cap.set(new_cap);

    Ok(())
}

/// Take the lock of a stream about to be written, flushed or closed.
fn lock_for_write<'a>(
    slot: &'a RegistrySlot,
    gen_claim: u64,
    stale: &str,
) -> Result<SlotGuard<'a>, MorlocError> {
    let guard = SlotGuard::lock(slot)?;
    if !slot_generation_is(slot, gen_claim) {
        return Err(MorlocError::Other(stale.into()));
    }
    if slot.kind.get() == MLC_KIND_OSTREAM {
        let q = slot_queue(slot)?;
        if let Some(e) = q.failure() {
            return Err(e);
        }
        if q.is_closed() {
            return Err(MorlocError::Other(stale.into()));
        }
    }
    Ok(guard)
}

fn slot_queue(slot: &RegistrySlot) -> Result<&'static crate::custody::CustodyQueue, MorlocError> {
    crate::custody::queue_at(slot.custody.get())
}

// SLOT-12: the full buffer moves to the queue and a spare takes its place.
fn queue_buffer(slot: &RegistrySlot) -> Result<(), MorlocError> {
    use crate::custody::{Item, ENTRY_BUFFER};
    let n = slot.write_buffer_index_count.get();
    if n == 0 {
        return Ok(());
    }
    let q = slot_queue(slot)?;
    let full = slot.write_buffer.get();
    let fresh = match q.take_spare() {
        Some(r) => r,
        None => {
            let size = unsafe { crate::shm::shm_block_size(crate::shm::rel2abs(full)?) }.ok_or_else(|| {
                MorlocError::Other("stream write buffer is not an SHM block".into())
            })?;
            let abs = crate::shm::shcalloc(1, size)?;
            slot_owns(crate::shm::abs2rel(abs)?)
        }
    };
    let item = Item {
        kind: ENTRY_BUFFER,
        status: 0,
        block: full,
        elems: n,
        index_cap: slot.write_buffer_index_cap.get(),
        data_used: slot.write_buffer_data_used.get(),
        len: 0,
        oversize: false,
        level: slot.compression_level.get(),
    };
    if let Err(e) = q.push(item) {
        if let Ok(abs) = crate::shm::rel2abs(fresh) {
            free_uncounted(abs);
        }
        return Err(e);
    }
    slot.write_buffer.set(fresh);
    slot.write_buffer_index_count.set(0);
    slot.write_buffer_data_used.set(0);
    Ok(())
}

// SLOT-12
fn queue_block(slot: &RegistrySlot, payload: &[u8], elems: u64, oversize: bool) -> Result<(), MorlocError> {
    use crate::custody::{Item, ENTRY_BLOCK};
    let q = slot_queue(slot)?;
    let abs = crate::shm::shmalloc(payload.len().max(1))?;
    // SAFETY: the block holds at least `payload.len()` bytes.
    unsafe { std::ptr::copy_nonoverlapping(payload.as_ptr(), abs as *mut u8, payload.len()) };
    let rel = slot_owns(crate::shm::abs2rel(abs)?);
    let item = Item {
        kind: ENTRY_BLOCK,
        status: 0,
        block: rel,
        elems,
        index_cap: 0,
        data_used: 0,
        len: payload.len() as u64,
        oversize,
        level: slot.compression_level.get(),
    };
    if let Err(e) = q.push(item) {
        free_uncounted(abs);
        return Err(e);
    }
    Ok(())
}

/// Hand the buffered elements on: an `OStream`'s to its custodian, a
/// channel's compacted onto its queue. Caller MUST hold the slot lock.
fn flush_write_buffer(
    slot: &RegistrySlot,
    local: &mut ProcessLocalSlot,
) -> Result<(), MorlocError> {
    if slot.kind.get() != MLC_KIND_CHANNEL {
        return queue_buffer(slot);
    }
    let n = slot.write_buffer_index_count.get();
    if n == 0 {
        return Ok(());
    }
    let buf_abs = crate::shm::rel2abs(slot.write_buffer.get())?;
    let payload_len = crate::write_behind::compact_sealed_buffer(
        buf_abs,
        n,
        slot.write_buffer_index_cap.get(),
        slot.write_buffer_data_used.get(),
        &local.elem_schema,
    )?;
    let payload_slice = unsafe { std::slice::from_raw_parts(buf_abs, payload_len) };
    let payload = PortablePayload::new(payload_slice, &local.value_schema)?;
    channel_enqueue(slot, local, payload)?;
    slot.write_buffer_index_count.set(0);
    slot.write_buffer_data_used.set(0);
    Ok(())
}

fn flush_full_buffer(
    slot: &RegistrySlot,
    local: &mut ProcessLocalSlot,
) -> Result<(), MorlocError> {
    flush_write_buffer(slot, local)
}

/// The index capacity a write buffer starts at: the default, clamped to what
/// a buffer of `buf_size` can hold at `w` bytes an element. Capacity then
/// doubles while the buffer has room.
fn initial_index_cap(buf_size: usize, w: usize) -> u64 {
    let max_index_cap = buf_size.saturating_sub(16) / w.max(1);
    (WRITE_BUFFER_INDEX_INITIAL_CAP as usize).min(max_index_cap).max(1) as u64
}

/// Append `n` elements of a fixed-width type, laid out contiguously at
/// `src`, to the write buffer: one memcpy per buffer-full, flushing a
/// sub-packet whenever the buffer's record region fills. A fixed-width
/// element holds no pointer and its padding is zero, so its bytes are
/// already its flattened form and no per-element walk is needed. The
/// record region grows to the capacity `append_one_element` reaches --
/// the initial capacity doubled while it fits -- so sub-packets are cut
/// where they always were. Caller MUST hold the slot lock.
fn append_flat_run(
    slot: &RegistrySlot,
    local: &mut ProcessLocalSlot,
    src: AbsPtr,
    n: usize,
    buf_size: usize,
) -> Result<(), MorlocError> {
    let w = local.elem_schema.width;
    let max_cap = ((buf_size - 16) / w) as u64;
    let mut cap = initial_index_cap(buf_size, w);
    while cap.saturating_mul(2) <= max_cap {
        cap *= 2;
    }
    let mut done = 0usize;
    while done < n {
        if slot.write_buffer_index_cap.get() < cap {
            grow_index_capacity(slot, local, cap)?;
        }
        let space = (slot.write_buffer_index_cap.get() - slot.write_buffer_index_count.get()) as usize;
        if space == 0 {
            flush_full_buffer(slot, local)?;
            continue;
        }
        let k = space.min(n - done);
        let buf_abs = crate::shm::rel2abs(slot.write_buffer.get())?;
        let at = 16 + (slot.write_buffer_index_count.get() as usize) * w;
        // SAFETY: the record region holds `write_buffer_index_cap` slots of
        // `w` bytes after the 16-byte header, all within the buffer, and
        // `src` holds `n` elements.
        unsafe {
            std::ptr::copy_nonoverlapping(src.add(done * w), buf_abs.add(at), k * w);
            slot.write_buffer_index_count.set(slot.write_buffer_index_count.get() + k as u64);
        }
        done += k;
    }
    Ok(())
}

/// Try to append one element from `elem_src` into the write buffer.
/// Returns Ok(()) on success (whether the element went into the
/// buffer or was emitted directly as an oversize sub-packet). Caller
/// MUST hold the slot lock.
/// `elem` is the element schema, with `res` its resolver: built once per
/// batch by the caller rather than once per element and walk.
fn append_one_element(
    slot: &RegistrySlot,
    local: &mut ProcessLocalSlot,
    elem_src: AbsPtr,
    buf_size: usize,
    scratch: &mut Vec<u8>,
    elem: &Schema,
    res: &crate::recur::Resolver<'_>,
) -> Result<(), MorlocError> {
    let w = elem.width;

    // Flatten the single element to a self-contained blob:
    //   blob[0..w]: inline (with buffer-relative relptrs into blob[w..])
    //   blob[w..]:  variable bytes (sub-allocations)
    // `scratch` reuses its allocation across the whole @write batch, so this
    // hot loop pays no per-element heap allocation. `buf_size` is read once by
    // the caller rather than re-querying the environment per element.
    crate::voidstar::flatten_into_portable_with(scratch, elem_src, elem, res)?;
    let elem_blob: &[u8] = scratch.as_slice();
    // Round the variable region up to 8-byte alignment. Successive
    // elements' variable regions concatenate in the write buffer at
    // `data_region_start + write_buffer_data_used`; if any element's
    // variable_size isn't a multiple of 8, the next element's
    // variable data lands at a misaligned offset and its Array
    // headers can't be dereferenced without violating alignment
    // requirements (fatal in a debug-checked runtime; silent UB in
    // release). The trailing pad bytes are zeroed where the element is
    // copied in; no relptr ever targets the padding.
    let raw_variable_size = elem_blob.len().saturating_sub(w);
    let variable_size = (raw_variable_size + 7) & !7;

    // Initialise the index cap on first write into this slot. Clamp
    // to what the buffer can actually hold: with a small env-overridden
    // buf_size (e.g. 4 KiB) and the default INITIAL_CAP (1024) at
    // 8-byte elements, a literal INITIAL_CAP-sized index section would
    // be 8 KiB on its own and the buffer would have no room left for
    // a single element.
    if slot.write_buffer_index_cap.get() == 0 {
        slot.write_buffer_index_cap.set(initial_index_cap(buf_size, w));
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
        oversize_payload.extend_from_slice(elem_blob);
        // Shift relptrs in the appended element by +16.
        let payload_base = oversize_payload.as_mut_ptr();
        unsafe {
            crate::voidstar::shift_buffer_relptrs(
                payload_base, oversize_len, 16, &local.elem_schema, 16isize,
            )?;
        }
        let payload = PortablePayload::new(&oversize_payload, &local.value_schema)?;
        if slot.kind.get() == MLC_KIND_CHANNEL {
            channel_enqueue(slot, local, payload)?;
        } else {
            queue_block(slot, payload.0, 1, true)?;
        }
        return Ok(());
    }

    // Try to fit one more element. If neither growing the index nor
    // the current data region's remaining space is enough, flush
    // first then retry (next iteration's empty buffer makes room).
    loop {
        let need_index_grow =
            slot.write_buffer_index_count.get() + 1 > slot.write_buffer_index_cap.get();
        let candidate_cap = if need_index_grow {
            slot.write_buffer_index_cap.get().saturating_mul(2)
        } else {
            slot.write_buffer_index_cap.get()
        };
        let candidate_index_bytes = (candidate_cap as usize) * w;
        let required = 16 + candidate_index_bytes
            + (slot.write_buffer_data_used.get() as usize)
            + variable_size;
        if required <= buf_size {
            if need_index_grow {
                grow_index_capacity(slot, local, candidate_cap)?;
            }
            break;
        }
        // Doesn't fit. On an empty buffer the element fits once the index
        // stops claiming the room its variable bytes need: a flush keeps the
        // capacity the previous sub-packet grew to, which a run of elements
        // with no variable bytes can grow to fill the whole buffer. With
        // nothing buffered, shrinking it moves nothing, and the oversize
        // check above guarantees one slot fits.
        if slot.write_buffer_index_count.get() == 0 {
            let fit = ((buf_size - 16 - variable_size) / w.max(1)).max(1) as u64;
            slot.write_buffer_index_cap.set(fit.min(slot.write_buffer_index_cap.get()));

            continue;
        }
        flush_full_buffer(slot, local)?;
        // Loop: buffer is empty now, try again.
    }

    // Copy inline + variable into the buffer and rebase relptrs.
    let buf_abs = crate::shm::rel2abs(slot.write_buffer.get())?;
    let index_offset = 16 + (slot.write_buffer_index_count.get() as usize) * w;
    let data_region_start = 16 + (slot.write_buffer_index_cap.get() as usize) * w;
    let data_offset = data_region_start + (slot.write_buffer_data_used.get() as usize);

    unsafe {
        std::ptr::copy_nonoverlapping(
            elem_blob.as_ptr(), buf_abs.add(index_offset), w,
        );
        // The buffer is reused across flushes, so the pad bytes up to
        // the 8-byte alignment are zeroed here; no relptr references them.
        if raw_variable_size > 0 {
            std::ptr::copy_nonoverlapping(
                elem_blob.as_ptr().add(w),
                buf_abs.add(data_offset),
                raw_variable_size,
            );
        }
        std::ptr::write_bytes(
            buf_abs.add(data_offset + raw_variable_size),
            0,
            variable_size - raw_variable_size,
        );
    }
    // Shift the copied element's relptrs from "blob-relative" (where
    // sub-allocations start at offset w) to "buffer-relative" (where
    // they now start at `data_offset`). We shift in place using pure
    // buffer arithmetic -- an SHM rebase would take a `rel2abs`
    // step on each Optional/Array descent that lands in the primary
    // SHM volume rather than in the write buffer (the relptrs are
    // pure offsets with `vol_idx == 0`), corrupting arbitrary bytes.
    let shift: isize = (data_offset as isize) - (w as isize);
    if shift != 0 {
        unsafe {
            crate::voidstar::shift_buffer_relptrs_with(
                buf_abs, data_offset + raw_variable_size, index_offset,
                elem, shift, res,
            )?;
        }
    }

    slot.write_buffer_index_count.set(slot.write_buffer_index_count.get() + 1);
    slot.write_buffer_data_used.set(slot.write_buffer_data_used.get() + variable_size as u64);

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
    level: crate::compression::CompressionLevel,
    payload_voidstar: AbsPtr,
) -> Result<(), MorlocError> {
    if payload_voidstar.is_null() {
        return Err(MorlocError::Other("@write: null payload".into()));
    }
    let level = level.raw();

    if channel_slot(handle)?.is_some() {
        channel_wait_room(handle)?;
    }
    let (gen_claim, _) = unpack_handle(handle);
    let queued = with_process_local_slot(handle, |local, slot| {
        if slot.kind.get() != MLC_KIND_OSTREAM && slot.kind.get() != MLC_KIND_CHANNEL {
            return Err(MorlocError::Other(format!(
                "@write on non-OStream handle (kind = {})",
                handle_kind_name(slot.kind.get()),
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

        let _guard = lock_for_write(slot, gen_claim, "the stream was closed, or its reader stopped")?;

        // The `@write` level is the default; on a stdio-bound stream the
        // nexus's explicit `-z` overrides it. The pool compresses the
        // sub-packet itself either way, so the footer's index describes the
        // bytes the nexus actually forwards and the redirected file is a
        // valid IFile.
        let level = if slot.is_stdio.get() != 0 {
            stdio_compression_override().unwrap_or(level)
        } else {
            level
        };

        // Pin compression level on first @write into this slot
        // (across all pools); subsequent writes must match.
        let pinned = if slot.kind.get() == MLC_KIND_OSTREAM {
            slot_queue(slot)?.pin_level(&slot.compression_level, level)
        } else if slot.element_count.get() == 0 && slot.write_buffer_index_count.get() == 0 {
            slot.compression_level.set(level);
            Ok(())
        } else if slot.compression_level.get() != level {
            Err(slot.compression_level.get())
        } else {
            Ok(())
        };
        if let Err(was) = pinned {
            return Err(MorlocError::Other(format!(
                "@write level mismatch: stream was opened/written at level {} \
                 but this call passed {}. All sub-packets must share a level.",
                was, level,
            )));
        }
        // SLOT-16: a stdio write returns once its batches are out, so a raw
        // print by this process never lands inside one.
        let stdio_queue = if slot.is_stdio.get() != 0 {
            let q = slot_queue(slot)?;
            Some((q, q.pushed()))
        } else {
            None
        };
        let queued = |stdio_queue: Option<(&'static crate::custody::CustodyQueue, u32)>| {
            stdio_queue.and_then(|(q, before)| {
                let after = q.pushed();
                (after != before).then_some((q, after, q.epoch()))
            })
        };

        // Walk the elements and append each. element_count updates
        // here (not at flush) so @flen reflects buffered elements too.
        // `buf_size` and `scratch` are hoisted out of the loop: the write-
        // buffer size is read from the environment once, and one scratch
        // buffer is reused for every element's flatten (no per-element
        // env lookup, no per-element heap allocation).
        let buf_size = read_write_buffer_bytes_env();
        let mut scratch: Vec<u8> = Vec::new();
        if slot.staged.get() != 0 {
            // One batch, one sub-packet: the whole `[a]` is flattened and
            // emitted as it is, so a batch is never split across frames or
            // merged with another, and an empty batch is an empty frame.
            if slot.write_buffer_index_count.get() > 0 {
                flush_write_buffer(slot, local)?;
            }
            crate::voidstar::flatten_into_portable(&mut scratch, payload_voidstar, &local.value_schema)?;
            let payload = PortablePayload::new(&scratch, &local.value_schema)?;
            queue_block(slot, payload.0, n_elements, false)?;
            slot.element_count.set(slot.element_count.get() + n_elements);
            return Ok(queued(stdio_queue));
        }
        if local.elem_schema.is_fixed_width() && w > 0 && 16 + w <= buf_size {
            append_flat_run(slot, local, elem_data_base, n_elements as usize, buf_size)?;
            slot.element_count.set(slot.element_count.get() + n_elements);
            return Ok(queued(stdio_queue));
        }
        // One copy of the element schema for the batch: its resolver
        // borrows it while the slot itself is updated per element.
        let elem = local.elem_schema.clone();
        let res = crate::recur::Resolver::new(&elem);
        for i in 0..n_elements {
            let elem_src = unsafe { elem_data_base.add((i as usize) * w) };
            append_one_element(slot, local, elem_src, buf_size, &mut scratch, &elem, &res)?;
            slot.element_count.set(slot.element_count.get() + 1);
        }
        Ok(queued(stdio_queue))
    })?;
    match queued {
        Some((q, seq, epoch)) => q.wait_done(seq, epoch),
        None => Ok(()),
    }
}

/// `@flush handle`: hand the buffered elements on now, without closing the
/// stream. On an `OStream` it returns once they are in the file (SLOT-15).
pub fn shared_flush_buffer(handle: i64) -> Result<(), MorlocError> {
    let (gen_claim, _) = unpack_handle(handle);
    let waiting = with_process_local_slot(handle, |local, slot| {
        if slot.kind.get() != MLC_KIND_OSTREAM && slot.kind.get() != MLC_KIND_CHANNEL {
            return Err(MorlocError::Other(format!(
                "@flush on non-OStream handle (kind = {})",
                handle_kind_name(slot.kind.get()),
            )));
        }
        let _guard = lock_for_write(slot, gen_claim, "the stream was closed, or its reader stopped")?;
        flush_write_buffer(slot, local)?;
        if slot.kind.get() != MLC_KIND_OSTREAM {
            return Ok(None);
        }
        let q = slot_queue(slot)?;
        let seq = q.push(crate::custody::Item::marker(crate::custody::ENTRY_FLUSH, 0))?;
        Ok(Some((q, seq, q.epoch())))
    })?;
    match waiting {
        Some((q, seq, epoch)) => q.wait_done(seq, epoch),
        None => Ok(()),
    }
}

/// `@next handle` on an IStream slot. Reads the next sub-packet's
/// header at the slot's current cursor (via this pool's mmap),
/// materialises it, advances the cursor, and returns an SHM
/// `Array<a>` AbsPtr.
///
/// The cursor advance is under the slot lock, so concurrent
/// readers from multiple pools each pull a distinct sub-packet
/// (the multi-reader IStream work-queue pattern). On EOF (cursor
/// at or past the file's footer / end-of-data) returns an empty
/// `Array<a>`.
pub fn shared_next_subpacket(handle: i64) -> Result<AbsPtr, MorlocError> {
    if channel_slot(handle)?.is_some() {
        return match channel_pop(handle)? {
            Some(p) => Ok(p),
            None => empty_shm_array(),
        };
    }
    if let Some(stdio_kind) = shared_handle_stdio_kind(handle)? {
        verify_stdio_opener_pid(handle)?;
        return stdio_next_via_rpc(handle, stdio_kind);
    }
    match next_file_subpacket(handle)? {
        Some(p) => Ok(p),
        None => empty_shm_array(),
    }
}

/// Read the next sub-packet of a file-backed IStream as one frame, telling
/// the end of the stream (`None`) apart from an empty frame. `@next`
/// cannot: it answers both with an empty list.
///
/// Standard input carries no footer to tell them apart by, so there an
/// empty sub-packet still ends the stream, as with `@next`.
pub fn shared_next_frame(handle: i64) -> Result<Option<AbsPtr>, MorlocError> {
    if channel_slot(handle)?.is_some() {
        return channel_pop(handle);
    }
    if let Some(stdio_kind) = shared_handle_stdio_kind(handle)? {
        verify_stdio_opener_pid(handle)?;
        let p = stdio_next_via_rpc(handle, stdio_kind)?;
        if unsafe { (*(p as *const shm_types_crate::Array)).size } == 0 {
            let _ = crate::shm::shfree(p);
            return Ok(None);
        }
        return Ok(Some(p));
    }
    next_file_subpacket(handle)
}

fn next_file_subpacket(handle: i64) -> Result<Option<AbsPtr>, MorlocError> {
    with_process_local_slot(handle, |local, slot| {
        if slot.kind.get() != MLC_KIND_ISTREAM {
            return Err(MorlocError::Other(format!(
                "@next on non-IStream handle (kind = {})",
                handle_kind_name(slot.kind.get()),
            )));
        }

        // Claim the next sub-packet under the lock. The lock window
        // is just header-read + cursor-advance; the actual
        // decompression / deep-copy happens with the lock dropped.
        let (claim_cursor, on_disk_size, header_is_data) = {
            let _guard = SlotGuard::lock(slot)?;
            if !slot_generation_is(slot, local.cached_generation) {
                return Err(MorlocError::Other(format!(
                    "stream handle {:#x}: the stream was closed", handle,
                )));
            }
            let cursor = slot.cursor.get();
            let end = if slot.data_end.get() != 0 { slot.data_end.get().min(local.mmap_size) } else { local.mmap_size };
            if cursor >= end || cursor + 32 > end {
                // EOF: leave cursor where it is.
                return Ok(None);
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
                // Footer encountered: end-of-stream.
                return Ok(None);
            }
            let size = 32 + header.offset as u64 + header.length;
            if cursor + size > end {
                return Err(MorlocError::Packet(format!(
                    "stream handle {:#x}: a sub-packet runs past the end of the stream", handle,
                )));
            }
            // Advance the cursor BEFORE we drop the lock so concurrent
            // @next on this slot from another pool reads from
            // cursor+size and claims a DIFFERENT sub-packet.
            slot.cursor.set(cursor + size);

            (cursor, size, true)
        };
        let _ = header_is_data;

        // Materialise the claimed sub-packet without holding the
        // lock. Other pools can advance through subsequent
        // sub-packets in parallel.
        let arr = materialize_and_finalise_subpacket(local, slot, claim_cursor)?;
        drop_read_pages(local, claim_cursor + on_disk_size);
        Ok(Some(arr))
    })
}

/// Hand back to the kernel the pages of an IStream's mapping that lie wholly
/// before `read_end`. A stream is read forward and each sub-packet is copied
/// out once, so nothing reads those bytes again; left mapped they are counted
/// against this process until the stream closes. The pages are replaced by a
/// fresh mapping of the same file range, so a page dropped here could only
/// fault back in from the file. Only this process's mapping is touched: the
/// page cache, which other pools reading the stream share, is left alone.
/// `madvise(MADV_DONTNEED)` is not used: on macOS it leaves the pages resident.
fn drop_read_pages(local: &mut ProcessLocalSlot, read_end: u64) {
    let page = crate::shm::page_size() as u64;
    let end = (read_end.min(local.mmap_size) / page) * page;
    if local.mmap_ptr.is_null() || end <= local.pages_dropped {
        return;
    }
    let Some(file) = local.map_file.as_ref() else { return };
    let start = local.pages_dropped;
    let ptr = unsafe {
        libc::mmap(
            (local.mmap_ptr as *mut u8).add(start as usize) as *mut libc::c_void,
            (end - start) as usize,
            libc::PROT_READ,
            libc::MAP_PRIVATE | libc::MAP_FIXED,
            file.as_raw_fd(),
            start as libc::off_t,
        )
    };
    if ptr != libc::MAP_FAILED {
        local.pages_dropped = end;
    }
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

    match src {
        SubpacketSrc::File { payload_base, payload_len, vol_idx_hint, .. } => {
            if arr_size == 0 {
                return empty_shm_array();
            }
            // SAFETY: the payload lies inside this pool's mmap of the file.
            let bytes = unsafe {
                std::slice::from_raw_parts(payload_base as *const u8, payload_len as usize)
            };
            payload_into_shm(bytes, &local.elem_schema, vol_idx_hint)
        }
        SubpacketSrc::Shm { arr_base } => Ok(arr_base),
    }
}

/// Copy a sub-packet payload -- its `Array` header, records and every
/// sub-allocation -- into one fresh SHM block and relocate it there. The
/// whole payload moves as one piece, so no relptr can be left pointing
/// into the source and no assumption about where sub-allocations sit is
/// needed; the rebase is bounded by the new block, so a relptr that
/// leaves the payload is an error. For a flat element type the rebase
/// touches only the header's data pointer.
fn payload_into_shm(
    bytes: &[u8],
    elem_schema: &Schema,
    vol_idx_hint: u16,
) -> Result<AbsPtr, MorlocError> {
    voidstar::read_binary_with_hint(bytes, &array_schema(elem_schema), vol_idx_hint)
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

/// Materialise an entire STREAM_PACKET file into a single self-contained
/// SHM `Array<a>` voidstar (the `[a]` list value).
///
/// `@load` on a stream file needs the whole list in one value, but a
/// constant-memory gather writes the list across MANY sub-packets. This
/// opens the file as an IStream, drains every sub-packet, and deep-copies
/// every element into one fresh element buffer. Because each element is
/// deep-copied (its variable-length sub-allocations are re-allocated in
/// fresh SHM blocks), the returned value is self-contained: the
/// per-sub-packet chunk buffers are freed before returning and nothing in
/// the result points back into them.
pub fn shared_load_stream_file_as_array(path: &str, requested: Option<&Schema>) -> Result<AbsPtr, MorlocError> {
    let handle = shared_open_istream(path)?;
    let result = check_stream_schema(handle, path, requested).and_then(|()| collect_istream_into_array(handle, path));
    // The stream is fully consumed here; release the slot and munmap the
    // backing file regardless of success so the handle never leaks.
    let _ = shared_discard_handle(handle);
    result
}

/// Copy one sub-packet's `sz` records and the `tail` bytes of
/// sub-allocations after them to `dst_rec` and `dst_tail`, then shift the
/// records' relptrs by however far the tail moved. Every relptr in the
/// records addresses that tail and nothing else, so one delta covers them.
///
/// # Safety
/// Both destinations must have room, and `records` must be followed by
/// its `tail` bytes within one block.
unsafe fn place_subpacket(
    records: AbsPtr,
    sz: usize,
    tail: usize,
    dst_rec: *mut u8,
    dst_tail: *mut u8,
    elem_schema: &Schema,
) -> Result<(), MorlocError> {
    let elem_width = elem_schema.width;
    let src_tail = (records as *const u8).add(sz * elem_width);
    std::ptr::copy_nonoverlapping(records as *const u8, dst_rec, sz * elem_width);
    // With no tail there is nothing to shift into, and the tail's address,
    // one past the block, may lie past the end of its volume.
    if tail == 0 {
        return Ok(());
    }
    std::ptr::copy_nonoverlapping(src_tail, dst_tail, tail);
    let delta = match (shm::abs2rel(src_tail as AbsPtr), shm::abs2rel(dst_tail as AbsPtr)) {
        (Ok(a), Ok(b)) => (b as i64).wrapping_sub(a as i64) as RelPtr,
        _ => return Err(MorlocError::Shm("@load: sub-packet is outside every volume".into())),
    };
    let window = voidstar::RelWindow::of_block(dst_tail, tail)?;
    voidstar::adjust_records_within(dst_rec as AbsPtr, sz, elem_schema, delta, window)
}

/// What went wrong with the sized route.
enum CollectSized {
    /// The index did not describe this file. The caller may read it the
    /// slow way instead; nothing has been allocated or consumed that it
    /// needs to know about beyond the cursor.
    Mismatch,
    /// A real failure, to be reported as is.
    Failed(MorlocError),
}

impl From<MorlocError> for CollectSized {
    fn from(e: MorlocError) -> Self {
        CollectSized::Failed(e)
    }
}

/// Put an IStream back at its first sub-packet.
fn reset_istream_cursor(handle: i64) -> Result<(), MorlocError> {
    with_process_local_slot(handle, |local, slot| {
        let _guard = SlotGuard::lock(slot)?;
        if !slot_generation_is(slot, local.cached_generation) {
            return Err(MorlocError::Other(format!(
                "stream handle {:#x}: the stream was closed", handle,
            )));
        }
        slot.cursor.set(slot.body_start.get());

        Ok(())
    })
}

/// Drain a stream into one block whose size the index already gave.
///
/// Each sub-packet is copied in and released before the next is read, so
/// the payload is resident once rather than twice. Every write is checked
/// against the regions the size bought: an index that describes more than
/// the file holds is answered with `Mismatch`, never with a write past the
/// block.
fn collect_sized(
    handle: i64,
    elem_schema: &Schema,
    elems: usize,
    payload: usize,
) -> Result<AbsPtr, CollectSized> {
    let hdr_size = std::mem::size_of::<shm_types_crate::Array>();
    let elem_width = elem_schema.width;
    let records_size = elems.checked_mul(elem_width)
        .ok_or(CollectSized::Mismatch)?;
    // A sub-packet's payload is its own header, its records and their
    // sub-allocations. The result drops the per-sub-packet headers, so the
    // sum is an upper bound on what it needs, never a short one.
    if payload < records_size {
        return Err(CollectSized::Mismatch);
    }
    let tail_cap = payload - records_size;
    if elems == 0 {
        let out = shm::shcalloc(1, hdr_size)?;
        let a = unsafe { &mut *(out as *mut shm_types_crate::Array) };
        a.size = 0;
        a.data = shm_types_crate::RELNULL;
        return Ok(out);
    }
    let out = shm::shmalloc(hdr_size + records_size + tail_cap)?;
    let out_records = unsafe { (out as *mut u8).add(hdr_size) };
    let out_tails = unsafe { out_records.add(records_size) };

    let mut rec_at = 0usize;
    let mut tail_at = 0usize;
    let finish = |out: AbsPtr, e: CollectSized| -> CollectSized {
        let _ = shm::shfree(out);
        e
    };
    loop {
        let block = match shared_next_frame(handle) {
            Ok(Some(p)) => p,
            Ok(None) => break,
            Err(e) => return Err(finish(out, e.into())),
        };
        let arr = unsafe { &*(block as *const shm_types_crate::Array) };
        let sz = arr.size;
        // An empty sub-packet is an empty batch, not the end of the stream.
        if sz == 0 {
            let _ = shm::shfree(block);
            continue;
        }
        let records = match shm::rel2abs(arr.data) {
            Ok(p) => p,
            Err(e) => {
                let _ = shm::shfree(block);
                return Err(finish(out, e.into()));
            }
        };
        let used = (records as usize) - (block as usize) + sz * elem_width;
        // The tail is whatever the block holds past its records. Reading
        // that size is not optional: the records' relptrs are shifted to
        // wherever the tail lands, so a tail that is not copied leaves them
        // addressing uninitialized bytes.
        let block_size = match unsafe { shm::shm_block_size(block) } {
            Some(n) if n >= used => n,
            _ => {
                let _ = shm::shfree(block);
                return Err(finish(out, CollectSized::Mismatch));
            }
        };
        let tail = block_size - used;
        // The index promised room for this; if it did not, stop before
        // writing rather than grow into whatever follows.
        if rec_at + sz > elems || tail_at + tail > tail_cap {
            let _ = shm::shfree(block);
            return Err(finish(out, CollectSized::Mismatch));
        }
        // SAFETY: the bound above proves both destinations have room, and
        // each source range lies inside the sub-packet's own block.
        let moved = unsafe {
            place_subpacket(
                records, sz, tail,
                out_records.add(rec_at * elem_width), out_tails.add(tail_at),
                elem_schema,
            )
        };
        if let Err(e) = moved {
            let _ = shm::shfree(block);
            return Err(finish(out, e.into()));
        }
        rec_at += sz;
        tail_at += tail;
        // The sub-packet's bytes are in the result now.
        let _ = shm::shfree(block);
    }
    if rec_at != elems {
        return Err(finish(out, CollectSized::Mismatch));
    }
    let records_rel = match shm::abs2rel(out_records as AbsPtr) {
        Ok(r) => r,
        Err(e) => return Err(finish(out, e.into())),
    };
    let a = unsafe { &mut *(out as *mut shm_types_crate::Array) };
    a.size = elems;
    a.data = records_rel;
    Ok(out)
}

/// How much a stream's sub-packets hold, read from the index alone.
///
/// Returns `(element count, payload bytes)`, where the payload of a
/// sub-packet is its `Array` header plus its records plus their
/// sub-allocations -- the number its own header carries, or, when it is
/// compressed, the sum its frame index carries. Neither touches a payload
/// byte, so this costs a header read per sub-packet.
///
/// This is a HINT. A footerless file's index was synthesized by a scan of a
/// writer that may have crashed, and a file can be appended to between this
/// and the read that follows, so a caller sizes from it and then bound-checks
/// what it actually writes.
fn stream_payload_hint(handle: i64) -> Option<(u64, u64)> {
    with_process_local_slot(handle, |local, _| {
        let mut elems = 0u64;
        let mut payload = 0u64;
        for i in 0..local.subpacket_entries_local.len() {
            let off = local.subpacket_entries_local[i].offset;
            elems += local.subpacket_entries_local[i].elem_count;
            let (header, _, payload_len) = read_subpacket_header(local, off)?;
            let data = unsafe { header.command.data };
            payload += if data.compression == PACKET_COMPRESSION_NONE {
                payload_len
            } else {
                let hdr_meta_len = 32 + header.offset as usize;
                // SAFETY: read_subpacket_header validated the header and its
                // metadata lie inside the mapping.
                let hdr_meta = unsafe {
                    std::slice::from_raw_parts(
                        (local.mmap_ptr as *const u8).add(off as usize),
                        hdr_meta_len,
                    )
                };
                match morloc_runtime_types::packet::read_frame_index_from_meta(hdr_meta)? {
                    Some(frames) => width::u64_from_usize(morloc_runtime_types::packet::frame_totals(&frames)?.0),
                    None => return Err(MorlocError::Packet(
                        "compressed sub-packet carries no frame index".into(),
                    )),
                }
            };
        }
        Ok((elems, payload))
    }).ok()
}

fn check_stream_schema(handle: i64, path: &str, requested: Option<&Schema>) -> Result<(), MorlocError> {
    let Some(requested) = requested else { return Ok(()) };
    let stored = shared_handle_schema_str(handle)?;
    let wanted = morloc_runtime_types::schema::schema_to_string(requested);
    if morloc_runtime_types::schema::schema_strings_compatible(&stored, &wanted) {
        Ok(())
    } else {
        Err(MorlocError::UserThrow(format!(
            "@load: schema mismatch reading '{path}': file has schema `{stored}`, requested `{wanted}`"
        )))
    }
}

/// Drain every sub-packet of an open IStream `handle` into one combined
/// SHM `Array<a>`. Split out from `shared_load_stream_file_as_array` so
/// the handle cleanup runs on every exit path.
fn collect_istream_into_array(handle: i64, path: &str) -> Result<AbsPtr, MorlocError> {
    // The slot caches the file's full list schema `[a]`; its element
    // schema drives the per-element deep copy of each sub-packet chunk.
    // Using the file's own schema (not the caller's) is what keeps the
    // element width and layout consistent with the bytes the sub-packet
    // materialiser wrote.
    let schema_str = shared_handle_schema_str(handle)?;
    let parsed = parse_schema(&schema_str).map_err(|e| {
        MorlocError::Schema(format!(
            "@load: stream file '{}' has unparseable schema '{}': {}",
            path, schema_str, e,
        ))
    })?;
    let (_value_schema, elem_schema) = derive_stream_schemas(&parsed);
    let elem_width = elem_schema.width;

    // Drain every sub-packet, holding each chunk alive until its bytes have
    // been copied into the combined value.
    //
    // A sub-packet is one self-contained block laid out `[Array][records]
    // [sub-allocations]`, with every relptr in the records pointing into its
    // own sub-allocation region. So combining K of them is two memcpys and
    // one bounded relptr shift per sub-packet, into a single destination
    // block laid out the same way. It replaces an element-by-element deep
    // copy that
    // allocated a fresh block for every variable-length field -- blocks the
    // caller's single `shfree` could never reach.
    let hdr_size = std::mem::size_of::<shm_types_crate::Array>();

    // Preferred route: the index says how many elements there are and how
    // many bytes their sub-packets hold, so the result can be sized before
    // anything is read. Each sub-packet is then copied in and released
    // immediately, and the stream's whole payload is never resident twice.
    if let Some((elems, payload)) = stream_payload_hint(handle) {
        match collect_sized(handle, &elem_schema, elems as usize, payload as usize) {
            Ok(p) => return Ok(p),
            // The index described a stream this file does not contain --
            // it was written by a process that did not finish, or the file
            // grew after it was read. Fall through and measure by reading.
            Err(CollectSized::Mismatch) => {
                reset_istream_cursor(handle)?;
            }
            Err(CollectSized::Failed(e)) => return Err(e),
        }
    }

    // (block, records base, element count, sub-allocation bytes)
    let mut chunks: Vec<(AbsPtr, AbsPtr, usize, usize)> = Vec::new();
    let mut total: usize = 0;
    let mut tail_total: usize = 0;

    let free_chunks = |chunks: &Vec<(AbsPtr, AbsPtr, usize, usize)>| {
        for &(block, _, _, _) in chunks {
            let _ = shm::shfree(block);
        }
    };

    loop {
        let block = match shared_next_frame(handle) {
            Ok(Some(p)) => p,
            Ok(None) => break,
            Err(e) => {
                free_chunks(&chunks);
                return Err(e);
            }
        };
        let arr = unsafe { &*(block as *const shm_types_crate::Array) };
        let sz = arr.size;
        // An empty sub-packet is an empty batch, not the end of the stream.
        if sz == 0 {
            let _ = shm::shfree(block);
            continue;
        }
        let records = match shm::rel2abs(arr.data) {
            Ok(p) => p,
            Err(e) => {
                let _ = shm::shfree(block);
                free_chunks(&chunks);
                return Err(e);
            }
        };
        // Everything in the block past the records is sub-allocation bytes,
        // plus whatever the allocator rounded the request up by. Carrying the
        // rounding costs a few bytes in the destination and nothing else: the
        // relptrs are shifted per sub-packet, so where its bytes land relative
        // to another sub-packet's does not matter.
        let used = (records as usize) - (block as usize) + sz * elem_width;
        let block_size = unsafe { shm::shm_block_size(block) }.unwrap_or(0);
        if block_size < used {
            free_chunks(&chunks);
            let _ = shm::shfree(block);
            return Err(MorlocError::Shm(format!(
                "@load: sub-packet block of {} bytes is smaller than the {} \
                 its own header describes", block_size, used,
            )));
        }
        let tail = block_size - used;
        chunks.push((block, records, sz, tail));
        total += sz;
        tail_total += tail;
    }

    if total == 0 {
        let out = match shm::shcalloc(1, hdr_size) {
            Ok(p) => p,
            Err(e) => {
                free_chunks(&chunks);
                return Err(e);
            }
        };
        let a = unsafe { &mut *(out as *mut shm_types_crate::Array) };
        a.size = 0;
        a.data = shm_types_crate::RELNULL;
        free_chunks(&chunks);
        return Ok(out);
    }

    let records_size = total * elem_width;
    let out = match shm::shmalloc(hdr_size + records_size + tail_total) {
        Ok(p) => p,
        Err(e) => {
            free_chunks(&chunks);
            return Err(e);
        }
    };
    let out_records = unsafe { (out as *mut u8).add(hdr_size) };
    let out_tails = unsafe { out_records.add(records_size) };

    let mut rec_at = 0usize;   // elements already placed
    let mut tail_at = 0usize;  // sub-allocation bytes already placed
    for &(_, records, sz, tail) in &chunks {
        // SAFETY: the destination was sized as the sum of these two regions
        // over every sub-packet, and each source range lies inside its own
        // block (checked above).
        let moved = unsafe {
            place_subpacket(
                records, sz, tail,
                out_records.add(rec_at * elem_width), out_tails.add(tail_at),
                &elem_schema,
            )
        };
        if let Err(e) = moved {
            let _ = shm::shfree(out);
            free_chunks(&chunks);
            return Err(e);
        }
        rec_at += sz;
        tail_at += tail;
    }

    free_chunks(&chunks);

    let records_rel = match shm::abs2rel(out_records as AbsPtr) {
        Ok(r) => r,
        Err(e) => {
            let _ = shm::shfree(out);
            return Err(e);
        }
    };
    let a = unsafe { &mut *(out as *mut shm_types_crate::Array) };
    a.size = total;
    a.data = records_rel;
    Ok(out)
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
    let (state, kind, count) = versioned_read(slot, gen_claim, |s| {
        Ok((s.state.load(Ordering::Acquire), s.kind.get(), s.element_count.get()))
    })?
    .ok_or_else(|| MorlocError::Other(format!(
        "shared_handle_length: handle {:#x} names a closed stream", handle,
    )))?;
    if state != SLOT_STATE_OPEN_SHARED {
        return Err(MorlocError::Other("shared_handle_length: slot is not OPEN".into()));
    }
    if kind != MLC_KIND_IFILE && kind != MLC_KIND_ISTREAM {
        return Err(MorlocError::Other(format!(
            "@flen is only defined on IFile / IStream handles (got kind = {})",
            handle_kind_name(kind),
        )));
    }
    Ok(count)
}

/// Per-sub-packet layout of an IFile, for parallel planning.
///
/// Returns one `(element_offset, element_count, uncompressed_size)` triple
/// per sub-packet, in file order:
///   - `element_offset`   -- cumulative element index at which the sub-packet
///                           starts (prefix-sum of the counts).
///   - `element_count`    -- number of elements in the sub-packet.
///   - `uncompressed_size`-- uncompressed payload byte count, recovered WITHOUT
///                           decompression: the header length for uncompressed
///                           sub-packets, the sum of the FRAME_INDEX frame sizes
///                           for compressed ones.
///
/// A DATA packet is the degenerate single-chunk case: exactly one triple
/// `(0, element_count, payload_length)` (IFile only accepts uncompressed DATA
/// packets, so the header length is the uncompressed size). An empty stream
/// returns an empty vec. The only failure is a malformed/corrupt packet.
pub fn shared_stream_layout(handle: i64) -> Result<Vec<(u64, u64, u64)>, MorlocError> {
    with_process_local_slot(handle, |local, slot| {
        if slot.kind.get() != MLC_KIND_IFILE {
            return Err(MorlocError::Other(format!(
                "@streamLayout is only defined on IFile handles (got kind = {})",
                handle_kind_name(slot.kind.get()),
            )));
        }

        // DATA packet: one chunk spanning the whole file. Its single
        // synthesized index entry carries the element count; the uncompressed
        // size is the outer packet's payload length (uncompressed by the IFile
        // open guarantee). Read the outer header directly rather than via
        // read_subpacket_header, which enforces the MESG source that stream
        // sub-packets -- but not necessarily a monolithic DATA packet -- carry.
        if local.is_data_packet {
            // is_data_packet is defined as exactly one synthesized entry;
            // guard rather than index blindly so a broken invariant surfaces
            // as an error instead of a slice-index panic.
            let elem_count = local
                .subpacket_entries_local
                .first()
                .ok_or_else(|| MorlocError::Packet(
                    "@streamLayout: DATA packet has no synthesized index entry".into(),
                ))?
                .elem_count;
            if local.mmap_size < 32 {
                return Err(MorlocError::Packet(
                    "@streamLayout: DATA packet is shorter than a packet header".into(),
                ));
            }
            let hdr_bytes = unsafe {
                std::slice::from_raw_parts(local.mmap_ptr as *const u8, 32)
            };
            let header = PacketHeader::from_bytes(hdr_bytes.try_into().unwrap())?;
            // IFile only opens uncompressed DATA packets (open_data_packet
            // rejects compressed monoliths), so header.length is the
            // uncompressed payload size. Assert it rather than trust the
            // distant open-time guarantee silently.
            let data = unsafe { header.command.data };
            if data.compression != PACKET_COMPRESSION_NONE {
                return Err(MorlocError::Packet(
                    "@streamLayout: DATA packet is compressed; cannot report an \
                     uncompressed size (IFile should have refused it at open)".into(),
                ));
            }
            return Ok(vec![(0u64, elem_count, header.length)]);
        }

        // Stream packet: walk the sub-packet index. element_offset is the
        // cached cumulative-count prefix sum (cum[i] is sub-packet i's start
        // index); uncompressed_size is derived per sub-packet without decompression.
        ensure_elem_cum(local)?;
        let n = local.subpacket_entries_local.len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let entry_off = local.subpacket_entries_local[i].offset;
            let entry_cnt = local.subpacket_entries_local[i].elem_count;
            let element_offset = local.subpacket_elem_cum.as_ref().unwrap()[i];
            let (header, _, payload_len) =
                read_subpacket_header(local, entry_off)?;
            let data = unsafe { header.command.data };
            let uncompressed_size = if data.compression == PACKET_COMPRESSION_NONE {
                payload_len
            } else {
                // Header + metadata are contiguous with the header; sum the
                // FRAME_INDEX frame uncompressed sizes (no payload touched).
                let hdr_meta_len = 32 + header.offset as usize;
                let hdr_meta = unsafe {
                    std::slice::from_raw_parts(
                        (local.mmap_ptr as *const u8).add(entry_off as usize),
                        hdr_meta_len,
                    )
                };
                match morloc_runtime_types::packet::read_frame_index_from_meta(hdr_meta)? {
                    Some(frames) => width::u64_from_usize(morloc_runtime_types::packet::frame_totals(&frames)?.0),
                    // Every compressed sub-packet carries a FRAME_INDEX by
                    // construction; its absence is a corrupt/foreign packet.
                    None => return Err(MorlocError::Packet(format!(
                        "@streamLayout: compressed sub-packet at offset {} \
                         is missing its FRAME_INDEX", entry_off,
                    ))),
                }
            };
            out.push((element_offset, entry_cnt, uncompressed_size));
        }
        Ok(out)
    })
}

/// Versioned-pointer read of `kind` from a shared slot. Used by the
/// cross-pool wire codec to know which `open_dispatch` arm to call on
/// the receiving side.
pub fn shared_handle_kind(handle: i64) -> Result<u8, MorlocError> {
    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "shared_handle_kind: slot index {} out of range", slot_idx,
    )))?;
    versioned_read(slot, gen_claim, |s| Ok(s.kind.get()))?.ok_or_else(|| MorlocError::Other(format!(
        "shared_handle_kind: handle {:#x} names a closed stream", handle,
    )))
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
    let (state, kind, path) = versioned_read(slot, gen_claim, |s| {
        Ok((s.state.load(Ordering::Acquire), s.kind.get(), copy_slot_bytes(s.file_path.get(), s.file_path_len.get() as usize)?))
    })?
    .ok_or_else(|| MorlocError::Other(format!(
        "shared_handle_path: handle {:#x} names a closed stream", handle,
    )))?;
    if state != SLOT_STATE_OPEN_SHARED {
        return Err(MorlocError::Other("shared_handle_path: slot is not OPEN".into()));
    }
    if kind == MLC_KIND_CHANNEL {
        return Err(MorlocError::Other(CHANNEL_HAS_NO_PATH.into()));
    }
    if path.is_empty() {
        return Err(MorlocError::Other("shared_handle_path: slot has empty file_path".into()));
    }
    String::from_utf8(path)
        .map_err(|_| MorlocError::Other("shared_handle_path: file_path is not valid UTF-8".into()))
}

/// Snapshot the slot's `schema_str` UTF-8 string. Same versioned-
/// pointer discipline as `shared_handle_path`. Used by the stdio
/// server bridge to build the STREAM_PACKET header on first write.
pub fn shared_handle_schema_str(handle: i64) -> Result<String, MorlocError> {
    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "shared_handle_schema_str: slot index {} out of range", slot_idx,
    )))?;
    let bytes = versioned_read(slot, gen_claim, |s| copy_slot_bytes(s.schema_str.get(), s.schema_str_len.get() as usize))?
        .ok_or_else(|| MorlocError::Other(format!(
            "shared_handle_schema_str: handle {:#x} names a closed stream", handle,
        )))?;
    if bytes.is_empty() {
        return Err(MorlocError::Other("shared_handle_schema_str: slot has empty schema_str".into()));
    }
    String::from_utf8(bytes)
        .map_err(|_| MorlocError::Other("shared_handle_schema_str: schema_str is not valid UTF-8".into()))
}

/// Sentinel path for STDIN (IStream) / STDOUT (OStream). Matches the
/// `-` convention shared by cat/sort/etc.
pub const STDIO_SENTINEL_STD: &str = "-";
/// Sentinel path for STDERR (OStream).
pub const STDIO_SENTINEL_ERR: &str = "-2";

/// `@stream :: IFile [a] -> <IO> IStream a`: open a fresh IStream slot
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
    let opened_on = handle_file_identity(ifile_handle)?;
    let h = shared_open_istream(&path)?;
    let found = handle_file_identity(h)?;
    if opened_on.1 != 0 && found != opened_on {
        let _ = shared_close_handle(h);
        return Err(MorlocError::Other(format!(
            "@stream: the file '{}' was replaced after it was opened", path,
        )));
    }
    Ok(h)
}

/// The device and inode a handle's file had when it was opened.
fn handle_file_identity(handle: i64) -> Result<(u64, u64), MorlocError> {
    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "stream handle {:#x}: slot index {} out of range", handle, slot_idx,
    )))?;
    versioned_read(slot, gen_claim, |s| Ok((s.file_dev.get(), s.file_ino.get())))?.ok_or_else(|| {
        MorlocError::Other(format!("stream handle {:#x}: the stream was closed", handle))
    })
}

/// Batched suballoc-size lookup over a slice of shared-registry handles.
/// Since bridges now uniformly emit TAG_HANDLE for stream-handle
/// fields (see `mlc_write_handle_voidstar`), no suballoc bytes are
/// needed and the sum is always 0. Kept for ABI parity: callers that
/// pre-size their output buffers from this result get a clean 0 and
/// the bridge writes only the 16-byte inline field per handle.
pub fn shared_handles_path_lens(
    handles: &[i64],
    mut out_lens: Option<&mut [i64]>,
) -> Result<u64, MorlocError> {
    if let Some(ref outs) = out_lens {
        debug_assert_eq!(handles.len(), outs.len());
    }
    if let Some(ref mut outs) = out_lens {
        for slot in outs.iter_mut() { *slot = 0; }
    }
    let _ = handles; // no-op: TAG_HANDLE has no per-handle suballoc.
    Ok(0)
}

/// Batched voidstar write for a `[stream-handle a]` pack pass. Every
/// handle is written in TAG_HANDLE form (bare slot id in the inline
/// 16-byte field). Cursor is not advanced -- there are no suballocs.
///
/// # Safety
///
/// `dest_base` must be writable for `handles.len()` slots of `elem_stride` bytes.
pub unsafe fn shared_write_handles_voidstar(
    handles: &[i64],
    dest_base: *mut u8,
    elem_stride: usize,
    _cursor: &mut *mut u8,
) -> Result<(), MorlocError> {
    use morloc_runtime_types::stream_handle as sh;
    for (i, &h) in handles.iter().enumerate() {
        let slot = unsafe { dest_base.add(i * elem_stride) };
        unsafe { sh::write_field(slot, sh::TAG_HANDLE, sh::handle_payload(h)); }
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
    // Root-only `.[:]` (slice) with optional field/key tail. The
    // fast path only fires when the tail is pure Field/Key steps
    // (parse_field_only_tail) AND the total arg count matches
    // exactly what the slice-with-field-tail path expects (3 for the
    // slice, none for the field tail). Any other shape (tail with
    // brackets, groups, or extra runtime args) falls through to the
    // general walker.
    if let Some(rest) = path.strip_prefix(".[:]") {
        if args.len() != 3 {
            return shared_ifile_general(handle, path, args);
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
        if slot.kind.get() != MLC_KIND_IFILE {
            return Err(MorlocError::Other(format!(
                "field access on non-IFile handle (kind = {})",
                handle_kind_name(slot.kind.get()),
            )));
        }
        if local.subpacket_entries_local.is_empty() {
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

/// `@append` to a stream file: resume it after its last complete
/// sub-packet, or start it if empty or absent.
pub fn shared_append_to_path(
    path: &str,
    expected_schema_str: &str,
) -> Result<i64, MorlocError> {
    reject_dev_stdio_path(path)?;
    crate::custody::open(
        morloc_runtime_types::stdio_proto::OPEN_APPEND,
        &absolute_path(path)?,
        expected_schema_str,
    )
}

fn host_append(path: &str, expected_schema_str: &str, opener: (u32, u64, u64)) -> Result<i64, HostOpenError> {
    // The caller's string may carry `<hint>` prefixes (a pool passes the
    // schema the compiler baked into its dispatch table); the header
    // never does. Canonicalize before comparing, and compare
    // structurally so a caller that skips normalization is still served.
    let requested_schema_str =
        morloc_runtime_types::schema::canonicalize_schema_str(expected_schema_str);

    // Lock before reading anything. The size, the choice between starting
    // the file and resuming it, and the truncation that acts on that choice
    // all happen under one lock, so a writer that finishes in between
    // cannot have its records cut away by an offset computed before they
    // existed.
    let fd = open_for_custody(path, "@append")?;

    let mut st: libc::stat = unsafe { std::mem::zeroed() };
    if unsafe { libc::fstat(fd, &mut st) } != 0 {
        let e = std::io::Error::last_os_error();
        unlock_and_close(fd);
        return Err(MorlocError::Io(e).into());
    }
    let file_size = st.st_size as u64;

    // An empty file holds nothing to protect: either it was absent until
    // the open above, or a previous writer died before writing a header.
    // Either way this is where the log starts.
    if file_size == 0 {
        let parsed_schema = match ostream_schema(&requested_schema_str, "@append", path) {
            Ok(s) => s,
            Err(e) => {
                unlock_and_close(fd);
                return Err(e.into());
            }
        };
        let header_bytes =
            morloc_runtime_types::packet::make_stream_header_block(&parsed_schema);
        return Ok(start_fresh(fd, path, &requested_schema_str, parsed_schema, header_bytes, opener)?);
    }

    // Resume. The mapping is built on the locked descriptor rather than a
    // fresh open, so nothing can change the file between the parse and the
    // truncation below.
    let (mmap_ptr, mmap_size) = match mmap_fd_readonly(fd, file_size, path) {
        Ok(t) => t,
        Err(e) => {
            unlock_and_close(fd);
            return Err(e.into());
        }
    };
    let unmap_and = |e: MorlocError| -> HostOpenError {
        unsafe {
            libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize);
            unlock_and_close(fd);
        }
        e.into()
    };
    let parsed = match parse_stream_file(path, mmap_ptr, mmap_size) {
        Ok(p) => p,
        Err(e) => return Err(unmap_and(e)),
    };
    if !morloc_runtime_types::schema::schema_strings_compatible(
        &parsed.schema_str, &requested_schema_str,
    ) {
        return Err(unmap_and(MorlocError::Other(format!(
            "@append: schema mismatch on '{}': file has '{}', open requested '{}'",
            path, parsed.schema_str, requested_schema_str
        ))));
    }
    if parsed.is_data_packet {
        return Err(unmap_and(MorlocError::Other(format!(
            "@append: '{}' holds a single value (written by @save), not a stream", path,
        ))));
    }
    // Resume where every reader stops, so none of them is cut short. Only a
    // final footer indexes the sub-packets; otherwise the index is rebuilt
    // from the file, or the resumed stream's index would lose them.
    let resume_off = parsed.data_end;
    let (entries, element_count) = if parsed.final_footer {
        (parsed.subpacket_entries.clone(), parsed.element_count)
    } else {
        match index_unclosed_stream(mmap_ptr, mmap_size, parsed.body_start, resume_off) {
            Ok(rebuilt) => rebuilt,
            Err(e) => return Err(unmap_and(e)),
        }
    };
    let body_start = parsed.body_start;
    let schema_str = parsed.schema_str.clone();
    let value_schema = parsed.value_schema.clone();
    unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
    if unsafe { libc::ftruncate(fd, resume_off as libc::off_t) } != 0 {
        let e = std::io::Error::last_os_error();
        unlock_and_close(fd);
        return Err(MorlocError::Io(e).into());
    }
    let resume = Resume { cursor: resume_off, body_start, entries, element_count };
    Ok(start_custody(fd, path, &schema_str, value_schema, resume, opener)?)
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
// UNDER the slot lock before discarding (so a fresh allocation that
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

struct Sweeper {
    tx: std::sync::mpsc::Sender<SweepRequest>,
    thread: std::thread::JoinHandle<()>,
}

static SWEEPER: crate::fork_policy::Reset<Option<Sweeper>> = crate::fork_policy::Reset::new(|| None);

static SWEEPER_WANTED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

// FORK-6: the thread starts on the first request, so attaching starts none.
pub fn sweeper_want() {
    let _guard = SWEEPER.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
    SWEEPER_WANTED.store(true, std::sync::atomic::Ordering::Relaxed);
}

#[cfg(test)]
pub fn sweeper_init() {
    let mut guard = SWEEPER.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
    SWEEPER_WANTED.store(true, std::sync::atomic::Ordering::Relaxed);
    if guard.is_none() {
        *guard = start_sweeper();
    }
}

fn start_sweeper() -> Option<Sweeper> {
    let (tx, rx) = std::sync::mpsc::channel::<SweepRequest>();
    let thread = std::thread::Builder::new()
        .name("morloc-stream-sweeper".into())
        .spawn(move || sweeper_main(rx))
        .ok()?;
    Some(Sweeper { tx, thread })
}

fn sweep_now(req: SweepRequest) {
    match req {
        SweepRequest::PerCall(call_id) => sweep_per_call(call_id),
        SweepRequest::PerPid(pid, start_time) => {
            sweep_per_pid(pid, start_time);
        }
    }
}

// FORK-6: a process that wants a sweeper starts its own, a forked child included.
fn sweeper_send(req: SweepRequest) {
    let mut guard = SWEEPER.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
    if !SWEEPER_WANTED.load(std::sync::atomic::Ordering::Relaxed) {
        return;
    }
    if guard.is_none() {
        *guard = start_sweeper();
    }
    if let Some(sweeper) = guard.as_ref() {
        let _ = sweeper.tx.send(req);
        return;
    }
    drop(guard);
    // FORK-6: no thread could be started; the request is served here.
    sweep_now(req);
}

#[cfg(test)]
fn sweeper_running() -> bool {
    SWEEPER.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock()).as_ref().is_some_and(|s| !s.thread.is_finished())
}

/// Stop the sweeper thread. Called before `registry_teardown` unmaps
/// the registry SHM so the sweeper can't race a null / freed base.
///
/// Drops the sender first: the sweeper's `rx.recv()` then returns
/// `Err` at the NEXT call (after any in-flight work item finishes), so
/// the loop exits cleanly. Then join. Idempotent: no-op if the sweeper
/// was never started or was already shut down.
pub fn sweeper_shutdown() {
    let taken = {
        let mut guard = SWEEPER.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
        SWEEPER_WANTED.store(false, std::sync::atomic::Ordering::Relaxed);
        guard.take()
    };
    if let Some(Sweeper { tx, thread }) = taken {
        drop(tx);
        let _ = thread.join();
    }
}

/// Enqueue a per-call sweep request. Non-blocking. Dropped in a process
/// that never attached the registry.
///
/// `CALL_ID_NO_SWEEP` (0) is filtered here: enqueueing a sweep for
/// the sentinel value would needlessly walk the registry without
/// matching anything.
pub fn sweeper_enqueue_call(call_id: u64) {
    if call_id == CALL_ID_NO_SWEEP {
        return;
    }
    sweeper_send(SweepRequest::PerCall(call_id));
}

/// Enqueue a per-PID sweep request. Called when a pool crash is detected
/// (the pool's PID + start_time uniquely identify the dead pool's slots).
pub fn sweeper_enqueue_pid(pid: u32, start_time: u64) {
    sweeper_send(SweepRequest::PerPid(pid, start_time));
}

/// Sweeper thread main loop. Drains the queue forever; exits when
/// the `Sender` half is dropped (only happens at clean shutdown if
/// somebody calls `sweeper_shutdown`).
fn sweeper_main(rx: std::sync::mpsc::Receiver<SweepRequest>) {
    while let Ok(req) = rx.recv() {
        sweep_now(req);
    }
}

/// End every open stream opened in call `call_id`, an `OStream` with a
/// PAUSED footer. Each slot is matched under a versioned read and ended by
/// `finish_stream`, which confirms its generation under the lock.
fn sweep_per_call(call_id: u64) {
    sweep_matching(|slot| slot.call_id.load(std::sync::atomic::Ordering::Acquire) == call_id);
}

fn sweep_matching(matches: impl Fn(&RegistrySlot) -> bool) {
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
        let gen = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
        let hit = matches(slot);
        if generation_after_read(slot) & GENERATION_MASK != gen || !hit {
            continue;
        }
        let _ = finish_stream(idx, gen, morloc_runtime_types::packet::FOOTER_STATUS_PAUSED as u32);
    }
}

/// End every open stream the process `(pid, start_time)` opened; the start
/// time tells a reused pid apart.
fn sweep_per_pid(pid: u32, start_time: u64) {
    sweep_matching(|slot| slot.opener_pid.get() == pid && slot.opener_pid_start_time.get() == start_time);
}

/// Reclaim a stdio singleton claim (@stdout/@stderr/@stdin) left open by
/// the pool dispatch that just returned. Only the three stdio singletons:
/// a leaked stdio claim is never legitimate, whereas a file-backed OStream
/// handle may outlive its opening dispatch. Its buffered output reaches the
/// nexus before this returns, so a caller that replies after it is not
/// overtaken by its own output.
///
/// Gate: the per-thread `call_id` is `CALL_ID_NO_SWEEP` unless THIS
/// dispatch opened a stdio handle (`open_stdio` lazily mints one), so the
/// common no-stdio dispatch pays only a thread-local read and returns.
/// Matching on the dispatch's own `call_id` keeps a concurrent same-pid
/// worker's live claim (Threads mode) safe. Resets the per-thread `call_id`
/// before returning.
pub(crate) fn pool_reclaim_stdio_after_dispatch() {
    use std::sync::atomic::Ordering;
    let call_id = current_call_id();
    if call_id == CALL_ID_NO_SWEEP {
        return;
    }
    for stdio_kind in [STDIO_KIND_STDIN, STDIO_KIND_STDOUT, STDIO_KIND_STDERR] {
        let Some(claim) = stdio_claim_slot(stdio_kind) else { continue };
        let existing = claim.load(Ordering::Acquire);
        if existing == STDIO_UNCLAIMED {
            continue;
        }
        let (gen, slot_idx) = unpack_handle(existing);
        let Some(slot) = slot_ref(slot_idx) else { continue };
        let owned = slot.is_stdio.get() != 0 && slot.call_id.load(Ordering::Acquire) == call_id;
        if !owned || generation_after_read(slot) & GENERATION_MASK != gen {
            continue;
        }
        if let Err(e) = finish_stream(slot_idx, gen, morloc_runtime_types::packet::FOOTER_STATUS_PAUSED as u32) {
            if !matches!(e, MorlocError::PipeClosed) {
                eprintln!("morloc: a stream write failed after its call returned: {e}");
            }
        }
    }
    set_current_call_id(CALL_ID_NO_SWEEP);
}

/// The nexus's explicit `-z N`, published to every pool as
/// `MORLOC_STDOUT_COMPRESSION_LEVEL`. Unset means the `@write` level
/// stands; an unparsable value is treated the same way rather than
/// silently selecting some other level.
pub fn stdio_compression_override() -> Option<u8> {
    std::env::var("MORLOC_STDOUT_COMPRESSION_LEVEL")
        .ok()
        .and_then(|s| s.parse::<u8>().ok())
}

pub fn read_write_buffer_bytes_env() -> usize {
    const MIN: usize = 4096;
    #[cfg(test)]
    {
        let n = TEST_WRITE_BUFFER_BYTES.load(std::sync::atomic::Ordering::SeqCst);
        if n != usize::MAX {
            return n.max(MIN);
        }
    }
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
/// and would only matter for a stale sweep entry; the under-lock
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
        /// emitted relptr). The file resolver ignores it for live
        /// dereferences; a copy out of the file (`copy_runs`) relocates
        /// by it.
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

    /// The space the sub-packet's relptrs resolve in.
    fn space(&self) -> SrcSpace {
        match *self {
            // SAFETY: a File source's payload stays mapped as long as its
            // slot, which outlives every use of the source.
            SubpacketSrc::File { payload_base, payload_len, .. } => SrcSpace::File(unsafe {
                voidstar::Local::new(payload_base, width::usize_from_u64(payload_len))
            }),
            SubpacketSrc::Shm { .. } => SrcSpace::Shm(voidstar::Arena),
        }
    }

    /// The sub-packet's root array: its length and the address of its
    /// records of `width` bytes each, the whole region checked to lie in
    /// the source. Null for an empty array.
    fn records(&self, width: usize) -> Result<(usize, AbsPtr), MorlocError> {
        // SAFETY: a source's root is an Array header, checked to fit its
        // payload when the source was made.
        let arr = unsafe { &*(self.arr_base() as *const shm_types_crate::Array) };
        if arr.size == 0 {
            return Ok((0, std::ptr::null_mut()));
        }
        let bytes = voidstar::region_len(arr.size, width)?;
        Ok((arr.size, self.space().resolve(arr.data, bytes)?))
    }

    /// The records of the array `arr` inside this sub-packet, `width` bytes
    /// each, the whole region checked to lie in the source. Null when empty.
    fn array_data(&self, arr: &shm_types_crate::Array, width: usize) -> Result<AbsPtr, MorlocError> {
        if arr.size == 0 {
            return Ok(std::ptr::null_mut());
        }
        self.space().resolve(arr.data, voidstar::region_len(arr.size, width)?)
    }
}

/// Where a sub-packet's relptrs point: its mapped file payload, or SHM.
enum SrcSpace {
    File(voidstar::Local),
    Shm(voidstar::Arena),
}

impl voidstar::Space for SrcSpace {
    #[inline]
    fn resolve(&self, rel: RelPtr, extent: usize) -> Result<AbsPtr, MorlocError> {
        match self {
            SrcSpace::File(l) => l.resolve(rel, extent),
            SrcSpace::Shm(a) => a.resolve(rel, extent),
        }
    }
}

#[cfg(test)]
static TEST_WRITE_BUFFER_BYTES: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(usize::MAX);

#[cfg(test)]
pub(crate) fn set_test_write_buffer_bytes(n: Option<usize>) {
    TEST_WRITE_BUFFER_BYTES.store(n.unwrap_or(usize::MAX), std::sync::atomic::Ordering::SeqCst);
}

#[cfg(test)]
static TEST_IFILE_CACHE_BYTES: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(u64::MAX);

#[cfg(test)]
pub(crate) fn set_test_ifile_cache_bytes(n: Option<u64>) {
    TEST_IFILE_CACHE_BYTES.store(n.unwrap_or(u64::MAX), std::sync::atomic::Ordering::SeqCst);
}

fn read_cache_cap_env() -> u64 {
    #[cfg(test)]
    {
        let n = TEST_IFILE_CACHE_BYTES.load(std::sync::atomic::Ordering::SeqCst);
        if n != u64::MAX {
            return n;
        }
    }
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
    subpacket_entries: Vec<morloc_runtime_types::packet::SubpacketEntry>,
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
    /// End of the last complete sub-packet: where a reader stops.
    data_end: u64,
}

/// Parse a mmap'd stream or data packet file into a `ParsedStreamFile`.
/// Caller is responsible for munmap on error.
fn parse_stream_file(
    path: &str,
    mmap_ptr: AbsPtr,
    mmap_size: u64,
) -> Result<ParsedStreamFile, MorlocError> {
    if mmap_size < 32 {
        return Err(MorlocError::Packet(format!(
            "file too short for a packet header\n{}",
            morloc_runtime_types::packet::NOT_A_PACKET,
        )));
    }
    let hdr_bytes = unsafe {
        std::slice::from_raw_parts(mmap_ptr as *const u8, 32)
    };
    let outer_header = PacketHeader::from_bytes(hdr_bytes.try_into().unwrap())?;

    let is_data_packet = outer_header.is_data();
    let mut footer_start: Option<u64> = None;
    let mut scanned_end: Option<u64> = None;
    let (schema_str, subpacket_entries, element_count, diag, final_footer, body_start):
        (String, Vec<morloc_runtime_types::packet::SubpacketEntry>, u64, Option<StreamDiag>, bool, u64) = if is_data_packet {
        let (schema, entries, count) = open_data_packet(path, mmap_ptr, mmap_size)?;
        // DATA-packet files have no stream header; the whole file is a
        // single sub-packet that starts at offset 0. IStream's forward
        // walker reads this as one sub-packet, then sees EOF. There is
        // no StreamDiag and no footer of either kind, so diag = None
        // and final_footer = false; the IFile gate uses is_data_packet
        // separately to know this branch is still random-access safe.
        (schema, entries, count, None, false, 0u64)
    } else if outer_header.is_stream() {
        let StreamHeader { schema: schema_str, body_start } =
            parse_stream_header(mmap_ptr, mmap_size)?;
        // An empty stream (opened + closed with no writes) has its final
        // footer at body_start and no data sub-packets, so read_subpacket_format
        // returns None; only validate the format when a DATA sub-packet exists.
        if body_start < mmap_size {
            if let Some(fmt) = read_subpacket_format(mmap_ptr, mmap_size, body_start)? {
                if fmt != PACKET_FORMAT_VOIDSTAR {
                    return Err(MorlocError::Other(format!(
                        "file '{}' has {}-format sub-packets; only voidstar is supported",
                        path, packet_format_name(fmt)
                    )));
                }
            }
        }
        // IStream walks forward from body_start without needing an
        // index, so an empty subpacket_entries on temp-footer files is
        // fine here; IFile's open path enforces final_footer separately.
        let (subpacket_entries, element_count, diag, final_footer) =
            match try_read_footer(mmap_ptr, mmap_size) {
                Ok(Some(parsed)) => {
                    footer_start = Some(parsed.footer_start);
                    (
                    parsed.subpacket_entries,
                    parsed.element_count,
                    parsed.diag,
                    parsed.final_footer,
                    )
                }
                Ok(None) | Err(_) => {
                    // Writer crashed before any footer. Forward-scan
                    // recovers offsets only; the file will resolve as
                    // IStream (IFile open refuses on !final_footer) so
                    // per-entry counts are never read.
                    let scanned = forward_scan_subpackets(mmap_ptr, mmap_size, body_start)?;
                    scanned_end = Some(body_start + scanned.bytes_scanned);
                    let entries = scanned.subpacket_offsets.into_iter().map(|offset|
                        morloc_runtime_types::packet::SubpacketEntry { offset, elem_count: 0 }
                    ).collect();
                    (entries, scanned.element_count, None, false)
                }
            };
        (schema_str, subpacket_entries, element_count, diag, final_footer, body_start)
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
    // STREAM_PACKET is always list-shaped; DATA_PACKET (IFile) may
    // hold any single value.
    if !is_data_packet {
        reject_non_list_stream_schema(&parsed_schema, "STREAM_PACKET read", path)?;
    }
    let (value_schema, elem_schema) = derive_stream_schemas(&parsed_schema);
    // Every footer, temp or final, follows the last complete sub-packet.
    // A file without one (its writer died mid-flush) is scanned forward.
    let data_end = if is_data_packet {
        match subpacket_entries.last() {
            Some(last) => read_subpacket_size(mmap_ptr, mmap_size, last.offset)
                .map_or(mmap_size, |size| (last.offset + size).min(mmap_size)),
            None => body_start,
        }
    } else if let Some(start) = footer_start {
        start
    } else {
        scanned_end.unwrap_or(body_start)
    };

    Ok(ParsedStreamFile {
        schema_str,
        value_schema,
        elem_schema,
        subpacket_entries,
        element_count,
        diag,
        final_footer,
        is_data_packet,
        body_start,
        data_end,
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

/// Map an already-open descriptor read-only. Used where the caller must
/// hold a lock across both the mapping and whatever it decides from it, so
/// reopening the path would defeat the lock.
fn mmap_fd_readonly(
    fd: std::os::unix::io::RawFd,
    size: u64,
    path: &str,
) -> Result<(AbsPtr, u64), MorlocError> {
    if size == 0 {
        return Err(MorlocError::Packet(format!(
            "file '{}' is empty (cannot be a stream packet)", path
        )));
    }
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
    Ok((ptr as AbsPtr, size))
}

fn mmap_file_readonly(path: &str) -> Result<(AbsPtr, u64), MorlocError> {
    mmap_file_readonly_keep(path).map(|(_, ptr, size)| (ptr, size))
}

/// 'mmap_file_readonly', also returning the open file.
fn mmap_file_readonly_keep(path: &str) -> Result<(std::fs::File, AbsPtr, u64), MorlocError> {
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

    // The mapping pins the inode whether or not the caller keeps `f`.
    Ok((f, ptr as AbsPtr, size))
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
/// Returns `(value_schema_str, subpacket_entries, element_count)`
/// where `subpacket_entries` is a single-element vec `[SubpacketEntry
/// { offset: 0, elem_count: <Array size> }]`. `value_schema_str` is
/// the file's full payload schema string.
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
) -> Result<(String, Vec<morloc_runtime_types::packet::SubpacketEntry>, u64), MorlocError> {
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
    // subpacket_entries = [{ offset: 0, elem_count }]: the "sub-packet"
    // is the whole file packet, whose header begins at byte 0. The
    // element count is the Array header's size field when the value is
    // list-shaped, 0 otherwise.
    Ok((
        value_schema_str,
        vec![morloc_runtime_types::packet::SubpacketEntry {
            offset: 0,
            elem_count: element_count,
        }],
        element_count,
    ))
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
/// Format byte of the sub-packet at `off`, or `None` when that packet is
/// the final footer -- an empty stream (writer opened + closed with no
/// writes) has its footer at `body_start` with no data sub-packets, so the
/// caller has nothing to format-validate.
fn read_subpacket_format(
    mmap_ptr: AbsPtr,
    size: u64,
    off: u64,
) -> Result<Option<u8>, MorlocError> {
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
    if header.is_footer() {
        return Ok(None); // empty stream: footer at body_start, no sub-packets
    }
    if !header.is_data() {
        return Err(MorlocError::Packet(format!(
            "sub-packet at offset {} is not a DATA packet", off
        )));
    }
    // SAFETY: header is_data() implies the data variant of the command.
    let data = unsafe { header.command.data };
    if data.source != morloc_runtime_types::packet::PACKET_SOURCE_MESG {
        return Err(MorlocError::Packet(format!(
            "sub-packet at offset {} has source byte 0x{:02x}; stream \
             files require MESG-source sub-packets",
            off, data.source,
        )));
    }
    Ok(Some(data.format))
}

// ── Footer parsing ────────────────────────────────────────────────────────

#[derive(Debug)]
struct ParsedFooter {
    /// Offset of the footer packet: where the stream's data ends.
    footer_start: u64,
    subpacket_entries: Vec<morloc_runtime_types::packet::SubpacketEntry>,
    element_count: u64,
    diag: Option<StreamDiag>,
    final_footer: bool,
    /// Status byte from `METADATA_TYPE_FOOTER_STATUS` if present, or
    /// `FOOTER_STATUS_CLOSED` when the block is absent (legacy footer).
    /// Parsed here so downstream tooling (`morloc-nexus file`, a
    /// future `@fstatus` intrinsic) can surface it without re-reading
    /// the footer.
    #[allow(dead_code)]
    footer_status: u8,
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

    let mut subpacket_entries: Vec<morloc_runtime_types::packet::SubpacketEntry> =
        Vec::new();
    let mut diag: Option<StreamDiag> = None;
    let mut final_footer = false;
    let mut footer_status =
        morloc_runtime_types::packet::FOOTER_STATUS_CLOSED;
    for (kind, body) in iter_packet_metadata(footer_slice)? {
        match kind {
            METADATA_TYPE_FOOTER_FINAL => { final_footer = true; }
            METADATA_TYPE_STREAM_DIAG => {
                diag = Some(StreamDiag::from_bytes(body)?);
            }
            METADATA_TYPE_SUBPACKET_INDEX => {
                subpacket_entries =
                    morloc_runtime_types::packet::decode_subpacket_index(body)?;
            }
            METADATA_TYPE_FOOTER_STATUS => {
                footer_status =
                    morloc_runtime_types::packet::decode_footer_status(body);
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
        footer_start,
        subpacket_entries,
        element_count,
        diag,
        final_footer,
        footer_status,
    }))
}

// ── Forward-scan recovery ─────────────────────────────────────────────────
//
// The pure walker lives in `morloc-runtime-types::packet::forward_scan_subpackets`.
// This wrapper adapts the mmap raw-pointer + size interface used by
// the runtime to the byte-slice interface the shared walker expects.

fn forward_scan_subpackets(
    mmap_ptr: AbsPtr,
    size: u64,
    body_start: u64,
) -> Result<morloc_runtime_types::packet::ForwardScan, MorlocError> {
    // SAFETY: caller guarantees mmap_ptr..mmap_ptr+size is a valid,
    // read-only mapping for the file's full length. The slice is only
    // consumed within this call; no external references escape.
    let mmap = unsafe {
        std::slice::from_raw_parts(mmap_ptr as *const u8, size as usize)
    };
    morloc_runtime_types::packet::forward_scan_subpackets(mmap, body_start)
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
///
/// # Safety
///
/// `dest_base` must be writable for `handles.len()` slots of `elem_stride` bytes.
pub unsafe fn write_handles_voidstar(
    handles: &[i64],
    dest_base: *mut u8,
    elem_stride: usize,
    cursor: &mut *mut u8,
) -> Result<(), MorlocError> {
    // SAFETY: forwarded from this function's own contract.
    unsafe { shared_write_handles_voidstar(handles, dest_base, elem_stride, cursor) }
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
//      a fresh copy and relocation per access; the per-handle
//      LRU cache hook is in place but not yet populated.
//   3. Walking to the local element and `deep_copy`ing it (or each
//      slice element) into a fresh result SHM block. The result has
//      `elem_schema.width` bytes for one element, or
//      `n_out * elem_width + sub_block_allocs` for a slice.

/// Container schema = Array(elem_schema). Used to build per-subpacket
/// payload schemas from a cached element schema.
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

/// Split a parsed on-disk schema into (value, elem). Streams and IFile
/// DATA_PACKETs both carry the full value schema on disk: for Array,
/// elem is `parameters[0]`; for a single-value IFile (e.g. `IFile Int`),
/// value and elem are the same.
fn derive_stream_schemas(parsed: &Schema) -> (Schema, Schema) {
    if parsed.serial_type == SerialType::Array && !parsed.parameters.is_empty() {
        (parsed.clone(), parsed.parameters[0].clone())
    } else {
        (parsed.clone(), parsed.clone())
    }
}

/// Enforce the "streams are list-shaped" invariant. Returns a
/// MorlocError::Schema on non-Array schemas naming the operation and
/// target so the user sees which stream failed. `op` is a short label
/// (e.g. "OStream open", "open_stdio"); `target` is the path or stdio
/// stream name.
pub(crate) fn reject_non_list_stream_schema(
    schema: &Schema, op: &str, target: &str,
) -> Result<(), MorlocError> {
    if schema.serial_type == SerialType::Array {
        Ok(())
    } else {
        Err(MorlocError::Schema(format!(
            "{}: streams are only supported for list-shaped data, but \
             '{}' has schema type {:?}. Wrap the value in a list.",
            op, target, schema.serial_type,
        )))
    }
}

/// Build the cumulative element-count vector from the footer's
/// per-sub-packet counts, if not already cached. Idempotent; built
/// lazily on first bracket access. Bracket lookups use
/// `partition_point` over the returned vec to locate a global index's
/// sub-packet.
fn ensure_elem_cum(local: &mut ProcessLocalSlot) -> Result<(), MorlocError> {
    if local.subpacket_elem_cum.is_some() {
        return Ok(());
    }
    let n = local.subpacket_entries_local.len();
    let mut cum = Vec::with_capacity(n + 1);
    cum.push(0u64);
    for entry in &local.subpacket_entries_local {
        let last = *cum.last().unwrap();
        cum.push(last.saturating_add(entry.elem_count));
    }
    local.subpacket_elem_cum = Some(cum);
    Ok(())
}

/// Parse a sub-packet's header at `subpacket_off` and return it with the
/// payload's offset and length, having checked that the header, metadata and
/// payload all lie inside the mmap.
fn read_subpacket_header(
    local: &ProcessLocalSlot,
    subpacket_off: u64,
) -> Result<(PacketHeader, u64 /*payload_off*/, u64 /*payload_len*/), MorlocError> {
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
    // Stream files carry embedded voidstar payloads by construction.
    // RPTR (payload is an SHM relptr) is meaningless once the writer
    // exits; FILE (payload is a filename) is meaningless for
    // sub-packets. A non-MESG source is either a producer bug or a
    // corrupt file -- fail loud rather than misinterpret the body.
    if data.source != morloc_runtime_types::packet::PACKET_SOURCE_MESG {
        return Err(MorlocError::Packet(format!(
            "sub-packet at {} has source byte 0x{:02x}; stream files \
             require MESG-source sub-packets (embedded voidstar body)",
            subpacket_off, data.source,
        )));
    }
    let payload_off = subpacket_off + 32 + header.offset as u64;
    let payload_len = header.length as u64;
    if payload_off + payload_len > local.mmap_size {
        return Err(MorlocError::Packet(
            "sub-packet payload past EOF".into(),
        ));
    }
    Ok((header, payload_off, payload_len))
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
    if sub_k >= local.subpacket_entries_local.len() {
        return Err(MorlocError::Other(format!(
            "sub-packet index {} out of range (have {})",
            sub_k, local.subpacket_entries_local.len(),
        )));
    }
    let subpacket_off = local.subpacket_entries_local[sub_k].offset;
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
    let (header, payload_off, payload_len) =
        read_subpacket_header(local, subpacket_off)?;
    let on_disk_size = 32 + header.offset as u64 + header.length;
    let data = unsafe { header.command.data };

    // The producer's Layer-3 vol_idx hint, from the header + metadata
    // bytes. Every copy out of the file relocates by it; walks that read
    // the mmap in place ignore it via the file resolver.
    let vol_idx_hint = {
        // SAFETY: bounds verified by read_subpacket_header.
        let hint_bytes = unsafe {
            std::slice::from_raw_parts(
                (local.mmap_ptr as *const u8).add(subpacket_off as usize),
                32 + header.offset as usize,
            )
        };
        morloc_runtime_types::packet::read_vol_index_from_meta(hint_bytes)
            .ok()
            .flatten()
            .unwrap_or(0)
    };

    // Uncompressed and aligned: walks read directly from the mmap'd
    // region; no SHM allocation, no copy. The kernel page cache shares
    // pages across pools opening the same path. A payload that does not
    // start 8-aligned (a file written before sub-packets were padded, or
    // one joined by @concat after an unpadded one) holds relptrs and
    // numbers no reader may load in place, so it is copied into SHM like
    // a decompressed payload.
    let payload_aligned =
        (local.mmap_ptr as usize).wrapping_add(payload_off as usize) % 8 == 0;
    if data.compression == PACKET_COMPRESSION_NONE && !payload_aligned {
        // SAFETY: bounds verified by read_subpacket_header.
        let bytes = unsafe {
            std::slice::from_raw_parts(
                (local.mmap_ptr as *const u8).add(payload_off as usize),
                payload_len as usize,
            )
        };
        let base = payload_into_shm(bytes, &local.elem_schema, vol_idx_hint)?;
        return Ok((SubpacketSrc::Shm { arr_base: base }, on_disk_size));
    }
    if data.compression == PACKET_COMPRESSION_NONE {
        if payload_len < width::u64_from_usize(std::mem::size_of::<shm_types_crate::Array>()) {
            return Err(MorlocError::Other(format!(
                "sub-packet payload of {} bytes cannot hold its root array", payload_len
            )));
        }
        let arr_base = unsafe {
            (local.mmap_ptr as *const u8).add(payload_off as usize) as AbsPtr
        };
        return Ok((SubpacketSrc::File {
            arr_base,
            payload_base: arr_base,
            payload_len,
            vol_idx_hint,
        }, on_disk_size));
    }

    // Slow path: compressed. Decompress the payload straight into SHM and
    // rebase its relptrs there.
    if data.compression != PACKET_COMPRESSION_ZSTD {
        return Err(MorlocError::Packet(format!(
            "sub-packet at {} has unknown compression byte {}",
            subpacket_off, data.compression,
        )));
    }
    // SAFETY: read_subpacket_header verified the header, metadata and
    // payload, which lie contiguously from `subpacket_off`, inside the mmap.
    let packet = unsafe {
        std::slice::from_raw_parts(
            (local.mmap_ptr as *const u8).add(subpacket_off as usize),
            (payload_off + payload_len - subpacket_off) as usize,
        )
    };
    let base = compressed_payload_into_shm(packet, &local.elem_schema, vol_idx_hint)?;
    Ok((SubpacketSrc::Shm { arr_base: base }, on_disk_size))
}

/// Decompress a zstd sub-packet's payload into one fresh SHM block and
/// relocate it there. `packet` is the whole sub-packet -- header, metadata
/// (which carries the frame index) and payload. The frames decode in
/// parallel directly into the block: no intermediate buffer holds the
/// packet or its decompressed payload.
fn compressed_payload_into_shm(
    packet: &[u8],
    elem_schema: &Schema,
    vol_idx_hint: u16,
) -> Result<AbsPtr, MorlocError> {
    let hdr: [u8; 32] = packet
        .get(..32)
        .and_then(|b| b.try_into().ok())
        .ok_or_else(|| MorlocError::Packet("compressed sub-packet shorter than its header".into()))?;
    let header = PacketHeader::from_bytes(&hdr)?;
    let payload_start = 32 + header.offset as usize;
    let payload_end = payload_start
        .checked_add(header.length as usize)
        .filter(|end| *end <= packet.len())
        .ok_or_else(|| MorlocError::Packet("compressed payload extends past the sub-packet".into()))?;
    let frames = morloc_runtime_types::packet::read_frame_index_from_meta(packet)?
        .ok_or_else(|| MorlocError::Packet(
            "compressed sub-packet missing METADATA_TYPE_FRAME_INDEX entry".into(),
        ))?;
    voidstar::land_compressed_frames(
        &frames,
        &packet[payload_start..payload_end],
        &array_schema(elem_schema),
        vol_idx_hint,
    )
}

/// Resolve a global element index, normalising negatives Python-style.
/// Returns `(sub_packet_k, local_idx)`.
fn resolve_global_index(
    local: &mut ProcessLocalSlot,
    requested: i64,
) -> Result<(usize, u64), MorlocError> {
    ensure_elem_cum(local)?;
    let cum = local
        .subpacket_elem_cum
        .as_ref()
        .expect("ensure_elem_cum just populated subpacket_elem_cum");
    let total = *cum.last().unwrap_or(&0u64);
    let idx_u = slice::resolve_index(requested, total).ok_or_else(|| {
        MorlocError::Other(format!(
            "IFile bracket index {} out of bounds (have {} elements)",
            requested, total,
        ))
    })?;
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
    level: crate::compression::CompressionLevel,
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

    // Built beside the destination and renamed onto it at the end, so a
    // source that is also the destination is read from its original bytes
    // and a merge that fails leaves the destination as it was. The rename
    // refuses a destination a stream is writing.
    let staged = crate::utility::AtomicFile::create(std::path::Path::new(dest))
        .map_err(MorlocError::Io)?;
    let dest_fd = staged.as_raw_fd();
    let mut dest_cursor: u64 = 0;
    let mut merged_entries: Vec<morloc_runtime_types::packet::SubpacketEntry> = Vec::new();
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
                return Err(e);
            }
        };
        let parsed = match parse_stream_file(p, mmap_ptr, mmap_size) {
            Ok(p2) => p2,
            Err(e) => {
                unsafe {
                    libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize);
                }
                return Err(e);
            }
        };
        match &reference_schema {
            None => {
                reference_schema = Some(parsed.schema_str.clone());
            }
            Some(ref_str)
                if !morloc_runtime_types::schema::schema_strings_compatible(
                    ref_str, &parsed.schema_str,
                ) =>
            {
                unsafe {
                    libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize);
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
                }
                return Err(e);
            }
        };

        // First sub-packet's body start = stream header end.
        let body_start = hdr.body_start;
        // Last sub-packet's end: take the largest start offset from the
        // index and read its size, OR if no index, body_start (empty
        // source contributes nothing).
        let body_end = if let Some(last_entry) = parsed.subpacket_entries.last() {
            let last_off = last_entry.offset;
            match read_subpacket_size(mmap_ptr, mmap_size, last_off) {
                Ok(sz) => last_off + sz,
                Err(e) => {
                    unsafe {
                        libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize);
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
                        return Err(MorlocError::Io(e));
        }

        if i == 0 {
            // Preserve the source's stream header verbatim so the
            // merged file's schema metadata block matches.
            if let Err(e) = sendfile_range(dest_fd, src_fd, 0, body_start, dest_cursor) {
                unsafe { libc::close(src_fd); }
                return Err(e);
            }
            dest_cursor += body_start;
        }

        // Remap each source's entries into the dest cursor space.
        for entry in &parsed.subpacket_entries {
            let delta = entry.offset - body_start;
            merged_entries.push(morloc_runtime_types::packet::SubpacketEntry {
                offset: dest_cursor + delta,
                elem_count: entry.elem_count,
            });
        }
        if body_end > body_start {
            let body_len = body_end - body_start;
            if let Err(e) = sendfile_range(dest_fd, src_fd, body_start, body_len, dest_cursor) {
                unsafe { libc::close(src_fd); }
                return Err(e);
            }
            dest_cursor += body_len;
        }
        unsafe { libc::close(src_fd); }
        total_element_count += parsed.element_count;
    }

    // Write the merged final footer (small, fine to pwrite from userspace).
    let mut diag = morloc_runtime_types::packet::StreamDiag::new();
    diag.subpacket_count = merged_entries.len() as u64;
    diag.element_count = total_element_count;
    let footer = morloc_runtime_types::packet::make_final_footer_packet(
        &diag,
        &merged_entries,
        morloc_runtime_types::packet::FOOTER_STATUS_CLOSED,
    );
    pwrite_all_fd(dest_fd, &footer, dest_cursor)?;
    staged.commit().map_err(MorlocError::Io)
}

/// `@stream :: IFile [a] -> <IO> IStream a`: open a fresh ISTREAM handle
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
    if slot.kind.get() != MLC_KIND_IFILE {
        return Err(MorlocError::Other(format!(
            "bracket index on non-IFile handle (kind = {})",
            handle_kind_name(slot.kind.get()),
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
            // SAFETY: a cached entry holds a live SHM packet.
            unsafe { shm::shincref(ce.shm_packet) }?;
            return Ok(SubpacketSrc::Shm { arr_base: ce.shm_packet });
        }
    }
    // Miss: materialise. For uncompressed sub-packets this returns a
    // File descriptor pointing at the mmap (no SHM allocation); for
    // compressed sub-packets a fresh SHM block (refcount = 1).
    let src = materialize_subpacket(local, sub_k)?;

    // Cache compressed (Shm) sub-packets; uncompressed (File) ones
    // don't need our cache -- the kernel pagecache handles them.
    if let (SubpacketSrc::Shm { arr_base }, true) = (&src, local.cache.capacity_bytes > 0) {
        let arr_base = *arr_base;
        let size_bytes = unsafe { shm::shm_block_size(arr_base).unwrap_or(0) } as u64;
        cache_make_room_for(&mut local.cache, size_bytes);
        // Install: shincref so the cache owns one ref and the caller
        // owns the other. Both decrement independently on shfree.
        // SAFETY: arr_base is the live SHM block of the materialised array.
        unsafe { shm::shincref(arr_base) }?;
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
    let elem_width = elem_schema.width;
    let (n, records) = src.records(elem_width)?;
    let idx = width::usize_from_u64(local_idx);
    if idx >= n {
        return Err(MorlocError::Other(format!(
            "local element index {} out of bounds for sub-packet of size {}",
            local_idx, n,
        )));
    }
    let dst = shm::shcalloc(1, elem_width)?;
    // SAFETY: the records were resolved as one region of `n` elements, and
    // dst is a fresh block of one element's width.
    unsafe {
        let elem_src = (records as *const u8).add(idx * elem_width);
        voidstar::deep_copy_with(elem_src, dst, elem_schema, &src.space())?;
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

    if slot.kind.get() != MLC_KIND_IFILE {
        return Err(MorlocError::Other(format!(
            "bracket slice on non-IFile handle (kind = {})",
            handle_kind_name(slot.kind.get()),
        )));
    }
    ensure_elem_cum(local)?;
    let (slice, runs) = {
        let cum = local
            .subpacket_elem_cum
            .as_ref()
            .expect("ensure_elem_cum populates subpacket_elem_cum");
        let slice = slice::Slice::new(
            *cum.last().unwrap_or(&0u64), start, stop, step,
        )?;
        // A forward, unit-step slice with no field tail is one contiguous
        // run in each sub-packet it touches. The runs follow from the cumulative
        // counts by two binary searches, whatever the slice's length.
        let runs = match slice.unit_range() {
            Some((lo, hi)) if tail_steps.is_empty() => {
                let sub_of = |i: u64| cum.partition_point(|&c| c <= i) - 1;
                Some((sub_of(lo)..=sub_of(hi - 1))
                    .map(|k| {
                        let first = lo.max(cum[k]) - cum[k];
                        let end = hi.min(cum[k + 1]) - cum[k];
                        (k, width::usize_from_u64(first), width::usize_from_u64(end - first))
                    })
                    .collect::<Vec<(usize, usize, usize)>>())
            }
            _ => None,
        };
        (slice, runs)
    };
    if let Some(runs) = runs {
        return slice_runs(local, &runs);
    }

    let mut work: SliceWork = {
        let cum = local
            .subpacket_elem_cum
            .as_ref()
            .expect("ensure_elem_cum populates subpacket_elem_cum");
        let plan: Vec<(usize, u64)> = slice
            .indices()
            .map(|idx_u| {
                let sub_k = cum.partition_point(|&c| c <= idx_u) - 1;
                (sub_k, idx_u - cum[sub_k])
            })
            .collect();
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
            work.elem_width, work.proj_offset,
        );
        for (_, s) in std::mem::take(&mut work.materialised) {
            s.release();
        }
        let _ = shm::shfree(arr_ptr);
        return r;
    }
    // The generic route copies element by element and cannot know the
    // result's size until it has, so it builds with a recorded block per
    // variable-length part and consolidates into one block at the end --
    // the shape a pool can release.
    let mut parts: Vec<AbsPtr> = Vec::new();
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
            // Defensive -- should be unreachable given the
            // cum-element-count plan.
            for p in parts {
                let _ = shm::shfree(p);
            }
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
        // The source's space resolves every relptr the copy follows: the
        // mapped payload for an uncompressed sub-packet, SHM for a
        // decompressed one. `proj_offset` hops over any record fields the
        // chain fusion is skipping.
        let space = src.space();
        let copied = src.records(work.elem_width).and_then(|(_, records)| {
            // SAFETY: local_idx is inside the records region just resolved.
            unsafe {
                let elem_src = (records as *const u8)
                    .add(local_idx as usize * work.elem_width)
                    .add(work.proj_offset);
                voidstar::deep_copy_alloc(
                    elem_src, dst, &work.proj_schema, &space,
                    voidstar::CopyAlloc::Recording(&mut parts),
                )
            }
        });
        if let Err(e) = copied {
            // Only `consolidate` gives the recorded blocks back, and it
            // never runs now.
            for p in parts {
                let _ = shm::shfree(p);
            }
            for (_, s) in std::mem::take(&mut work.materialised) {
                s.release();
            }
            let _ = shm::shfree(buf_ptr);
            let _ = shm::shfree(arr_ptr);
            return Err(e);
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
    parts.push(buf_ptr);
    let out_schema = array_schema(&work.proj_schema);
    // SAFETY: `parts` lists every block the copies above took and the
    // element bank they were written into; nothing else points into them.
    unsafe { voidstar::consolidate(arr_ptr, &out_schema, &parts) }
}

/// Allocate one block holding an `Array` header followed by `data_size`
/// bytes of element data, and return `(block, data)`.
///
/// A value handed back to a pool is released by a single `shfree` of the
/// pointer it was given -- that is the only release a pool performs, and it
/// frees one block. So a value must BE one block. Giving the header a block
/// of its own hands the pool a pointer to the header and no way ever to
/// reach the data again, which loses the whole payload on every call.
///
/// The data keeps the alignment it had when it was a block of its own: an
/// `Array` header is exactly one `BLOCK_ALIGN`, and every block begins on
/// that boundary, so putting the header in front moves the data by a whole
/// multiple of its old alignment.
pub(crate) fn alloc_array_block(data_size: usize) -> Result<(AbsPtr, *mut u8), MorlocError> {
    let hdr = std::mem::size_of::<shm_types_crate::Array>();
    debug_assert_eq!(hdr % morloc_runtime_types::shm_types::BLOCK_ALIGN, 0);
    let block = shm::shmalloc(hdr + data_size)?;
    // SAFETY: the block owns `hdr + data_size` bytes, so the data region
    // beginning one header in lies inside it.
    let data = unsafe { (block as *mut u8).add(hdr) };
    Ok((block, data))
}

/// The contiguous runs `(sub_k, first, count)` of a unit-step slice,
/// concatenated into one self-contained SHM `Array` block laid out
/// `[records of every run][variable bytes of run 1][run 2]...`.
///
/// A flat element type is one memcpy of records per run. Otherwise each
/// run's records are copied together with the byte range their live
/// pointers address -- found by walking those pointers, never assumed from
/// where the writer usually places sub-allocations -- and relocated by one
/// rebase bounded by that run's own region.
fn slice_runs(
    local: &mut ProcessLocalSlot,
    runs: &[(usize, usize, usize)],
) -> Result<AbsPtr, MorlocError> {
    let elem_schema = local.elem_schema.clone();
    if voidstar::schema_holds(&elem_schema, SerialType::Table) {
        return Err(MorlocError::Other(
            "IFile slice over elements holding an Arrow table is not supported".into(),
        ));
    }
    let mut sources: Vec<SubpacketSrc> = Vec::with_capacity(runs.len());
    let mut located = Ok(());
    for &(sub_k, _, _) in runs {
        match cache_get_or_materialize(local, sub_k) {
            Ok(src) => sources.push(src),
            Err(e) => {
                located = Err(e);
                break;
            }
        }
    }
    let r = located.and_then(|()| {
        let plans = runs
            .iter()
            .zip(&sources)
            .map(|(&(_, first, count), src)| plan_run(src, &elem_schema, first, count))
            .collect::<Result<Vec<_>, _>>()?;
        copy_runs(&plans, &elem_schema)
    });
    for src in sources {
        src.release();
    }
    r
}

/// Where one run's bytes are in its sub-packet, and how its relptrs map
/// back to payload offsets.
struct RunPlan<'s> {
    src: &'s SubpacketSrc,
    records: *const u8,
    count: usize,
    /// `(offset, len)` in the payload of the bytes the run's live pointers
    /// address, starting 8-aligned; `None` when none is live.
    var: Option<(usize, usize)>,
}

/// The byte length of a sub-packet source's payload.
fn src_payload_len(src: &SubpacketSrc) -> Result<usize, MorlocError> {
    match *src {
        SubpacketSrc::File { payload_len, .. } => Ok(payload_len as usize),
        SubpacketSrc::Shm { arr_base } => unsafe { shm::shm_block_size(arr_base) }
            .ok_or_else(|| MorlocError::Shm("sub-packet block has no size".into())),
    }
}

fn plan_run<'s>(
    src: &'s SubpacketSrc,
    elem_schema: &Schema,
    first: usize,
    count: usize,
) -> Result<RunPlan<'s>, MorlocError> {
    let payload = src.arr_base() as *const u8;
    let arr = unsafe { &*(payload as *const shm_types_crate::Array) };
    if first.checked_add(count).map_or(true, |end| end > arr.size) {
        return Err(MorlocError::Other(format!(
            "IFile slice [{first}, +{count}) exceeds the sub-packet's {} elements",
            arr.size
        )));
    }
    // Every byte the run copies must lie inside its payload: counts and
    // relptrs come from the file, so a truncated or corrupt one must fail
    // here rather than copy whatever follows the payload.
    let payload_end = payload as usize + src_payload_len(src)?;
    let space = src.space();
    let records = space.resolve(arr.data, voidstar::region_len(first + count, elem_schema.width)?)? as *const u8;
    let records_end = (count + first)
        .checked_mul(elem_schema.width)
        .and_then(|n| (records as usize).checked_add(n));
    if records_end.map_or(true, |end| end > payload_end) {
        return Err(MorlocError::Other(
            "IFile slice: the sub-packet's records extend past its payload".into(),
        ));
    }
    let records = unsafe { records.add(first * elem_schema.width) };
    let span = if elem_schema.is_fixed_width() {
        None
    } else {
        let bound = (payload as usize, payload_end);
        unsafe { voidstar::pointer_span(records, count, elem_schema, &space, bound)? }
    };
    // Starting on an 8-byte boundary of the payload keeps every value's
    // alignment in the destination, whose variable regions also start
    // 8-aligned (records of a pointer-bearing type are a multiple of 8
    // wide, and each region is rounded up to 8).
    let var = span.map(|(lo, hi)| {
        let lo_off = (lo - payload as usize) & !7;
        (lo_off, hi - payload as usize - lo_off)
    });
    Ok(RunPlan { src, records, count, var })
}

fn copy_runs(plans: &[RunPlan<'_>], elem_schema: &Schema) -> Result<AbsPtr, MorlocError> {
    let w = elem_schema.width;
    let total: usize = plans.iter().map(|p| p.count).sum();
    let records_size = total * w;
    let var_total: usize = plans.iter().map(|p| p.var.map_or(0, |(_, n)| n.next_multiple_of(8))).sum();

    let (block, buf) = alloc_array_block(records_size + var_total)?;
    let built = (|| -> Result<(), MorlocError> {
        let whole = voidstar::RelWindow::of_block(buf, records_size + var_total)?;
        let mut rec_at = 0usize;
        let mut var_at = records_size;
        for p in plans {
            let payload = p.src.arr_base() as *const u8;
            unsafe {
                std::ptr::copy_nonoverlapping(p.records, buf.add(rec_at), p.count * w);
                if let Some((lo_off, n)) = p.var {
                    std::ptr::copy_nonoverlapping(payload.add(lo_off), buf.add(var_at), n);
                    let producer = match *p.src {
                        SubpacketSrc::File { vol_idx_hint, .. } => {
                            morloc_runtime_types::shm_types::encode_relptr(vol_idx_hint as usize, lo_off)
                        }
                        SubpacketSrc::Shm { .. } => shm::abs2rel(payload.add(lo_off) as AbsPtr)?,
                    };
                    let dst = shm::abs2rel(buf.add(var_at) as AbsPtr)?;
                    let delta = (dst as i64).wrapping_sub(producer as i64) as RelPtr;
                    voidstar::adjust_records_within(
                        buf.add(rec_at) as AbsPtr, p.count, elem_schema, delta,
                        whole.narrow(var_at, n)?,
                    )?;
                    var_at += n.next_multiple_of(8);
                }
            }
            rec_at += p.count * w;
        }
        unsafe {
            let out = &mut *(block as *mut shm_types_crate::Array);
            out.size = total;
            out.data = shm::abs2rel(buf as AbsPtr)?;
        }
        Ok(())
    })();
    if let Err(e) = built {
        let _ = shm::shfree(block);
        return Err(e);
    }
    Ok(block)
}

/// Bulk-pack a String-valued slice into one SHM block.
///
/// Layout in the output buffer:
///   [Array{size,data} * n_out] [concatenated u8 bytes]
///
/// Each Array's `data` relptr addresses into the byte tail of the
/// same buffer. One shmalloc for the whole slice, regardless of N --
/// the value's own `Array` header included, so the caller's single
/// `shfree` gives all of it back. Replaces the per-element shmemcpy in
/// `deep_copy_with`'s String arm under the ALLOC_MUTEX -- the dominant
/// cost for record slices that the chain-fusion path projects down to a
/// String field.
///
/// Caller is responsible for releasing `work.materialised`.
fn slice_bulk_pack_str(
    local: &mut ProcessLocalSlot,
    plan: &mut Vec<(usize, u64)>,
    materialised: &mut std::collections::BTreeMap<usize, SubpacketSrc>,
    elem_width: usize,
    proj_offset: usize,
) -> Result<AbsPtr, MorlocError> {
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
        // The sub-packet's records, resolved once, and the space its
        // per-element string relptrs resolve in.
        arr_data: AbsPtr,
        arr_size: u64,
        space: SrcSpace,
    }
    let mut ctx = SrcCtx {
        sub_k: None,
        arr_data: std::ptr::null::<u8>() as AbsPtr,
        arr_size: 0,
        space: SrcSpace::Shm(voidstar::Arena),
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
            if let SubpacketSrc::File { payload_base, payload_len, .. } = *src {
                // Tell the kernel to prefault this sub-packet's payload
                // range. For a single-pass walk that touches the records
                // section + the string tail, bulk readahead cuts the cost
                // from N synchronous single-page faults to payload_len /
                // readahead-window pages.
                unsafe {
                    libc::madvise(
                        payload_base as *mut libc::c_void,
                        payload_len as usize,
                        libc::MADV_WILLNEED,
                    );
                }
            }
            let (n, arr_data) = src.records(elem_width)?;
            ctx = SrcCtx {
                sub_k: Some(sub_k),
                arr_data,
                arr_size: width::u64_from_usize(n),
                space: src.space(),
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
            ctx.space.resolve(str_arr.data, str_len)?
        };
        srcs.push((src_str_data, str_len));
        total_tail += str_len;
    }

    // One bare shmalloc for the whole slice -- Pass 2 writes every
    // byte (headers in front, string tail behind), so a zero-fill
    // would be 100 % wasted work.
    // The value's own Array header sits in front of the element headers, so
    // the whole slice -- header, elements and bytes -- is one block.
    let buf_size = n_out * hdr_size + total_tail;
    let block = shm::shmalloc(hdr_size + buf_size)?;
    let buf_ptr = unsafe { (block as *mut u8).add(hdr_size) };
    let hdr_start = buf_ptr;
    let mut tail_cursor = unsafe { buf_ptr.add(n_out * hdr_size) };
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
    let arr = unsafe { &mut *(block as *mut shm_types_crate::Array) };
    arr.size = n_out;
    arr.data = buf_relptr;
    Ok(block)
}

/// Parse a `.<step>.<step>...` suffix consisting purely of Field/Key
/// steps. Returns `None` if any bracket, group, or other non-field
/// step is present -- the caller falls back to the general walker
/// (which handles slice+group via broadcast_slice_tail). Used to
/// detect chain-fusable tails after a root-level `.[:]`.
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
        // Terminal-step enforcement: nothing may follow a Group in
        // the same chain (a group already materialises a fresh value
        // that isn't necessarily well-defined for further walking).
        // `BracketSlice` is NOT terminal: `.[:]<tail>` broadcasts
        // <tail> over each element of the slice, matching the
        // compiler's IntrMap desugar. `BracketIndex` is also not
        // terminal.
        if let Some(WalkStep::Group(_)) = out.last() {
            return Err(MorlocError::Other(
                "walk path: groups are terminal -- nothing may follow `.(.x;.y)`".into()
            ));
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
                // Slice consumes 3 runtime args (start, stop, step).
                // Terminal slice deep-copies; slice-with-tail
                // broadcasts the tail over each element (matches the
                // compiler's IntrMap desugar).
                let (s, e, p) = args.next_three()?;
                if cur_schema_ref.serial_type != SerialType::Array {
                    return Err(MorlocError::Other(format!(
                        "walk: bracket-slice requires an Array, got {:?}",
                        cur_schema_ref.serial_type
                    )));
                }
                let remaining = &tail[1..];
                if remaining.is_empty() {
                    return inline_bracket_slice(
                        src, cur_ptr, cur_schema_ref, s, e, p, out,
                    );
                }
                return broadcast_slice_tail(
                    src, cur_ptr, cur_schema_ref, s, e, p,
                    remaining, args, out, out_schema,
                );
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
    let actual = slice::resolve_array_index(idx, arr.size)
        .ok_or_else(|| MorlocError::Other(format!(
            "walk: bracket-index {} out of bounds (size {})", idx, arr.size
        )))?;
    let elem_schema = &arr_schema.parameters[0];
    let data_abs = src.array_data(arr, elem_schema.width)?;
    let elem_ptr = unsafe {
        (data_abs as *const u8).add(actual * elem_schema.width) as AbsPtr
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
    let elem_schema = &arr_schema.parameters[0];
    let elem_w = elem_schema.width;
    let slice = slice::Slice::over_array(arr.size, start, stop, step)?;
    let n_out = width::usize_from_u64(slice.len());
    // Allocate the element bank. shcalloc handles n_out == 0 by
    // returning a sentinel; we still need to size the output header.
    let buf_ptr = if n_out == 0 {
        std::ptr::null::<u8>() as AbsPtr
    } else {
        shm::shcalloc(n_out, elem_w)?
    };
    let data_abs = src.array_data(arr, elem_w)?;
    for (k, i) in slice.positions().enumerate() {
        let elem_src = unsafe { (data_abs as *const u8).add(i * elem_w) as AbsPtr };
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

/// Slice with a tail chain: for each element of the sliced source,
/// walk `tail` on that element, and collect the results into a fresh
/// `Array<tail_result>` written to `out`. Mirrors the compiler's
/// IntrMap desugar (`.[:].tail` lowers to `map (\e -> e.tail) slice`).
///
/// `arr_schema` describes the input Array; `out_schema` describes the
/// output Array (`Array<tail_result_from_elem>`, produced by
/// `infer_chain_schema`).
///
/// Args semantics: the tail is walked once per output element; any
/// arg-consuming step in the tail (BracketIndex, nested BracketSlice)
/// consumes fresh args on each iteration, so the caller must supply
/// enough. In practice broadcast tails are field/tuple-idx / groups
/// with field-only children -- no runtime args -- but this
/// implementation doesn't restrict that.
fn broadcast_slice_tail(
    src: &SubpacketSrc,
    arr_ptr: AbsPtr,
    arr_schema: &Schema,
    start: Option<i64>,
    stop: Option<i64>,
    step: Option<i64>,
    tail: &[WalkStep],
    args: &mut ArgsCursor<'_>,
    out: AbsPtr,
    out_schema: &Schema,
) -> Result<(), MorlocError> {
    debug_assert_eq!(arr_schema.serial_type, SerialType::Array);
    debug_assert_eq!(out_schema.serial_type, SerialType::Array);
    if arr_schema.parameters.is_empty() {
        return Err(MorlocError::Other(
            "walk: Array schema missing element type at slice broadcast".into(),
        ));
    }
    if out_schema.parameters.is_empty() {
        return Err(MorlocError::Other(
            "walk: output Array schema missing element type at slice broadcast".into(),
        ));
    }
    let arr = unsafe { &*(arr_ptr as *const shm_types_crate::Array) };
    let elem_schema = &arr_schema.parameters[0];
    let elem_w = elem_schema.width;
    let out_elem_schema = &out_schema.parameters[0];
    let out_elem_w = out_elem_schema.width;
    let slice = slice::Slice::over_array(arr.size, start, stop, step)?;
    let n_out = width::usize_from_u64(slice.len());
    let buf_ptr = if n_out == 0 {
        std::ptr::null::<u8>() as AbsPtr
    } else {
        shm::shcalloc(n_out, out_elem_w)?
    };
    let data_abs = src.array_data(arr, elem_w)?;
    // The tail's runtime args (any BracketIndex/BracketSlice in the
    // tail) apply uniformly to every element of the slice, matching
    // `map (\e -> e.[i]) slice`. Snapshot the arg cursor, rewind
    // before each element, then advance once after the loop so the
    // outer caller sees a single consumption of the tail's args.
    let snapshot_pos = args.pos;
    let mut per_iter_consumed: Option<usize> = None;
    for (k, i) in slice.positions().enumerate() {
        let elem_src_ptr = unsafe {
            (data_abs as *const u8).add(i * elem_w) as AbsPtr
        };
        let elem_dst_ptr = unsafe {
            (buf_ptr as *mut u8).add(k * out_elem_w) as AbsPtr
        };
        args.pos = snapshot_pos;
        if let Err(e) = walk_into(
            src, elem_src_ptr, elem_schema,
            tail, args, elem_dst_ptr, out_elem_schema,
        ) {
            if !buf_ptr.is_null() { let _ = shm::shfree(buf_ptr); }
            return Err(e);
        }
        let consumed = args.pos - snapshot_pos;
        match per_iter_consumed {
            None => per_iter_consumed = Some(consumed),
            Some(prev) if prev != consumed => {
                if !buf_ptr.is_null() { let _ = shm::shfree(buf_ptr); }
                return Err(MorlocError::Other(format!(
                    "walk: broadcast tail consumed different arg counts \
                     across iterations ({} vs {}); pattern is malformed",
                    prev, consumed,
                )));
            }
            _ => {}
        }
    }
    // Advance the outer cursor once past the tail's arg budget.
    // For empty slices no iteration ran, so we compute the count
    // statically from the tail's step shapes instead of observing it.
    let per_iter = match per_iter_consumed {
        Some(c) => c,
        None => count_walk_args(tail),
    };
    args.pos = snapshot_pos + per_iter;
    let data_rel = if buf_ptr.is_null() {
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

/// Count the number of runtime args a chain of WalkSteps would
/// consume. Each BracketIndex uses 1, each BracketSlice uses 3, Field
/// uses 0. Groups accumulate the counts of every child chain.
/// Used by `broadcast_slice_tail` to skip the tail's arg budget when
/// the slice is empty (no iteration to observe consumption).
fn count_walk_args(steps: &[WalkStep]) -> usize {
    let mut n = 0;
    for s in steps {
        match s {
            WalkStep::Field(_) => {}
            WalkStep::BracketIndex => n += 1,
            WalkStep::BracketSlice => n += 3,
            WalkStep::Group(children) => {
                for c in children {
                    n += count_walk_args(c);
                }
            }
        }
    }
    n
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
                // Terminal slice: shape unchanged. Slice with a tail:
                // broadcast the tail over each element, so the result
                // becomes `Array<tail_result_from_elem>`.
                let remaining = &chain[step_i + 1..];
                if !remaining.is_empty() {
                    if cur.parameters.is_empty() {
                        return Err(MorlocError::Other(
                            "Array schema missing element type at slice broadcast".into(),
                        ));
                    }
                    let elem = cur.parameters[0].clone();
                    let tail_result = infer_chain_schema(&elem, remaining)?;
                    return Ok(array_schema(&tail_result));
                }
                // Else: cur stays unchanged (terminal slice).
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

/// Deep-copy a value of the sub-packet, resolving through its space.
fn deep_copy_one(
    src: &SubpacketSrc,
    cur_ptr: AbsPtr,
    cur_schema: &Schema,
    dst: AbsPtr,
) -> Result<(), MorlocError> {
    unsafe { voidstar::deep_copy_with(cur_ptr, dst, cur_schema, &src.space()) }
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
pub(crate) fn pwrite_all_fd(fd: i32, mut buf: &[u8], mut offset: u64) -> Result<(), MorlocError> {
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
///
/// The non-Linux path falls back to a userspace `pread`/`write` loop:
/// BSD `sendfile` is socket-oriented and can't do file-to-file, and
/// `fcopyfile` doesn't accept a subrange. Correctness is preserved;
/// only the zero-copy fast path is lost off Linux.
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

    #[cfg(target_os = "linux")]
    {
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

    #[cfg(not(target_os = "linux"))]
    {
        let mut buf = [0u8; 64 * 1024];
        let mut src_pos = src_off;
        let mut remaining = count;
        while remaining > 0 {
            let want = std::cmp::min(remaining as usize, buf.len());
            let n = unsafe {
                libc::pread(
                    src_fd,
                    buf.as_mut_ptr() as *mut libc::c_void,
                    want,
                    src_pos as libc::off_t,
                )
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
            let got = n as usize;
            write_all_fd(dest_fd, &buf[..got])?;
            src_pos += got as u64;
            remaining = remaining.saturating_sub(got as u64);
        }
        Ok(())
    }
}

/// Dispatch entry point used by the FFI shim in `intrinsics.rs`.
///
/// OSTREAM is intentionally NOT dispatched here -- the writer needs the
/// element schema in hand at open time, which the typed entry point
/// `mlc_open_ostream(schema_str, path)` provides. Routing OSTREAM
/// through `mlc_open(path, kind)` would create a Nil-schema header on
/// disk, breaking the receiving IStream/IFile reader's schema parse.
pub fn open_dispatch(path: &str, kind: u8) -> Result<i64, MorlocError> {
    if is_stdin_device(path) {
        // stdin is forward-only and lives on the nexus's fd 0. IFile
        // (random access) is impossible; the error is catchable, so the
        // `@catch (@open :: IFile) (@open :: IStream)` idiom falls back to
        // IStream. The reject reads NO bytes, so the fallback consumes the
        // pipe from byte 0. IStream must arrive via the typed
        // `mlc_open_istream` entry (which carries the schema); a bare
        // generic open of stdin as IStream is a codegen/interpreter gap.
        return match kind {
            MLC_KIND_IFILE => Err(MorlocError::Other(
                "@open '/dev/stdin' :: IFile is not seekable; stdin is \
                 forward-only. Open it as IStream (e.g. try IFile first and \
                 fall back to IStream with @catch).".into(),
            )),
            MLC_KIND_ISTREAM => Err(MorlocError::Other(
                "reading stdin as an IStream is not supported in a command \
                 evaluated directly by the nexus (a pure command with no \
                 foreign function calls). Add a call to a sourced function so \
                 the command runs in a language pool, which can read stdin."
                    .into(),
            )),
            _ => Err(MorlocError::Other(format!(
                "mlc_open: stdin device with unsupported kind {} ({})",
                kind, handle_kind_name(kind),
            ))),
        };
    }
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

/// Dispatch entry for the typed `mlc_open_istream(schema_str, path)`.
///
/// A real file seeds its element schema from the on-disk stream header
/// (the `schema_str` arg is consulted only for the stdin sentinel). The
/// stdin sentinel routes to the nexus stdin channel via `open_stdio`,
/// declaring the ascribed `[a]` schema so the nexus can guard the
/// incoming stream against the opener's type.
pub fn open_dispatch_istream(path: &str, schema_str: &str) -> Result<i64, MorlocError> {
    if is_stdin_device(path) {
        return open_stdio(MLC_KIND_ISTREAM, STDIO_KIND_STDIN, schema_str);
    }
    // A stream file is self-describing, so the reader decodes from the
    // file's own schema and needs no argument. The ascribed type still
    // matters: the pool walks the resulting voidstar with its
    // compile-time schema, so a mismatch is a structural walk of a
    // buffer that schema does not describe. Both strings are in hand
    // here, so check once at open rather than producing garbage later.
    let handle = shared_open_istream(path)?;
    if !schema_str.is_empty() {
        let stored = shared_handle_schema_str(handle)?;
        let requested =
            morloc_runtime_types::schema::canonicalize_schema_str(schema_str);
        if !morloc_runtime_types::schema::schema_strings_compatible(
            &stored, &requested,
        ) {
            let _ = shared_close_handle(handle);
            return Err(MorlocError::Other(format!(
                "@open IStream: schema mismatch on '{}': \
                 file has '{}', open requested '{}'",
                path, stored, requested
            )));
        }
    }
    Ok(handle)
}

// ── `morloc-nexus view` conversion helpers ────────────────────────────────
//
// These three functions back the runtime FFI entries used by
// `morloc-nexus view` for packet-type conversion and pattern-walker
// dispatch on footer-less streams. They compose existing shared_*
// primitives (open, next/slice, write, close) so the authoritative
// footer writer stays in one place.

/// Materialise a sub-packet (already read into a byte buffer) into a
/// fresh SHM voidstar `Array<T>`. Given the sub-packet's full bytes
/// (32-byte header + metadata + payload; decompressed by the caller
/// or done here for zstd) and the element schema, returns the AbsPtr
/// of the newly-allocated SHM block. Caller is responsible for
/// freeing via `shm::shfree` (or letting the `eval_arena` sweep on
/// scope drop).
///
/// Used by `morloc-nexus view -` when streaming stdin: nexus reads a
/// sub-packet at a time off fd 0, calls this to materialise it, then
/// hands the voidstar to the appropriate emitter (`print_voidstar_jsonl`,
/// `mlc_write` for OStream output, or `mlc_save_voidstar` for the
/// buffered `-d` arm). Mirrors the compressed-path branch of
/// `materialize_subpacket_at_offset`, but works from a byte slice
/// instead of a mmap region so it can back a pipe/fd-based reader.
pub fn shared_materialize_subpacket_from_bytes(
    subpacket_bytes: &[u8],
    elem_schema: &morloc_runtime_types::schema::Schema,
) -> Result<AbsPtr, MorlocError> {
    if subpacket_bytes.len() < 32 {
        return Err(MorlocError::Packet(format!(
            "materialise sub-packet: {} bytes < 32-byte header",
            subpacket_bytes.len(),
        )));
    }
    let hdr_arr: [u8; 32] = subpacket_bytes[..32].try_into().unwrap();
    let header = PacketHeader::from_bytes(&hdr_arr)?;
    if !header.is_data() {
        return Err(MorlocError::Packet(format!(
            "materialise sub-packet: header cmd_type = {}; expected DATA",
            unsafe { header.command.cmd_type.cmd_type },
        )));
    }
    let data = unsafe { header.command.data };
    if data.format != PACKET_FORMAT_VOIDSTAR {
        return Err(MorlocError::Packet(format!(
            "materialise sub-packet: format = {}; expected voidstar",
            packet_format_name(data.format),
        )));
    }
    let meta_len = header.offset as usize;
    let payload_len = header.length as usize;
    let total = 32 + meta_len + payload_len;
    if subpacket_bytes.len() < total {
        return Err(MorlocError::Packet(format!(
            "materialise sub-packet: buffer holds {} bytes but header \
             declares {}+{}+{}={}",
            subpacket_bytes.len(), 32, meta_len, payload_len, total,
        )));
    }

    // Decompress (if needed), then copy the payload into one SHM block and
    // relocate it there, by the producer's vol_idx hint.
    let vol_idx_hint = morloc_runtime_types::packet::read_vol_index_from_meta(
        &subpacket_bytes[..32 + meta_len],
    )
    .ok()
    .flatten()
    .unwrap_or(0);
    match data.compression {
        PACKET_COMPRESSION_NONE => {
            payload_into_shm(&subpacket_bytes[32 + meta_len..total], elem_schema, vol_idx_hint)
        }
        PACKET_COMPRESSION_ZSTD => {
            compressed_payload_into_shm(&subpacket_bytes[..total], elem_schema, vol_idx_hint)
        }
        other => Err(MorlocError::Packet(format!(
            "materialise sub-packet: unknown compression byte {}",
            other,
        ))),
    }
}

/// Convert a MORLOC_STREAM_PACKET file to another MORLOC_STREAM_PACKET
/// file. Drains the input via IStream `@next` and rewrites each
/// element batch via OStream `@write`, so recompression, schema
/// override, and footer normalisation all happen through the same
/// authoritative writers used by `@write` / `@close`.
///
/// Handles footer-less input as a first-class case (IStream forward-
/// walks regardless of footer state).
///
/// Returns the number of sub-packets emitted to the output. The size
/// guardrail check (`--force` gate) is applied by the caller before
/// invoking this function; the runtime does not have a policy on
/// output size.
pub fn shared_view_stream_to_stream(
    in_path: &str,
    out_path: &str,
    compression_level: u8,
    schema_override: Option<&str>,
) -> Result<u64, MorlocError> {
    let compression_level = crate::compression::CompressionLevel::from_u8(compression_level)?;
    // Determine the OStream's element schema. Prefer the caller's
    // override; otherwise pull the stored element schema from the
    // input file's stream header.
    let schema_str = if let Some(s) = schema_override {
        s.to_string()
    } else {
        read_schema_from_file(in_path)?
    };

    let in_handle = shared_open_istream(in_path)?;
    let close_in_on_err = |e: MorlocError| -> MorlocError {
        let _ = shared_discard_handle(in_handle);
        e
    };

    let out_handle = shared_open_ostream_with_schema(out_path, &schema_str)
        .map_err(close_in_on_err)?;

    let cleanup = |e: MorlocError| -> MorlocError {
        let _ = shared_discard_handle(out_handle);
        let _ = shared_discard_handle(in_handle);
        e
    };

    // Drain: pull one sub-packet from the IStream and hand it to
    // the OStream, until the end of the stream. An empty sub-packet is
    // an empty frame and is dropped, as `@write` of an empty list is.
    let mut n_written: u64 = 0;
    loop {
        let sub_ptr = match shared_next_frame(in_handle).map_err(cleanup)? {
            Some(p) => p,
            None => break,
        };
        let size = unsafe { *(sub_ptr as *const u64) };
        if size == 0 {
            let _ = crate::shm::shfree(sub_ptr);
            continue;
        }
        if let Err(e) = shared_write_subpacket(out_handle, compression_level, sub_ptr) {
            let _ = crate::shm::shfree(sub_ptr);
            return Err(cleanup(e));
        }
        let _ = crate::shm::shfree(sub_ptr);
        n_written += 1;
    }

    shared_close_handle(out_handle).map_err(|e| {
        let _ = shared_discard_handle(in_handle);
        e
    })?;
    shared_close_handle(in_handle)?;
    Ok(n_written)
}

/// Convert a MORLOC_DATA_PACKET file to a MORLOC_STREAM_PACKET file.
/// Opens the input as an IFile so element access uses the existing
/// mmap + bracket-slice discipline, iterates in chunks sized by a
/// per-file heuristic (~1/10 of one OStream frame per chunk), and
/// writes each chunk to a fresh OStream. The OStream's own buffering
/// coalesces chunks into full-size sub-packets.
///
/// Chunk size heuristic (per plan): target ~`FRAME_CHUNK_SIZE / 10`
/// bytes per read range, computed from the input's average element
/// size. Clamped to `[1, 1 << 20]`. Tiny-element degenerate cases
/// (`[Bool]`, `[Unit]`) get the upper clamp so a billion-element
/// chunk doesn't overflow the bracket-slice i64.
///
/// Returns the number of sub-packets emitted. Refuses compressed
/// DATA inputs (matches `open_data_packet`).
pub fn shared_view_data_to_stream(
    in_path: &str,
    out_path: &str,
    compression_level: u8,
) -> Result<u64, MorlocError> {
    let compression_level = crate::compression::CompressionLevel::from_u8(compression_level)?;
    use morloc_runtime_types::schema::schema_to_string;

    let in_handle = shared_open_ifile(in_path)?;

    // Pull the value schema `[a]` from the process-local slot. Streams
    // are list-shaped so the OStream we open takes the same schema as
    // the input's on-disk value schema.
    let value_schema_str = with_process_local_slot(in_handle, |local, _slot| {
        Ok(schema_to_string(&local.value_schema))
    })?;

    // Element count for the loop bound.
    let elem_count = shared_handle_length(in_handle)?;
    // File size for the average-element heuristic.
    let file_size = std::fs::metadata(in_path)
        .map(|m| m.len())
        .unwrap_or(0);

    let chunk_elems = compute_view_chunk_elems(elem_count, file_size);

    let out_handle = shared_open_ostream_with_schema(out_path, &value_schema_str)
        .map_err(|e| {
            let _ = shared_discard_handle(in_handle);
            e
        })?;

    let cleanup_on_err = |e: MorlocError| -> MorlocError {
        let _ = shared_discard_handle(out_handle);
        let _ = shared_discard_handle(in_handle);
        e
    };

    let mut n_written: u64 = 0;
    let mut i: u64 = 0;
    while i < elem_count {
        let stop = std::cmp::min(i + chunk_elems, elem_count);
        let slice_ptr = shared_ifile_bracket_slice_with_tail(
            in_handle,
            Some(i as i64),
            Some(stop as i64),
            None,
            &[],
        ).map_err(cleanup_on_err)?;
        if let Err(e) = shared_write_subpacket(out_handle, compression_level, slice_ptr) {
            let _ = crate::shm::shfree(slice_ptr);
            return Err(cleanup_on_err(e));
        }
        let _ = crate::shm::shfree(slice_ptr);
        n_written += 1;
        i = stop;
    }

    shared_close_handle(out_handle).map_err(|e| {
        let _ = shared_discard_handle(in_handle);
        e
    })?;
    shared_close_handle(in_handle)?;
    Ok(n_written)
}

/// Chunk-size heuristic for `shared_view_data_to_stream`. Aims for
/// ~1/10 of an OStream frame per read range so the OStream buffer
/// coalesces about 10 chunks into a full-size sub-packet. Clamped to
/// `[1, 1 << 20]` -- the upper bound prevents tiny-element degenerate
/// cases (`[Bool]`, `[Unit]`) from overflowing the bracket-slice i64
/// or putting a billion elements into one chunk.
fn compute_view_chunk_elems(elem_count: u64, file_size: u64) -> u64 {
    const TARGET_CHUNK_BYTES: u64 = (16u64 << 20) / 10;
    const MIN_CHUNK: u64 = 1;
    const MAX_CHUNK: u64 = 1 << 20;
    if elem_count == 0 {
        return MIN_CHUNK;
    }
    let per_elem = (file_size / elem_count).max(1);
    let raw = TARGET_CHUNK_BYTES / per_elem;
    raw.clamp(MIN_CHUNK, MAX_CHUNK)
}

/// Open a footer-less MORLOC_STREAM_PACKET file as an IFile, trusting
/// caller-supplied forward-scan output (sub-packet offsets and
/// element count) in place of the missing final footer. On a
/// file WITH a final footer this delegates to `shared_open_ifile`
/// so the caller's offsets are ignored -- the on-disk footer is
/// authoritative when present.
///
/// The caller (`morloc-nexus view --pattern` on a truncated stream)
/// runs `forward_scan_subpackets` first, then hands the offsets +
/// element count here.
pub fn shared_open_ifile_recovered(
    path: &str,
    caller_offsets: &[u64],
    caller_counts: &[u64],
    caller_element_count: u64,
) -> Result<i64, MorlocError> {
    use std::sync::atomic::Ordering;

    reject_dev_stdio_path(path)?;

    let (map_file, mmap_ptr, mmap_size) = mmap_file_readonly_keep(path)?;
    let (file_dev, file_ino) = file_identity_of(&map_file);
    drop(map_file);
    let parsed = match parse_stream_file(path, mmap_ptr, mmap_size) {
        Ok(p) => p,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };

    // If the file has a clean final footer, honour it and dispose of
    // the caller-supplied offsets. This keeps the "cleanly closed
    // file is authoritative" invariant.
    if parsed.is_data_packet || parsed.final_footer {
        unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
        return shared_open_ifile(path);
    }

    // Choose the entries to publish. Prefer caller's if non-empty
    // (nexus's forward-scan may have counted better than the runtime's).
    // Caller must pass a counts slice of matching length (or empty for
    // the delegated path).
    if !caller_offsets.is_empty() && caller_offsets.len() != caller_counts.len() {
        unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
        return Err(MorlocError::Other(format!(
            "shared_open_ifile_recovered: caller_offsets has {} entries but \
             caller_counts has {} entries; they must match",
            caller_offsets.len(), caller_counts.len(),
        )));
    }
    let (subpacket_entries, element_count): (Vec<morloc_runtime_types::packet::SubpacketEntry>, u64) =
        if !caller_offsets.is_empty() {
            (
                caller_offsets.iter().zip(caller_counts.iter()).map(|(&offset, &elem_count)|
                    morloc_runtime_types::packet::SubpacketEntry { offset, elem_count }
                ).collect(),
                caller_element_count,
            )
        } else {
            (parsed.subpacket_entries.clone(), parsed.element_count)
        };

    // Allocate a slot and publish (mirrors shared_open_ifile).
    let (_slot_idx, slot, _guard) = match allocate_slot_cas() {
        Ok(s) => s,
        Err(e) => {
            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
            return Err(e);
        }
    };

    let publish_result = (|| -> Result<u64, MorlocError> {
        let mut pending = Unpublished::default();
        let path_rel = pending.hold(shm_copy_bytes(path.as_bytes())?);
        let schema_rel = pending.hold(shm_copy_bytes(parsed.schema_str.as_bytes())?);
        let idx_rel = if !subpacket_entries.is_empty() {
            pending.hold(shm_copy_entries_slice(&subpacket_entries)?)
        } else {
            shm_types_crate::RELNULL
        };
        unsafe {
            slot.kind.set(MLC_KIND_IFILE);
            slot.file_dev.set(file_dev);
            slot.file_ino.set(file_ino);
            slot.file_path.set(pending.own(path_rel));
            slot.file_path_len.set(path.len() as u32);
            slot.schema_str.set(pending.own(schema_rel));
            slot.schema_str_len.set(parsed.schema_str.len() as u32);
            slot.subpacket_entries.set(pending.own(idx_rel));
            slot.subpacket_entries_len.set(subpacket_entries.len() as u64);
            slot.subpacket_entries_cap.set(0);
            slot.body_start.set(parsed.body_start);
            // Mark as clean so downstream bracket walkers don't refuse
            // the handle. The trust boundary is at this function: if
            // the caller's forward-scan lied, the walker's mmap
            // bounds check catches it at access time.
            slot.final_footer.set(1);
            slot.cursor.set(0);
            slot.element_count.set(element_count);
            slot.compression_level.set(0);
            slot.write_buffer.set(shm_types_crate::RELNULL);
            slot.write_buffer_index_cap.set(0);
            slot.write_buffer_index_count.set(0);
            slot.write_buffer_data_used.set(0);
            if let Some(d) = parsed.diag.as_ref() {
                *slot.diag.get() = *d;
            }
        }
        let bump = registry_gen_salt() | 1;
        // wrapping_add: the generation is a wrapping counter masked to
        // GENERATION_MASK; a large random salt can overflow u64, which
        // panics in debug builds without the wrapping form.
        let new_gen = slot.generation.fetch_add(bump, Ordering::AcqRel)
            .wrapping_add(bump)
            & GENERATION_MASK;
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

    // Install the process-local slot mirroring shared_open_ifile's
    // final block so future access reuses the mmap without re-parsing.
    let cap_bytes = read_cache_cap_env();
    let local = ProcessLocalSlot {
        cached_generation: new_gen,
        mmap_ptr,
        mmap_size,
        pages_dropped: 0,
        map_file: None,
        fd: -1,
        cache: crate::fork_policy::ForkLocal::new(Box::new(StreamCache::new(cap_bytes))),
        value_schema: parsed.value_schema.clone(),
        elem_schema: parsed.elem_schema.clone(),
        subpacket_entries_local: subpacket_entries,
        subpacket_elem_cum: None,
        is_data_packet: parsed.is_data_packet,
        holds_lock: false,
        fork_epoch: fork_epoch(),
    };
    let handle = pack_handle(new_gen, _slot_idx);
    install_process_local_slot(handle, local);
    Ok(handle)
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
            ".[a]",              // bracket with non-empty contents
            ".[",                // unclosed bracket
        ];
        for path in bad {
            assert!(parse_walk_path(path).is_err(),
                    "expected parse failure for {:?}", path);
        }
    }

    /// Build a minimal valid STREAM_PACKET file (no sub-packets) and
    /// confirm open_ifile + close_handle round-trip. Sub-packet walking
    /// + cache + pattern eval are exercised in task #13's integration
    /// tests.
    // A slot's path/schema/index/buffer blocks belong to the registry.
    // Their lifetime is the slot's, which spans dispatches and can be
    // shared across processes, so allocating them while an eval arena
    // is active must not enroll them in that arena. If it does, arena
    // scope exit frees blocks the registry still owns and the eventual
    // slot release frees them a second time.
    #[test]
    fn arena_scope_exit_leaves_slot_blocks_owned() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!(
            "morloc_arena_slot_test_{}", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("arena_slot.idx");

        let schema = crate::schema::Schema::primitive(
            crate::schema::SerialType::Uint32,
        );
        let schema_str =
            morloc_runtime_types::schema::schema_to_string(&list_schema(&schema));

        let handle = {
            let _arena = crate::eval_arena::enter().unwrap();
            shared_open_ostream_with_schema(path.to_str().unwrap(), &schema_str)
                .unwrap()
        };

        let (_gen, slot_idx) = unpack_handle(handle);
        let slot = slot_ref(slot_idx).expect("slot index in range");
        for (field, rel) in [
            ("file_path", slot.file_path.get()),
            ("schema_str", slot.schema_str.get()),
            ("subpacket_entries", slot.subpacket_entries.get()),
            ("write_buffer", slot.write_buffer.get()),
        ] {
            if rel == shm_types_crate::RELNULL {
                continue;
            }
            let abs = crate::shm::rel2abs(rel).expect("slot relptr resolves");
            let rc = unsafe { crate::shm::reference_count(abs) };
            assert!(
                matches!(rc, Some(c) if c > 0),
                "slot {} block was released while the slot still owns it (refcount {:?})",
                field, rc,
            );
        }

        shared_close_handle(handle).unwrap();
        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn open_close_empty_stream_file() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("empty.idx");

        // Create a canonical empty CLOSED stream by opening a file-backed
        // OStream and closing it with no writes -- the writer emits the
        // header plus an empty final footer. A cleanly-closed empty stream
        // opens as an IFile; a footerless (header-only) file would
        // correctly be rejected, since random access needs the footer's
        // sub-packet index.
        let schema = crate::schema::Schema::primitive(
            crate::schema::SerialType::Uint32,
        );
        let schema_str =
            morloc_runtime_types::schema::schema_to_string(&list_schema(&schema));
        let out = shared_open_ostream_with_schema(path.to_str().unwrap(), &schema_str)
            .unwrap();
        shared_close_handle(out).unwrap();

        let handle = shared_open_ifile(path.to_str().unwrap()).unwrap();
        assert!(handle > 0);
        let kind = shared_handle_kind(handle).unwrap();
        assert_eq!(kind, MLC_KIND_IFILE);
        shared_close_handle(handle).unwrap();

        // Re-close is an error.
        assert!(shared_close_handle(handle).is_err());

        let _ = std::fs::remove_file(&path);
    }

    // A pool hands the runtime the schema string the compiler baked into
    // its dispatch table, which carries the language's concrete-type hint
    // (`a<dict>m...`). The stream header stores the hint-free form,
    // because every writer renders through `schema_to_string`. An append
    // must accept the pool's form: the two describe one wire type.
    //
    // The nexus's own evaluator normalizes before calling, so only the
    // pool path exercises this.
    #[test]
    fn concat_supports_in_place_append() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("inplace");
        let a = dir.join("a.idx");
        let b = dir.join("b.idx");
        write_int_stream(&a, &[&[1, 2, 3]]);
        write_int_stream(&b, &[&[4, 5]]);

        // Merging a file into itself is the obvious way to append a batch
        // to a log, and it must not consume the file it is reading.
        concat_files(
            &[a.to_str().unwrap(), b.to_str().unwrap()],
            a.to_str().unwrap(),
        )
        .expect("a destination that is also a source must be supported");
        assert_eq!(stream_len(&a), 5);
    }

    #[test]
    fn concat_preserves_dest_on_missing_source() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("missing");
        let good = dir.join("good.idx");
        let dest = dir.join("dest.idx");
        write_int_stream(&good, &[&[1, 2, 3]]);
        write_int_stream(&dest, &[&[9, 9, 9, 9]]);

        // A merge that cannot run must leave the destination alone; it is
        // a file the user asked to merge into, not scratch space.
        let missing = dir.join("not-here.idx");
        assert!(
            concat_files(
                &[good.to_str().unwrap(), missing.to_str().unwrap()],
                dest.to_str().unwrap(),
            )
            .is_err(),
            "a missing source must be an error",
        );
        assert!(dest.exists(), "the destination must survive a failed merge");
        assert_eq!(stream_len(&dest), 4);
    }

    #[test]
    fn concat_preserves_dest_on_schema_mismatch() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("schema");
        let ints = dir.join("ints.idx");
        let other = dir.join("other.idx");
        let dest = dir.join("dest.idx");
        write_int_stream(&ints, &[&[1, 2]]);
        std::fs::write(
            &other,
            build_stream_file(
                &TSchema::primitive(TSerialType::Sint32),
                &[&[3, 4]],
            ),
        )
        .unwrap();
        write_int_stream(&dest, &[&[7, 7, 7]]);

        assert!(
            concat_files(
                &[ints.to_str().unwrap(), other.to_str().unwrap()],
                dest.to_str().unwrap(),
            )
            .is_err(),
            "sources disagreeing on element type must be an error",
        );
        assert!(dest.exists(), "the destination must survive a failed merge");
        assert_eq!(stream_len(&dest), 3);
    }

    #[test]
    fn concat_leaves_no_temp_on_failure() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("temps");
        let good = dir.join("good.idx");
        let dest = dir.join("dest.idx");
        write_int_stream(&good, &[&[1]]);
        write_int_stream(&dest, &[&[2]]);

        let missing = dir.join("not-here.idx");
        let _ = concat_files(
            &[good.to_str().unwrap(), missing.to_str().unwrap()],
            dest.to_str().unwrap(),
        );

        // Building beside the destination is only safe if the scaffolding
        // is removed when the build does not finish.
        let strays: Vec<String> = std::fs::read_dir(&dir)
            .unwrap()
            .flatten()
            .map(|e| e.file_name().to_string_lossy().into_owned())
            .filter(|n| n.contains(".tmp."))
            .collect();
        assert!(strays.is_empty(), "left behind: {:?}", strays);
    }

    #[test]
    // SLOT-10, SHM-8: an opener holds neither the file's lock nor a counted
    // reference, so an open stream does not keep it from retiring.
    fn an_open_output_stream_does_not_keep_its_opener_from_retiring() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("retire_lock");
        let p = dir.join("out.idx").to_str().unwrap().to_string();
        let q = p.clone();
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(move || {
            let h = shared_open_ostream_with_schema(&q, "ai4").unwrap();
            let schema = parse_schema("ai4").unwrap();
            let v = crate::json::read_json_with_schema("[1,2,3]", &schema).unwrap();
            shared_write_subpacket(h, crate::compression::CompressionLevel::NONE, v).unwrap();
            crate::shm::shfree(v).unwrap();
            let open = held_stream_locks() == 0 && morloc_retire_blockers() == 0;
            if !open {
                eprintln!("open: {} locks, {} blockers", held_stream_locks(), morloc_retire_blockers());
            }
            open
        }));
    }

    #[test]
    fn a_closed_output_stream_leaves_no_reference_counted_to_its_process() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("held_refs");
        let p = dir.join("out.idx").to_str().unwrap().to_string();
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(move || {
            let schema = parse_schema("ai4").unwrap();
            let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();
            for _ in 0..3 {
                let v = crate::json::read_json_with_schema("[1,2,3]", &schema).unwrap();
                shared_write_subpacket(h, crate::compression::CompressionLevel::NONE, v).unwrap();
                crate::shm::shfree(v).unwrap();
            }
            shared_close_handle(h).unwrap();
            let held = crate::shm::held_references();
            if held != 0 {
                eprintln!("a closed output stream left {held} references counted");
            }
            held == 0
        }));
    }

    fn concat_test_dir(tag: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "morloc_concat_{}_{}", tag, std::process::id()
        ));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn write_int_stream(path: &std::path::Path, subs: &[&[i64]]) {
        let elem = TSchema::primitive(TSerialType::Sint64);
        std::fs::write(path, build_stream_file(&elem, subs)).unwrap();
    }

    fn stream_len(path: &std::path::Path) -> u64 {
        let h = open_ifile(path.to_str().unwrap()).unwrap();
        let n = handle_length(h).unwrap();
        shared_close_handle(h).unwrap();
        n
    }

    #[test]
    fn append_creates_an_absent_file() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!(
            "morloc_append_create_test_{}", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let schema = "ai4";

        // Appending to a path that does not exist yet starts the file.
        // Without this an append-only log has no way to begin except by
        // falling back to an open, which truncates.
        let fresh = dir.join("fresh.idx");
        let f = fresh.to_str().unwrap();
        let _ = std::fs::remove_file(f);
        let h = shared_append_to_path(f, schema)
            .expect("append must create a file that is not there yet");
        shared_close_handle(h).unwrap();

        // What it created is a stream file the ordinary readers accept.
        let (mp, sz) = mmap_file_readonly(f).unwrap();
        let parsed = parse_stream_file(f, mp, sz).unwrap();
        unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize); }
        assert_eq!(parsed.schema_str, schema);
        assert_eq!(parsed.element_count, 0);

        // A second append opens the file it just made, rather than
        // creating it again.
        let h2 = shared_append_to_path(f, schema).expect("second append");
        shared_close_handle(h2).unwrap();

        let _ = std::fs::remove_file(f);
    }

    /// Fork a child that holds every descriptor it inherited until
    /// `release_forked_holder`. Returns the child's pid and the pipe end
    /// that releases it.
    fn fork_holder() -> (libc::pid_t, libc::c_int) {
        let mut fds = [0 as libc::c_int; 2];
        assert_eq!(unsafe { morloc_runtime_types::fd::pipe(fds.as_mut_ptr()) }, 0);
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            unsafe {
                libc::close(fds[1]);
                let mut b = 0u8;
                libc::read(fds[0], &mut b as *mut u8 as *mut libc::c_void, 1);
                libc::_exit(0);
            }
        }
        unsafe { libc::close(fds[0]); }
        (pid, fds[1])
    }

    fn release_forked_holder((pid, release): (libc::pid_t, libc::c_int)) {
        let mut status = 0;
        unsafe {
            libc::close(release);
            libc::waitpid(pid, &mut status, 0);
        }
    }

    fn has_local_slot(handle: i64) -> bool {
        PROCESS_LOCAL_SLOTS.lock().as_ref().is_some_and(|m| m.contains_key(&handle))
    }

    #[test]
    fn a_readers_slot_for_a_stream_another_process_closed_is_released() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("stale_reader");
        let p = dir.join("in.idx").to_str().unwrap().to_string();
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(move || {
            // The stream is written elsewhere, so this process only reads
            // and runs no release service: only a dispatch end can drop it.
            let writer = unsafe { libc::fork() };
            if writer == 0 {
                let schema = parse_schema("ai4").unwrap();
                let w = shared_open_ostream_with_schema(&p, "ai4").unwrap();
                for _ in 0..3 {
                    let v = crate::json::read_json_with_schema("[1,2,3]", &schema).unwrap();
                    shared_write_subpacket(w, crate::compression::CompressionLevel::NONE, v).unwrap();
                    crate::shm::shfree(v).unwrap();
                }
                let ok = shared_close_handle(w).is_ok();
                unsafe { libc::_exit(if ok { 0 } else { 1 }) };
            }
            let mut wst = 0;
            unsafe { libc::waitpid(writer, &mut wst, 0) };
            let r = shared_open_istream(&p).unwrap();
            if let Some(frame) = shared_next_frame(r).unwrap() {
                crate::shm::shfree(frame).unwrap();
            }
            let had = has_local_slot(r);
            let closer = unsafe { libc::fork() };
            if closer == 0 {
                let ok = shared_close_handle(r).is_ok();
                unsafe { libc::_exit(if ok { 0 } else { 1 }) };
            }
            let mut st = 0;
            unsafe { libc::waitpid(closer, &mut st, 0) };
            let (id, prev) = crate::intrinsics::begin_dispatch();
            crate::intrinsics::end_dispatch(id, prev);
            let released = !has_local_slot(r);
            let held = crate::shm::held_references();
            if !(had && released && held == 0) {
                eprintln!("slot before {had}, released {released}, references held {held}");
            }
            had && released && held == 0
        }));
    }

    #[test]
    fn a_forked_child_does_not_keep_a_finished_stream_locked() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("fork_lock");
        let schema = "ai4";

        // A child forked while a stream is open shares the descriptor that
        // holds its lock. Ending the stream must free the path anyway, or
        // a pool whose user code forked workers could never reopen it.
        type End = fn(i64) -> Result<(), MorlocError>;
        let ends: [(&str, End); 2] = [
            ("close", shared_close_handle),
            ("discard", shared_discard_handle),
        ];
        for (name, end) in ends {
            let path = dir.join(format!("{name}.idx"));
            let p = path.to_str().unwrap();
            let h = shared_open_ostream_with_schema(p, schema).unwrap();
            let holder = fork_holder();
            end(h).unwrap();
            let again = shared_append_to_path(p, schema);
            release_forked_holder(holder);
            let h2 = again.unwrap_or_else(|e| {
                panic!("append after {name} while a forked child lives: {e:?}")
            });
            shared_close_handle(h2).unwrap();
        }
    }

    fn run_in_forked_child(work: fn()) -> libc::c_int {
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            unsafe { libc::alarm(10); }
            let ok = std::panic::catch_unwind(work).is_ok();
            unsafe { libc::_exit(if ok { 0 } else { 2 }); }
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0); }
        status
    }

    #[test]
    fn first_uses_on_two_threads_build_one_registry() {
        use std::sync::atomic::{AtomicBool, Ordering};
        static AT_GAP: AtomicBool = AtomicBool::new(false);
        static RESUME: AtomicBool = AtomicBool::new(false);
        let _shm = crate::own_test_registry();
        registry_teardown();
        registry_reopen();
        AT_GAP.store(false, Ordering::SeqCst);
        RESUME.store(false, Ordering::SeqCst);
        *BOOTSTRAP_GAP_HOOK.lock().unwrap() = Some(|| {
            if std::thread::current().name() == Some("first-user") {
                AT_GAP.store(true, Ordering::SeqCst);
                while !RESUME.load(Ordering::SeqCst) {
                    std::thread::yield_now();
                }
            }
        });
        let first = std::thread::Builder::new()
            .name("first-user".into())
            .spawn(registry_bootstrap)
            .unwrap();
        while !AT_GAP.load(Ordering::SeqCst) {
            std::thread::yield_now();
        }
        registry_bootstrap().unwrap();
        let base = REGISTRY_BASE.load(Ordering::SeqCst);
        RESUME.store(true, Ordering::SeqCst);
        first.join().unwrap().unwrap();
        *BOOTSTRAP_GAP_HOOK.lock().unwrap() = None;
        let still_mapped = unsafe { libc::msync(base as *mut libc::c_void, 4096, libc::MS_ASYNC) } == 0;
        let published = REGISTRY_BASE.load(Ordering::SeqCst);
        assert!(still_mapped, "a second first use unmapped the registry the first one published");
        assert_eq!(published, base, "the published registry changed under a thread using it");
    }

    #[test]
    fn a_forked_child_leaves_its_parents_cached_reads_alone() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("fork_cache");
        let path = dir.join("z.idx");
        let p = path.to_str().unwrap().to_string();
        crate::write_behind::set_test_depth(Some(0));
        let w = shared_open_ostream_with_schema(&p, "ai8").unwrap();
        let list = parse_schema("ai8").unwrap();
        let level = crate::compression::CompressionLevel::from_u8(3).unwrap();
        for json in ["[1, 2, 3]", "[4, 5]"] {
            let v = crate::json::read_json_with_schema(json, &list).unwrap();
            shared_write_subpacket(w, level, v).unwrap();
            shm::shfree(v).unwrap();
            shared_flush_buffer(w).unwrap();
        }
        shared_close_handle(w).unwrap();
        crate::write_behind::set_test_depth(None);

        static HANDLE: std::sync::atomic::AtomicI64 = std::sync::atomic::AtomicI64::new(0);
        let f = open_ifile(&p).unwrap();
        HANDLE.store(f, std::sync::atomic::Ordering::SeqCst);
        let read = |i: i64| -> i64 {
            let ptr = ifile_bracket_index(f, i).unwrap();
            let v = unsafe { *(ptr as *const i64) };
            shm::shfree(ptr).unwrap();
            v
        };
        assert_eq!(read(0), 1);

        let status = run_in_forked_child(|| {
            let h = HANDLE.load(std::sync::atomic::Ordering::SeqCst);
            let ptr = ifile_bracket_index(h, 3).unwrap();
            shm::shfree(ptr).unwrap();
        });
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0,
                "child failed: status {status}");

        let ptr = ifile_bracket_index(f, 1)
            .unwrap_or_else(|e| panic!("parent's cached read after the child ran: {e:?}"));
        let v = unsafe { *(ptr as *const i64) };
        shm::shfree(ptr).unwrap();
        assert_eq!(v, 2);
        shared_close_handle(f).unwrap();
    }

    #[test]
    fn wanting_a_sweeper_starts_no_thread_and_a_childs_first_request_starts_one() {
        let _shm = crate::own_test_registry();
        let ok = crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            sweeper_shutdown();
            sweeper_want();
            let idle = !sweeper_running() && crate::fork_policy::thread_count() == Some(1);
            idle && crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
                sweeper_enqueue_pid(1, u64::MAX);
                sweeper_running()
            })
        });
        assert!(ok, "wanting a sweeper started a thread, or a child dropped its request");
    }

    #[test]
    fn a_forked_child_starts_its_own_sweeper_when_its_parent_had_one() {
        let _shm = crate::own_test_registry();
        sweeper_init();
        let ok = crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            sweeper_enqueue_pid(1, u64::MAX);
            sweeper_running()
        });
        assert!(ok, "a forked child dropped its sweep requests");
    }

    #[test]
    fn a_forked_child_can_shut_down_without_its_parents_sweeper() {
        let _shm = crate::own_test_registry();
        sweeper_init();
        let status = run_in_forked_child(|| {
            sweeper_enqueue_call(u64::MAX);
            sweeper_enqueue_pid(1, u64::MAX);
            sweeper_shutdown();
        });
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0,
                "child shutting down the sweeper did not exit cleanly: status {status}");
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn a_descendant_with_its_ancestors_pid_ignores_the_ancestors_in_use_marks() {
        use std::sync::mpsc::channel;
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("pid_collision");
        let p = dir.join("log.idx").to_str().unwrap().to_string();
        let ran = crate::fork_policy::as_pid_one(move || {
            let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();
            let (inside_tx, inside_rx) = channel::<()>();
            let (done_tx, done_rx) = channel::<()>();
            let first = std::thread::spawn(move || {
                with_process_local_slot(h, |_, _| {
                    inside_tx.send(()).unwrap();
                    done_rx.recv().unwrap();
                    Ok(())
                })
                .unwrap();
            });
            inside_rx.recv().unwrap();
            let claimed = crate::fork_policy::in_a_descendant_with_the_same_pid(move || {
                with_process_local_slot(h, |_, _| Ok(())).is_ok()
            });
            done_tx.send(()).unwrap();
            first.join().unwrap();
            claimed
        });
        assert_ne!(ran, Some(false), "a descendant sharing its ancestor's pid waited on its ancestor's thread");
    }

    static GAP_SLOT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    static AT_GAP: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);
    static RESUME: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

    fn pause_at_gap(slot: &RegistrySlot) {
        use std::sync::atomic::Ordering;
        if slot as *const RegistrySlot as usize != GAP_SLOT.load(Ordering::SeqCst) {
            return;
        }
        AT_GAP.store(true, Ordering::SeqCst);
        while !RESUME.load(Ordering::SeqCst) {
            std::thread::yield_now();
        }
    }

    fn arm_gap(h: i64) {
        use std::sync::atomic::Ordering;
        GAP_SLOT.store(slot_ref(unpack_handle(h).1).unwrap() as *const RegistrySlot as usize, Ordering::SeqCst);
        AT_GAP.store(false, Ordering::SeqCst);
        RESUME.store(false, Ordering::SeqCst);
    }

    fn wait_at_gap() {
        while !AT_GAP.load(std::sync::atomic::Ordering::SeqCst) {
            std::thread::yield_now();
        }
    }

    #[test]
    fn a_read_during_a_release_never_accepts_the_cleared_slot() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("release_gap");
        let p = dir.join("r.idx").to_str().unwrap().to_string();
        let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();
        let before = shared_handle_kind(h).unwrap();
        arm_gap(h);
        *RELEASE_GAP_HOOK.lock().unwrap() = Some(pause_at_gap);
        let closer = std::thread::spawn(move || shared_close_handle(h));
        wait_at_gap();
        let during = shared_handle_kind(h);
        RESUME.store(true, std::sync::atomic::Ordering::SeqCst);
        let _ = closer.join().unwrap();
        *RELEASE_GAP_HOOK.lock().unwrap() = None;
        assert!(during.is_err(), "a read during the release accepted {during:?} (the slot held {before} before)");
    }

    #[test]
    fn a_read_overlapping_a_release_reports_the_handle_stale() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("read_gap");
        let p = dir.join("r.idx").to_str().unwrap().to_string();
        let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();
        arm_gap(h);
        *READ_GAP_HOOK.lock().unwrap() = Some(pause_at_gap);
        let reader = std::thread::spawn(move || shared_handle_kind(h));
        wait_at_gap();
        *READ_GAP_HOOK.lock().unwrap() = None;
        shared_close_handle(h).unwrap();
        RESUME.store(true, std::sync::atomic::Ordering::SeqCst);
        let read = reader.join().unwrap();
        assert!(read.is_err(), "a read that overlapped a release accepted {read:?}");
    }

    #[test]
    fn a_stream_used_by_two_threads_stays_locked() {
        use std::sync::mpsc::channel;
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("two_threads");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();

        // One thread is inside an operation on the stream when a second
        // starts one. However the two finish, this process must keep the
        // descriptor that holds the lock while the stream is open.
        let (inside_tx, inside_rx) = channel::<()>();
        let (trying_tx, trying_rx) = channel::<()>();
        let (done_tx, done_rx) = channel::<()>();
        let first = std::thread::spawn(move || {
            with_process_local_slot(h, |_, _| {
                inside_tx.send(()).unwrap();
                trying_rx.recv().unwrap();
                std::thread::sleep(std::time::Duration::from_millis(100));
                Ok(())
            })
            .unwrap();
            done_tx.send(()).unwrap();
        });
        inside_rx.recv().unwrap();
        trying_tx.send(()).unwrap();
        with_process_local_slot(h, |_, _| {
            done_rx
                .recv_timeout(std::time::Duration::from_secs(10))
                .expect("the first operation never finished");
            Ok(())
        })
        .unwrap();
        first.join().unwrap();

        let c_path = std::ffi::CString::new(p.as_str()).unwrap();
        let fd = unsafe { libc::open(c_path.as_ptr(), libc::O_RDWR | libc::O_CLOEXEC) };
        assert!(fd >= 0);
        let rc = unsafe { libc::flock(fd, libc::LOCK_EX | libc::LOCK_NB) };
        unsafe { libc::close(fd); }
        assert_ne!(rc, 0, "an open stream's path could be locked by another writer");
        shared_close_handle(h).unwrap();
    }

    #[test]
    fn threads_read_one_input_file_at_once() {
        use std::sync::mpsc::channel;
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("parallel_read");
        let path = dir.join("in.idx");
        write_int_stream(&path, &[&[1, 2], &[3, 4]]);
        let h = open_ifile(path.to_str().unwrap()).unwrap();

        // Partitioning an input across threads reads one handle from all
        // of them; each read must not wait for another to finish.
        let (inside_tx, inside_rx) = channel::<()>();
        let (second_tx, second_rx) = channel::<()>();
        let first = std::thread::spawn(move || {
            with_process_local_slot(h, |_, _| {
                inside_tx.send(()).unwrap();
                second_rx
                    .recv_timeout(std::time::Duration::from_secs(10))
                    .map_err(|_| MorlocError::Other("reads of one file were serialized".into()))
            })
        });
        inside_rx.recv().unwrap();
        with_process_local_slot(h, |_, _| {
            second_tx.send(()).unwrap();
            Ok(())
        })
        .unwrap();
        first.join().unwrap().unwrap();
        shared_close_handle(h).unwrap();
    }

    /// Run `act` in a child process, which then reports one i64 and waits
    /// until released. Returns the child's pid, the value, and the pipe end
    /// that releases it.
    fn child_reporting(act: impl FnOnce() -> i64) -> (libc::pid_t, i64, libc::c_int) {
        let mut report = [0 as libc::c_int; 2];
        let mut release = [0 as libc::c_int; 2];
        unsafe {
            assert_eq!(morloc_runtime_types::fd::pipe(report.as_mut_ptr()), 0);
            assert_eq!(morloc_runtime_types::fd::pipe(release.as_mut_ptr()), 0);
        }
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            unsafe {
                libc::close(report[0]);
                libc::close(release[1]);
            }
            let v = act().to_le_bytes();
            unsafe {
                libc::write(report[1], v.as_ptr() as *const libc::c_void, 8);
                let mut b = 0u8;
                libc::read(release[0], &mut b as *mut u8 as *mut libc::c_void, 1);
                libc::_exit(0);
            }
        }
        let mut v = [0u8; 8];
        unsafe {
            libc::close(report[1]);
            libc::close(release[0]);
            assert_eq!(libc::read(report[0], v.as_mut_ptr() as *mut libc::c_void, 8), 8);
            libc::close(report[0]);
        }
        (pid, i64::from_le_bytes(v), release[1])
    }

    #[test]
    fn a_stream_closed_by_another_process_frees_its_path() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("cross_close");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();

        // A handle opened in one pool may be closed in another. The opener
        // is then idle; the path must still be free once the stream ends.
        let q = p.clone();
        let (pid, h, release) = child_reporting(move || {
            shared_open_ostream_with_schema(&q, "ai4").unwrap()
        });
        let closed = shared_close_handle(h);
        let began = std::time::Instant::now();
        let again = shared_append_to_path(&p, "ai4");
        let waited = began.elapsed();
        release_forked_holder((pid, release));
        closed.unwrap();
        // The opener is woken, not polled: well under its fallback period.
        assert!(waited < std::time::Duration::from_millis(500), "reopen took {waited:?}");
        let h2 = again.unwrap_or_else(|e| {
            panic!("append after another process closed the stream: {e:?}")
        });
        shared_close_handle(h2).unwrap();
    }

    /// Open an OStream on `path` in a child process, which then stops, so
    /// it can neither write nor release anything. Returns its pid, the
    /// handle, and the pipe end that releases it once continued.
    fn stopped_opener(path: &str) -> (libc::pid_t, i64, libc::c_int) {
        let q = path.to_string();
        let (pid, h, release) = child_reporting(move || {
            shared_open_ostream_with_schema(&q, "ai4").unwrap()
        });
        unsafe { libc::kill(pid, libc::SIGSTOP); }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, libc::WUNTRACED); }
        (pid, h, release)
    }

    #[test]
    fn a_stream_ended_elsewhere_refuses_writes_and_stays_valid() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("ended_writes");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        let (pid, h, release) = stopped_opener(&p);

        // Ending the stream must kill its handle at once, even while the
        // opener still holds the file: a write after the footer would
        // report success and be missing from the index.
        shared_close_handle(h).unwrap();
        let list = parse_schema("ai4").unwrap();
        let v = crate::json::read_json_with_schema("[1,2]", &list).unwrap();
        let wrote = shared_write_subpacket(h, crate::compression::CompressionLevel::from_u8(0).unwrap(), v);
        shm::shfree(v).unwrap();
        assert!(wrote.is_err(), "a write after the stream ended succeeded");
        let (mp, sz) = mmap_file_readonly(&p).unwrap();
        let parsed = parse_stream_file(&p, mp, sz);
        unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize); }
        assert_eq!(parsed.unwrap().element_count, 0);

        unsafe { libc::kill(pid, libc::SIGCONT); }
        release_forked_holder((pid, release));
    }

    #[test]
    fn a_stream_left_for_an_opener_that_died_is_reclaimed() {
        use std::sync::atomic::Ordering;
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("ended_dead_opener");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        let (pid, h, release) = stopped_opener(&p);
        let start = morloc_runtime_types::process::start_time(pid as u32);
        shared_close_handle(h).unwrap();
        unsafe { libc::kill(pid, libc::SIGKILL); }
        release_forked_holder((pid, release));

        // The kernel dropped the dead opener's lock, so the path is free; its
        // slot, left for it to release, is reclaimed by the crash sweep.
        let h2 = shared_append_to_path(&p, "ai4").expect("append after the opener died");
        shared_close_handle(h2).unwrap();
        within_seconds("the crash sweep", move || sweep_per_pid(pid as u32, start));
        let (_, idx) = unpack_handle(h);
        assert_eq!(slot_ref(idx).unwrap().state.load(Ordering::Acquire), SLOT_STATE_FREE);
    }

    #[test]
    fn an_opener_reopens_a_stream_another_process_ended() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("opener_reopens");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();

        // A pool hands its stream to another, which closes it; the pool
        // then appends to the same file in the same call.
        let (pid, _, release) = child_reporting(move || {
            shared_close_handle(h).unwrap();
            0
        });
        release_forked_holder((pid, release));
        let h2 = shared_append_to_path(&p, "ai4").expect("append after another process closed");
        shared_close_handle(h2).unwrap();
    }

    #[test]
    fn a_child_of_a_dead_opener_does_not_hold_its_file() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("orphan_holder");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();

        // The opener forks a long-lived child (user code's worker pool)
        // and then dies. The stream's lock must not outlive the opener in
        // the child, which never asked for it.
        let mut report = [0 as libc::c_int; 2];
        unsafe { assert_eq!(morloc_runtime_types::fd::pipe(report.as_mut_ptr()), 0) };
        let opener = unsafe { libc::fork() };
        assert!(opener >= 0);
        if opener == 0 {
            let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();
            let worker = unsafe { libc::fork() };
            if worker == 0 {
                unsafe {
                    libc::sleep(30);
                    libc::_exit(0);
                }
            }
            let mut msg = [0u8; 16];
            msg[..8].copy_from_slice(&h.to_le_bytes());
            msg[8..].copy_from_slice(&(worker as i64).to_le_bytes());
            unsafe {
                libc::write(report[1], msg.as_ptr() as *const libc::c_void, 16);
                loop {
                    libc::pause();
                }
            }
        }
        let mut msg = [0u8; 16];
        unsafe {
            libc::close(report[1]);
            assert_eq!(libc::read(report[0], msg.as_mut_ptr() as *mut libc::c_void, 16), 16);
        }
        let h = i64::from_le_bytes(msg[..8].try_into().unwrap());
        let worker = i64::from_le_bytes(msg[8..].try_into().unwrap()) as libc::pid_t;
        unsafe {
            libc::kill(opener, libc::SIGKILL);
            let mut status = 0;
            libc::waitpid(opener, &mut status, 0);
        }
        let closed = shared_close_handle(h);
        let again = shared_append_to_path(&p, "ai4");
        unsafe { libc::kill(worker, libc::SIGKILL); }
        closed.unwrap();
        let h2 = again.expect("append while the dead opener's child lives");
        shared_close_handle(h2).unwrap();
    }

    #[test]
    fn a_stream_whose_opener_awaits_reaping_is_released_at_once() {
        use std::sync::atomic::Ordering;
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("zombie_opener");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        let q = p.clone();
        let (pid, h, release) = child_reporting(move || {
            shared_open_ostream_with_schema(&q, "ai4").unwrap()
        });

        // Dead but not yet reaped: nothing can release a slot left for it.
        unsafe {
            libc::kill(pid, libc::SIGKILL);
            let mut info: libc::siginfo_t = std::mem::zeroed();
            libc::waitid(libc::P_PID, pid as libc::id_t, &mut info, libc::WEXITED | libc::WNOWAIT);
        }
        shared_close_handle(h).unwrap();
        let (_, idx) = unpack_handle(h);
        let state = slot_ref(idx).unwrap().state.load(Ordering::Acquire);
        release_forked_holder((pid, release));
        assert_eq!(state, SLOT_STATE_FREE, "a slot was left for an opener that had exited");
    }

    #[test]
    fn a_replaced_input_file_is_not_read_through_its_old_index() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("replaced_input");
        let path = dir.join("in.idx");
        let p = path.to_str().unwrap().to_string();
        write_int_stream(&path, &[&[1, 2], &[3, 4]]);
        let q = p.clone();
        let (pid, h, release) = child_reporting(move || open_ifile(&q).unwrap());

        // Another file renamed over the path after the open: a pool that
        // joins the stream now must not read it with the original's index.
        let other = dir.join("other.idx");
        write_int_stream(&other, &[&[9, 9, 9, 9, 9, 9, 9]]);
        std::fs::rename(&other, &path).unwrap();
        let read = handle_length(h).and_then(|_| shared_stream_layout(h).map(|_| ()));
        release_forked_holder((pid, release));
        let e = read.expect_err("a replaced file was read through the original's index");
        assert!(format!("{e:?}").contains("replaced"), "unexpected error: {e:?}");
    }

    #[test]
    fn a_stream_of_a_replaced_input_file_is_refused() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("replaced_derive");
        let path = dir.join("in.idx");
        write_int_stream(&path, &[&[1, 2]]);
        let h = open_ifile(path.to_str().unwrap()).unwrap();
        let other = dir.join("other.idx");
        write_int_stream(&other, &[&[7]]);
        std::fs::rename(&other, &path).unwrap();
        let e = shared_derive_istream(h).expect_err("@stream read a file that replaced its input");
        assert!(format!("{e:?}").contains("replaced"), "unexpected error: {e:?}");
        shared_close_handle(h).unwrap();
    }

    #[test]
    fn a_file_being_written_as_a_stream_is_not_replaced() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("replace_live");
        let src = dir.join("src.idx");
        write_int_stream(&src, &[&[1, 2]]);
        let dest = dir.join("dest.idx");
        let d = dest.to_str().unwrap().to_string();
        let h = shared_open_ostream_with_schema(&d, "ai4").unwrap();

        // Renaming another file over a stream's path would leave its
        // writer writing into a file nobody can reach.
        let merged = concat_files(&[src.to_str().unwrap()], &d);
        let written = crate::utility::write_atomic_path(&dest, b"replacement");
        assert!(merged.is_err(), "@concat replaced a file a stream was writing");
        assert!(written.is_err(), "an atomic write replaced a file a stream was writing");
        shared_close_handle(h).unwrap();
        assert_eq!(stream_len(&dest), 0, "the stream's file was replaced");

        // Once the stream is closed the path is an ordinary file again.
        concat_files(&[src.to_str().unwrap()], &d).unwrap();
        assert_eq!(stream_len(&dest), 2);
    }

    #[test]
    fn a_replaced_output_file_is_not_written() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("replaced_output");
        let path = dir.join("out.idx");
        let p = path.to_str().unwrap().to_string();
        let q = p.clone();
        let (pid, h, release) = child_reporting(move || {
            shared_open_ostream_with_schema(&q, "ai4").unwrap()
        });

        // SLOT-10: the stream is written to the file it opened, not to
        // whatever now has its name.
        let other = dir.join("other.bin");
        std::fs::write(&other, b"not a stream").unwrap();
        std::fs::rename(&other, &path).unwrap();
        let list = parse_schema("ai4").unwrap();
        let v = crate::json::read_json_with_schema("[1,2]", &list).unwrap();
        let level = crate::compression::CompressionLevel::from_u8(0).unwrap();
        let wrote = shared_write_subpacket(h, level, v).and_then(|_| shared_flush_buffer(h));
        shm::shfree(v).unwrap();
        let after = std::fs::read(&path).unwrap();
        release_forked_holder((pid, release));
        wrote.unwrap();
        shared_close_handle(h).unwrap();
        assert_eq!(after, b"not a stream", "the replacing file was written");
        assert_eq!(std::fs::read(&path).unwrap(), b"not a stream", "closing wrote the replacing file");
    }

    fn staging_files(dir: &std::path::Path) -> Vec<String> {
        std::fs::read_dir(dir).unwrap()
            .filter_map(|e| e.ok().map(|e| e.file_name().to_string_lossy().into_owned()))
            .filter(|n| n.contains(".tmp."))
            .collect()
    }

    #[test]
    fn replacements_of_one_path_take_turns() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("replace_race");
        let dest = dir.join("v.dat");
        std::fs::write(&dest, b"first").unwrap();
        // Another replacement is between taking the file's lock and renaming
        // its copy over it, as concurrent stores of one cache entry are. This
        // one must wait its turn: renaming now would displace a file it has
        // not locked, which a stream may have just opened.
        let other = crate::utility::ReplaceGuard::take(&dest).unwrap();
        let (tx, rx) = std::sync::mpsc::channel();
        let target = dest.clone();
        std::thread::spawn(move || {
            let _ = tx.send(crate::utility::write_atomic_path(&target, b"second"));
        });
        std::thread::sleep(std::time::Duration::from_millis(100));
        assert!(rx.try_recv().is_err(), "a replacement renamed over a file another held");
        drop(other);
        rx.recv_timeout(std::time::Duration::from_secs(5))
            .expect("the waiting replacement never finished")
            .expect("a replacement was refused because another was in flight");
        assert_eq!(std::fs::read(&dest).unwrap(), b"second");
        assert!(staging_files(&dir).is_empty(), "staging files left: {:?}", staging_files(&dir));
    }

    #[test]
    fn a_replacement_refused_by_a_stream_leaves_no_staging_file() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("replace_refused");
        let dest = dir.join("v.dat");
        std::fs::write(&dest, b"streaming").unwrap();
        let c_dest = std::ffi::CString::new(dest.to_str().unwrap()).unwrap();
        let fd = unsafe { libc::open(c_dest.as_ptr(), libc::O_RDWR | libc::O_CLOEXEC) };
        assert!(fd >= 0);
        lock_stream_file(fd).expect("the stream writer's lock");
        let refused = crate::utility::write_atomic_path(&dest, b"replacement");
        unlock_and_close(fd);
        let e = refused.expect_err("a file a stream is writing was replaced");
        assert!(e.to_string().contains("stream"), "unexpected error: {e}");
        assert_eq!(std::fs::read(&dest).unwrap(), b"streaming");
        assert!(staging_files(&dir).is_empty(), "staging files left: {:?}", staging_files(&dir));
    }

    #[cfg(target_os = "linux")]
    #[test]
    fn a_descendant_with_its_ancestors_pid_opens_its_own_nexus_connection() {
        use std::io::{Read, Write};
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("stdio_sock_pid");
        let sock = dir.join("nexus.sock");
        let ran = crate::fork_policy::as_pid_one(move || {
            let listener = std::os::unix::net::UnixListener::bind(&sock).unwrap();
            crate::fork_policy::set_test_env("MORLOC_NEXUS_STDIO_SOCK", Some(sock.as_os_str()));
            with_stdio_sock(|_| Ok(())).unwrap();
            let (_ancestor_conn, _) = listener.accept().unwrap();
            let wrote = crate::fork_policy::in_a_descendant_with_the_same_pid(|| {
                with_stdio_sock(|s| s.write_all(b"d").map_err(MorlocError::Io)).is_ok()
            });
            listener.set_nonblocking(true).unwrap();
            let fresh = match listener.accept() {
                Ok((mut conn, _)) => {
                    conn.set_nonblocking(false).unwrap();
                    let mut b = [0u8; 1];
                    conn.read_exact(&mut b).is_ok() && &b == b"d"
                }
                Err(_) => false,
            };
            wrote && fresh
        });
        assert_ne!(ran, Some(false), "a descendant sharing its ancestor's pid used its ancestor's connection");
    }

    #[test]
    fn the_nexus_connection_works_from_a_thread_local_destructor() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("stdio_sock_dtor");
        let sock = dir.join("nexus.sock");
        let listener = std::os::unix::net::UnixListener::bind(&sock).unwrap();
        let ok = crate::fork_policy::exits_cleanly_in_a_forked_child(move || {
            struct UsesTheNexus;
            impl Drop for UsesTheNexus {
                fn drop(&mut self) {
                    let _ = with_stdio_sock(|_| Ok(()));
                }
            }
            thread_local! {
                static LATE: UsesTheNexus = const { UsesTheNexus };
            }
            crate::fork_policy::set_test_env("MORLOC_NEXUS_STDIO_SOCK", Some(sock.as_os_str()));
            std::thread::spawn(|| {
                LATE.with(|_| {});
                with_stdio_sock(|_| Ok(())).unwrap();
            })
            .join()
            .is_ok()
        });
        drop(listener);
        assert!(ok, "using the nexus connection from a thread-local destructor aborted");
    }

    #[test]
    fn a_forked_child_does_not_share_its_parents_nexus_connection() {
        use std::io::{Read, Write};
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("stdio_sock");
        let sock = dir.join("nexus.sock");
        let ok = crate::fork_policy::exits_cleanly_in_a_forked_child(move || {
            let listener = std::os::unix::net::UnixListener::bind(&sock).unwrap();
            crate::fork_policy::set_test_env("MORLOC_NEXUS_STDIO_SOCK", Some(sock.as_os_str()));
            with_stdio_sock(|_| Ok(())).unwrap();
            let (_parent_conn, _) = listener.accept().unwrap();

            // Requests and replies on one socket from two processes interleave:
            // a child must open its own connection. The child names itself on
            // whatever connection it uses, so its pid arrives on a new one only
            // if it opened one.
            let pid = unsafe { libc::fork() };
            if pid == 0 {
                let me = unsafe { libc::getpid() };
                let ok = with_stdio_sock(|s| {
                    s.write_all(&me.to_le_bytes()).map_err(MorlocError::Io)
                }).is_ok();
                unsafe { libc::_exit(if ok { 0 } else { 2 }) };
            }
            listener.set_nonblocking(true).unwrap();
            let began = std::time::Instant::now();
            let peer = loop {
                match listener.accept() {
                    Ok((mut conn, _)) => {
                        let mut buf = [0u8; std::mem::size_of::<libc::pid_t>()];
                        let mut got = 0;
                        while got < buf.len() && began.elapsed() < std::time::Duration::from_secs(3) {
                            match conn.read(&mut buf[got..]) {
                                Ok(0) => break,
                                Ok(n) => got += n,
                                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                                    std::thread::sleep(std::time::Duration::from_millis(5));
                                }
                                Err(_) => break,
                            }
                        }
                        break (got == buf.len()).then(|| libc::pid_t::from_le_bytes(buf));
                    }
                    Err(_) if began.elapsed() < std::time::Duration::from_secs(3) => {
                        std::thread::sleep(std::time::Duration::from_millis(5));
                    }
                    Err(_) => break None,
                }
            };
            let mut status = 0;
            unsafe { libc::waitpid(pid, &mut status, 0) };
            peer == Some(pid)
        });
        assert!(ok, "the child reused its parent's connection");
    }

    /// Run `act` in a forked child and return its exit code, or the signal
    /// that killed it as a negative number, so a SIGBUS fails the test
    /// instead of the test process.
    fn in_child(act: impl FnOnce() -> bool) -> i32 {
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            let ok = std::panic::catch_unwind(std::panic::AssertUnwindSafe(act)).unwrap_or(false);
            unsafe { libc::_exit(if ok { 0 } else { 1 }) };
        }
        let mut status = 0;
        unsafe { libc::waitpid(pid, &mut status, 0) };
        if libc::WIFSIGNALED(status) { -libc::WTERMSIG(status) } else { libc::WEXITSTATUS(status) }
    }

    fn drain_frames(h: i64) -> Result<usize, MorlocError> {
        let mut n = 0;
        while let Some(p) = shared_next_frame(h)? {
            shm::shfree(p)?;
            n += 1;
        }
        Ok(n)
    }

    fn append_one(path: &str) {
        let h = shared_append_to_path(path, "ai8").unwrap();
        let list = parse_schema("ai8").unwrap();
        let v = crate::json::read_json_with_schema("[7, 7]", &list).unwrap();
        let level = crate::compression::CompressionLevel::from_u8(0).unwrap();
        shared_write_subpacket(h, level, v).unwrap();
        shm::shfree(v).unwrap();
        shared_close_handle(h).unwrap();
    }

    #[test]
    fn a_stream_being_read_survives_an_append() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("read_append");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        write_int_stream(&path, &[&[1, 2], &[3, 4]]);

        // An append cuts the footer a reader stops at and writes over it;
        // the reader must see the stream as it was when it opened it.
        let code = in_child(move || {
            let h = open_istream(&p).unwrap();
            append_one(&p);
            matches!(drain_frames(h), Ok(2))
        });
        assert_eq!(code, 0, "a reader of an appended stream failed (negative: a signal)");
        assert_eq!(stream_len(&path), 6);
    }

    #[test]
    fn a_reader_stops_where_a_live_writer_was_when_it_opened() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("read_live");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        let w = shared_open_ostream_with_schema(&p, "ai8").unwrap();
        append_ints8(w, "[1, 2]");
        shared_flush_buffer(w).unwrap();

        // The writer's next sub-packet lands over the temp footer a reader
        // opened against; the reader must not read the head of one and the
        // half-written body of another.
        let r = open_istream(&p).unwrap();
        append_ints8(w, "[3]");
        shared_flush_buffer(w).unwrap();
        let read = drain_frames(r);
        shared_close_handle(w).unwrap();
        assert_eq!(read.unwrap(), 1);
    }

    fn append_ints8(h: i64, json: &str) {
        let list = parse_schema("ai8").unwrap();
        let v = crate::json::read_json_with_schema(json, &list).unwrap();
        let level = crate::compression::CompressionLevel::from_u8(0).unwrap();
        shared_write_subpacket(h, level, v).unwrap();
        shm::shfree(v).unwrap();
    }

    #[test]
    fn appending_to_an_unclosed_stream_keeps_what_it_holds() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("append_unclosed");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();

        // A writer that ended without @close leaves its sub-packets behind a
        // temp footer; resuming must keep them.
        let w = shared_open_ostream_with_schema(&p, "ai8").unwrap();
        append_ints8(w, "[1, 2]");
        shared_flush_buffer(w).unwrap();
        append_ints8(w, "[3]");
        shared_flush_buffer(w).unwrap();
        shared_discard_handle(w).unwrap();
        let a = shared_append_to_path(&p, "ai8").unwrap();
        append_ints8(a, "[4, 5]");
        shared_close_handle(a).unwrap();
        let f = open_ifile(&p).unwrap();
        let layout = shared_stream_layout(f).unwrap();
        shared_close_handle(f).unwrap();
        let held: Vec<u64> = layout.iter().map(|(_, n, _)| *n).collect();
        assert_eq!(held, vec![2, 1, 2], "the sub-packets written before the append were lost");
    }

    #[test]
    fn appending_to_an_unclosed_compressed_stream_keeps_its_counts() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("append_unclosed_z");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        crate::write_behind::set_test_depth(Some(0));
        let w = shared_open_ostream_with_schema(&p, "ai8").unwrap();
        let list = parse_schema("ai8").unwrap();
        let level = crate::compression::CompressionLevel::from_u8(3).unwrap();
        for json in ["[1, 2, 3]", "[4]"] {
            let v = crate::json::read_json_with_schema(json, &list).unwrap();
            shared_write_subpacket(w, level, v).unwrap();
            shm::shfree(v).unwrap();
            shared_flush_buffer(w).unwrap();
        }
        shared_discard_handle(w).unwrap();
        crate::write_behind::set_test_depth(None);
        let a = shared_append_to_path(&p, "ai8").unwrap();
        append_ints8(a, "[5, 6]");
        shared_close_handle(a).unwrap();
        let f = open_ifile(&p).unwrap();
        let layout = shared_stream_layout(f).unwrap();
        shared_close_handle(f).unwrap();
        let held: Vec<u64> = layout.iter().map(|(_, n, _)| *n).collect();
        assert_eq!(held, vec![3, 1, 2]);
    }

    #[test]
    fn a_saved_value_is_not_appended_to() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("append_data_packet");
        let path = dir.join("saved.dat");
        let p = path.to_str().unwrap().to_string();
        let bytes = build_int_voidstar_subpacket(&[1, 2]);
        std::fs::write(&path, &bytes).unwrap();

        // A file holding one value has no stream to resume: appending would
        // leave elements after it that no reader sees.
        let e = shared_append_to_path(&p, "ai8").expect_err("@append resumed a saved value");
        assert!(format!("{e:?}").contains("single value"), "unexpected error: {e:?}");
        assert_eq!(std::fs::read(&path).unwrap(), bytes);
    }

    #[test]
    fn readers_keep_the_file_they_opened_when_it_is_rewritten() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("read_rewrite");
        let path = dir.join("data.idx");
        let p = path.to_str().unwrap().to_string();
        write_int_stream(&path, &[&[1, 2], &[3, 4], &[5, 6]]);

        // Rewriting a file being read must not pull its pages from under
        // the readers.
        let code = in_child(move || {
            let file = open_ifile(&p).unwrap();
            let stream = open_istream(&p).unwrap();
            let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();
            shared_close_handle(h).unwrap();
            handle_length(file).ok() == Some(6) && matches!(drain_frames(stream), Ok(3))
        });
        assert_eq!(code, 0, "a reader of a rewritten file failed (negative: a signal)");
        assert_eq!(stream_len(&path), 0, "the rewrite is not visible at the path");
    }

    #[test]
    fn rewriting_through_a_symlink_keeps_the_link_and_the_mode() {
        use std::os::unix::fs::PermissionsExt;
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("rewrite_link");
        let target = dir.join("target.idx");
        let link = dir.join("link.idx");
        write_int_stream(&target, &[&[1, 2]]);
        std::fs::set_permissions(&target, std::fs::Permissions::from_mode(0o640)).unwrap();
        std::os::unix::fs::symlink(&target, &link).unwrap();
        let h = shared_open_ostream_with_schema(link.to_str().unwrap(), "ai4").unwrap();
        shared_close_handle(h).unwrap();
        assert!(std::fs::symlink_metadata(&link).unwrap().file_type().is_symlink());
        assert_eq!(stream_len(&target), 0);
        let mode = std::fs::metadata(&target).unwrap().permissions().mode() & 0o777;
        assert_eq!(mode, 0o640);
    }

    #[test]
    fn a_file_that_cannot_be_replaced_is_rewritten_in_place_unless_read() {
        use std::os::unix::fs::PermissionsExt;
        if unsafe { libc::geteuid() } == 0 {
            return; // root writes any directory, so the rename never fails
        }
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("rewrite_readonly_dir");
        let path = dir.join("out.idx");
        let p = path.to_str().unwrap().to_string();
        write_int_stream(&path, &[&[1, 2]]);
        std::fs::set_permissions(&dir, std::fs::Permissions::from_mode(0o555)).unwrap();

        // A directory this process may not write cannot take a new file;
        // the old one is rewritten where it is, but not while it is read.
        let reader = open_ifile(&p).unwrap();
        let refused = shared_open_ostream_with_schema(&p, "ai8");
        shared_close_handle(reader).unwrap();
        let rewritten = shared_open_ostream_with_schema(&p, "ai8");
        std::fs::set_permissions(&dir, std::fs::Permissions::from_mode(0o755)).unwrap();
        assert!(refused.is_err(), "a file being read was rewritten in place");
        shared_close_handle(rewritten.expect("a file nobody reads was not rewritten")).unwrap();
        assert_eq!(stream_len(&path), 0);
    }

    #[test]
    fn an_empty_file_is_written_in_place() {
        use std::os::unix::fs::MetadataExt;
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("empty_in_place");
        let path = dir.join("out.idx");
        std::fs::write(&path, b"").unwrap();
        let ino = std::fs::metadata(&path).unwrap().ino();
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "ai4").unwrap();
        shared_close_handle(h).unwrap();
        assert_eq!(std::fs::metadata(&path).unwrap().ino(), ino);
    }

    #[test]
    fn a_file_renamed_in_before_the_lock_is_the_one_written() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("rename_before_lock");
        let path = dir.join("out.idx");
        let p = path.to_str().unwrap().to_string();
        std::fs::write(&path, b"").unwrap();

        // A rename landing between the open and the lock would leave the
        // writer writing a file the path no longer names.
        let other = dir.join("other.bin");
        let (o, q) = (other.clone(), path.clone());
        *BEFORE_STREAM_LOCK.lock().unwrap() = Some((p.clone(), Box::new(move || {
            std::fs::write(&o, b"").unwrap();
            std::fs::rename(&o, &q).unwrap();
        })));
        let h = shared_open_ostream_with_schema(&p, "ai4");
        *BEFORE_STREAM_LOCK.lock().unwrap() = None;
        let h = h.unwrap();
        append_ints(h, "[1,2]");
        shared_close_handle(h).unwrap();
        assert_eq!(stream_len(&path), 2, "the stream was written to a file the path no longer names");
    }

    fn append_ints(h: i64, json: &str) {
        let list = parse_schema("ai4").unwrap();
        let v = crate::json::read_json_with_schema(json, &list).unwrap();
        let level = crate::compression::CompressionLevel::from_u8(0).unwrap();
        shared_write_subpacket(h, level, v).unwrap();
        shm::shfree(v).unwrap();
    }

    /// Kill a child process while it holds the slot lock of `h`.
    fn die_inside(h: i64) {
        let (pid, _, release) = child_reporting(move || {
            let (_, idx) = unpack_handle(h);
            std::mem::forget(SlotGuard::lock_any(slot_ref(idx).unwrap()).unwrap());
            0
        });
        unsafe { libc::kill(pid, libc::SIGKILL); }
        release_forked_holder((pid, release));
    }

    /// Run `f` on another thread, failing the test if it has not returned
    /// within ten seconds.
    fn within_seconds<T: Send + 'static>(what: &str, f: impl FnOnce() -> T + Send + 'static) -> T {
        let (tx, rx) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            let _ = tx.send(f());
        });
        rx.recv_timeout(std::time::Duration::from_secs(10))
            .unwrap_or_else(|_| panic!("{what} never returned"))
    }

    #[test]
    fn a_process_dying_inside_a_stream_does_not_hang_the_rest() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("dead_holder");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();

        // A pool killed while it holds a stream's slot (a signal, the OOM
        // killer) must not leave every other process spinning on it.
        die_inside(h);
        let closed = within_seconds("closing a stream whose holder died", move || {
            shared_close_handle(h)
        });
        let e = closed.expect_err("a stream a process died inside closed cleanly");
        assert!(format!("{e:?}").contains("died"), "unexpected error: {e:?}");

        // The failed close still ends the stream: the handle is gone, and
        // the path and the slot can be used again.
        assert!(shared_close_handle(h).is_err());
        let h2 = shared_append_to_path(&p, "ai4").expect("append after the failed close");
        shared_close_handle(h2).unwrap();
    }

    #[test]
    fn readers_of_a_stream_a_process_died_inside_are_refused() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("dead_reader");
        let path = dir.join("in.idx");
        write_int_stream(&path, &[&[1, 2], &[3, 4]]);
        let h = open_istream(path.to_str().unwrap()).unwrap();

        // A reader that died inside the lock may have left the cursor
        // half-advanced; the stream is not read past that.
        die_inside(h);
        let next = within_seconds("reading a stream whose holder died", move || {
            shared_next_subpacket(h).map(|_| ())
        });
        assert!(next.is_err(), "a stream a process died inside was read");
        assert!(shared_close_handle(h).is_err());
        let h2 = open_istream(path.to_str().unwrap()).unwrap();
        shared_close_handle(h2).unwrap();
    }

    #[test]
    fn the_crash_sweep_reclaims_a_stream_its_opener_died_inside() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("dead_opener");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();

        let q = p.clone();
        let (pid, h, release) = child_reporting(move || {
            let h = shared_open_ostream_with_schema(&q, "ai4").unwrap();
            let (_, idx) = unpack_handle(h);
            std::mem::forget(SlotGuard::lock_any(slot_ref(idx).unwrap()).unwrap());
            h
        });
        let start = morloc_runtime_types::process::start_time(pid as u32);
        unsafe { libc::kill(pid, libc::SIGKILL); }
        release_forked_holder((pid, release));

        within_seconds("the crash sweep", move || sweep_per_pid(pid as u32, start));
        let (gen, idx) = unpack_handle(h);
        assert!(!slot_generation_is(slot_ref(idx).unwrap(), gen), "the sweep left the slot open");
        assert!(shared_close_handle(h).is_err(), "the swept stream is still open");
        let h2 = shared_append_to_path(&p, "ai4").expect("append after the sweep");
        shared_close_handle(h2).unwrap();
    }

    #[test]
    fn the_crash_sweep_reclaims_a_slot_its_opener_died_in_before_publishing() {
        use std::sync::atomic::Ordering;
        let _shm = crate::own_test_registry();
        registry_init().unwrap();

        // An open that dies between taking a slot and publishing its stream
        // must not leave the slot taken for the rest of the run.
        let (pid, idx, release) = child_reporting(|| {
            let (idx, _, guard) = allocate_slot_cas().unwrap();
            std::mem::forget(guard);
            idx as i64
        });
        let start = morloc_runtime_types::process::start_time(pid as u32);
        unsafe { libc::kill(pid, libc::SIGKILL); }
        release_forked_holder((pid, release));

        within_seconds("the crash sweep", move || sweep_per_pid(pid as u32, start));
        let slot = slot_ref(idx as usize).unwrap();
        assert_eq!(slot.state.load(Ordering::Acquire), SLOT_STATE_FREE);
    }

    #[test]
    fn a_panic_inside_a_stream_poisons_it() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("panic_inside");
        let path = dir.join("log.idx");
        let p = path.to_str().unwrap().to_string();
        let h = shared_open_ostream_with_schema(&p, "ai4").unwrap();

        // Unwinding releases the lock as a death would, mid-update.
        let (_, idx) = unpack_handle(h);
        let slot = slot_ref(idx).unwrap();
        let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let _g = SlotGuard::lock(slot).unwrap();
            panic!("inside the slot");
        }));
        assert!(caught.is_err());
        let e = shared_close_handle(h).expect_err("a stream that panicked inside closed cleanly");
        assert!(format!("{e:?}").contains("died"), "unexpected error: {e:?}");
        let h2 = shared_append_to_path(&p, "ai4").unwrap();
        shared_close_handle(h2).unwrap();
    }

    #[test]
    fn a_released_slot_clears_only_its_own_stdio_claim() {
        use std::sync::atomic::Ordering;
        let _shm = crate::own_test_registry();
        registry_init().unwrap();
        let claim = stdio_claim_slot(STDIO_KIND_STDIN).expect("registry attached");
        let h1 = open_stdio(MLC_KIND_ISTREAM, STDIO_KIND_STDIN, "").unwrap();

        // A slot of the same kind that does not hold the claim, as an open
        // that lost the race to claim it leaves behind.
        let (_, slot, guard) = allocate_slot_cas().unwrap();
        slot.is_stdio.set(1);
        slot.stdio_kind.set(STDIO_KIND_STDIN);
        release_slot_locked(slot);
        drop(guard);
        assert_eq!(claim.load(Ordering::Acquire), h1, "another slot's release cleared the claim");
        close_handle(h1).unwrap();
        assert_eq!(claim.load(Ordering::Acquire), STDIO_UNCLAIMED);
    }

    #[test]
    fn an_appended_file_matches_one_that_was_opened() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!(
            "morloc_append_parity_test_{}", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let schema = "ai4";

        // Creating by appending and creating by opening must agree, or a
        // log's first record would sit in a differently shaped file than
        // every record after it.
        let a = dir.join("via_append.idx");
        let b = dir.join("via_open.idx");
        let (pa, pb) = (a.to_str().unwrap(), b.to_str().unwrap());
        let _ = std::fs::remove_file(pa);
        let _ = std::fs::remove_file(pb);

        // The registry slot must agree too, not just the bytes: a hint-
        // bearing string reaching one path and the canonical form reaching
        // the other would make a handle's reported schema depend on whether
        // the file happened to exist.
        let h = shared_append_to_path(pa, schema).unwrap();
        let via_append = shared_handle_schema_str(h).unwrap();
        shared_close_handle(h).unwrap();
        let g = shared_open_ostream_with_schema(pb, schema).unwrap();
        let via_open = shared_handle_schema_str(g).unwrap();
        shared_close_handle(g).unwrap();
        assert_eq!(via_append, via_open, "handle schema must not depend on the path taken");

        assert_eq!(
            std::fs::read(pa).unwrap(),
            std::fs::read(pb).unwrap(),
            "a file created by appending must match one created by opening",
        );

        let _ = std::fs::remove_file(pa);
        let _ = std::fs::remove_file(pb);
    }

    #[test]
    fn append_accepts_hint_bearing_schema() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!(
            "morloc_append_hint_test_{}", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("hinted.idx");
        let p = path.to_str().unwrap();

        // A two-field record of (Str, Int): the shape `record py => T =
        // "dict"` produces. Stored hint-free, requested with `<dict>`.
        let stored_form = "am24kinds2idj";
        let pool_form = "a<dict>m24kinds2idj";

        let out = shared_open_ostream_with_schema(p, pool_form).unwrap();
        shared_close_handle(out).unwrap();

        // The header keeps the canonical form regardless of what the
        // opener passed.
        let parsed_hdr = {
            let (mp, sz) = mmap_file_readonly(p).unwrap();
            let parsed = parse_stream_file(p, mp, sz).unwrap();
            unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize); }
            parsed.schema_str
        };
        assert_eq!(parsed_hdr, stored_form);

        // Appending with the pool's hint-bearing form must work.
        let app = shared_append_to_path(p, pool_form)
            .expect("append must accept a hint-bearing schema");
        shared_close_handle(app).unwrap();

        // The hint-free form must work too -- a library caller that read
        // the schema back off the file passes this.
        let app2 = shared_append_to_path(p, stored_form)
            .expect("append must accept the canonical schema");
        shared_close_handle(app2).unwrap();

        // A genuinely different element type is still refused.
        assert!(
            shared_append_to_path(p, "a<dict>m25alphas4betaj").is_err(),
            "append must still reject a different element type",
        );

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

    // A stream header carries the list-shaped value schema `[a]`, not the
    // bare element `a`. Production always builds it from the parsed `[a]`
    // (open_ostream/open_stdio enforce Array via reject_non_list_stream_schema);
    // tests must mirror that so the header is well-formed. The `a` prefix is
    // the Array schema-string form.
    fn list_schema(elem: &TSchema) -> TSchema {
        morloc_runtime_types::schema::parse_schema(&format!(
            "a{}",
            morloc_runtime_types::schema::schema_to_string(elem),
        )).unwrap()
    }

    /// A sub-packet whose elements are Strings, so every element carries a
    /// sub-allocation below the list. The int builder above produces a value
    /// with nothing below the root, which is exactly the shape that never
    /// leaked; a leak test needs this one.
    ///
    /// Payload layout, all offsets buffer-relative:
    ///   [outer Array] [n element Arrays] [concatenated bytes]
    fn build_str_voidstar_subpacket(values: &[&str]) -> Vec<u8> {
        let elem_inner = TSchema::primitive(TSerialType::String);
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
        let schema_len = schema_bytes.len() + 1;
        let padded_meta_len = (8 + schema_len + 31) / 32 * 32;

        let n = values.len();
        let hdr = std::mem::size_of::<ShmArray>();
        let elems_at = hdr;
        let bytes_at = elems_at + n * hdr;
        let total_bytes: usize = values.iter().map(|v| v.len()).sum();
        // Round the payload out so the sub-packet that follows starts on the
        // same boundary the first one did. A real writer pads for the same
        // reason; string bytes are the only part of this layout whose length
        // is not already a multiple of the element width.
        let payload_size = (bytes_at + total_bytes).div_ceil(32) * 32;
        let mut payload = vec![0u8; payload_size];

        payload[0..8].copy_from_slice(&(n as u64).to_le_bytes());
        payload[8..16].copy_from_slice(&(encode_relptr(0, elems_at) as i64).to_le_bytes());

        let mut cursor = bytes_at;
        for (i, v) in values.iter().enumerate() {
            let at = elems_at + i * hdr;
            payload[at..at + 8].copy_from_slice(&(v.len() as u64).to_le_bytes());
            payload[at + 8..at + 16]
                .copy_from_slice(&(encode_relptr(0, cursor) as i64).to_le_bytes());
            payload[cursor..cursor + v.len()].copy_from_slice(v.as_bytes());
            cursor += v.len();
        }

        let mut h = PacketHeader::data_mesg(VOIDSTAR, payload_size as u64);
        h.offset = padded_meta_len as u32;
        let mut packet = h.to_bytes().to_vec();
        let mut meta = vec![0u8; padded_meta_len];
        meta[0..3].copy_from_slice(&METADATA_HEADER_MAGIC);
        meta[3] = METADATA_TYPE_SCHEMA_STRING;
        meta[4..8].copy_from_slice(&(schema_len as u32).to_le_bytes());
        meta[8..8 + schema_bytes.len()].copy_from_slice(schema_bytes);
        packet.extend_from_slice(&meta);
        packet.extend_from_slice(&payload);
        packet
    }

    /// Build a stream file from already-rendered sub-packets.
    fn build_stream_file_from(
        value_schema: &TSchema,
        subs: Vec<Vec<u8>>,
        counts: &[u64],
    ) -> Vec<u8> {
        let mut out = make_stream_header_block(value_schema);
        let mut entries: Vec<morloc_runtime_types::packet::SubpacketEntry> = Vec::new();
        let mut element_count = 0u64;
        for (sub, &c) in subs.iter().zip(counts) {
            entries.push(morloc_runtime_types::packet::SubpacketEntry {
                offset: out.len() as u64,
                elem_count: c,
            });
            out.extend_from_slice(sub);
            element_count += c;
        }
        let mut diag = TStreamDiag::new();
        diag.subpacket_count = subs.len() as u64;
        diag.element_count = element_count;
        out.extend_from_slice(&make_final_footer_packet(
            &diag, &entries, morloc_runtime_types::packet::FOOTER_STATUS_CLOSED,
        ));
        out
    }

    fn build_stream_file(
        elem_schema: &TSchema,
        sub_values: &[&[i64]],
    ) -> Vec<u8> {
        let mut out = make_stream_header_block(&list_schema(elem_schema));
        let mut subpacket_entries: Vec<morloc_runtime_types::packet::SubpacketEntry> =
            Vec::with_capacity(sub_values.len());
        let mut element_count: u64 = 0;
        for values in sub_values {
            subpacket_entries.push(morloc_runtime_types::packet::SubpacketEntry {
                offset: out.len() as u64,
                elem_count: values.len() as u64,
            });
            let sub = build_int_voidstar_subpacket(values);
            out.extend_from_slice(&sub);
            element_count += values.len() as u64;
        }
        // Build a final footer carrying StreamDiag (with element_count
        // for `length f`) and the full sub-packet index.
        let mut diag = TStreamDiag::new();
        diag.subpacket_count = sub_values.len() as u64;
        diag.element_count = element_count;
        let footer = make_final_footer_packet(
            &diag, &subpacket_entries,
            morloc_runtime_types::packet::FOOTER_STATUS_CLOSED,
        );
        out.extend_from_slice(&footer);
        out
    }

    /// Live SHM blocks right now.
    fn live_blocks() -> usize {
        let mut hist = [0usize; 40];
        shm::live_block_stats(&mut hist).0
    }

    /// Every value the runtime hands a pool must be ONE self-contained block.
    ///
    /// A pool releases a value with a single `shfree` of the pointer it was
    /// given. That is the only release it performs and there is no
    /// schema-aware alternative anywhere on the pool side, so a value built
    /// as a graph of blocks loses everything below its root on every call.
    /// This runs each value-returning entry point in a loop, releases each
    /// result the way a pool does, and fails if the live block count moved.
    ///
    /// The count, not the byte total, is the assertion: a leak of one block
    /// per call is the failure, whatever its size.
    /// A stream file is outside data. A string whose recorded length runs
    /// past the end of its sub-packet must be refused by every reader, never
    /// read beyond the payload.
    #[test]
    fn a_string_longer_than_its_payload_is_refused() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir()
            .join(format!("morloc_overlong_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let elem = TSchema::primitive(TSerialType::String);
        let value_schema = list_schema(&elem);

        let mut sub = build_str_voidstar_subpacket(&["ab", "cd"]);
        let mut hdr_bytes = [0u8; 32];
        hdr_bytes.copy_from_slice(&sub[..32]);
        let hdr = PacketHeader::from_bytes(&hdr_bytes).unwrap();
        // The first element's size field follows the 16-byte outer Array.
        let size_at = 32 + hdr.offset as usize + 16;
        sub[size_at..size_at + 8].copy_from_slice(&(1u64 << 30).to_le_bytes());

        let path = dir.join("overlong.idx");
        std::fs::write(&path, build_stream_file_from(&value_schema, vec![sub], &[2])).unwrap();
        let p = path.to_str().unwrap();

        let f = open_ifile(p).unwrap();
        assert!(ifile_bracket_index(f, 0).is_err(), "bracket index");
        assert!(ifile_bracket_slice(f, Some(0), Some(1), None).is_err(), "bracket slice");
        assert!(
            shared_ifile_walk(f, ".[]", &[crate::intrinsics::IFileWalkArg::opt(Some(0))]).is_err(),
            "pattern walk"
        );
        close_handle(f).unwrap();

        let s = open_istream(p).unwrap();
        assert!(shared_next_frame(s).is_err(), "@next");
        let _ = shared_discard_handle(s);
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_value_handed_to_a_pool_is_one_block() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir()
            .join(format!("morloc_oneblock_{}", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);

        let strs: &[&str] = &[
            "alpha", "bravo", "charlie", "delta", "echo", "foxtrot",
        ];
        let elem = TSchema::primitive(TSerialType::String);
        let value_schema = list_schema(&elem);
        let path = dir.join("strs.idx");
        std::fs::write(&path, build_stream_file_from(
            &value_schema,
            vec![
                build_str_voidstar_subpacket(&strs[..3]),
                build_str_voidstar_subpacket(&strs[3..]),
            ],
            &[3, 3],
        )).unwrap();
        let p = path.to_str().unwrap();

        // Each case runs once to settle whatever it allocates lazily, then
        // the count is taken and the loop must not move it.
        let mut failures: Vec<String> = Vec::new();
        let mut check = |name: &str, mut f: Box<dyn FnMut()>| {
            f();
            let before = live_blocks();
            for _ in 0..8 {
                f();
            }
            let after = live_blocks();
            if after != before {
                failures.push(format!(
                    "{}: {} live blocks -> {} over 8 calls", name, before, after
                ));
            }
        };

        check("@next", Box::new(|| {
            let h = open_istream(p).unwrap();
            loop {
                let v = shared_next_subpacket(h).unwrap();
                let empty = unsafe { (*(v as *const shm_types_crate::Array)).size } == 0;
                shm::shfree(v).unwrap();
                if empty { break; }
            }
            let _ = shared_discard_handle(h);
        }));

        check("@load", Box::new(|| {
            let v = shared_load_stream_file_as_array(p, None).unwrap();
            shm::shfree(v).unwrap();
        }));

        check("@ifile .[:]", Box::new(|| {
            let h = open_ifile(p).unwrap();
            let v = ifile_bracket_slice(h, None, None, None).unwrap();
            shm::shfree(v).unwrap();
            let _ = shared_discard_handle(h);
        }));

        let json_strs = "[\"alpha\",\"bravo\",\"charlie\",\"delta\"]";
        let str_list = morloc_runtime_types::schema::parse_schema("as").unwrap();
        check("@read list of Str", Box::new(move || {
            let v = crate::json::read_json_with_schema(json_strs, &str_list).unwrap();
            shm::shfree(v).unwrap();
        }));

        let int_list = morloc_runtime_types::schema::parse_schema("ai4").unwrap();
        check("@read list of Int", Box::new(move || {
            let v = crate::json::read_json_with_schema("[1,2,3,4]", &int_list).unwrap();
            shm::shfree(v).unwrap();
        }));

        assert!(failures.is_empty(), "values that are not one block:\n  {}",
                failures.join("\n  "));
    }

    #[test]
    fn ifile_bracket_index_end_to_end() {
        let _shm = crate::own_test_registry();
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
        let _shm = crate::own_test_registry();
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
        let _shm = crate::own_test_registry();
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
        let _shm = crate::own_test_registry();
        crate::stream::set_test_ifile_cache_bytes(Some(256));
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
        crate::stream::set_test_ifile_cache_bytes(None);
    }

    #[test]
    fn a_cache_of_capacity_zero_holds_nothing() {
        let _shm = crate::own_test_registry();
        let dir = concat_test_dir("cache_zero");
        let path = dir.join("z.idx");
        let p = path.to_str().unwrap().to_string();
        crate::write_behind::set_test_depth(Some(0));
        let w = shared_open_ostream_with_schema(&p, "ai8").unwrap();
        let list = parse_schema("ai8").unwrap();
        let level = crate::compression::CompressionLevel::from_u8(3).unwrap();
        for json in ["[1, 2]", "[3, 4]", "[5, 6]"] {
            let v = crate::json::read_json_with_schema(json, &list).unwrap();
            shared_write_subpacket(w, level, v).unwrap();
            shm::shfree(v).unwrap();
            shared_flush_buffer(w).unwrap();
        }
        shared_close_handle(w).unwrap();
        crate::write_behind::set_test_depth(None);

        crate::stream::set_test_ifile_cache_bytes(Some(0));
        let f = open_ifile(&p).unwrap();
        crate::stream::set_test_ifile_cache_bytes(None);
        for i in 0..6 {
            let ptr = ifile_bracket_index(f, i).unwrap();
            assert_eq!(unsafe { *(ptr as *const i64) }, i + 1);
            shm::shfree(ptr).unwrap();
        }
        let held = with_process_local_slot(f, |local, _| Ok(local.cache.entries.len())).unwrap();
        shared_close_handle(f).unwrap();
        assert_eq!(held, 0, "a cache of capacity 0 kept {held} sub-packets");
    }

    /// DATA_PACKET file: a single voidstar packet (no STREAM header,
    /// no footer). The IFile dispatch treats the whole file as one
    /// sub-packet and exercises bracket index + slice + length over
    /// the file's payload via the file resolver (zero-copy).
    #[test]
    fn ifile_data_packet_zero_copy() {
        let _shm = crate::own_test_registry();
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
        // One free: a slice is one block, its element data included.
        shm::shfree(ptr).unwrap();

        // Out-of-bounds errors cleanly.
        assert!(ifile_bracket_index(handle, 20).is_err());

        close_handle(handle).unwrap();
        let _ = std::fs::remove_file(&path);
    }


    /// A footer-less stream file (writer crashed before writing the
    /// final footer) cannot be opened as an IFile: random access
    /// requires the per-sub-packet element counts, and those are only
    /// recorded in the final footer's SUBPACKET_INDEX block. IStream
    /// forward-drain remains available for such files.
    #[test]
    fn ifile_rejects_footerless_stream() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}_norec", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("ints-no-footer.idx");

        let elem_schema = TSchema::primitive(TSerialType::Sint64);
        let mut bytes = make_stream_header_block(&list_schema(&elem_schema));
        let sub_a: &[i64] = &[7, 8, 9];
        let sub_b: &[i64] = &[11];
        for vs in &[sub_a, sub_b] {
            bytes.extend_from_slice(&build_int_voidstar_subpacket(vs));
        }
        // No footer, no EOF tail -- simulates a crashed writer.
        std::fs::write(&path, &bytes).unwrap();

        let err = open_ifile(path.to_str().unwrap()).unwrap_err();
        let msg = format!("{:?}", err);
        assert!(
            msg.contains("no final footer"),
            "expected rejection to mention missing footer, got: {}",
            msg,
        );
        let _ = std::fs::remove_file(&path);
    }

    /// Group pattern `.(.[];.[])` against `[i64]`: the walker
    /// materialises a Tuple2 with (arr[i], arr[j]) in one call. This
    /// is the tuple-returning mode required by
    /// `PatternAccessible.__extract_pattern__` for group accessors.
    #[test]
    fn ifile_group_pattern_returns_tuple() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!(
            "morloc_stream_test_{}_group", std::process::id()
        ));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("ints-group.idx");

        let elem_schema = TSchema::primitive(TSerialType::Sint64);
        let sub: &[i64] = &[10, 20, 30, 40, 50];
        let bytes = build_stream_file(&elem_schema, &[sub]);
        std::fs::write(&path, &bytes).unwrap();

        let handle = open_ifile(path.to_str().unwrap()).unwrap();

        let arg = |v: i64| crate::intrinsics::IFileWalkArg {
            has: 1, _pad: [0u8; 7], value: v,
        };
        let args = [arg(1), arg(3)];
        let ptr = shared_ifile_walk(handle, ".(.[];.[])", &args)
            .expect("group walk failed");

        // Result is a Tuple2 (Sint64, Sint64). Voidstar tuple layout
        // packs the two 8-byte fields contiguously at offsets 0 and 8.
        let slot0 = unsafe { *(ptr as *const i64) };
        let slot1 = unsafe { *((ptr as *const u8).add(8) as *const i64) };
        assert_eq!(slot0, 20, "tuple slot 0 should be arr[1] = 20");
        assert_eq!(slot1, 40, "tuple slot 1 should be arr[3] = 40");

        shm::shfree(ptr).unwrap();
        close_handle(handle).unwrap();
        let _ = std::fs::remove_file(&path);
    }

    // Fork a child that exits immediately and reap it, returning its
    // now-dead pid (guaranteed to be ESRCH until the OS reuses it).
    fn reap_dead_child_pid() -> u32 {
        unsafe {
            let pid = libc::fork();
            if pid == 0 {
                libc::_exit(0);
            }
            let mut status = 0;
            libc::waitpid(pid, &mut status, 0);
            pid as u32
        }
    }

    /// A stale or corrupt @stdin claim must be reclaimed at open time so a
    /// fresh open succeeds instead of wedging with "already open", while a
    /// LIVE self-owned claim must still error. (@stdin is an IStream, so the
    /// reclaim skips OStream finalize and needs no nexus RPC.) Fails without
    /// the open-time self-heal (try_reclaim_stale_stdio_claim).
    #[test]
    fn stdio_stale_or_corrupt_claim_is_reclaimed() {
        use std::sync::atomic::Ordering;
        let _shm = crate::own_test_registry();
        registry_init().unwrap();
        let claim = stdio_claim_slot(STDIO_KIND_STDIN).expect("registry attached");

        // Corrupt claim: the handle unpacks to an out-of-range slot (60000
        // exceeds the default slot count). A fresh open reclaims the garbage.
        claim.store(pack_handle(1, 60000), Ordering::Release);
        let hc = open_stdio(MLC_KIND_ISTREAM, STDIO_KIND_STDIN, "")
            .expect("corrupt @stdin claim should be reclaimed");
        close_handle(hc).unwrap();

        // Live self-owned claim: a genuine double-open must still error.
        let h1 = open_stdio(MLC_KIND_ISTREAM, STDIO_KIND_STDIN, "").unwrap();
        assert!(
            open_stdio(MLC_KIND_ISTREAM, STDIO_KIND_STDIN, "").is_err(),
            "a live self-owned @stdin claim must still error",
        );

        // Dead owner: mark the slot's opener pid as a reaped (dead) child;
        // the next open must detect the dead owner, reclaim, and succeed.
        let (_g, idx) = unpack_handle(h1);
        let dead_pid = reap_dead_child_pid();
        let slot = slot_ref(idx).unwrap();
        slot.opener_pid.set(dead_pid);
        slot.opener_pid_start_time.set(0);
        let h2 = open_stdio(MLC_KIND_ISTREAM, STDIO_KIND_STDIN, "")
            .expect("dead-owner @stdin claim should be reclaimed");
        close_handle(h2).unwrap();
    }

    /// A stdio claim opened in a pool dispatch with no call id set and left
    /// open is released by the post-dispatch reclaim, so the next open
    /// succeeds.
    #[test]
    fn stdio_claim_left_open_by_a_pool_dispatch_is_reclaimed() {
        use std::sync::atomic::Ordering;
        let _shm = crate::own_test_registry();
        registry_init().unwrap();
        let claim = stdio_claim_slot(STDIO_KIND_STDIN).expect("registry attached");
        let prev = set_current_call_id(CALL_ID_NO_SWEEP);

        let _leaked = open_stdio(MLC_KIND_ISTREAM, STDIO_KIND_STDIN, "").unwrap();
        pool_reclaim_stdio_after_dispatch();
        assert_eq!(claim.load(Ordering::Acquire), STDIO_UNCLAIMED, "the leaked claim was not reclaimed");

        let h = open_stdio(MLC_KIND_ISTREAM, STDIO_KIND_STDIN, "")
            .expect("@stdin should open after the reclaim");
        close_handle(h).unwrap();
        set_current_call_id(prev);
    }

    /// Trim a sub-packet's payload to its first `keep` bytes, as a writer
    /// that does not pad payloads would have emitted it.
    fn unpadded(mut packet: Vec<u8>, keep: usize) -> Vec<u8> {
        let mut h = PacketHeader::from_bytes(packet[..32].try_into().unwrap()).unwrap();
        packet.truncate(32 + h.offset as usize + keep);
        h.length = keep as u64;
        packet[..32].copy_from_slice(&h.to_bytes());
        packet
    }

    /// A sub-packet whose payload is not 8-aligned in the file -- written
    /// before payloads were padded, or joined by @concat after one that
    /// was not -- reads back through every reader.
    #[test]
    fn unaligned_subpacket_reads_back() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!("morloc_stream_test_{}_unaligned", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("unaligned.stream");
        let path = path.to_str().unwrap();

        // One element: a 16-byte header, one 16-byte string slot, 3 bytes.
        let first = unpadded(build_str_voidstar_subpacket(&["abc"]), 16 + 16 + 3);
        let second = build_str_voidstar_subpacket(&["de", "", "fghij"]);
        let list = parse_schema("as").unwrap();
        let bytes = build_stream_file_from(&list, vec![first.clone(), second], &[1, 3]);
        let second_at = make_stream_header_block(&list).len() + first.len();
        let second_meta = PacketHeader::from_bytes(
            bytes[second_at..second_at + 32].try_into().unwrap(),
        ).unwrap().offset as usize;
        assert_ne!((second_at + 32 + second_meta) % 8, 0, "fixture must be misaligned");
        std::fs::write(path, &bytes).unwrap();

        let render = |p: AbsPtr| {
            let s = crate::json::voidstar_to_json_string(p, &list).unwrap();
            shm::shfree(p).unwrap();
            s
        };
        let r = shared_open_istream(path).unwrap();
        let mut frames = Vec::new();
        while let Some(p) = shared_next_frame(r).unwrap() {
            frames.push(render(p));
        }
        let _ = shared_discard_handle(r);
        assert_eq!(frames, vec![r#"["abc"]"#, r#"["de","","fghij"]"#]);

        assert_eq!(render(shared_load_stream_file_as_array(path, Some(&list)).unwrap()), r#"["abc","de","","fghij"]"#);

        let f = open_ifile(path).unwrap();
        let arg = crate::intrinsics::IFileWalkArg::opt;
        let one = shared_ifile_walk(f, ".[]", &[arg(Some(3))]).unwrap();
        let elem = parse_schema("s").unwrap();
        assert_eq!(crate::json::voidstar_to_json_string(one, &elem).unwrap(), r#""fghij""#);
        shm::shfree(one).unwrap();
        let run = shared_ifile_walk(f, ".[:]", &[arg(Some(1)), arg(Some(4)), arg(None)]).unwrap();
        assert_eq!(render(run), r#"["de","","fghij"]"#);
        close_handle(f).unwrap();
        let _ = std::fs::remove_dir_all(&dir);
    }

    #[test]
    fn a_failed_publish_frees_the_blocks_the_slot_never_took() {
        let _shm = crate::own_test_registry();
        let live = || shm::live_block_stats(&mut [0usize; 64]).0;
        let before = live();
        let taken;
        {
            let mut pending = Unpublished::default();
            let _left = pending.hold(shm_copy_bytes(b"left behind").unwrap());
            let held = pending.hold(shm_copy_bytes(b"taken").unwrap());
            taken = pending.own(held);
        }
        assert_eq!(live(), before + 1);
        shm::shfree(shm::rel2abs(taken).unwrap()).unwrap();
        shm::forget_held_references();
    }

    #[test]
    fn loading_a_stream_file_as_another_type_is_refused() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!("morloc_stream_test_{}_retype", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("strs.stream");
        let path = path.to_str().unwrap();
        let list = parse_schema("as").unwrap();
        let sub = build_str_voidstar_subpacket(&["ab", "cd"]);
        std::fs::write(path, build_stream_file_from(&list, vec![sub], &[2])).unwrap();

        let reals = parse_schema("af8").unwrap();
        let err = shared_load_stream_file_as_array(path, Some(&reals)).unwrap_err();
        assert!(err.to_string().contains("schema mismatch"), "{err}");
        let p = shared_load_stream_file_as_array(path, Some(&list)).unwrap();
        shm::shfree(p).unwrap();
        let _ = std::fs::remove_dir_all(&dir);
    }

    /// A string whose length claims more bytes than its sub-packet holds is
    /// rejected by every reader rather than read past the payload.
    #[test]
    fn overlong_string_in_file_is_rejected() {
        let _shm = crate::own_test_registry();
        let dir = std::env::temp_dir().join(format!("morloc_stream_test_{}_overlong", std::process::id()));
        let _ = std::fs::create_dir_all(&dir);
        let path = dir.join("overlong.stream");
        let path = path.to_str().unwrap();

        let mut sub = build_str_voidstar_subpacket(&["ab", "cd", "ef"]);
        let meta = PacketHeader::from_bytes(sub[..32].try_into().unwrap()).unwrap().offset as usize;
        // The third string's slot is the third 16-byte header after the
        // list's own; its first 8 bytes are the length.
        let at = 32 + meta + 16 + 2 * 16;
        sub[at..at + 8].copy_from_slice(&(1u64 << 20).to_le_bytes());
        let list = parse_schema("as").unwrap();
        std::fs::write(path, build_stream_file_from(&list, vec![sub], &[3])).unwrap();

        let r = shared_open_istream(path).unwrap();
        assert!(shared_next_frame(r).is_err(), "@next accepted an overlong string");
        let _ = shared_discard_handle(r);
        assert!(shared_load_stream_file_as_array(path, Some(&list)).is_err(), "@load accepted it");

        let f = open_ifile(path).unwrap();
        let arg = crate::intrinsics::IFileWalkArg::opt;
        for (i, j) in [(0, 3), (2, 3), (1, 3)] {
            let r = shared_ifile_walk(f, ".[:]", &[arg(Some(i)), arg(Some(j)), arg(None)]);
            assert!(r.is_err(), "slice .[{i}:{j}] accepted an overlong string");
        }
        // Slices that do not reach the bad element are unaffected.
        let ok = shared_ifile_walk(f, ".[:]", &[arg(Some(0)), arg(Some(2)), arg(None)]).unwrap();
        assert_eq!(crate::json::voidstar_to_json_string(ok, &list).unwrap(), r#"["ab","cd"]"#);
        shm::shfree(ok).unwrap();
        close_handle(f).unwrap();
        let _ = std::fs::remove_dir_all(&dir);
    }
}


// -- Channels -------------------------------------------------------------
//
// A channel is a stream whose producer and readers run at the same time: a
// producer writes it as an OStream (`@write`, `@flush`, `@close`) and any
// pool reads it as an IStream (`@next`), with every batch passing through
// shared memory. Each flushed sub-packet is copied into one SHM block and
// relocated in place there ('payload_into_shm'), so a reader takes a ready
// array without a further copy. The queue is a linked list; a writer waits
// before a `@write` while the queue holds `channel_depth()` batches or more,
// so memory stays bounded by that depth plus the batch being written.
//
// The slot's `subpacket_entries` field (IFile/OStream bookkeeping, unused by
// a channel) holds the channel block. Every mutation of the block is made
// under the slot lock; waits happen outside it.

/// Why a channel cannot be named by a path: it exists only in this
/// program's shared memory, so it cannot be sent to a remote pool, stored
/// in a stream file, used as a cache key or returned as a result.
pub const CHANNEL_HAS_NO_PATH: &str =
    "a stream read from a @parse argument exists only while this command runs \
     and cannot leave it (sent to a remote pool, stored, cached or returned); \
     save it to a file first";

const CHANNEL_RUNNING: u32 = 0;
const CHANNEL_DONE: u32 = 1;
const CHANNEL_FAILED: u32 = 2;

#[repr(C)]
struct ChannelBlock {
    status: u32,
    /// Set when a reader was handed the failure: the error then belongs to
    /// the command, not only to the part of the input nobody read.
    delivered: u32,
    count: u64,
    head: RelPtr,
    tail: RelPtr,
    fail_msg: RelPtr,
    fail_len: u64,
}

#[repr(C)]
struct ChannelNode {
    next: RelPtr,
    arr: RelPtr,
}

fn channel_depth() -> u64 {
    static DEPTH: morloc_runtime_types::publish_once::PublishOnce<u64> = morloc_runtime_types::publish_once::PublishOnce::new();
    *DEPTH.get_or_init(|| {
        std::env::var("MORLOC_CHANNEL_DEPTH")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .filter(|&d| d > 0)
            .unwrap_or(4)
    })
}

/// Back off between polls of a channel: a short spin, then sleeps growing
/// to a millisecond.
fn channel_backoff(round: &mut u32) {
    *round += 1;
    if *round < 64 {
        std::thread::yield_now();
    } else {
        let us = (50u64 << ((*round - 64).min(5))).min(1000);
        std::thread::sleep(std::time::Duration::from_micros(us));
    }
}

/// The channel block of a slot. Caller holds the slot lock and has checked
/// the slot is a channel.
fn channel_block(slot: &RegistrySlot) -> Result<*mut ChannelBlock, MorlocError> {
    Ok(crate::shm::rel2abs(slot.subpacket_entries.get())? as *mut ChannelBlock)
}

/// The slot a handle names, if it is still that channel.
/// Whether `handle` names an open channel, whose reads and writes may wait.
pub fn shared_is_channel(handle: i64) -> bool {
    matches!(channel_slot(handle), Ok(Some(_)))
}

fn channel_slot(handle: i64) -> Result<Option<(&'static RegistrySlot, u64)>, MorlocError> {
    use std::sync::atomic::Ordering;
    let (gen_claim, slot_idx) = unpack_handle(handle);
    let slot = slot_ref(slot_idx).ok_or_else(|| MorlocError::Other(format!(
        "stream handle {:#x}: slot index {} out of range", handle, slot_idx,
    )))?;
    let gen_now = slot.generation.load(Ordering::Acquire) & GENERATION_MASK;
    if gen_now != gen_claim || slot.state.load(Ordering::Acquire) != SLOT_STATE_OPEN_SHARED {
        return Ok(None);
    }
    if slot.kind.get() != MLC_KIND_CHANNEL {
        return Ok(None);
    }
    Ok(Some((slot, gen_claim)))
}

fn slot_generation_is(slot: &RegistrySlot, gen_claim: u64) -> bool {
    use std::sync::atomic::Ordering;
    slot.generation.load(Ordering::Acquire) & GENERATION_MASK == gen_claim
}

/// Open a channel carrying values of the list schema `schema_str` (`[a]`).
/// The one handle serves as both the producer's OStream and the readers'
/// IStream.
pub fn shared_open_channel(schema_str: &str) -> Result<i64, MorlocError> {
    use std::sync::atomic::Ordering;
    let parsed_schema = parse_schema(schema_str).map_err(|e| MorlocError::Schema(format!(
        "channel open: unparseable schema '{}': {}", schema_str, e,
    )))?;
    reject_non_list_stream_schema(&parsed_schema, "channel open", "<channel>")?;

    let (slot_idx, slot, guard) = allocate_slot_cas()?;
    let publish = (|| -> Result<u64, MorlocError> {
        let mut pending = Unpublished::default();
        let schema_rel = pending.hold(shm_copy_bytes(schema_str.as_bytes())?);
        let buf_abs = crate::shm::shcalloc(1, read_write_buffer_bytes_env())?;
        let buf_rel = pending.hold(crate::shm::abs2rel(buf_abs)?);
        let block_abs = crate::shm::shcalloc(1, std::mem::size_of::<ChannelBlock>())?;
        unsafe {
            let b = block_abs as *mut ChannelBlock;
            (*b).status = CHANNEL_RUNNING;
            (*b).head = shm_types_crate::RELNULL;
            (*b).tail = shm_types_crate::RELNULL;
            (*b).fail_msg = shm_types_crate::RELNULL;
        }
        let block_rel = pending.hold(crate::shm::abs2rel(block_abs)?);
        unsafe {
            slot.kind.set(MLC_KIND_CHANNEL);
            slot.file_path.set(shm_types_crate::RELNULL);
            slot.file_path_len.set(0);
            slot.schema_str.set(pending.own(schema_rel));
            slot.schema_str_len.set(schema_str.len() as u32);
            slot.subpacket_entries.set(pending.own(block_rel));
            slot.subpacket_entries_len.set(0);
            slot.subpacket_entries_cap.set(0);
            slot.body_start.set(0);
            slot.final_footer.set(0);
            slot.cursor.set(0);
            slot.element_count.set(0);
            slot.compression_level.set(0);
            *slot.diag.get() = StreamDiag::new();
            slot.write_buffer.set(pending.own(buf_rel));
            slot.write_buffer_index_cap.set(0);
            slot.write_buffer_index_count.set(0);
            slot.write_buffer_data_used.set(0);
        }
        let bump = registry_gen_salt() | 1;
        Ok(slot.generation.fetch_add(bump, Ordering::AcqRel).wrapping_add(bump) & GENERATION_MASK)
    })();
    let new_gen = match publish {
        Ok(g) => g,
        Err(e) => {
            release_slot_locked(slot);
            return Err(e);
        }
    };
    drop(guard);
    let handle = pack_handle(new_gen, slot_idx);
    install_process_local_slot(handle, channel_local(new_gen, &parsed_schema));
    Ok(handle)
}

/// A channel's process-local state: its schemas; no file.
fn channel_local(generation: u64, value_schema: &Schema) -> ProcessLocalSlot {
    ProcessLocalSlot {
        cached_generation: generation,
        mmap_ptr: std::ptr::null_mut(),
        mmap_size: 0,
        pages_dropped: 0,
        map_file: None,
        fd: -1,
        cache: crate::fork_policy::ForkLocal::new(Box::new(StreamCache::new(0))),
        value_schema: value_schema.clone(),
        elem_schema: value_schema.parameters[0].clone(),
        subpacket_entries_local: Vec::new(),
        subpacket_elem_cum: None,
        is_data_packet: false,
        holds_lock: false,
        fork_epoch: fork_epoch(),
    }
}

/// Before a `@write` to a channel: wait, outside the slot lock, until the
/// queue has room. A channel no longer running refuses the write -- its
/// readers are gone (the slot was released) or it was already finished --
/// which is what stops a producer nobody reads any more.
fn channel_wait_room(handle: i64) -> Result<(), MorlocError> {
    let mut round = 0u32;
    loop {
        let Some((slot, gen_claim)) = channel_slot(handle)? else {
            return Err(MorlocError::Other("the reader of this stream has stopped".into()));
        };
        {
            let _g = SlotGuard::lock(slot)?;
            if !slot_generation_is(slot, gen_claim) {
                return Err(MorlocError::Other("the reader of this stream has stopped".into()));
            }
            let b = channel_block(slot)?;
            let (status, count) = unsafe { ((*b).status, (*b).count) };
            if status != CHANNEL_RUNNING {
                return Err(MorlocError::Other("this stream was already finished".into()));
            }
            if count < channel_depth() {
                return Ok(());
            }
        }
        channel_backoff(&mut round);
    }
}

/// Queue one flushed sub-packet. Caller holds the slot lock.
fn channel_enqueue(
    slot: &RegistrySlot,
    local: &ProcessLocalSlot,
    payload: PortablePayload<'_>,
) -> Result<(), MorlocError> {
    let arr = payload_into_shm(payload.0, &local.elem_schema, 0)?;
    let node_abs = match crate::shm::shmalloc(std::mem::size_of::<ChannelNode>()) {
        Ok(n) => n,
        Err(e) => {
            let _ = crate::shm::shfree(arr);
            return Err(e);
        }
    };
    let node_rel = crate::shm::abs2rel(node_abs)?;
    unsafe {
        let node = node_abs as *mut ChannelNode;
        (*node).next = shm_types_crate::RELNULL;
        (*node).arr = slot_owns(crate::shm::abs2rel(arr)?);
        let b = channel_block(slot)?;
        if (*b).tail == shm_types_crate::RELNULL {
            (*b).head = node_rel;
        } else {
            let tail = crate::shm::rel2abs((*b).tail)? as *mut ChannelNode;
            (*tail).next = node_rel;
        }
        (*b).tail = slot_owns(node_rel);
        (*b).count += 1;
    }
    Ok(())
}

/// Take the next batch of a channel, waiting for the producer: `Some` array,
/// `None` at the end, or the producer's failure (which is then marked
/// delivered).
fn channel_pop(handle: i64) -> Result<Option<AbsPtr>, MorlocError> {
    let mut round = 0u32;
    loop {
        let Some((slot, gen_claim)) = channel_slot(handle)? else {
            return Err(MorlocError::Other(format!(
                "stream handle {:#x}: the stream was closed", handle,
            )));
        };
        {
            let _g = SlotGuard::lock(slot)?;
            if !slot_generation_is(slot, gen_claim) {
                continue;
            }
            let b = channel_block(slot)?;
            unsafe {
                if (*b).count > 0 {
                    let node_abs = crate::shm::rel2abs((*b).head)?;
                    let node = node_abs as *mut ChannelNode;
                    let arr = crate::shm::rel2abs((*node).arr)?;
                    (*b).head = (*node).next;
                    if (*b).head == shm_types_crate::RELNULL {
                        (*b).tail = shm_types_crate::RELNULL;
                    }
                    (*b).count -= 1;
                    free_uncounted(node_abs);
                    // SHM-8: the batch is this process's from here.
                    crate::shm::take_on_reference();
                    return Ok(Some(arr));
                }
                match (*b).status {
                    CHANNEL_DONE => return Ok(None),
                    CHANNEL_FAILED => {
                        (*b).delivered = 1;
                        return Err(MorlocError::Other(channel_fail_message(b)));
                    }
                    _ => {}
                }
            }
        }
        channel_backoff(&mut round);
    }
}

fn channel_fail_message(b: *mut ChannelBlock) -> String {
    unsafe {
        if (*b).fail_msg == shm_types_crate::RELNULL {
            return "the stream's producer failed".into();
        }
        match crate::shm::rel2abs((*b).fail_msg) {
            Ok(p) => String::from_utf8_lossy(std::slice::from_raw_parts(p, (*b).fail_len as usize)).into_owned(),
            Err(_) => "the stream's producer failed".into(),
        }
    }
}

/// The producer finished: every batch it wrote has been queued. Caller
/// holds the slot lock.
fn channel_finish(slot: &RegistrySlot) -> Result<(), MorlocError> {
    let b = channel_block(slot)?;
    unsafe {
        if (*b).status == CHANNEL_RUNNING {
            (*b).status = CHANNEL_DONE;
        }
    }
    Ok(())
}

/// The producer stopped without finishing: its readers see `msg` after the
/// batches already queued. No effect on a channel already finished or
/// released.
pub fn shared_channel_fail(handle: i64, msg: &str) -> Result<(), MorlocError> {
    let Some((slot, gen_claim)) = channel_slot(handle)? else {
        return Ok(());
    };
    let _g = SlotGuard::lock(slot)?;
    if !slot_generation_is(slot, gen_claim) {
        return Ok(());
    }
    let b = channel_block(slot)?;
    unsafe {
        if (*b).status != CHANNEL_RUNNING {
            return Ok(());
        }
        let rel = shm_copy_bytes(msg.as_bytes())?;
        (*b).fail_msg = slot_owns(rel);
        (*b).fail_len = msg.len() as u64;
        (*b).status = CHANNEL_FAILED;
    }
    Ok(())
}

/// The producer's call returned. It closes the channel when it finishes, so
/// this only matters for one that returned without closing it: its readers
/// see the end after what it queued.
pub fn shared_channel_ended(handle: i64) -> Result<(), MorlocError> {
    let Some((slot, gen_claim)) = channel_slot(handle)? else {
        return Ok(());
    };
    let _g = SlotGuard::lock(slot)?;
    if !slot_generation_is(slot, gen_claim) {
        return Ok(());
    }
    channel_finish(slot)
}

/// Settle a channel once its readers are done with it, releasing the slot.
/// A failure a reader was handed is returned as the error; a failure in
/// input nobody read, and a producer still running (its readers stopped
/// early), are not errors: the producer's next write is refused. Never
/// waits for the producer.
pub fn shared_settle_channel(handle: i64) -> Result<(), MorlocError> {
    let Some((slot, gen_claim)) = channel_slot(handle)? else {
        return Ok(());
    };
    let failure = {
        let _g = SlotGuard::lock_any(slot)?;
        if !slot_generation_is(slot, gen_claim) {
            return Ok(());
        }
        if slot.poisoned.get() != 0 {
            release_slot_locked(slot);
            drop(_g);
            invalidate_process_local_slot(handle);
            return Err(died_inside());
        }
        let b = channel_block(slot)?;
        let failure = unsafe {
            if (*b).status == CHANNEL_FAILED && (*b).delivered != 0 {
                Some(channel_fail_message(b))
            } else {
                None
            }
        };
        release_slot_locked(slot);
        failure
    };
    invalidate_process_local_slot(handle);
    match failure {
        Some(msg) => Err(MorlocError::Other(msg)),
        None => Ok(()),
    }
}

/// Free a channel's queued batches and its failure message. Caller holds
/// the slot lock; the block itself is freed with the slot's other blocks.
fn channel_free_queue(slot: &RegistrySlot) {
    let Ok(b) = channel_block(slot) else { return };
    unsafe {
        let mut node_rel = (*b).head;
        while node_rel != shm_types_crate::RELNULL {
            let Ok(node_abs) = crate::shm::rel2abs(node_rel) else { break };
            let node = node_abs as *mut ChannelNode;
            if let Ok(arr) = crate::shm::rel2abs((*node).arr) {
                free_uncounted(arr);
            }
            node_rel = (*node).next;
            free_uncounted(node_abs);
        }
        (*b).head = shm_types_crate::RELNULL;
        (*b).tail = shm_types_crate::RELNULL;
        (*b).count = 0;
        if (*b).fail_msg != shm_types_crate::RELNULL {
            if let Ok(p) = crate::shm::rel2abs((*b).fail_msg) {
                free_uncounted(p);
            }
            (*b).fail_msg = shm_types_crate::RELNULL;
        }
    }
}

#[cfg(test)]
mod channel_tests {
    use super::*;
    use crate::json::{read_json_with_schema, voidstar_to_json_string};

    fn batch(json: &str) -> AbsPtr {
        read_json_with_schema(json, &parse_schema("as").unwrap()).unwrap()
    }

    fn write(h: i64, json: &str) -> Result<(), MorlocError> {
        let v = batch(json);
        let r = shared_write_subpacket(h, crate::compression::CompressionLevel::NONE, v)
            .and_then(|()| shared_flush_buffer(h));
        crate::shm::shfree(v).unwrap();
        r
    }

    fn read(h: i64) -> Result<Option<String>, MorlocError> {
        match shared_next_frame(h)? {
            None => Ok(None),
            Some(p) => {
                let s = voidstar_to_json_string(p, &parse_schema("as").unwrap()).unwrap();
                crate::shm::shfree(p).unwrap();
                Ok(Some(s))
            }
        }
    }

    #[test]
    fn a_settled_channel_leaves_no_reference_counted_to_its_process() {
        let _shm = crate::own_test_registry();
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            let h = shared_open_channel("as").unwrap();
            write(h, r#"["a"]"#).unwrap();
            write(h, r#"["b"]"#).unwrap();
            shared_close_handle(h).unwrap();
            let _ = read(h).unwrap();
            shared_settle_channel(h).unwrap();
            let held = crate::shm::held_references();
            if held != 0 {
                eprintln!("a settled channel left {held} references counted");
            }
            held == 0
        }));
    }

    #[test]
    fn a_channel_delivers_batches_in_order_then_ends() {
        let _shm = crate::own_test_registry();
        let h = shared_open_channel("as").unwrap();
        write(h, r#"["a","bc"]"#).unwrap();
        write(h, r#"["","d"]"#).unwrap();
        shared_close_handle(h).unwrap();
        assert_eq!(read(h).unwrap().as_deref(), Some(r#"["a","bc"]"#));
        assert_eq!(read(h).unwrap().as_deref(), Some(r#"["","d"]"#));
        assert_eq!(read(h).unwrap(), None);
        shared_settle_channel(h).unwrap();
    }

    #[test]
    fn a_failure_follows_the_queued_batches_and_settles_as_an_error_once_read() {
        let _shm = crate::own_test_registry();
        let h = shared_open_channel("as").unwrap();
        write(h, r#"["x"]"#).unwrap();
        shared_channel_fail(h, "\u{1}0\u{1f}bad line 7\u{2}").unwrap();
        assert_eq!(read(h).unwrap().as_deref(), Some(r#"["x"]"#));
        let e = read(h).unwrap_err().to_string();
        assert!(e.contains("\u{1}0\u{1f}bad line 7\u{2}"), "{e}");
        let e = shared_settle_channel(h).unwrap_err().to_string();
        assert!(e.contains("bad line 7"), "{e}");
    }

    #[test]
    fn a_failure_nobody_read_is_not_an_error() {
        let _shm = crate::own_test_registry();
        let h = shared_open_channel("as").unwrap();
        write(h, r#"["x"]"#).unwrap();
        shared_channel_fail(h, "bad line 9").unwrap();
        assert_eq!(read(h).unwrap().as_deref(), Some(r#"["x"]"#));
        shared_settle_channel(h).unwrap();
    }

    #[test]
    fn settling_early_stops_the_producer_at_its_next_write() {
        let _shm = crate::own_test_registry();
        let h = shared_open_channel("as").unwrap();
        write(h, r#"["x"]"#).unwrap();
        shared_settle_channel(h).unwrap();
        assert!(write(h, r#"["y"]"#).is_err());
        // Settling again, or the producer's call ending, is harmless.
        shared_settle_channel(h).unwrap();
        shared_channel_ended(h).unwrap();
    }

    #[test]
    fn a_full_channel_holds_its_producer_until_a_reader_takes_a_batch() {
        let _shm = crate::own_test_registry();
        let h = shared_open_channel("as").unwrap();
        for _ in 0..channel_depth() {
            write(h, r#"["x"]"#).unwrap();
        }
        let produced = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
        let p2 = produced.clone();
        let producer = std::thread::spawn(move || {
            write(h, r#"["last"]"#).unwrap();
            p2.store(true, std::sync::atomic::Ordering::SeqCst);
            shared_close_handle(h).unwrap();
        });
        std::thread::sleep(std::time::Duration::from_millis(50));
        assert!(!produced.load(std::sync::atomic::Ordering::SeqCst));
        let mut n = 0;
        while let Some(_) = read(h).unwrap() {
            n += 1;
        }
        producer.join().unwrap();
        assert_eq!(n, channel_depth() + 1);
        shared_settle_channel(h).unwrap();
    }

    #[test]
    fn a_channel_has_no_path() {
        let _shm = crate::own_test_registry();
        let h = shared_open_channel("as").unwrap();
        let e = crate::handle_scan::portable_path(h).unwrap_err().to_string();
        assert!(e.contains("cannot leave it"), "{e}");
        shared_settle_channel(h).unwrap();
    }
}

#[cfg(test)]
mod write_behind_tests {
    use super::*;
    use crate::compression::CompressionLevel;

    fn test_dir(tag: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "morloc_write_behind_{}_{}", tag, std::process::id()
        ));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    /// Write `batches` of strings to a file OStream through `@write`, with a
    /// `buf_bytes` write buffer and `depth` batches compressed behind the
    /// writer, flushing after the batches listed in `flush_after`.
    fn write_strs(
        path: &std::path::Path,
        batches: &[Vec<String>],
        level: u8,
        buf_bytes: usize,
        depth: usize,
        flush_after: &[usize],
    ) {
        crate::stream::set_test_write_buffer_bytes(Some(buf_bytes));
        crate::write_behind::set_test_depth(Some(depth));
        let list = parse_schema("as").unwrap();
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        for (i, b) in batches.iter().enumerate() {
            let json = serde_json::to_string(b).unwrap();
            let v = crate::json::read_json_with_schema(&json, &list).unwrap();
            shared_write_subpacket(h, CompressionLevel::from_u8(level).unwrap(), v).unwrap();
            shm::shfree(v).unwrap();
            if flush_after.contains(&i) {
                shared_flush_buffer(h).unwrap();
            }
        }
        shared_close_handle(h).unwrap();
        crate::stream::set_test_write_buffer_bytes(None);
        crate::write_behind::set_test_depth(None);
    }

    fn read_strs(path: &std::path::Path) -> Vec<String> {
        let list = parse_schema("as").unwrap();
        let h = shared_open_istream(path.to_str().unwrap()).unwrap();
        let mut out = Vec::new();
        loop {
            let arr = shared_next_subpacket(h).unwrap();
            let n = unsafe { (*(arr as *const shm_types_crate::Array)).size };
            if n == 0 {
                shm::shfree(arr).unwrap();
                break;
            }
            let json = crate::json::voidstar_to_json_string(arr, &list).unwrap();
            out.extend(serde_json::from_str::<Vec<String>>(&json).unwrap());
            shm::shfree(arr).unwrap();
        }
        shared_close_handle(h).unwrap();
        out
    }

    /// The sub-packets of a closed stream file, each as its on-disk bytes.
    fn subpackets(path: &std::path::Path) -> Vec<Vec<u8>> {
        let bytes = std::fs::read(path).unwrap();
        let p = path.to_str().unwrap();
        let (mp, sz) = mmap_file_readonly(p).unwrap();
        let parsed = parse_stream_file(p, mp, sz).unwrap();
        unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize); }
        parsed
            .subpacket_entries
            .iter()
            .map(|e| {
                let at = e.offset as usize;
                let hdr = PacketHeader::from_bytes(bytes[at..at + 32].try_into().unwrap()).unwrap();
                let len = 32 + hdr.offset as usize + hdr.length as usize;
                bytes[at..at + len].to_vec()
            })
            .collect()
    }

    /// Strings of lengths 1..=13, so element data regions need padding to 8.
    fn odd_batches(n_batches: usize, per_batch: usize, fill: char) -> Vec<Vec<String>> {
        (0..n_batches)
            .map(|b| {
                (0..per_batch)
                    .map(|i| {
                        let k = b * per_batch + i;
                        let mut s = format!("{k}:");
                        while s.len() < 1 + (k % 13) + 2 {
                            s.push(fill);
                        }
                        s
                    })
                    .collect()
            })
            .collect()
    }

    // A sub-packet's bytes depend only on its elements. The write buffer
    // is reused across flushes, so alignment padding left unwritten would
    // carry bytes of the batch before it.
    #[test]
    fn subpacket_bytes_do_not_depend_on_earlier_batches() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("padding");
        let dirty = dir.join("dirty.idx");
        let clean = dir.join("clean.idx");
        let first = vec![(0..40).map(|i| "Z".repeat(9 + i % 7)).collect::<Vec<_>>()];
        let second = odd_batches(1, 40, 'a');
        let both: Vec<Vec<String>> = first.iter().chain(second.iter()).cloned().collect();
        write_strs(&dirty, &both, 0, 1 << 20, 0, &[0]);
        write_strs(&clean, &second, 0, 1 << 20, 0, &[]);
        let d = subpackets(&dirty);
        let c = subpackets(&clean);
        assert_eq!(d.len(), 2);
        assert_eq!(c.len(), 1);
        assert_eq!(d[1], c[0], "the second sub-packet carries bytes of the first");
    }

    // The queue's depth changes when a sub-packet is written, never what or
    // where: the file is the one a synchronous writer makes.
    #[test]
    fn the_queue_depth_changes_when_sub_packets_are_written_never_what() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("same");
        let sync = dir.join("sync.idx");
        let behind = dir.join("behind.idx");
        let batches = odd_batches(60, 37, 'q');
        for depth in [1, 3, 7] {
            write_strs(&sync, &batches, 3, 4096, 1, &[17, 41]);
            write_strs(&behind, &batches, 3, 4096, depth, &[17, 41]);
            let s = subpackets(&sync);
            assert!(s.len() > 20, "the test must fill many buffers, got {}", s.len());
            assert_eq!(s, subpackets(&behind), "depth {depth}");
            let want: Vec<String> = batches.concat();
            assert_eq!(read_strs(&behind), want, "depth {depth}");
        }
    }

    // SLOT-12: no more batches are outstanding than the queue's depth.
    #[test]
    fn outstanding_batches_never_exceed_the_queue_depth() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("depth");
        let path = dir.join("depth.idx");
        crate::stream::set_test_write_buffer_bytes(Some(4096));
        crate::write_behind::set_test_depth(Some(3));
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let (_gen, idx) = unpack_handle(h);
        let q = slot_queue(slot_ref(idx).unwrap()).unwrap();
        let before = q.pushed();
        let mut most = 0;
        for b in odd_batches(40, 37, 's') {
            write_one(h, &b, 3);
            most = most.max(q.outstanding());
            assert!(q.outstanding() <= 3, "{} batches outstanding, depth is 3", q.outstanding());
        }
        assert!(q.pushed().wrapping_sub(before) > 10, "the test must queue many buffers");
        shared_close_handle(h).unwrap();
        crate::stream::set_test_write_buffer_bytes(None);
        crate::write_behind::set_test_depth(None);
        assert!(most >= 1);
        assert_eq!(read_strs(&path), odd_batches(40, 37, 's').concat());
    }

    // SLOT-1
    #[test]
    fn a_stale_handle_never_touches_the_slot_it_used_to_name() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("stale_write");
        let path = dir.join("stale.idx");
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let (gen_claim, idx) = unpack_handle(h);
        let slot = slot_ref(idx).unwrap();
        let q = slot_queue(slot).unwrap();
        let stale = pack_handle((gen_claim + 1) & GENERATION_MASK, idx);
        let pushed = q.pushed();
        let count = slot.element_count.get();
        let list = parse_schema("as").unwrap();
        let v = crate::json::read_json_with_schema("[\"a\"]", &list).unwrap();
        assert!(shared_write_subpacket(stale, CompressionLevel::from_u8(0).unwrap(), v).is_err());
        assert!(shared_flush_buffer(stale).is_err());
        assert!(shared_close_handle(stale).is_err());
        shm::shfree(v).unwrap();
        assert_eq!(q.pushed(), pushed, "a stale handle queued something");
        assert_eq!(slot.element_count.get(), count, "a stale handle wrote an element");
        assert!(lock_for_write(slot, (gen_claim + 1) & GENERATION_MASK, "stale").is_err());
        shared_close_handle(h).unwrap();
    }

    fn fork_writer(h: i64, batches: Vec<Vec<String>>) -> libc::pid_t {
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            unsafe { libc::alarm(20) };
            let ok = std::panic::catch_unwind(|| {
                for b in &batches {
                    write_one(h, b, 0);
                }
            })
            .is_ok();
            unsafe { libc::_exit(if ok { 0 } else { 3 }) };
        }
        pid
    }

    fn reap(pid: libc::pid_t) -> i32 {
        let mut st = 0;
        unsafe { libc::waitpid(pid, &mut st, 0) };
        st
    }

    // SLOT-13: a writer that leaves through _exit, with elements still
    // buffered, loses none of them.
    #[test]
    fn a_forked_writer_that_exits_without_flushing_loses_nothing() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("child_exit");
        let path = dir.join("child.idx");
        crate::stream::set_test_write_buffer_bytes(Some(4096));
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let batches = odd_batches(9, 23, 'c');
        let st = reap(fork_writer(h, batches.clone()));
        assert!(libc::WIFEXITED(st) && libc::WEXITSTATUS(st) == 0, "writer status {st}");
        shared_close_handle(h).unwrap();
        crate::stream::set_test_write_buffer_bytes(None);
        assert_eq!(read_strs(&path), batches.concat());
    }

    // SLOT-11: writes from two processes interleave only between writes.
    #[test]
    fn writes_from_two_processes_keep_each_write_whole() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("two_writers");
        let path = dir.join("two.idx");
        crate::stream::set_test_write_buffer_bytes(Some(4096));
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let tagged = |tag: char| -> Vec<Vec<String>> {
            (0..30).map(|w| (0..17).map(|i| format!("{tag}{w}:{i}")).collect()).collect()
        };
        let a = fork_writer(h, tagged('a'));
        let b = fork_writer(h, tagged('b'));
        let (sa, sb) = (reap(a), reap(b));
        assert!(libc::WIFEXITED(sa) && libc::WEXITSTATUS(sa) == 0);
        assert!(libc::WIFEXITED(sb) && libc::WEXITSTATUS(sb) == 0);
        shared_close_handle(h).unwrap();
        crate::stream::set_test_write_buffer_bytes(None);
        let got = read_strs(&path);
        assert_eq!(got.len(), 2 * 30 * 17);
        for chunk in got.chunks(17) {
            let write = chunk[0].split(':').next().unwrap();
            for (i, e) in chunk.iter().enumerate() {
                assert_eq!(e, &format!("{write}:{i}"), "a write was split: {chunk:?}");
            }
        }
        for tag in ['a', 'b'] {
            let order: Vec<&str> = got.iter().step_by(17).map(|e| e.split(':').next().unwrap()).filter(|w| w.starts_with(tag)).collect();
            let want: Vec<String> = (0..30).map(|w| format!("{tag}{w}")).collect();
            assert_eq!(order, want, "one process's writes out of order");
        }
    }

    // SLOT-14: a writer killed inside the slot lock fails the stream; what
    // was queued before it is still written, and the footer says FAILED.
    #[test]
    fn a_writer_killed_inside_the_slot_lock_fails_the_stream() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("killed_inside");
        let path = dir.join("killed.idx");
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let first = vec!["kept".to_string()];
        write_one(h, &first, 0);
        shared_flush_buffer(h).unwrap();
        let (_gen, idx) = unpack_handle(h);
        let pid = unsafe { libc::fork() };
        if pid == 0 {
            let slot = slot_ref(idx).unwrap();
            std::mem::forget(SlotGuard::lock(slot));
            unsafe { libc::_exit(0) };
        }
        reap(pid);
        let closed = shared_close_handle(h);
        assert!(closed.is_err(), "closing a stream a writer died inside succeeded");
        let (mp, sz) = mmap_file_readonly(path.to_str().unwrap()).unwrap();
        let footer = try_read_footer(mp, sz);
        unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize) };
        let footer = footer.unwrap().expect("a final footer");
        assert_eq!(footer.footer_status, morloc_runtime_types::packet::FOOTER_STATUS_FAILED);
        assert_eq!(read_strs(&path), first);
    }

    // SLOT-15: once close returns the file is finished, so its path reopens
    // at once; writes through the closed handle fail.
    #[test]
    fn a_closed_stream_refuses_writes_and_its_path_reopens_at_once() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("reopen");
        let path = dir.join("reopen.idx");
        let p = path.to_str().unwrap();
        let h = shared_open_ostream_with_schema(p, "as").unwrap();
        write_one(h, &["x".to_string()], 0);
        shared_close_handle(h).unwrap();
        let list = parse_schema("as").unwrap();
        let v = crate::json::read_json_with_schema("[\"late\"]", &list).unwrap();
        assert!(shared_write_subpacket(h, CompressionLevel::from_u8(0).unwrap(), v).is_err());
        shm::shfree(v).unwrap();
        let began = std::time::Instant::now();
        let again = shared_append_to_path(p, "as").unwrap();
        assert!(began.elapsed() < std::time::Duration::from_millis(500));
        write_one(again, &["y".to_string()], 0);
        shared_close_handle(again).unwrap();
        assert_eq!(read_strs(&path), vec!["x".to_string(), "y".to_string()]);
    }

    // SLOT-15: a flush returns once its elements are in the file.
    #[test]
    fn a_flush_makes_its_elements_readable_from_the_open_file() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("flush_visible");
        let path = dir.join("flush.idx");
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let batch: Vec<String> = (0..5).map(|i| format!("f{i}")).collect();
        write_one(h, &batch, 3);
        shared_flush_buffer(h).unwrap();
        assert_eq!(read_strs(&path), batch);
        shared_close_handle(h).unwrap();
    }

    static SINK: Mutex<Vec<(i64, Vec<u8>)>> = Mutex::new(Vec::new());

    unsafe extern "C" fn recording_sink(handle: i64, rel: i64, len: u64, _err: *mut *mut libc::c_char) -> i32 {
        let abs = crate::shm::rel2abs(rel as RelPtr).unwrap();
        let bytes = std::slice::from_raw_parts(abs as *const u8, len as usize).to_vec();
        SINK.lock().unwrap().push((handle, bytes));
        crate::custody::SINK_WRITTEN
    }

    // SLOT-16: a stdout write that queued a batch returns once the batch has
    // reached the nexus, and the close's footer follows it there.
    #[test]
    fn a_stdout_write_returns_once_its_batches_reach_the_nexus() {
        let _shm = crate::own_test_registry();
        crate::custody::set_stdio_sink(recording_sink);
        crate::stream::set_test_write_buffer_bytes(Some(4096));
        let h = open_stdio(MLC_KIND_OSTREAM, STDIO_KIND_STDOUT, "as").unwrap();
        let mine = || SINK.lock().unwrap().iter().filter(|(x, _)| *x == h).count();
        let batch: Vec<String> = (0..400).map(|i| format!("{i}:{}", "s".repeat(20))).collect();
        write_one(h, &batch, 0);
        let after_write = mine();
        shared_close_handle(h).unwrap();
        let after_close = mine();
        crate::stream::set_test_write_buffer_bytes(None);
        assert!(after_write >= 1, "a write that filled buffers returned before any reached the nexus");
        assert_eq!(after_close, after_write + 2, "the close sent other than its tail and footer");
        assert!(open_stdio(MLC_KIND_OSTREAM, STDIO_KIND_STDOUT, "as").map(shared_close_handle).is_ok(), "the stdout claim was not released");
    }

    // SLOT-13, DAEMON-1: stopping a writer finishes its file with the given
    // status from what reached the queue.
    #[test]
    fn a_stopped_writer_finishes_its_file_with_the_given_status() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("stopped");
        let path = dir.join("stopped.idx");
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let first = vec!["queued".to_string()];
        write_one(h, &first, 0);
        shared_flush_buffer(h).unwrap();
        write_one(h, &["buffered".to_string()], 0);
        let (gen, idx) = unpack_handle(h);
        crate::custody::host_stop(idx, morloc_runtime_types::packet::FOOTER_STATUS_FAILED);
        release_closed_slot(idx, gen);
        let (mp, sz) = mmap_file_readonly(path.to_str().unwrap()).unwrap();
        let footer = try_read_footer(mp, sz);
        unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize) };
        assert_eq!(footer.unwrap().expect("a final footer").footer_status, morloc_runtime_types::packet::FOOTER_STATUS_FAILED);
        assert_eq!(read_strs(&path), first);
    }

    // A close after the writer stopped returns at once and frees the slot.
    #[test]
    fn a_close_after_its_writer_stopped_does_not_wait() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("after_stop");
        let path = dir.join("after_stop.idx");
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let (gen, idx) = unpack_handle(h);
        crate::custody::host_stop(idx, crate::custody::STATUS_DISCARD as u8);
        let began = std::time::Instant::now();
        let _ = shared_close_handle(h);
        assert!(began.elapsed() < std::time::Duration::from_secs(2), "a close waited on a stopped writer");
        assert!(!slot_generation_is(slot_ref(idx).unwrap(), gen), "the slot was not released");
    }

    // A slot released without a close ends its writer, which then leaves
    // the queue alone.
    #[test]
    fn a_writer_whose_slot_was_released_stops_reading_its_queue() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("released_under");
        let path = dir.join("released.idx");
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let (gen, idx) = unpack_handle(h);
        let slot = slot_ref(idx).unwrap();
        let q = slot_queue(slot).unwrap();
        release_closed_slot(idx, gen);
        let began = std::time::Instant::now();
        while q.has_consumer() && began.elapsed() < std::time::Duration::from_secs(5) {
            std::thread::sleep(std::time::Duration::from_millis(10));
        }
        assert!(!q.has_consumer(), "the writer kept reading a released slot's queue");
    }

    // A discarded stream writes what was queued, drops what was buffered,
    // and keeps the temporary footer: the "writer did not finish" signal.
    #[test]
    fn a_discarded_stream_keeps_what_was_queued_and_no_final_footer() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("discard");
        let path = dir.join("discard.idx");
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let first = vec!["queued".to_string()];
        write_one(h, &first, 0);
        shared_flush_buffer(h).unwrap();
        write_one(h, &["buffered".to_string()], 0);
        shared_discard_handle(h).unwrap();
        let (mp, sz) = mmap_file_readonly(path.to_str().unwrap()).unwrap();
        let footer = try_read_footer(mp, sz);
        unsafe { libc::munmap(mp as *mut libc::c_void, sz as usize) };
        assert!(footer.unwrap().is_none_or(|f| !f.final_footer), "a discarded stream has a final footer");
        assert_eq!(read_strs(&path), first);
    }

    fn write_one(h: i64, batch: &[String], level: u8) {
        let list = parse_schema("as").unwrap();
        let v = crate::json::read_json_with_schema(&serde_json::to_string(batch).unwrap(), &list).unwrap();
        shared_write_subpacket(h, CompressionLevel::from_u8(level).unwrap(), v).unwrap();
        shm::shfree(v).unwrap();
    }

    // A staged stream writes each batch as its own sub-packet. One too large
    // for a single frame is compressed on the spot, and must still land
    // after the batches compressing behind the writer.
    #[test]
    fn staged_batch_of_many_frames_follows_queued_batches() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("staged");
        let path = dir.join("staged.idx");
        crate::write_behind::set_test_depth(Some(8));
        let h = shared_open_ostream_with_schema(path.to_str().unwrap(), "as").unwrap();
        let (_gen, idx) = unpack_handle(h);
        let slot = slot_ref(idx).unwrap();
        slot.staged.set(1);
        let small = odd_batches(4, 20, 'u');
        let big: Vec<String> = (0..1100).map(|i| format!("{i}:{}", "B".repeat(16 * 1024))).collect();
        let mut want = Vec::new();
        for b in &small[..3] {
            write_one(h, b, 3);
            want.extend(b.iter().cloned());
        }
        write_one(h, &big, 3);
        want.extend(big.iter().cloned());
        write_one(h, &small[3], 3);
        want.extend(small[3].iter().cloned());
        shared_close_handle(h).unwrap();
        crate::write_behind::set_test_depth(None);
        assert_eq!(read_strs(&path), want);
    }

    // An element too large for the buffer is written on its own, between
    // what came before and after it, with batches still compressing.
    #[test]
    fn oversize_element_keeps_its_place_behind_pending_batches() {
        let _shm = crate::own_test_registry();
        let dir = test_dir("oversize");
        let path = dir.join("oversize.idx");
        let mut batches = odd_batches(20, 37, 'r');
        batches.insert(12, vec!["big".to_string(), "x".repeat(20_000), "after".to_string()]);
        write_strs(&path, &batches, 3, 4096, 8, &[]);
        assert_eq!(read_strs(&path), batches.concat());
    }
}

mod c_abi {

    #[no_mangle]
    pub extern "C" fn morloc_retire_blockers() -> i64 {
        super::morloc_retire_blockers()
    }
}

#[cfg(test)]
mod process_identity_tests {
    #[test]
    fn a_recorded_pid_is_this_process_only_with_its_start_stamp() {
        let me = std::process::id();
        let start = super::read_pid_start_time();
        assert!(super::is_this_process(me, start));
        assert!(super::is_this_process(me, 0));
        assert!(!super::is_this_process(me, start + 1));
        assert!(!super::is_this_process(me.wrapping_add(1), start));
    }
}

#[cfg(test)]
mod teardown_tests {
    #[test]
    fn a_registry_torn_down_is_not_created_again() {
        let _shm = crate::own_test_registry();
        assert!(crate::fork_policy::exits_cleanly_in_a_forked_child(|| {
            if super::registry_bootstrap().is_err() {
                return false;
            }
            super::registry_teardown();
            super::registry_bootstrap().is_err()
        }));
    }
}
