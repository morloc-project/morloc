//! Stream registry, IFile mmap management, and file-targeting pattern
//! walker support for the streaming I/O system.
//!
//! ## Scope (v1)
//!
//! - **Process-local registry**: one `Mutex<StreamRegistry>` per
//!   libmorloc.so instance. Each pool process has its own slot table.
//!   Cross-pool sharing of decompression work is deferred to v2 (kernel
//!   pagecache already shares the underlying file bytes across pools that
//!   mmap the same path).
//! - **Handle layout**: `(generation << 16) | slot`. Generation is per-slot
//!   and bumps by a random salt step on each close; double-close,
//!   foreign-int collision, and ABA reuse all return a clean error.
//! - **IFile only** in this stage: `mlc_open` for `MLC_KIND_ISTREAM` and
//!   `MLC_KIND_OSTREAM` returns a clear "not yet implemented" error and
//!   will be filled in by Stages 3 and 4.
//! - **Voidstar-only sub-packets**: `open_ifile` rejects a stream whose
//!   first sub-packet's format byte is not `PACKET_FORMAT_VOIDSTAR`.
//!
//! ## Future-extension hooks
//!
//! - `StreamEntry::cache` is the per-handle LRU; today it is allocated but
//!   not consulted (walker lands in task #13).
//! - `subpacket_index` is built greedily at open time (cheap for small N,
//!   acceptable for the million-sub-packet case at ~8 MiB). v2 may stream-
//!   populate it on demand.

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

/// State markers for a slot. `OPEN` slots are eligible for operations;
/// `CLOSED` is freed and re-allocatable; `INVALIDATED` is reserved for
/// future use (e.g. daemon-recovery invalidation).
// SLOT_STATE_FREE is implicit -- a Vec<Option<StreamEntry>> slot at None
// is free. The state byte on an OPEN entry exists so future v2 work
// (SHM-shared registry) can distinguish OPEN from INVALIDATED without
// dropping the entry; in v1 it's always OPEN for occupied slots.
const SLOT_STATE_OPEN: u8 = 1;

/// Default cache capacity (bytes) for decompressed sub-packets per
/// handle. Overridable via `MORLOC_IFILE_CACHE_BYTES`.
const DEFAULT_IFILE_CACHE_BYTES: u64 = 256 * 1024 * 1024;

// ── Types ─────────────────────────────────────────────────────────────────

/// Per-handle cache of decompressed (and relptr-adjusted) sub-packets in
/// SHM. v1 uses an approximate clock-hand LRU; eviction is `shfree` on
/// the entry's `shm_packet` block (the `BlockHeader` refcount handles
/// any still-in-flight reader via the existing `shincref` mechanism;
/// see plan A4). Walker landing in task #13 will populate and consult
/// this table.
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
    /// a no-op (the mmap is owned by the StreamEntry). For `Shm`, we
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

/// One slot in the per-process stream registry. Only `IFile` is fully
/// populated in v1; `IStream` and `OStream` extensions land in
/// Stages 3 and 4.
#[derive(Debug)]
pub struct StreamEntry {
    pub generation:        u64,
    pub kind:              u8,
    pub state:             u8,
    pub file_path:         String,
    pub schema_str:        String,
    /// Full schema of the file's logical value. For DATA_PACKET this
    /// is the file's payload schema directly (any voidstar type). For
    /// STREAM_PACKET this is `[elem_schema]` (the concatenated array
    /// view of all sub-packets). PatternStruct walkers navigate this
    /// type; bracket walkers consult `value_schema.parameters[0]`
    /// when this resolves to an Array.
    pub value_schema:      Schema,
    /// Element schema = `value_schema.parameters[0]` when the file's
    /// logical value is an Array; equal to `value_schema` otherwise
    /// (in which case bracket access on the IFile directly is a
    /// type error, but PatternStruct access can still work).
    pub elem_schema:       Schema,
    pub mmap_ptr:          AbsPtr,   // null for stdin/stdout/stderr
    pub mmap_size:         u64,
    pub subpacket_index:   Vec<u64>, // file byte-offsets, one per sub-packet
    /// Cumulative element count BEFORE each sub-packet. Length equals
    /// `subpacket_index.len() + 1`; the last entry is the grand total
    /// element count. Built lazily on first random-access query
    /// (BracketIndex / BracketSlice). For a file with no final footer
    /// the build cost is one `pread(16 B)`-equivalent slice read per
    /// sub-packet against the mmap'd region (~1 page-fault per
    /// sub-packet on cold pagecache).
    pub subpacket_elem_cum: Option<Vec<u64>>,
    pub element_count:     u64,
    pub diag:              Option<StreamDiag>,
    pub final_footer:      bool,     // true if METADATA_TYPE_FOOTER_FINAL present
    pub cache:             StreamCache,
}

impl Drop for StreamEntry {
    fn drop(&mut self) {
        // Best-effort cleanup if a slot is dropped without going through
        // close_handle (shouldn't happen, but defends against bugs).
        if !self.mmap_ptr.is_null() && self.mmap_size > 0 {
            // SAFETY: mmap_ptr was returned by libc::mmap with
            // mmap_size, and we are the unique owner of this slot.
            unsafe {
                libc::munmap(self.mmap_ptr as *mut libc::c_void, self.mmap_size as usize);
            }
        }
        // Evict any cached sub-packets; shfree decrements the per-block
        // refcount so still-held SHM relptrs from earlier walkers survive.
        for entry in self.cache.entries.drain(..) {
            let _ = crate::shm::shfree(entry.shm_packet);
        }
    }
}

// ── Registry ──────────────────────────────────────────────────────────────

struct StreamRegistry {
    slots: Vec<Option<StreamEntry>>,
    /// Per-process random salt used to perturb generation increments.
    /// Mixing this in makes adversarial-int handle collision require
    /// knowing the salt, not just guessing low bits.
    gen_salt: u64,
}

// SAFETY: The registry's only !Send field is StreamEntry.mmap_ptr (a
// raw `*mut u8` returned by mmap) and CacheEntry.shm_packet (a raw
// `AbsPtr` to a refcounted SHM block). The mmap region is process-wide
// read-only; SHM blocks are protected by the existing refcount + the
// registry mutex serialises every access to the slot. The pointers
// are never dereferenced without going through the per-slot access
// gate.
unsafe impl Send for StreamRegistry {}
unsafe impl Sync for StreamRegistry {}

impl StreamRegistry {
    fn new() -> Self {
        // Slots are lazily Some'd on allocation. We size the Vec to
        // STREAM_SLOT_COUNT up front so slot-index arithmetic is direct.
        let mut slots = Vec::with_capacity(STREAM_SLOT_COUNT);
        for _ in 0..STREAM_SLOT_COUNT {
            slots.push(None);
        }
        let gen_salt = read_random_salt();
        Self { slots, gen_salt }
    }
}

fn read_random_salt() -> u64 {
    // Cheap: read 8 bytes of /dev/urandom; fall back to a time-mixed
    // value if that fails. The salt only needs to be unpredictable to
    // a foreign-language adversary that might forge handle bits; it
    // does not need to be cryptographically strong.
    use std::io::Read;
    if let Ok(mut f) = std::fs::File::open("/dev/urandom") {
        let mut buf = [0u8; 8];
        if f.read_exact(&mut buf).is_ok() {
            // OR with 1 so the increment is always non-zero (else a
            // close+open cycle would never bump generation).
            return u64::from_le_bytes(buf) | 1;
        }
    }
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos() as u64)
        .unwrap_or(0xDEADBEEF);
    (now ^ 0xA5A5_A5A5_A5A5_A5A5) | 1
}

fn read_cache_cap_env() -> u64 {
    if let Ok(s) = std::env::var("MORLOC_IFILE_CACHE_BYTES") {
        if let Ok(n) = s.parse::<u64>() {
            return n;
        }
    }
    DEFAULT_IFILE_CACHE_BYTES
}

static REGISTRY: Mutex<Option<StreamRegistry>> = Mutex::new(None);

/// Lazily initialise the per-process registry on first use.
fn registry_with<R>(f: impl FnOnce(&mut StreamRegistry) -> R) -> R {
    let mut guard = REGISTRY.lock().expect("stream registry mutex poisoned");
    if guard.is_none() {
        *guard = Some(StreamRegistry::new());
    }
    f(guard.as_mut().expect("stream registry init"))
}

// ── Handle encoding ───────────────────────────────────────────────────────

fn make_handle(generation: u64, slot: usize) -> i64 {
    // High 48 bits: generation; low 16 bits: slot.
    let gen_bits = generation & 0x0000_FFFF_FFFF_FFFF;
    ((gen_bits << 16) | (slot as u64 & 0xFFFF)) as i64
}

fn decode_handle(handle: i64) -> (u64, usize) {
    let h = handle as u64;
    let slot = (h & 0xFFFF) as usize;
    let generation = h >> 16;
    (generation, slot)
}

// ── Slot allocation / free ────────────────────────────────────────────────

fn allocate_slot(
    reg: &mut StreamRegistry,
    entry_init: impl FnOnce(/*generation*/ u64) -> StreamEntry,
) -> Result<i64, MorlocError> {
    // Random-probe scan for a free slot. Random starting offset spreads
    // contention away from a hot prefix and makes successive opens land
    // in different cache lines.
    let start = {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(0);
        (now ^ reg.gen_salt) as usize & (STREAM_SLOT_COUNT - 1)
    };
    for off in 0..STREAM_SLOT_COUNT {
        let idx = (start + off) & (STREAM_SLOT_COUNT - 1);
        if reg.slots[idx].is_none() {
            // First-time init: generation starts at 1 (never 0; 0 is a
            // valid "default-initialised int" and we want generation
            // mismatch on such an int).
            let generation = 1u64;
            let entry = entry_init(generation);
            reg.slots[idx] = Some(entry);
            return Ok(make_handle(generation, idx));
        }
    }
    Err(MorlocError::Other(format!(
        "stream registry exhausted ({} slots all in use)", STREAM_SLOT_COUNT
    )))
}

/// Mark a slot free, bumping its generation by the per-process salt so
/// the next allocation in this slot produces a different handle ID and
/// any held stale handle fails the generation check.
fn release_slot(reg: &mut StreamRegistry, slot: usize) -> Option<u64> {
    let entry = reg.slots[slot].take()?;
    // Compute the next generation seed (held only for diagnostics; the
    // ALLOCATION above starts at 1, but we mix the salt in here so an
    // adversarial caller can't predict the next handle from the
    // previous one. The fresh entry's effective generation is the seed
    // we'll reuse when this slot is next re-allocated.)
    let new_gen = entry.generation.wrapping_add(reg.gen_salt) | 1;
    drop(entry); // explicit; runs Drop (mmun map, cache shfree).
    Some(new_gen)
}

// ── Resolve a handle (with generation re-check) ───────────────────────────

/// Run `f` against the StreamEntry behind `handle`. Returns a clean
/// generation-mismatch error if the slot has been closed and reused, or
/// if the handle was forged.
///
/// Holds the registry lock for the duration of `f`. In v1 (process-
/// local registry) the cost is acceptable; future v2 SHM-shared variant
/// will switch to a per-slot lock.
fn with_entry<R>(
    handle: i64,
    f: impl FnOnce(&mut StreamEntry) -> Result<R, MorlocError>,
) -> Result<R, MorlocError> {
    if handle < 0 {
        return Err(MorlocError::Other(format!("invalid handle: {}", handle)));
    }
    let (gen_claim, slot) = decode_handle(handle);
    if slot >= STREAM_SLOT_COUNT {
        return Err(MorlocError::Other(format!(
            "handle slot {} out of range", slot
        )));
    }
    registry_with(|reg| {
        match reg.slots[slot].as_mut() {
            Some(entry) if entry.generation == gen_claim
                && entry.state == SLOT_STATE_OPEN => f(entry),
            Some(_) => Err(MorlocError::Other(
                "stream handle stale: generation mismatch (was the handle reused after close?)"
                    .into(),
            )),
            None => Err(MorlocError::Other(
                "stream handle invalid: slot is free (handle never opened or already closed)"
                    .into(),
            )),
        }
    })
}

// ── Public API: open / close / fschema ────────────────────────────────────

/// Open a STREAM_PACKET or DATA_PACKET file as a random-access IFile.
/// Returns the morloc-level Int handle on success.
///
/// DATA_PACKET files are treated as a degenerate STREAM with one
/// implicit "sub-packet" -- the file's own packet IS the sub-packet
/// the walker locates. Reading `.[i] f` from a DATA_PACKET works
/// zero-copy via the file resolver; compressed DATA_PACKET files are
/// rejected (decompressing the whole payload defeats IFile's purpose).
pub fn open_ifile(path: &str) -> Result<i64, MorlocError> {
    // 1. mmap the file PROT_READ + MADV_RANDOM.
    let (mmap_ptr, mmap_size) = mmap_file_readonly(path)?;

    // 2. Peek the first packet header to dispatch between STREAM and
    //    DATA. Cleanup on early error: munmap before returning so we
    //    don't leak file mappings to libc.
    if mmap_size < 32 {
        unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
        return Err(MorlocError::Packet(
            "file too short for a packet header".into(),
        ));
    }
    // SAFETY: bounds verified.
    let hdr_bytes = unsafe {
        std::slice::from_raw_parts(mmap_ptr as *const u8, 32)
    };
    let outer_header = PacketHeader::from_bytes(hdr_bytes.try_into().unwrap())?;

    // `is_data_packet` distinguishes the DATA-PACKET path (full payload
    // schema in metadata; one virtual sub-packet) from the STREAM-PACKET
    // path (stream header schema is the element schema, value type is
    // a List of those elements concatenated across sub-packets).
    let is_data_packet = outer_header.is_data();
    let (schema_str, subpacket_index, element_count, diag, final_footer) =
    if is_data_packet {
        // Single DATA_PACKET file. The whole file's payload IS the
        // value of type `value_schema`. We synthesise a one-entry
        // sub-packet index pointing at offset 0 so the rest of the
        // IFile walker works without special-casing.
        let result = open_data_packet(path, mmap_ptr, mmap_size);
        match result {
            Ok((schema, index, count)) => (schema, index, count, None, false),
            Err(e) => {
                unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
                return Err(e);
            }
        }
    } else if outer_header.is_stream() {
        // STREAM_PACKET: existing multi-sub-packet handling.
        let StreamHeader { schema: schema_str, body_start } =
            match parse_stream_header(mmap_ptr, mmap_size) {
                Ok(h) => h,
                Err(e) => {
                    unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
                    return Err(e);
                }
            };
        // Reject non-voidstar sub-packets.
        if body_start < mmap_size {
            let fmt = match read_subpacket_format(mmap_ptr, mmap_size, body_start) {
                Ok(f) => f,
                Err(e) => {
                    unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
                    return Err(e);
                }
            };
            if fmt != PACKET_FORMAT_VOIDSTAR {
                unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
                return Err(MorlocError::Other(format!(
                    "file '{}' has {}-format sub-packets; only voidstar is supported for IFile (reopen as IStream or use @load)",
                    path, packet_format_name(fmt)
                )));
            }
        }
        let (subpacket_index, element_count, diag, final_footer) =
            match try_read_footer(mmap_ptr, mmap_size) {
                Ok(Some(parsed)) => (
                    parsed.subpacket_index,
                    parsed.element_count,
                    parsed.diag,
                    parsed.final_footer,
                ),
                Ok(None) | Err(_) => {
                    let scanned = match forward_scan_subpackets(mmap_ptr, mmap_size, body_start) {
                        Ok(s) => s,
                        Err(e) => {
                            unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
                            return Err(e);
                        }
                    };
                    (scanned.subpacket_index, scanned.element_count, None, false)
                }
            };
        (schema_str, subpacket_index, element_count, diag, final_footer)
    } else {
        unsafe { libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize); }
        return Err(MorlocError::Packet(format!(
            "file '{}' is neither a STREAM_PACKET nor a DATA_PACKET (cmd_type = {})",
            path,
            unsafe { outer_header.command.cmd_type.cmd_type }
        )));
    };

    // 3. Parse schemas once; reused on every random access. For
    //    DATA_PACKET, schema_str is the full payload schema (the
    //    value's type). For STREAM_PACKET, schema_str is the
    //    element type and value_schema is `Array(elem_schema)`.
    let parsed_schema = parse_schema(&schema_str).map_err(|e| {
        unsafe {
            libc::munmap(mmap_ptr as *mut libc::c_void, mmap_size as usize);
        }
        MorlocError::Schema(format!(
            "file '{}' has unparseable schema '{}': {}",
            path, schema_str, e
        ))
    })?;
    let (value_schema, elem_schema) = if is_data_packet {
        // value_schema = full payload schema; elem_schema =
        // value_schema.parameters[0] when value is an Array, else the
        // value type itself (used by walkers that don't depend on
        // arrayness; bracket walkers check at runtime).
        let elem = if parsed_schema.serial_type == SerialType::Array
            && !parsed_schema.parameters.is_empty()
        {
            parsed_schema.parameters[0].clone()
        } else {
            parsed_schema.clone()
        };
        (parsed_schema.clone(), elem)
    } else {
        // STREAM_PACKET: schema_str is the element type. Value type
        // is List of those elements concatenated across sub-packets.
        (array_schema(&parsed_schema), parsed_schema.clone())
    };

    // 6. Allocate a slot and return the handle. Re-read the cache
    //    cap from the env each open so tests can adjust between
    //    handles without process restart.
    let cap_bytes = read_cache_cap_env();
    let handle = registry_with(|reg| {
        allocate_slot(reg, |generation| StreamEntry {
            generation,
            kind: MLC_KIND_IFILE,
            state: SLOT_STATE_OPEN,
            file_path: path.to_string(),
            schema_str: schema_str.clone(),
            value_schema: value_schema.clone(),
            elem_schema: elem_schema.clone(),
            mmap_ptr,
            mmap_size,
            subpacket_index,
            subpacket_elem_cum: None,
            element_count,
            diag,
            final_footer,
            cache: StreamCache::new(cap_bytes),
        })
    })?;

    // 6. Record in the active eval_arena so a Python exception unwinding
    //    out of the pool auto-closes us at scope exit.
    crate::eval_arena::record_slot_if_active(handle);

    Ok(handle)
}

/// Close any open handle. Bumps the slot's generation; subsequent
/// operations on the same Int return a clean generation-mismatch error.
pub fn close_handle(handle: i64) -> Result<(), MorlocError> {
    if handle < 0 {
        return Err(MorlocError::Other(format!("invalid handle: {}", handle)));
    }
    let (gen_claim, slot) = decode_handle(handle);
    if slot >= STREAM_SLOT_COUNT {
        return Err(MorlocError::Other(format!(
            "handle slot {} out of range", slot
        )));
    }
    let result = registry_with(|reg| {
        match reg.slots[slot].as_ref() {
            Some(entry) if entry.generation == gen_claim => {
                release_slot(reg, slot);
                Ok(())
            }
            Some(_) => Err(MorlocError::Other(
                "close_handle: generation mismatch (handle already closed and reused?)".into(),
            )),
            None => Err(MorlocError::Other(
                "close_handle: slot is already free (double-close?)".into(),
            )),
        }
    });
    // Forget the handle from the active arena AFTER the close so an
    // exception during close still produces a recovery attempt at arena
    // drop. Forget is a no-op if not tracked.
    crate::eval_arena::forget_slot_if_active(handle);
    result
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
    with_entry(handle, |e| Ok(e.kind))
}

/// Read the file path bound to an open handle. The cross-pool wire
/// codec for IFile values ships this path so the receiving pool can
/// `open_dispatch(path, kind)` and bind a fresh local handle of its
/// own; each pool keeps an independent fd + mmap + slot, and the
/// receiver's own `eval_arena` is what closes the new handle on scope
/// exit. Symmetric path on the receive side: `open_dispatch` after
/// reading `(kind, path)` off the wire.
pub fn handle_path(handle: i64) -> Result<String, MorlocError> {
    with_entry(handle, |e| Ok(e.file_path.clone()))
}

/// Resolve a handle to its open StreamEntry under an already-held registry
/// lock. Shared by the batch IFile-array entry points so they can amortise
/// the lock across N handles instead of acquiring it per element.
fn resolve_open_slot<'a>(
    reg: &'a StreamRegistry,
    handle: i64,
    idx: usize,
) -> Result<&'a StreamEntry, MorlocError> {
    if handle < 0 {
        return Err(MorlocError::Other(format!(
            "handle at index {}: invalid handle {}", idx, handle
        )));
    }
    let (gen_claim, slot) = decode_handle(handle);
    if slot >= STREAM_SLOT_COUNT {
        return Err(MorlocError::Other(format!(
            "handle at index {}: slot {} out of range", idx, slot
        )));
    }
    match reg.slots[slot].as_ref() {
        Some(e) if e.generation == gen_claim && e.state == SLOT_STATE_OPEN => Ok(e),
        Some(_) => Err(MorlocError::Other(format!(
            "handle at index {}: generation mismatch (stale handle?)", idx
        ))),
        None => Err(MorlocError::Other(format!(
            "handle at index {}: slot is free (never opened or already closed)", idx
        ))),
    }
}

/// Batched length lookup for an [IFile a] sizing pass: one registry
/// acquire, N path-length reads. Returns the sum. When `out_lens` is
/// `Some`, the per-handle lengths are written there too; callers that
/// only need the sum pass `None`. No String clone -- we only need
/// `.len()`.
pub fn handles_path_lens(
    handles: &[i64],
    mut out_lens: Option<&mut [i64]>,
) -> Result<u64, MorlocError> {
    if let Some(ref outs) = out_lens {
        debug_assert_eq!(handles.len(), outs.len());
    }
    registry_with(|reg| {
        let mut sum: u64 = 0;
        for (i, &h) in handles.iter().enumerate() {
            let e = resolve_open_slot(reg, h, i)?;
            let len = e.file_path.len() as i64;
            if let Some(ref mut outs) = out_lens {
                outs[i] = len;
            }
            sum += len as u64;
        }
        Ok(sum)
    })
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
    use crate::shm_types::Array;
    registry_with(|reg| {
        for (i, &h) in handles.iter().enumerate() {
            let e = resolve_open_slot(reg, h, i)?;
            let bytes = e.file_path.as_bytes();
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
    })
}

/// Read a copy of the schema string stored for an open handle. Used by
/// the runtime schema-recheck-on-access defense.
pub fn handle_schema(handle: i64) -> Result<String, MorlocError> {
    with_entry(handle, |e| Ok(e.schema_str.clone()))
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
    with_entry(handle, |e| {
        if e.kind != MLC_KIND_IFILE {
            return Err(MorlocError::Other(format!(
                "length on non-IFile handle (kind = {})",
                handle_kind_name(e.kind),
            )));
        }
        if e.value_schema.serial_type != SerialType::Array {
            return Err(MorlocError::Other(format!(
                "length is undefined for IFile of non-list value type (schema {:?}); \
                 length only applies to lists",
                e.value_schema.serial_type,
            )));
        }
        Ok(e.element_count)
    })
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
    // Root-only brackets keep the dedicated fast paths -- they carry
    // the sub-packet selection logic used by STREAM_PACKET IFiles
    // (looking up which sub-packet holds element K, decompressing it
    // if needed). The general walker only navigates within a single
    // sub-packet's payload.
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
        return ifile_bracket_index(handle, args[0].value);
    }
    // Bracket slice at the path root, with or without a static
    // field/key tail. Chain fusion: `.[:].0`, `.[:].foo`, `.[:].0.bar`
    // etc. are routed through the slice walker with a per-element
    // projection offset, so the walker only deep-copies the projected
    // sub-field rather than each full record. The plain `.[:]` case
    // is just the empty tail.
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
            // If the tail contains anything other than Field/Key, fall
            // through to ifile_general -- it handles bracket-in-tail
            // via the per-sub-packet walker.
            if parsed.is_none() {
                return ifile_general(handle, path, args);
            }
            parsed.unwrap()
        };
        return ifile_bracket_slice_with_tail(
            handle, opt(&args[0]), opt(&args[1]), opt(&args[2]), &tail_steps,
        );
    }
    // General path: anything else (field chains, groups, mixed
    // field+bracket inside groups). The walker validates arg counts
    // internally as it consumes them in DFS order.
    ifile_general(handle, path, args)
}

/// General-purpose dispatch for any walk path that isn't a root-only
/// bracket. Parses the path, opens sub-packet 0 (DATA_PACKET case),
/// and runs the recursive walker.
fn ifile_general(
    handle: i64,
    path: &str,
    args: &[crate::intrinsics::IFileWalkArg],
) -> Result<AbsPtr, MorlocError> {
    let steps = parse_walk_path(path)?;
    with_entry(handle, |entry| {
        if entry.kind != MLC_KIND_IFILE {
            return Err(MorlocError::Other(format!(
                "field access on non-IFile handle (kind = {})",
                handle_kind_name(entry.kind),
            )));
        }
        if entry.subpacket_index.is_empty() {
            return Err(MorlocError::Other(
                "IFile has no sub-packets (empty file?)".into(),
            ));
        }
        let src = materialize_subpacket(entry, 0)?;
        let result = walk_into_fresh(&entry.value_schema, &src, &steps, args);
        src.release();
        result
    })
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
fn ensure_elem_index(entry: &mut StreamEntry) -> Result<(), MorlocError> {
    if entry.subpacket_elem_cum.is_some() {
        return Ok(());
    }
    let n = entry.subpacket_index.len();
    let mut cum = Vec::with_capacity(n + 1);
    cum.push(0u64);
    for &subpacket_off in &entry.subpacket_index {
        let sz = read_subpacket_element_count(
            entry.mmap_ptr,
            entry.mmap_size,
            subpacket_off,
            &entry.elem_schema,
        )?;
        let last = *cum.last().unwrap();
        cum.push(last.saturating_add(sz));
    }
    entry.subpacket_elem_cum = Some(cum);
    Ok(())
}

/// Read the element count from a sub-packet's payload header. The
/// payload of each sub-packet is a voidstar Array; the first 16 bytes
/// are `{ size: usize, data: RelPtr }`.
///
/// For compressed sub-packets the size is encoded inside the
/// compressed payload; in v1 we decompress just enough to read the
/// first frame's first 16 bytes via the existing per-packet
/// decompressor. v1.0 takes the simpler path: refuse to build the
/// element-count index against a stream with any compressed sub-packet
/// until decompression-on-demand is wired through here (task #13b).
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
            // Decompress just to read the 16-byte Array header. For v1
            // simplicity we decompress the whole sub-packet (the
            // compression module's decompress_packet expects a full
            // packet, not a payload-only slice, so reconstruct it).
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
    entry: &StreamEntry,
    subpacket_off: u64,
) -> Result<(PacketHeader, u64 /*payload_off*/, u64 /*payload_len*/, u64 /*meta_off*/), MorlocError> {
    if subpacket_off + 32 > entry.mmap_size {
        return Err(MorlocError::Packet(
            "sub-packet header past EOF".into(),
        ));
    }
    // SAFETY: bounds verified.
    let hdr_bytes = unsafe {
        std::slice::from_raw_parts(
            (entry.mmap_ptr as *const u8).add(subpacket_off as usize),
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
    if payload_off + payload_len > entry.mmap_size {
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
    entry: &StreamEntry,
    sub_k: usize,
) -> Result<SubpacketSrc, MorlocError> {
    if sub_k >= entry.subpacket_index.len() {
        return Err(MorlocError::Other(format!(
            "sub-packet index {} out of range (have {})",
            sub_k, entry.subpacket_index.len(),
        )));
    }
    let subpacket_off = entry.subpacket_index[sub_k];
    let (header, payload_off, payload_len, meta_off) =
        read_subpacket_header(entry, subpacket_off)?;
    let data = unsafe { header.command.data };

    // Fast path: uncompressed. The walker reads directly from the
    // mmap'd region; no SHM allocation, no copy. The kernel page-
    // cache shares pages across pools opening the same path.
    if data.compression == PACKET_COMPRESSION_NONE {
        let arr_base = unsafe {
            (entry.mmap_ptr as *const u8).add(payload_off as usize) as AbsPtr
        };
        // Parse the producer's Layer-3 vol_idx hint from the header +
        // metadata bytes (zero-allocation; the slices view directly
        // into the mmap). Hint is only used by the contiguous-slice
        // bulk-copy path; other walks ignore it via the file resolver.
        let hint_bytes_len = 32 + header.offset as usize;
        let mut hint_buf = Vec::with_capacity(hint_bytes_len);
        unsafe {
            hint_buf.extend_from_slice(std::slice::from_raw_parts(
                (entry.mmap_ptr as *const u8).add(subpacket_off as usize),
                hint_bytes_len,
            ));
        }
        let vol_idx_hint = morloc_runtime_types::packet::read_vol_index_from_meta(&hint_buf)
            .ok()
            .flatten()
            .unwrap_or(0);
        return Ok(SubpacketSrc::File {
            arr_base,
            payload_base: arr_base,
            payload_len,
            vol_idx_hint,
        });
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
            (entry.mmap_ptr as *const u8).add(subpacket_off as usize),
            32,
        )
    };
    let meta = unsafe {
        std::slice::from_raw_parts(
            (entry.mmap_ptr as *const u8).add(meta_off as usize),
            header.offset as usize,
        )
    };
    let payload = unsafe {
        std::slice::from_raw_parts(
            (entry.mmap_ptr as *const u8).add(payload_off as usize),
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
    let arr_schema = array_schema(&entry.elem_schema);
    if let Err(e) = voidstar::adjust_relptrs(base, &arr_schema, base_rel) {
        let _ = shm::shfree(base);
        return Err(e);
    }
    Ok(SubpacketSrc::Shm { arr_base: base })
}

/// Resolve a global element index, normalising negatives Python-style.
/// Returns `(sub_packet_k, local_idx)`.
fn resolve_global_index(
    entry: &mut StreamEntry,
    requested: i64,
) -> Result<(usize, u64), MorlocError> {
    ensure_elem_index(entry)?;
    let cum = entry
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
    let local = idx_u - cum[k];
    Ok((k, local))
}

/// Implementation of `.[i] f` on an IFile handle. Returns an AbsPtr to
/// a freshly-allocated SHM block of `elem_schema.width` bytes holding
/// the materialized element (with any sub-allocations also in SHM).
pub fn ifile_bracket_index(handle: i64, index: i64) -> Result<AbsPtr, MorlocError> {
    with_entry(handle, |entry| {
        if entry.kind != MLC_KIND_IFILE {
            return Err(MorlocError::Other(format!(
                "bracket index on non-IFile handle (kind = {})",
                handle_kind_name(entry.kind),
            )));
        }
        let (sub_k, local_idx) = resolve_global_index(entry, index)?;
        let src = cache_get_or_materialize(entry, sub_k)?;
        let result = ifile_extract_element(&entry.elem_schema, &src, local_idx);
        // For File variant this is a no-op; for Shm variant it
        // decrements the caller's ref (the cache keeps its own).
        src.release();
        result
    })
}

/// Acquire a source descriptor for sub-packet `sub_k`. Uncompressed
/// sub-packets are zero-copy (`File` variant); compressed sub-packets
/// are decompressed into SHM and cached (`Shm` variant) with the
/// `shincref` discipline so eviction of an entry that another caller
/// still holds is benign.
///
/// Caller drops their source descriptor via `SubpacketSrc::release()`.
fn cache_get_or_materialize(
    entry: &mut StreamEntry,
    sub_k: usize,
) -> Result<SubpacketSrc, MorlocError> {
    // Hit path (compressed only -- uncompressed sub-packets bypass
    // the cache since the mmap'd region serves the walker directly).
    for ce in entry.cache.entries.iter_mut() {
        if ce.subpacket_idx == sub_k as u64 {
            ce.clock_bit = 1;
            shm::shincref(ce.shm_packet)?;
            return Ok(SubpacketSrc::Shm { arr_base: ce.shm_packet });
        }
    }
    // Miss: materialise. For uncompressed sub-packets this returns a
    // File descriptor pointing at the mmap (no SHM allocation); for
    // compressed sub-packets a fresh SHM block (refcount = 1).
    let src = materialize_subpacket(entry, sub_k)?;

    // Cache compressed (Shm) sub-packets; uncompressed (File) ones
    // don't need our cache -- the kernel pagecache handles them.
    if let SubpacketSrc::Shm { arr_base } = src {
        let size_bytes = unsafe { shm::shm_block_size(arr_base).unwrap_or(0) } as u64;
        cache_make_room_for(&mut entry.cache, size_bytes);
        // Install: shincref so the cache owns one ref and the caller
        // owns the other. Both decrement independently on shfree.
        shm::shincref(arr_base)?;
        entry.cache.entries.push(CacheEntry {
            subpacket_idx: sub_k as u64,
            shm_packet: arr_base,
            size_bytes,
            clock_bit: 1,
        });
        entry.cache.current_bytes = entry.cache.current_bytes.saturating_add(size_bytes);
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
    ifile_bracket_slice_with_tail(handle, start, stop, step, &[])
}

/// Bracket-slice + statically-resolved field projection. The tail is
/// a sequence of Field/Key steps applied per element BEFORE the deep
/// copy, so the walker only materialises the projected sub-field
/// rather than the full element. Chain-fused at codegen time when the
/// caller has `.[:].field` (or longer field chains); the empty tail
/// degenerates to the plain slice path.
fn ifile_bracket_slice_with_tail(
    handle: i64,
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

    let mut work: SliceWork = with_entry(handle, |entry| {
        if entry.kind != MLC_KIND_IFILE {
            return Err(MorlocError::Other(format!(
                "bracket slice on non-IFile handle (kind = {})",
                handle_kind_name(entry.kind),
            )));
        }
        ensure_elem_index(entry)?;
        let cum = entry
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

        // Build the (sub_k, local) plan via binary search per index.
        // This is O(|slice| * log N_sub) which dominates the in-sub-
        // packet linear scan you'd get if you walked sub-packets;
        // typical slices touch 1-2 sub-packets so the log term is
        // ~1-2 lookups per element.
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
        // Resolve the static projection inside each element.
        let (proj_offset, proj_schema) =
            navigate_static_field_offset(&entry.elem_schema, tail_steps)?;
        Ok(SliceWork {
            proj_offset,
            proj_schema,
            elem_width: entry.elem_schema.width,
            plan,
            materialised: std::collections::BTreeMap::new(),
        })
    })?;

    let n_out = work.plan.len();

    // Phase 2: allocate the output Array structure (16 B) + the
    // element-data buffer (n_out * elem_width bytes). The Array struct
    // is a separate SHM block; its `data` relptr points to the buffer.
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
        let src = with_entry(handle, |entry| {
            cache_get_or_materialize(entry, sub_k)
        })?;
        let arr_base = src.arr_base();
        let arr = unsafe { &*(arr_base as *const shm_types_crate::Array) };
        let arr_size = arr.size as u64;
        if let SubpacketSrc::File { payload_base, payload_len, vol_idx_hint, .. } = src {
            let resolver = make_file_resolver(payload_base, payload_len);
            let arr_data = resolver(arr.data)?;
            // Snapshot the entry's elem_schema so we don't hold the
            // registry lock across the shmalloc + memcpys.
            let elem_schema = with_entry(handle, |e| Ok(e.elem_schema.clone()))?;
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
            handle, &mut work.plan, &mut work.materialised,
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

    // Phase 3: for each output slot, ensure the source sub-packet is
    // located, then deep_copy the projected sub-field into the output
    // buffer slot. Sub-packets are cached by sub_k for the duration of
    // the walk so a slice spanning many elements within one sub-packet
    // pays only one location cost.
    for (out_i, &(sub_k, local_idx)) in work.plan.iter().enumerate() {
        if !work.materialised.contains_key(&sub_k) {
            let src = with_entry(handle, |entry| {
                cache_get_or_materialize(entry, sub_k)
            })?;
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

    // Phase 4: release our source descriptors and fill in the output
    // Array struct. The cache (for Shm variants) keeps its own refs;
    // File variants are no-op releases.
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
    handle: i64,
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
                let src = with_entry(handle, |entry| {
                    cache_get_or_materialize(entry, sub_k)
                })?;
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

// ── ISTREAM / OSTREAM stubs ───────────────────────────────────────────────

pub fn open_istream(_path: &str) -> Result<i64, MorlocError> {
    Err(MorlocError::Other(
        "open IStream: not yet implemented".into(),
    ))
}

pub fn open_ostream(_path: &str) -> Result<i64, MorlocError> {
    Err(MorlocError::Other(
        "open OStream: not yet implemented".into(),
    ))
}

/// Dispatch entry point used by the FFI shim in `intrinsics.rs`.
pub fn open_dispatch(path: &str, kind: u8) -> Result<i64, MorlocError> {
    match kind {
        MLC_KIND_IFILE => open_ifile(path),
        MLC_KIND_ISTREAM => open_istream(path),
        MLC_KIND_OSTREAM => open_ostream(path),
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

        let handle = open_ifile(path.to_str().unwrap()).unwrap();
        assert!(handle > 0);
        let kind = handle_kind(handle).unwrap();
        assert_eq!(kind, MLC_KIND_IFILE);
        close_handle(handle).unwrap();

        // Re-close is an error.
        assert!(close_handle(handle).is_err());

        let _ = std::fs::remove_file(&path);
    }

    #[test]
    fn handle_encoding_roundtrip() {
        let h = make_handle(0xDEAD_BEEF_CAFE, 0x1234);
        let (g, s) = decode_handle(h);
        assert_eq!(g, 0xDEAD_BEEF_CAFE);
        assert_eq!(s, 0x1234);
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
