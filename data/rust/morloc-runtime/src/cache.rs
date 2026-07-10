//! File-based packet caching with xxHash keys.
//! Replaces cache.c.

use std::ffi::{c_char, c_void, CStr, CString};
use std::path::PathBuf;
use std::ptr;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;

use crate::cschema::CSchema;
use crate::error::{clear_errmsg, set_errmsg, MorlocError};
use crate::hash;
use crate::shm;

extern "C" {
    fn parse_schema(schema_str: *const c_char, errmsg: *mut *mut c_char) -> *mut CSchema;
    fn free_schema(schema: *mut CSchema);
    fn write_atomic(
        filename: *const c_char,
        data: *const u8,
        size: usize,
        errmsg: *mut *mut c_char,
    ) -> i32;
    fn read_binary_file(
        filename: *const c_char,
        file_size: *mut usize,
        errmsg: *mut *mut c_char,
    ) -> *mut u8;
    fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize;
    fn get_morloc_data_packet_value(
        data: *const u8,
        schema: *const CSchema,
        errmsg: *mut *mut c_char,
    ) -> *mut u8;
}

/// Resolve the on-disk path `<cache_dir>/<label>/<key:016x>.dat`,
/// creating the label directory if needed. Returns a heap C-string the
/// caller frees with `libc::free`, or null with `*errmsg` set on
/// failure.
unsafe fn resolve_cache_filename(
    key: u64,
    label: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    let mut err: *mut c_char = ptr::null_mut();
    let cache_path_c = morloc_cache_path(label, &mut err);
    if cache_path_c.is_null() {
        if !err.is_null() { *errmsg = err; }
        return ptr::null_mut();
    }
    let filename = make_cache_filename(key, cache_path_c, &mut err);
    libc::free(cache_path_c as *mut c_void);
    if filename.is_null() {
        if !err.is_null() { *errmsg = err; }
        return ptr::null_mut();
    }
    filename
}

/// Resolve the shared content-addressed data directory
/// `<cache_base>/data/`, creating it on demand. Heap C-string the
/// caller frees with `libc::free`, or null with `*errmsg` set.
unsafe fn resolve_data_dir(errmsg: *mut *mut c_char) -> *mut c_char {
    let dir = cache_base().join("data");
    if std::fs::create_dir_all(&dir).is_err() {
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!(
                "failed to create cache data directory: {}",
                dir.display()
            )),
        );
        return ptr::null_mut();
    }
    match CString::new(dir.to_string_lossy().as_bytes()) {
        Ok(cs) => cs.into_raw(),
        Err(_) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other("cache data path contains an interior NUL".into()),
            );
            ptr::null_mut()
        }
    }
}

/// Resolve `<cache_base>/data/<data_hash:016x>.dat`.
unsafe fn resolve_data_filename(
    data_hash: u64,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    let dir = resolve_data_dir(errmsg);
    if dir.is_null() {
        return ptr::null_mut();
    }
    let mut err: *mut c_char = ptr::null_mut();
    let filename = make_cache_filename(data_hash, dir, &mut err);
    libc::free(dir as *mut c_void);
    if filename.is_null() {
        if !err.is_null() { *errmsg = err; }
        return ptr::null_mut();
    }
    filename
}

// ── Pool-source fingerprint (from MORLOC_POOL_HASH) ─────────────────────────

/// Parsed once on first call, cached for the lifetime of the process.
/// The pool source fingerprint is mixed into every cache key so editing
/// the morloc source (or any declared @hash-include@ file) invalidates
/// the cache. `0` is returned if the env var is absent or unparseable;
/// that means the runtime keeps working (cache keys still derive from
/// midx + args) but the source-edit-invalidation property is lost.
fn pool_hash() -> u64 {
    static CACHED: OnceLock<u64> = OnceLock::new();
    *CACHED.get_or_init(|| {
        std::env::var("MORLOC_POOL_HASH")
            .ok()
            .and_then(|s| u64::from_str_radix(s.trim(), 16).ok())
            .unwrap_or(0)
    })
}

/// C-ABI accessor for the pool's source fingerprint. Pool wrap code
/// calls this once at first cache lookup to mix into the cache key.
#[no_mangle]
pub extern "C" fn morloc_pool_hash() -> u64 {
    pool_hash()
}

// ── Cache base directory ──────────────────────────────────────────────────

/// Resolve the cache base directory. Precedence:
///
///   1. `MORLOC_CACHE_BASE` env var (explicit override; useful for
///      Docker bind mounts and shared filesystems where SLURM workers
///      need the same path).
///   2. `$XDG_CACHE_HOME/morloc/cache` if `XDG_CACHE_HOME` is set.
///   3. `~/.cache/morloc/cache` default.
///   4. `/tmp/morloc/cache` if `HOME` is also unset.
fn cache_base() -> PathBuf {
    static CACHED: OnceLock<PathBuf> = OnceLock::new();
    CACHED
        .get_or_init(|| {
            if let Ok(s) = std::env::var("MORLOC_CACHE_BASE") {
                if !s.is_empty() {
                    return PathBuf::from(s);
                }
            }
            if let Ok(s) = std::env::var("XDG_CACHE_HOME") {
                if !s.is_empty() {
                    return PathBuf::from(s).join("morloc/cache");
                }
            }
            if let Ok(home) = std::env::var("HOME") {
                if !home.is_empty() {
                    return PathBuf::from(home).join(".cache/morloc/cache");
                }
            }
            PathBuf::from("/tmp/morloc/cache")
        })
        .clone()
}

/// Resolve the per-label cache directory, creating it on demand. Returns
/// an absolute path string the caller frees via `libc::free`. NULL on
/// failure. A NULL or empty `label` resolves to the synthetic
/// `_unlabeled` subdirectory so legacy unlabeled callers (the SLURM
/// remote-call cache today) stay segregated from user-labeled entries.
#[no_mangle]
pub unsafe extern "C" fn morloc_cache_path(
    label: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let label_part = if label.is_null() {
        "_unlabeled".to_string()
    } else {
        match CStr::from_ptr(label).to_str() {
            Ok(s) if !s.is_empty() => s.to_string(),
            _ => "_unlabeled".to_string(),
        }
    };
    let dir = cache_base().join(label_part);
    if std::fs::create_dir_all(&dir).is_err() {
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!(
                "failed to create cache directory: {}",
                dir.display()
            )),
        );
        return ptr::null_mut();
    }
    match CString::new(dir.to_string_lossy().as_bytes()) {
        Ok(cs) => cs.into_raw(),
        Err(_) => {
            set_errmsg(
                errmsg,
                &MorlocError::Other("cache path contains an interior NUL".into()),
            );
            ptr::null_mut()
        }
    }
}

// ── Hit/miss counters ──────────────────────────────────────────────────────

static CACHE_HITS: AtomicU64 = AtomicU64::new(0);
static CACHE_MISSES: AtomicU64 = AtomicU64::new(0);
static CACHE_STORES: AtomicU64 = AtomicU64::new(0);

/// Increment the cache hit counter. Called by the labeled-call wrap on
/// every successful lookup.
#[no_mangle]
pub extern "C" fn morloc_cache_record_hit() {
    CACHE_HITS.fetch_add(1, Ordering::Relaxed);
}

/// Increment the cache miss counter. Called by the labeled-call wrap
/// before it falls through to compute the value.
#[no_mangle]
pub extern "C" fn morloc_cache_record_miss() {
    CACHE_MISSES.fetch_add(1, Ordering::Relaxed);
}

/// Increment the cache store counter. Called by the labeled-call wrap
/// after it has written a freshly-computed value to disk.
#[no_mangle]
pub extern "C" fn morloc_cache_record_store() {
    CACHE_STORES.fetch_add(1, Ordering::Relaxed);
}

/// Read the (hits, misses, stores) counter triple. Consumed by the
/// nexus at exit to print the run summary.
#[no_mangle]
pub extern "C" fn morloc_cache_stats(
    hits_out: *mut u64,
    misses_out: *mut u64,
    stores_out: *mut u64,
) {
    if !hits_out.is_null() {
        unsafe { *hits_out = CACHE_HITS.load(Ordering::Relaxed) };
    }
    if !misses_out.is_null() {
        unsafe { *misses_out = CACHE_MISSES.load(Ordering::Relaxed) };
    }
    if !stores_out.is_null() {
        unsafe { *stores_out = CACHE_STORES.load(Ordering::Relaxed) };
    }
}

// ── Cache key computation ──────────────────────────────────────────────────

/// Compute a cache key from the manifold id and the schema-resolved
/// content of each argument packet. Formula:
///
///   key = mix(pool_hash, midx)
///   for each arg_packet, arg_schema:
///       key = mix(key, hash_morloc_packet(arg_packet, arg_schema, key))
///
/// The hash dereferences any shared-memory relative pointers in the
/// packet so two structurally equivalent inputs map to the same key
/// regardless of whether the data is inlined in the packet or stored
/// in shared memory. Hashing raw packet bytes would be wrong: SHM
/// addressing puts a relptr inside the packet, and two equivalent
/// values with different relptrs would hash differently.
///
/// `arg_schemas[i]` must be a non-null, well-formed schema string for
/// `arg_packets[i]`. If any schema is null, missing, or unparseable the
/// function aborts (returns 0); silently falling back to byte hashing
/// is unsafe -- it would let a SHM-backed cache miss masquerade as a
/// "different input" forever.
#[no_mangle]
pub unsafe extern "C" fn morloc_cache_key_compute(
    midx: u32,
    arg_packets: *const *const u8,
    arg_schemas: *const *const c_char,
    n_args: usize,
    errmsg: *mut *mut c_char,
) -> u64 {
    clear_errmsg(errmsg);

    let mut key = crate::utility::mix(pool_hash(), midx as u64);

    for i in 0..n_args {
        let packet = *arg_packets.add(i);
        let schema_cstr = *arg_schemas.add(i);
        if packet.is_null() {
            set_errmsg(errmsg, &MorlocError::Other(format!(
                "cache_key_compute: arg {} packet pointer is NULL", i
            )));
            return 0;
        }
        if schema_cstr.is_null() {
            set_errmsg(errmsg, &MorlocError::Other(format!(
                "cache_key_compute: arg {} schema string is NULL", i
            )));
            return 0;
        }

        let mut perr: *mut c_char = ptr::null_mut();
        let schema = parse_schema(schema_cstr, &mut perr);
        if schema.is_null() {
            if !perr.is_null() {
                *errmsg = perr;
            } else {
                set_errmsg(errmsg, &MorlocError::Other(format!(
                    "cache_key_compute: failed to parse schema for arg {}", i
                )));
            }
            return 0;
        }

        let mut arg_hash: u64 = 0;
        let ok = hash_morloc_packet(
            packet,
            schema as *const CSchema,
            key,
            &mut arg_hash,
            &mut perr,
        );
        free_schema(schema);
        if !ok {
            if !perr.is_null() {
                *errmsg = perr;
            } else {
                set_errmsg(errmsg, &MorlocError::Other(format!(
                    "cache_key_compute: hash_morloc_packet failed for arg {}", i
                )));
            }
            return 0;
        }
        key = crate::utility::mix(key, arg_hash);
    }
    key
}

/// Shared read-side path for both `morloc_cache_lookup` and
/// `get_cache_packet`: read `filename`, run `cache_entry_is_valid`,
/// return the malloc'd bytes on success. Consumes and frees
/// `filename`; on any failure frees the malloc'd bytes too and
/// returns a null pointer with `*size_out = 0`.
unsafe fn read_and_validate_cache_file(
    filename: *mut c_char,
    size_out: *mut usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    let mut err: *mut c_char = ptr::null_mut();
    let mut sb: libc::stat = std::mem::zeroed();
    if libc::stat(filename, &mut sb) != 0 {
        libc::free(filename as *mut c_void);
        return ptr::null_mut(); // miss: not an error
    }
    let mut file_size: usize = 0;
    let data = read_binary_file(filename, &mut file_size, &mut err);
    if data.is_null() {
        libc::free(filename as *mut c_void);
        if !err.is_null() { *errmsg = err; }
        return ptr::null_mut();
    }
    if !cache_entry_is_valid(filename, data, file_size) {
        libc::free(data as *mut c_void);
        libc::free(filename as *mut c_void);
        return ptr::null_mut();
    }
    libc::free(filename as *mut c_void);
    if !size_out.is_null() {
        *size_out = file_size;
    }
    data
}

// ── Label-aware lookup / store wrappers ────────────────────────────────────

/// Look up a cached result packet under @label/@ for the given key.
/// Returns the packet bytes (caller frees with `libc::free`) and writes
/// the byte length into `*size_out` on hit. Returns NULL on miss or on
/// any I/O error. Does not record hit/miss -- callers do that
/// explicitly so the cross-pool dispatch loop and the intra-pool
/// `call_cached` helper attribute their own results.
#[no_mangle]
pub unsafe extern "C" fn morloc_cache_lookup(
    key: u64,
    label: *const c_char,
    size_out: *mut usize,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    if !size_out.is_null() {
        *size_out = 0;
    }
    let filename = resolve_cache_filename(key, label, errmsg);
    if filename.is_null() {
        return ptr::null_mut();
    }
    read_and_validate_cache_file(filename, size_out, errmsg)
}

/// Peek at a cache entry's on-disk packet header and decide whether it
/// belongs to the current cache format. Returns false and unlinks the
/// stale files if not.
///
/// Cases returning false (miss + side-effect unlink):
/// - The bytes are shorter than a packet header (corrupt).
/// - Header claims `source=FILE, format=MSGPACK` — a legacy entry
///   from before the msgpack->voidstar migration; unlink both the
///   indirection and its `.dat` sidecar.
/// - Header claims `source=FILE, format=DATA` but the referenced
///   content sidecar does not exist, or its size differs from what
///   the indirection's length field claims (atomicity crash between
///   writes); unlink the dangling indirection.
///
/// All other headers pass through (the caller trusts the packet
/// framework to reject any leftover invalid combinations).
unsafe fn cache_entry_is_valid(
    indirection_filename: *const c_char,
    data: *const u8,
    data_len: usize,
) -> bool {
    use morloc_runtime_types::packet::{
        PACKET_FORMAT_DATA, PACKET_FORMAT_MSGPACK, PACKET_SOURCE_FILE,
        PacketHeader,
    };
    if data_len < 32 {
        libc::unlink(indirection_filename);
        return false;
    }
    let hdr_arr: [u8; 32] = match std::slice::from_raw_parts(data, 32).try_into() {
        Ok(a) => a,
        Err(_) => { libc::unlink(indirection_filename); return false; }
    };
    let hdr = match PacketHeader::from_bytes(&hdr_arr) {
        Ok(h) => h,
        Err(_) => { libc::unlink(indirection_filename); return false; }
    };
    if !hdr.is_data() {
        return true; // Non-data (call/ping/stream) -- not our concern.
    }
    let cmd = hdr.command.data;
    let src = cmd.source;
    let fmt = cmd.format;
    if src != PACKET_SOURCE_FILE {
        return true; // Inline / RPTR entries need no sidecar checks.
    }

    let payload_start = 32 + hdr.offset as usize;
    let payload_len = hdr.length as usize;
    if payload_start.saturating_add(payload_len) > data_len {
        libc::unlink(indirection_filename);
        return false;
    }
    let filename_bytes = std::slice::from_raw_parts(
        data.add(payload_start),
        payload_len.min(4096),
    );
    let ref_filename_str = match std::str::from_utf8(filename_bytes) {
        Ok(s) => s.trim_end_matches('\0').to_string(),
        Err(_) => { libc::unlink(indirection_filename); return false; }
    };
    let ref_cstring = match CString::new(ref_filename_str.as_bytes()) {
        Ok(c) => c,
        Err(_) => { libc::unlink(indirection_filename); return false; }
    };

    if fmt == PACKET_FORMAT_MSGPACK {
        // Cold-flush: legacy pre-migration entry. Unlink both files.
        libc::unlink(ref_cstring.as_ptr());
        libc::unlink(indirection_filename);
        return false;
    }
    if fmt == PACKET_FORMAT_DATA {
        // Atomicity: the content sidecar must exist and be non-empty.
        // A missing or empty file means the two-file write got
        // interrupted between rename(2) calls. `unlink` on a missing
        // sidecar is a no-op.
        let mut sb: libc::stat = std::mem::zeroed();
        let sidecar_ok =
            libc::stat(ref_cstring.as_ptr(), &mut sb) == 0 && sb.st_size > 0;
        if !sidecar_ok {
            libc::unlink(ref_cstring.as_ptr());
            libc::unlink(indirection_filename);
            return false;
        }
        return true;
    }
    // Unknown FILE format under the new scheme (e.g. TEXT / JSON via
    // an old test). Don't touch it -- let the reader surface its own
    // error.
    true
}

/// Store a cached value under two files:
///
///   `<cache_base>/<label>/<call_key:016x>.dat`
///       A `PACKET_TYPE_DATA` indirection packet
///       (source=FILE, format=DATA, compression=NONE) whose payload is
///       the path to the content file.
///   `<cache_base>/data/<data_hash:016x>.dat`
///       A full voidstar data packet (32-byte header + SCHEMA_STRING
///       metadata + optional zstd payload), content-addressed.
///
/// `data_hash = hash_voidstar(voidstar)` — the schema-directed
/// structural hash — so equal values from different call sites (any
/// label, any pool language) collapse onto one content file
/// regardless of allocation history.
///
/// Content ordering: content file is written first (via rename), then
/// the indirection. A crash between the two leaves an orphan content
/// file that a later run silently overwrites via the same content
/// hash. The reverse ordering would leave an indirection pointing at
/// a missing content file, which the reader treats as a cache miss.
#[no_mangle]
pub unsafe extern "C" fn morloc_cache_store(
    key: u64,
    label: *const c_char,
    data: *const u8,
    size: usize,
    schema_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);

    if data.is_null() || size == 0 {
        set_errmsg(errmsg, &MorlocError::Other(
            "cache_store: NULL or empty packet".into()
        ));
        return false;
    }
    if schema_str.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "cache_store: NULL schema string".into()
        ));
        return false;
    }

    let mut err: *mut c_char = ptr::null_mut();

    let schema = parse_schema(schema_str, &mut err);
    if schema.is_null() {
        if !err.is_null() { *errmsg = err; }
        else {
            set_errmsg(errmsg, &MorlocError::Other(
                "cache_store: failed to parse schema".into()
            ));
        }
        return false;
    }

    // Materialize the packet into SHM voidstar so we can hash and
    // re-serialize it. This handles RPTR / FILE / MESG uniformly.
    let voidstar = get_morloc_data_packet_value(data, schema, &mut err);
    if voidstar.is_null() {
        free_schema(schema);
        if !err.is_null() { *errmsg = err; }
        else {
            set_errmsg(errmsg, &MorlocError::Other(
                "cache_store: get_morloc_data_packet_value failed".into()
            ));
        }
        return false;
    }

    let rs = CSchema::to_rust(schema);
    // Structural hash matches the caller-side lookup key semantics
    // (see slurm_ffi::remote_call using `hash_voidstar`) so store and
    // lookup agree. Seed 0 is the shared default across the runtime.
    let data_hash = match hash_voidstar_inner(voidstar as *const u8, &rs, 0) {
        Ok(h) => h,
        Err(e) => {
            free_schema(schema);
            set_errmsg(errmsg, &e);
            return false;
        }
    };

    let dat_filename = resolve_data_filename(data_hash, &mut err);
    if dat_filename.is_null() {
        free_schema(schema);
        if !err.is_null() { *errmsg = err; }
        return false;
    }

    // Skip the dat write if the content-addressed file is already
    // present. Two distinct call sites returning the same value share
    // one on-disk copy.
    let dat_already_exists = {
        let mut sb: libc::stat = std::mem::zeroed();
        libc::stat(dat_filename, &mut sb) == 0
    };
    if !dat_already_exists {
        let packet_bytes = match build_persistence_data_packet(
            voidstar as *const u8, schema, &mut err,
        ) {
            Some(b) => b,
            None => {
                libc::free(dat_filename as *mut c_void);
                free_schema(schema);
                if !err.is_null() { *errmsg = err; }
                return false;
            }
        };
        let rc = write_atomic(
            dat_filename,
            packet_bytes.as_ptr(),
            packet_bytes.len(),
            &mut err,
        );
        if rc != 0 {
            libc::free(dat_filename as *mut c_void);
            free_schema(schema);
            if !err.is_null() { *errmsg = err; }
            return false;
        }
    }

    let pkt = crate::packet_ffi::make_data_indirection_packet(dat_filename, schema);
    libc::free(dat_filename as *mut c_void);
    free_schema(schema);
    if pkt.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "cache_store: make_data_indirection_packet failed".into()
        ));
        return false;
    }

    let pkt_size = morloc_packet_size(pkt, &mut err);
    if !err.is_null() {
        libc::free(pkt as *mut c_void);
        *errmsg = err;
        return false;
    }

    let pkt_filename = resolve_cache_filename(key, label, errmsg);
    if pkt_filename.is_null() {
        libc::free(pkt as *mut c_void);
        return false;
    }

    let rc = write_atomic(pkt_filename, pkt, pkt_size, &mut err);
    libc::free(pkt_filename as *mut c_void);
    libc::free(pkt as *mut c_void);
    if rc != 0 {
        if !err.is_null() { *errmsg = err; }
        return false;
    }
    true
}

// ── hash_voidstar ──────────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn hash_voidstar(
    data: *const c_void,
    schema: *const CSchema,
    seed: u64,
    errmsg: *mut *mut c_char,
) -> u64 {
    clear_errmsg(errmsg);
    let rs = CSchema::to_rust(schema);
    match hash_voidstar_inner(data as *const u8, &rs, seed) {
        Ok(h) => h,
        Err(e) => {
            set_errmsg(errmsg, &e);
            0
        }
    }
}

fn hash_voidstar_inner(
    data: *const u8,
    schema: &crate::schema::Schema,
    seed: u64,
) -> Result<u64, MorlocError> {
    use crate::schema::SerialType;
    use crate::utility::mix;

    // Fold the SerialType tag into the seed at every recursion so
    // structurally distinct schemas can never share a payload hash. Without
    // this, single-element wrappings collide: `[[1.0, 2.0]]` as `[[Real]]`
    // walks a 1-iteration outer loop into a fixed-width inner array of 16
    // bytes and returns `xxh64({1.0,2.0}, 0)` -- exactly the hash `[Real]`
    // produces for `[1.0, 2.0]`. Arrays additionally mix in `arr.size` so
    // `[[a,b]]` and `[[a],[b]]` differ even when their flat element bytes
    // and total counts match.
    let seed = mix(seed, schema.serial_type as u64);

    // SAFETY: data points to voidstar data in SHM with layout described by schema.
    // All reads (Array headers, element data) are within schema-defined bounds.
    unsafe {
        match schema.serial_type {
            SerialType::Int => {
                // Inline BigInt: hash the value directly for size <= 1
                let size = *(data as *const usize);
                if size <= 1 {
                    let bytes = std::slice::from_raw_parts(data.add(8), 8);
                    Ok(hash::xxh64_with_seed(bytes, seed))
                } else {
                    let relptr = *(data.add(std::mem::size_of::<usize>()) as *const shm::RelPtr);
                    let limb_data = shm::rel2abs(relptr)?;
                    let bytes = std::slice::from_raw_parts(limb_data, size * 8);
                    Ok(hash::xxh64_with_seed(bytes, seed))
                }
            }
            SerialType::IFile | SerialType::OStream | SerialType::IStream => {
                // Cache key is content-derived. Resolve TAG_HANDLE to its
                // path via the local SHM registry so two handles to the
                // same file produce the same hash. TAG_PATH reads the
                // path straight out of the suballoc.
                use morloc_runtime_types::stream_handle as sh;
                let field = data as *const u8;
                let tag = sh::read_tag(field);
                let payload = sh::read_payload(field);
                let owned: Vec<u8>;
                let borrowed: &[u8];
                if tag == sh::TAG_PATH {
                    if payload == sh::RELNULL_PAYLOAD {
                        borrowed = &[];
                    } else {
                        let suballoc = shm::rel2abs(payload as shm::RelPtr)?;
                        let path_len = sh::read_path_size(suballoc) as usize;
                        borrowed = std::slice::from_raw_parts(suballoc.add(8), path_len);
                    }
                } else if tag == sh::TAG_HANDLE {
                    owned = crate::stream::handle_path(payload as i64)?.into_bytes();
                    borrowed = &owned;
                } else {
                    return Err(MorlocError::Other(format!(
                        "cache hash: unsupported stream-handle tag {}", tag,
                    )));
                }
                Ok(hash::xxh64_with_seed(borrowed, seed))
            }
            SerialType::String | SerialType::Array => {
                let arr = &*(data as *const shm::Array);
                let seed = mix(seed, arr.size as u64);
                let elem_width = if schema.parameters.is_empty() {
                    1 // string bytes
                } else {
                    schema.parameters[0].width
                };
                let elem_data = shm::rel2abs(arr.data)?;

                if schema.is_fixed_width()
                    || matches!(schema.serial_type, SerialType::String)
                {
                    let total = elem_width * arr.size;
                    let bytes = std::slice::from_raw_parts(elem_data, total);
                    Ok(hash::xxh64_with_seed(bytes, seed))
                } else {
                    let mut h = seed;
                    for i in 0..arr.size {
                        h = hash_voidstar_inner(
                            elem_data.add(i * elem_width),
                            &schema.parameters[0],
                            h,
                        )?;
                    }
                    Ok(h)
                }
            }
            SerialType::Tuple | SerialType::Map => {
                if schema.is_fixed_width() {
                    let bytes = std::slice::from_raw_parts(data, schema.width);
                    Ok(hash::xxh64_with_seed(bytes, seed))
                } else {
                    let mut h = seed;
                    for i in 0..schema.parameters.len() {
                        h = hash_voidstar_inner(
                            data.add(schema.offsets[i]),
                            &schema.parameters[i],
                            h,
                        )?;
                    }
                    Ok(h)
                }
            }
            _ => {
                let bytes = std::slice::from_raw_parts(data, schema.width);
                Ok(hash::xxh64_with_seed(bytes, seed))
            }
        }
    }
}

// ── hash_morloc_packet ─────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn hash_morloc_packet(
    packet: *const u8,
    schema: *const CSchema,
    seed: u64,
    hash_out: *mut u64,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    *hash_out = 0;

    extern "C" {
        fn read_morloc_packet_header(
            msg: *const u8,
            errmsg: *mut *mut c_char,
        ) -> *const crate::packet::PacketHeader;
    }

    let mut err: *mut c_char = ptr::null_mut();
    let header = read_morloc_packet_header(packet, &mut err);
    if header.is_null() {
        if !err.is_null() {
            *errmsg = err;
        }
        return false;
    }

    let cmd_type = (*header).command_type();
    if cmd_type == crate::packet::PACKET_TYPE_CALL {
        let midx = { (*header).command.call.midx };
        *hash_out = crate::utility::mix(seed, midx as u64);
        let offset = { (*header).offset } as usize;
        let length = { (*header).length } as usize;
        let arg_data = packet.add(32 + offset);
        let mut arg_start = 0usize;
        while arg_start < length {
            let arg_size = morloc_packet_size(arg_data.add(arg_start), &mut err);
            if !err.is_null() {
                *errmsg = err;
                return false;
            }
            let arg_bytes = std::slice::from_raw_parts(arg_data.add(arg_start), arg_size);
            *hash_out = crate::utility::mix(*hash_out, hash::xxh64_with_seed(arg_bytes, *hash_out));
            arg_start += arg_size;
        }
    } else if cmd_type == crate::packet::PACKET_TYPE_DATA {
        let voidstar = get_morloc_data_packet_value(packet, schema, &mut err);
        if voidstar.is_null() {
            if !err.is_null() {
                *errmsg = err;
            }
            return false;
        }
        let rs = CSchema::to_rust(schema);
        match hash_voidstar_inner(voidstar, &rs, seed) {
            Ok(h) => *hash_out = h,
            Err(e) => {
                set_errmsg(errmsg, &e);
                return false;
            }
        }
    } else {
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!("Cannot hash packet with command 0x{:02x}", cmd_type)),
        );
        return false;
    }

    true
}

// ── Cache filename generation ──────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn make_cache_filename_ext(
    key: u64,
    cache_path: *const c_char,
    ext: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let path = CStr::from_ptr(cache_path).to_string_lossy();
    let extension = CStr::from_ptr(ext).to_string_lossy();
    let filename = format!("{}/{:016x}{}", path, key, extension);
    match CString::new(filename) {
        Ok(cs) => cs.into_raw(),
        Err(_) => {
            set_errmsg(errmsg, &MorlocError::Other("CString conversion failed".into()));
            ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn make_cache_filename(
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    // `.dat` for every on-disk cache entry -- both the caller-key index
    // and the content-hash blob. Reader tells them apart by inspecting
    // the packet header bytes.
    let ext = CString::new(".dat").unwrap();
    make_cache_filename_ext(key, cache_path, ext.as_ptr(), errmsg)
}

/// Compression level read from `MORLOC_CACHE_COMPRESSION_LEVEL`. Default
/// is 0 (uncompressed) so `morloc dump` renders cache entries without
/// paying decompression cost. Users opt into higher levels via env for
/// large-payload workloads where disk footprint matters. Cached once on
/// first read like `pool_hash` and `cache_base`.
fn read_cache_compression_level() -> u8 {
    static CACHED: OnceLock<u8> = OnceLock::new();
    *CACHED.get_or_init(|| {
        std::env::var("MORLOC_CACHE_COMPRESSION_LEVEL")
            .ok()
            .and_then(|s| s.trim().parse::<u8>().ok())
            .unwrap_or(0)
    })
}

/// Serialize an SHM voidstar as a self-contained MORLOC_DATA_PACKET
/// suitable for on-disk persistence. Runs the persistence rewrite
/// (TAG_HANDLE -> TAG_PATH, stdio rejection) then optionally zstd-
/// compresses per `MORLOC_CACHE_COMPRESSION_LEVEL`. The returned bytes
/// are the entire packet (32-byte header + SCHEMA_STRING metadata +
/// payload); write them to disk as one atomic file.
pub(crate) unsafe fn build_persistence_data_packet(
    voidstar: *const u8,
    schema: *const CSchema,
    errmsg: *mut *mut c_char,
) -> Option<Vec<u8>> {
    use morloc_runtime_types::packet::{
        append_metadata_entry, METADATA_TYPE_SCHEMA_STRING,
        PACKET_COMPRESSION_NONE, PACKET_COMPRESSION_ZSTD,
        PACKET_FORMAT_VOIDSTAR, PacketHeader,
    };

    let rs = CSchema::to_rust(schema);

    let mut blob = match crate::voidstar::flatten_to_buffer(
        voidstar as crate::shm::AbsPtr,
        &rs,
    ) {
        Ok(b) => b,
        Err(e) => { set_errmsg(errmsg, &e); return None; }
    };

    // TAG_HANDLE -> TAG_PATH: slot ids don't survive across nexus
    // lifetimes. Guards against stdio-bound handles (rejected with a
    // typed error before any bytes reach disk).
    let fields = match crate::handle_scan::collect_stream_fields(&blob, &rs) {
        Ok(f) => f,
        Err(e) => { set_errmsg(errmsg, &e); return None; }
    };
    if let Err(e) = crate::handle_scan::rewrite_handles_to_paths(&mut blob, &fields) {
        set_errmsg(errmsg, &e);
        return None;
    }

    let clvl = match crate::compression::CompressionLevel::from_u8(
        read_cache_compression_level(),
    ) {
        Ok(l) => l,
        Err(e) => { set_errmsg(errmsg, &e); return None; }
    };
    let payload = match crate::compression::compress_payload_zstd(&blob, clvl) {
        Ok((bytes, _frames)) => bytes,
        Err(e) => { set_errmsg(errmsg, &e); return None; }
    };
    let compression_byte = if clvl.is_none() {
        PACKET_COMPRESSION_NONE
    } else {
        PACKET_COMPRESSION_ZSTD
    };
    drop(blob);

    let schema_str = crate::schema::schema_to_string(&rs);
    let mut schema_body = schema_str.into_bytes();
    schema_body.push(0); // NUL-terminator matches decode_schema_entry
    let metadata = append_metadata_entry(
        &[], METADATA_TYPE_SCHEMA_STRING, &schema_body,
    );
    let mut header = PacketHeader::data_mesg(PACKET_FORMAT_VOIDSTAR, payload.len() as u64);
    header.offset = metadata.len() as u32;
    let hdr_bytes = header.to_bytes();
    // Compression byte lives inside the 32-byte header at byte 15
    // (see PacketHeader::data_mesg comment in packet.rs). Patch it
    // in the serialized form.
    let mut hdr_bytes = hdr_bytes;
    hdr_bytes[15] = compression_byte;

    let mut out = Vec::with_capacity(32 + metadata.len() + payload.len());
    out.extend_from_slice(&hdr_bytes);
    out.extend_from_slice(&metadata);
    out.extend_from_slice(&payload);
    Some(out)
}

// ── Cache operations ───────────────────────────────────────────────────────

#[no_mangle]
pub unsafe extern "C" fn put_cache_packet(
    voidstar: *const u8,
    schema: *const CSchema,
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);

    let mut err: *mut c_char = ptr::null_mut();

    let pkt_filename = make_cache_filename(key, cache_path, &mut err);
    if pkt_filename.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    // One file per cache entry: the caller-supplied `key` here is
    // already `hash_voidstar(arg_voidstars[i])`, so key == content
    // hash and the "indirection" packet degenerates to the data
    // packet itself.
    let packet_bytes = match build_persistence_data_packet(voidstar, schema, &mut err) {
        Some(b) => b,
        None => {
            libc::free(pkt_filename as *mut c_void);
            if !err.is_null() { *errmsg = err; }
            return ptr::null_mut();
        }
    };

    let rc = write_atomic(
        pkt_filename,
        packet_bytes.as_ptr(),
        packet_bytes.len(),
        &mut err,
    );
    if rc != 0 {
        libc::free(pkt_filename as *mut c_void);
        if !err.is_null() { *errmsg = err; }
        return ptr::null_mut();
    }

    let result = libc::strdup(pkt_filename);
    libc::free(pkt_filename as *mut c_void);
    result
}

#[no_mangle]
pub unsafe extern "C" fn get_cache_packet(
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut u8 {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    let filename = make_cache_filename(key, cache_path, &mut err);
    if filename.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }
    read_and_validate_cache_file(filename, ptr::null_mut(), errmsg)
}

#[no_mangle]
pub unsafe extern "C" fn del_cache_packet(
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> bool {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    let filename = make_cache_filename(key, cache_path, &mut err);
    if filename.is_null() {
        *errmsg = err;
        return false;
    }

    let rc = libc::unlink(filename);
    if rc != 0 {
        set_errmsg(
            errmsg,
            &MorlocError::Other(format!(
                "Failed to delete cache file '{}'",
                CStr::from_ptr(filename).to_string_lossy()
            )),
        );
        libc::free(filename as *mut c_void);
        return false;
    }
    libc::free(filename as *mut c_void);
    true
}

#[no_mangle]
pub unsafe extern "C" fn check_cache_packet(
    key: u64,
    cache_path: *const c_char,
    errmsg: *mut *mut c_char,
) -> *mut c_char {
    clear_errmsg(errmsg);
    let mut err: *mut c_char = ptr::null_mut();

    let filename = make_cache_filename(key, cache_path, &mut err);
    if filename.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    // Cache existence + format check on the hot SLURM dispatch path.
    // Bounded peek: PacketHeader is 32 B, the filename payload rarely
    // exceeds a few hundred bytes, so 4 KiB is a safe upper bound
    // that covers the whole indirection while avoiding the full-file
    // read a stale/oversized entry would trigger.
    let fd = libc::open(filename, libc::O_RDONLY);
    if fd < 0 {
        libc::free(filename as *mut c_void);
        return ptr::null_mut(); // miss / unreadable
    }
    let mut buf: [u8; 4096] = [0; 4096];
    let n = libc::read(fd, buf.as_mut_ptr() as *mut c_void, buf.len());
    libc::close(fd);
    if n < 32 {
        libc::unlink(filename);
        libc::free(filename as *mut c_void);
        return ptr::null_mut();
    }
    if !cache_entry_is_valid(filename, buf.as_ptr(), n as usize) {
        libc::free(filename as *mut c_void);
        return ptr::null_mut();
    }
    let result = libc::strdup(filename);
    libc::free(filename as *mut c_void);
    result
}
