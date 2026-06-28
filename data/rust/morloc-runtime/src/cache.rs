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
}

/// Resolve the on-disk path `<cache_dir>/<label>/<key:016x>.packet`,
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
    let dat_ext = CString::new(".dat").unwrap();
    let filename = make_cache_filename_ext(data_hash, dir, dat_ext.as_ptr(), &mut err);
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
/// nexus at exit to print the run summary (Stage 4).
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
    let mut err: *mut c_char = ptr::null_mut();
    let mut sb: libc::stat = std::mem::zeroed();
    if libc::stat(filename, &mut sb) != 0 {
        libc::free(filename as *mut c_void);
        return ptr::null_mut(); // miss: not an error
    }
    extern "C" {
        fn read_binary_file(
            filename: *const c_char,
            file_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }
    let mut file_size: usize = 0;
    let data = read_binary_file(filename, &mut file_size, &mut err);
    libc::free(filename as *mut c_void);
    if data.is_null() {
        if !err.is_null() { *errmsg = err; }
        return ptr::null_mut();
    }
    if !size_out.is_null() {
        *size_out = file_size;
    }
    data
}

/// Store a cached value under two files:
///
///   `<cache_base>/<label>/<call_key:016x>.packet`
///       A `PACKET_TYPE_DATA` packet (source=FILE, format=MSGPACK) whose
///       payload is the path to the dat file.
///   `<cache_base>/data/<data_hash:016x>.dat`
///       Raw msgpack bytes of the cached value, content-addressed.
///
/// `data_hash = xxh64(materialized_msgpack_bytes)` so equal values
/// from different call sites (any label, any pool language)
/// collapse onto one dat file. The .packet pointer makes the call→data
/// indirection explicit and lets the existing get_value pipeline read
/// the value back via the standard FILE-source packet path.
#[no_mangle]
pub unsafe extern "C" fn morloc_cache_store(
    key: u64,
    label: *const c_char,
    data: *const u8,
    size: usize,
    schema_str: *const c_char,
    errmsg: *mut *mut c_char,
) -> bool {
    extern "C" {
        fn get_data_packet_as_mpk(
            packet: *const u8,
            schema: *const CSchema,
            mpk_out: *mut *mut c_char,
            mpk_size_out: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
        fn make_mpk_data_packet(
            filename: *const c_char,
            schema: *const CSchema,
        ) -> *mut u8;
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize;
    }

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

    // Materialize the packet's payload to msgpack so the on-disk dat
    // file is the schema-agnostic content (no header, no relptr).
    let mut mpk: *mut c_char = ptr::null_mut();
    let mut mpk_size: usize = 0;
    let ok = get_data_packet_as_mpk(data, schema, &mut mpk, &mut mpk_size, &mut err);
    if ok == 0 || mpk.is_null() {
        free_schema(schema);
        if !err.is_null() { *errmsg = err; }
        else {
            set_errmsg(errmsg, &MorlocError::Other(
                "cache_store: get_data_packet_as_mpk failed".into()
            ));
        }
        return false;
    }

    let mpk_slice = std::slice::from_raw_parts(mpk as *const u8, mpk_size);
    let data_hash = hash::xxh64_with_seed(mpk_slice, 0);

    let dat_filename = resolve_data_filename(data_hash, &mut err);
    if dat_filename.is_null() {
        libc::free(mpk as *mut c_void);
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
        let rc = write_atomic(dat_filename, mpk as *const u8, mpk_size, &mut err);
        if rc != 0 {
            libc::free(dat_filename as *mut c_void);
            libc::free(mpk as *mut c_void);
            free_schema(schema);
            if !err.is_null() { *errmsg = err; }
            return false;
        }
    }
    libc::free(mpk as *mut c_void);

    let pkt = make_mpk_data_packet(dat_filename, schema);
    libc::free(dat_filename as *mut c_void);
    free_schema(schema);
    if pkt.is_null() {
        set_errmsg(errmsg, &MorlocError::Other(
            "cache_store: make_mpk_data_packet failed".into()
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

    // SAFETY: data points to voidstar data in SHM with layout described by schema.
    // All reads (Array headers, element data) are within schema-defined bounds.
    unsafe {
        match schema.serial_type {
            SerialType::Int => {
                // Inline BigInt: hash the value directly for size ≤ 1
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
            SerialType::String | SerialType::IFile | SerialType::Array => {
                let arr = &*(data as *const shm::Array);
                let elem_width = if schema.parameters.is_empty() {
                    1 // string bytes
                } else {
                    schema.parameters[0].width
                };
                let elem_data = shm::rel2abs(arr.data)?;

                if schema.is_fixed_width()
                    || matches!(schema.serial_type, SerialType::String | SerialType::IFile)
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
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize;
        fn get_morloc_data_packet_value(
            data: *const u8,
            schema: *const CSchema,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
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
    let ext = CString::new(".packet").unwrap();
    make_cache_filename_ext(key, cache_path, ext.as_ptr(), errmsg)
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

    extern "C" {
        fn make_mpk_data_packet(filename: *const c_char, schema: *const CSchema) -> *mut u8;
        fn morloc_packet_size(packet: *const u8, errmsg: *mut *mut c_char) -> usize;
        fn pack_with_schema(
            mlc: *const c_void, schema: *const CSchema,
            mpk: *mut *mut c_char, mpk_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
        fn write_atomic(
            filename: *const c_char, data: *const u8, size: usize,
            errmsg: *mut *mut c_char,
        ) -> i32;
    }

    let mut err: *mut c_char = ptr::null_mut();

    // Generate filenames
    let pkt_filename = make_cache_filename(key, cache_path, &mut err);
    if pkt_filename.is_null() {
        *errmsg = err;
        return ptr::null_mut();
    }

    let dat_ext = CString::new(".dat").unwrap();
    let dat_filename = make_cache_filename_ext(key, cache_path, dat_ext.as_ptr(), &mut err);
    if dat_filename.is_null() {
        libc::free(pkt_filename as *mut c_void);
        *errmsg = err;
        return ptr::null_mut();
    }

    // Create data packet pointing to the .dat file
    let data_packet = make_mpk_data_packet(dat_filename, schema);
    if data_packet.is_null() {
        libc::free(pkt_filename as *mut c_void);
        libc::free(dat_filename as *mut c_void);
        set_errmsg(errmsg, &MorlocError::Other("Failed to create data packet".into()));
        return ptr::null_mut();
    }

    let pkt_size = morloc_packet_size(data_packet, &mut err);

    // Pack voidstar to msgpack
    let mut mpk_data: *mut c_char = ptr::null_mut();
    let mut mpk_size: usize = 0;
    let rc = pack_with_schema(voidstar as *const c_void, schema, &mut mpk_data, &mut mpk_size, &mut err);
    if rc != 0 {
        libc::free(data_packet as *mut c_void);
        libc::free(pkt_filename as *mut c_void);
        libc::free(dat_filename as *mut c_void);
        *errmsg = err;
        return ptr::null_mut();
    }

    // Write packet file
    write_atomic(pkt_filename, data_packet, pkt_size, &mut err);
    libc::free(data_packet as *mut c_void);
    if !err.is_null() {
        libc::free(mpk_data as *mut c_void);
        libc::free(pkt_filename as *mut c_void);
        libc::free(dat_filename as *mut c_void);
        *errmsg = err;
        return ptr::null_mut();
    }

    // Write data file
    write_atomic(dat_filename, mpk_data as *const u8, mpk_size, &mut err);
    libc::free(mpk_data as *mut c_void);
    libc::free(dat_filename as *mut c_void);
    if !err.is_null() {
        libc::free(pkt_filename as *mut c_void);
        *errmsg = err;
        return ptr::null_mut();
    }

    // Return the packet filename
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

    extern "C" {
        fn read_binary_file(
            filename: *const c_char, file_size: *mut usize,
            errmsg: *mut *mut c_char,
        ) -> *mut u8;
    }

    let mut file_size: usize = 0;
    let data = read_binary_file(filename, &mut file_size, &mut err);
    libc::free(filename as *mut c_void);
    if data.is_null() {
        *errmsg = err;
    }
    data
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

    let mut sb: libc::stat = std::mem::zeroed();
    if libc::stat(filename, &mut sb) == 0 {
        let result = libc::strdup(filename);
        libc::free(filename as *mut c_void);
        return result;
    }
    libc::free(filename as *mut c_void);
    ptr::null_mut() // Not an error — cache miss
}
