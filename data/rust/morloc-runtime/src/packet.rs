//! Packet protocol: header layout (stateless, in types crate) plus the
//! runtime-tunable config statics and SHM-touching payload helpers
//! (defined here so the nexus does not duplicate them via the rlib).

pub use morloc_runtime_types::packet::*;

use crate::error::MorlocError;

// ── Inline threshold (runtime-tunable; stays in libmorloc.so) ──────────────

// Runtime-tunable copy of the inline threshold, in bytes. Data with a
// flat size <= this value is embedded directly in the packet (MESG);
// larger payloads are routed via SHM (RPTR) or, when SHM is disabled,
// via a temp file (FILE).
//
// Mutated by `config::morloc_set_inline_threshold` (FFI) and by the
// `MORLOC_INLINE_SIZE` env var (read once via `ensure_config_loaded`).
pub static INLINE_THRESHOLD: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(MORLOC_INLINE_THRESHOLD);

// Runtime-tunable SHM enable flag. False = data over the inline
// threshold is written to a temp file instead of shared memory.
//
// Mutated by `config::morloc_set_shm_enabled` (FFI) and by the
// `MORLOC_NO_SHM` env var.
pub static SHM_ENABLED: std::sync::atomic::AtomicBool =
    std::sync::atomic::AtomicBool::new(true);

/// Process-wide serialization gate for any test that mutates or
/// observes [`INLINE_THRESHOLD`] / [`SHM_ENABLED`] / the file-packet
/// tmpdir. Without it, `cargo test`'s default multi-threaded
/// runner interleaves writes from `config_ffi::tests` with reads
/// from `packet_ffi::auto_routing_tests`, producing nondeterministic
/// failures.
///
/// Each test acquires the lock at entry; the guard is dropped at
/// the end of the test. Lock contention is irrelevant -- the
/// shared atomics need exclusive access for the duration of any
/// observation that depends on a specific value.
#[cfg(test)]
pub static TEST_CONFIG_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

// Read the live inline threshold. Cheap and lock-free.
#[inline]
pub fn inline_threshold() -> usize {
    ensure_config_loaded();
    INLINE_THRESHOLD.load(std::sync::atomic::Ordering::Relaxed)
}

// Read the live SHM-enabled flag. Cheap and lock-free.
#[inline]
pub fn shm_enabled() -> bool {
    ensure_config_loaded();
    SHM_ENABLED.load(std::sync::atomic::Ordering::Relaxed)
}

// Tmpdir for file-packet intermediates (written when SHM_ENABLED is
// false and data exceeds the inline threshold). Mutex<Option<String>>
// rather than an atomic because the value is a variable-length path.
// Empty/None = "use std::env::temp_dir() at call time".
static TMPDIR: std::sync::Mutex<Option<String>> = std::sync::Mutex::new(None);

/// Read the configured tmpdir for file packets. None = fall back to
/// `std::env::temp_dir()` (or `/tmp`) at the call site. Read by
/// `make_file_data_packet_voidstar`.
pub fn file_packet_tmpdir() -> Option<String> {
    ensure_config_loaded();
    TMPDIR.lock().ok().and_then(|g| g.clone())
}

/// Override the file-packet tmpdir. Empty string = unset (fall back
/// to `std::env::temp_dir()`). Idempotent.
pub fn set_file_packet_tmpdir(path: Option<String>) {
    if let Ok(mut g) = TMPDIR.lock() {
        *g = path.filter(|p| !p.is_empty());
    }
}

// One-shot loader for env-var fallbacks. Pool processes inherit the
// nexus's `MORLOC_INLINE_SIZE` / `MORLOC_NO_SHM` / `MORLOC_TMPDIR`
// env vars on execvp; libmorloc reads them the first time
// inline_threshold() / shm_enabled() / file_packet_tmpdir() is
// called and pokes the globals. After that, FFI setters are the
// only way to change the values.
static CONFIG_INIT: std::sync::Once = std::sync::Once::new();

fn ensure_config_loaded() {
    CONFIG_INIT.call_once(|| {
        if let Ok(s) = std::env::var("MORLOC_INLINE_SIZE") {
            if let Ok(n) = s.parse::<i64>() {
                if n >= 0 {
                    INLINE_THRESHOLD.store(n as usize, std::sync::atomic::Ordering::Relaxed);
                }
            }
        }
        if let Ok(s) = std::env::var("MORLOC_NO_SHM") {
            let v = s.trim().to_ascii_lowercase();
            let off = matches!(v.as_str(), "1" | "true" | "yes" | "on");
            if off {
                SHM_ENABLED.store(false, std::sync::atomic::Ordering::Relaxed);
            }
        }
        if let Ok(s) = std::env::var("MORLOC_TMPDIR") {
            if !s.is_empty() {
                if let Ok(mut g) = TMPDIR.lock() {
                    *g = Some(s);
                }
            }
        }
    });
}

// ── SHM-resolving payload extraction (stays in libmorloc.so) ───────────────

/// Get the voidstar value from a data packet (resolves relptr to absptr).
pub fn get_data_value(
    packet: &[u8],
    schema: &crate::Schema,
) -> Result<crate::shm::AbsPtr, MorlocError> {
    let header = PacketHeader::from_bytes(packet[..32].try_into().unwrap())?;
    let source = unsafe { header.command.data.source };
    let format = unsafe { header.command.data.format };

    let payload = get_data_payload(packet)?;

    match source {
        PACKET_SOURCE_RPTR => {
            // Payload is a relptr
            if payload.len() < std::mem::size_of::<crate::shm::RelPtr>() {
                return Err(MorlocError::Packet("relptr payload too small".into()));
            }
            let relptr = crate::shm::RelPtr::from_ne_bytes(
                payload[..std::mem::size_of::<crate::shm::RelPtr>()].try_into().unwrap()
            );
            crate::shm::rel2abs(relptr)
        }
        PACKET_SOURCE_MESG => {
            match format {
                PACKET_FORMAT_MSGPACK => {
                    crate::mpack::unpack_with_schema(payload, schema)
                }
                PACKET_FORMAT_JSON => {
                    let json_str = std::str::from_utf8(payload)
                        .map_err(|e| MorlocError::Packet(format!("invalid UTF-8: {}", e)))?;
                    crate::json::read_json_with_schema(json_str, schema)
                }
                PACKET_FORMAT_VOIDSTAR => {
                    read_voidstar_binary(payload, schema)
                }
                _ => {
                    Err(MorlocError::Packet(format!(
                        "unsupported data format: {}", format
                    )))
                }
            }
        }
        _ => Err(MorlocError::Packet(format!("unsupported source: {}", source))),
    }
}

/// Read a flat voidstar binary blob into shared memory, adjusting relptrs.
fn read_voidstar_binary(
    blob: &[u8],
    schema: &crate::Schema,
) -> Result<crate::shm::AbsPtr, MorlocError> {
    use crate::shm;

    let base = shm::shmalloc(blob.len())?;
    unsafe { std::ptr::copy_nonoverlapping(blob.as_ptr(), base, blob.len()) };

    let base_rel = shm::abs2rel(base)?;
    crate::voidstar::adjust_relptrs(base, schema, base_rel)?;
    Ok(base)
}
