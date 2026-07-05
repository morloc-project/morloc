//! `morloc-nexus view` packet-conversion dispatcher.
//!
//! Routes the four packet-in / packet-out quadrants:
//!
//!   * **DATA -> DATA**: byte-copy (or loader re-emit if `--schema`
//!     differs from embedded).
//!   * **DATA -> STREAM**: `mlc_view_data_to_stream` FFI. The runtime
//!     opens the input as an IFile, chunk-slices via the existing
//!     bracket-slice discipline, and writes each chunk through the
//!     canonical OStream writer.
//!   * **STREAM -> STREAM**: `mlc_view_stream_to_stream` FFI. Drains
//!     the input via IStream `@next`; writes via OStream `@write`.
//!     Handles footer-less input (IStream forward-walks unconditionally).
//!   * **STREAM -> DATA**: the existing `load_with_schema` path already
//!     materialises stream -> voidstar -> DATA on `-f packet`. This
//!     module only adds the `--force` size guardrail and delegates.
//!
//! Atomic-output convention: when `-o FILE` is set, write to
//! `FILE.partial` then rename on success. If `-o` is unset the caller
//! writes to a temp file and streams to stdout at the end.

use std::ffi::{c_char, CString};
use std::path::{Path, PathBuf};
use std::ptr;

use crate::file::{Classification, FooterInfo};
use crate::process::{path_to_cstring, take_c_errmsg};

extern "C" {
    fn mlc_view_stream_to_stream(
        in_path: *const c_char,
        out_path: *const c_char,
        compression_level: u8,
        schema_override: *const c_char,
        out_subpackets: *mut u64,
        errmsg: *mut *mut c_char,
    ) -> i32;
    fn mlc_view_data_to_stream(
        in_path: *const c_char,
        out_path: *const c_char,
        compression_level: u8,
        out_subpackets: *mut u64,
        errmsg: *mut *mut c_char,
    ) -> i32;
}

/// Default size threshold above which buffered outputs (stream ->
/// data materialisation, single-value formats on large streams)
/// require `--force`. Overridable via `MORLOC_VIEW_MAX_BUFFER_BYTES`.
pub const DEFAULT_LARGE_BUFFER_BYTES: u64 = 1 << 30; // 1 GiB

/// Which policy triggered a guardrail refusal, for error messages.
#[derive(Debug, Clone, Copy)]
pub enum GuardrailKind {
    /// Buffered output would exceed the size threshold.
    Size,
    /// Binary output would be written to a terminal.
    BinaryToTty,
}

/// Read the `--force` size threshold. Env var overrides the default.
pub fn size_threshold_bytes() -> u64 {
    std::env::var("MORLOC_VIEW_MAX_BUFFER_BYTES")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(DEFAULT_LARGE_BUFFER_BYTES)
}

/// Return an error string when a guardrail fires. The message names
/// which guardrail hit so the user knows what `--force` unlocks.
pub fn refuse(kind: GuardrailKind, detail: &str) -> String {
    match kind {
        GuardrailKind::Size => format!(
            "{}; pass --force to proceed",
            detail
        ),
        GuardrailKind::BinaryToTty => format!(
            "refusing to write binary output to a terminal; \
             pass --force or redirect stdout"
        ),
    }
}

// ── Path helpers ──────────────────────────────────────────────────────────

/// Compute the atomic-write scratch path for a given output path.
/// `foo/out.packet` -> `foo/out.packet.partial`.
pub fn partial_path(final_path: &Path) -> PathBuf {
    let mut s = final_path.as_os_str().to_owned();
    s.push(".partial");
    PathBuf::from(s)
}

/// Rename `.partial` -> final. Best-effort; caller has already
/// verified success.
pub fn commit_partial(partial: &Path, final_path: &Path) -> Result<(), String> {
    std::fs::rename(partial, final_path).map_err(|e| {
        format!(
            "atomic rename of '{}' to '{}' failed: {}",
            partial.display(),
            final_path.display(),
            e
        )
    })
}

// ── DATA <-> DATA ────────────────────────────────────────────────────────

/// Byte-copy `in_path` to `out_path`. Used for DATA -> DATA when the
/// caller has not passed `--schema` (no re-emit needed).
pub fn data_to_data_copy(in_path: &Path, out_path: &Path) -> Result<(), String> {
    let partial = partial_path(out_path);
    std::fs::copy(in_path, &partial)
        .map_err(|e| format!("byte-copy from '{}' failed: {}", in_path.display(), e))?;
    commit_partial(&partial, out_path)
}

// ── DATA -> STREAM ───────────────────────────────────────────────────────

/// Convert a DATA_PACKET file to a STREAM_PACKET file via the runtime
/// FFI `mlc_view_data_to_stream`. Returns the number of sub-packets
/// emitted on success.
pub fn data_to_stream(
    in_path: &Path,
    out_path: &Path,
    compression_level: u8,
) -> Result<u64, String> {
    let partial = partial_path(out_path);
    let in_c = path_to_cstring(in_path)?;
    let out_c = path_to_cstring(&partial)?;

    let mut n: u64 = 0;
    let mut errmsg: *mut c_char = ptr::null_mut();
    let rc = unsafe {
        mlc_view_data_to_stream(
            in_c.as_ptr(),
            out_c.as_ptr(),
            compression_level,
            &mut n,
            &mut errmsg,
        )
    };
    if rc != 0 {
        let msg = take_c_errmsg(errmsg)
            .unwrap_or_else(|| "mlc_view_data_to_stream failed".to_string());
        let _ = std::fs::remove_file(&partial);
        return Err(msg);
    }
    commit_partial(&partial, out_path)?;
    Ok(n)
}

// ── STREAM -> STREAM ─────────────────────────────────────────────────────

/// Convert a STREAM_PACKET file to another STREAM_PACKET file via
/// `mlc_view_stream_to_stream`. Handles footer-less input as a
/// first-class case (IStream drains forward regardless). Returns the
/// number of sub-packets emitted.
pub fn stream_to_stream(
    in_path: &Path,
    out_path: &Path,
    compression_level: u8,
    schema_override: Option<&str>,
) -> Result<u64, String> {
    let partial = partial_path(out_path);
    let in_c = path_to_cstring(in_path)?;
    let out_c = path_to_cstring(&partial)?;
    let schema_c = schema_override
        .map(|s| CString::new(s).map_err(|_| "schema contains NUL".to_string()))
        .transpose()?;

    let schema_ptr = schema_c
        .as_ref()
        .map(|c| c.as_ptr())
        .unwrap_or(ptr::null());

    let mut n: u64 = 0;
    let mut errmsg: *mut c_char = ptr::null_mut();
    let rc = unsafe {
        mlc_view_stream_to_stream(
            in_c.as_ptr(),
            out_c.as_ptr(),
            compression_level,
            schema_ptr,
            &mut n,
            &mut errmsg,
        )
    };
    if rc != 0 {
        let msg = take_c_errmsg(errmsg)
            .unwrap_or_else(|| "mlc_view_stream_to_stream failed".to_string());
        let _ = std::fs::remove_file(&partial);
        return Err(msg);
    }
    commit_partial(&partial, out_path)?;
    Ok(n)
}

// ── Guardrail helper: projected size for stream -> data ─────────────────

/// Projected uncompressed payload size for a stream input, used by
/// the `--force` gate when converting stream -> data. Uses the
/// footer's `bytes_uncompressed_total` when present; for footer-less
/// files no diag exists, so the compressed file size is used as a
/// conservative lower bound (uncompressed will be at least this
/// large) -- otherwise a partially-written multi-GB stream would
/// silently bypass the guardrail.
pub fn projected_stream_uncompressed_bytes(footer: &FooterInfo, in_path: &Path) -> u64 {
    match footer {
        FooterInfo::Final { diag, .. } | FooterInfo::Temp { diag } => {
            diag.bytes_uncompressed_total
        }
        FooterInfo::Missing { .. } => {
            std::fs::metadata(in_path).map(|m| m.len()).unwrap_or(0)
        }
    }
}

/// Sum the DATA input's on-disk size as a projection for the `--force`
/// guardrail before DATA -> STREAM conversion. The IFile inline
/// region is materialised into RAM by mlc_open_ifile, so we count the
/// full file size (a conservative upper bound for the working set).
pub fn projected_data_working_set_bytes(in_path: &Path) -> u64 {
    std::fs::metadata(in_path).map(|m| m.len()).unwrap_or(0)
}

// ── Classification helpers ───────────────────────────────────────────────

/// True iff the input is a MORLOC_DATA_PACKET file (regardless of
/// format subtype).
pub fn is_morloc_data_packet(cls: &Classification) -> bool {
    matches!(cls, Classification::MorlocDataPacket { .. })
}

/// True iff the input is a MORLOC_STREAM_PACKET file (any footer state).
pub fn is_morloc_stream_packet(cls: &Classification) -> bool {
    matches!(cls, Classification::MorlocStreamPacket { .. })
}

/// Extract the stream footer state from a MORLOC_STREAM_PACKET
/// classification. Returns `None` for non-stream inputs.
pub fn stream_footer(cls: &Classification) -> Option<&FooterInfo> {
    if let Classification::MorlocStreamPacket { footer, .. } = cls {
        Some(footer)
    } else {
        None
    }
}
