//! Wire-format layout for stream-handle fields (`IFile`, `OStream`,
//! `IStream`).
//!
//! Every stream-handle field is a fixed 16-byte tagged union. The schema
//! code (`F` / `O` / `I`) carries the morloc-level type; the tag byte in
//! the field itself carries the encoding that this particular instance
//! is using. See `SerialType::OStream` in `schema.rs` for the surface
//! description.
//!
//! Layout:
//!
//! ```text
//! offset  0..1   u8   tag
//! offset  1..8        padding (zero on write, ignored on read)
//! offset  8..16  u64  payload
//! ```
//!
//! Tag values:
//!
//! - `TAG_PATH` (`0`): payload is a relptr into the suballoc region
//!   pointing at a `PathSuballoc` (`{ size: u64, bytes... }`). The
//!   receiving pool opens the path locally via `mlc_open(path, kind)`
//!   and binds a fresh handle in its own SHM registry.
//! - `TAG_HANDLE` (`1`): payload is a bare `uint64_t` slot id meaningful
//!   in the receiver's shared SHM registry. Only valid when sender and
//!   receiver share that registry (i.e. intra-nexus cross-pool). A
//!   receiver that reads `TAG_HANDLE` from a foreign-registry source
//!   sees a generation-mismatch error from `slot_ref`.
//! - `2..=255`: reserved (future content-hash / URI / inline-blob
//!   encodings). Decoders return a clean "unsupported encoding" error.

use crate::shm_types::Array;

/// Total inline width of a stream-handle field in voidstar.
pub const STREAM_HANDLE_FIELD_SIZE: usize = 16;

/// Byte offset of the tag byte within a stream-handle field.
pub const TAG_OFFSET: usize = 0;

/// Byte offset of the payload `u64` within a stream-handle field.
pub const PAYLOAD_OFFSET: usize = 8;

/// `TAG_PATH`: payload is a relptr to a `PathSuballoc`.
pub const TAG_PATH: u8 = 0;
/// `TAG_HANDLE`: payload is a bare slot-id handle.
pub const TAG_HANDLE: u8 = 1;

/// The RELNULL sentinel expressed as the `u64` payload bit pattern used
/// in `TAG_PATH` fields whose path is empty. Matches
/// `shm::RELNULL as u64` (`0xFFFF_FFFF_FFFF_FFFF`) so codec sites can
/// compare payloads to it directly without casting `isize` to `u64` at
/// every use.
pub const RELNULL_PAYLOAD: u64 = !0u64;

/// Field-size invariant: the inline layout must match `Array<u8>`'s 16
/// bytes so the schema width and alignment we report for F/O/I match the
/// inline-size invariant the voidstar walker assumes.
const _: () = {
    assert!(STREAM_HANDLE_FIELD_SIZE == std::mem::size_of::<Array>());
};

/// Read the tag byte of a stream-handle field.
///
/// # Safety
///
/// `field_ptr` must point at a 16-byte-aligned stream-handle field.
#[inline]
pub unsafe fn read_tag(field_ptr: *const u8) -> u8 {
    *field_ptr.add(TAG_OFFSET)
}

/// Read the 8-byte payload of a stream-handle field as `u64`.
///
/// # Safety
///
/// `field_ptr` must point at a 16-byte-aligned stream-handle field.
#[inline]
pub unsafe fn read_payload(field_ptr: *const u8) -> u64 {
    std::ptr::read_unaligned(field_ptr.add(PAYLOAD_OFFSET) as *const u64)
}

/// Write `tag` + `payload` into the 16 bytes at `field_ptr`. Zero-fills
/// the 7-byte padding between them so a reader inspecting the padding
/// for a tag-version discriminator sees only zeros.
///
/// # Safety
///
/// `field_ptr` must point at a writable 16-byte-aligned stream-handle
/// field.
#[inline]
pub unsafe fn write_field(field_ptr: *mut u8, tag: u8, payload: u64) {
    std::ptr::write_bytes(field_ptr, 0, STREAM_HANDLE_FIELD_SIZE);
    *field_ptr.add(TAG_OFFSET) = tag;
    std::ptr::write_unaligned(
        field_ptr.add(PAYLOAD_OFFSET) as *mut u64,
        payload,
    );
}

/// Byte length of a `PathSuballoc` carrying `path_len` path bytes:
/// `8` (length prefix) + `path_len`.
#[inline]
pub fn path_suballoc_size(path_len: usize) -> usize {
    8 + path_len
}

/// Read the path size prefix from a `PathSuballoc`.
///
/// # Safety
///
/// `suballoc_ptr` must point at a `PathSuballoc` header (`{ size: u64,
/// bytes... }`); the caller must ensure `suballoc_ptr..+8` is readable.
#[inline]
pub unsafe fn read_path_size(suballoc_ptr: *const u8) -> u64 {
    std::ptr::read_unaligned(suballoc_ptr as *const u64)
}

/// Write a path size + bytes into a freshly-allocated `PathSuballoc`
/// at `suballoc_ptr`.
///
/// # Safety
///
/// `suballoc_ptr..+8+path.len()` must be writable.
#[inline]
pub unsafe fn write_path_suballoc(suballoc_ptr: *mut u8, path: &[u8]) {
    std::ptr::write_unaligned(suballoc_ptr as *mut u64, path.len() as u64);
    std::ptr::copy_nonoverlapping(
        path.as_ptr(),
        suballoc_ptr.add(8),
        path.len(),
    );
}
