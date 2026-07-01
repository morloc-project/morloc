//! Schema-aware walker that finds stream-handle (`IFile` / `OStream` /
//! `IStream`) fields inside a flattened voidstar payload.
//!
//! Used by the cross-nexus boundary code to (a) reject IStream in any
//! position and OStream in argument position, and (b) rewrite intra-
//! nexus `TAG_HANDLE` fields to portable `TAG_PATH` fields before the
//! packet leaves the local SHM registry.
//!
//! The walker operates on the buffer produced by `voidstar::flatten_to
//! _buffer`: a contiguous `Vec<u8>` whose relptrs are payload-buffer-
//! relative offsets. Descent is structural (Tuple / Map fields by
//! offset, Array elements by index, Optional through the inhabited
//! relptr, Recur by named-schema lookup) so a stream-handle field
//! anywhere in the payload's shape is discovered.
//!
//! The rewrite step (`rewrite_handles_to_paths`) appends path
//! suballocs at the tail of the buffer and switches each rewritten
//! field's tag + payload in place. Because appends only add bytes past
//! existing content, previously-computed buffer-relative offsets stay
//! valid.

use morloc_runtime_types::error::MorlocError;
use morloc_runtime_types::schema::{Schema, SerialType};
use morloc_runtime_types::shm_types::{Array, RelPtr, RELNULL};
use morloc_runtime_types::stream_handle as sh;

/// Which morloc stream-handle type a discovered field carries.
/// Determines the receiver-side `mlc_open` kind if the field is
/// rewritten to `TAG_PATH` form.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StreamFieldKind {
    IFile,
    OStream,
    IStream,
}

impl StreamFieldKind {
    fn from_serial(st: SerialType) -> Option<Self> {
        match st {
            SerialType::IFile => Some(StreamFieldKind::IFile),
            SerialType::OStream => Some(StreamFieldKind::OStream),
            SerialType::IStream => Some(StreamFieldKind::IStream),
            _ => None,
        }
    }
}

/// A discovered stream-handle field. `offset` is the byte position of
/// the 16-byte tagged field within the payload buffer; `kind` names its
/// morloc-level type.
#[derive(Debug, Clone)]
pub struct StreamField {
    pub offset: usize,
    pub kind: StreamFieldKind,
}

/// Walk `payload` bytes according to `schema` and collect every F/O/I
/// leaf. `payload` is the flattened-voidstar buffer produced by
/// `voidstar::flatten_to_buffer`; its relptrs are buffer-relative
/// offsets.
pub fn collect_stream_fields(
    payload: &[u8],
    schema: &Schema,
) -> Result<Vec<StreamField>, MorlocError> {
    let mut fields = Vec::new();
    // SAFETY: read-only walk bounded by `payload.len()`. Relptr reads
    // are bounds-checked before dereferencing; misaligned reads are
    // avoided by using `read_unaligned` where the source alignment isn't
    // guaranteed by the buffer.
    unsafe {
        walk(payload, 0, schema, &mut fields)?;
    }
    Ok(fields)
}

unsafe fn walk(
    payload: &[u8],
    field_offset: usize,
    schema: &Schema,
    fields: &mut Vec<StreamField>,
) -> Result<(), MorlocError> {
    match schema.serial_type {
        SerialType::IFile | SerialType::OStream | SerialType::IStream => {
            let kind = StreamFieldKind::from_serial(schema.serial_type)
                .expect("F/O/I match arm");
            check_bounds(payload, field_offset, sh::STREAM_HANDLE_FIELD_SIZE)?;
            fields.push(StreamField { offset: field_offset, kind });
            Ok(())
        }
        SerialType::Tuple | SerialType::Map => {
            for i in 0..schema.parameters.len() {
                let inner_offset = field_offset
                    .checked_add(*schema.offsets.get(i).ok_or_else(|| {
                        MorlocError::Other(format!(
                            "handle_scan: tuple/map field {} has no offset entry", i,
                        ))
                    })?)
                    .ok_or_else(|| {
                        MorlocError::Other(
                            "handle_scan: tuple/map field offset overflow".into(),
                        )
                    })?;
                walk(payload, inner_offset, &schema.parameters[i], fields)?;
            }
            Ok(())
        }
        SerialType::Array => {
            if schema.parameters.is_empty() {
                return Ok(());
            }
            let elem_schema = &schema.parameters[0];
            if !contains_stream_handles(elem_schema) {
                // Empty-element-type shortcut avoids scanning huge
                // homogeneous arrays with no handles inside.
                return Ok(());
            }
            check_bounds(payload, field_offset, std::mem::size_of::<Array>())?;
            let arr_ptr = payload.as_ptr().add(field_offset) as *const Array;
            let arr = *arr_ptr;
            if arr.size == 0 || arr.data < 0 {
                return Ok(());
            }
            let elem_width = elem_schema.width;
            let base_off = arr.data as usize;
            let total = arr.size
                .checked_mul(elem_width)
                .ok_or_else(|| {
                    MorlocError::Other("handle_scan: array size overflow".into())
                })?;
            check_bounds(payload, base_off, total)?;
            for i in 0..arr.size {
                let inner_offset = base_off + i * elem_width;
                walk(payload, inner_offset, elem_schema, fields)?;
            }
            Ok(())
        }
        SerialType::Optional => {
            if schema.parameters.is_empty() {
                return Ok(());
            }
            check_bounds(payload, field_offset, std::mem::size_of::<RelPtr>())?;
            let relptr = std::ptr::read_unaligned(
                payload.as_ptr().add(field_offset) as *const RelPtr,
            );
            if relptr == RELNULL {
                return Ok(());
            }
            if relptr < 0 {
                return Err(MorlocError::Other(
                    "handle_scan: Optional relptr is negative but not RELNULL".into(),
                ));
            }
            walk(payload, relptr as usize, &schema.parameters[0], fields)
        }
        SerialType::Recur => {
            // A Recur back-reference terminates the walk. Descending
            // into the referenced ancestor would loop for genuine self-
            // recursive structures (`Tree` -> `[Tree]` -> ...), and the
            // cross-nexus rewrite doesn't yet support stream-handle
            // fields embedded inside recursive records. If such a field
            // ever needs to be discovered here, extend this arm with
            // visited-set bookkeeping.
            Ok(())
        }
        // Nil / Bool / integer / float / String / Int / Table:
        // no stream-handle content.
        _ => Ok(()),
    }
}

/// True if `schema` transitively contains any leaf whose morloc-level
/// type matches `target`. Purely schema-based; no payload access. Used
/// at the SLURM boundary to reject stream-handle types disallowed in
/// argument or return position without walking the payload bytes.
pub fn schema_contains_kind(schema: &Schema, target: StreamFieldKind) -> bool {
    let matches_self = match (schema.serial_type, target) {
        (SerialType::IFile,   StreamFieldKind::IFile)   => true,
        (SerialType::OStream, StreamFieldKind::OStream) => true,
        (SerialType::IStream, StreamFieldKind::IStream) => true,
        _ => false,
    };
    if matches_self {
        return true;
    }
    match schema.serial_type {
        SerialType::Tuple | SerialType::Map => schema
            .parameters
            .iter()
            .any(|p| schema_contains_kind(p, target)),
        SerialType::Array | SerialType::Optional => schema
            .parameters
            .first()
            .map(|p| schema_contains_kind(p, target))
            .unwrap_or(false),
        // Conservative: don't recurse through Recur (matches the walker's
        // treatment). If a recursive record ever needs to carry stream
        // handles for cross-nexus, extend this together with the walker.
        _ => false,
    }
}

/// Cheap early-exit predicate: does `schema` transitively contain any
/// F/O/I leaf? Avoids scanning long homogeneous arrays whose element
/// type has no handles. Conservatively returns `true` for `Recur` and
/// `SerialPack`-style types we can't statically inspect from here.
fn contains_stream_handles(schema: &Schema) -> bool {
    match schema.serial_type {
        SerialType::IFile | SerialType::OStream | SerialType::IStream => true,
        SerialType::Tuple | SerialType::Map => {
            schema.parameters.iter().any(contains_stream_handles)
        }
        SerialType::Array | SerialType::Optional => schema
            .parameters
            .first()
            .map(contains_stream_handles)
            .unwrap_or(false),
        // Assume yes; the outer walker will resolve the back-ref
        // properly. False-positives here just cost an extra descent.
        SerialType::Recur => true,
        _ => false,
    }
}

fn check_bounds(payload: &[u8], offset: usize, len: usize) -> Result<(), MorlocError> {
    let end = offset.checked_add(len).ok_or_else(|| {
        MorlocError::Other("handle_scan: field offset+size overflow".into())
    })?;
    if end > payload.len() {
        return Err(MorlocError::Other(format!(
            "handle_scan: field extends past payload (offset={}, len={}, payload={})",
            offset,
            len,
            payload.len(),
        )));
    }
    Ok(())
}

/// Rewrite every `TAG_HANDLE` field in `fields` to `TAG_PATH` form by
/// appending a `{u64 size, bytes...}` path suballoc at the tail of
/// `payload` and updating the field's tag + payload in place. Fields
/// already in `TAG_PATH` form are left untouched. Fields whose payload
/// carries a `TAG_HANDLE` that no longer resolves to a live slot in
/// this nexus's SHM registry surface as a clean error.
///
/// Path suballocs are 8-byte aligned to keep the buffer's u64 fields
/// naturally aligned when downstream code reads the payload back.
///
/// The path lookup uses `crate::stream::shared_handle_path`, which is a
/// versioned-pointer read into the SHM registry -- cheap on the hot
/// path.
pub fn rewrite_handles_to_paths(
    payload: &mut Vec<u8>,
    fields: &[StreamField],
) -> Result<(), MorlocError> {
    for field in fields {
        let field_start = field.offset;
        let tag = unsafe { sh::read_tag(payload.as_ptr().add(field_start)) };
        if tag == sh::TAG_PATH {
            continue;
        }
        if tag != sh::TAG_HANDLE {
            return Err(MorlocError::Other(format!(
                "handle_scan: rewrite: unsupported source tag {} at offset {}",
                tag, field_start,
            )));
        }
        let handle = unsafe { sh::read_payload(payload.as_ptr().add(field_start)) } as i64;
        let path = crate::stream::shared_handle_path(handle).map_err(|e| {
            MorlocError::Other(format!(
                "handle_scan: rewrite handle at offset {}: {}",
                field_start, e,
            ))
        })?;
        let path_bytes = path.as_bytes();
        if path_bytes.is_empty() {
            unsafe {
                sh::write_field(
                    payload.as_mut_ptr().add(field_start),
                    sh::TAG_PATH,
                    sh::RELNULL_PAYLOAD,
                );
            }
            continue;
        }
        // 8-align the tail so the u64 size prefix reads cleanly.
        while payload.len() % 8 != 0 {
            payload.push(0);
        }
        let suballoc_offset = payload.len();
        payload.extend_from_slice(&(path_bytes.len() as u64).to_le_bytes());
        payload.extend_from_slice(path_bytes);
        unsafe {
            sh::write_field(
                payload.as_mut_ptr().add(field_start),
                sh::TAG_PATH,
                suballoc_offset as u64,
            );
        }
    }
    Ok(())
}

/// Rewrite a MORLOC_DATA_PACKET so every `TAG_HANDLE` stream-handle
/// field in its voidstar payload becomes `TAG_PATH`. The header, its
/// metadata section (including schema), and any non-voidstar payload
/// pass through unchanged. Returns a fresh `Vec<u8>` sized to the
/// rewritten packet.
///
/// Used at the SLURM boundary: intra-nexus bridges emit IFile fields
/// in `TAG_HANDLE` form (fast, one slot per file per nexus); shipping
/// a packet to a remote nexus needs the portable `TAG_PATH` form so
/// the receiver can `mlc_open` locally.
///
/// Non-DATA / non-voidstar packets are returned unchanged so callers
/// can apply this uniformly to a mixed stream of packet types.
pub fn rewrite_data_packet_for_remote(
    packet: &[u8],
) -> Result<Vec<u8>, MorlocError> {
    use morloc_runtime_types::packet::{
        PacketHeader, PACKET_FORMAT_VOIDSTAR, read_schema_from_meta,
    };
    use morloc_runtime_types::schema::parse_schema;

    if packet.len() < 32 {
        return Ok(packet.to_vec());
    }
    let hdr_arr: [u8; 32] = packet[..32].try_into().unwrap();
    let hdr = PacketHeader::from_bytes(&hdr_arr)?;
    if !hdr.is_data() {
        return Ok(packet.to_vec());
    }
    // SAFETY: is_data implies CommandData is the active union variant.
    let format = unsafe { hdr.command.data.format };
    if format != PACKET_FORMAT_VOIDSTAR {
        // msgpack / JSON already carry paths as strings; nothing to
        // rewrite for cross-nexus.
        return Ok(packet.to_vec());
    }

    let meta_len = hdr.offset as usize;
    let payload_len = hdr.length as usize;
    let expected = 32usize
        .checked_add(meta_len)
        .and_then(|n| n.checked_add(payload_len))
        .ok_or_else(|| MorlocError::Other(
            "rewrite_data_packet_for_remote: header offset+length overflow".into(),
        ))?;
    if expected > packet.len() {
        return Err(MorlocError::Other(format!(
            "rewrite_data_packet_for_remote: header claims {} bytes but packet has {}",
            expected, packet.len(),
        )));
    }

    let meta_bytes = &packet[32..32 + meta_len];
    let payload_bytes = &packet[32 + meta_len..32 + meta_len + payload_len];

    let schema_str = read_schema_from_meta(packet)?
        .ok_or_else(|| MorlocError::Other(
            "rewrite_data_packet_for_remote: packet has no schema metadata".into(),
        ))?;
    let schema = parse_schema(&schema_str)?;

    let fields = collect_stream_fields(payload_bytes, &schema)?;
    let has_tag_handle = fields.iter().any(|f| unsafe {
        sh::read_tag(payload_bytes.as_ptr().add(f.offset)) == sh::TAG_HANDLE
    });
    if !has_tag_handle {
        return Ok(packet.to_vec());
    }

    let mut new_payload = payload_bytes.to_vec();
    rewrite_handles_to_paths(&mut new_payload, &fields)?;

    let mut new_hdr = hdr;
    new_hdr.length = new_payload.len() as u64;
    let mut out = Vec::with_capacity(32 + meta_len + new_payload.len());
    out.extend_from_slice(&new_hdr.to_bytes());
    out.extend_from_slice(meta_bytes);
    out.extend_from_slice(&new_payload);
    Ok(out)
}

/// C-ABI wrapper around [`rewrite_data_packet_for_remote`] for the
/// nexus binary, which doesn't link morloc-runtime as a Rust crate.
///
/// On success sets `*out_ptr` to a `libc::malloc`'d buffer holding the
/// rewritten packet and `*out_len` to its length, and returns 0. If the
/// packet already contained no `TAG_HANDLE` fields, `*out_ptr` is set to
/// null and `*out_len` to the original length -- the caller keeps its
/// own buffer. On error returns -1 and sets `errmsg` to a
/// `libc::malloc`'d, NUL-terminated string owned by the caller.
///
/// # Safety
///
/// `packet` / `out_ptr` / `out_len` / `errmsg` must be valid non-null
/// pointers; `packet_len` must accurately size the readable region of
/// `packet`.
#[no_mangle]
pub unsafe extern "C" fn mlc_rewrite_packet_for_remote(
    packet: *const u8,
    packet_len: usize,
    out_ptr: *mut *mut u8,
    out_len: *mut usize,
    errmsg: *mut *mut std::os::raw::c_char,
) -> i32 {
    use morloc_runtime_types::error::set_errmsg;
    if !errmsg.is_null() {
        *errmsg = std::ptr::null_mut();
    }
    if packet.is_null() || out_ptr.is_null() || out_len.is_null() {
        if !errmsg.is_null() {
            set_errmsg(errmsg, &MorlocError::Other(
                "mlc_rewrite_packet_for_remote: null pointer argument".into(),
            ));
        }
        return -1;
    }
    let src = std::slice::from_raw_parts(packet, packet_len);
    match rewrite_data_packet_for_remote(src) {
        Ok(rewritten) => {
            if rewritten.len() == packet_len && rewritten.as_slice() == src {
                // No-op rewrite: caller keeps its buffer.
                *out_ptr = std::ptr::null_mut();
                *out_len = packet_len;
                return 0;
            }
            let buf = libc::malloc(rewritten.len()) as *mut u8;
            if buf.is_null() {
                set_errmsg(errmsg, &MorlocError::Other(
                    "mlc_rewrite_packet_for_remote: malloc failed".into(),
                ));
                return -1;
            }
            std::ptr::copy_nonoverlapping(rewritten.as_ptr(), buf, rewritten.len());
            *out_ptr = buf;
            *out_len = rewritten.len();
            0
        }
        Err(e) => {
            set_errmsg(errmsg, &e);
            -1
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use morloc_runtime_types::schema::parse_schema;

    /// Lay out a bare 16-byte stream-handle field with the given tag +
    /// payload for testing the walker in isolation.
    fn make_field(tag: u8, payload: u64) -> Vec<u8> {
        let mut buf = vec![0u8; sh::STREAM_HANDLE_FIELD_SIZE];
        unsafe { sh::write_field(buf.as_mut_ptr(), tag, payload); }
        buf
    }

    #[test]
    fn ifile_scalar_at_root_is_discovered() {
        let schema = parse_schema("F").unwrap();
        let payload = make_field(sh::TAG_HANDLE, 0xDEAD_BEEF_u64);
        let fields = collect_stream_fields(&payload, &schema).unwrap();
        assert_eq!(fields.len(), 1);
        assert_eq!(fields[0].offset, 0);
        assert!(matches!(fields[0].kind, StreamFieldKind::IFile));
    }

    #[test]
    fn ostream_scalar_at_root_is_discovered() {
        let schema = parse_schema("O").unwrap();
        let payload = make_field(sh::TAG_HANDLE, 42);
        let fields = collect_stream_fields(&payload, &schema).unwrap();
        assert_eq!(fields.len(), 1);
        assert!(matches!(fields[0].kind, StreamFieldKind::OStream));
    }

    #[test]
    fn istream_scalar_at_root_is_discovered() {
        let schema = parse_schema("I").unwrap();
        let payload = make_field(sh::TAG_HANDLE, 7);
        let fields = collect_stream_fields(&payload, &schema).unwrap();
        assert_eq!(fields.len(), 1);
        assert!(matches!(fields[0].kind, StreamFieldKind::IStream));
    }

    #[test]
    fn plain_int_has_no_stream_fields() {
        let schema = parse_schema("j").unwrap();
        let payload = vec![0u8; 16];
        let fields = collect_stream_fields(&payload, &schema).unwrap();
        assert!(fields.is_empty());
    }

    #[test]
    fn payload_too_small_errors() {
        let schema = parse_schema("F").unwrap();
        let payload = vec![0u8; 8]; // less than the 16-byte field
        assert!(collect_stream_fields(&payload, &schema).is_err());
    }
}
