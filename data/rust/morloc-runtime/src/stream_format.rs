//! The stream file format: parsing a file's header, footer and sub-packet
//! index, mapping files, and framing sub-packets for writing. Pure: no
//! registry, lock or process state.

use std::fs::OpenOptions;
use std::os::unix::io::AsRawFd;
use std::path::Path;

use morloc_runtime_types::packet::{
    decode_stream_tail, iter_packet_metadata, read_schema_from_meta, PacketHeader,
    METADATA_TYPE_FOOTER_FINAL, METADATA_TYPE_FOOTER_STATUS, METADATA_TYPE_STREAM_DIAG,
    METADATA_TYPE_SUBPACKET_INDEX, PACKET_COMPRESSION_NONE,
    PACKET_FORMAT_VOIDSTAR, StreamDiag, STREAM_TAIL_SIZE, packet_format_name,
};
use morloc_runtime_types::schema::{parse_schema, Schema, SerialType};
use morloc_runtime_types::shm_types::{self as shm_types_crate};

use crate::error::MorlocError;
use crate::shm::AbsPtr;
use crate::stream::*;

/// Parsed form of a stream/data packet file ready to populate a
/// `ProcessLocalSlot` + SHM `RegistrySlot`. Shared by `shared_open_ifile`
/// and `shared_open_istream` since the on-disk format is identical for
/// both kinds; only post-open access semantics differ.
pub(crate) struct ParsedStreamFile {
    pub(crate) schema_str: String,
    pub(crate) value_schema: Schema,
    pub(crate) elem_schema: Schema,
    pub(crate) subpacket_entries: Vec<morloc_runtime_types::packet::SubpacketEntry>,
    pub(crate) element_count: u64,
    pub(crate) diag: Option<StreamDiag>,
    /// True iff the file is a STREAM_PACKET that carries a
    /// `METADATA_TYPE_FOOTER_FINAL` block. False for STREAM_PACKET files
    /// that only have a temp footer (or none at all), and false for
    /// DATA_PACKET files (which inherently have no footer concept; see
    /// `is_data_packet` for that signal).
    pub(crate) final_footer: bool,
    /// True iff the file is a single DATA_PACKET (the `@save` shape),
    /// false for STREAM_PACKET. DATA-packet files are self-contained
    /// and inherently complete: they have no header / footer concept,
    /// and IFile can always open them for random access.
    pub(crate) is_data_packet: bool,
    /// Byte offset of the first sub-packet header (i.e. end of the
    /// stream header). For DATA-packet files (single-packet shape) this
    /// is 0: the whole file IS the sub-packet. IStream uses this as the
    /// initial cursor position; IFile ignores it.
    pub(crate) body_start: u64,
    /// End of the last complete sub-packet: where a reader stops.
    pub(crate) data_end: u64,
}

/// Parse a mmap'd stream or data packet file into a `ParsedStreamFile`.
/// Caller is responsible for munmap on error.
pub(crate) fn parse_stream_file(
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

/// Map an already-open descriptor read-only. Used where the caller must
/// hold a lock across both the mapping and whatever it decides from it, so
/// reopening the path would defeat the lock.
pub(crate) fn mmap_fd_readonly(
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

pub(crate) fn mmap_file_readonly(path: &str) -> Result<(AbsPtr, u64), MorlocError> {
    mmap_file_readonly_keep(path).map(|(_, ptr, size)| (ptr, size))
}

/// 'mmap_file_readonly', also returning the open file.
pub(crate) fn mmap_file_readonly_keep(path: &str) -> Result<(std::fs::File, AbsPtr, u64), MorlocError> {
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

#[derive(Debug)]
pub(crate) struct StreamHeader {
    /// Schema string of the stream's element type.
    pub(crate) schema: String,
    /// Byte offset where the first sub-packet starts (immediately after
    /// the stream header's 32-byte header + metadata block).
    pub(crate) body_start: u64,
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
pub(crate) fn open_data_packet(
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

pub(crate) fn parse_stream_header(mmap_ptr: AbsPtr, size: u64) -> Result<StreamHeader, MorlocError> {
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
pub(crate) fn read_subpacket_format(
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

#[derive(Debug)]
pub(crate) struct ParsedFooter {
    /// Offset of the footer packet: where the stream's data ends.
    pub(crate) footer_start: u64,
    pub(crate) subpacket_entries: Vec<morloc_runtime_types::packet::SubpacketEntry>,
    pub(crate) element_count: u64,
    pub(crate) diag: Option<StreamDiag>,
    pub(crate) final_footer: bool,
    /// Status byte from `METADATA_TYPE_FOOTER_STATUS` if present, or
    /// `FOOTER_STATUS_CLOSED` when the block is absent (legacy footer).
    /// Parsed here so downstream tooling (`morloc-nexus file`, a
    /// future `@fstatus` intrinsic) can surface it without re-reading
    /// the footer.
    #[allow(dead_code)]
    pub(crate) footer_status: u8,
}

/// Try to read the footer at EOF; returns `Ok(None)` if no footer tail
/// magic is present (writer crashed mid-stream, or live tail). Returns
/// `Err` on corrupt footer.
pub(crate) fn try_read_footer(
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

pub(crate) fn forward_scan_subpackets(
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

/// Parse a sub-packet's header at `subpacket_off` and return it with the
/// payload's offset and length, having checked that the header, metadata and
/// payload all lie inside the mmap.
pub(crate) fn read_subpacket_header(
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

/// Read the on-disk byte size of a sub-packet at `offset`. Used by
/// `@append` to advance past the last complete sub-packet to the
/// resume cursor.
pub(crate) fn read_subpacket_size(
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

/// The sub-packet index and element count of a stream with no final
/// footer, from its complete sub-packets up to `data_end`.
pub(crate) fn index_unclosed_stream(
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

/// Push a sub-packet offset into the diag's tail-window. The window is
/// length-prefixed; once full, slide forward by overwriting the oldest.
///
/// Manipulates the packed struct via local copies because `StreamDiag`
/// is `#[repr(C, packed)]` -- direct field references are unaligned.
pub(crate) fn push_tail_window(d: &mut StreamDiag, offset: u64) {
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

pub(crate) fn unix_micros_now() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_micros() as u64)
        .unwrap_or(0)
}

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

/// `write_all` for a raw fd: loops past EINTR / partial writes.
pub(crate) fn write_all_fd(fd: i32, mut buf: &[u8]) -> Result<(), MorlocError> {
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
pub(crate) fn sendfile_range(
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
