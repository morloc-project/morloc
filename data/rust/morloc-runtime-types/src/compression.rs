//! Zstd compression for morloc packet payloads.
//!
//! The packet header reserves a `compression` byte; this module is the only
//! place that maps that byte to a real codec. The on-disk contract is:
//!
//! * The 32-byte header and the metadata block (header.offset bytes) stay
//!   in the clear. Decoders must read schema metadata before decompressing.
//! * The payload region (the next header.length bytes) carries either raw
//!   bytes (compression == PACKET_COMPRESSION_NONE) or a complete zstd
//!   frame (compression == PACKET_COMPRESSION_ZSTD).
//! * On compression, header.length is updated to the compressed size; on
//!   decompression it is restored to the original size.
//!
//! User-facing level 0-9 maps to zstd presets (see `CompressionLevel`).
//! Multithreaded compression is enabled automatically for payloads above
//! 1 MiB via `choose_workers`; decompression is single-threaded by the
//! zstd frame format.
//!
//! Public surface:
//! * `CompressionLevel::from_u8` / `zstd_level` / `use_long` / `is_none`
//! * `compress_payload_zstd` / `decompress_payload_zstd`     (raw bytes)
//! * `compress_packet`        / `decompress_packet`           (full packets)
//! * `decompress_packet_if_needed`                            (no-op for
//!   non-packets and uncompressed packets; used by `mlc_load`)

use std::io::{Read, Write};

use crate::error::MorlocError;
use crate::packet::{
    PacketHeader, PACKET_COMPRESSION_NONE, PACKET_COMPRESSION_ZSTD, PACKET_MAGIC,
    PACKET_SOURCE_MESG, PACKET_TYPE_DATA,
};

// ── Compression level (0-9 user-facing, mapped to zstd 1-22) ──────────────

/// User-facing compression preset. 0 disables compression; 1-9 select a
/// zstd level + long-mode combination. Constructed via `from_u8`, which
/// rejects out-of-range values before any compression work starts.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CompressionLevel(u8);

impl CompressionLevel {
    /// Build a CompressionLevel from the raw user input.
    pub fn from_u8(n: u8) -> Result<Self, MorlocError> {
        if n > 9 {
            return Err(MorlocError::Other(format!(
                "compression level must be in 0..=9, got {n}"
            )));
        }
        Ok(CompressionLevel(n))
    }

    pub fn raw(self) -> u8 {
        self.0
    }

    pub fn is_none(self) -> bool {
        self.0 == 0
    }

    /// Map the preset to a concrete zstd compression level.
    pub fn zstd_level(self) -> i32 {
        match self.0 {
            0 => 0,
            1 => 1,
            2 => 3,
            3 => 6,
            4 => 9,
            5 => 12,
            6 => 15,
            7 => 19,
            8 => 21,
            _ => 22,
        }
    }

    /// Whether to enable zstd long-range mode (~128 MB window). Beneficial
    /// for very large redundant payloads (genomic data with repeats across
    /// far distances); kicks in at the ultra levels.
    pub fn use_long(self) -> bool {
        self.0 >= 7
    }
}

// ── Worker-count heuristic ────────────────────────────────────────────────

/// Choose the number of zstd worker threads to spawn for a payload of
/// the given size. Returns 0 (single-threaded) below the threshold where
/// the spawn and bookkeeping cost would exceed the parallelization win.
///
/// Heuristic: ~1 worker per 4 MiB of input, capped at the number of
/// available cores and at 16 to avoid pathological scaling on big
/// servers where marginal returns vanish.
pub fn choose_workers(payload_bytes: usize) -> u32 {
    if payload_bytes < (1 << 20) {
        return 0;
    }
    let cores = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let by_size = (payload_bytes / (4 << 20)).max(1);
    by_size.min(cores).min(16) as u32
}

// ── Long-mode window logs ─────────────────────────────────────────────────

/// Encoder window log used when long-mode is active. 27 == 128 MiB,
/// matching the zstd CLI's --long default.
const LONG_WINDOW_LOG: u32 = 27;

/// Decoder windowLogMax: must cover any window an encoder might have used.
/// 31 == 2 GiB, the maximum zstd permits.
const DECODER_WINDOW_LOG_MAX: u32 = 31;

// ── Raw-payload compress / decompress ─────────────────────────────────────

/// Compress raw bytes with zstd. Multithreaded when the payload is large
/// enough to amortize worker overhead; long-mode enabled for preset 7+.
pub fn compress_payload_zstd(
    raw: &[u8],
    lvl: CompressionLevel,
) -> Result<Vec<u8>, MorlocError> {
    if lvl.is_none() {
        return Err(MorlocError::Other(
            "compress_payload_zstd called with level 0".into(),
        ));
    }

    let mut encoder = zstd::stream::Encoder::new(Vec::new(), lvl.zstd_level())
        .map_err(MorlocError::Io)?;

    let workers = choose_workers(raw.len());
    if workers > 0 {
        encoder
            .multithread(workers)
            .map_err(MorlocError::Io)?;
    }
    if lvl.use_long() {
        encoder
            .window_log(LONG_WINDOW_LOG)
            .map_err(MorlocError::Io)?;
    }

    encoder.write_all(raw).map_err(MorlocError::Io)?;
    encoder.finish().map_err(MorlocError::Io)
}

/// Decompress a zstd frame back to raw bytes. The decoder's windowLogMax
/// is raised so long-mode frames decode without error.
pub fn decompress_payload_zstd(compressed: &[u8]) -> Result<Vec<u8>, MorlocError> {
    let mut decoder = zstd::stream::Decoder::new(compressed).map_err(MorlocError::Io)?;
    decoder
        .window_log_max(DECODER_WINDOW_LOG_MAX)
        .map_err(MorlocError::Io)?;
    let mut out = Vec::new();
    decoder.read_to_end(&mut out).map_err(MorlocError::Io)?;
    Ok(out)
}

// ── Packet-level compress / decompress ────────────────────────────────────

/// Result of `compress_packet`. `Compressed` carries the new packet
/// bytes; `NoOp` indicates the input was returned unchanged (only DATA
/// packets with source MESG are eligible).
pub enum CompressOutcome {
    Compressed(Vec<u8>),
    NoOp,
}

/// Compress the payload region of a complete morloc packet. The 32-byte
/// header and metadata block are passed through; the header's
/// `compression` byte is set to `PACKET_COMPRESSION_ZSTD` and `length`
/// is updated to the compressed size. Returns `NoOp` for command types
/// that should not be compressed (CALL/PING, or DATA packets whose source
/// is FILE/RPTR rather than MESG -- a FILE payload is a path, RPTR is a
/// shared-memory offset, neither benefits from compression and the
/// downstream consumer would not know how to interpret the result).
pub fn compress_packet(
    packet: &[u8],
    lvl: CompressionLevel,
) -> Result<CompressOutcome, MorlocError> {
    if lvl.is_none() {
        return Ok(CompressOutcome::NoOp);
    }
    if packet.len() < 32 {
        return Err(MorlocError::Packet(format!(
            "packet too small to compress: {} bytes",
            packet.len()
        )));
    }
    let mut hdr_buf = [0u8; 32];
    hdr_buf.copy_from_slice(&packet[..32]);
    let header = PacketHeader::from_bytes(&hdr_buf)?;

    // Only DATA + MESG packets are eligible; everything else passes through.
    if !header.is_data() {
        return Ok(CompressOutcome::NoOp);
    }
    let data = unsafe { header.command.data };
    if data.source != PACKET_SOURCE_MESG {
        return Ok(CompressOutcome::NoOp);
    }
    if data.compression != PACKET_COMPRESSION_NONE {
        return Err(MorlocError::Packet(format!(
            "packet already carries compression byte 0x{:02x}",
            data.compression
        )));
    }

    let offset = { header.offset } as usize;
    let length = { header.length } as usize;
    let payload_start = 32 + offset;
    let payload_end = payload_start
        .checked_add(length)
        .ok_or_else(|| MorlocError::Packet("offset+length overflow".into()))?;
    if payload_end > packet.len() {
        return Err(MorlocError::Packet(
            "payload region extends past packet end".into(),
        ));
    }

    let compressed = compress_payload_zstd(&packet[payload_start..payload_end], lvl)?;

    let mut new_header = header;
    // Writing a union field is safe in Rust (only reads require unsafe).
    new_header.command.data.compression = PACKET_COMPRESSION_ZSTD;
    // Rewriting the packed `length` field via the typed setter would
    // require an unaligned ref; use a byte-level write at the known offset
    // (length is at byte 24 of the 32-byte header).
    let mut new_hdr_bytes = new_header.to_bytes();
    let compressed_len = compressed.len() as u64;
    new_hdr_bytes[24..32].copy_from_slice(&compressed_len.to_le_bytes());

    let mut out = Vec::with_capacity(32 + offset + compressed.len());
    out.extend_from_slice(&new_hdr_bytes);
    out.extend_from_slice(&packet[32..payload_start]); // metadata
    out.extend_from_slice(&compressed);
    Ok(CompressOutcome::Compressed(out))
}

/// Inverse of `compress_packet`. If the header's compression byte is
/// PACKET_COMPRESSION_NONE returns the input unchanged (as a cloned Vec
/// to keep the return type uniform). If PACKET_COMPRESSION_ZSTD
/// decompresses the payload and emits a new packet with compression
/// byte cleared and length restored. Unknown compression bytes are an
/// error.
pub fn decompress_packet(packet: &[u8]) -> Result<Vec<u8>, MorlocError> {
    if packet.len() < 32 {
        return Err(MorlocError::Packet(format!(
            "packet too small to decompress: {} bytes",
            packet.len()
        )));
    }
    let mut hdr_buf = [0u8; 32];
    hdr_buf.copy_from_slice(&packet[..32]);
    let header = PacketHeader::from_bytes(&hdr_buf)?;

    if !header.is_data() {
        return Ok(packet.to_vec());
    }
    let data = unsafe { header.command.data };
    match data.compression {
        PACKET_COMPRESSION_NONE => Ok(packet.to_vec()),
        PACKET_COMPRESSION_ZSTD => {
            let offset = { header.offset } as usize;
            let length = { header.length } as usize;
            let payload_start = 32 + offset;
            let payload_end = payload_start
                .checked_add(length)
                .ok_or_else(|| MorlocError::Packet("offset+length overflow".into()))?;
            if payload_end > packet.len() {
                return Err(MorlocError::Packet(
                    "compressed payload extends past packet end".into(),
                ));
            }
            let raw = decompress_payload_zstd(&packet[payload_start..payload_end])?;
            let mut new_header = header;
            // Writing a union field is safe (only reads require unsafe).
            new_header.command.data.compression = PACKET_COMPRESSION_NONE;
            let mut new_hdr_bytes = new_header.to_bytes();
            let raw_len = raw.len() as u64;
            new_hdr_bytes[24..32].copy_from_slice(&raw_len.to_le_bytes());

            let mut out = Vec::with_capacity(32 + offset + raw.len());
            out.extend_from_slice(&new_hdr_bytes);
            out.extend_from_slice(&packet[32..payload_start]); // metadata
            out.extend_from_slice(&raw);
            Ok(out)
        }
        other => Err(MorlocError::Packet(format!(
            "unknown compression byte 0x{other:02x}"
        ))),
    }
}

/// Best-effort decompress used by `mlc_load`. Detects whether the bytes
/// are a morloc packet at all (by magic check) and, if so, applies
/// `decompress_packet`. Non-packet inputs (raw JSON, raw msgpack) are
/// passed through verbatim; the downstream format detector handles them.
pub fn decompress_packet_if_needed(bytes: &[u8]) -> Result<Vec<u8>, MorlocError> {
    if bytes.len() < 4 {
        return Ok(bytes.to_vec());
    }
    let magic = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
    if magic != PACKET_MAGIC {
        return Ok(bytes.to_vec());
    }
    if bytes.len() < 32 {
        // Looks like a packet by magic but is truncated -- let the
        // downstream packet validator surface the error rather than us.
        return Ok(bytes.to_vec());
    }
    let mut hdr_buf = [0u8; 32];
    hdr_buf.copy_from_slice(&bytes[..32]);
    let header = PacketHeader::from_bytes(&hdr_buf)?;
    if header.command_type() != PACKET_TYPE_DATA {
        return Ok(bytes.to_vec());
    }
    let comp = unsafe { header.command.data.compression };
    if comp == PACKET_COMPRESSION_NONE {
        return Ok(bytes.to_vec());
    }
    decompress_packet(bytes)
}

// ── Tests ─────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packet::{make_mesg_data_packet, PACKET_FORMAT_MSGPACK};
    use crate::schema::parse_schema;

    /// A msgpack-shaped payload that compresses cleanly: a repeating byte
    /// pattern guarantees high redundancy regardless of zstd level.
    fn synthetic_packet(payload_len: usize) -> Vec<u8> {
        let payload: Vec<u8> = (0..payload_len).map(|i| (i & 0x07) as u8).collect();
        let schema = parse_schema("au1").unwrap();
        // make_mesg_data_packet expects valid msgpack-ish bytes but only
        // wraps them; it does not parse. Reuse it so the metadata + header
        // construction matches the real path.
        make_mesg_data_packet(&payload, &schema)
    }

    #[test]
    fn roundtrip_each_level() {
        // ~1 MiB so multithreading kicks in for at least one level.
        let original = synthetic_packet(1 << 20);
        for n in 1u8..=9 {
            let lvl = CompressionLevel::from_u8(n).unwrap();
            let outcome = compress_packet(&original, lvl).unwrap();
            let compressed = match outcome {
                CompressOutcome::Compressed(v) => v,
                CompressOutcome::NoOp => {
                    panic!("expected compression at level {n}, got NoOp");
                }
            };
            assert!(
                compressed.len() < original.len(),
                "level {n}: compressed {} >= original {}",
                compressed.len(),
                original.len()
            );
            let recovered = decompress_packet(&compressed).unwrap();
            assert_eq!(
                recovered, original,
                "level {n}: roundtrip mismatch"
            );
        }
    }

    #[test]
    fn level_zero_is_noop() {
        let packet = synthetic_packet(4096);
        let outcome = compress_packet(&packet, CompressionLevel::from_u8(0).unwrap()).unwrap();
        assert!(matches!(outcome, CompressOutcome::NoOp));
    }

    #[test]
    fn reject_unknown_compression_byte() {
        let mut packet = synthetic_packet(4096);
        // Byte 15 of the header is the data-command compression slot.
        packet[15] = 0xFE;
        let err = decompress_packet(&packet).unwrap_err();
        match err {
            MorlocError::Packet(_) => {}
            other => panic!("expected Packet error, got {other:?}"),
        }
    }

    #[test]
    fn accept_long_range_frames() {
        // Level 7 enables long mode. Ensure our decoder handles it.
        let original = synthetic_packet(1 << 20);
        let lvl = CompressionLevel::from_u8(7).unwrap();
        let compressed = match compress_packet(&original, lvl).unwrap() {
            CompressOutcome::Compressed(v) => v,
            CompressOutcome::NoOp => panic!("expected compression at level 7"),
        };
        let recovered = decompress_packet(&compressed).unwrap();
        assert_eq!(recovered, original);
    }

    #[test]
    fn choose_workers_thresholds() {
        assert_eq!(choose_workers(512 * 1024), 0, "sub-MiB single-threaded");
        assert!(choose_workers(1 << 20) >= 1, "1 MiB triggers >=1 worker");
        assert!(choose_workers(8usize << 30) <= 16, "cap at 16 workers");
    }

    #[test]
    fn decompress_packet_if_needed_passes_through_non_packet() {
        // Raw JSON-looking bytes (no morloc magic) must come back unchanged.
        let bytes = b"{\"x\":1}".to_vec();
        let out = decompress_packet_if_needed(&bytes).unwrap();
        assert_eq!(out, bytes);
    }

    #[test]
    fn decompress_packet_if_needed_passes_through_uncompressed_packet() {
        let packet = synthetic_packet(4096);
        let out = decompress_packet_if_needed(&packet).unwrap();
        assert_eq!(out, packet);
    }
}
