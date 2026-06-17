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
//! 1 MiB via `choose_workers`; decompression is single-threaded because
//! the underlying libzstd stream decoder does not expose a worker knob
//! and a single zstd frame cannot be decoded by parallel workers.
//! Multithreaded decompression would require a multi-frame compressed
//! format on the encoder side.
//!
//! Public surface:
//! * `CompressionLevel::from_u8` / `zstd_level` / `use_long` / `is_none`
//! * `compress_payload_zstd` / `decompress_payload_zstd`     (raw bytes)
//! * `compress_packet`        / `decompress_packet`           (full packets)
//! * `decompress_packet_if_needed`                            (no-op for
//!   non-packets and uncompressed packets; used by `mlc_load`)

use std::borrow::Cow;
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
pub const LONG_WINDOW_LOG: u32 = 27;

/// Decoder windowLogMax: must cover any window an encoder might have used.
/// 31 == 2 GiB, the maximum zstd permits.
const DECODER_WINDOW_LOG_MAX: u32 = 31;

/// Buffer between the byte-producing walker (e.g. voidstar flatten) and
/// the encoder. Sized to fit comfortably in L2 cache while still being
/// big enough to amortize per-call dispatch over thousands of small
/// struct-slot writes. 256 KiB is a good compromise on modern CPUs.
const STREAM_INPUT_BUFFER: usize = 256 * 1024;

// ── Raw-payload compress / decompress ─────────────────────────────────────

/// Compress raw bytes with zstd. Single entry point for every
/// compression site in the runtime: takes a user-facing level (0..=9)
/// and decides threading internally via `choose_workers`. Long-mode
/// kicks in for preset 7+. Level 0 returns a clean copy of `raw`
/// (no zstd frame), so callers don't have to special-case it.
pub fn compress_payload_zstd(
    raw: &[u8],
    lvl: CompressionLevel,
) -> Result<Vec<u8>, MorlocError> {
    if lvl.is_none() {
        return Ok(raw.to_vec());
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
///
/// Single-threaded by design: libzstd's stream decoder does not expose
/// a worker-thread knob (`DParameter` only carries `WindowLogMax` and
/// `Format`). Multithreaded decompression in zstd requires the input
/// to be a sequence of independent frames so workers can decode them
/// in parallel; our compressor emits a single frame per payload, so
/// there is nothing for workers to split across. If multithreaded
/// decompression becomes worth chasing, the path is on the encoder
/// side: split the payload into N chunked frames at compress time and
/// emit a small frame index in the metadata block, then dispatch the
/// frames to workers at decompress time.
pub fn decompress_payload_zstd(compressed: &[u8]) -> Result<Vec<u8>, MorlocError> {
    let mut decoder = zstd::stream::Decoder::new(compressed).map_err(MorlocError::Io)?;
    decoder
        .window_log_max(DECODER_WINDOW_LOG_MAX)
        .map_err(MorlocError::Io)?;
    let mut out = Vec::new();
    decoder.read_to_end(&mut out).map_err(MorlocError::Io)?;
    Ok(out)
}

// ── Streaming encoders (no intermediate Vec for the compressed bytes) ────

/// Stream-compress everything pulled from `reader` into `writer`.
/// Used when neither side needs to be fully buffered in memory --
/// e.g. compressing a multi-GB file straight to disk. Worker count
/// is computed from `estimated_input_bytes` via `choose_workers`, so
/// the caller is responsible for passing a sensible estimate (e.g.
/// `fstat` size for a source file).
pub fn stream_encode_reader<W: Write, R: Read>(
    writer: W,
    reader: &mut R,
    lvl: CompressionLevel,
    estimated_input_bytes: usize,
) -> Result<(), MorlocError> {
    if lvl.is_none() {
        // std::io::copy uses its own 8 KiB ping-pong buffer, so a
        // BufWriter here would just double-buffer; route directly.
        let mut w = writer;
        std::io::copy(reader, &mut w).map_err(MorlocError::Io)?;
        return Ok(());
    }
    let workers = choose_workers(estimated_input_bytes);
    let mut encoder = zstd::stream::write::Encoder::new(writer, lvl.zstd_level())
        .map_err(MorlocError::Io)?;
    if workers > 0 {
        encoder.multithread(workers).map_err(MorlocError::Io)?;
    }
    if lvl.use_long() {
        encoder.window_log(LONG_WINDOW_LOG).map_err(MorlocError::Io)?;
    }
    std::io::copy(reader, &mut encoder).map_err(MorlocError::Io)?;
    encoder.finish().map_err(MorlocError::Io)?;
    Ok(())
}

/// Run a writer-producing closure with optional zstd compression
/// applied to its output. Used by streaming paths that need to
/// hand a `&mut dyn Write` sink to a flatten-and-emit routine but
/// also want compression wrapped transparently around the sink.
/// At level 0 the closure writes directly to `writer`; at level
/// 1-9 it writes to a multithreaded encoder whose output goes to
/// `writer`. Worker count is computed from `estimated_input_bytes`
/// via `choose_workers`, so the caller is responsible for passing a
/// sensible estimate (e.g. `calc_voidstar_size_inner` for a SHM
/// voidstar that the closure is about to flatten).
pub fn stream_encode_with<W, F>(
    writer: W,
    lvl: Option<CompressionLevel>,
    estimated_input_bytes: usize,
    body: F,
) -> Result<(), MorlocError>
where
    W: Write,
    F: FnOnce(&mut dyn Write) -> Result<(), MorlocError>,
{
    // Buffer between the byte producer and the encoder / fd: a flatten
    // walk that emits per-relptr u64s would otherwise generate one
    // Write::write_all dispatch per field.
    let is_compress = matches!(lvl, Some(l) if !l.is_none());
    if !is_compress {
        let mut buf = std::io::BufWriter::with_capacity(STREAM_INPUT_BUFFER, writer);
        body(&mut buf)?;
        return buf.flush().map_err(MorlocError::Io);
    }
    let lvl = lvl.unwrap();
    let workers = choose_workers(estimated_input_bytes);
    let mut encoder = zstd::stream::write::Encoder::new(writer, lvl.zstd_level())
        .map_err(MorlocError::Io)?;
    if workers > 0 {
        encoder.multithread(workers).map_err(MorlocError::Io)?;
    }
    if lvl.use_long() {
        encoder.window_log(LONG_WINDOW_LOG).map_err(MorlocError::Io)?;
    }
    let mut buf = std::io::BufWriter::with_capacity(STREAM_INPUT_BUFFER, encoder);
    body(&mut buf)?;
    buf.flush().map_err(MorlocError::Io)?;
    let encoder = buf
        .into_inner()
        .map_err(|e| MorlocError::Io(e.into_error()))?;
    encoder.finish().map_err(MorlocError::Io)?;
    Ok(())
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

/// Best-effort decompress used by `mlc_load` and `--call-packet` ingest.
/// Detects whether the bytes are a morloc packet at all (by magic check)
/// and, if so, applies `decompress_packet`. Non-packet inputs (raw JSON,
/// raw msgpack) are passed through verbatim; the downstream format
/// detector handles them. Returns `Cow::Borrowed` on every pass-through
/// path so callers do not pay a full copy on the common case where the
/// input was not compressed.
pub fn decompress_packet_if_needed(bytes: &[u8]) -> Result<Cow<'_, [u8]>, MorlocError> {
    if bytes.len() < 4 {
        return Ok(Cow::Borrowed(bytes));
    }
    let magic = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
    if magic != PACKET_MAGIC {
        return Ok(Cow::Borrowed(bytes));
    }
    if bytes.len() < 32 {
        // Looks like a packet by magic but is truncated -- let the
        // downstream packet validator surface the error rather than us.
        return Ok(Cow::Borrowed(bytes));
    }
    let mut hdr_buf = [0u8; 32];
    hdr_buf.copy_from_slice(&bytes[..32]);
    let header = PacketHeader::from_bytes(&hdr_buf)?;
    if header.command_type() != PACKET_TYPE_DATA {
        return Ok(Cow::Borrowed(bytes));
    }
    let comp = unsafe { header.command.data.compression };
    if comp == PACKET_COMPRESSION_NONE {
        return Ok(Cow::Borrowed(bytes));
    }
    decompress_packet(bytes).map(Cow::Owned)
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
        assert!(matches!(out, Cow::Borrowed(_)), "expected zero-copy passthrough");
        assert_eq!(out.as_ref(), bytes.as_slice());
    }

    #[test]
    fn decompress_packet_if_needed_passes_through_uncompressed_packet() {
        let packet = synthetic_packet(4096);
        let out = decompress_packet_if_needed(&packet).unwrap();
        assert!(matches!(out, Cow::Borrowed(_)), "expected zero-copy passthrough");
        assert_eq!(out.as_ref(), packet.as_slice());
    }

    #[test]
    fn decompress_packet_if_needed_unwraps_compressed_packet() {
        let original = synthetic_packet(1 << 20);
        let lvl = CompressionLevel::from_u8(3).unwrap();
        let compressed = match compress_packet(&original, lvl).unwrap() {
            CompressOutcome::Compressed(v) => v,
            CompressOutcome::NoOp => panic!("expected compression"),
        };
        let out = decompress_packet_if_needed(&compressed).unwrap();
        assert!(matches!(out, Cow::Owned(_)), "expected owned output for compressed input");
        assert_eq!(out.as_ref(), original.as_slice());
    }

    #[test]
    fn level_zero_payload_is_clean_copy() {
        // compress_payload_zstd at level 0 must return the input bytes
        // verbatim -- no zstd frame -- so callers can rely on it as a
        // uniform pass-through.
        let raw = vec![0xAB; 1024];
        let lvl = CompressionLevel::from_u8(0).unwrap();
        let out = compress_payload_zstd(&raw, lvl).unwrap();
        assert_eq!(out, raw);
    }

    #[test]
    fn empty_payload_roundtrip() {
        // Empty payload must compress (to a small empty frame) and
        // decompress back to empty without error.
        let original = synthetic_packet(0);
        let lvl = CompressionLevel::from_u8(3).unwrap();
        let compressed = match compress_packet(&original, lvl).unwrap() {
            CompressOutcome::Compressed(v) => v,
            CompressOutcome::NoOp => panic!("expected compression at level 3"),
        };
        let recovered = decompress_packet(&compressed).unwrap();
        assert_eq!(recovered, original);
    }

    #[test]
    fn stream_encode_reader_roundtrip() {
        // Reader-driven streaming encode must roundtrip through the
        // decoder. Use 4 MiB so multithreading actually fires.
        let original: Vec<u8> = (0..(4 << 20)).map(|i| (i & 0x0F) as u8).collect();
        let lvl = CompressionLevel::from_u8(3).unwrap();
        let mut src: &[u8] = &original;
        let mut compressed: Vec<u8> = Vec::new();
        stream_encode_reader(&mut compressed, &mut src, lvl, original.len()).unwrap();
        let decoded = decompress_payload_zstd(&compressed).unwrap();
        assert_eq!(decoded, original);
    }

    #[test]
    fn roundtrip_spans_threading_threshold() {
        // 64 KiB sits below choose_workers' 1 MiB threshold (single-
        // threaded encode). 1 MiB exactly hits it. 16 MiB is well into
        // multithreaded territory (~4 workers on a typical box). All
        // three must roundtrip identically across levels 1, 3, 9.
        for &size in &[64 * 1024usize, 1 << 20, 16 << 20] {
            let original = synthetic_packet(size);
            for n in [1u8, 3, 9] {
                let lvl = CompressionLevel::from_u8(n).unwrap();
                let compressed = match compress_packet(&original, lvl).unwrap() {
                    CompressOutcome::Compressed(v) => v,
                    CompressOutcome::NoOp => {
                        panic!("size={size} level={n}: expected Compressed, got NoOp");
                    }
                };
                let recovered = decompress_packet(&compressed).unwrap();
                assert_eq!(
                    recovered, original,
                    "size={size} level={n}: roundtrip mismatch"
                );
            }
        }
    }
}
