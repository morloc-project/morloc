//! Unit tests for the file classifier.
//!
//! These cover the cheap (no `--validate`) classification path:
//! magic-byte sniffing, morloc packet header parsing, CALL packet
//! walking with payload-skip seeking, size-consistency checks, and
//! rendering in plain / brief / JSON modes.

use std::io::{Cursor, Read, Seek, SeekFrom};

use super::*;
use morloc_runtime_types::packet::{
    encode_stream_tail, make_final_footer_packet, make_temp_footer_packet, PacketHeader,
    StreamDiag, FOOTER_STATUS_CLOSED, FOOTER_STATUS_FAILED, METADATA_HEADER_MAGIC,
    METADATA_TYPE_SCHEMA_STRING, PACKET_COMPRESSION_NONE, PACKET_COMPRESSION_ZSTD,
    PACKET_ENTRYPOINT_LOCAL, PACKET_ENTRYPOINT_REMOTE_SFS, PACKET_FORMAT_JSON,
    PACKET_FORMAT_MSGPACK, PACKET_FORMAT_VOIDSTAR, PACKET_SOURCE_FILE,
    PACKET_SOURCE_MESG, PACKET_SOURCE_RPTR, PACKET_TYPE_STREAM,
};

// ---------------------------------------------------------------------------
// Builders for synthetic packets
// ---------------------------------------------------------------------------

/// Pack a schema string into a metadata block. Returns the bytes
/// (padded to a 32-byte boundary, matching make_mesg_data_packet).
fn build_metadata_with_schema(schema: &str) -> Vec<u8> {
    let schema_bytes = schema.as_bytes();
    let schema_len = schema_bytes.len() + 1; // +1 for null terminator
    let header_size = 8;
    let raw = header_size + schema_len;
    let padded = ((raw + 31) / 32) * 32;
    let mut meta = vec![0u8; padded];
    meta[0..3].copy_from_slice(&METADATA_HEADER_MAGIC);
    meta[3] = METADATA_TYPE_SCHEMA_STRING;
    let size_le = (schema_len as u32).to_le_bytes();
    meta[4..8].copy_from_slice(&size_le);
    meta[8..8 + schema_bytes.len()].copy_from_slice(schema_bytes);
    meta
}

/// Build a DATA packet with the given source/format/compression,
/// optional schema, and a payload of `payload_size` zero bytes.
fn build_data_packet(
    source: u8,
    format: u8,
    compression: u8,
    schema: Option<&str>,
    payload_size: usize,
) -> Vec<u8> {
    let meta = schema.map(build_metadata_with_schema).unwrap_or_default();
    let mut header = match source {
        PACKET_SOURCE_MESG => PacketHeader::data_mesg(format, payload_size as u64),
        PACKET_SOURCE_RPTR => PacketHeader::data_rptr(format, payload_size as u64),
        _ => {
            // The constructors only cover MESG/RPTR; for FILE we
            // craft the bytes manually.
            let mut h = PacketHeader::data_mesg(format, payload_size as u64);
            h.command.data.source = source;
            h
        }
    };
    unsafe { h_set_offset(&mut header, meta.len() as u32) };
    if compression != PACKET_COMPRESSION_NONE {
        header.command.data.compression = compression;
    }
    let mut packet = header.to_bytes().to_vec();
    packet.extend_from_slice(&meta);
    packet.extend(std::iter::repeat(0u8).take(payload_size));
    packet
}

unsafe fn h_set_offset(h: &mut PacketHeader, offset: u32) {
    let hdr_ptr = h as *mut PacketHeader as *mut u8;
    let offset_ptr = hdr_ptr.add(20) as *mut u32;
    *offset_ptr = offset;
}

fn build_call_packet(midx: u32, entrypoint: u8, args: &[Vec<u8>]) -> Vec<u8> {
    let payload_len: usize = args.iter().map(|p| p.len()).sum();
    let mut header = PacketHeader::local_call(midx, payload_len as u64);
    header.command.call.entrypoint = entrypoint;
    header.command.call.midx = midx;
    let mut packet = header.to_bytes().to_vec();
    for arg in args {
        packet.extend_from_slice(arg);
    }
    packet
}

fn build_ping_packet() -> Vec<u8> {
    PacketHeader::ping().to_bytes().to_vec()
}

// ---------------------------------------------------------------------------
// Counting Read+Seek wrapper for the I/O-budget test
// ---------------------------------------------------------------------------

struct Counting<R> {
    inner: R,
    /// Bytes the reader has actually pulled (via `read`).
    bytes_read: u64,
    /// Ranges (start..end, in bytes) the reader has actually pulled.
    /// Used to assert "no read fell inside the payload region".
    read_ranges: Vec<std::ops::Range<u64>>,
}

impl<R: Read + Seek> Counting<R> {
    fn new(inner: R) -> Self {
        Counting { inner, bytes_read: 0, read_ranges: Vec::new() }
    }
}

impl<R: Read + Seek> Read for Counting<R> {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let pos = self.inner.stream_position()?;
        let n = self.inner.read(buf)?;
        if n > 0 {
            self.bytes_read += n as u64;
            self.read_ranges.push(pos..(pos + n as u64));
        }
        Ok(n)
    }
}

impl<R: Seek> Seek for Counting<R> {
    fn seek(&mut self, pos: SeekFrom) -> std::io::Result<u64> {
        self.inner.seek(pos)
    }
}

// ---------------------------------------------------------------------------
// Tests: cheap classification path
// ---------------------------------------------------------------------------

fn classify(bytes: &[u8]) -> Classification {
    let len = bytes.len() as u64;
    classify_reader(&mut Cursor::new(bytes), len, None, 1024)
}

#[test]
fn data_packet_mesg_msgpack_no_compression() {
    let packet = build_data_packet(
        PACKET_SOURCE_MESG,
        PACKET_FORMAT_MSGPACK,
        PACKET_COMPRESSION_NONE,
        Some("as"),
        128,
    );
    match classify(&packet) {
        Classification::MorlocDataPacket { arg } => {
            assert_eq!(arg.source, PACKET_SOURCE_MESG);
            assert_eq!(arg.format, PACKET_FORMAT_MSGPACK);
            assert_eq!(arg.compression, PACKET_COMPRESSION_NONE);
            assert_eq!(arg.schema.as_deref(), Some("as"));
            assert_eq!(arg.payload_bytes, 128);
            assert!(arg.metadata_bytes >= 8);
        }
        other => panic!("expected DATA packet, got {:?}", other),
    }
}

#[test]
fn data_packet_rptr_voidstar_with_zstd() {
    let packet = build_data_packet(
        PACKET_SOURCE_RPTR,
        PACKET_FORMAT_VOIDSTAR,
        PACKET_COMPRESSION_ZSTD,
        Some("ad8"),
        8,
    );
    match classify(&packet) {
        Classification::MorlocDataPacket { arg, .. } => {
            assert_eq!(arg.source, PACKET_SOURCE_RPTR);
            assert_eq!(arg.format, PACKET_FORMAT_VOIDSTAR);
            assert_eq!(arg.compression, PACKET_COMPRESSION_ZSTD);
            assert_eq!(arg.schema.as_deref(), Some("ad8"));
        }
        other => panic!("expected DATA packet, got {:?}", other),
    }
}

#[test]
fn data_packet_file_source() {
    let packet = build_data_packet(
        PACKET_SOURCE_FILE,
        PACKET_FORMAT_MSGPACK,
        PACKET_COMPRESSION_NONE,
        Some("u4"),
        16,
    );
    match classify(&packet) {
        Classification::MorlocDataPacket { arg, .. } => {
            assert_eq!(arg.source, PACKET_SOURCE_FILE);
        }
        other => panic!("expected DATA packet, got {:?}", other),
    }
}

#[test]
fn truncated_packet_is_error() {
    let mut packet = build_data_packet(
        PACKET_SOURCE_MESG,
        PACKET_FORMAT_MSGPACK,
        PACKET_COMPRESSION_NONE,
        Some("u4"),
        128,
    );
    // Chop the last 32 bytes of payload off — file size now disagrees
    // with the declared header.
    let trimmed = &packet[..packet.len() - 32];
    let cls = classify(trimmed);
    match cls {
        Classification::Error(msg) => {
            assert!(
                msg.contains("truncated"),
                "expected 'truncated' diagnostic, got: {}",
                msg
            );
        }
        other => panic!("expected Error, got {:?}", other),
    }
    // Re-classify the original to make sure we built it consistently.
    packet.truncate(packet.len());
    match classify(&packet) {
        Classification::MorlocDataPacket { .. } => {}
        other => panic!("baseline packet should classify, got {:?}", other),
    }
}

#[test]
fn oversize_packet_is_error() {
    let mut packet = build_data_packet(
        PACKET_SOURCE_MESG,
        PACKET_FORMAT_MSGPACK,
        PACKET_COMPRESSION_NONE,
        Some("u4"),
        8,
    );
    packet.extend(std::iter::repeat(0u8).take(64));
    match classify(&packet) {
        Classification::Error(msg) => {
            assert!(msg.contains("oversize"), "got: {}", msg);
        }
        other => panic!("expected Error, got {:?}", other),
    }
}

#[test]
fn ping_packet() {
    let packet = build_ping_packet();
    assert!(matches!(classify(&packet), Classification::MorlocPingPacket));
}

#[test]
fn call_packet_zero_args() {
    let packet = build_call_packet(7, PACKET_ENTRYPOINT_LOCAL, &[]);
    match classify(&packet) {
        Classification::MorlocCallPacket {
            midx, entrypoint, args, payload_bytes, ..
        } => {
            assert_eq!(midx, 7);
            assert_eq!(entrypoint, PACKET_ENTRYPOINT_LOCAL);
            assert_eq!(payload_bytes, 0);
            assert_eq!(args.len(), 0);
        }
        other => panic!("expected CALL packet, got {:?}", other),
    }
}

#[test]
fn call_packet_one_arg() {
    let arg = build_data_packet(
        PACKET_SOURCE_MESG, PACKET_FORMAT_MSGPACK, PACKET_COMPRESSION_NONE,
        Some("i4"), 4,
    );
    let packet = build_call_packet(42, PACKET_ENTRYPOINT_LOCAL, &[arg.clone()]);
    match classify(&packet) {
        Classification::MorlocCallPacket {
            midx, entrypoint, args, payload_bytes, ..
        } => {
            assert_eq!(midx, 42);
            assert_eq!(entrypoint, PACKET_ENTRYPOINT_LOCAL);
            assert_eq!(payload_bytes, arg.len() as u64);
            assert_eq!(args.len(), 1);
            match &args[0] {
                ArgEntry::Data(d) => {
                    assert_eq!(d.format, PACKET_FORMAT_MSGPACK);
                    assert_eq!(d.schema.as_deref(), Some("i4"));
                    assert_eq!(d.payload_bytes, 4);
                }
                other => panic!("expected Data arg, got {:?}", other),
            }
        }
        other => panic!("expected CALL packet, got {:?}", other),
    }
}

#[test]
fn call_packet_three_args_remote() {
    let a = build_data_packet(
        PACKET_SOURCE_MESG, PACKET_FORMAT_MSGPACK, PACKET_COMPRESSION_NONE,
        Some("i4"), 4,
    );
    let b = build_data_packet(
        PACKET_SOURCE_MESG, PACKET_FORMAT_MSGPACK, PACKET_COMPRESSION_NONE,
        Some("as"), 16,
    );
    let c = build_data_packet(
        PACKET_SOURCE_RPTR, PACKET_FORMAT_VOIDSTAR, PACKET_COMPRESSION_NONE,
        Some("ad8"), 8,
    );
    let packet = build_call_packet(
        99, PACKET_ENTRYPOINT_REMOTE_SFS,
        &[a.clone(), b.clone(), c.clone()],
    );
    match classify(&packet) {
        Classification::MorlocCallPacket { args, entrypoint, .. } => {
            assert_eq!(entrypoint, PACKET_ENTRYPOINT_REMOTE_SFS);
            assert_eq!(args.len(), 3);
            // Spot-check the third arg to make sure the walker
            // advanced correctly past the first two.
            match &args[2] {
                ArgEntry::Data(d) => {
                    assert_eq!(d.source, PACKET_SOURCE_RPTR);
                    assert_eq!(d.schema.as_deref(), Some("ad8"));
                }
                other => panic!("expected Data, got {:?}", other),
            }
        }
        other => panic!("expected CALL packet, got {:?}", other),
    }
}

#[test]
fn call_packet_malformed_arg_overrun() {
    // Build an arg whose declared length overruns the parent CALL's
    // payload region. Build a normal arg, then corrupt its `length`
    // field to point past the end.
    let mut arg = build_data_packet(
        PACKET_SOURCE_MESG, PACKET_FORMAT_MSGPACK, PACKET_COMPRESSION_NONE,
        Some("i4"), 4,
    );
    // length lives at bytes 24..32 of the arg's header.
    let huge: u64 = 1024 * 1024;
    arg[24..32].copy_from_slice(&huge.to_le_bytes());
    let mut packet = build_call_packet(1, PACKET_ENTRYPOINT_LOCAL, &[arg]);
    // The outer length is now stale (it was computed from the original
    // arg size); fix the outer file-size mismatch by truncating the
    // tail so the outer-size check passes and the walker is the one
    // that catches the overrun. The outer length field stays as it
    // was; the actual payload bytes we have on disk match it.
    let outer_len_bytes = packet[24..32].to_vec();
    let outer_len = u64::from_le_bytes([
        outer_len_bytes[0], outer_len_bytes[1], outer_len_bytes[2], outer_len_bytes[3],
        outer_len_bytes[4], outer_len_bytes[5], outer_len_bytes[6], outer_len_bytes[7],
    ]);
    // The file should be exactly 32 + outer_len bytes.
    packet.truncate(32 + outer_len as usize);
    match classify(&packet) {
        Classification::MorlocCallPacket { args, .. } => {
            assert!(args.iter().any(|a| matches!(a, ArgEntry::Malformed(_))));
        }
        other => panic!("expected CALL packet, got {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Tests: I/O budget — walking CALL packets must not read payload bytes
// ---------------------------------------------------------------------------

#[test]
fn call_walker_does_not_read_arg_payloads() {
    // Build three args with tiny metadata but artificially large
    // payloads (1 MB each). The walker should skip each payload via
    // `seek` and never `read` inside it.
    let make = |schema: &str, payload_size: usize| {
        build_data_packet(
            PACKET_SOURCE_MESG, PACKET_FORMAT_MSGPACK, PACKET_COMPRESSION_NONE,
            Some(schema), payload_size,
        )
    };
    let args = vec![make("i4", 1 << 20), make("as", 1 << 20), make("ad8", 1 << 20)];
    let packet = build_call_packet(11, PACKET_ENTRYPOINT_LOCAL, &args);

    // Record the byte ranges of each arg's payload so we can assert
    // the reader stayed out of them. Each arg's payload starts at
    // (arg_start + 32 + metadata) and runs for (payload_size) bytes.
    let mut payload_ranges: Vec<std::ops::Range<u64>> = Vec::new();
    let mut pos: u64 = 32; // skip outer header
    for arg in &args {
        // Re-read offset/length from the arg's own header.
        let arg_offset = u32::from_le_bytes([
            arg[20], arg[21], arg[22], arg[23],
        ]) as u64;
        let arg_length = u64::from_le_bytes([
            arg[24], arg[25], arg[26], arg[27],
            arg[28], arg[29], arg[30], arg[31],
        ]);
        let payload_start = pos + 32 + arg_offset;
        let payload_end = payload_start + arg_length;
        payload_ranges.push(payload_start..payload_end);
        pos = payload_end;
    }

    let total = packet.len() as u64;
    let mut counter = Counting::new(Cursor::new(packet.clone()));
    let cls = classify_reader(&mut counter, total, None, 1024);
    assert!(matches!(cls, Classification::MorlocCallPacket { .. }));

    // No recorded read range may overlap with any payload region.
    for read_range in &counter.read_ranges {
        for payload_range in &payload_ranges {
            // Check overlap.
            let overlaps =
                read_range.start < payload_range.end && payload_range.start < read_range.end;
            assert!(
                !overlaps,
                "read range {:?} overlaps payload region {:?}",
                read_range, payload_range
            );
        }
    }

    // Sanity: at least one byte was actually read (else the test
    // would be vacuous because the reader saw nothing).
    assert!(counter.bytes_read > 0);
    // And the total bytes read is far less than the file size (the
    // file is >3 MB; we should be reading only headers + metadata,
    // well under 4 KB).
    assert!(
        counter.bytes_read < 4096,
        "expected <4 KB of reads, got {}",
        counter.bytes_read
    );
}

// ---------------------------------------------------------------------------
// Tests: non-packet sniffing
// ---------------------------------------------------------------------------

#[test]
fn json_object_lead() {
    assert!(matches!(classify(b"{\"a\":1}"), Classification::Json));
}

#[test]
fn json_array_lead() {
    assert!(matches!(classify(b"[1,2,3]"), Classification::Json));
}

#[test]
fn json_string_lead() {
    assert!(matches!(classify(b"\"hello\""), Classification::Json));
}

#[test]
fn json_with_leading_whitespace() {
    assert!(matches!(classify(b"  \n  {\"a\":1}"), Classification::Json));
}

#[test]
fn json_negative_number_lead() {
    assert!(matches!(classify(b"-3.14"), Classification::Json));
}

#[test]
fn json_null_lead() {
    assert!(matches!(classify(b"null"), Classification::Json));
}

#[test]
fn arrow_ipc_magic() {
    let mut bytes = b"ARROW1".to_vec();
    bytes.extend_from_slice(&[0u8; 32]);
    assert!(matches!(classify(&bytes), Classification::Arrow));
}

#[test]
fn parquet_magic_both_ends() {
    // Need PAR1 at head AND tail, and at least 8 bytes total.
    let mut bytes = b"PAR1".to_vec();
    bytes.extend_from_slice(&[0u8; 16]);
    bytes.extend_from_slice(b"PAR1");
    assert!(matches!(classify(&bytes), Classification::Parquet));
}

#[test]
fn parquet_only_head_is_not_parquet() {
    // PAR1 + NUL padding + XXXX. The PAR1 head is there but the tail
    // doesn't match. NUL bytes also kill the text path. → Unknown.
    let mut bytes = b"PAR1".to_vec();
    bytes.extend_from_slice(&[0u8; 16]);
    bytes.extend_from_slice(b"XXXX");
    assert!(matches!(classify(&bytes), Classification::Unknown));
}

#[test]
fn csv_by_content_comma() {
    // Real CSV with a header and two data rows. The arrow-csv inferrer
    // accepts this with delimiter `,` and infers 3 columns. The age
    // column is also typed as int by the same inference pass.
    let bytes = b"name,age,city\nAlice,30,NYC\nBob,25,LA\n";
    match classify(bytes) {
        Classification::Csv { delimiter, columns } => {
            assert_eq!(delimiter, ',');
            let names: Vec<_> = columns.iter().map(|c| c.name.as_str()).collect();
            assert_eq!(names, ["name", "age", "city"]);
            let age_type = &columns
                .iter()
                .find(|c| c.name == "age")
                .unwrap()
                .type_category;
            assert_eq!(age_type, "int");
        }
        other => panic!("expected CSV, got {:?}", other),
    }
}

#[test]
fn csv_by_content_tab() {
    // TSV: tabs separate columns. Tab delimiter is tried first; comma
    // path would also fall through but tab wins by virtue of order.
    let bytes = b"name\tage\tcity\nAlice\t30\tNYC\nBob\t25\tLA\n";
    match classify(bytes) {
        Classification::Csv { delimiter, columns } => {
            assert_eq!(delimiter, '\t');
            assert_eq!(columns.len(), 3);
        }
        other => panic!("expected TSV, got {:?}", other),
    }
}

#[test]
fn single_column_text_is_not_csv() {
    // A README-shaped file has no commas or tabs in the header line.
    // The 2-column floor in `looks_like_csv` rejects it; it falls
    // through to the text classifier.
    let bytes = b"README\n\nThis is a description of the project.\nMore notes here.\n";
    match classify(bytes) {
        Classification::Text { ascii_only } => assert!(ascii_only),
        other => panic!("expected ASCII text, got {:?}", other),
    }
}

#[test]
fn msgpack_single_byte_fixint() {
    // A 1-byte file containing 0x00 parses as a complete msgpack root
    // value (positive fixint 0) with no trailing data. Degenerate case
    // -- 1-byte ASCII files are inherently ambiguous with msgpack
    // fixints. → MessagePack.
    assert!(matches!(classify(&[0x00]), Classification::MessagePack));
}

#[test]
fn msgpack_array_full_consume() {
    // 0x91 = fixarray of 1; 0xa1 = fixstr of 1; 0x41 = 'A'. Walker
    // consumes all 3 bytes → MessagePack.
    assert!(matches!(
        classify(&[0x91, 0xa1, 0x41]),
        Classification::MessagePack
    ));
}

#[test]
fn msgpack_truncated_array_mid_value() {
    // 0x93 = fixarray of 3; 0xa1 0x61 = "a"; then EOF before the
    // remaining 2 elements. The walker reports Eof, which we accept
    // as plausible MessagePack (probe boundary cut a real value).
    assert!(matches!(
        classify(&[0x93, 0xa1, 0x61]),
        Classification::MessagePack
    ));
}

#[test]
fn msgpack_complete_followed_by_garbage_rejected() {
    // 0x41 = 'A' = positive fixint 65 (a complete 1-byte msgpack
    // value) AND a valid ASCII byte. The probe has more bytes after
    // it -- msgpack's trailing-data rule rejects, and the whole thing
    // then classifies cleanly as ASCII text.
    let bytes = b"Ahello world";
    match classify(bytes) {
        Classification::Text { ascii_only: true } => {}
        other => panic!("expected ASCII Text, got {:?}", other),
    }
}

#[test]
fn reserved_marker_rejected() {
    // 0xc1 is the reserved marker. msgpack rejects, text rejects (0xc1
    // is invalid as UTF-8 leading byte), → Unknown.
    assert!(matches!(
        classify(&[0xc1, 0xc1, 0xc1]),
        Classification::Unknown
    ));
}

#[test]
fn empty_file_is_empty() {
    assert!(matches!(classify(b""), Classification::Empty));
}

#[test]
fn text_readme_is_ascii_text() {
    // The bug we set out to fix: ASCII text files were being
    // misclassified as MessagePack because every leading byte is a
    // valid fixint. With the full-consume rule, readme-like text
    // falls through to the text classifier.
    let bytes = b"README\n\nThis is a project. It does things. Use it at your own risk.\n";
    match classify(bytes) {
        Classification::Text { ascii_only } => assert!(ascii_only),
        other => panic!("expected ASCII text, got {:?}", other),
    }
}

#[test]
fn text_fasta_is_ascii_text() {
    let bytes = b">chr1\nACGTACGTACGTACGT\n>chr2\nTTTTAAAACCCCGGGG\n";
    match classify(bytes) {
        Classification::Text { ascii_only } => assert!(ascii_only),
        other => panic!("expected ASCII text, got {:?}", other),
    }
}

#[test]
fn text_fastq_is_ascii_text() {
    let bytes = b"@SRR123.1\nACGTACGTACGT\n+\nIIIIIIIIIIII\n@SRR123.2\nGGGGAAAACCCC\n+\nIIIIIIIIIIII\n";
    match classify(bytes) {
        Classification::Text { ascii_only } => assert!(ascii_only),
        other => panic!("expected ASCII text, got {:?}", other),
    }
}

#[test]
fn utf8_text_marks_non_ascii() {
    // UTF-8 text with high-bit codepoints (e-acute, o-umlaut) classifies
    // as Text { ascii_only: false }.
    let bytes = "Caf\u{00e9}: na\u{00ef}vet\u{00e9} of l'h\u{00f4}tel.\n".as_bytes();
    match classify(bytes) {
        Classification::Text { ascii_only } => assert!(!ascii_only),
        other => panic!("expected UTF-8 text, got {:?}", other),
    }
}

#[test]
fn binary_with_embedded_nul_is_unknown() {
    // A file with a NUL byte is not text. 0xff = FixNeg(-1), a
    // one-byte complete msgpack value; the trailing bytes make
    // msgpack reject too. 0xff is also invalid UTF-8, and the input
    // has no comma/tab to fool the CSV inferrer. → Unknown.
    let bytes = &[0xff, 0xff, 0x00, 0xff, 0xff, 0xff, 0xff][..];
    assert!(matches!(classify(bytes), Classification::Unknown));
}

// ---------------------------------------------------------------------------
// Tests: rendering
// ---------------------------------------------------------------------------

const DEFAULT_OPTS: RenderOpts = RenderOpts {
    no_file: false,
    no_description: false,
    verbose: false,
};

#[test]
fn plain_format_data_packet() {
    let cls = Classification::MorlocDataPacket {
        arg: DataArg {
            source: PACKET_SOURCE_MESG,
            format: PACKET_FORMAT_MSGPACK,
            compression: PACKET_COMPRESSION_NONE,
            schema: Some("as".to_string()),
            payload_bytes: 1024,
            metadata_bytes: 64,
        },
    };
    let s = format_plain(&cls, "foo.packet", &DEFAULT_OPTS, None);
    assert!(s.starts_with("foo.packet: data-packet "));
    assert!(s.contains("source=mesg"));
    assert!(s.contains("format=msgpack"));
    assert!(!s.contains("compression=")); // none suppressed
    assert!(s.contains("schema=\"as\""));
    assert!(s.contains("payload_full_size=1024"));
    assert!(s.contains("metadata=64"));
    assert!(!s.contains("file_bytes")); // deliberately omitted
    assert!(!s.contains("file_size"));
}

#[test]
fn no_file_drops_prefix_no_description_drops_fields() {
    let cls = Classification::Json;
    assert_eq!(
        format_plain(&cls, "foo.json", &DEFAULT_OPTS, None),
        "foo.json: json"
    );
    let no_file = RenderOpts { no_file: true, ..DEFAULT_OPTS };
    assert_eq!(format_plain(&cls, "foo.json", &no_file, None), "json");

    // -D on a kind that *has* fields proves the suppression.
    let csv = Classification::Csv {
        delimiter: ',',
        columns: vec![
            CsvColumn { name: "a".into(), type_category: "int".into() },
            CsvColumn { name: "b".into(), type_category: "str".into() },
        ],
    };
    let no_desc = RenderOpts { no_description: true, ..DEFAULT_OPTS };
    assert_eq!(format_plain(&csv, "x.csv", &no_desc, None), "x.csv: csv");

    // -FD collapses to just the type token.
    let fd = RenderOpts {
        no_file: true,
        no_description: true,
        verbose: false,
    };
    assert_eq!(format_plain(&csv, "x.csv", &fd, None), "csv");
}

#[test]
fn default_call_packet_is_one_line() {
    // Default rendering enforces the one-line-per-file guarantee:
    // call packets do NOT spill arg details onto extra lines.
    let cls = Classification::MorlocCallPacket {
        midx: 42,
        entrypoint: PACKET_ENTRYPOINT_LOCAL,
        args: vec![
            ArgEntry::Data(DataArg {
                source: PACKET_SOURCE_MESG,
                format: PACKET_FORMAT_MSGPACK,
                compression: PACKET_COMPRESSION_NONE,
                schema: Some("i4".to_string()),
                payload_bytes: 4,
                metadata_bytes: 32,
            }),
            ArgEntry::Data(DataArg {
                source: PACKET_SOURCE_RPTR,
                format: PACKET_FORMAT_VOIDSTAR,
                compression: PACKET_COMPRESSION_NONE,
                schema: Some("ad8".to_string()),
                payload_bytes: 8,
                metadata_bytes: 32,
            }),
        ],
        payload_bytes: 144,
    };
    let s = format_plain(&cls, "foo.cp", &DEFAULT_OPTS, None);
    assert_eq!(s.lines().count(), 1);
    assert!(s.starts_with("foo.cp: call-packet "));
    assert!(s.contains("midx=42"));
    assert!(s.contains("entrypoint=local"));
    assert!(s.contains("nargs=2"));
}

#[test]
fn verbose_call_packet_emits_arg_lines() {
    let cls = Classification::MorlocCallPacket {
        midx: 42,
        entrypoint: PACKET_ENTRYPOINT_LOCAL,
        args: vec![
            ArgEntry::Data(DataArg {
                source: PACKET_SOURCE_MESG,
                format: PACKET_FORMAT_MSGPACK,
                compression: PACKET_COMPRESSION_NONE,
                schema: Some("i4".to_string()),
                payload_bytes: 4,
                metadata_bytes: 32,
            }),
            ArgEntry::Data(DataArg {
                source: PACKET_SOURCE_RPTR,
                format: PACKET_FORMAT_VOIDSTAR,
                compression: PACKET_COMPRESSION_NONE,
                schema: Some("ad8".to_string()),
                payload_bytes: 8,
                metadata_bytes: 32,
            }),
        ],
        payload_bytes: 144,
    };
    let verbose = RenderOpts { verbose: true, ..DEFAULT_OPTS };
    let s = format_plain(&cls, "foo.cp", &verbose, None);
    let lines: Vec<&str> = s.lines().collect();
    assert_eq!(lines.len(), 3);
    assert!(lines[0].starts_with("foo.cp: call-packet "));
    assert!(lines[1].starts_with("  arg[0]: data-packet "));
    assert!(lines[2].starts_with("  arg[1]: data-packet "));
    assert!(lines[2].contains("source=rptr"));
}

#[test]
fn json_format_data_packet() {
    let cls = Classification::MorlocDataPacket {
        arg: DataArg {
            source: PACKET_SOURCE_MESG,
            format: PACKET_FORMAT_MSGPACK,
            compression: PACKET_COMPRESSION_NONE,
            schema: Some("as".to_string()),
            payload_bytes: 1024,
            metadata_bytes: 64,
        },
    };
    let s = format_json(&cls, "foo.packet", None);
    let v: serde_json::Value = serde_json::from_str(&s).unwrap();
    assert_eq!(v["path"], "foo.packet");
    assert_eq!(v["kind"], "morloc-packet");
    assert_eq!(v["subkind"], "data");
    assert_eq!(v["source"], "mesg");
    assert_eq!(v["format"], "msgpack");
    assert_eq!(v["compression"], "none");
    assert_eq!(v["schema"], "as");
    assert_eq!(v["payload_full_size"], 1024);
    assert_eq!(v["metadata_bytes"], 64);
    assert!(v.get("file_bytes").is_none());
    assert!(v.get("file_size").is_none());
}

#[test]
fn json_format_call_packet_with_nested_args() {
    let cls = Classification::MorlocCallPacket {
        midx: 7,
        entrypoint: PACKET_ENTRYPOINT_LOCAL,
        args: vec![ArgEntry::Data(DataArg {
            source: PACKET_SOURCE_MESG,
            format: PACKET_FORMAT_JSON,
            compression: PACKET_COMPRESSION_NONE,
            schema: None,
            payload_bytes: 16,
            metadata_bytes: 0,
        })],
        payload_bytes: 48,
    };
    let s = format_json(&cls, "x", None);
    let v: serde_json::Value = serde_json::from_str(&s).unwrap();
    assert_eq!(v["subkind"], "call");
    assert_eq!(v["midx"], 7);
    assert_eq!(v["entrypoint"], "local");
    assert_eq!(v["nargs"], 1);
    let args = v["args"].as_array().unwrap();
    assert_eq!(args.len(), 1);
    assert_eq!(args[0]["subkind"], "data");
    assert_eq!(args[0]["format"], "json");
    // Schema absent on inner arg; should not appear in the JSON.
    assert!(args[0].get("schema").is_none());
}

#[test]
fn json_format_error() {
    let cls = Classification::Error("cannot open (No such file or directory)".to_string());
    let s = format_json(&cls, "missing", None);
    let v: serde_json::Value = serde_json::from_str(&s).unwrap();
    assert_eq!(v["kind"], "error");
    assert!(v["error"].as_str().unwrap().contains("No such file"));
}

#[test]
fn validated_field_appends_to_plain() {
    let cls = Classification::MorlocPingPacket;
    let s = format_plain(&cls, "p", &DEFAULT_OPTS, Some(&ValidationOutcome::Passed));
    assert!(s.contains("validated=yes"));
    let s = format_plain(
        &cls,
        "p",
        &DEFAULT_OPTS,
        Some(&ValidationOutcome::Failed("schema mismatch".into())),
    );
    assert!(s.contains("validated=no"));
    assert!(s.contains("error=\"schema mismatch\""));
    let s = format_plain(&cls, "p", &DEFAULT_OPTS, Some(&ValidationOutcome::StructureOnly));
    assert!(s.contains("validated=structure-only"));
}

#[test]
fn plain_format_csv_default() {
    let cls = Classification::Csv {
        delimiter: ',',
        columns: vec![
            CsvColumn { name: "a".into(), type_category: "int".into() },
            CsvColumn { name: "b".into(), type_category: "str".into() },
            CsvColumn { name: "c".into(), type_category: "float".into() },
            CsvColumn { name: "d".into(), type_category: "bool".into() },
        ],
    };
    let s = format_plain(&cls, "data.csv", &DEFAULT_OPTS, None);
    assert_eq!(s, "data.csv: csv columns=4 delimiter=\",\"");
}

#[test]
fn plain_format_csv_verbose_emits_column_lines() {
    let cls = Classification::Csv {
        delimiter: '\t',
        columns: vec![
            CsvColumn { name: "a".into(), type_category: "int".into() },
            CsvColumn { name: "b".into(), type_category: "bool".into() },
            CsvColumn { name: "c".into(), type_category: "str".into() },
            CsvColumn { name: "d".into(), type_category: "float".into() },
        ],
    };
    let verbose = RenderOpts { verbose: true, ..DEFAULT_OPTS };
    let s = format_plain(&cls, "x.tsv", &verbose, None);
    let lines: Vec<&str> = s.lines().collect();
    assert_eq!(lines.len(), 5);
    assert_eq!(lines[0], "x.tsv: csv columns=4 delimiter=\"\\t\"");
    assert_eq!(lines[1], "  a:int");
    assert_eq!(lines[2], "  b:bool");
    assert_eq!(lines[3], "  c:str");
    assert_eq!(lines[4], "  d:float");
}

#[test]
fn plain_format_text_distinguishes_ascii_vs_utf8() {
    let cls = Classification::Text { ascii_only: true };
    let s = format_plain(&cls, "README", &DEFAULT_OPTS, None);
    assert_eq!(s, "README: text encoding=ascii");
    let cls = Classification::Text { ascii_only: false };
    let s = format_plain(&cls, "haiku.txt", &DEFAULT_OPTS, None);
    assert_eq!(s, "haiku.txt: text encoding=utf-8");
}

#[test]
fn json_format_csv_carries_columns_and_types() {
    let cls = Classification::Csv {
        delimiter: ',',
        columns: vec![
            CsvColumn { name: "a".into(), type_category: "int".into() },
            CsvColumn { name: "b".into(), type_category: "str".into() },
        ],
    };
    let s = format_json(&cls, "data.csv", None);
    let v: serde_json::Value = serde_json::from_str(&s).unwrap();
    assert_eq!(v["kind"], "csv");
    assert_eq!(v["delimiter"], ",");
    assert_eq!(v["column_count"], 2);
    let cols = v["columns"].as_array().unwrap();
    assert_eq!(cols.len(), 2);
    assert_eq!(cols[0]["name"], "a");
    assert_eq!(cols[0]["type"], "int");
    assert_eq!(cols[1]["name"], "b");
    assert_eq!(cols[1]["type"], "str");
}

#[test]
fn json_format_text_carries_encoding() {
    let cls = Classification::Text { ascii_only: false };
    let s = format_json(&cls, "haiku.txt", None);
    let v: serde_json::Value = serde_json::from_str(&s).unwrap();
    assert_eq!(v["kind"], "text");
    assert_eq!(v["encoding"], "utf-8");
}

// ---------------------------------------------------------------------------
// Tests: STREAM packets
// ---------------------------------------------------------------------------

/// Build a stream header block by hand (independent of
/// `make_stream_header_block`, which requires a parsed `Schema`).
/// The schema string is embedded verbatim inside a SCHEMA_STRING
/// metadata entry, padded to a 32-byte alignment boundary.
fn build_stream_header(schema: &str) -> Vec<u8> {
    let meta = build_metadata_with_schema(schema);
    let mut hdr = PacketHeader::stream();
    hdr.offset = meta.len() as u32;
    let mut out = hdr.to_bytes().to_vec();
    out.extend_from_slice(&meta);
    out
}

/// A single DATA sub-packet with a msgpack payload of `payload_size`
/// zero bytes and no metadata block. Sufficient shape for the
/// stream-classification tests; the classifier never inspects a stream
/// body.
fn build_stream_subpacket(payload_size: usize) -> Vec<u8> {
    build_data_packet(
        PACKET_SOURCE_MESG,
        PACKET_FORMAT_MSGPACK,
        PACKET_COMPRESSION_NONE,
        None,
        payload_size,
    )
}

fn diag_with(sub: u64, elem: u64, uncompressed: u64, compressed: u64) -> StreamDiag {
    let mut d = StreamDiag::new();
    d.writer_pid = 12345;
    d.subpacket_count = sub;
    d.element_count = elem;
    d.bytes_uncompressed_total = uncompressed;
    d.bytes_compressed_total = compressed;
    d.first_flush_time = 1000;
    d.last_flush_time = 2000;
    d.largest_packet_uncompressed = uncompressed / sub.max(1);
    d.largest_packet_idx = 0;
    d.tail_len = 0;
    d
}

#[test]
fn stream_packet_no_footer_walks_sub_packets() {
    // A stream file whose EOF tail does NOT carry the STREAM_TAIL_MAGIC:
    // stream header + a couple of subpackets + no footer + no tail.
    // The classifier state stays `missing` (file is unchanged on disk);
    // a diagnostic walk populates `walk` with the sub-packet count.
    // The msgpack sub-packet shape here is not voidstar so element_count
    // stays 0 and the walker records them under compressed_uncounted.
    let mut file = build_stream_header("u4");
    file.extend_from_slice(&build_stream_subpacket(64));
    file.extend_from_slice(&build_stream_subpacket(64));
    let total = file.len() as u64;
    match classify_reader(&mut Cursor::new(&file), total, None, 1024) {
        Classification::MorlocStreamPacket { schema, footer } => {
            assert_eq!(schema.as_deref(), Some("u4"));
            match footer {
                FooterInfo::Missing { walk: Some(w) } => {
                    assert_eq!(w.subpacket_count, 2);
                    assert_eq!(w.element_count, 0);
                    assert!(!w.partial);
                    assert_eq!(w.compressed_uncounted, 2);
                    assert!(!w.scan_capped);
                }
                other => panic!(
                    "expected FooterInfo::Missing with walk data, got {:?}",
                    other
                ),
            }
        }
        other => panic!("expected stream packet, got {:?}", other),
    }
}

#[test]
fn stream_packet_temp_footer_reports_diag() {
    let mut file = build_stream_header("i8");
    file.extend_from_slice(&build_stream_subpacket(128));
    let diag = diag_with(1, 16, 128, 128);
    file.extend_from_slice(&make_temp_footer_packet(&diag));
    let total = file.len() as u64;
    match classify_reader(&mut Cursor::new(&file), total, None, 1024) {
        Classification::MorlocStreamPacket { schema, footer, .. } => {
            assert_eq!(schema.as_deref(), Some("i8"));
            match footer {
                FooterInfo::Temp { diag: v } => {
                    assert_eq!(v.subpacket_count, 1);
                    assert_eq!(v.element_count, 16);
                    assert_eq!(v.bytes_uncompressed_total, 128);
                }
                other => panic!("expected temp footer, got {:?}", other),
            }
        }
        other => panic!("expected stream packet, got {:?}", other),
    }
}

#[test]
fn stream_packet_final_footer_reports_index_count_and_status() {
    let mut file = build_stream_header("u4");
    for _ in 0..3 {
        file.extend_from_slice(&build_stream_subpacket(32));
    }
    let diag = diag_with(3, 3, 96, 96);
    let offsets = [0u64, 100, 200];
    file.extend_from_slice(
        &make_final_footer_packet(&diag, &offsets, FOOTER_STATUS_FAILED),
    );
    let total = file.len() as u64;
    match classify_reader(&mut Cursor::new(&file), total, None, 1024) {
        Classification::MorlocStreamPacket { schema, footer, .. } => {
            assert_eq!(schema.as_deref(), Some("u4"));
            match footer {
                FooterInfo::Final { diag: v, status } => {
                    assert_eq!(v.subpacket_count, offsets.len() as u64);
                    assert_eq!(status, FOOTER_STATUS_FAILED);
                    assert_eq!(v.element_count, 3);
                }
                other => panic!("expected final footer, got {:?}", other),
            }
        }
        other => panic!("expected stream packet, got {:?}", other),
    }
}

#[test]
fn stream_packet_giant_length_field_no_longer_errors() {
    // The regression case: before this change the classifier ran
    // 32 + offset + length against the file size on every packet. A
    // stream header's length is u64::MAX (STREAM_LENGTH_SENTINEL),
    // which the sum-check misread as "truncated by 18 exabytes".
    let mut file = build_stream_header("u4");
    file.extend_from_slice(&build_stream_subpacket(1024));
    let total = file.len() as u64;
    let cls = classify_reader(&mut Cursor::new(&file), total, None, 1024);
    assert!(
        matches!(cls, Classification::MorlocStreamPacket { .. }),
        "stream should classify without triggering the size gate, got {:?}",
        cls
    );
}

#[test]
fn stream_footer_body_length_mismatch_is_error() {
    // Corrupt the EOF tail so its declared footer_len points to bytes
    // that aren't a valid footer packet. The classifier should surface
    // an error rather than silently returning `Missing`.
    let mut file = build_stream_header("u4");
    file.extend_from_slice(&build_stream_subpacket(64));
    let diag = diag_with(1, 8, 64, 64);
    let footer_pkt = make_temp_footer_packet(&diag);
    let footer_body_len = footer_pkt.len() - 8; // strip embedded tail
    file.extend_from_slice(&footer_pkt[..footer_body_len]);
    // Write a bogus tail that says the footer is 12 bytes when it's
    // actually a full temp-footer packet body.
    file.extend_from_slice(&encode_stream_tail(12));
    let total = file.len() as u64;
    let cls = classify_reader(&mut Cursor::new(&file), total, None, 1024);
    assert!(
        matches!(cls, Classification::Error(_)),
        "expected Error on corrupt tail, got {:?}", cls
    );
}

#[test]
fn stream_packet_default_plain_render_missing() {
    let cls = Classification::MorlocStreamPacket {
        schema: Some("u4".into()),
        footer: FooterInfo::Missing { walk: None },
    };
    let s = format_plain(&cls, "s.packet", &DEFAULT_OPTS, None);
    assert_eq!(s.lines().count(), 1);
    assert!(s.starts_with("s.packet: stream-packet "));
    assert!(s.contains("schema=\"u4\""));
    assert!(s.contains("state=missing"));
    assert!(s.contains("footer_note="));
    assert!(!s.contains("file_bytes"));
    assert!(!s.contains("file_size"));
}

#[test]
fn stream_packet_default_plain_render_final() {
    let mut d = StreamDiag::new();
    d.subpacket_count = 5;
    d.element_count = 5;
    d.bytes_uncompressed_total = 500;
    d.bytes_compressed_total = 250;
    let cls = Classification::MorlocStreamPacket {
        schema: Some("u4".into()),
        footer: FooterInfo::Final {
            diag: d.snapshot(),
            status: FOOTER_STATUS_CLOSED,
        },
    };
    let s = format_plain(&cls, "s.packet", &DEFAULT_OPTS, None);
    assert_eq!(s.lines().count(), 1);
    assert!(s.contains("state=final"));
    assert!(s.contains("status=closed"));
    assert!(s.contains("subpackets=5"));
    assert!(s.contains("elements=5"));
    assert!(s.contains("payload_full_size=500"));
    assert!(s.contains("payload_wire_size=250"));
    assert!(!s.contains("file_bytes"));
    assert!(!s.contains("file_size"));
}

#[test]
fn stream_packet_verbose_render_dumps_diag() {
    let mut d = StreamDiag::new();
    d.writer_pid = 4242;
    d.subpacket_count = 2;
    d.element_count = 2;
    d.first_flush_time = 111;
    d.last_flush_time = 222;
    d.largest_packet_uncompressed = 100;
    d.tail_len = 2;
    d.tail[0] = 32;
    d.tail[1] = 200;
    let cls = Classification::MorlocStreamPacket {
        schema: Some("u4".into()),
        footer: FooterInfo::Temp { diag: d.snapshot() },
    };
    let verbose = RenderOpts { verbose: true, ..DEFAULT_OPTS };
    let s = format_plain(&cls, "s.packet", &verbose, None);
    let lines: Vec<&str> = s.lines().collect();
    assert!(lines.len() >= 2);
    assert!(lines[0].starts_with("s.packet: stream-packet "));
    let body = lines[1..].join("\n");
    assert!(body.contains("writer_pid=4242"));
    assert!(body.contains("first_flush_time=111"));
    assert!(body.contains("last_flush_time=222"));
    assert!(body.contains("largest_packet_uncompressed=100"));
    assert!(body.contains("tail_window=[32,200]"));
}

#[test]
fn stream_packet_json_render_final() {
    let mut d = StreamDiag::new();
    d.subpacket_count = 3;
    d.element_count = 30;
    d.bytes_uncompressed_total = 900;
    let cls = Classification::MorlocStreamPacket {
        schema: Some("u4".into()),
        footer: FooterInfo::Final {
            diag: d.snapshot(),
            status: FOOTER_STATUS_CLOSED,
        },
    };
    let s = format_json(&cls, "s.packet", None);
    let v: serde_json::Value = serde_json::from_str(&s).unwrap();
    assert_eq!(v["kind"], "morloc-packet");
    assert_eq!(v["subkind"], "stream");
    assert_eq!(v["schema"], "u4");
    assert!(v.get("file_bytes").is_none());
    assert!(v.get("file_size").is_none());
    assert_eq!(v["footer"]["state"], "final");
    assert_eq!(v["footer"]["status"], "closed");
    assert_eq!(v["footer"]["subpacket_count"], 3);
    assert_eq!(v["footer"]["diag"]["element_count"], 30);
    assert_eq!(v["footer"]["diag"]["bytes_uncompressed_total"], 900);
}

#[test]
fn stream_packet_json_render_missing() {
    let cls = Classification::MorlocStreamPacket {
        schema: None,
        footer: FooterInfo::Missing { walk: None },
    };
    let s = format_json(&cls, "s.packet", None);
    let v: serde_json::Value = serde_json::from_str(&s).unwrap();
    assert_eq!(v["subkind"], "stream");
    assert!(v.get("schema").is_none());
    assert_eq!(v["footer"]["state"], "missing");
    assert!(v["footer"].get("diag").is_none());
}

#[test]
fn stream_truncated_past_header_reports_missing_with_schema() {
    // Real-world case: the writer got its 32-byte header out, wrote
    // the leading SCHEMA_STRING metadata entry, then crashed before
    // finishing the padded metadata block, the subpackets, the footer,
    // or the tail. The file is smaller than the header claims but the
    // classifier should still surface the schema and report
    // state=missing rather than erroring on the truncated metadata
    // block.
    //
    // File shape (49 B, matching the user-observed bug):
    //   [0..32]   stream header (offset=32, length=u64::MAX)
    //   [32..42]  a 10-byte SCHEMA_STRING entry for "j"
    //   [42..49]  seven trailing zero bytes
    let hdr = {
        let mut h = PacketHeader::stream();
        h.offset = 32;
        h
    };
    let mut file = hdr.to_bytes().to_vec();
    // SCHEMA_STRING metadata entry: mmh + type=SCHEMA + size=2 + "j\0".
    file.extend_from_slice(&METADATA_HEADER_MAGIC);
    file.push(METADATA_TYPE_SCHEMA_STRING);
    file.extend_from_slice(&2u32.to_le_bytes());
    file.extend_from_slice(b"j\0");
    // Pad to 49 B total (the size in the hex dump the user posted).
    file.resize(49, 0);
    assert_eq!(file.len(), 49);

    let total = file.len() as u64;
    match classify_reader(&mut Cursor::new(&file), total, None, 1024) {
        Classification::MorlocStreamPacket { schema, footer } => {
            assert_eq!(schema.as_deref(), Some("j"));
            assert!(matches!(footer, FooterInfo::Missing { .. }));
        }
        other => panic!("expected stream packet, got {:?}", other),
    }
}

#[test]
fn stream_command_type_constant_present() {
    // Guard: if PACKET_TYPE_STREAM ever moves values, the dispatch arm
    // in `classify_morloc_packet` needs to be revisited.
    assert_eq!(PACKET_TYPE_STREAM, 3);
}
