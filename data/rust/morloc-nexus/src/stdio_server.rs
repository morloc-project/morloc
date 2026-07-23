//! Nexus-side stdio RPC server.
//!
//! The nexus is the sole owner of fd 0/1/2. Pools reach stdin/stdout/
//! stderr through synchronous RPCs over a dedicated Unix socket exported
//! via `MORLOC_NEXUS_STDIO_SOCK`.
//!
//! Wire protocol (all little-endian):
//!
//!   Request:  [opcode: u8]
//!             [slot_id: i64]                       (both directions)
//!             [payload_relptr: i64][size: u64]     (WRITE_STDIO only)
//!
//!   Response: [status: u8]
//!             where status = 0 (ok), 1 (err), 2 (eof)
//!     if ok and NEXT_STDIO: [relptr: i64][size: u64]
//!     if err:                [err_len: u32][err_bytes...]
//!
//! Bytes never travel over the socket in bulk: sub-packets ride in the
//! shared SHM arena and only a `RelPtr` moves.
//!
//! Concurrency: three separate mutexes for stdin / stdout / stderr so a
//! slow downstream consumer of stdout doesn't stall reads on stdin. The
//! mutex is held only across the underlying `read(2)` / `write(2)`
//! syscall (plus the fixed-cost header parse for stdin).

use std::io::{Read, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::sync::Mutex;

use morloc_runtime_types::packet::PacketHeader;
use morloc_runtime_types::stdio_proto::{
    OP_NEXT_STDIO, OP_WRITE_STDIO,
    STATUS_OK, STATUS_ERR, STATUS_EOF,
    STDIO_KIND_STDOUT, STDIO_KIND_STDERR,
};

use crate::dispatch::OutputFormat;

/// State for one stdio kind. Serialises access to the underlying fd,
/// tracks whether the stream-packet header has been consumed (stdin) /
/// emitted (stdout / stderr), and (stdout `-f json` only) whether the
/// opening `[` and any array element have been written.
struct StdioSlot {
    fd: i32,
    header_done: bool,
    json_open: bool,
    json_any: bool,
    // stdin lone-DATA support: when stdin carries a single morloc DATA
    // packet (not a stream), its already-read [header][metadata] bytes are
    // buffered here (a pipe can't be rewound) and emitted as one batch;
    // `single_data` marks that the stream ends after that one packet.
    pending_prefix: Option<Vec<u8>>,
    single_data: bool,
}

static STDIN_SLOT:  Mutex<StdioSlot> =
    Mutex::new(StdioSlot { fd: 0, header_done: false, json_open: false, json_any: false, pending_prefix: None, single_data: false });
static STDOUT_SLOT: Mutex<StdioSlot> =
    Mutex::new(StdioSlot { fd: 1, header_done: false, json_open: false, json_any: false, pending_prefix: None, single_data: false });
static STDERR_SLOT: Mutex<StdioSlot> =
    Mutex::new(StdioSlot { fd: 2, header_done: false, json_open: false, json_any: false, pending_prefix: None, single_data: false });

static NEXUS_PID: std::sync::atomic::AtomicI32 =
    std::sync::atomic::AtomicI32::new(0);

/// Render configuration for streamed stdout, captured from the nexus
/// `NexusConfig` at server start. Streamed `@stdout` output is
/// re-encoded per `format` (and recompressed at `level` for the packet
/// formats) so it honours `-f`/`-z` exactly like a returned value.
/// stderr is unaffected (it stays raw stream-packet framing).
#[derive(Clone, Copy)]
struct RenderCfg {
    format: OutputFormat,
    level: u8,
}

// Mutable (not OnceLock) because a `render` terminal flag is resolved AFTER
// `start` runs, and must retarget streamed stdout to the raw format before the
// pool begins streaming (which happens later still, during dispatch).
static RENDER_CFG: Mutex<RenderCfg> =
    Mutex::new(RenderCfg { format: OutputFormat::Json, level: 0 });

fn render_cfg() -> RenderCfg {
    RENDER_CFG
        .lock()
        .map(|g| *g)
        .unwrap_or(RenderCfg { format: OutputFormat::Json, level: 0 })
}

/// Retarget the streamed-stdout output format. Used by the dispatcher to
/// force `raw` when a `render` terminal flag is chosen (see phase2/main).
pub fn set_output_format(format: OutputFormat) {
    if let Ok(mut g) = RENDER_CFG.lock() {
        g.format = format;
    }
}

fn format_name(f: OutputFormat) -> &'static str {
    match f {
        OutputFormat::Json => "json",
        OutputFormat::Jsonl => "jsonl",
        OutputFormat::MessagePack => "mpk",
        OutputFormat::VoidStar => "voidstar",
        OutputFormat::Packet => "packet",
        OutputFormat::Arrow => "arrow",
        OutputFormat::Parquet => "parquet",
        OutputFormat::Csv => "csv",
        OutputFormat::Raw => "raw",
    }
}

/// Start the stdio server. Binds a Unix socket in `$TMPDIR`, exports
/// its path via `MORLOC_NEXUS_STDIO_SOCK`, spawns an accept-loop
/// thread. Idempotent: subsequent calls no-op. SIGPIPE is set to
/// `SIG_IGN` here so a downstream consumer closing the pipe surfaces
/// as `EPIPE` on `write(2)` instead of a nexus signal death.
pub fn start(tmpdir: &str, output_format: OutputFormat, compression_level: u8) {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        if let Ok(mut g) = RENDER_CFG.lock() {
            *g = RenderCfg { format: output_format, level: compression_level };
        }
        NEXUS_PID.store(std::process::id() as i32, std::sync::atomic::Ordering::Release);

        // SIGPIPE ignore. One-liner but easy to miss; without it a
        // `write(1)` to a closed downstream pipe kills the nexus.
        unsafe { libc::signal(libc::SIGPIPE, libc::SIG_IGN); }

        let pid = std::process::id();
        let path = format!("{}/morloc-nexus-stdio-{}.sock", tmpdir, pid);
        // Best-effort cleanup of a stale socket from a prior run at
        // the same PID (unlikely but cheap).
        let _ = std::fs::remove_file(&path);
        let listener = match UnixListener::bind(&path) {
            Ok(l) => l,
            Err(e) => {
                eprintln!("stdio_server: failed to bind {}: {}", path, e);
                return;
            }
        };
        // Export to child pools via env.
        std::env::set_var("MORLOC_NEXUS_STDIO_SOCK", &path);
        let socket_path = path.clone();
        std::thread::Builder::new()
            .name("morloc-stdio-server".into())
            .spawn(move || accept_loop(listener, socket_path))
            .expect("failed to spawn stdio server thread");
    });
}

/// Accept loop. Each connection gets its own worker thread; the loop
/// itself does no I/O per-request. Errors on accept are logged and
/// looped over (a transient EMFILE shouldn't kill the server).
fn accept_loop(listener: UnixListener, _socket_path: String) {
    for incoming in listener.incoming() {
        match incoming {
            Ok(stream) => {
                std::thread::Builder::new()
                    .name("morloc-stdio-worker".into())
                    .spawn(move || {
                        // Per-connection: drain requests until the client
                        // closes or errors. A single pool may send many
                        // `NEXT_STDIO` / `WRITE_STDIO` requests over the
                        // same connection to amortise the connect cost.
                        let _ = handle_connection(stream);
                    })
                    .ok();
            }
            Err(e) => {
                eprintln!("stdio_server: accept error: {}", e);
            }
        }
    }
}

fn handle_connection(mut stream: UnixStream) -> std::io::Result<()> {
    loop {
        assert_nexus_pid();
        let mut op_buf = [0u8; 1];
        if stream.read_exact(&mut op_buf).is_err() {
            return Ok(()); // client closed
        }
        match op_buf[0] {
            OP_NEXT_STDIO => {
                let mut req = [0u8; 8];
                stream.read_exact(&mut req)?;
                let slot_id = i64::from_le_bytes(req);
                let resp = do_next(slot_id);
                write_response(&mut stream, resp)?;
            }
            OP_WRITE_STDIO => {
                let mut req = [0u8; 24];
                stream.read_exact(&mut req)?;
                let slot_id = i64::from_le_bytes(req[0..8].try_into().unwrap());
                let relptr = i64::from_le_bytes(req[8..16].try_into().unwrap());
                let size = u64::from_le_bytes(req[16..24].try_into().unwrap());
                let resp = do_write(slot_id, relptr, size);
                write_response(&mut stream, resp)?;
            }
            other => {
                write_response(
                    &mut stream,
                    Resp::Err(format!("unknown stdio opcode {}", other)),
                )?;
                return Ok(());
            }
        }
    }
}

enum Resp {
    Ok(i64, u64), // relptr, size (NEXT_STDIO success)
    Ack,           // WRITE_STDIO success
    Eof,
    Err(String),
}

fn write_response(stream: &mut UnixStream, resp: Resp) -> std::io::Result<()> {
    match resp {
        Resp::Ok(relptr, size) => {
            stream.write_all(&[STATUS_OK])?;
            stream.write_all(&relptr.to_le_bytes())?;
            stream.write_all(&size.to_le_bytes())?;
        }
        Resp::Ack => {
            stream.write_all(&[STATUS_OK])?;
        }
        Resp::Eof => {
            stream.write_all(&[STATUS_EOF])?;
        }
        Resp::Err(msg) => {
            stream.write_all(&[STATUS_ERR])?;
            let bytes = msg.as_bytes();
            let len = bytes.len().min(u32::MAX as usize) as u32;
            stream.write_all(&len.to_le_bytes())?;
            stream.write_all(&bytes[..len as usize])?;
        }
    }
    Ok(())
}

fn assert_nexus_pid() {
    let expected = NEXUS_PID.load(std::sync::atomic::Ordering::Acquire);
    let actual = std::process::id() as i32;
    debug_assert_eq!(
        expected, actual,
        "stdio_server handler running in a forked child (expected pid {}, got {})",
        expected, actual,
    );
}

/// Handle a `NEXT_STDIO` request. First call consumes the stream-packet
/// header from stdin (which validates the schema and marks the position
/// of the first sub-packet). Every subsequent call consumes one
/// sub-packet.
///
/// Returns an SHM `RelPtr` pointing at a fresh block that holds the
/// sub-packet bytes (header + metadata + payload as a self-contained
/// `MORLOC_DATA_PACKET`). Caller owns the block and is responsible for
/// `shfree`.
fn do_next(slot_id: i64) -> Resp {
    use morloc_runtime_types::packet::PacketHeader;
    let mut slot = match STDIN_SLOT.lock() {
        Ok(s) => s,
        Err(e) => return Resp::Err(format!("stdin mutex poisoned: {}", e)),
    };
    // First call: sniff the leading 32-byte header and classify the input
    // as a morloc STREAM packet, a lone morloc DATA packet, or foreign
    // (rejected). The declared schema (from the opener's `@open :: IStream
    // T` ascription) is compared against the incoming stream/packet schema
    // so a wrong-typed pipe fails at the boundary instead of mis-decoding.
    if !slot.header_done {
        let expected_schema = match unsafe {
            crate::stdio_bridge::stdio_slot_schema(slot_id)
        } {
            Ok(s) => s,
            Err(e) => return Resp::Err(format!("NEXT_STDIO schema lookup: {}", e)),
        };
        let mut hdr = [0u8; 32];
        match read_fill_fd(slot.fd, &mut hdr) {
            Ok(0)  => return Resp::Eof, // empty stdin: no data is legitimate
            Ok(32) => {}                // full header -> classify below
            Ok(n)  => return Resp::Err(format!(
                "stdin ended after {} byte(s); not a morloc packet (a packet \
                 header is 32 bytes). Foreign or truncated input is not \
                 supported on stdin.",
                n,
            )),
            Err(e) => return Resp::Err(e),
        }
        let header = match PacketHeader::from_bytes(&hdr) {
            Ok(h) => h,
            Err(_) => return Resp::Err(
                "stdin is not a morloc packet; expected a morloc data or \
                 stream packet. Foreign formats (JSON, MessagePack, CSV, ...) \
                 are not yet supported on stdin -- pipe morloc packet output \
                 (the default streamed format) instead.".into(),
            ),
        };
        if header.is_stream() {
            let meta_len = header.offset as usize;
            let mut meta = vec![0u8; meta_len];
            if meta_len > 0 && !read_exact_fd(slot.fd, &mut meta).unwrap_or(false) {
                return Resp::Err("stdin: EOF inside stream metadata block".into());
            }
            if let Err(e) = check_incoming_schema(&meta, &expected_schema, "stdin stream") {
                return Resp::Err(e);
            }
            slot.header_done = true;
            // fall through: read_one_subpacket reads the first sub-packet.
        } else if header.is_data() {
            // Lone DATA packet: the sniffed bytes ARE the (only) sub-packet's
            // header. Read its metadata, validate the schema (the opener's
            // schema is the list `[a]`, so a bare-`a` packet fails the
            // equality check -- this doubles as the list-shape guard), then
            // buffer [header][metadata] and present the whole packet as one
            // batch. A pipe can't be rewound, hence the buffering.
            let meta_len = header.offset as usize;
            let mut prefix = vec![0u8; 32 + meta_len];
            prefix[..32].copy_from_slice(&hdr);
            if meta_len > 0 && !read_exact_fd(slot.fd, &mut prefix[32..]).unwrap_or(false) {
                return Resp::Err("stdin: EOF inside data-packet metadata".into());
            }
            if let Err(e) = check_incoming_schema(&prefix[32..], &expected_schema, "stdin data packet") {
                return Resp::Err(e);
            }
            slot.pending_prefix = Some(prefix);
            slot.single_data = true;
            slot.header_done = true;
        } else {
            return Resp::Err(
                "stdin morloc packet is neither a data packet nor a stream \
                 packet (a call/ping/footer packet cannot be read as an \
                 input stream)".into(),
            );
        }
    }
    match read_one_subpacket_into_shm(&mut *slot) {
        Ok(Some((relptr, size))) => Resp::Ok(relptr, size),
        Ok(None) => Resp::Eof,
        Err(e) => Resp::Err(e),
    }
}

/// Extract the `SCHEMA_STRING` from a packet's metadata block and compare
/// it against the opener's declared schema. Empty declared schema (an
/// untyped open) adopts the incoming schema without a check; otherwise a
/// mismatch is a hard, boundary-level error.
fn check_incoming_schema(meta: &[u8], expected_schema: &str, what: &str) -> Result<(), String> {
    use morloc_runtime_types::packet::{
        iter_metadata, decode_schema_entry, METADATA_TYPE_SCHEMA_STRING,
    };
    let mut incoming: Option<String> = None;
    for (kind, data) in iter_metadata(meta) {
        if kind == METADATA_TYPE_SCHEMA_STRING {
            incoming = Some(decode_schema_entry(data));
            break;
        }
    }
    let incoming = incoming.ok_or_else(|| {
        format!("{}: header carries no SCHEMA_STRING metadata", what)
    })?;
    // An empty declared schema adopts the incoming one without a check. Current
    // codegen always threads the `[a]` schema (@open :: IStream and @stdin
    // both), so this branch is a defensive fallback, not a reachable path.
    if !expected_schema.is_empty() && incoming != expected_schema {
        return Err(format!(
            "{}-schema mismatch: opener declared `{}`, incoming carries `{}`",
            what, expected_schema, incoming,
        ));
    }
    Ok(())
}

/// Handle a `WRITE_STDIO` request.
///
/// **stderr** stays the raw diagnostic channel: the first call emits the
/// stream-packet header, subsequent calls append sub-packet/footer bytes
/// verbatim.
///
/// **stdout** is transcoded per the nexus `-f`/`-z`. Each incoming blob
/// is a self-contained voidstar `MORLOC_DATA_PACKET` sub-packet (or the
/// stream footer). Depending on the output format it is emitted
/// immediately (jsonl / json array / packet / voidstar -- constant
/// memory) or, for the table/msgpack formats which cannot be produced
/// chunk-by-chunk, rejected with an actionable error.
fn do_write(slot_id: i64, relptr: i64, size: u64) -> Resp {
    use crate::stdio_bridge as br;

    // Which stdio kind? Read from the SHM registry via the runtime.
    let stdio_kind = match unsafe { br::stdio_kind_of(slot_id) } {
        Ok(Some(k)) => k,
        Ok(None) => return Resp::Err(format!(
            "WRITE_STDIO: slot {:#x} is not stdio-bound", slot_id,
        )),
        Err(e) => return Resp::Err(format!("WRITE_STDIO: {}", e)),
    };
    // `WRITE_STDIO` on a stdin-bound slot is a caller bug (writer on
    // a reader).
    let slot_mtx = match stdio_kind {
        STDIO_KIND_STDOUT => &STDOUT_SLOT,
        STDIO_KIND_STDERR => &STDERR_SLOT,
        _ => return Resp::Err(format!(
            "WRITE_STDIO: unsupported stdio_kind {} for write path", stdio_kind,
        )),
    };
    let mut slot = match slot_mtx.lock() {
        Ok(s) => s,
        Err(e) => return Resp::Err(format!("stdio write mutex poisoned: {}", e)),
    };

    // stderr: unchanged verbatim pass-through (raw stream-packet framing).
    if stdio_kind == STDIO_KIND_STDERR {
        if !slot.header_done {
            match unsafe { br::write_stream_header_for_slot(slot.fd, slot_id) } {
                Ok(()) => { slot.header_done = true; }
                Err(e) => return Resp::Err(format!("WRITE_STDIO header: {}", e)),
            }
        }
        return match unsafe { br::write_shm_bytes_to_fd(slot.fd, relptr, size) } {
            Ok(()) => Resp::Ack,
            Err(e) => Resp::Err(format!("WRITE_STDIO body: {}", e)),
        };
    }

    // stdout: transcode per -f. Classify from the 32-byte header only;
    // the transcoding branches read the full sub-packet lazily so the
    // packet pass-through path stays zero-copy for large streams.
    let cfg = render_cfg();
    let hdr_vec = match unsafe { br::read_shm_bytes(relptr, 32) } {
        Ok(h) => h,
        Err(e) => return Resp::Err(format!("WRITE_STDIO read: {}", e)),
    };
    if hdr_vec.len() < 32 {
        return Resp::Err(format!(
            "WRITE_STDIO: blob is {} bytes, smaller than a packet header", hdr_vec.len(),
        ));
    }
    let mut hdr = [0u8; 32];
    hdr.copy_from_slice(&hdr_vec[..32]);
    let header = match PacketHeader::from_bytes(&hdr) {
        Ok(h) => h,
        Err(e) => return Resp::Err(format!("WRITE_STDIO header parse: {}", e)),
    };
    let is_footer = header.is_footer();
    let is_data = header.is_data();

    let result: Result<(), String> = (|| -> Result<(), String> {
        match cfg.format {
            OutputFormat::Packet | OutputFormat::VoidStar => {
                if !slot.header_done {
                    unsafe { br::write_stream_header_for_slot(slot.fd, slot_id) }?;
                    slot.header_done = true;
                }
                // At level 0 (the packet default) pass every frame through
                // byte-for-byte: the stream -- and its footer's sub-packet
                // index, which records the pool's exact offsets -- stays
                // identical to what the pool emitted. Only recompress data
                // frames when `-z > 0`; the footer always passes through.
                if is_data && cfg.level > 0 {
                    let bytes = unsafe { br::read_shm_bytes(relptr, size) }?;
                    unsafe { br::emit_subpacket_as_packet(slot.fd, &bytes, cfg.level) }
                } else {
                    unsafe { br::write_shm_bytes_to_fd(slot.fd, relptr, size) }
                }
            }
            OutputFormat::Jsonl => {
                if is_data {
                    let bytes = unsafe { br::read_shm_bytes(relptr, size) }?;
                    let schema = br::subpacket_value_schema_str(&bytes)?;
                    unsafe { br::emit_subpacket_jsonl(&bytes, &schema) }
                } else {
                    Ok(()) // footer: jsonl has no closing frame
                }
            }
            OutputFormat::Raw => {
                if is_data {
                    let bytes = unsafe { br::read_shm_bytes(relptr, size) }?;
                    let schema = br::subpacket_value_schema_str(&bytes)?;
                    unsafe { br::emit_subpacket_raw(&bytes, &schema) }
                } else {
                    Ok(()) // footer: raw has no closing frame
                }
            }
            OutputFormat::Json => {
                if is_data {
                    let bytes = unsafe { br::read_shm_bytes(relptr, size) }?;
                    let schema = br::subpacket_value_schema_str(&bytes)?;
                    let inner = unsafe { br::subpacket_json_inner(&bytes, &schema) }?;
                    if !slot.json_open {
                        unsafe { br::write_bytes_to_fd(slot.fd, b"[") }?;
                        slot.json_open = true;
                    }
                    if !inner.is_empty() {
                        if slot.json_any {
                            unsafe { br::write_bytes_to_fd(slot.fd, b",") }?;
                        }
                        unsafe { br::write_bytes_to_fd(slot.fd, inner.as_bytes()) }?;
                        slot.json_any = true;
                    }
                    Ok(())
                } else if is_footer {
                    if !slot.json_open {
                        unsafe { br::write_bytes_to_fd(slot.fd, b"[") }?;
                        slot.json_open = true;
                    }
                    unsafe { br::write_bytes_to_fd(slot.fd, b"]\n") }
                } else {
                    Ok(())
                }
            }
            OutputFormat::MessagePack
            | OutputFormat::Arrow
            | OutputFormat::Parquet
            | OutputFormat::Csv => Err(format!(
                "streamed stdout output does not support -f {}; \
                 use -f json, -f jsonl, or -f packet",
                format_name(cfg.format),
            )),
        }
    })();

    match result {
        Ok(()) => Resp::Ack,
        Err(e) => Resp::Err(format!("WRITE_STDIO body: {}", e)),
    }
}

/// Read one full `MORLOC_DATA_PACKET` sub-packet into a fresh SHM block.
/// Returns the block's SHM `RelPtr` + total size, or `None` on EOF /
/// footer.
///
/// A lone DATA packet (`slot.pending_prefix` set) is completed by reading
/// its payload after the buffered [header][metadata]; that is the single
/// batch of a one-packet stream, so the next call returns `None`. Stream
/// sub-packets are read fresh off `slot.fd`.
fn read_one_subpacket_into_shm(slot: &mut StdioSlot) -> Result<Option<(i64, u64)>, String> {
    use morloc_runtime_types::packet::PacketHeader;

    // Lone DATA packet: [header][metadata] already buffered; read payload.
    if let Some(prefix) = slot.pending_prefix.take() {
        let header = PacketHeader::from_bytes(prefix[..32].try_into().unwrap())
            .map_err(|e| format!("stdin data-packet header parse: {}", e))?;
        reject_unreadable_subpacket(&header)?;
        let payload_len = header.length as usize;
        let base = prefix.len();
        let mut buf = prefix;
        buf.resize(base + payload_len, 0);
        if payload_len > 0 && !read_exact_fd(slot.fd, &mut buf[base..])? {
            return Err("stdin: EOF inside data-packet payload".into());
        }
        return unsafe { crate::stdio_bridge::copy_into_shm(&buf) }.map(Some);
    }
    // A lone data packet's single batch was already emitted -> end of stream.
    if slot.single_data {
        return Ok(None);
    }

    // Stream sub-packet: read a fresh header + body.
    let fd = slot.fd;
    let mut hdr = [0u8; 32];
    if !read_exact_fd(fd, &mut hdr)? {
        return Ok(None);
    }
    let header = PacketHeader::from_bytes(&hdr)
        .map_err(|e| format!("subpacket header parse: {}", e))?;
    if !header.is_data() {
        // Footer or anything else -> treat as end-of-stream.
        return Ok(None);
    }
    reject_unreadable_subpacket(&header)?;
    let meta_len = header.offset as u64;
    let payload_len = header.length;
    let total = 32u64
        .checked_add(meta_len)
        .and_then(|n| n.checked_add(payload_len))
        .ok_or_else(|| "subpacket size overflow".to_string())?;
    let usize_total = total as usize;
    let mut buf = vec![0u8; usize_total];
    buf[..32].copy_from_slice(&hdr);
    if usize_total > 32 && !read_exact_fd(fd, &mut buf[32..])? {
        return Err("stdin: EOF inside sub-packet body".into());
    }
    unsafe { crate::stdio_bridge::copy_into_shm(&buf) }
        .map(Some)
}

/// Reject a stdin sub-packet the pool decoder can't consume: a non-MESG
/// source or a non-voidstar format. Compression is allowed -- a `-z`
/// (zstd) sub-packet is forwarded verbatim and the pool decompresses it
/// on demand at @next.
fn reject_unreadable_subpacket(
    header: &morloc_runtime_types::packet::PacketHeader,
) -> Result<(), String> {
    use morloc_runtime_types::packet::{PACKET_SOURCE_MESG, PACKET_FORMAT_VOIDSTAR};
    // SAFETY: callers gate on is_data(), so the data variant is valid.
    let data = unsafe { header.command.data };
    if data.source != PACKET_SOURCE_MESG {
        return Err(format!(
            "stdin sub-packet has source byte 0x{:02x}; expected MESG",
            data.source,
        ));
    }
    if data.format != PACKET_FORMAT_VOIDSTAR {
        return Err(format!(
            "stdin sub-packet has format byte 0x{:02x}; expected voidstar",
            data.format,
        ));
    }
    // Compression is fine: the nexus forwards the sub-packet's bytes verbatim
    // (compressed payload and all) and the pool decompresses each one on
    // demand when @next materializes it (stdio_decode_packet ->
    // get_morloc_data_packet_value).
    Ok(())
}

/// Read up to `buf.len()` bytes, returning the number actually read (may
/// be less than `buf.len()` if EOF is hit first). Retries on `EINTR`.
/// Used to classify the leading stdin header: 0 = empty (legitimate),
/// a partial fill = truncated/foreign (an error), a full fill = a packet
/// header to classify.
fn read_fill_fd(fd: i32, buf: &mut [u8]) -> Result<usize, String> {
    let mut off = 0usize;
    while off < buf.len() {
        let n = unsafe {
            libc::read(
                fd,
                buf.as_mut_ptr().add(off) as *mut libc::c_void,
                buf.len() - off,
            )
        };
        if n < 0 {
            let e = std::io::Error::last_os_error();
            if e.raw_os_error() == Some(libc::EINTR) { continue; }
            return Err(format!("read(fd {}): {}", fd, e));
        }
        if n == 0 { break; }
        off += n as usize;
    }
    Ok(off)
}

/// Fill `buf` exactly, retrying on `EINTR`. Returns `Ok(true)` when the
/// whole buffer was filled, `Ok(false)` if EOF was hit first (short read).
fn read_exact_fd(fd: i32, buf: &mut [u8]) -> Result<bool, String> {
    Ok(read_fill_fd(fd, buf)? == buf.len())
}
