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

use morloc_runtime_types::stdio_proto::{
    OP_NEXT_STDIO, OP_WRITE_STDIO,
    STATUS_OK, STATUS_ERR, STATUS_EOF,
    STDIO_KIND_STDOUT, STDIO_KIND_STDERR,
};

/// State for one stdio kind. Serialises access to the underlying fd,
/// tracks whether the stream-packet header has been consumed (stdin) /
/// emitted (stdout / stderr).
struct StdioSlot {
    fd: i32,
    header_done: bool,
}

static STDIN_SLOT:  Mutex<StdioSlot> =
    Mutex::new(StdioSlot { fd: 0, header_done: false });
static STDOUT_SLOT: Mutex<StdioSlot> =
    Mutex::new(StdioSlot { fd: 1, header_done: false });
static STDERR_SLOT: Mutex<StdioSlot> =
    Mutex::new(StdioSlot { fd: 2, header_done: false });

static NEXUS_PID: std::sync::atomic::AtomicI32 =
    std::sync::atomic::AtomicI32::new(0);

/// Start the stdio server. Binds a Unix socket in `$TMPDIR`, exports
/// its path via `MORLOC_NEXUS_STDIO_SOCK`, spawns an accept-loop
/// thread. Idempotent: subsequent calls no-op. SIGPIPE is set to
/// `SIG_IGN` here so a downstream consumer closing the pipe surfaces
/// as `EPIPE` on `write(2)` instead of a nexus signal death.
pub fn start(tmpdir: &str) {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| {
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
    let mut slot = match STDIN_SLOT.lock() {
        Ok(s) => s,
        Err(e) => return Resp::Err(format!("stdin mutex poisoned: {}", e)),
    };
    // Consume the stream-packet header once. On EOF here return EOF
    // directly; a well-formed stdin always has at least the header.
    if !slot.header_done {
        let expected_schema = match unsafe {
            crate::stdio_bridge::stdio_slot_schema(slot_id)
        } {
            Ok(s) => s,
            Err(e) => return Resp::Err(format!("NEXT_STDIO schema lookup: {}", e)),
        };
        match read_and_validate_stream_header(slot.fd, &expected_schema) {
            Ok(true)  => { slot.header_done = true; }
            Ok(false) => { return Resp::Eof; }
            Err(e)    => { return Resp::Err(e); }
        }
    }
    match read_one_subpacket_into_shm(slot.fd) {
        Ok(Some((relptr, size))) => Resp::Ok(relptr, size),
        Ok(None) => Resp::Eof,
        Err(e) => Resp::Err(e),
    }
}

/// Handle a `WRITE_STDIO` request. First call emits the stream-packet
/// header derived from the slot's stored element schema. Subsequent
/// calls append the sub-packet bytes verbatim.
fn do_write(slot_id: i64, relptr: i64, size: u64) -> Resp {
    // Which stdio kind? Read from the SHM registry via the runtime.
    let stdio_kind = match unsafe { crate::stdio_bridge::stdio_kind_of(slot_id) } {
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
    if !slot.header_done {
        match unsafe { crate::stdio_bridge::write_stream_header_for_slot(slot.fd, slot_id) } {
            Ok(()) => { slot.header_done = true; }
            Err(e) => return Resp::Err(format!("WRITE_STDIO header: {}", e)),
        }
    }
    match unsafe { crate::stdio_bridge::write_shm_bytes_to_fd(slot.fd, relptr, size) } {
        Ok(()) => Resp::Ack,
        Err(e) => Resp::Err(format!("WRITE_STDIO body: {}", e)),
    }
}

/// Read the leading `PACKET_TYPE_STREAM` header off fd 0, parse the
/// metadata block, and enforce that its SCHEMA_STRING matches the
/// opener's declared value schema (`expected_schema`, from the slot).
/// Returns `Ok(true)` on match, `Ok(false)` if EOF was hit before any
/// header bytes arrived, or an error on parse failure or schema
/// mismatch.
///
/// The mismatch case is the whole point: `@stdin :: IStream Int` in
/// a program consuming an `IStream Str` stream file must fail loudly
/// at the boundary, not silently pass wrong-shaped bytes to the
/// pool's decoder.
fn read_and_validate_stream_header(fd: i32, expected_schema: &str) -> Result<bool, String> {
    use morloc_runtime_types::packet::{
        iter_metadata, decode_schema_entry,
        METADATA_TYPE_SCHEMA_STRING, PacketHeader,
    };
    let mut hdr = [0u8; 32];
    if !read_exact_fd(fd, &mut hdr)? {
        return Ok(false);
    }
    let header = PacketHeader::from_bytes(&hdr)
        .map_err(|e| format!("stream header parse: {}", e))?;
    if !header.is_stream() {
        return Err(format!(
            "expected a STREAM packet on stdin, got cmd_type {}",
            unsafe { header.command.cmd_type.cmd_type },
        ));
    }
    let meta_len = header.offset as usize;
    let mut meta = vec![0u8; meta_len];
    if meta_len > 0 && !read_exact_fd(fd, &mut meta)? {
        return Err("stdin: EOF inside stream metadata block".into());
    }
    let mut incoming: Option<String> = None;
    for (kind, data) in iter_metadata(&meta) {
        if kind == METADATA_TYPE_SCHEMA_STRING {
            incoming = Some(decode_schema_entry(data));
            break;
        }
    }
    let incoming = incoming.ok_or_else(|| {
        "stdin STREAM_PACKET header carries no SCHEMA_STRING metadata".to_string()
    })?;
    if incoming != expected_schema {
        return Err(format!(
            "stdin stream-schema mismatch: opener declared `{}`, \
             incoming stream carries `{}`",
            expected_schema, incoming,
        ));
    }
    Ok(true)
}

/// Read one full `MORLOC_DATA_PACKET` sub-packet off `fd` into a fresh
/// SHM block. Returns the block's SHM `RelPtr` + total size, or `None`
/// on EOF / footer.
fn read_one_subpacket_into_shm(fd: i32) -> Result<Option<(i64, u64)>, String> {
    let mut hdr = [0u8; 32];
    if !read_exact_fd(fd, &mut hdr)? {
        return Ok(None);
    }
    let header = morloc_runtime_types::packet::PacketHeader::from_bytes(&hdr)
        .map_err(|e| format!("subpacket header parse: {}", e))?;
    if !header.is_data() {
        // Footer or anything else -> treat as end-of-stream. A more
        // careful implementation would parse the footer's status.
        return Ok(None);
    }
    // SAFETY: is_data() implies the data variant of the command.
    let data = unsafe { header.command.data };
    if data.source != morloc_runtime_types::packet::PACKET_SOURCE_MESG {
        return Err(format!(
            "stdin sub-packet has source byte 0x{:02x}; expected MESG",
            data.source,
        ));
    }
    if data.format != morloc_runtime_types::packet::PACKET_FORMAT_VOIDSTAR {
        return Err(format!(
            "stdin sub-packet has format byte 0x{:02x}; expected voidstar",
            data.format,
        ));
    }
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

/// `libc::read` wrapper that retries on `EINTR` and returns `Ok(false)`
/// on immediate EOF.
fn read_exact_fd(fd: i32, buf: &mut [u8]) -> Result<bool, String> {
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
        if n == 0 {
            return Ok(off > 0 && off == buf.len());
        }
        off += n as usize;
    }
    Ok(true)
}
