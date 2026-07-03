//! Zstd compression for morloc packet payloads.
//!
//! The packet header reserves a `compression` byte; this module is the only
//! place that maps that byte to a real codec. The on-disk contract is:
//!
//! * The 32-byte header and the metadata block (header.offset bytes) stay
//!   in the clear.
//! * `PACKET_COMPRESSION_NONE` payloads are one contiguous uncompressed
//!   region of `header.length` bytes.
//! * `PACKET_COMPRESSION_ZSTD` payloads are a concatenation of
//!   independent zstd frames, each capped at `FRAME_CHUNK_SIZE` of
//!   uncompressed input. The packet's metadata block carries a
//!   `METADATA_TYPE_FRAME_INDEX` entry listing per-frame
//!   `(uncompressed_size, compressed_size)`; decoders use it to
//!   `shmalloc` the total uncompressed size up front and dispatch
//!   per-frame decompression in parallel.
//! * On compression, `header.length` is the sum of per-frame compressed
//!   sizes; on decompression it is restored to the sum of per-frame
//!   uncompressed sizes.
//!
//! User-facing level 0-9 maps to zstd presets (see `CompressionLevel`).
//! Compression is parallel across frames (one libzstd encoder per
//! frame; the encoder buffers a frame's worth of uncompressed bytes,
//! sets `pledgedSrcSize` exactly, then emits a self-contained frame).
//! Decompression is per-frame and trivially parallelizable; the
//! current implementation is single-threaded but the on-disk format
//! is ready for a worker pool.
//!
//! Public surface:
//! * `CompressionLevel::from_u8` / `zstd_level` / `use_long` / `is_none`
//! * `FRAME_CHUNK_SIZE`, `MultiFrameEncoder`, `max_frames_for`,
//!   `frame_index_entry_max_bytes`
//! * `compress_payload_zstd` / `decompress_payload_zstd_into` (raw bytes)
//! * `compress_packet`       / `decompress_packet`           (full packets)
//! * `stream_encode_with`    / `stream_encode_reader`        (streaming)
//! * `decompress_packet_if_needed` (no-op for non-packets and
//!   uncompressed packets; used by `mlc_load`)

use std::borrow::Cow;
use std::io::{Read, Write};

use crate::error::MorlocError;
use crate::packet::{
    encode_frame_index_entry, iter_packet_metadata, FrameEntry, PacketHeader,
    FRAME_ENTRY_BYTES, FRAME_INDEX_HEADER_BYTES, METADATA_HEADER_MAGIC,
    METADATA_TYPE_FRAME_INDEX, PACKET_COMPRESSION_NONE, PACKET_COMPRESSION_ZSTD,
    PACKET_MAGIC, PACKET_SOURCE_MESG, PACKET_TYPE_DATA,
};

// ── Multi-frame chunking ──────────────────────────────────────────────────

/// Target uncompressed bytes per zstd frame in multi-frame output.
/// Each frame is independent (its own header, FCS, and content), so
/// frames can be decompressed in parallel. 16 MiB is the sweet spot:
/// large enough to amortize the per-frame header + still keep
/// compression ratio close to the single-frame baseline; small enough
/// that ~150 frames cover a multi-GiB payload, saturating a typical
/// worker count with comfortable per-worker memory.
pub const FRAME_CHUNK_SIZE: usize = 16 * 1024 * 1024;

/// Worst-case bytes the metadata-entry payload occupies for an index
/// covering up to `max_frames` frames. The streaming encoder reserves
/// this many bytes upfront, then patches the actual index in after
/// the payload streams out.
pub const fn frame_index_entry_max_bytes(max_frames: usize) -> usize {
    FRAME_INDEX_HEADER_BYTES + max_frames * FRAME_ENTRY_BYTES
}

/// Compute the worst-case frame count for a payload of
/// `estimated_uncompressed_bytes`. Used by the streaming encoder to
/// reserve metadata space for the frame index before the actual
/// frame count is known.
pub fn max_frames_for(estimated_uncompressed_bytes: usize) -> usize {
    // `ceil(bytes / chunk_size)`, with a floor of 1 -- even an empty
    // payload still produces one (empty) frame so readers can rely on
    // `frame_count > 0` for compressed packets.
    if estimated_uncompressed_bytes == 0 {
        1
    } else {
        estimated_uncompressed_bytes.div_ceil(FRAME_CHUNK_SIZE)
    }
}

/// Construct a `zstd::bulk::Decompressor` configured for our window
/// sizes. Returned ready to be reused across many frames -- the `DCtx`
/// retains its internal state between calls, avoiding the per-frame
/// allocation cost of `zstd::stream::Decoder::new` (~5-10% of decode
/// wall time at 16 MiB chunks).
fn make_bulk_decompressor<'a>() -> std::io::Result<zstd::bulk::Decompressor<'a>> {
    let mut d = zstd::bulk::Decompressor::new()?;
    d.window_log_max(DECODER_WINDOW_LOG_MAX)?;
    Ok(d)
}

/// Construct a `zstd::bulk::Compressor` at the given level, ready to
/// be reused across many frames. Each frame sets its own
/// `pledgedSrcSize` via the underlying CCtx before
/// `compress_to_buffer`; the compressor itself retains its `CCtx`
/// state between calls.
fn make_bulk_compressor<'a>(
    lvl: CompressionLevel,
) -> std::io::Result<zstd::bulk::Compressor<'a>> {
    let mut c = zstd::bulk::Compressor::new(lvl.zstd_level())?;
    if lvl.use_long() {
        c.window_log(LONG_WINDOW_LOG)?;
    }
    Ok(c)
}

/// Compress a single chunk into one complete zstd frame with
/// `pledgedSrcSize = raw.len()`, reusing the caller-supplied
/// compressor's `CCtx`. The output is written into `out`, which is
/// resized to `compress_bound(raw.len())` and then truncated to the
/// actual compressed size -- callers may pass a previously-used `Vec`
/// to amortize the allocation across many calls. Frames are
/// independent (no cross-frame dictionary), so concatenating them
/// yields a valid multi-frame stream that workers can decode in
/// parallel.
fn compress_one_frame_into(
    compressor: &mut zstd::bulk::Compressor<'_>,
    raw: &[u8],
    out: &mut Vec<u8>,
) -> Result<(), MorlocError> {
    compressor
        .context_mut()
        .set_pledged_src_size(Some(raw.len() as u64))
        .map_err(|code| MorlocError::Io(std::io::Error::other(format!(
            "zstd set_pledged_src_size failed: code {}", code
        ))))?;
    let bound = zstd::zstd_safe::compress_bound(raw.len());
    out.clear();
    out.resize(bound, 0);
    let written = compressor
        .compress_to_buffer(raw, &mut out[..])
        .map_err(MorlocError::Io)?;
    out.truncate(written);
    Ok(())
}

/// Single-shot wrapper around `compress_one_frame_into` for callers
/// that don't keep their own compressor around. Less efficient than
/// reusing the compressor across frames; intended for the
/// `finish()`-time single-chunk fast path and the empty-input
/// invariant frame.
fn compress_one_frame(
    raw: &[u8],
    lvl: CompressionLevel,
) -> Result<Vec<u8>, MorlocError> {
    let mut compressor = make_bulk_compressor(lvl).map_err(MorlocError::Io)?;
    let mut out = Vec::new();
    compress_one_frame_into(&mut compressor, raw, &mut out)?;
    Ok(out)
}

/// Streaming sink that splits writes into `chunk_size`-byte chunks
/// and compresses each chunk into an independent zstd frame using a
/// background worker pool. Returns the per-frame `[uncompressed_size,
/// compressed_size]` index from `finish()`, which callers stamp into
/// the packet's `METADATA_TYPE_FRAME_INDEX` entry.
///
/// The pipeline is asynchronous: chunks are dispatched to workers
/// over per-worker channels (round-robin), workers compress and push
/// results back on a single result channel, and the caller's writer
/// thread drains the result channel between writes, emitting frames
/// in original chunk order via a small reorder buffer. The walker
/// never blocks on a worker except for backpressure when every
/// worker's input queue is full (default depth 2, so ~`2 * n_workers`
/// chunks may be in flight before the walker has to wait).
///
/// `n_workers` defaults to `frame_workers()` (currently
/// `available_parallelism()` capped at `MAX_FRAME_WORKERS`).
/// Lazily-allocated worker pool state. Held in an `Option` on the
/// encoder so single-chunk payloads (the common case for small
/// packets) never pay the thread-spawn cost: the pool is built on the
/// first chunk that's dispatched to a worker, and small payloads that
/// finish in one chunk go through an inline `compress_one_frame` path
/// in `finish()`.
struct EncoderPool {
    worker_txs: Vec<std::sync::mpsc::SyncSender<(usize, Vec<u8>)>>,
    result_rx: std::sync::mpsc::Receiver<(usize, std::io::Result<Vec<u8>>)>,
    worker_handles: Vec<std::thread::JoinHandle<()>>,
}

pub struct MultiFrameEncoder<W: Write> {
    // `writer` is held in an Option so `finish()` can move it out
    // via `take()` without conflicting with the Drop impl that does
    // worker-thread cleanup.
    writer: Option<W>,
    pending: Vec<u8>,
    chunk_size: usize,
    lvl: CompressionLevel,
    frames: Vec<FrameEntry>,
    next_seq: usize,
    write_next_seq: usize,
    n_workers: usize,
    pool: Option<EncoderPool>,
    reorder_buf: std::collections::BTreeMap<usize, Vec<u8>>,
    seq_uncompressed_size: std::collections::BTreeMap<usize, usize>,
}

impl<W: Write> MultiFrameEncoder<W> {
    pub fn new(writer: W, lvl: CompressionLevel, chunk_size: usize) -> Self {
        Self::with_n_workers(writer, lvl, chunk_size, frame_workers())
    }

    /// Construct an encoder with an explicit worker count. Tests use
    /// `n_workers = 1` to force fully-serial behavior. Worker threads
    /// are NOT spawned here -- they're spawned lazily on the first
    /// dispatch so single-chunk payloads pay no thread-spawn cost.
    pub fn with_n_workers(
        writer: W,
        lvl: CompressionLevel,
        chunk_size: usize,
        n_workers: usize,
    ) -> Self {
        Self {
            writer: Some(writer),
            pending: Vec::with_capacity(chunk_size.min(1 << 20)),
            chunk_size,
            lvl,
            frames: Vec::new(),
            next_seq: 0,
            write_next_seq: 0,
            n_workers: n_workers.max(1),
            pool: None,
            reorder_buf: std::collections::BTreeMap::new(),
            seq_uncompressed_size: std::collections::BTreeMap::new(),
        }
    }

    /// Shorthand for the inner writer reference. Panics if called
    /// after `finish()` -- the encoder is consumed at that point.
    fn writer_mut(&mut self) -> &mut W {
        self.writer
            .as_mut()
            .expect("MultiFrameEncoder used after finish()")
    }

    /// Spawn the worker pool on demand. Subsequent calls are no-ops.
    fn ensure_pool(&mut self) -> &mut EncoderPool {
        if self.pool.is_some() {
            return self.pool.as_mut().unwrap();
        }
        // Bounded per-worker queue depth. Four slots lets the walker
        // queue several chunks ahead while the worker is busy on the
        // current one, smoothing out tail-latency stalls (page-fault
        // bursts, allocator hiccups). With 12 workers and 16 MiB
        // chunks this caps in-flight memory at ~768 MiB of
        // uncompressed buffers, comfortably below the SHM destination.
        const WORKER_QUEUE_DEPTH: usize = 4;
        let lvl = self.lvl;
        let n = self.n_workers;
        let (result_tx, result_rx) =
            std::sync::mpsc::channel::<(usize, std::io::Result<Vec<u8>>)>();
        let mut worker_txs = Vec::with_capacity(n);
        let mut worker_handles = Vec::with_capacity(n);
        for _ in 0..n {
            let (tx, rx) = std::sync::mpsc::sync_channel::<(usize, Vec<u8>)>(WORKER_QUEUE_DEPTH);
            let result_tx = result_tx.clone();
            let h = std::thread::spawn(move || {
                // One bulk compressor per worker, reused across every
                // chunk this worker pulls. Holding the `CCtx` across
                // calls avoids per-frame zstd context allocation and
                // its window-buffer setup.
                let mut compressor = match make_bulk_compressor(lvl) {
                    Ok(c) => c,
                    Err(e) => {
                        // Best-effort signal: send the error on the
                        // first chunk we receive, then exit.
                        if let Ok((seq, _chunk)) = rx.recv() {
                            let _ = result_tx.send((seq, Err(e)));
                        }
                        return;
                    }
                };
                let mut out_buf: Vec<u8> = Vec::new();
                while let Ok((seq, chunk)) = rx.recv() {
                    let result = compress_one_frame_into(
                        &mut compressor,
                        &chunk,
                        &mut out_buf,
                    )
                    .map(|()| std::mem::take(&mut out_buf))
                    .map_err(|e| std::io::Error::other(e.to_string()));
                    if result_tx.send((seq, result)).is_err() {
                        // Receiver dropped (encoder dropped mid-flight).
                        return;
                    }
                }
            });
            worker_txs.push(tx);
            worker_handles.push(h);
        }
        self.pool = Some(EncoderPool {
            worker_txs,
            result_rx,
            worker_handles,
        });
        self.pool.as_mut().unwrap()
    }

    /// Drain any ready results and write the ones whose sequence
    /// numbers are next in line. Non-blocking: stops when `try_recv`
    /// would block. No-op when the pool hasn't been spawned yet.
    fn drain_ready_results(&mut self) -> std::io::Result<()> {
        loop {
            // Scoped borrow of the pool so we can call `write_in_order`
            // (which needs `&mut self`) after extracting the next ready
            // result.
            let item = match self.pool.as_ref() {
                Some(pool) => pool.result_rx.try_recv(),
                None => return Ok(()),
            };
            match item {
                Ok((seq, result)) => {
                    let bytes = result?;
                    self.reorder_buf.insert(seq, bytes);
                    self.write_in_order()?;
                }
                Err(std::sync::mpsc::TryRecvError::Empty) => return Ok(()),
                Err(std::sync::mpsc::TryRecvError::Disconnected) => return Ok(()),
            }
        }
    }

    /// Block until at least one result arrives (and any later
    /// in-order results queued behind it). Returns Ok(true) on
    /// progress, Ok(false) if the channel is closed (no workers
    /// alive), Err on writer or worker failure.
    fn await_one_result(&mut self) -> std::io::Result<bool> {
        let item = match self.pool.as_ref() {
            Some(pool) => pool.result_rx.recv(),
            None => return Ok(false),
        };
        match item {
            Ok((seq, result)) => {
                let bytes = result?;
                self.reorder_buf.insert(seq, bytes);
                self.write_in_order()?;
                self.drain_ready_results()?;
                Ok(true)
            }
            Err(_) => Ok(false),
        }
    }

    /// Emit any frames in the reorder buffer whose sequence number is
    /// next in line, advancing `write_next_seq`. Frees the per-seq
    /// uncompressed-size and compressed-bytes entries as they're
    /// written.
    fn write_in_order(&mut self) -> std::io::Result<()> {
        while let Some(bytes) = self.reorder_buf.remove(&self.write_next_seq) {
            let uncompressed_size = self
                .seq_uncompressed_size
                .remove(&self.write_next_seq)
                .expect("seq_uncompressed_size missing entry");
            let compressed_size = bytes.len();
            self.writer_mut().write_all(&bytes)?;
            // bytes drops here, freeing the per-chunk compressed Vec
            self.frames.push(FrameEntry {
                uncompressed_size: uncompressed_size as u64,
                compressed_size: compressed_size as u64,
            });
            self.write_next_seq += 1;
        }
        Ok(())
    }

    /// Take the current `pending` chunk and hand it to a worker.
    /// Round-robins on the worker index; spawns the worker pool on
    /// first call. Blocks only if every worker's queue is full --
    /// backpressure that keeps the in-flight budget bounded at
    /// `n_workers * WORKER_QUEUE_DEPTH` chunks.
    fn dispatch_pending(&mut self) -> std::io::Result<()> {
        if self.pending.is_empty() {
            return Ok(());
        }
        let chunk = std::mem::replace(
            &mut self.pending,
            Vec::with_capacity(self.chunk_size.min(1 << 20)),
        );
        let seq = self.next_seq;
        self.next_seq += 1;
        self.seq_uncompressed_size.insert(seq, chunk.len());
        // Spawn pool on first dispatch (no-op on subsequent calls).
        self.ensure_pool();
        // Drain ready results first so the writer doesn't fall too
        // far behind and so the reorder buffer doesn't grow unbounded.
        self.drain_ready_results()?;
        // Round-robin dispatch to per-worker queues. `pool` is Some
        // because we just called ensure_pool above.
        let pool = self.pool.as_ref().expect("pool just spawned");
        let worker_idx = seq % pool.worker_txs.len();
        if let Err(e) = pool.worker_txs[worker_idx].send((seq, chunk)) {
            return Err(std::io::Error::other(format!(
                "worker channel closed: {e}"
            )));
        }
        Ok(())
    }

    /// Flush any pending bytes as a final frame and return the inner
    /// writer plus the per-frame index. When the entire input fits in
    /// one chunk (the common case for small packets) no worker pool
    /// is ever spawned -- the pending chunk is compressed inline.
    /// Otherwise the worker pool is drained in order and joined.
    pub fn finish(mut self) -> Result<(W, Vec<FrameEntry>), MorlocError> {
        // Common-case fast path: no chunks dispatched yet means the
        // whole input fits in `pending`. Compress it inline without
        // spawning workers. Handles both single-chunk payloads and
        // truly empty input (one zero-byte frame for the invariant).
        if self.next_seq == 0 {
            let pending = std::mem::take(&mut self.pending);
            let compressed = compress_one_frame(&pending, self.lvl)?;
            let compressed_size = compressed.len();
            self.writer_mut()
                .write_all(&compressed)
                .map_err(MorlocError::Io)?;
            self.frames.push(FrameEntry {
                uncompressed_size: pending.len() as u64,
                compressed_size: compressed_size as u64,
            });
            let writer = self.writer.take().expect("writer present");
            let frames = std::mem::take(&mut self.frames);
            return Ok((writer, frames));
        }

        // Multi-chunk path: dispatch the final partial chunk, close
        // worker channels (so workers drain and exit), then loop
        // collecting results in order until every dispatched chunk
        // has been written.
        if !self.pending.is_empty() {
            self.dispatch_pending().map_err(MorlocError::Io)?;
        }
        if let Some(pool) = self.pool.as_mut() {
            pool.worker_txs.clear();
        }
        while self.write_next_seq < self.next_seq {
            if !self.await_one_result().map_err(MorlocError::Io)? {
                return Err(MorlocError::Other(
                    "frame compression workers all exited before draining input".into(),
                ));
            }
        }
        if let Some(mut pool) = self.pool.take() {
            for h in pool.worker_handles.drain(..) {
                let _ = h.join();
            }
            // pool drops here, releasing the channels.
        }
        let writer = self.writer.take().expect("writer present");
        let frames = std::mem::take(&mut self.frames);
        Ok((writer, frames))
    }
}

impl<W: Write> Drop for MultiFrameEncoder<W> {
    /// Clean teardown when the encoder is dropped without `finish()`
    /// (e.g. early-return on a write error, panic in the walker).
    /// Closing the worker input channels causes workers to exit after
    /// they drain whatever they've already pulled; we then join their
    /// handles so no thread state is left dangling.
    fn drop(&mut self) {
        if let Some(mut pool) = self.pool.take() {
            pool.worker_txs.clear();
            for h in pool.worker_handles.drain(..) {
                let _ = h.join();
            }
        }
    }
}

impl<W: Write> Write for MultiFrameEncoder<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut consumed = 0;
        while consumed < buf.len() {
            let room = self.chunk_size - self.pending.len();
            let take = room.min(buf.len() - consumed);
            self.pending
                .extend_from_slice(&buf[consumed..consumed + take]);
            consumed += take;
            if self.pending.len() >= self.chunk_size {
                self.dispatch_pending()?;
            }
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        // Intentionally a no-op: emitting a frame on every flush call
        // (e.g. from an upstream BufWriter draining its buffer mid-
        // payload) would fragment the output into tiny frames. The
        // final partial frame is flushed by `finish()`.
        Ok(())
    }
}

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
) -> Result<(Vec<u8>, Vec<FrameEntry>), MorlocError> {
    if lvl.is_none() {
        // Level 0 passes through unchanged with no frame index.
        // Uncompressed payloads are stored as a single contiguous
        // region; downstream readers detect compression == NONE in
        // the packet header and skip the frame-index lookup.
        return Ok((raw.to_vec(), Vec::new()));
    }
    let mut encoder = MultiFrameEncoder::new(Vec::new(), lvl, FRAME_CHUNK_SIZE);
    encoder.write_all(raw).map_err(MorlocError::Io)?;
    encoder.finish()
}

/// Decompress a single zstd frame from `reader` directly into `dest`.
/// Each frame in a multi-frame stream carries its own exact
/// `pledgedSrcSize`, so the caller can size `dest` to the frame's
/// uncompressed-size entry from the packet's frame-index metadata.
/// Returns the number of bytes actually written. This is the
/// zero-intermediate-buffer path used to land per-frame compressed
/// bytes directly into SHM.
pub fn decompress_payload_zstd_into<R: Read>(
    reader: R,
    dest: &mut [u8],
) -> Result<usize, MorlocError> {
    let mut decoder = zstd::stream::Decoder::new(reader).map_err(MorlocError::Io)?;
    decoder
        .window_log_max(DECODER_WINDOW_LOG_MAX)
        .map_err(MorlocError::Io)?;
    let mut total = 0;
    while total < dest.len() {
        match decoder.read(&mut dest[total..]) {
            Ok(0) => break,
            Ok(n) => total += n,
            Err(e) => return Err(MorlocError::Io(e)),
        }
    }
    Ok(total)
}

// ── Parallel frame decompression ──────────────────────────────────────────

/// Default upper bound on worker threads for the parallel paths. The
/// encoder + decoder both honor `MORLOC_FRAME_WORKERS` (positive
/// integer) to override; absent that, they use
/// `available_parallelism().min(MAX_FRAME_WORKERS)`.
pub const MAX_FRAME_WORKERS: usize = 16;

/// Read the worker count from `MORLOC_FRAME_WORKERS` or fall back to
/// `min(available_parallelism, MAX_FRAME_WORKERS)`. Returns at least 1.
/// Cached on first call (matches the `MORLOC_TRACE` / `MORLOC_QUIET`
/// pattern), so subsequent calls are a single atomic load.
pub fn frame_workers() -> usize {
    static CACHED: std::sync::OnceLock<usize> = std::sync::OnceLock::new();
    *CACHED.get_or_init(|| {
        if let Ok(s) = std::env::var("MORLOC_FRAME_WORKERS") {
            if let Ok(n) = s.parse::<usize>() {
                if n > 0 {
                    return n.min(MAX_FRAME_WORKERS);
                }
            }
        }
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1)
            .min(MAX_FRAME_WORKERS)
            .max(1)
    })
}

/// Decompress every frame in `frames` from `compressed` into the
/// non-overlapping slices of `dest` they index into. Compressed and
/// uncompressed offsets are computed from the prefix sums of
/// `compressed_size` and `uncompressed_size`; the caller has already
/// ensured `compressed.len()` and `dest.len()` match the totals.
///
/// Each frame is independent (its own header, FCS, content), so this
/// dispatches them to a small scoped worker pool with no
/// inter-worker synchronization on the destination -- workers each
/// own a disjoint slice of `dest` by construction.
pub fn parallel_decompress_frames(
    frames: &[FrameEntry],
    compressed: &[u8],
    dest: &mut [u8],
) -> Result<(), MorlocError> {
    if frames.is_empty() {
        return Ok(());
    }

    // Per-frame (comp_offset, comp_len, unc_offset, unc_len). Built
    // here so workers index into the original buffers by offset
    // rather than holding slice references they would have to split.
    let mut work: Vec<(usize, usize, usize, usize)> = Vec::with_capacity(frames.len());
    let mut comp_cur = 0usize;
    let mut unc_cur = 0usize;
    for f in frames {
        let cs = f.compressed_size as usize;
        let us = f.uncompressed_size as usize;
        work.push((comp_cur, cs, unc_cur, us));
        comp_cur += cs;
        unc_cur += us;
    }
    if comp_cur != compressed.len() {
        return Err(MorlocError::Packet(format!(
            "compressed buffer is {} bytes; frame index sums to {}",
            compressed.len(),
            comp_cur
        )));
    }
    if unc_cur != dest.len() {
        return Err(MorlocError::Packet(format!(
            "dest buffer is {} bytes; frame index sums to {}",
            dest.len(),
            unc_cur
        )));
    }

    // Single frame: skip the thread pool dance entirely.
    if frames.len() == 1 {
        let (comp_off, cs, unc_off, us) = work[0];
        let mut decoder = make_bulk_decompressor()?;
        let written = decoder
            .decompress_to_buffer(
                &compressed[comp_off..comp_off + cs],
                &mut dest[unc_off..unc_off + us],
            )
            .map_err(MorlocError::Io)?;
        if written != us {
            return Err(MorlocError::Packet(format!(
                "frame 0 decompressed {} bytes, expected {}",
                written, us
            )));
        }
        return Ok(());
    }

    let n_workers = frame_workers().min(frames.len());
    // Atomic work-stealing index: each worker fetches the next frame
    // index and processes it until none remain. Cheaper than an mpsc
    // channel and lets us share `&work` immutably across the scope.
    let next_idx = std::sync::atomic::AtomicUsize::new(0);
    // `*mut u8` is `!Send`, so we move the pointer as a `usize` and
    // reconstruct inside each worker. Soundness: each worker only
    // ever derefs the per-frame subrange `(unc_off, us)`, which the
    // prefix-sum construction guarantees is disjoint from every other
    // worker's subrange.
    let dest_ptr_addr = dest.as_mut_ptr() as usize;

    std::thread::scope(|s| -> Result<(), MorlocError> {
        let mut handles = Vec::with_capacity(n_workers);
        for _ in 0..n_workers {
            let work_ref = &work;
            let compressed_ref = compressed;
            let next_idx_ref = &next_idx;
            let h = s.spawn(move || -> Result<(), MorlocError> {
                // One bulk decompressor per worker, reused across
                // every frame this worker pulls. The `DCtx` keeps its
                // ~64 KiB internal state allocated; without reuse
                // we'd construct + tear it down per frame, which can
                // cost ~5-10% of decode wall time at our chunk size.
                let mut decoder = make_bulk_decompressor()
                    .map_err(MorlocError::Io)?;
                loop {
                    let i = next_idx_ref.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
                    if i >= work_ref.len() {
                        return Ok(());
                    }
                    let (comp_off, cs, unc_off, us) = work_ref[i];
                    let comp_slice = &compressed_ref[comp_off..comp_off + cs];
                    // SAFETY: per-frame subranges are disjoint by
                    // construction (prefix sums of `uncompressed_size`).
                    let dest_slice = unsafe {
                        std::slice::from_raw_parts_mut(
                            (dest_ptr_addr as *mut u8).add(unc_off),
                            us,
                        )
                    };
                    let written = decoder
                        .decompress_to_buffer(comp_slice, dest_slice)
                        .map_err(MorlocError::Io)?;
                    if written != us {
                        return Err(MorlocError::Packet(format!(
                            "frame {} decompressed {} bytes, expected {}",
                            i, written, us
                        )));
                    }
                }
            });
            handles.push(h);
        }
        let mut first_err: Option<MorlocError> = None;
        for h in handles {
            match h.join() {
                Ok(Ok(())) => {}
                Ok(Err(e)) => {
                    if first_err.is_none() {
                        first_err = Some(e);
                    }
                }
                Err(_) => {
                    if first_err.is_none() {
                        first_err = Some(MorlocError::Other(
                            "frame decompression worker panicked".into(),
                        ));
                    }
                }
            }
        }
        match first_err {
            Some(e) => Err(e),
            None => Ok(()),
        }
    })
}

// ── Streaming encoders (no intermediate Vec for the compressed bytes) ────

/// Stream-compress everything pulled from `reader` into `writer` as a
/// multi-frame zstd stream chunked at `FRAME_CHUNK_SIZE` boundaries.
/// Returns the frame index the caller stamps into the packet's
/// `METADATA_TYPE_FRAME_INDEX` entry. At compression level 0, copies
/// through unchanged and returns an empty frame index (decoders use
/// the packet header's compression byte to decide whether to consult
/// the index at all).
pub fn stream_encode_reader<W: Write, R: Read>(
    writer: W,
    reader: &mut R,
    lvl: CompressionLevel,
) -> Result<(W, Vec<FrameEntry>), MorlocError> {
    if lvl.is_none() {
        // std::io::copy uses its own 8 KiB ping-pong buffer, so a
        // BufWriter here would just double-buffer; route directly.
        let mut w = writer;
        std::io::copy(reader, &mut w).map_err(MorlocError::Io)?;
        return Ok((w, Vec::new()));
    }
    let mut encoder = MultiFrameEncoder::new(writer, lvl, FRAME_CHUNK_SIZE);
    std::io::copy(reader, &mut encoder).map_err(MorlocError::Io)?;
    encoder.finish()
}

/// Run a writer-producing closure with optional zstd compression
/// applied to its output. Used by streaming paths that need to
/// hand a `&mut dyn Write` sink to a flatten-and-emit routine but
/// also want compression wrapped transparently around the sink.
/// At level 0 the closure writes directly to `writer`; at level
/// 1-9 it writes to a `MultiFrameEncoder` that chunks output into
/// independent zstd frames of up to `FRAME_CHUNK_SIZE` bytes.
/// Returns the per-frame index for the caller to stamp into the
/// packet's `METADATA_TYPE_FRAME_INDEX` entry.
pub fn stream_encode_with<W, F>(
    writer: W,
    lvl: Option<CompressionLevel>,
    body: F,
) -> Result<(W, Vec<FrameEntry>), MorlocError>
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
        buf.flush().map_err(MorlocError::Io)?;
        let w = buf
            .into_inner()
            .map_err(|e| MorlocError::Io(e.into_error()))?;
        return Ok((w, Vec::new()));
    }
    let lvl = lvl.unwrap();
    let encoder = MultiFrameEncoder::new(writer, lvl, FRAME_CHUNK_SIZE);
    let mut buf = std::io::BufWriter::with_capacity(STREAM_INPUT_BUFFER, encoder);
    body(&mut buf)?;
    buf.flush().map_err(MorlocError::Io)?;
    let encoder = buf
        .into_inner()
        .map_err(|e| MorlocError::Io(e.into_error()))?;
    encoder.finish()
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

    let (compressed, frame_index) =
        compress_payload_zstd(&packet[payload_start..payload_end], lvl)?;

    // Splice the frame-index entry into the existing metadata block;
    // the helper handles "place after last valid entry, pad to
    // METADATA_BLOCK_ALIGNMENT".
    let frame_index_body = encode_frame_index_entry(&frame_index);
    let new_meta = crate::packet::append_metadata_entry(
        &packet[32..payload_start],
        METADATA_TYPE_FRAME_INDEX,
        &frame_index_body,
    );

    let mut new_header = header;
    // Writing a union field is safe in Rust (only reads require unsafe).
    new_header.command.data.compression = PACKET_COMPRESSION_ZSTD;
    new_header.offset = new_meta.len() as u32;
    let mut new_hdr_bytes = new_header.to_bytes();
    new_hdr_bytes[24..32].copy_from_slice(&(compressed.len() as u64).to_le_bytes());

    let mut out = Vec::with_capacity(32 + new_meta.len() + compressed.len());
    out.extend_from_slice(&new_hdr_bytes);
    out.extend_from_slice(&new_meta);
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

            // Frame index is mandatory under the multi-frame format.
            // A compressed packet without an index is rejected (the
            // hard cutover: old single-frame packets are no longer
            // readable by this decoder).
            let frames = crate::packet::read_frame_index_from_meta(packet)?
                .ok_or_else(|| MorlocError::Packet(
                    "compressed packet missing METADATA_TYPE_FRAME_INDEX entry".into(),
                ))?;

            // Total uncompressed size = sum of per-frame uncompressed
            // sizes. Each frame's compressed bytes live consecutively
            // in the payload region; we compute prefix sums of
            // compressed_size to find frame boundaries.
            let total_uncompressed: usize = frames
                .iter()
                .map(|f| f.uncompressed_size as usize)
                .sum();
            let total_compressed: usize = frames
                .iter()
                .map(|f| f.compressed_size as usize)
                .sum();
            if total_compressed != length {
                return Err(MorlocError::Packet(format!(
                    "frame index sums to {} compressed bytes but header.length = {}",
                    total_compressed, length
                )));
            }

            // Rebuild the metadata block without the frame-index entry
            // (the decompressed packet has no compression and the index
            // is meaningless). Pad to 32 bytes so the payload alignment
            // matches what `make_mesg_data_packet` produces for a fresh
            // uncompressed packet -- preserving byte-equality on a
            // compress/decompress round-trip from an originally-padded
            // input.
            let mut new_meta = Vec::new();
            for (kind, data) in iter_packet_metadata(packet)? {
                if kind == METADATA_TYPE_FRAME_INDEX {
                    continue;
                }
                new_meta.extend_from_slice(&METADATA_HEADER_MAGIC);
                new_meta.push(kind);
                new_meta.extend_from_slice(&(data.len() as u32).to_le_bytes());
                new_meta.extend_from_slice(data);
            }
            let padded_meta_len = new_meta.len().div_ceil(32) * 32;
            new_meta.resize(padded_meta_len, 0);

            let new_meta_size = new_meta.len();
            let mut new_header = header;
            // Writing a union field is safe (only reads require unsafe).
            new_header.command.data.compression = PACKET_COMPRESSION_NONE;
            new_header.offset = new_meta_size as u32;
            let mut new_hdr_bytes = new_header.to_bytes();
            new_hdr_bytes[24..32]
                .copy_from_slice(&(total_uncompressed as u64).to_le_bytes());

            let mut out = vec![0u8; 32 + new_meta_size + total_uncompressed];
            out[0..32].copy_from_slice(&new_hdr_bytes);
            out[32..32 + new_meta_size].copy_from_slice(&new_meta);
            let out_payload_start = 32 + new_meta_size;

            // Decompress all frames in parallel into disjoint slices
            // of the output payload region.
            let compressed = &packet[payload_start..payload_start + length];
            parallel_decompress_frames(
                &frames,
                compressed,
                &mut out[out_payload_start..out_payload_start + total_uncompressed],
            )?;

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
    fn parallel_encode_decode_matches_serial() {
        // The parallel encoder is order-preserving: dispatch a batch
        // of chunks to workers, sort results by chunk index, then emit
        // frames in order. The compressed bytes should be identical to
        // what a `batch_size = 1` (effectively serial) encoder
        // produces from the same input, since each frame is
        // independent and zstd compression is deterministic.
        let raw: Vec<u8> = (0..(FRAME_CHUNK_SIZE * 3 + 1234))
            .map(|i| (i & 0xFF) as u8)
            .collect();
        let lvl = CompressionLevel::from_u8(3).unwrap();

        // Serial encoding (batch_size = 1).
        let mut serial_out = Vec::new();
        let mut enc = MultiFrameEncoder::with_n_workers(
            &mut serial_out,
            lvl,
            FRAME_CHUNK_SIZE,
            1,
        );
        enc.write_all(&raw).unwrap();
        let (_w, serial_frames) = enc.finish().unwrap();

        // Parallel encoding (batch_size = 8 workers).
        let mut par_out = Vec::new();
        let mut enc = MultiFrameEncoder::with_n_workers(
            &mut par_out,
            lvl,
            FRAME_CHUNK_SIZE,
            8,
        );
        enc.write_all(&raw).unwrap();
        let (_w, par_frames) = enc.finish().unwrap();

        assert_eq!(serial_frames, par_frames, "frame index drifted across batch sizes");
        assert_eq!(serial_out, par_out, "compressed bytes drifted across batch sizes");

        // Parallel decode round-trip.
        let mut decoded = vec![0u8; raw.len()];
        parallel_decompress_frames(&par_frames, &par_out, &mut decoded).unwrap();
        assert_eq!(decoded, raw);
    }

    #[test]
    fn single_chunk_payload_does_not_spawn_workers() {
        // Small payloads that fit in one chunk should compress inline
        // in `finish()` without spawning a worker pool. We can't
        // observe thread spawning directly, but we can verify the
        // encoder produces a one-frame index for any payload <=
        // FRAME_CHUNK_SIZE and that finish() returns successfully.
        let raw = vec![0xAB; 4096]; // far smaller than FRAME_CHUNK_SIZE
        let lvl = CompressionLevel::from_u8(3).unwrap();
        let mut out = Vec::new();
        let mut enc = MultiFrameEncoder::new(&mut out, lvl, FRAME_CHUNK_SIZE);
        enc.write_all(&raw).unwrap();
        let (_w, frames) = enc.finish().unwrap();
        assert_eq!(frames.len(), 1);
        assert_eq!(frames[0].uncompressed_size, raw.len() as u64);
    }

    #[test]
    fn dropping_without_finish_cleans_up_workers() {
        // Construct an encoder, dispatch enough bytes to spawn the
        // worker pool, then drop without calling finish(). The Drop
        // impl must close the worker input channels and join all
        // worker threads, leaving no dangling threads. We verify by
        // dropping inside a thread::scope: if any worker survived
        // the encoder's Drop, the scope wouldn't be able to join.
        let raw = vec![0u8; FRAME_CHUNK_SIZE * 2];
        let lvl = CompressionLevel::from_u8(1).unwrap();
        std::thread::scope(|s| {
            let h = s.spawn(|| {
                let mut sink = Vec::new();
                let mut enc =
                    MultiFrameEncoder::with_n_workers(&mut sink, lvl, FRAME_CHUNK_SIZE, 4);
                // Write enough to spawn the pool (the first
                // dispatch_pending fires once `pending` hits one
                // chunk).
                enc.write_all(&raw).unwrap();
                // Drop without finish().
                drop(enc);
            });
            h.join().unwrap();
        });
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
        // verbatim -- no zstd frame, no frame index -- so callers can
        // rely on it as a uniform pass-through.
        let raw = vec![0xAB; 1024];
        let lvl = CompressionLevel::from_u8(0).unwrap();
        let (bytes, frames) = compress_payload_zstd(&raw, lvl).unwrap();
        assert_eq!(bytes, raw);
        assert!(frames.is_empty());
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
        // Reader-driven streaming encode produces a multi-frame zstd
        // stream that the legacy single-frame decoder still consumes
        // (concatenated independent frames are a valid zstd stream).
        // Use 4 MiB so chunking actually fires.
        let original: Vec<u8> = (0..(4 << 20)).map(|i| (i & 0x0F) as u8).collect();
        let lvl = CompressionLevel::from_u8(3).unwrap();
        let mut src: &[u8] = &original;
        let mut compressed: Vec<u8> = Vec::new();
        let (_w, frames) =
            stream_encode_reader(&mut compressed, &mut src, lvl).unwrap();
        // Round-trip via the per-frame decoder used by `decompress_packet`.
        let mut decoded = vec![0u8; original.len()];
        let mut comp_cur = 0usize;
        let mut unc_cur = 0usize;
        for frame in &frames {
            let cs = frame.compressed_size as usize;
            let us = frame.uncompressed_size as usize;
            let comp_slice = &compressed[comp_cur..comp_cur + cs];
            let unc_slice = &mut decoded[unc_cur..unc_cur + us];
            let written = decompress_payload_zstd_into(comp_slice, unc_slice).unwrap();
            assert_eq!(written, us);
            comp_cur += cs;
            unc_cur += us;
        }
        assert_eq!(comp_cur, compressed.len());
        assert_eq!(decoded, original);
    }

    #[test]
    fn compressed_packet_carries_frame_index() {
        // Every multi-frame compressed packet emitted by compress_packet
        // carries a METADATA_TYPE_FRAME_INDEX entry. The decoder relies
        // on this entry; without it, decompress_packet rejects the packet.
        for &size in &[1usize, 1024, 1 << 20] {
            let original = synthetic_packet(size);
            let lvl = CompressionLevel::from_u8(3).unwrap();
            let compressed = match compress_packet(&original, lvl).unwrap() {
                CompressOutcome::Compressed(v) => v,
                CompressOutcome::NoOp => panic!("expected compression at size {size}"),
            };
            let frames = crate::packet::read_frame_index_from_meta(&compressed)
                .unwrap()
                .unwrap_or_else(|| panic!("size={size}: frame index entry missing"));
            assert!(!frames.is_empty(), "size={size}: frame index has no entries");
            let total_unc: u64 = frames.iter().map(|f| f.uncompressed_size).sum();
            // synthetic_packet wraps `size` payload bytes; the frame
            // index sums to exactly that payload length.
            let unwrapped = decompress_packet(&compressed).unwrap();
            assert_eq!(unwrapped, original, "size={size}");
            // Total uncompressed across frames matches the recovered
            // payload's length field.
            let recovered_payload_len = u64::from_le_bytes(
                unwrapped[24..32].try_into().unwrap(),
            );
            assert_eq!(total_unc, recovered_payload_len, "size={size}");
        }
    }

    #[test]
    fn multiframe_roundtrip_across_chunk_boundary() {
        // FRAME_CHUNK_SIZE is 16 MiB; pick sizes that straddle the
        // boundary so the encoder emits multiple frames.
        let chunk = FRAME_CHUNK_SIZE;
        for &size in &[
            chunk,             // exactly one chunk
            chunk + 1,         // one chunk + 1 byte
            chunk * 2,         // two full chunks
            chunk * 3 + 1234,  // three chunks + partial
        ] {
            let raw: Vec<u8> = (0..size).map(|i| (i & 0xFF) as u8).collect();
            let lvl = CompressionLevel::from_u8(1).unwrap();
            let (bytes, frames) = compress_payload_zstd(&raw, lvl).unwrap();
            let expected_frame_count = size.div_ceil(chunk);
            assert_eq!(
                frames.len(),
                expected_frame_count,
                "size={size}: expected {} frames, got {}",
                expected_frame_count,
                frames.len()
            );
            let total_unc: u64 = frames.iter().map(|f| f.uncompressed_size).sum();
            let total_comp: u64 = frames.iter().map(|f| f.compressed_size).sum();
            assert_eq!(total_unc as usize, size);
            assert_eq!(total_comp as usize, bytes.len());
            // Decompress each frame and reassemble.
            let mut decoded = vec![0u8; size];
            let mut comp_cur = 0usize;
            let mut unc_cur = 0usize;
            for frame in &frames {
                let cs = frame.compressed_size as usize;
                let us = frame.uncompressed_size as usize;
                let comp_slice = &bytes[comp_cur..comp_cur + cs];
                let unc_slice = &mut decoded[unc_cur..unc_cur + us];
                let written =
                    decompress_payload_zstd_into(comp_slice, unc_slice).unwrap();
                assert_eq!(written, us);
                comp_cur += cs;
                unc_cur += us;
            }
            assert_eq!(decoded, raw, "size={size}: roundtrip mismatch");
        }
    }

    #[test]
    fn decompress_packet_rejects_compressed_without_frame_index() {
        // Hard cutover: a compressed packet MUST carry the frame index
        // metadata entry. Construct a packet with the ZSTD compression
        // byte but only a schema entry, and confirm rejection.
        let raw = vec![0u8; 1024];
        let schema_bytes = b"au1\0";
        let mut meta = Vec::new();
        meta.extend_from_slice(&METADATA_HEADER_MAGIC);
        meta.push(crate::packet::METADATA_TYPE_SCHEMA_STRING);
        meta.extend_from_slice(&(schema_bytes.len() as u32).to_le_bytes());
        meta.extend_from_slice(schema_bytes);
        let mut hdr = PacketHeader::data_mesg(
            crate::packet::PACKET_FORMAT_VOIDSTAR,
            raw.len() as u64,
        );
        hdr.offset = meta.len() as u32;
        // Patch compression byte to ZSTD.
        let mut hdr_bytes = hdr.to_bytes();
        hdr_bytes[15] = PACKET_COMPRESSION_ZSTD;
        let mut packet = hdr_bytes.to_vec();
        packet.extend_from_slice(&meta);
        packet.extend_from_slice(&raw);
        let err = decompress_packet(&packet).unwrap_err();
        match err {
            MorlocError::Packet(_) => {}
            other => panic!("expected Packet error, got {other:?}"),
        }
    }

    #[test]
    fn streamed_output_carries_frame_index() {
        // stream_encode_with -- the streaming-emit path used by
        // stream_packet_to_fd -- must produce a parseable multi-frame
        // stream with a non-empty frame index.
        let raw: Vec<u8> = (0..(4 << 20)).map(|i| (i & 0xFF) as u8).collect();
        let lvl = CompressionLevel::from_u8(3).unwrap();
        let frame_buf: Vec<u8> = Vec::new();
        let (compressed, frames) =
            stream_encode_with(frame_buf, Some(lvl), |sink| {
                sink.write_all(&raw).map_err(MorlocError::Io)
            })
            .unwrap();
        assert!(!frames.is_empty());
        let total_unc: u64 = frames.iter().map(|f| f.uncompressed_size).sum();
        let total_comp: u64 = frames.iter().map(|f| f.compressed_size).sum();
        assert_eq!(total_unc as usize, raw.len());
        assert_eq!(total_comp as usize, compressed.len());
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
