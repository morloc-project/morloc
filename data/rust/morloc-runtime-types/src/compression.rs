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
//!   independent zstd frames, each of at most `FRAME_CHUNK_SIZE`
//!   uncompressed bytes. The metadata block carries a
//!   `METADATA_TYPE_FRAME_INDEX` entry listing per-frame
//!   `(uncompressed_size, compressed_size)`.
//! * On compression, `header.length` is the sum of per-frame compressed
//!   sizes; on decompression it is restored to the sum of per-frame
//!   uncompressed sizes.
//!
//! User-facing level 0-9 maps to zstd presets (see `CompressionLevel`).

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

/// Target uncompressed bytes per zstd frame. Frames are independent, so
/// they can be decompressed in parallel.
pub const FRAME_CHUNK_SIZE: usize = 16 * 1024 * 1024;

/// The size of the frames a compressed stream sub-packet is cut into: a
/// function of the level alone, so a stream's bytes never depend on the
/// host, its cores or its queue. Small frames let one sub-packet compress
/// and decompress on several threads; the long-mode levels (7-9) keep
/// whole frames, whose window is what they are for.
pub fn subpacket_frame_size(lvl: CompressionLevel) -> usize {
    if lvl.use_long() {
        FRAME_CHUNK_SIZE
    } else {
        2 * 1024 * 1024
    }
}

/// Worst-case bytes of a frame-index metadata entry for up to
/// `max_frames` frames.
pub const fn frame_index_entry_max_bytes(max_frames: usize) -> usize {
    FRAME_INDEX_HEADER_BYTES + max_frames * FRAME_ENTRY_BYTES
}

/// Worst-case frame count for a payload of
/// `estimated_uncompressed_bytes`; at least 1.
pub fn max_frames_for(estimated_uncompressed_bytes: usize) -> usize {
    if estimated_uncompressed_bytes == 0 {
        1
    } else {
        estimated_uncompressed_bytes.div_ceil(FRAME_CHUNK_SIZE)
    }
}

fn make_bulk_decompressor<'a>() -> std::io::Result<zstd::bulk::Decompressor<'a>> {
    let mut d = zstd::bulk::Decompressor::new()?;
    d.window_log_max(DECODER_WINDOW_LOG_MAX)?;
    Ok(d)
}

fn make_bulk_compressor<'a>(
    lvl: CompressionLevel,
) -> std::io::Result<zstd::bulk::Compressor<'a>> {
    let mut c = zstd::bulk::Compressor::new(lvl.zstd_level())?;
    if lvl.use_long() {
        c.window_log(LONG_WINDOW_LOG)?;
    }
    Ok(c)
}

/// Compress `raw` into one independent zstd frame with its size pledged,
/// writing the frame into `out`.
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

fn compress_one_frame(
    raw: &[u8],
    lvl: CompressionLevel,
) -> Result<Vec<u8>, MorlocError> {
    let mut compressor = make_bulk_compressor(lvl).map_err(MorlocError::Io)?;
    let mut out = Vec::new();
    compress_one_frame_into(&mut compressor, raw, &mut out)?;
    Ok(out)
}

/// A zstd context for one level, kept by a thread that compresses many
/// payloads of at most `FRAME_CHUNK_SIZE` bytes, each into one frame. The
/// output equals `compress_payload_zstd` on the same payload.
pub struct FrameCompressor {
    inner: zstd::bulk::Compressor<'static>,
}

impl FrameCompressor {
    pub fn new(lvl: CompressionLevel) -> Result<Self, MorlocError> {
        Ok(FrameCompressor { inner: make_bulk_compressor(lvl).map_err(MorlocError::Io)? })
    }

    /// Compress `raw` into one frame; returns the frame and its index.
    pub fn compress(&mut self, raw: &[u8]) -> Result<(Vec<u8>, Vec<FrameEntry>), MorlocError> {
        if raw.len() > FRAME_CHUNK_SIZE {
            return Err(MorlocError::Other(format!(
                "a single zstd frame holds at most {FRAME_CHUNK_SIZE} bytes, got {}",
                raw.len()
            )));
        }
        let mut out = Vec::new();
        compress_one_frame_into(&mut self.inner, raw, &mut out)?;
        let entry = FrameEntry {
            uncompressed_size: raw.len() as u64,
            compressed_size: out.len() as u64,
        };
        Ok((out, vec![entry]))
    }
}

struct EncoderPool {
    worker_txs: Vec<std::sync::mpsc::SyncSender<(usize, Vec<u8>)>>,
    result_rx: std::sync::mpsc::Receiver<(usize, std::io::Result<Vec<u8>>)>,
    worker_handles: Vec<std::thread::JoinHandle<()>>,
}

/// A writer that compresses its input into independent zstd frames of
/// `chunk_size` uncompressed bytes, in input order. `finish()` returns the
/// frame index.
pub struct MultiFrameEncoder<W: Write> {
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

    /// Construct an encoder with an explicit worker count.
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

    /// Panics if called after `finish()`.
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
                let mut compressor = match make_bulk_compressor(lvl) {
                    Ok(c) => c,
                    Err(e) => {
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

    /// Write any results that are ready, without blocking.
    fn drain_ready_results(&mut self) -> std::io::Result<()> {
        loop {
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

    /// Block until one result arrives and write what is in order. Returns
    /// `Ok(false)` when no worker is left.
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

    /// Write the frames in the reorder buffer that are next in sequence.
    fn write_in_order(&mut self) -> std::io::Result<()> {
        while let Some(bytes) = self.reorder_buf.remove(&self.write_next_seq) {
            let uncompressed_size = self
                .seq_uncompressed_size
                .remove(&self.write_next_seq)
                .expect("seq_uncompressed_size missing entry");
            let compressed_size = bytes.len();
            self.writer_mut().write_all(&bytes)?;
            self.frames.push(FrameEntry {
                uncompressed_size: uncompressed_size as u64,
                compressed_size: compressed_size as u64,
            });
            self.write_next_seq += 1;
        }
        Ok(())
    }

    /// Hand the `pending` chunk to a worker, spawning the pool on first use.
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
        self.ensure_pool();
        // Drain ready results first so the writer doesn't fall too
        // far behind and so the reorder buffer doesn't grow unbounded.
        self.drain_ready_results()?;
        let pool = self.pool.as_ref().expect("pool just spawned");
        let worker_idx = seq % pool.worker_txs.len();
        if let Err(e) = pool.worker_txs[worker_idx].send((seq, chunk)) {
            return Err(std::io::Error::other(format!(
                "worker channel closed: {e}"
            )));
        }
        Ok(())
    }

    /// Flush pending bytes as a final frame; return the inner writer and the
    /// frame index.
    pub fn finish(mut self) -> Result<(W, Vec<FrameEntry>), MorlocError> {
        // No chunk dispatched yet: compress inline, one frame even for empty input.
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
        }
        let writer = self.writer.take().expect("writer present");
        let frames = std::mem::take(&mut self.frames);
        Ok((writer, frames))
    }
}

impl<W: Write> Drop for MultiFrameEncoder<W> {
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
        // A frame per flush would fragment the output; `finish()` emits the
        // final partial frame.
        Ok(())
    }
}

// ── Compression level (0-9 user-facing, mapped to zstd 1-22) ──────────────

/// User-facing compression preset: 0 disables compression, 1-9 select a
/// zstd level and long mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CompressionLevel(u8);

impl CompressionLevel {
    /// No compression.
    pub const NONE: CompressionLevel = CompressionLevel(0);

    /// Build a CompressionLevel from a morloc `Int`, as the user wrote it.
    pub fn from_int(n: i64) -> Result<Self, MorlocError> {
        match u8::try_from(n) {
            Ok(b) if b <= 9 => Ok(CompressionLevel(b)),
            _ => Err(MorlocError::Other(format!(
                "compression level must be in 0..=9, got {n}"
            ))),
        }
    }

    pub fn from_u8(n: u8) -> Result<Self, MorlocError> {
        Self::from_int(i64::from(n))
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

    /// Whether to enable zstd long-range mode.
    pub fn use_long(self) -> bool {
        self.0 >= 7
    }
}

// ── Long-mode window logs ─────────────────────────────────────────────────

/// Encoder window log used when long-mode is active. 27 == 128 MiB,
/// matching the zstd CLI's --long default.
pub const LONG_WINDOW_LOG: u32 = 27;

/// Decoder windowLogMax: must cover any window an encoder might have used.
/// 31 == 2 GiB, the maximum zstd permits.
const DECODER_WINDOW_LOG_MAX: u32 = 31;

/// Buffer between the byte-producing walker and the encoder.
const STREAM_INPUT_BUFFER: usize = 256 * 1024;

// ── Raw-payload compress / decompress ─────────────────────────────────────

/// Compress raw bytes into zstd frames, returning them with their index.
/// Level 0 returns a copy of `raw` and an empty index.
pub fn compress_payload_zstd(
    raw: &[u8],
    lvl: CompressionLevel,
) -> Result<(Vec<u8>, Vec<FrameEntry>), MorlocError> {
    if lvl.is_none() {
        return Ok((raw.to_vec(), Vec::new()));
    }
    let mut encoder = MultiFrameEncoder::new(Vec::new(), lvl, FRAME_CHUNK_SIZE);
    encoder.write_all(raw).map_err(MorlocError::Io)?;
    encoder.finish()
}

/// Decompress one zstd frame from `reader` into `dest`; returns the number
/// of bytes written.
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

/// Upper bound on frame worker threads, including a
/// `MORLOC_FRAME_WORKERS` override.
pub const MAX_FRAME_WORKERS: usize = 16;

/// The worker count: `MORLOC_FRAME_WORKERS` if positive, else the available
/// parallelism, capped at `MAX_FRAME_WORKERS`; at least 1. Read once.
pub fn frame_workers() -> usize {
    static CACHED: crate::publish_once::PublishOnce<usize> = crate::publish_once::PublishOnce::new();
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

/// Decompress every frame in `frames` from `compressed` into its slice of
/// `dest`. The index totals must equal the two buffer lengths.
pub fn parallel_decompress_frames(
    frames: &[FrameEntry],
    compressed: &[u8],
    dest: &mut [u8],
) -> Result<(), MorlocError> {
    if frames.is_empty() {
        return Ok(());
    }

    // The totals are checked first, so no prefix sum below can overflow.
    let (unc_cur, comp_cur) = crate::packet::frame_totals(frames)?;
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

    let mut work: Vec<(usize, usize, usize, usize)> = Vec::with_capacity(frames.len());
    let (mut comp_off, mut unc_off) = (0usize, 0usize);
    for f in frames {
        let cs = crate::width::usize_from_u64(f.compressed_size);
        let us = crate::width::usize_from_u64(f.uncompressed_size);
        work.push((comp_off, cs, unc_off, us));
        comp_off += cs;
        unc_off += us;
    }

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
    let next_idx = std::sync::atomic::AtomicUsize::new(0);
    let dest_ptr_addr = dest.as_mut_ptr() as usize;

    std::thread::scope(|s| -> Result<(), MorlocError> {
        let mut handles = Vec::with_capacity(n_workers);
        for _ in 0..n_workers {
            let work_ref = &work;
            let compressed_ref = compressed;
            let next_idx_ref = &next_idx;
            let h = s.spawn(move || -> Result<(), MorlocError> {
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

/// One payload for [`parallel_decompress_frame_sets`]: its frame index, its
/// compressed bytes and where its decompressed bytes go.
pub struct FrameSet<'a> {
    pub frames: &'a [FrameEntry],
    pub compressed: &'a [u8],
    pub dest: &'a mut [u8],
}

/// Decompress the frames of several payloads on one set of `frame_workers()`
/// threads, so payloads of a frame or two each still decode in parallel.
/// Each set is checked as [`parallel_decompress_frames`] checks its one
/// payload before anything is decoded; the result is the first error, or
/// `Ok` when every frame of every set decoded to its indexed size.
pub fn parallel_decompress_frame_sets(sets: &mut [FrameSet<'_>]) -> Result<(), MorlocError> {
    // (set, comp_off, comp_len, unc_off, unc_len)
    let mut work: Vec<(usize, usize, usize, usize, usize)> = Vec::new();
    for (k, set) in sets.iter().enumerate() {
        let (unc, comp) = crate::packet::frame_totals(set.frames)?;
        if comp != set.compressed.len() {
            return Err(MorlocError::Packet(format!(
                "compressed buffer is {} bytes; frame index sums to {}",
                set.compressed.len(),
                comp
            )));
        }
        if unc != set.dest.len() {
            return Err(MorlocError::Packet(format!(
                "dest buffer is {} bytes; frame index sums to {}",
                set.dest.len(),
                unc
            )));
        }
        let (mut comp_off, mut unc_off) = (0usize, 0usize);
        for f in set.frames {
            let cs = crate::width::usize_from_u64(f.compressed_size);
            let us = crate::width::usize_from_u64(f.uncompressed_size);
            work.push((k, comp_off, cs, unc_off, us));
            comp_off += cs;
            unc_off += us;
        }
    }
    if work.is_empty() {
        return Ok(());
    }
    let dests: Vec<(usize, usize)> = sets.iter_mut().map(|s| (s.dest.as_mut_ptr() as usize, s.dest.len())).collect();
    let comps: Vec<&[u8]> = sets.iter().map(|s| s.compressed).collect();
    let decode_one = |decoder: &mut zstd::bulk::Decompressor<'_>, i: usize| -> Result<(), MorlocError> {
        let (k, comp_off, cs, unc_off, us) = work[i];
        let comp_slice = &comps[k][comp_off..comp_off + cs];
        // SAFETY: frame `i`'s destination range lies inside set k's dest
        // (checked against the index above) and no other frame's overlaps it.
        let dest_slice = unsafe { std::slice::from_raw_parts_mut((dests[k].0 as *mut u8).add(unc_off), us) };
        let written = decoder.decompress_to_buffer(comp_slice, dest_slice).map_err(MorlocError::Io)?;
        if written != us {
            return Err(MorlocError::Packet(format!(
                "frame {} decompressed {} bytes, expected {}",
                i, written, us
            )));
        }
        Ok(())
    };
    let n_workers = frame_workers().min(work.len());
    if n_workers <= 1 {
        let mut decoder = make_bulk_decompressor().map_err(MorlocError::Io)?;
        for i in 0..work.len() {
            decode_one(&mut decoder, i)?;
        }
        return Ok(());
    }
    let next_idx = std::sync::atomic::AtomicUsize::new(0);
    let first_err: std::sync::Mutex<Option<MorlocError>> = std::sync::Mutex::new(None);
    let work = || {
        let fail = |e: MorlocError| {
            first_err.lock().unwrap_or_else(|p| p.into_inner()).get_or_insert(e);
            next_idx.store(usize::MAX / 2, std::sync::atomic::Ordering::Relaxed);
        };
        let mut decoder = match make_bulk_decompressor() {
            Ok(d) => d,
            Err(e) => return fail(MorlocError::Io(e)),
        };
        loop {
            let i = next_idx.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            if i >= work.len() {
                return;
            }
            if let Err(e) = decode_one(&mut decoder, i) {
                return fail(e);
            }
        }
    };
    std::thread::scope(|s| {
        // The calling thread works too, so a refused spawn only narrows it.
        for _ in 1..n_workers {
            if std::thread::Builder::new().spawn_scoped(s, &work).is_err() {
                break;
            }
        }
        work();
    });
    first_err.into_inner().unwrap_or_else(|p| p.into_inner()).map_or(Ok(()), Err)
}

// ── Streaming encoders (no intermediate Vec for the compressed bytes) ────

/// Compress everything read from `reader` into `writer` as frames of
/// `FRAME_CHUNK_SIZE` bytes, returning the frame index. Level 0 copies
/// unchanged and returns an empty index.
pub fn stream_encode_reader<W: Write, R: Read>(
    writer: W,
    reader: &mut R,
    lvl: CompressionLevel,
) -> Result<(W, Vec<FrameEntry>), MorlocError> {
    if lvl.is_none() {
        let mut w = writer;
        std::io::copy(reader, &mut w).map_err(MorlocError::Io)?;
        return Ok((w, Vec::new()));
    }
    let mut encoder = MultiFrameEncoder::new(writer, lvl, FRAME_CHUNK_SIZE);
    std::io::copy(reader, &mut encoder).map_err(MorlocError::Io)?;
    encoder.finish()
}

/// Run `body` against `writer`, compressed into frames of
/// `FRAME_CHUNK_SIZE` bytes unless `lvl` is `None` or 0. Returns the frame
/// index, empty when uncompressed.
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

/// Result of `compress_packet`: the new packet, or `NoOp` when the input
/// is left as it is.
pub enum CompressOutcome {
    Compressed(Vec<u8>),
    NoOp,
}

/// Compress the payload of a DATA packet with source MESG, appending a
/// frame-index metadata entry and setting the compression byte and
/// length. Returns `NoOp` at level 0 and for any other packet.
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

    let frame_index_body = encode_frame_index_entry(&frame_index);
    let new_meta = crate::packet::append_metadata_entry(
        &packet[32..payload_start],
        METADATA_TYPE_FRAME_INDEX,
        &frame_index_body,
    );

    let mut new_header = header;
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

/// Inverse of `compress_packet`. A packet that is not a compressed DATA
/// packet is returned as a copy; an unknown compression byte is an error.
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

            let frames = crate::packet::read_frame_index_from_meta(packet)?
                .ok_or_else(|| MorlocError::Packet(
                    "compressed packet missing METADATA_TYPE_FRAME_INDEX entry".into(),
                ))?;

            let (total_uncompressed, total_compressed) = crate::packet::frame_totals(&frames)?;
            if total_compressed != length {
                return Err(MorlocError::Packet(format!(
                    "frame index sums to {} compressed bytes but header.length = {}",
                    total_compressed, length
                )));
            }

            // Drop the frame-index entry and pad to 32 bytes, as
            // `make_mesg_data_packet` pads, so a round trip is byte-exact.
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
            new_header.command.data.compression = PACKET_COMPRESSION_NONE;
            new_header.offset = new_meta_size as u32;
            let mut new_hdr_bytes = new_header.to_bytes();
            new_hdr_bytes[24..32]
                .copy_from_slice(&(total_uncompressed as u64).to_le_bytes());

            let mut out = vec![0u8; 32 + new_meta_size + total_uncompressed];
            out[0..32].copy_from_slice(&new_hdr_bytes);
            out[32..32 + new_meta_size].copy_from_slice(&new_meta);
            let out_payload_start = 32 + new_meta_size;

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

/// Decompress `bytes` if they are a compressed DATA packet. Anything else,
/// including non-packets and truncated packets, is returned borrowed and
/// unchanged.
pub fn decompress_packet_if_needed(bytes: &[u8]) -> Result<Cow<'_, [u8]>, MorlocError> {
    if bytes.len() < 4 {
        return Ok(Cow::Borrowed(bytes));
    }
    let magic = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]);
    if magic != PACKET_MAGIC {
        return Ok(Cow::Borrowed(bytes));
    }
    if bytes.len() < 32 {
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
    use crate::packet::make_mesg_data_packet;
    use crate::schema::parse_schema;

    #[test]
    fn level_is_checked_at_full_int_width() {
        for n in 0..=9 {
            assert_eq!(CompressionLevel::from_int(n).unwrap().raw(), n as u8);
        }
        for n in [-1, 10, 255, 256, 265, i64::MIN, i64::MAX] {
            let e = CompressionLevel::from_int(n).unwrap_err().to_string();
            assert!(e.ends_with(&format!("got {n}")), "{e}");
        }
    }

    /// A packet with a highly redundant payload.
    fn synthetic_packet(payload_len: usize) -> Vec<u8> {
        let payload: Vec<u8> = (0..payload_len).map(|i| (i & 0x07) as u8).collect();
        let schema = parse_schema("au1").unwrap();
        make_mesg_data_packet(&payload, &schema)
    }

    #[test]
    fn roundtrip_each_level() {
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
        // Output must not depend on the worker count.
        let raw: Vec<u8> = (0..(FRAME_CHUNK_SIZE * 3 + 1234))
            .map(|i| (i & 0xFF) as u8)
            .collect();
        let lvl = CompressionLevel::from_u8(3).unwrap();

        let mut serial_out = Vec::new();
        let mut enc = MultiFrameEncoder::with_n_workers(
            &mut serial_out,
            lvl,
            FRAME_CHUNK_SIZE,
            1,
        );
        enc.write_all(&raw).unwrap();
        let (_w, serial_frames) = enc.finish().unwrap();

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

        let mut decoded = vec![0u8; raw.len()];
        parallel_decompress_frames(&par_frames, &par_out, &mut decoded).unwrap();
        assert_eq!(decoded, raw);
    }

    #[test]
    fn single_chunk_payload_does_not_spawn_workers() {
        // A payload within one chunk yields a one-frame index.
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
        // Dropping an encoder with a live worker pool, without finish(), must
        // not hang or panic.
        let raw = vec![0u8; FRAME_CHUNK_SIZE * 2];
        let lvl = CompressionLevel::from_u8(1).unwrap();
        std::thread::scope(|s| {
            let h = s.spawn(|| {
                let mut sink = Vec::new();
                let mut enc =
                    MultiFrameEncoder::with_n_workers(&mut sink, lvl, FRAME_CHUNK_SIZE, 4);
                enc.write_all(&raw).unwrap();
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
        let raw = vec![0xAB; 1024];
        let lvl = CompressionLevel::from_u8(0).unwrap();
        let (bytes, frames) = compress_payload_zstd(&raw, lvl).unwrap();
        assert_eq!(bytes, raw);
        assert!(frames.is_empty());
    }

    #[test]
    fn empty_payload_roundtrip() {
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
        let original: Vec<u8> = (0..(4 << 20)).map(|i| (i & 0x0F) as u8).collect();
        let lvl = CompressionLevel::from_u8(3).unwrap();
        let mut src: &[u8] = &original;
        let mut compressed: Vec<u8> = Vec::new();
        let (_w, frames) =
            stream_encode_reader(&mut compressed, &mut src, lvl).unwrap();
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
        // compress_packet always writes a frame index.
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
            let unwrapped = decompress_packet(&compressed).unwrap();
            assert_eq!(unwrapped, original, "size={size}");
            let recovered_payload_len = u64::from_le_bytes(
                unwrapped[24..32].try_into().unwrap(),
            );
            assert_eq!(total_unc, recovered_payload_len, "size={size}");
        }
    }

    #[test]
    fn multiframe_roundtrip_across_chunk_boundary() {
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
        // A ZSTD packet with no frame-index entry is rejected.
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
        // One small frame, one frame of exactly 1 MiB, and one full
        // 16 MiB frame must roundtrip identically across levels 1, 3, 9.
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

/// The element count of the data sub-packet starting at `off` in `file`:
/// the array length its payload begins with. A compressed payload is
/// decoded only as far as those first bytes.
pub fn subpacket_elem_count(file: &[u8], off: u64) -> Result<u64, MorlocError> {
    let off = off as usize;
    let header_bytes: &[u8; 32] = file
        .get(off..off + 32)
        .and_then(|b| b.try_into().ok())
        .ok_or_else(|| MorlocError::Packet("sub-packet header past the end of the file".into()))?;
    let header = crate::packet::PacketHeader::from_bytes(header_bytes)?;
    if !header.is_data() {
        return Err(MorlocError::Packet("expected a data sub-packet".into()));
    }
    // SAFETY: is_data() implies the data variant of the command union.
    let compression = unsafe { header.command.data.compression };
    let start = off + 32 + header.offset as usize;
    let payload = file
        .get(start..start + header.length as usize)
        .ok_or_else(|| MorlocError::Packet("sub-packet payload past the end of the file".into()))?;
    let mut first = [0u8; 8];
    match compression {
        crate::packet::PACKET_COMPRESSION_NONE => {
            first.copy_from_slice(payload.get(..8).ok_or_else(|| {
                MorlocError::Packet("sub-packet payload shorter than its length".into())
            })?);
        }
        crate::packet::PACKET_COMPRESSION_ZSTD => {
            let mut decoder = zstd::stream::read::Decoder::with_buffer(payload)
                .map_err(|e| MorlocError::Packet(format!("sub-packet payload: {e}")))?;
            decoder
                .read_exact(&mut first)
                .map_err(|e| MorlocError::Packet(format!("sub-packet payload: {e}")))?;
        }
        other => {
            return Err(MorlocError::Packet(format!("sub-packet has unknown compression byte {other}")));
        }
    }
    Ok(u64::from_le_bytes(first))
}
