//! The custodian of written streams (model/runtime/streams.md, SLOT-10..16).

use std::sync::atomic::{AtomicIsize, AtomicU32, AtomicU64, AtomicU8, Ordering};
use std::time::Duration;

use morloc_runtime_types::shm_types::{RelPtr, RELNULL};
use morloc_runtime_types::wait_word;

use crate::error::MorlocError;

pub(crate) const QUEUE_SLOTS: usize = 8;
const MAX_DEPTH: usize = QUEUE_SLOTS - 1;
const LEVEL0_DEPTH: usize = 1;

pub(crate) const ENTRY_BUFFER: u32 = 1;
pub(crate) const ENTRY_BLOCK: u32 = 2;
pub(crate) const ENTRY_FLUSH: u32 = 3;
pub(crate) const ENTRY_CLOSE: u32 = 4;

pub(crate) const STATUS_DISCARD: u32 = 255;

pub(crate) const FAILED_IO: u32 = 1;
pub(crate) const FAILED_PIPE_CLOSED: u32 = 2;

const ERR_BYTES: usize = 240;
const WAIT_SLICE: Duration = Duration::from_millis(100);
const SPIN: Duration = Duration::from_micros(50);

#[repr(C)]
pub(crate) struct Entry {
    kind: AtomicU32,
    status: AtomicU32,
    block: AtomicIsize,
    elems: AtomicU64,
    index_cap: AtomicU64,
    data_used: AtomicU64,
    len: AtomicU64,
    oversize: AtomicU32,
    level: AtomicU32,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) struct Item {
    pub kind: u32,
    pub status: u32,
    pub block: RelPtr,
    pub elems: u64,
    pub index_cap: u64,
    pub data_used: u64,
    pub len: u64,
    pub oversize: bool,
    pub level: u8,
}

impl Item {
    pub(crate) fn marker(kind: u32, status: u32) -> Item {
        Item { kind, status, block: RELNULL, elems: 0, index_cap: 0, data_used: 0, len: 0, oversize: false, level: 0 }
    }
}

#[repr(C)]
pub(crate) struct CustodyQueue {
    tail: AtomicU32,
    head: AtomicU32,
    done: AtomicU32,
    failed: AtomicU32,
    spare_tail: AtomicU32,
    spare_head: AtomicU32,
    depth: AtomicU32,
    host_pid: AtomicU32,
    host_start: AtomicU64,
    err_len: AtomicU32,
    closed: AtomicU32,
    pinned: AtomicU32,
    stopped: AtomicU32,
    consumer: AtomicU32,
    epoch: AtomicU32,
    closer_pid: AtomicU32,
    closer_start: AtomicU64,
    entries: [Entry; QUEUE_SLOTS],
    spares: [AtomicIsize; QUEUE_SLOTS],
    err: [AtomicU8; ERR_BYTES],
}

impl CustodyQueue {
    fn host_alive(&self) -> bool {
        if self.stopped.load(Ordering::Acquire) != 0 {
            return false;
        }
        let pid = self.host_pid.load(Ordering::Acquire);
        pid == 0 || morloc_runtime_types::process::alive(pid, self.host_start.load(Ordering::Acquire))
    }

    /// Whether a custodian has taken this stream.
    pub(crate) fn hosted(&self) -> bool {
        self.host_pid.load(Ordering::Acquire) != 0
    }

    /// Whether the stream's writer has stopped for good.
    pub(crate) fn is_stopped(&self) -> bool {
        self.stopped.load(Ordering::Acquire) != 0
    }

    /// Which opening of its slot this queue serves.
    pub(crate) fn epoch(&self) -> u32 {
        self.epoch.load(Ordering::Acquire)
    }

    /// Whether a writer may still pop this queue.
    pub(crate) fn has_consumer(&self) -> bool {
        self.consumer.load(Ordering::Acquire) != 0
    }

    fn gone() -> MorlocError {
        MorlocError::Other("the process writing this stream's file is gone".into())
    }

    pub(crate) fn set_host(&self, pid: u32, start: u64) {
        self.host_start.store(start, Ordering::Release);
        self.host_pid.store(pid, Ordering::Release);
    }

    pub(crate) fn set_depth(&self, depth: usize) {
        self.depth.store(depth.clamp(1, MAX_DEPTH) as u32, Ordering::Release);
    }

    /// Ready the queue for a new stream in its slot. The counters run on
    /// across streams, so a waiter from an earlier one never sees them go back.
    pub(crate) fn reset_for_open(&self, depth: usize) {
        self.failed.store(0, Ordering::Relaxed);
        self.err_len.store(0, Ordering::Relaxed);
        self.closed.store(0, Ordering::Relaxed);
        self.pinned.store(0, Ordering::Relaxed);
        self.closer_pid.store(0, Ordering::Relaxed);
        self.closer_start.store(0, Ordering::Relaxed);
        self.host_pid.store(0, Ordering::Relaxed);
        self.host_start.store(0, Ordering::Relaxed);
        self.stopped.store(0, Ordering::Relaxed);
        self.set_depth(depth);
        self.epoch.fetch_add(1, Ordering::AcqRel);
    }

    /// Pin the stream's compression level at its first write; `Err` holds
    /// the pinned level when `level` differs.
    pub(crate) fn pin_level(&self, slot_level: &AtomicU8, level: u8) -> Result<(), u8> {
        if self.pinned.load(Ordering::Acquire) == 0 {
            slot_level.store(level, Ordering::Relaxed);
            // SLOT-12: an uncompressed batch needs little room behind its writer.
            if level == 0 {
                self.set_depth((self.depth() as usize).min(LEVEL0_DEPTH));
            }
            self.pinned.store(1, Ordering::Release);
            return Ok(());
        }
        let pinned = slot_level.load(Ordering::Relaxed);
        if pinned == level { Ok(()) } else { Err(pinned) }
    }

    pub(crate) fn closer(&self) -> (u32, u64) {
        (self.closer_pid.load(Ordering::Acquire), self.closer_start.load(Ordering::Acquire))
    }

    pub(crate) fn depth(&self) -> u32 {
        self.depth.load(Ordering::Acquire).clamp(1, MAX_DEPTH as u32)
    }

    pub(crate) fn failure(&self) -> Option<MorlocError> {
        match self.failed.load(Ordering::Acquire) {
            0 => None,
            FAILED_PIPE_CLOSED => Some(MorlocError::PipeClosed),
            _ => {
                let n = (self.err_len.load(Ordering::Acquire) as usize).min(ERR_BYTES);
                let bytes: Vec<u8> = self.err[..n].iter().map(|b| b.load(Ordering::Relaxed)).collect();
                Some(MorlocError::Other(format!(
                    "writing the stream failed: {}",
                    String::from_utf8_lossy(&bytes),
                )))
            }
        }
    }

    fn closed_error() -> MorlocError {
        MorlocError::Other("the stream was closed".into())
    }

    pub(crate) fn is_closed(&self) -> bool {
        self.closed.load(Ordering::Acquire) != 0
    }

    // SLOT-12: a producer holds the slot lock; it waits only on the custodian.
    pub(crate) fn push(&self, item: Item) -> Result<u32, MorlocError> {
        self.push_with(item, true)
    }

    /// Queue the close: once queued, nothing more is.
    pub(crate) fn push_close(&self, status: u32) -> Result<u32, MorlocError> {
        self.push_with(Item::marker(ENTRY_CLOSE, status), false)
    }

    /// Wait until the queue has room; a buffer the custodian finished is
    /// then among the spares.
    pub(crate) fn wait_room(&self) -> Result<(), MorlocError> {
        loop {
            if let Some(e) = self.failure() {
                return Err(e);
            }
            if self.is_closed() {
                return Err(Self::closed_error());
            }
            let d = self.done.load(Ordering::Acquire);
            if self.tail.load(Ordering::Relaxed).wrapping_sub(d) < self.depth() {
                return Ok(());
            }
            if !self.host_alive() {
                return Err(Self::gone());
            }
            wait_word::wait(&self.done, d, WAIT_SLICE);
        }
    }

    fn push_with(&self, item: Item, refuse_failed: bool) -> Result<u32, MorlocError> {
        loop {
            if refuse_failed {
                if let Some(e) = self.failure() {
                    return Err(e);
                }
            }
            if self.is_closed() {
                return Err(Self::closed_error());
            }
            let t = self.tail.load(Ordering::Relaxed);
            let d = self.done.load(Ordering::Acquire);
            if t.wrapping_sub(d) < self.depth() {
                let e = &self.entries[t as usize % QUEUE_SLOTS];
                e.kind.store(item.kind, Ordering::Relaxed);
                e.status.store(item.status, Ordering::Relaxed);
                e.block.store(item.block, Ordering::Relaxed);
                e.elems.store(item.elems, Ordering::Relaxed);
                e.index_cap.store(item.index_cap, Ordering::Relaxed);
                e.data_used.store(item.data_used, Ordering::Relaxed);
                e.len.store(item.len, Ordering::Relaxed);
                e.oversize.store(item.oversize as u32, Ordering::Relaxed);
                e.level.store(item.level as u32, Ordering::Relaxed);
                if item.kind == ENTRY_CLOSE {
                    // SLOT-13: the closer is the process that queued the close.
                    let pid = std::process::id();
                    self.closer_start.store(morloc_runtime_types::process::start_time(pid), Ordering::Relaxed);
                    self.closer_pid.store(pid, Ordering::Relaxed);
                    self.closed.store(1, Ordering::Release);
                }
                self.tail.store(t.wrapping_add(1), Ordering::Release);
                wait_word::wake_all(&self.tail);
                return Ok(t.wrapping_add(1));
            }
            if !self.host_alive() {
                return Err(Self::gone());
            }
            wait_word::wait(&self.done, d, WAIT_SLICE);
        }
    }

    pub(crate) fn pushed(&self) -> u32 {
        self.tail.load(Ordering::Acquire)
    }

    pub(crate) fn outstanding(&self) -> u32 {
        self.tail.load(Ordering::Acquire).wrapping_sub(self.done.load(Ordering::Acquire))
    }

    /// Wait until entry `seq`, pushed while the queue served opening
    /// `epoch`, is finished, and report the stream's failure if any.
    pub(crate) fn wait_done(&self, seq: u32, epoch: u32) -> Result<(), MorlocError> {
        let spin_until = std::time::Instant::now() + SPIN;
        loop {
            let d = self.done.load(Ordering::Acquire);
            if (d.wrapping_sub(seq) as i32) >= 0 {
                break;
            }
            if std::time::Instant::now() < spin_until {
                std::hint::spin_loop();
                continue;
            }
            if !self.host_alive() {
                return Err(Self::gone());
            }
            wait_word::wait(&self.done, d, WAIT_SLICE);
        }
        let failure = self.failure();
        if self.epoch() != epoch {
            // The slot was reopened since: this entry's outcome is gone.
            return Err(Self::closed_error());
        }
        match failure {
            Some(e) => Err(e),
            None => Ok(()),
        }
    }

    /// Mark the writer gone for good: pushes and waits fail from now on.
    pub(crate) fn stop(&self) {
        self.stopped.store(1, Ordering::Release);
        wait_word::wake_all(&self.done);
    }

    pub(crate) fn take_spare(&self) -> Option<RelPtr> {
        let sh = self.spare_head.load(Ordering::Relaxed);
        let st = self.spare_tail.load(Ordering::Acquire);
        if sh == st {
            return None;
        }
        let r = self.spares[sh as usize % QUEUE_SLOTS].load(Ordering::Relaxed);
        self.spare_head.store(sh.wrapping_add(1), Ordering::Release);
        Some(r)
    }

    pub(crate) fn pop(&self) -> Option<Item> {
        let h = self.head.load(Ordering::Relaxed);
        let t = self.tail.load(Ordering::Acquire);
        if h == t {
            return None;
        }
        let e = &self.entries[h as usize % QUEUE_SLOTS];
        let item = Item {
            kind: e.kind.load(Ordering::Relaxed),
            status: e.status.load(Ordering::Relaxed),
            block: e.block.load(Ordering::Relaxed),
            elems: e.elems.load(Ordering::Relaxed),
            index_cap: e.index_cap.load(Ordering::Relaxed),
            data_used: e.data_used.load(Ordering::Relaxed),
            len: e.len.load(Ordering::Relaxed),
            oversize: e.oversize.load(Ordering::Relaxed) != 0,
            level: e.level.load(Ordering::Relaxed) as u8,
        };
        self.head.store(h.wrapping_add(1), Ordering::Release);
        wait_word::wake_all(&self.head);
        Some(item)
    }

    pub(crate) fn wait_pushed(&self, timeout: Duration) {
        let h = self.head.load(Ordering::Relaxed);
        let spin_until = std::time::Instant::now() + SPIN;
        while std::time::Instant::now() < spin_until {
            if self.tail.load(Ordering::Acquire) != h {
                return;
            }
            std::hint::spin_loop();
        }
        wait_word::wait(&self.tail, h, timeout);
    }

    pub(crate) fn finish(&self, n: u32) {
        self.done.fetch_add(n, Ordering::AcqRel);
        wait_word::wake_all(&self.done);
    }

    pub(crate) fn give_spare(&self, block: RelPtr) -> bool {
        let st = self.spare_tail.load(Ordering::Relaxed);
        let sh = self.spare_head.load(Ordering::Acquire);
        if st.wrapping_sub(sh) as usize >= QUEUE_SLOTS {
            return false;
        }
        self.spares[st as usize % QUEUE_SLOTS].store(block, Ordering::Relaxed);
        self.spare_tail.store(st.wrapping_add(1), Ordering::Release);
        true
    }

    pub(crate) fn fail(&self, kind: u32, msg: &str) {
        if self.failed.load(Ordering::Acquire) != 0 {
            return;
        }
        let b = msg.as_bytes();
        let n = b.len().min(ERR_BYTES);
        for (dst, src) in self.err.iter().zip(&b[..n]) {
            dst.store(*src, Ordering::Relaxed);
        }
        self.err_len.store(n as u32, Ordering::Release);
        self.failed.store(kind, Ordering::Release);
        wait_word::wake_all(&self.head);
        wait_word::wake_all(&self.done);
    }

    pub(crate) fn drain_spares(&self) -> Vec<RelPtr> {
        let mut out = Vec::new();
        while let Some(r) = self.take_spare() {
            out.push(r);
        }
        out
    }
}

pub(crate) fn new_queue() -> Result<RelPtr, MorlocError> {
    let abs = crate::shm::shcalloc(1, std::mem::size_of::<CustodyQueue>())?;
    let q = unsafe { &*(abs as *const CustodyQueue) };
    q.reset_for_open(crate::write_behind::depth().max(1));
    crate::shm::abs2rel(abs)
}

pub(crate) fn queue_at(rel: RelPtr) -> Result<&'static CustodyQueue, MorlocError> {
    if rel == RELNULL {
        return Err(MorlocError::Other("the stream has no custody queue".into()));
    }
    let abs = crate::shm::rel2abs(rel)?;
    // SAFETY: SLOT-10: a slot index keeps its queue block for the registry's life.
    Ok(unsafe { &*(abs as *const CustodyQueue) })
}

/// The nexus's stdout and stderr writer: given the stream's handle and a
/// block of `len` bytes (a sub-packet or a footer) at `rel`, it returns
/// `SINK_WRITTEN`, `SINK_PIPE_CLOSED`, or `SINK_FAILED` with a malloc'd
/// message in `*errmsg`.
pub type StdioSink = unsafe extern "C" fn(handle: i64, rel: i64, len: u64, errmsg: *mut *mut libc::c_char) -> i32;

pub const SINK_WRITTEN: i32 = 0;
pub const SINK_FAILED: i32 = 1;
pub const SINK_PIPE_CLOSED: i32 = 2;

static STDIO_SINK: morloc_runtime_types::publish_once::PublishOnce<StdioSink> =
    morloc_runtime_types::publish_once::PublishOnce::new();

pub fn set_stdio_sink(sink: StdioSink) {
    STDIO_SINK.get_or_init(|| sink);
}

pub(crate) enum Out {
    File(i32),
    Stdio,
}

pub(crate) const STOP_NONE: u32 = u32::MAX;

pub(crate) struct Writer {
    q: &'static CustodyQueue,
    slot: &'static crate::stream::RegistrySlot,
    slot_idx: usize,
    gen: u64,
    out: Out,
    value_schema: morloc_runtime_types::schema::Schema,
    elem_schema: morloc_runtime_types::schema::Schema,
    cursor: u64,
    entries: Vec<morloc_runtime_types::packet::SubpacketEntry>,
    diag: morloc_runtime_types::packet::StreamDiag,
    inflight: std::collections::VecDeque<(crate::write_behind::Ticket, Item)>,
    failed: bool,
}

// SAFETY: SLOT-10: the writer alone uses the blocks it references while it runs.
unsafe impl Send for Writer {}

pub(crate) struct WriterInit {
    pub q: &'static CustodyQueue,
    pub slot: &'static crate::stream::RegistrySlot,
    pub slot_idx: usize,
    pub gen: u64,
    pub out: Out,
    pub value_schema: morloc_runtime_types::schema::Schema,
    pub elem_schema: morloc_runtime_types::schema::Schema,
    pub cursor: u64,
    pub entries: Vec<morloc_runtime_types::packet::SubpacketEntry>,
    pub diag: morloc_runtime_types::packet::StreamDiag,
}

impl Writer {
    pub(crate) fn new(i: WriterInit) -> Writer {
        Writer {
            q: i.q, slot: i.slot, slot_idx: i.slot_idx, gen: i.gen, out: i.out,
            value_schema: i.value_schema, elem_schema: i.elem_schema,
            cursor: i.cursor, entries: i.entries, diag: i.diag,
            inflight: Default::default(),
            failed: false,
        }
    }

    fn handle(&self) -> i64 {
        crate::stream::pack_handle(self.gen, self.slot_idx)
    }

    /// Serve the queue until a close is written, until `stop` holds a
    /// footer status (then finish with it, leaving what is still queued), or
    /// until the slot is released without a close.
    pub(crate) fn run(mut self, stop: &AtomicU32) {
        let q = self.q;
        self.serve_queue(stop);
        // SLOT-10: from here this writer touches the queue no more.
        q.consumer.store(0, Ordering::Release);
    }

    fn serve_queue(&mut self, stop: &AtomicU32) {
        loop {
            let status = stop.load(Ordering::Acquire);
            if status != STOP_NONE {
                self.commit_all();
                self.close(status);
                self.q.stop();
                return;
            }
            if !crate::stream::slot_generation_is_pub(self.slot, self.gen) {
                self.commit_all();
                self.close(STATUS_DISCARD);
                self.q.stop();
                return;
            }
            match self.q.pop() {
                Some(item) => {
                    if self.serve(item) {
                        self.await_release(stop);
                        return;
                    }
                }
                None if !self.inflight.is_empty() => self.commit_front(),
                None => self.q.wait_pushed(WAIT_SLICE),
            }
        }
    }

    // SLOT-10
    fn serve(&mut self, item: Item) -> bool {
        match item.kind {
            ENTRY_BUFFER | ENTRY_BLOCK => {
                if self.failed {
                    self.release(&item);
                    self.q.finish(1);
                } else if item.level == 0 {
                    self.commit_now(item);
                } else if self.payload_len(&item) > morloc_runtime_types::compression::FRAME_CHUNK_SIZE {
                    self.commit_all();
                    self.commit_framed(item);
                } else {
                    let ticket = crate::compression::CompressionLevel::from_u8(item.level)
                        .and_then(|level| {
                            let base = self.block_of(&item)?;
                            Ok(if item.kind == ENTRY_BUFFER {
                                crate::write_behind::compress_buffer(
                                    base, item.elems, item.index_cap, item.data_used, &self.elem_schema, level,
                                )
                            } else {
                                crate::write_behind::compress_slice(base, item.len as usize, level)
                            })
                        });
                    match ticket {
                        Ok(t) => {
                            self.inflight.push_back((t, item));
                            while self.inflight.len() >= self.q.depth() as usize {
                                self.commit_front();
                            }
                        }
                        Err(e) => {
                            self.fail(FAILED_IO, &e.to_string());
                            self.release(&item);
                            self.q.finish(1);
                        }
                    }
                }
                false
            }
            ENTRY_FLUSH => {
                self.commit_all();
                self.q.finish(1);
                false
            }
            ENTRY_CLOSE => {
                self.commit_all();
                self.close(item.status);
                self.q.finish(1);
                true
            }
            _ => {
                self.fail(FAILED_IO, &format!("unknown queue entry {}", item.kind));
                self.q.finish(1);
                false
            }
        }
    }

    // SLOT-13: a closer that dies before releasing the slot leaves it here.
    fn await_release(&self, stop: &AtomicU32) {
        loop {
            if !crate::stream::slot_generation_is_pub(self.slot, self.gen) || stop.load(Ordering::Acquire) != STOP_NONE {
                return;
            }
            let (pid, start) = self.q.closer();
            if pid != 0 && !morloc_runtime_types::process::alive(pid, start) {
                crate::stream::release_closed_slot(self.slot_idx, self.gen);
                return;
            }
            std::thread::sleep(Duration::from_millis(20));
        }
    }

    fn block_of(&self, item: &Item) -> Result<crate::shm::AbsPtr, MorlocError> {
        crate::shm::rel2abs(item.block)
    }

    fn commit_all(&mut self) {
        while !self.inflight.is_empty() {
            self.commit_front();
        }
    }

    fn commit_front(&mut self) {
        let Some((ticket, item)) = self.inflight.pop_front() else { return };
        if self.failed {
            ticket.settle();
        } else {
            match ticket.wait() {
                Ok(c) => {
                    let prepared = crate::stream_format::PreparedPayload {
                        bytes: std::borrow::Cow::Owned(c.bytes),
                        frames: Some(c.frames),
                        uncompressed_len: c.uncompressed_len,
                    };
                    self.emit(prepared, &item);
                }
                Err(e) => self.fail(FAILED_IO, &e.to_string()),
            }
        }
        self.release(&item);
        self.q.finish(1);
    }

    fn payload_len(&self, item: &Item) -> usize {
        if item.kind == ENTRY_BUFFER {
            16 + item.elems as usize * self.elem_schema.width + item.data_used as usize
        } else {
            item.len as usize
        }
    }

    /// Compress a payload too large for one frame in parallel frames, here.
    fn commit_framed(&mut self, item: Item) {
        let res = self.compact(&item).and_then(|bytes| {
            let level = crate::compression::CompressionLevel::from_u8(item.level)?;
            let (out, frames) = crate::compression::compress_payload_zstd(bytes, level)?;
            Ok(crate::stream_format::PreparedPayload {
                bytes: std::borrow::Cow::Owned(out),
                frames: Some(frames),
                uncompressed_len: bytes.len(),
            })
        });
        match res {
            Ok(prepared) => self.emit(prepared, &item),
            Err(e) => self.fail(FAILED_IO, &e.to_string()),
        }
        self.release(&item);
        self.q.finish(1);
    }

    fn compact(&self, item: &Item) -> Result<&'static [u8], MorlocError> {
        let base = self.block_of(item)?;
        let len = if item.kind == ENTRY_BUFFER {
            crate::write_behind::compact_sealed_buffer(
                base, item.elems, item.index_cap, item.data_used, &self.elem_schema,
            )?
        } else {
            item.len as usize
        };
        // SAFETY: SLOT-10: the block holds `len` payload bytes and is the
        // writer's until it is released.
        Ok(unsafe { std::slice::from_raw_parts(base as *const u8, len) })
    }

    fn commit_now(&mut self, item: Item) {
        match self.compact(&item) {
            Ok(bytes) => {
                crate::stream_format::debug_assert_payload_elem_count(item.elems, bytes, "custody");
                let prepared = crate::stream_format::PreparedPayload {
                    bytes: std::borrow::Cow::Borrowed(bytes),
                    frames: None,
                    uncompressed_len: bytes.len(),
                };
                self.emit(prepared, &item);
            }
            Err(e) => self.fail(FAILED_IO, &e.to_string()),
        }
        self.release(&item);
        self.q.finish(1);
    }

    fn emit(&mut self, prepared: crate::stream_format::PreparedPayload<'_>, item: &Item) {
        let sub = match crate::stream_format::build_subpacket_bytes(&self.value_schema, prepared) {
            Ok(s) => s,
            Err(e) => return self.fail(FAILED_IO, &e.to_string()),
        };
        let cursor = self.cursor;
        let written = match self.out {
            Out::File(fd) => write_subpacket_to_file(fd, &sub, cursor),
            Out::Stdio => self.send_stdio(&[&sub.head, &sub.payload, &[0u8; 8][..sub.pad]]),
        };
        if let Err((kind, msg)) = written {
            return self.fail(kind, &msg);
        }
        self.cursor = cursor + sub.len() as u64;
        self.diag.element_count += item.elems;
        if item.oversize {
            self.diag.n_oversize_subpackets += 1;
        }
        // SAFETY: SLOT-10: `diag` is the writer's own.
        unsafe {
            crate::stream_format::record_subpacket_flush(
                &mut self.diag,
                sub.uncompressed_len as u64,
                sub.compressed_payload_len as u64,
                Some(cursor),
            );
        }
        self.entries.push(morloc_runtime_types::packet::SubpacketEntry { offset: cursor, elem_count: item.elems });
        if let Out::File(fd) = self.out {
            let footer = morloc_runtime_types::packet::make_temp_footer_packet(&self.diag);
            if let Err(e) = crate::stream_format::pwrite_all_fd(fd, &footer, self.cursor) {
                self.fail(FAILED_IO, &e.to_string());
            }
        }
    }

    fn send_stdio(&self, parts: &[&[u8]]) -> Result<(), (u32, String)> {
        let Some(sink) = STDIO_SINK.get() else {
            return Err((FAILED_IO, "no stdout writer in this process".into()));
        };
        let len: usize = parts.iter().map(|p| p.len()).sum();
        let abs = crate::shm::shmalloc(len.max(1)).map_err(|e| (FAILED_IO, e.to_string()))?;
        let mut at = 0;
        for p in parts {
            // SAFETY: SLOT-16: the block holds `len` bytes.
            unsafe { std::ptr::copy_nonoverlapping(p.as_ptr(), (abs as *mut u8).add(at), p.len()) };
            at += p.len();
        }
        let mut err: *mut libc::c_char = std::ptr::null_mut();
        let res = crate::shm::abs2rel(abs)
            .map_err(|e| (FAILED_IO, e.to_string()))
            // SAFETY: SLOT-16: the sink reads `len` bytes at `rel` before returning.
            .map(|rel| unsafe { sink(self.handle(), rel as i64, len as u64, &mut err) });
        let _ = crate::shm::shfree(abs);
        let msg = if err.is_null() {
            String::from("the stdout writer failed")
        } else {
            // SAFETY: SLOT-16: the sink returns a malloc'd NUL-terminated message.
            let m = unsafe { std::ffi::CStr::from_ptr(err) }.to_string_lossy().into_owned();
            unsafe { libc::free(err as *mut libc::c_void) };
            m
        };
        match res? {
            SINK_WRITTEN => Ok(()),
            SINK_PIPE_CLOSED => Err((FAILED_PIPE_CLOSED, "the reader of this stream has closed it".into())),
            _ => Err((FAILED_IO, msg)),
        }
    }

    // SLOT-14: a failed or discarded stream keeps its temporary footer.
    fn close(&mut self, status: u32) {
        if !self.failed && status != STATUS_DISCARD {
            let footer = morloc_runtime_types::packet::make_final_footer_packet(&self.diag, &self.entries, status as u8);
            let res = match self.out {
                Out::File(fd) => crate::stream_format::pwrite_all_fd(fd, &footer, self.cursor)
                    .map_err(|e| (FAILED_IO, e.to_string()))
                    .and_then(|_| {
                        // SAFETY: SLOT-10: fd is the writer's open descriptor.
                        if unsafe { crate::utility::sync_file_data(fd) } != 0 {
                            Err((FAILED_IO, std::io::Error::last_os_error().to_string()))
                        } else {
                            Ok(())
                        }
                    }),
                Out::Stdio => self.send_stdio(&[&footer]),
            };
            if let Err((kind, msg)) = res {
                self.fail(kind, &msg);
            }
        }
        if let Out::File(fd) = self.out {
            if fd >= 0 {
                // FORK-3: unlock first; a copy of the descriptor may outlive this one.
                crate::stream::unlock_and_close(fd);
            }
            self.out = Out::File(-1);
        }
        while let Some(item) = self.q.pop() {
            self.release(&item);
            self.q.finish(1);
        }
    }

    fn release(&self, item: &Item) {
        if item.block == RELNULL {
            return;
        }
        if item.kind == ENTRY_BUFFER && self.q.give_spare(item.block) {
            return;
        }
        if let Ok(abs) = crate::shm::rel2abs(item.block) {
            crate::shm::free_uncounted(abs);
        }
    }

    fn fail(&mut self, kind: u32, msg: &str) {
        self.failed = true;
        self.q.fail(kind, msg);
    }
}

// SLOT-15: payload first, head last, so a reader never takes a half-written
// sub-packet for one.
fn write_subpacket_to_file(fd: i32, sub: &crate::stream_format::SubpacketBytes<'_>, cursor: u64) -> Result<(), (u32, String)> {
    let w = |bytes: &[u8], at: u64| crate::stream_format::pwrite_all_fd(fd, bytes, at).map_err(|e| (FAILED_IO, e.to_string()));
    w(&[0u8; 32], cursor)?;
    w(&sub.payload, cursor + sub.head.len() as u64)?;
    w(&[0u8; 8][..sub.pad], cursor + (sub.head.len() + sub.payload.len()) as u64)?;
    w(&sub.head, cursor)
}

struct Hosted {
    queue: &'static CustodyQueue,
    stop: std::sync::Arc<AtomicU32>,
    thread: Option<std::thread::JoinHandle<()>>,
    slot_idx: usize,
    gen: u64,
}

// SAFETY: SLOT-10: `queue` points into SHM that outlives the hosted writer.
unsafe impl Send for Hosted {}

static HOSTED: crate::fork_policy::Reset<Vec<Hosted>> = crate::fork_policy::Reset::new(Vec::new);
static HOST_GENERATION: AtomicU64 = AtomicU64::new(0);

/// Make this process the custodian of the streams it opens or adopts.
pub fn host_start() {
    HOST_GENERATION.store(crate::fork_policy::generation() + 1, Ordering::Release);
}

// FORK-14: a forked child of the custodian is not one.
pub(crate) fn is_host() -> bool {
    HOST_GENERATION.load(Ordering::Acquire) == crate::fork_policy::generation() + 1
}

pub(crate) fn host_spawn(writer: Writer) -> Result<(), MorlocError> {
    let queue = writer.q;
    let slot_idx = writer.slot_idx;
    let gen = writer.gen;
    queue.consumer.store(1, Ordering::Release);
    let stop = std::sync::Arc::new(AtomicU32::new(STOP_NONE));
    let thread_stop = stop.clone();
    let thread = std::thread::Builder::new()
        .name("morloc-custody".into())
        .spawn(move || {
            morloc_runtime_types::panic::outside_scope(|| writer.run(&thread_stop));
        })
        .map_err(|e| {
            queue.consumer.store(0, Ordering::Release);
            queue.stop();
            MorlocError::Other(format!("cannot start a stream writer: {e}"))
        })?;
    let mut hosted = HOSTED.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
    hosted.retain_mut(|h| {
        if h.thread.as_ref().is_some_and(|t| t.is_finished()) {
            if let Some(t) = h.thread.take() {
                let _ = t.join();
            }
            false
        } else {
            true
        }
    });
    hosted.push(Hosted { queue, stop, thread: Some(thread), slot_idx, gen });
    Ok(())
}

/// The slot and generation of each stream this process writes and has not
/// finished.
pub(crate) fn hosted_slots() -> Vec<(usize, u64)> {
    let hosted = HOSTED.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
    hosted.iter().filter(|h| h.thread.as_ref().is_some_and(|t| !t.is_finished())).map(|h| (h.slot_idx, h.gen)).collect()
}

/// Record this process as the custodian of `q`.
pub(crate) fn take_custody(q: &CustodyQueue) {
    let pid = std::process::id();
    q.set_host(pid, morloc_runtime_types::process::start_time(pid));
}

/// Stop every writer this process hosts, finishing each file with
/// `status` and leaving what is still queued, and wait for them.
pub fn host_stop_all(status: u8) {
    let taken = {
        let mut hosted = HOSTED.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
        std::mem::take(&mut *hosted)
    };
    stop_and_join(taken, status);
}

/// Stop the writer of the stream in `slot_idx`, as `host_stop_all` does.
pub(crate) fn host_stop(slot_idx: usize, status: u8) {
    let taken: Vec<Hosted> = {
        let mut hosted = HOSTED.lock().unwrap_or_else(|_| morloc_runtime_types::panic::poisoned_lock());
        let (stop, keep) = std::mem::take(&mut *hosted).into_iter().partition(|h| h.slot_idx == slot_idx);
        *hosted = keep;
        stop
    };
    stop_and_join(taken, status);
}

fn stop_and_join(taken: Vec<Hosted>, status: u8) {
    for h in &taken {
        h.stop.store(status as u32, Ordering::Release);
        wait_word::wake_all(&h.queue.tail);
    }
    for mut h in taken {
        if let Some(t) = h.thread.take() {
            let _ = t.join();
        }
    }
}

pub(crate) fn stop_before_unmap() {
    host_stop_all(STATUS_DISCARD as u8);
}

/// End every stream this process still writes: stdout and stderr with a
/// footer of `status`, a file the program never closed by a discard, which
/// keeps its temporary footer (SLOT-13).
pub fn host_finish_all(status: u8) {
    use crate::stream::SlotField;
    for (idx, gen) in hosted_slots() {
        let Some(slot) = crate::stream::slot_ref(idx) else { continue };
        let status = if slot.is_stdio.get() != 0 { status as u32 } else { STATUS_DISCARD };
        let _ = crate::stream::finish_stream(idx, gen, status);
    }
}

/// End the stdout and stderr streams this process still writes.
pub fn host_finish_stdio(status: u8) {
    use crate::stream::SlotField;
    for (idx, gen) in hosted_slots() {
        let Some(slot) = crate::stream::slot_ref(idx) else { continue };
        if slot.is_stdio.get() != 0 {
            let _ = crate::stream::finish_stream(idx, gen, status as u32);
        }
    }
}

/// Open a file `OStream` for writing (`mode` from `stdio_proto`): in this
/// process if it is the custodian, otherwise through the nexus.
pub(crate) fn open(mode: u8, path: &str, schema_str: &str) -> Result<i64, MorlocError> {
    let pid = std::process::id();
    let start = morloc_runtime_types::process::start_time(pid);
    let call_id = crate::stream::current_call_id_for_open();
    if is_host() {
        return crate::stream::host_open(mode, path, schema_str, pid, start, call_id);
    }
    use std::io::Write;
    crate::stream::with_stdio_sock(|s| {
        let mut req = Vec::with_capacity(32 + path.len() + schema_str.len());
        req.push(morloc_runtime_types::stdio_proto::OP_OPEN_STREAM);
        req.push(mode);
        req.extend_from_slice(&pid.to_le_bytes());
        req.extend_from_slice(&start.to_le_bytes());
        req.extend_from_slice(&call_id.to_le_bytes());
        req.extend_from_slice(&(path.len() as u32).to_le_bytes());
        req.extend_from_slice(path.as_bytes());
        req.extend_from_slice(&(schema_str.len() as u32).to_le_bytes());
        req.extend_from_slice(schema_str.as_bytes());
        s.write_all(&req).map_err(|e| MorlocError::Other(format!("@open: send: {e}")))?;
        let handle = read_reply(s, "@open")?;
        Ok(handle)
    })
}

/// Have the custodian start writing a stdout or stderr stream this process
/// published.
pub(crate) fn adopt(handle: i64) -> Result<(), MorlocError> {
    if is_host() {
        return crate::stream::host_adopt(handle);
    }
    use std::io::Write;
    crate::stream::with_stdio_sock(|s| {
        let mut req = [0u8; 9];
        req[0] = morloc_runtime_types::stdio_proto::OP_ADOPT_STREAM;
        req[1..9].copy_from_slice(&handle.to_le_bytes());
        s.write_all(&req).map_err(|e| MorlocError::Other(format!("@stdout: send: {e}")))?;
        read_reply(s, "@stdout").map(|_| ())
    })
}

fn read_reply(s: &mut std::os::unix::net::UnixStream, what: &str) -> Result<i64, MorlocError> {
    use morloc_runtime_types::stdio_proto::{STATUS_ERR, STATUS_OK};
    use std::io::Read;
    let mut status = [0u8; 1];
    s.read_exact(&mut status).map_err(|e| MorlocError::Other(format!("{what}: recv: {e}")))?;
    match status[0] {
        STATUS_OK => {
            let mut v = [0u8; 16];
            s.read_exact(&mut v).map_err(|e| MorlocError::Other(format!("{what}: recv: {e}")))?;
            Ok(i64::from_le_bytes(v[..8].try_into().unwrap()))
        }
        STATUS_ERR => Err(MorlocError::Other(crate::stream::read_error_message(s))),
        other => Err(MorlocError::Other(format!("{what}: unknown status byte {other}"))),
    }
}

mod c_abi {
    use std::ffi::c_char;

    #[no_mangle]
    pub extern "C" fn morloc_custody_host_start() {
        super::host_start()
    }

    /// # Safety
    /// `sink` is null or a function of type `StdioSink`.
    #[no_mangle]
    pub unsafe extern "C" fn morloc_custody_set_stdio_sink(sink: *const std::ffi::c_void) {
        if !sink.is_null() {
            super::set_stdio_sink(std::mem::transmute::<*const std::ffi::c_void, super::StdioSink>(sink))
        }
    }

    /// # Safety
    /// `path` and `schema` point to `path_len` and `schema_len` bytes;
    /// `errmsg` is null or writable.
    #[no_mangle]
    pub unsafe extern "C" fn morloc_custody_open(
        mode: u8,
        path: *const u8,
        path_len: usize,
        schema: *const u8,
        schema_len: usize,
        pid: u32,
        start: u64,
        call_id: u64,
        errmsg: *mut *mut c_char,
    ) -> i64 {
        let text = |p: *const u8, n: usize| -> Result<String, crate::error::MorlocError> {
            if n == 0 {
                return Ok(String::new());
            }
            String::from_utf8(std::slice::from_raw_parts(p, n).to_vec())
                .map_err(|_| crate::error::MorlocError::Other("a stream path or schema is not UTF-8".into()))
        };
        let res = text(path, path_len).and_then(|path| {
            let schema = text(schema, schema_len)?;
            crate::stream::host_open(mode, &path, &schema, pid, start, call_id)
        });
        match res {
            Ok(h) => h,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                -1
            }
        }
    }

    /// # Safety
    /// `errmsg` is null or writable.
    #[no_mangle]
    pub unsafe extern "C" fn morloc_custody_adopt(handle: i64, errmsg: *mut *mut c_char) -> i32 {
        match crate::stream::host_adopt(handle) {
            Ok(()) => 0,
            Err(e) => {
                crate::error::set_errmsg(errmsg, &e);
                -1
            }
        }
    }

    #[no_mangle]
    pub extern "C" fn morloc_custody_finish_all(status: u8) {
        super::host_finish_all(status)
    }

    #[no_mangle]
    pub extern "C" fn morloc_custody_finish_stdio(status: u8) {
        super::host_finish_stdio(status)
    }

    #[no_mangle]
    pub extern "C" fn morloc_custody_stop_all(status: u8) {
        super::host_stop_all(status)
    }
}

/// The socket of this test process's custody server, which forked test
/// children reach as pools reach the nexus.
#[cfg(test)]
pub(crate) static TEST_SOCK: std::sync::OnceLock<String> = std::sync::OnceLock::new();

/// Make this test process the custodian, serving opens from its children.
#[cfg(test)]
pub(crate) fn test_host() {
    host_start();
    TEST_SOCK.get_or_init(|| {
        let path = std::env::temp_dir().join(format!("morloc-test-custody-{}.sock", std::process::id()));
        let _ = std::fs::remove_file(&path);
        let listener = std::os::unix::net::UnixListener::bind(&path).expect("test custody socket");
        std::thread::spawn(move || {
            for conn in listener.incoming().flatten() {
                std::thread::spawn(move || serve_test(conn));
            }
        });
        path.to_str().expect("UTF-8 temp dir").to_string()
    });
}

#[cfg(test)]
fn serve_test(mut s: std::os::unix::net::UnixStream) {
    use morloc_runtime_types::stdio_proto::{OP_ADOPT_STREAM, OP_OPEN_STREAM, STATUS_ERR, STATUS_OK};
    use std::io::{Read, Write};
    fn sized(s: &mut std::os::unix::net::UnixStream) -> std::io::Result<String> {
        let mut n = [0u8; 4];
        s.read_exact(&mut n)?;
        let mut v = vec![0u8; u32::from_le_bytes(n) as usize];
        s.read_exact(&mut v)?;
        Ok(String::from_utf8_lossy(&v).into_owned())
    }
    loop {
        let mut op = [0u8; 1];
        if s.read_exact(&mut op).is_err() {
            return;
        }
        let res: Result<i64, MorlocError> = match op[0] {
            OP_OPEN_STREAM => {
                let mut head = [0u8; 21];
                if s.read_exact(&mut head).is_err() {
                    return;
                }
                let (Ok(path), Ok(schema)) = (sized(&mut s), sized(&mut s)) else { return };
                crate::stream::host_open(
                    head[0],
                    &path,
                    &schema,
                    u32::from_le_bytes(head[1..5].try_into().unwrap()),
                    u64::from_le_bytes(head[5..13].try_into().unwrap()),
                    u64::from_le_bytes(head[13..21].try_into().unwrap()),
                )
            }
            OP_ADOPT_STREAM => {
                let mut h = [0u8; 8];
                if s.read_exact(&mut h).is_err() {
                    return;
                }
                crate::stream::host_adopt(i64::from_le_bytes(h)).map(|_| 0)
            }
            _ => return,
        };
        let mut out = Vec::new();
        match res {
            Ok(h) => {
                out.push(STATUS_OK);
                out.extend_from_slice(&h.to_le_bytes());
                out.extend_from_slice(&0u64.to_le_bytes());
            }
            Err(e) => {
                let m = e.to_string();
                out.push(STATUS_ERR);
                out.extend_from_slice(&(m.len() as u32).to_le_bytes());
                out.extend_from_slice(m.as_bytes());
            }
        }
        if s.write_all(&out).is_err() {
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fresh() -> (crate::ArenaOwned, &'static CustodyQueue) {
        let arena = crate::own_test_registry();
        let q = queue_at(new_queue().unwrap()).unwrap();
        (arena, q)
    }

    #[test]
    fn items_come_out_in_the_order_they_went_in() {
        let (_a, q) = fresh();
        q.set_depth(4);
        for i in 0..3u64 {
            let mut it = Item::marker(ENTRY_BLOCK, 0);
            it.elems = i;
            q.push(it).unwrap();
        }
        let got: Vec<u64> = std::iter::from_fn(|| q.pop()).map(|i| i.elems).collect();
        assert_eq!(got, vec![0, 1, 2]);
    }

    #[test]
    fn a_full_queue_makes_the_producer_wait_until_the_custodian_takes_one() {
        let (_a, q) = fresh();
        q.set_depth(1);
        q.push(Item::marker(ENTRY_FLUSH, 0)).unwrap();
        let qa = q as *const CustodyQueue as usize;
        let taker = std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(200));
            let q = unsafe { &*(qa as *const CustodyQueue) };
            let item = q.pop().unwrap();
            q.finish(1);
            item
        });
        let began = std::time::Instant::now();
        q.push(Item::marker(ENTRY_CLOSE, 0)).unwrap();
        assert!(began.elapsed() >= Duration::from_millis(150));
        taker.join().unwrap();
        assert_eq!(q.pop().unwrap().kind, ENTRY_CLOSE);
    }

    #[test]
    fn a_waiter_sees_its_entry_done_and_any_failure() {
        let (_a, q) = fresh();
        let seq = q.push(Item::marker(ENTRY_FLUSH, 0)).unwrap();
        q.pop().unwrap();
        q.finish(1);
        q.wait_done(seq, q.epoch()).unwrap();
        let seq = q.push(Item::marker(ENTRY_FLUSH, 0)).unwrap();
        q.pop().unwrap();
        q.fail(FAILED_IO, "disk full");
        q.finish(1);
        let e = q.wait_done(seq, q.epoch()).unwrap_err();
        assert!(format!("{e:?}").contains("disk full"));
        assert!(q.push(Item::marker(ENTRY_FLUSH, 0)).is_err());
    }

    #[test]
    fn a_waiter_whose_slot_was_reopened_reports_no_success() {
        let (_a, q) = fresh();
        let seq = q.push(Item::marker(ENTRY_FLUSH, 0)).unwrap();
        let epoch = q.epoch();
        q.pop().unwrap();
        q.fail(FAILED_IO, "lost");
        q.finish(1);
        q.reset_for_open(3);
        assert!(q.wait_done(seq, epoch).is_err());
    }

    #[test]
    fn a_wait_on_a_custodian_that_died_fails() {
        let (_a, q) = fresh();
        let child = unsafe { libc::fork() };
        if child == 0 {
            unsafe { libc::_exit(0) };
        }
        let mut st = 0;
        unsafe { libc::waitpid(child, &mut st, 0) };
        q.set_host(child as u32, 0);
        let seq = q.push(Item::marker(ENTRY_FLUSH, 0)).unwrap();
        assert!(q.wait_done(seq, q.epoch()).is_err());
    }

    #[test]
    fn spares_return_in_order_and_the_ring_refuses_when_full() {
        let (_a, q) = fresh();
        for i in 0..QUEUE_SLOTS {
            assert!(q.give_spare((i + 1) as RelPtr));
        }
        assert!(!q.give_spare(99));
        assert_eq!(q.take_spare(), Some(1));
        assert_eq!(q.drain_spares().len(), QUEUE_SLOTS - 1);
        assert_eq!(q.take_spare(), None);
    }
}
