//! Compression of sealed OStream sub-packets on background threads.
//!
//! When a compressed stream's write buffer fills, the writer seals it: the
//! buffer is detached from the slot, a fresh one takes its place, and the
//! sealed buffer is queued here. A service thread compacts the buffer and
//! compresses it into one zstd frame. The writer commits a batch -- writes it
//! and does the slot's bookkeeping -- only on its own thread, in seal order,
//! at fixed points: when a seal leaves more than `depth` batches queued, and
//! at every flush, close and dispatch end. Which call commits a batch, and so
//! which call reports its I/O error, depends only on the data written.
//!
//! Service threads touch nothing but the job they run: never the registry
//! slot, the process-local slot map, or the SHM allocator.

use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};

use morloc_runtime_types::compression::{CompressionLevel, FrameCompressor};
use morloc_runtime_types::packet::FrameEntry;
use morloc_runtime_types::schema::Schema;

use crate::error::MorlocError;
use crate::shm::AbsPtr;

/// Sealed batches a stream may have queued before a seal waits for the
/// oldest one. Each is at most one 16 MiB frame.
const DEFAULT_DEPTH: usize = 8;

/// `MORLOC_WRITE_BEHIND_DEPTH`, or the default. 0 compresses every
/// sub-packet on the writing thread.
pub(crate) fn depth() -> usize {
    std::env::var("MORLOC_WRITE_BEHIND_DEPTH")
        .ok()
        .and_then(|s| s.parse::<usize>().ok())
        .unwrap_or(DEFAULT_DEPTH)
}

/// What a job compresses.
pub(crate) enum Input {
    /// A sealed write buffer: `[Array hdr 16][cap * w index][data_used]`
    /// holding `n` elements. The job compacts it in place first.
    Buffer {
        base: usize,
        n: u64,
        cap: u64,
        data_used: u64,
        elem: Schema,
    },
    /// A complete payload.
    Owned(Vec<u8>),
}

/// A compressed payload: one zstd frame and its index entry.
pub(crate) struct Compressed {
    pub bytes: Vec<u8>,
    pub frames: Vec<FrameEntry>,
    pub uncompressed_len: usize,
}

struct Job {
    input: Mutex<Option<Input>>,
    level: CompressionLevel,
    result: Mutex<Option<Result<Compressed, MorlocError>>>,
    done: Condvar,
}

impl Job {
    fn wait(&self) -> Result<Compressed, MorlocError> {
        let mut r = self.result.lock().unwrap();
        loop {
            if let Some(res) = r.take() {
                return res;
            }
            r = self.done.wait(r).unwrap();
        }
    }

    /// Block until the job has run, discarding its result.
    fn settle(&self) {
        let mut r = self.result.lock().unwrap();
        while r.is_none() {
            r = self.done.wait(r).unwrap();
        }
    }
}

struct Service {
    queue: Mutex<ServiceQueue>,
    ready: Condvar,
}

struct ServiceQueue {
    jobs: VecDeque<Arc<Job>>,
    threads: usize,
    idle: usize,
}

/// The service of the current process. A forked child starts its own: the
/// parent's threads do not exist in it.
static SERVICE: Mutex<Option<(u32, Arc<Service>)>> = Mutex::new(None);

fn service() -> Arc<Service> {
    let pid = std::process::id();
    let mut g = SERVICE.lock().unwrap();
    match g.as_ref() {
        Some((p, s)) if *p == pid => s.clone(),
        _ => {
            let s = Arc::new(Service {
                queue: Mutex::new(ServiceQueue { jobs: VecDeque::new(), threads: 0, idle: 0 }),
                ready: Condvar::new(),
            });
            if let Some(old) = g.take() {
                // The parent's service belongs to threads this process
                // does not have; its locks may be held forever.
                std::mem::forget(old);
            }
            *g = Some((pid, s.clone()));
            s
        }
    }
}

fn submit(input: Input, level: CompressionLevel) -> Arc<Job> {
    let job = Arc::new(Job {
        input: Mutex::new(Some(input)),
        level,
        result: Mutex::new(None),
        done: Condvar::new(),
    });
    let svc = service();
    let mut q = svc.queue.lock().unwrap();
    q.jobs.push_back(job.clone());
    if q.idle == 0 && q.threads < morloc_runtime_types::compression::frame_workers() {
        q.threads += 1;
        let s = svc.clone();
        std::thread::Builder::new()
            .name("morloc-compress".into())
            .spawn(move || worker(s))
            .expect("morloc-compress: thread spawn failed");
    }
    drop(q);
    svc.ready.notify_one();
    job
}

fn worker(svc: Arc<Service>) {
    let mut compressors: Vec<(u8, FrameCompressor)> = Vec::new();
    loop {
        let job = {
            let mut q = svc.queue.lock().unwrap();
            loop {
                if let Some(j) = q.jobs.pop_front() {
                    break j;
                }
                q.idle += 1;
                q = svc.ready.wait(q).unwrap();
                q.idle -= 1;
            }
        };
        let input = job.input.lock().unwrap().take();
        let res = match input {
            Some(input) => std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run(&mut compressors, job.level, input)
            }))
            .unwrap_or_else(|_| Err(MorlocError::Other("stream compression panicked".into()))),
            None => Err(MorlocError::Other("stream compression job ran twice".into())),
        };
        *job.result.lock().unwrap() = Some(res);
        job.done.notify_all();
    }
}

fn run(
    compressors: &mut Vec<(u8, FrameCompressor)>,
    level: CompressionLevel,
    input: Input,
) -> Result<Compressed, MorlocError> {
    let c = match compressors.iter().position(|(l, _)| *l == level.raw()) {
        Some(i) => i,
        None => {
            compressors.push((level.raw(), FrameCompressor::new(level)?));
            compressors.len() - 1
        }
    };
    let payload: &[u8];
    let owned;
    match input {
        Input::Buffer { base, n, cap, data_used, elem } => {
            let len = compact_sealed_buffer(base as AbsPtr, n, cap, data_used, &elem)?;
            // SAFETY: the sealed buffer is owned by this job until it
            // completes and holds `len` bytes after compaction.
            payload = unsafe { std::slice::from_raw_parts(base as *const u8, len) };
        }
        Input::Owned(v) => {
            owned = v;
            payload = &owned;
        }
    }
    let (bytes, frames) = compressors[c].1.compress(payload)?;
    Ok(Compressed { bytes, frames, uncompressed_len: payload.len() })
}

/// Close the gap between the used index slots and the data region of a
/// write buffer, shifting relptrs to match, and write its Array header.
/// Returns the payload length. Pure buffer arithmetic: safe off the
/// writer's thread on a detached buffer.
pub(crate) fn compact_sealed_buffer(
    buf: AbsPtr,
    n: u64,
    cap: u64,
    data_used: u64,
    elem: &Schema,
) -> Result<usize, MorlocError> {
    let w = elem.width;
    let n = n as usize;
    let data_used = data_used as usize;
    let wasted = (cap as usize - n) * w;
    if wasted > 0 && data_used > 0 {
        let old_data_offset = 16 + cap as usize * w;
        let new_data_offset = 16 + n * w;
        unsafe {
            std::ptr::copy(buf.add(old_data_offset), buf.add(new_data_offset), data_used);
        }
        let res = crate::recur::Resolver::new(elem);
        for i in 0..n {
            let inline_off = 16 + i * w;
            unsafe {
                crate::voidstar::shift_buffer_relptrs_with(
                    buf, new_data_offset + data_used, inline_off, elem, -(wasted as isize), &res,
                )?;
            }
        }
    }
    unsafe {
        let hdr = buf as *mut morloc_runtime_types::shm_types::Array;
        (*hdr).size = n;
        (*hdr).data = 16 as morloc_runtime_types::shm_types::RelPtr;
    }
    Ok(16 + n * w + data_used)
}

/// One sealed batch awaiting commit.
pub(crate) struct Pending {
    job: Arc<Job>,
    pub elem_count: u64,
    /// The sealed write buffer, returned to the stream once the job is done.
    pub buffer: Option<AbsPtr>,
}

// SAFETY: `buffer` is an SHM block owned by this entry; no other thread
// dereferences it once the job has completed.
unsafe impl Send for Pending {}

impl Pending {
    pub(crate) fn wait(&self) -> Result<Compressed, MorlocError> {
        self.job.wait()
    }
}

/// A stream's queue of sealed batches and the spare write buffers it may
/// reuse. Lives in the stream's process-local slot, which a forked child
/// inherits: the queue and buffers stay the parent's, so in any other
/// process they are forgotten, never waited on or freed.
#[derive(Default)]
pub(crate) struct WriteBehind {
    pending: VecDeque<Pending>,
    pub spare: Vec<AbsPtr>,
    /// The process the contents belong to; 0 while empty.
    pid: u32,
}

// SAFETY: see `Pending`; spare buffers are unreferenced SHM blocks.
unsafe impl Send for WriteBehind {}

impl WriteBehind {
    /// Take ownership for this process, forgetting anything inherited
    /// from a parent across a fork.
    pub(crate) fn claim(&mut self) {
        let me = std::process::id();
        if self.pid != me {
            if self.pid != 0 {
                std::mem::forget(std::mem::take(&mut self.pending));
                self.spare.clear();
            }
            self.pid = me;
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.pending.len()
    }

    pub(crate) fn pop_front(&mut self) -> Option<Pending> {
        self.pending.pop_front()
    }

    pub(crate) fn seal_buffer(
        &mut self,
        base: AbsPtr,
        n: u64,
        cap: u64,
        data_used: u64,
        elem: &Schema,
        level: CompressionLevel,
    ) {
        self.claim();
        let job = submit(
            Input::Buffer { base: base as usize, n, cap, data_used, elem: elem.clone() },
            level,
        );
        self.pending.push_back(Pending { job, elem_count: n, buffer: Some(base) });
    }

    pub(crate) fn seal_owned(&mut self, payload: Vec<u8>, elem_count: u64, level: CompressionLevel) {
        self.claim();
        let job = submit(Input::Owned(payload), level);
        self.pending.push_back(Pending { job, elem_count, buffer: None });
    }

    /// A spare write buffer, if one is free.
    pub(crate) fn take_spare(&mut self) -> Option<AbsPtr> {
        self.claim();
        self.spare.pop()
    }

    /// Return a committed batch's buffer for reuse, keeping at most `keep`.
    pub(crate) fn recycle(&mut self, buffer: Option<AbsPtr>, keep: usize) {
        if let Some(b) = buffer {
            if self.spare.len() < keep {
                self.spare.push(b);
            } else {
                let _ = crate::shm::shfree(b);
            }
        }
    }

    /// Drop every queued batch unwritten, once its job has finished with its
    /// buffer.
    pub(crate) fn abandon(&mut self) {
        self.claim();
        for p in self.pending.drain(..) {
            p.job.settle();
            if let Some(b) = p.buffer {
                let _ = crate::shm::shfree(b);
            }
        }
    }
}

impl Drop for WriteBehind {
    fn drop(&mut self) {
        self.claim();
        self.abandon();
        for b in self.spare.drain(..) {
            let _ = crate::shm::shfree(b);
        }
    }
}

/// Streams this process holds sealed batches of, whichever thread sealed
/// them: every point where another process may take over a stream drains
/// them all.
static SEALED: Mutex<Vec<i64>> = Mutex::new(Vec::new());

thread_local! {
    /// The subset this thread sealed: a failure to write one of these is the
    /// error of the call running on this thread.
    static THREAD_SEALED: std::cell::RefCell<Vec<i64>> = const { std::cell::RefCell::new(Vec::new()) };
}

pub(crate) fn note_sealed(handle: i64) {
    let mut all = SEALED.lock().unwrap();
    if !all.contains(&handle) {
        all.push(handle);
    }
    drop(all);
    THREAD_SEALED.with(|h| {
        let mut h = h.borrow_mut();
        if !h.contains(&handle) {
            h.push(handle);
        }
    });
}

pub(crate) fn sealed_handles() -> Vec<i64> {
    SEALED.lock().unwrap().clone()
}

pub(crate) fn forget_sealed(handle: i64) {
    SEALED.lock().unwrap().retain(|h| *h != handle);
}

pub(crate) fn take_thread_sealed() -> Vec<i64> {
    THREAD_SEALED.with(|h| std::mem::take(&mut *h.borrow_mut()))
}

pub(crate) fn thread_sealed() -> Vec<i64> {
    THREAD_SEALED.with(|h| h.borrow().clone())
}

#[cfg(test)]
mod tests {
    use super::*;

    // What a forked child inherits belongs to its parent: dropping it in the
    // child must neither wait on the parent's jobs nor free its buffers.
    #[test]
    fn a_copy_in_another_process_leaves_the_parents_buffers() {
        let _shm = crate::own_test_registry();
        let buf = crate::shm::shcalloc(1, 4096).unwrap();
        let mut wb = WriteBehind::default();
        wb.spare.push(buf);
        wb.claim();
        wb.pid = wb.pid.wrapping_add(1);
        drop(wb);
        let rc = unsafe { crate::shm::reference_count(buf) };
        assert_eq!(rc, Some(1), "a copy in another process freed the parent's buffer");
        crate::shm::shfree(buf).unwrap();
    }
}
