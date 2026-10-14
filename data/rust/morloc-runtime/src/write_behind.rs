//! Compression of written-stream batches on background threads, for the
//! stream custodian (`custody`), which writes them in queue order.

use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex};

use morloc_runtime_types::compression::{subpacket_frame_size, CompressionLevel, FrameCompressor};
use morloc_runtime_types::packet::FrameEntry;
use morloc_runtime_types::schema::Schema;

use crate::error::MorlocError;
use crate::shm::AbsPtr;

/// Batches a compressed stream may have outstanding before a writer waits
/// for the oldest, by default. A batch's frames already compress in
/// parallel, so a few batches keep the threads busy; each more holds a
/// 16 MiB buffer.
fn default_depth() -> usize {
    morloc_runtime_types::compression::frame_workers().clamp(2, 4)
}

/// `MORLOC_WRITE_BEHIND_DEPTH` as the process started, or the default.
// FORK-9
pub(crate) fn depth() -> usize {
    #[cfg(test)]
    {
        let d = TEST_DEPTH.load(std::sync::atomic::Ordering::SeqCst);
        if d != usize::MAX {
            return d;
        }
    }
    static DEPTH: morloc_runtime_types::publish_once::PublishOnce<usize> =
        morloc_runtime_types::publish_once::PublishOnce::new();
    *DEPTH.get_or_init(|| {
        match std::env::var("MORLOC_WRITE_BEHIND_DEPTH").ok().and_then(|s| s.parse::<usize>().ok()) {
            Some(d) if d > crate::custody::MAX_DEPTH => {
                eprintln!(
                    "morloc: MORLOC_WRITE_BEHIND_DEPTH={d} is above the most a stream can hold ({}); using {}",
                    crate::custody::MAX_DEPTH,
                    crate::custody::MAX_DEPTH
                );
                crate::custody::MAX_DEPTH
            }
            Some(d) => d,
            None => default_depth(),
        }
    })
}

#[cfg(test)]
static TEST_DEPTH: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(usize::MAX);

/// Override the depth for a test; `None` restores it.
#[cfg(test)]
pub(crate) fn set_test_depth(depth: Option<usize>) {
    TEST_DEPTH.store(depth.unwrap_or(usize::MAX), std::sync::atomic::Ordering::SeqCst);
}

struct Sealed {
    n: u64,
    cap: u64,
    data_used: u64,
    elem: Schema,
}

struct SplitInput {
    base: usize,
    len: usize,
    sealed: Option<Sealed>,
}

struct FrameInput {
    base: usize,
    len: usize,
}

/// A compressed payload: its zstd frames and their index entries.
pub(crate) struct Compressed {
    pub bytes: Vec<u8>,
    pub frames: Vec<FrameEntry>,
    pub uncompressed_len: usize,
}

struct Job<I, R> {
    input: Mutex<Option<I>>,
    level: CompressionLevel,
    result: Mutex<Option<Result<R, MorlocError>>>,
    done: Condvar,
}

type FrameJob = Job<FrameInput, Compressed>;
type SplitJob = Job<SplitInput, Vec<Arc<FrameJob>>>;

impl<I, R> Job<I, R> {
    fn new(input: I, level: CompressionLevel) -> Arc<Self> {
        Arc::new(Job { input: Mutex::new(Some(input)), level, result: Mutex::new(None), done: Condvar::new() })
    }

    fn wait(&self) -> Result<R, MorlocError> {
        let mut r = self.result.lock().unwrap();
        loop {
            if let Some(res) = r.take() {
                return res;
            }
            r = self.done.wait(r).unwrap();
        }
    }

    fn finish(&self, res: Result<R, MorlocError>) {
        *self.result.lock().unwrap() = Some(res);
        self.done.notify_all();
    }

    /// SLOT-24: withdraw a job no worker has started, or wait for it.
    fn withdraw_or_wait(&self) {
        if self.input.lock().unwrap().take().is_some() {
            return;
        }
        let mut r = self.result.lock().unwrap();
        while r.is_none() {
            r = self.done.wait(r).unwrap();
        }
    }
}

enum Task {
    Split(Arc<SplitJob>),
    Frame(Arc<FrameJob>),
}

struct Service {
    queue: Mutex<ServiceQueue>,
    ready: Condvar,
}

struct ServiceQueue {
    jobs: VecDeque<Task>,
    threads: usize,
    idle: usize,
}

static SERVICE: crate::fork_policy::Reset<Option<Arc<Service>>> = crate::fork_policy::Reset::new(|| None);

fn service() -> Arc<Service> {
    let mut g = SERVICE.lock().unwrap();
    g.get_or_insert_with(|| {
        Arc::new(Service {
            queue: Mutex::new(ServiceQueue { jobs: VecDeque::new(), threads: 0, idle: 0 }),
            ready: Condvar::new(),
        })
    })
    .clone()
}

fn submit(task: Task) {
    let svc = service();
    let mut q = svc.queue.lock().unwrap();
    q.jobs.push_back(task);
    if q.jobs.len() > q.idle && q.threads < morloc_runtime_types::compression::frame_workers() {
        q.threads += 1;
        let s = svc.clone();
        if std::thread::Builder::new().name("morloc-compress".into()).spawn(move || worker(s)).is_err() {
            q.threads -= 1;
            if q.threads == 0 {
                // SLOT-24: with no thread to run it, the task runs here.
                let task = q.jobs.pop_back();
                drop(q);
                if let Some(task) = task {
                    run_task(&mut Vec::new(), task);
                }
                return;
            }
        }
    }
    drop(q);
    svc.ready.notify_one();
}

/// How long a compression thread keeps its zstd contexts with no work.
const IDLE_RELEASE: std::time::Duration = std::time::Duration::from_secs(1);

fn worker(svc: Arc<Service>) {
    let mut compressors: Vec<(u8, FrameCompressor)> = Vec::new();
    loop {
        let task = {
            let mut q = svc.queue.lock().unwrap();
            loop {
                if let Some(t) = q.jobs.pop_front() {
                    break t;
                }
                q.idle += 1;
                if compressors.is_empty() {
                    q = svc.ready.wait(q).unwrap();
                    q.idle -= 1;
                } else {
                    let (g, waited) = svc.ready.wait_timeout(q, IDLE_RELEASE).unwrap();
                    q = g;
                    q.idle -= 1;
                    if waited.timed_out() && q.jobs.is_empty() {
                        compressors.clear();
                    }
                }
            }
        };
        run_task(&mut compressors, task);
    }
}

/// SLOT-24: a task withdrawn before it started is skipped.
fn run_task(compressors: &mut Vec<(u8, FrameCompressor)>, task: Task) {
    match task {
        Task::Split(job) => {
            let input = job.input.lock().unwrap().take();
            if let Some(input) = input {
                job.finish(split(input, job.level));
            }
        }
        Task::Frame(job) => {
            let input = job.input.lock().unwrap().take();
            if let Some(input) = input {
                job.finish(compress_frame(compressors, job.level, input));
            }
        }
    }
}

fn split(input: SplitInput, level: CompressionLevel) -> Result<Vec<Arc<FrameJob>>, MorlocError> {
    let len = match input.sealed {
        Some(Sealed { n, cap, data_used, elem }) => {
            compact_sealed_buffer(input.base as AbsPtr, n, cap, data_used, &elem)?
        }
        None => input.len,
    };
    let frame = subpacket_frame_size(level);
    let mut jobs = Vec::with_capacity(len.div_ceil(frame).max(1));
    let mut at = 0;
    loop {
        let n = frame.min(len - at);
        let job = FrameJob::new(FrameInput { base: input.base + at, len: n }, level);
        jobs.push(job.clone());
        submit(Task::Frame(job));
        at += n;
        if at >= len {
            break;
        }
    }
    Ok(jobs)
}

fn compress_frame(
    compressors: &mut Vec<(u8, FrameCompressor)>,
    level: CompressionLevel,
    input: FrameInput,
) -> Result<Compressed, MorlocError> {
    let c = match compressors.iter().position(|(l, _)| *l == level.raw()) {
        Some(i) => i,
        None => {
            compressors.push((level.raw(), FrameCompressor::new(level)?));
            compressors.len() - 1
        }
    };
    // SAFETY: SLOT-24
    let payload = unsafe { std::slice::from_raw_parts(input.base as *const u8, input.len) };
    let (mut bytes, frames) = compressors[c].1.compress(payload)?;
    bytes.shrink_to_fit();
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
        let shallow = crate::voidstar::shallow_array_slots(elem);
        for i in 0..n {
            let inline_off = 16 + i * w;
            unsafe {
                crate::voidstar::shift_element_relptrs(
                    buf, new_data_offset + data_used, inline_off, elem, -(wasted as isize), &res,
                    shallow.as_deref(),
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

/// A batch being compressed: a split job and, once it has run, its frame
/// jobs in payload order.
pub(crate) struct Ticket(Arc<SplitJob>);

impl Ticket {
    /// The batch's frames, joined in payload order (SLOT-24).
    pub(crate) fn wait(&self) -> Result<Compressed, MorlocError> {
        let jobs = self.0.wait()?;
        let mut parts = Vec::with_capacity(jobs.len());
        let mut failure = None;
        for job in &jobs {
            match job.wait() {
                Ok(c) => parts.push(c),
                Err(e) => {
                    failure.get_or_insert(e);
                }
            }
        }
        if let Some(e) = failure {
            return Err(e);
        }
        let total = parts.iter().map(|c| c.bytes.len()).sum();
        let mut out = Compressed { bytes: Vec::with_capacity(total), frames: Vec::new(), uncompressed_len: 0 };
        for c in parts {
            out.bytes.extend_from_slice(&c.bytes);
            out.frames.extend(c.frames);
            out.uncompressed_len += c.uncompressed_len;
        }
        Ok(out)
    }

    /// SLOT-24: discard the batch.
    pub(crate) fn settle(&self) {
        self.0.withdraw_or_wait();
        let jobs = self.0.result.lock().unwrap().take();
        if let Some(Ok(jobs)) = jobs {
            for job in &jobs {
                job.withdraw_or_wait();
            }
        }
    }
}

/// Compact a sealed write buffer of `n` elements and compress it, one job
/// per frame of `subpacket_frame_size(level)` bytes.
pub(crate) fn compress_buffer(
    base: AbsPtr,
    n: u64,
    cap: u64,
    data_used: u64,
    elem: &Schema,
    level: CompressionLevel,
) -> Ticket {
    let sealed = Sealed { n, cap, data_used, elem: elem.clone() };
    let job = SplitJob::new(SplitInput { base: base as usize, len: 0, sealed: Some(sealed) }, level);
    submit(Task::Split(job.clone()));
    Ticket(job)
}

/// Compress the compact payload of `len` bytes at `base`.
pub(crate) fn compress_slice(base: AbsPtr, len: usize, level: CompressionLevel) -> Ticket {
    let job = SplitJob::new(SplitInput { base: base as usize, len, sealed: None }, level);
    submit(Task::Split(job.clone()));
    Ticket(job)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// SLOT-24: a payload's frames join in payload order and decode to the
    /// payload; settling a ticket returns once nothing reads the payload.
    #[test]
    fn frames_of_a_payload_join_in_order_and_settle_returns() {
        let level = CompressionLevel::from_u8(3).unwrap();
        let payload: Vec<u8> = (0..(5usize << 20) + 7).map(|i| (i * 31 % 253) as u8).collect();
        let c = compress_slice(payload.as_ptr() as AbsPtr, payload.len(), level).wait().unwrap();
        assert_eq!(c.frames.len(), 3);
        assert_eq!(c.uncompressed_len, payload.len());
        let mut out = vec![0u8; payload.len()];
        morloc_runtime_types::compression::parallel_decompress_frames(&c.frames, &c.bytes, &mut out).unwrap();
        assert!(out == payload);

        for _ in 0..8 {
            compress_slice(payload.as_ptr() as AbsPtr, payload.len(), level).settle();
        }
    }
}
