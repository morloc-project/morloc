//! Compression of written-stream batches on background threads, for the
//! stream custodian (`custody`), which writes them in queue order.
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

/// Batches a compressed stream may have outstanding before a writer waits
/// for the oldest; each is at most one 16 MiB frame.
const DEFAULT_DEPTH: usize = 7;

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
        std::env::var("MORLOC_WRITE_BEHIND_DEPTH")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .unwrap_or(DEFAULT_DEPTH)
    })
}

#[cfg(test)]
static TEST_DEPTH: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(usize::MAX);

/// Override the depth for a test; `None` restores it.
#[cfg(test)]
pub(crate) fn set_test_depth(depth: Option<usize>) {
    TEST_DEPTH.store(depth.unwrap_or(usize::MAX), std::sync::atomic::Ordering::SeqCst);
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
    /// A complete payload in a block nothing else touches until the job is
    /// done.
    Slice { base: usize, len: usize },
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
            Some(input) => run(&mut compressors, job.level, input),
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
    match input {
        Input::Buffer { base, n, cap, data_used, elem } => {
            let len = compact_sealed_buffer(base as AbsPtr, n, cap, data_used, &elem)?;
            // SAFETY: the sealed buffer is owned by this job until it
            // completes and holds `len` bytes after compaction.
            payload = unsafe { std::slice::from_raw_parts(base as *const u8, len) };
        }
        Input::Slice { base, len } => {
            // SAFETY: the block is the job's until it completes.
            payload = unsafe { std::slice::from_raw_parts(base as *const u8, len) };
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

/// A compression job in flight.
pub(crate) struct Ticket(Arc<Job>);

impl Ticket {
    pub(crate) fn wait(&self) -> Result<Compressed, MorlocError> {
        self.0.wait()
    }

    pub(crate) fn settle(&self) {
        self.0.settle()
    }
}

/// Compact and compress a write buffer of `n` elements.
pub(crate) fn compress_buffer(
    base: AbsPtr,
    n: u64,
    cap: u64,
    data_used: u64,
    elem: &Schema,
    level: CompressionLevel,
) -> Ticket {
    Ticket(submit(Input::Buffer { base: base as usize, n, cap, data_used, elem: elem.clone() }, level))
}

/// Compress the compact payload of `len` bytes at `base`.
pub(crate) fn compress_slice(base: AbsPtr, len: usize, level: CompressionLevel) -> Ticket {
    Ticket(submit(Input::Slice { base: base as usize, len }, level))
}
