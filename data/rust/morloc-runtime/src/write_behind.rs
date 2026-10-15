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
    let (start, len) = match input.sealed {
        Some(Sealed { n, cap, data_used, elem }) => {
            compact_sealed_buffer(input.base as AbsPtr, n, cap, data_used, &elem)?
        }
        None => (0, input.len),
    };
    let base = input.base + start;
    let frame = subpacket_frame_size(level);
    let mut jobs = Vec::with_capacity(len.div_ceil(frame).max(1));
    let mut at = 0;
    loop {
        let n = frame.min(len - at);
        let job = FrameJob::new(FrameInput { base: base + at, len: n }, level);
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

/// # Safety
/// `base` must hold `16 + n * elem.width + data_used` bytes laid out so.
pub(crate) unsafe fn shift_records(
    base: AbsPtr,
    n: usize,
    wasted: usize,
    data_used: usize,
    elem: &Schema,
) -> Result<(), MorlocError> {
    if wasted == 0 || data_used == 0 {
        return Ok(());
    }
    let w = elem.width;
    let res = crate::recur::Resolver::new(elem);
    let shallow = crate::voidstar::shallow_array_slots(elem);
    for i in 0..n {
        crate::voidstar::shift_element_relptrs(
            base, 16 + n * w + data_used, 16 + i * w, elem, -(wasted as isize), &res, shallow.as_deref(),
        )?;
    }
    Ok(())
}

/// Close the gap between the used index slots and the data region of a
/// write buffer, shifting relptrs to match, and write its Array header.
/// Returns the payload's offset in the buffer and its length.
pub(crate) fn compact_sealed_buffer(
    buf: AbsPtr,
    n: u64,
    cap: u64,
    data_used: u64,
    elem: &Schema,
) -> Result<(usize, usize), MorlocError> {
    let w = elem.width;
    let n = n as usize;
    let data_used = data_used as usize;
    let wasted = (cap as usize - n) * w;
    let records_end = 16 + n * w;
    let mut start = 0;
    if wasted > 0 && data_used > 0 {
        if records_end < data_used {
            unsafe { std::ptr::copy(buf, buf.add(wasted), records_end) };
            start = wasted;
        } else {
            unsafe { std::ptr::copy(buf.add(16 + cap as usize * w), buf.add(records_end), data_used) };
        }
        unsafe { shift_records(buf.add(start), n, wasted, data_used, elem)? };
    }
    unsafe {
        let hdr = buf.add(start) as *mut morloc_runtime_types::shm_types::Array;
        (*hdr).size = n;
        (*hdr).data = 16 as morloc_runtime_types::shm_types::RelPtr;
    }
    Ok((start, records_end + data_used))
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

    /// A sealed buffer of `xs` with room for `cap` records, laid out as the
    /// write path lays it out.
    fn sealed(schema: &str, xs: &[String], cap: usize) -> (Vec<u64>, usize, Schema) {
        let elem = morloc_runtime_types::schema::parse_schema(schema).unwrap();
        let res = crate::recur::Resolver::new(&elem);
        let w = elem.width;
        let vs: Vec<AbsPtr> = xs.iter().map(|x| crate::json::read_json_with_schema(x, &elem).unwrap()).collect();
        let flats: Vec<usize> =
            vs.iter().map(|&v| crate::ffi::calc_voidstar_size_portable_with(v, &elem, &res).unwrap()).collect();
        let total: usize = flats.iter().map(|f| (f - w).next_multiple_of(8)).sum();
        let mut buf = vec![0xffu64; (16 + cap * w + total).div_ceil(8)];
        let base = buf.as_mut_ptr() as *mut u8;
        let mut used = 0;
        for (i, (&v, &flat)) in vs.iter().zip(&flats).enumerate() {
            let var = (flat - w).next_multiple_of(8);
            unsafe {
                crate::voidstar::flatten_portable_in_place(base, 16 + i * w, 16 + cap * w + used, w + var, v, &elem, &res)
                    .unwrap();
            }
            crate::shm::shfree(v).unwrap();
            used += var;
        }
        (buf, used, elem)
    }

    /// Compaction gives the payload bytes of a buffer that had no room to
    /// spare, whichever side it moves: the data (short records) or the
    /// header and records (long ones).
    #[test]
    fn compaction_gives_the_same_bytes_whichever_side_moves() {
        let _shm = crate::own_test_registry();
        let quote = |x: String| format!("{:?}", x);
        let short: Vec<String> = (0..300).map(|i| quote("x".repeat(i % 3))).collect();
        let long: Vec<String> = (0..5).map(|i| quote("y".repeat(1000 + i * 37))).collect();
        let nested_short: Vec<String> =
            (0..200).map(|i| format!("[{}]", vec![quote("z".repeat(i % 2)); i % 3].join(","))).collect();
        let nested_long: Vec<String> =
            (0..4).map(|i| format!("[{}]", vec![quote("w".repeat(700 + i)); 3].join(","))).collect();
        for (schema, xs) in [("s", short), ("s", long), ("as", nested_short), ("as", nested_long)] {
            let n = xs.len();
            let (mut tight, used, elem) = sealed(schema, &xs, n);
            let (s0, l0) =
                compact_sealed_buffer(tight.as_mut_ptr() as AbsPtr, n as u64, n as u64, used as u64, &elem).unwrap();
            let want = unsafe { std::slice::from_raw_parts((tight.as_ptr() as *const u8).add(s0), l0) }.to_vec();
            let (mut loose, used2, _) = sealed(schema, &xs, n * 3 + 7);
            let (s1, l1) =
                compact_sealed_buffer(loose.as_mut_ptr() as AbsPtr, n as u64, (n * 3 + 7) as u64, used2 as u64, &elem)
                    .unwrap();
            let got = unsafe { std::slice::from_raw_parts((loose.as_ptr() as *const u8).add(s1), l1) }.to_vec();
            assert_eq!(s0, 0);
            assert_eq!(s1 != 0, 16 + n * elem.width < used2, "the larger side moved");
            assert!(got == want, "{n} records: compaction changed the payload");
        }
    }

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
