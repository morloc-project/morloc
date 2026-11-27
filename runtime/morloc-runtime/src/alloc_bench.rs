//! Throughput of the SHM allocator under threads of one process. Ignored by
//! default; run in release mode:
//!
//!   cargo test --release -p morloc-runtime --lib alloc_bench -- \
//!       --ignored --nocapture --test-threads=1
//!
//! Each thread repeats a small working set of allocations of mixed sizes,
//! freeing one at random and allocating a replacement. A line reports the
//! minimum and median over three runs of the wall time per operation.
//!
//! Every bench also prints `MLCBENCH` rows to stderr in the summary format of
//! benchmark labels, so /work/tests can record them.

use crate::shm;
use crate::shm_types::AbsPtr;
use std::time::Instant;

const REPS: usize = 3;

/// One `MLCBENCH` row: per-operation seconds of each sample.
pub(crate) fn mlcbench(group: &str, name: &str, samples: &[f64]) {
    let n = samples.len() as f64;
    let total: f64 = samples.iter().sum();
    let mean = total / n;
    let min = samples.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = samples.iter().cloned().fold(0.0, f64::max);
    let var = samples.iter().map(|x| (x - mean) * (x - mean)).sum::<f64>() / n;
    eprintln!("MLCBENCH\t{group}\t{name}\trust\t{}\t{mean:.9}\t{min:.9}\t{max:.9}\t{:.9}\t{total:.9}", samples.len(), var.sqrt());
}
const OPS_PER_THREAD: usize = 200_000;
const HELD: usize = 16;

fn churn(seed: u64) {
    let mut rng = seed | 1;
    let mut held: Vec<AbsPtr> = Vec::with_capacity(HELD);
    for _ in 0..OPS_PER_THREAD {
        rng ^= rng << 13;
        rng ^= rng >> 7;
        rng ^= rng << 17;
        if held.len() == HELD {
            let p = held.swap_remove((rng as usize >> 3) % HELD);
            shm::shfree(p).unwrap();
        }
        // Mostly small values, now and then one of a few KiB.
        let n = if rng & 0xF == 0 { 4096 + (rng as usize >> 20) % 4096 } else { 16 + (rng as usize >> 20) % 240 };
        held.push(shm::shmalloc(n).unwrap());
    }
    for p in held {
        shm::shfree(p).unwrap();
    }
}

#[test]
#[ignore]
fn alloc_bench() {
    let _shm = crate::own_test_registry();
    for threads in [1usize, 2, 4, 8] {
        let mut ns: Vec<f64> = (0..REPS)
            .map(|_| {
                let t = Instant::now();
                let handles: Vec<_> = (0..threads)
                    .map(|k| std::thread::spawn(move || churn(0x9E37_79B9_7F4A_7C15 ^ k as u64)))
                    .collect();
                for h in handles {
                    h.join().unwrap();
                }
                t.elapsed().as_secs_f64() * 1e9 / (threads * OPS_PER_THREAD) as f64
            })
            .collect();
        mlcbench("alloc", &format!("churn-t{threads}"), &ns.iter().map(|x| x / 1e9).collect::<Vec<_>>());
        ns.sort_by(|a, b| a.partial_cmp(b).unwrap());
        println!(
            "BENCH alloc threads={threads:<2} min {:>8.1} ns/op   median {:>8.1} ns/op",
            ns[0],
            ns[REPS / 2]
        );
    }
}

/// The longest a small allocation waits while another thread allocates and
/// frees a large block, against the same allocation with no large block.
#[test]
#[ignore]
fn big_free_stall() {
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Arc;
    let _shm = crate::own_test_registry();
    const BIG: usize = 256 << 20;
    let probe = |stop: Arc<AtomicBool>| {
        std::thread::spawn(move || {
            let mut worst = 0f64;
            while !stop.load(Ordering::Relaxed) {
                let t = Instant::now();
                let p = shm::shmalloc(64).unwrap();
                shm::shfree(p).unwrap();
                worst = worst.max(t.elapsed().as_secs_f64());
            }
            worst
        })
    };
    let mut quiet = Vec::new();
    let mut loaded = Vec::new();
    for _ in 0..REPS {
        let stop = Arc::new(AtomicBool::new(false));
        let h = probe(stop.clone());
        std::thread::sleep(std::time::Duration::from_millis(200));
        stop.store(true, Ordering::Relaxed);
        quiet.push(h.join().unwrap());

        let big = shm::shmalloc(BIG).unwrap();
        unsafe { std::ptr::write_bytes(big, 1, BIG) };
        let stop = Arc::new(AtomicBool::new(false));
        let h = probe(stop.clone());
        std::thread::sleep(std::time::Duration::from_millis(20));
        shm::shfree(big).unwrap();
        let again = shm::shmalloc(BIG).unwrap();
        shm::shfree(again).unwrap();
        std::thread::sleep(std::time::Duration::from_millis(20));
        stop.store(true, Ordering::Relaxed);
        loaded.push(h.join().unwrap());
    }
    mlcbench("alloc", "small-worst-quiet", &quiet);
    mlcbench("alloc", "small-worst-during-256m-free", &loaded);
}
