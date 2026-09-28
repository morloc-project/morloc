//! Throughput of the SHM allocator under threads of one process. Ignored by
//! default; run in release mode:
//!
//!   cargo test --release -p morloc-runtime --lib alloc_bench -- \
//!       --ignored --nocapture --test-threads=1
//!
//! Each thread repeats a small working set of allocations of mixed sizes,
//! freeing one at random and allocating a replacement. A line reports the
//! minimum and median over three runs of the wall time per operation.

use crate::shm;
use crate::shm_types::AbsPtr;
use std::time::Instant;

const REPS: usize = 3;
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
        ns.sort_by(|a, b| a.partial_cmp(b).unwrap());
        println!(
            "BENCH alloc threads={threads:<2} min {:>8.1} ns/op   median {:>8.1} ns/op",
            ns[0],
            ns[REPS / 2]
        );
    }
}
