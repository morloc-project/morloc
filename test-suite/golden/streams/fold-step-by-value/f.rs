use std::cell::Cell;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

const SPARE: usize = 1 << 16;

static ENTERED: AtomicUsize = AtomicUsize::new(0);
thread_local!(static WAITED: Cell<bool> = Cell::new(false));

// Each thread's first step waits until another thread's step has started.
fn meet() {
    if WAITED.with(|w| w.replace(true)) {
        return;
    }
    ENTERED.fetch_add(1, Ordering::SeqCst);
    let start = Instant::now();
    while ENTERED.load(Ordering::SeqCst) < 2 && start.elapsed() < Duration::from_secs(10) {
        std::thread::yield_now();
    }
}

pub fn produce<F: Fn(&Vec<i64>)>(n: i64, sink: F) {
    for i in 0..n {
        sink(&vec![i]);
    }
}

pub fn produce2<F: Fn(&Vec<i64>) + Sync>(n: i64, sink: F) {
    std::thread::scope(|s| {
        for t in 0..2 {
            let sink = &sink;
            s.spawn(move || {
                for i in 0..n {
                    sink(&vec![t * n + i]);
                }
            });
        }
    });
}

pub fn init_acc() -> Vec<i64> {
    vec![0]
}

pub fn step(mut acc: Vec<i64>, _xs: &Vec<i64>) -> Vec<i64> {
    meet();
    if acc.capacity() < SPARE {
        acc[0] += 1;
        acc.reserve(SPARE);
    }
    acc
}

pub fn comb(mut a: Vec<i64>, b: Vec<i64>) -> Vec<i64> {
    let fresh = (a.capacity() < SPARE) as i64 + (b.capacity() < SPARE) as i64;
    a[0] += b[0] + 100 * fresh;
    a
}

pub fn step_any<B: Clone>(acc: &B, _xs: &Vec<i64>) -> B {
    acc.clone()
}

pub fn fresh(acc: &Vec<i64>) -> i64 {
    acc[0]
}

pub fn bump(xs: &Vec<i64>) -> Vec<i64> {
    xs.iter().map(|x| x + 1).collect()
}

