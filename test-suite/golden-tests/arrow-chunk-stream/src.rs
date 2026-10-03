use rustmorloc::arrow_array::RecordBatch;

pub fn n_rows(t: &RecordBatch) -> i64 {
    t.num_rows() as i64
}

extern "C" {
    fn total_shm_size() -> usize;
}

/// The most shared memory this run has needed at once, seen from the pool
/// that drove the loop. The argument is the run's result: it is what makes
/// the measurement happen after the run.
pub fn high_water_mib(done: i64) -> i64 {
    let _ = done;
    (unsafe { total_shm_size() } / (1024 * 1024)) as i64
}
