use rustmorloc::arrow_array::RecordBatch;

pub fn n_rows(t: &RecordBatch) -> i64 {
    t.num_rows() as i64
}
