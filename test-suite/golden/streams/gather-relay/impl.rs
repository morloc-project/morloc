pub fn produce<F: Fn(&Vec<i64>)>(sink: F) {
    sink(&vec![1, 2]);
    sink(&vec![3]);
}
