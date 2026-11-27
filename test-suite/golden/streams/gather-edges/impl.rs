pub fn produce_none<F: Fn(&Vec<i64>)>(_sink: F) {}

pub fn produce_fail<F: Fn(&Vec<i64>)>(sink: F) {
    sink(&vec![1, 2]);
    sink(&vec![3]);
    panic!("producer failed after 2 batches");
}

pub fn produce_ten<F: Fn(&Vec<i64>)>(sink: F) {
    sink(&vec![0, 1, 2]);
    sink(&vec![]);
    sink(&(3..10).collect());
}

pub fn show_ints(xs: &Vec<i64>) -> String {
    let body: String = xs.iter().map(|x| format!(" {}", x)).collect();
    format!("rust {}:{}\n", xs.len(), body)
}

pub fn mark_ran(xs: &Vec<i64>) -> String {
    std::fs::write("handler-ran", format!("{}\n", xs.len())).unwrap();
    "handler ran\n".to_string()
}
