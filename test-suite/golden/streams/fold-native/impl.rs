pub fn produce_none<F: Fn(&Vec<i64>)>(_sink: F) {}

pub fn produce_threads<F: Fn(&Vec<i64>) + Sync>(sink: F) {
    std::thread::scope(|s| {
        for t in 0..4i64 {
            let sink = &sink;
            s.spawn(move || {
                for b in 0..5i64 {
                    sink(&(0..=b).map(|k| t * 100 + k).collect());
                }
            });
        }
    });
}

pub fn add_sum(acc: i64, xs: &Vec<i64>) -> i64 {
    acc + xs.iter().sum::<i64>()
}

pub fn merge(a: i64, b: i64) -> i64 {
    a + b
}

pub fn show_n(n: i64) -> String {
    format!("rust n={}\n", n)
}
