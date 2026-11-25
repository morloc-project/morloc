fn feed<S: Fn(&Vec<i64>) -> R, R>(k: i64, p: &String, sink: S) { sink(&vec![k, p.len() as i64]); }
fn put(xs: &Vec<i64>) { println!("{}", xs[0] + xs[1]); }
fn feed_all<S: Fn(&Vec<i64>) -> R, R>(p: &String, sink: S) { sink(&vec![1, p.len() as i64]); }
