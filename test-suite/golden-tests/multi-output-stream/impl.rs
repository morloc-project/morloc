use std::io::Write;

pub fn produce<F: Fn(&Vec<i64>)>(log: &String, sink: F) {
    let mut f = std::fs::OpenOptions::new().append(true).create(true).open(log).unwrap();
    f.write_all(b"ran\n").unwrap();
    sink(&vec![1, 2]);
    sink(&vec![]);
    sink(&vec![3, 4, 5]);
    sink(&(0..20000).collect());
}

pub fn produce_tail<F: Fn(&Vec<i64>)>(sink: F) {
    sink(&vec![7]);
}

pub fn join_all(xs: &Vec<i64>) -> String {
    let head: Vec<String> = xs.iter().take(8).map(|x| x.to_string()).collect();
    format!("{}: {}\n", xs.len(), head.join(","))
}

pub fn show_batch(xs: &Vec<i64>) -> String {
    format!("batch of {}\n", xs.len())
}

pub fn zero() -> (i64, Vec<i64>) {
    (0, vec![])
}

pub fn add_batch(acc: &(i64, Vec<i64>), xs: &Vec<i64>) -> (i64, Vec<i64>) {
    let mut sizes = acc.1.clone();
    sizes.push(xs.len() as i64);
    (acc.0 + xs.iter().sum::<i64>(), sizes)
}

pub fn merge(a: &(i64, Vec<i64>), b: &(i64, Vec<i64>)) -> (i64, Vec<i64>) {
    let mut sizes = a.1.clone();
    sizes.extend(b.1.iter().cloned());
    (a.0 + b.0, sizes)
}

pub fn show_acc(acc: &(i64, Vec<i64>)) -> String {
    let sizes: Vec<String> = acc.1.iter().map(|x| x.to_string()).collect();
    format!("sum={} sizes={}\n", acc.0, sizes.join(","))
}
