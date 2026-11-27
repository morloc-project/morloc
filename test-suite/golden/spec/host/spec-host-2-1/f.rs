pub fn emit<S: Fn(&Vec<i64>) -> R, R>(a: i64, b: i64, sink: S) {
    sink(&vec![a, b]);
}

pub fn say(xs: &Vec<i64>) {
    eprintln!("rust {} {}", xs[0], xs[1]);
}
