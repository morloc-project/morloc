pub fn inc(x: i64) -> i64 {
    x + 1
}

pub fn mapf(f: impl Fn(&i64) -> i64, n: i64) -> i64 {
    let mut s = 0;
    for i in 0..n {
        s += f(&i);
    }
    s
}

pub fn native(n: i64) -> i64 {
    let mut s = 0;
    for i in 0..n {
        s += inc(i);
    }
    s
}
