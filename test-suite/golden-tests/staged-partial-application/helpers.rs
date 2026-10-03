use std::io::Write;

fn tick_log() -> String {
    std::env::var("TICK_LOG").unwrap()
}

pub fn tick(x: i64) -> i64 {
    let mut f = std::fs::OpenOptions::new().create(true).append(true).open(tick_log()).unwrap();
    f.write_all(b"tick\n").unwrap();
    x
}

fn count_ticks() -> i64 {
    std::fs::read_to_string(tick_log()).map(|s| s.lines().count() as i64).unwrap_or(0)
}

pub fn ticks(_x: i64) -> i64 {
    count_ticks()
}

pub fn ticks_l(_x: &[i64]) -> i64 {
    count_ticks()
}

pub fn host_map<A, B, F: Fn(&A) -> B>(f: F, xs: &[A]) -> Vec<B> {
    xs.iter().map(|x| f(x)).collect()
}

pub fn host_map2<A, B, C, F: Fn(&A, &B) -> C>(f: F, xs: &[A], ys: &[B]) -> Vec<C> {
    xs.iter().zip(ys.iter()).map(|(x, y)| f(x, y)).collect()
}
