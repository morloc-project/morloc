pub fn par_map<F>(f: F, xs: &Vec<i64>) -> Vec<i64>
where
    F: Fn(&i64) -> i64 + Sync,
{
    let mid = xs.len() / 2;
    let (a, b) = xs.split_at(mid);
    std::thread::scope(|s| {
        let h = s.spawn(|| a.iter().map(|x| f(x)).collect::<Vec<i64>>());
        let mut rest: Vec<i64> = b.iter().map(|x| f(x)).collect();
        let mut out = h.join().unwrap();
        out.append(&mut rest);
        out
    })
}

pub fn force<T, P>(p: P) -> T
where
    P: Fn() -> T,
{
    p()
}

pub fn par_map_io<F>(f: F, xs: &Vec<i64>) -> Vec<i64>
where
    F: Fn(&i64) -> i64 + Sync,
{
    par_map(f, xs)
}
