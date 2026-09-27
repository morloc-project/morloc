pub fn rounds<F>(f: F, k: i64) -> i64
where
    F: Fn(&i64) -> i64 + Sync,
{
    let mut total = 0;
    for r in 0..k {
        total += std::thread::scope(|s| {
            let hs: Vec<_> = (0..8).map(|t| { let f = &f; s.spawn(move || f(&(r * 8 + t))) }).collect();
            hs.into_iter().map(|h| h.join().unwrap()).sum::<i64>()
        });
    }
    total
}
