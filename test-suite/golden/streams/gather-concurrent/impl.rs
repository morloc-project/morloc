pub fn produce_threads<F: Fn(&Vec<i64>) + Sync>(nthreads: i64, nbatches: i64, sink: F) {
    std::thread::scope(|s| {
        for t in 0..nthreads {
            let sink = &sink;
            s.spawn(move || {
                for b in 0..nbatches {
                    let v = t * 1000 + b * 4;
                    sink(&vec![v, v + 1, v + 2, v + 3]);
                }
            });
        }
    });
}

pub fn tally(xs: &Vec<i64>) -> String {
    let sum: i64 = xs.iter().sum();
    let whole = xs.len() % 4 == 0
        && xs.chunks(4).all(|c| c[0] % 4 == 0 && c.iter().enumerate().all(|(k, &x)| x == c[0] + k as i64));
    let mut last = std::collections::BTreeMap::new();
    let mut ordered = true;
    for &x in xs {
        if let Some(&p) = last.get(&(x / 1000)) {
            if p >= x {
                ordered = false;
            }
        }
        last.insert(x / 1000, x);
    }
    let d: std::collections::BTreeSet<i64> = xs.iter().cloned().collect();
    format!(
        "rust n={} distinct={} sum={} whole={} ordered={}\n",
        xs.len(),
        d.len(),
        sum,
        if whole { "yes" } else { "no" },
        if ordered { "yes" } else { "no" }
    )
}
