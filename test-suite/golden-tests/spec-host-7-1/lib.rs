#[derive(Clone, Debug, PartialEq)]
pub struct Opts {
    pub lo: i64,
    pub hi: i64,
}

#[derive(Clone)]
pub struct Fns {
    pub run: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
    pub tag: i64,
}

pub fn par_map<A, B, F>(f: F, xs: &Vec<A>) -> Vec<B>
where
    A: Sync,
    B: Send,
    F: Fn(&A) -> B + Sync,
{
    let n = 4;
    let chunk = (xs.len() + n - 1) / n.max(1);
    if chunk == 0 {
        return Vec::new();
    }
    std::thread::scope(|s| {
        let hs: Vec<_> = xs
            .chunks(chunk)
            .map(|c| {
                let f = &f;
                s.spawn(move || c.iter().map(|x| f(x)).collect::<Vec<B>>())
            })
            .collect();
        hs.into_iter().flat_map(|h| h.join().unwrap()).collect()
    })
}

pub fn make_adder(k: i64) -> std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync> {
    std::sync::Arc::new(move |x: &i64| x + k)
}
