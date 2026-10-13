#[derive(Clone)]
pub struct Ops {
    pub run: rustmorloc::FnValue1<i64, i64>,
    pub later: rustmorloc::FnValue0<i64>,
}

pub fn make_adder(k: i64) -> rustmorloc::FnValue1<i64, i64> {
    std::sync::Arc::new(move |x: &i64| x + k)
}

pub fn use_ops(ops: &Ops, x: i64) -> i64 {
    let ops = ops.clone();
    std::thread::spawn(move || ops.run.call1(&x) + ops.later.call0())
        .join()
        .unwrap()
}
