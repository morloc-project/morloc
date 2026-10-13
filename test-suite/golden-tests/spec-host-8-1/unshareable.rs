pub fn make_counter(k: i64) -> rustmorloc::FnValue1<i64, i64> {
    let base = std::rc::Rc::new(k);
    std::sync::Arc::new(move |x: &i64| x + *base)
}
