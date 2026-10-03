pub fn make_and_apply<G: Fn(&i64) -> std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>>(g: G, x: i64) -> i64 {
    rustmorloc::MorlocFn1::call1(&g(&x), &x)
}

pub fn make_and_apply3<
    G: Fn(&i64) -> std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>>>,
>(g: G, x: i64) -> i64 {
    let h = g(&x);
    let k = rustmorloc::MorlocFn1::call1(&h, &x);
    rustmorloc::MorlocFn1::call1(&k, &x)
}
