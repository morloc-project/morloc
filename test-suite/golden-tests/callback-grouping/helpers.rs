pub fn make_and_apply<G: Fn(&i64, &i64) -> i64>(g: G, x: i64) -> i64 {
    g(&x, &x)
}

pub fn make_and_apply3<G: Fn(&i64, &i64, &i64) -> i64>(g: G, x: i64) -> i64 {
    g(&x, &x, &x)
}
