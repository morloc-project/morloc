pub fn drain<P>(pull: P) -> i64
where
    P: Fn() -> (bool, Vec<i64>),
{
    let mut total = 0;
    loop {
        let (ok, xs) = pull();
        if !ok || xs.is_empty() { return total; }
        total += xs.iter().sum::<i64>();
    }
}

pub fn force<T, P>(p: P) -> T
where
    P: Fn() -> T,
{
    p()
}

pub fn same<A: Clone>(x: &A) -> A {
    x.clone()
}
