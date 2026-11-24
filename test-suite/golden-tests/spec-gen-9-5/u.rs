#[derive(Clone)]
pub struct MyL<A> { pub val: A, pub next: Option<Box<MyL<A>>> }
pub fn same<A: Clone>(x: &A) -> A { x.clone() }
pub fn innerL(l: &MyL<MyL<i64>>) -> i64 { l.val.val }
