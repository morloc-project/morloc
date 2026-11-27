// Sourced Rust for the rust-record golden. LL and Tree are user structs the
// morloc-generated marshalling impls target; recursion is broken by Box (?LL)
// and by Vec ([Tree]).
pub fn rust_double(x: i64) -> i64 {
    x * 2
}

pub struct LL {
    pub head: i64,
    pub tail: Option<Box<LL>>,
}
pub fn build_ll(n: i64) -> LL {
    LL { head: n, tail: Some(Box::new(LL { head: n + 1, tail: None })) }
}
pub fn sum_ll(l: &LL) -> i64 {
    l.head + match &l.tail {
        Some(b) => sum_ll(b),
        None => 0,
    }
}

pub struct Tree {
    pub value: i64,
    pub children: Vec<Tree>,
}
pub fn sum_tree(t: &Tree) -> i64 {
    t.value + t.children.iter().map(sum_tree).sum::<i64>()
}
