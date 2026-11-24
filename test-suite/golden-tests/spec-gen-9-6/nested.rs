use crate::mlc::types::_local::main::Container;
pub fn same<A: Clone>(x: &A) -> A { x.clone() }
pub fn innerVal(c: &Container<Container<i64>>) -> i64 { c.val.val }
