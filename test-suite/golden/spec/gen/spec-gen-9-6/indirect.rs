pub fn same<A: Clone>(x: &A) -> A { x.clone() }
use crate::mlc::types::_local::main::Container;
pub fn firstVal(c: &Container<Vec<Container<i64>>>) -> i64 { c.val[0].val }
