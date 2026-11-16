use crate::mlc::types::_local::main::Container;
pub fn innerCI(c: &Container<Container<i64>>) -> i64 { c.val.val }
pub fn innerFoo(c: &Container<Container<i64>>) -> i64 { c.val.val }
