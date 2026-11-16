use crate::mlc::types::_local::main::Container;
pub fn same<A: Clone>(x: &A) -> A { x.clone() }
pub fn depthOf(c: &Container<i64>) -> i64 { let mut d = 0; let mut p = c; while let Some(s) = &p.sub { d += 1; p = s; } d }
