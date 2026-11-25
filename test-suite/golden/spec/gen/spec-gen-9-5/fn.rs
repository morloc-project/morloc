use crate::mlc::types::_local::main::T;
use std::sync::Arc;
pub fn inc(x: i64) -> i64 { x + 1 }
pub fn apply(t: &T<Arc<dyn Fn(i64) -> i64 + Send + Sync>, i64>) -> i64 { (t.val)(41) }
