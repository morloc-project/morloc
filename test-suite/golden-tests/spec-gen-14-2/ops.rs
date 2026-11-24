pub fn addOne(x: i64) -> i64 { x + 1 }
pub fn sumOps(o: &crate::mlc::types::_local::main::Ops) -> i64 { o.fs.iter().map(|f| f(o.n)).sum() }
