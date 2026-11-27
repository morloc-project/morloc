pub struct NC { pub v: i64 }
pub struct H { pub p: NC, pub k: i64 }
pub fn mk_nc(n: i64) -> NC { NC { v: n } }
pub fn mk_h(n: i64) -> H { H { p: NC { v: n }, k: 1 } }
pub fn val(x: &NC) -> i64 { x.v }
pub fn take_nc(x: NC) -> i64 { x.v }
