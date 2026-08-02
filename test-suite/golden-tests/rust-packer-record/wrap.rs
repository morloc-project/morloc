#[derive(Clone)]
pub struct MyWrap { pub v: i64 }
pub fn pack_wrap(x: i64) -> MyWrap { MyWrap { v: x } }
pub fn unpack_wrap(w: &MyWrap) -> i64 { w.v }

// Native `MyStr` over a `String` WIRE type: unlike `MyWrap` (wire `i64`, Copy),
// the wire type here is not Copy, so the deserialize-side pack argument is
// borrowed. `pack_str` therefore takes the wire `String` by reference.
#[derive(Clone)]
pub struct MyStr { pub v: String }
pub fn pack_str(x: &String) -> MyStr { MyStr { v: x.clone() } }
pub fn unpack_str(w: &MyStr) -> String { w.v.clone() }
