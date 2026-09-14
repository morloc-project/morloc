#[derive(Clone)]
pub struct MyWrap { pub v: i64 }

pub fn pack_wrap(x: i64) -> MyWrap { MyWrap { v: x } }
pub fn unpack_wrap(w: &MyWrap) -> i64 { w.v }

pub fn mk_wrapped(x: i64) -> MyWrap { MyWrap { v: x } }

// The callback is spelled with std vocabulary only.
pub fn apply_wrapped(f: impl Fn(&MyWrap) -> i64, w: &MyWrap) -> i64 {
    f(w)
}

// The closure RESULT is the packed type: `MyWrap` natively, `i64` on the wire.
pub fn apply_mk(f: impl Fn(&i64) -> MyWrap, x: i64) -> i64 {
    f(&x).v
}

pub fn add_wrapped_rust(a: i64, w: &MyWrap) -> i64 { a + w.v }

pub fn tick_rust() -> i64 { 0 }

// A record of closures BUILT in Rust and sent to another pool. The field's
// argument is the packed type, so Rust holds it as `MyWrap` while the wire
// carries `Int`.
pub struct Ops {
    pub f: std::rc::Rc<dyn rustmorloc::MorlocFn1<MyWrap, i64>>,
}
