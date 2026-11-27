use crate::mlc::types::_local::main::{A, B, E, V, T, Wrap, H, N};
pub fn fh(h: &H) -> i64 { match &h.v { V::Plain => 0, _ => 1 } }
pub fn fn_(n: &N) -> i64 { n.v }
pub fn fb(b: &B) -> i64 { b.y }
pub fn fe(e: E) -> i64 { if e == E::Green { 1 } else { 0 } }
pub fn fv(_v: &V) -> i64 { 0 }
pub fn ft(t: &T) -> i64 { t.v }
pub fn fbox(_b: &Wrap<i64>) -> i64 { 0 }
pub fn fa(a: &A) -> i64 {
    let same = B { y: 2 } == B { y: 2 } && B::default().y == 0 && format!("{:?}", B { y: 2 }).contains('2');
    a.x + fb(&B { y: 2 }) + fe(E::Green) + if same { 0 } else { 100 }
}
