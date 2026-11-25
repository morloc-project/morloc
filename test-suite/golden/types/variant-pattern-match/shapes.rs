// Non-Copy values reach a sourced Rust function by reference, so a
// pass-through returns a clone rather than the borrow.
pub fn ident(x: &Shape) -> Shape {
    x.clone()
}
