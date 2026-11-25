use mlc_types::mlc::types::_local::main::{Chain, ChainFields, Shape, Shape_Rect, P};
fn main() {
    let p = P { x: 3, name: "a".to_string() };
    let s = Shape::Rect(Shape_Rect::new((2.0, 3.0)));
    let mut c = Chain::new(ChainFields { v: 0, next: None });
    for i in 1..=100000 { c = Chain::new(ChainFields { v: i, next: Some(c) }); }
    let mut n = 0;
    let mut q = &c.next;
    while let Some(x) = q { n += 1; q = &x.next; }
    let area = match &s { Shape::Rect(r) => r.0 * r.1, Shape::Circle(r) => r.0 };
    println!("{} {} {} {}", p.x, p.name, area, n);
}
