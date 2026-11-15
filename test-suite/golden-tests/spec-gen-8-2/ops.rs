use crate::mlc::types::_local::main::{Chain, Ops};

pub fn bump(x: i64) -> i64 { x + 1 }

pub fn apply_ops(o: &Ops, x: i64) -> i64 { (o.step)(&x) * 10 }

pub fn make_ops(n: &String) -> Ops {
  Ops { step: std::sync::Arc::new(|x: &i64| x * 2), name: n.clone() }
}

pub fn apply_chain(c: &Chain, x: i64) -> i64 {
  let y = (c.run)(&x);
  match &c.next { Some(n) => apply_chain(n, y), None => y }
}
