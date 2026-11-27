pub fn countAll<C>(c: &Vec<C>) -> i64 { c.len() as i64 }
pub fn same<A: Clone>(x: &A) -> A { x.clone() }
