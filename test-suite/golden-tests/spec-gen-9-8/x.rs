#[derive(Clone, Debug)]
pub struct VT<A> { pub xs: Vec<A>, pub tag: A }
pub fn same<T: Clone>(x: &T) -> T { x.clone() }
