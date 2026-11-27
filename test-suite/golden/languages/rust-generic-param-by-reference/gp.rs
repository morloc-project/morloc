pub fn is_none<A>(x: &Option<A>) -> bool { x.is_none() }
pub fn first_of<A: Clone, B>(x: &(A, B)) -> A { x.0.clone() }
pub fn first_is_none<A>(x: &(Option<A>, i64)) -> bool { x.0.is_none() }
