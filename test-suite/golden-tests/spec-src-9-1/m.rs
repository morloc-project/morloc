pub fn my_require<A: Clone + Default>(x: &Option<A>) -> A { x.clone().unwrap_or_default() }
