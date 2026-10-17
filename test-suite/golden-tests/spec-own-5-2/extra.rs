pub struct Pair {
    pub a: i64,
    pub b: Vec<i64>,
    pub c: Vec<i64>,
}

impl Clone for Pair {
    fn clone(&self) -> Pair {
        Pair { a: self.a, b: self.b.clone(), c: self.c.clone() }
    }
}

pub fn mk_pair(n: i64) -> Pair {
    Pair { a: n, b: (0..n).collect(), c: Vec::new() }
}
