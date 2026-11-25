#[derive(Clone)]
pub struct Pair {
    pub a: i64,
    pub b: Vec<i64>,
}

impl Drop for Pair {
    fn drop(&mut self) {}
}

pub fn mk_pair(n: i64) -> Pair {
    Pair { a: n, b: (0..n).collect() }
}

pub fn remake(b: &Vec<i64>) -> Pair {
    Pair { a: 0, b: b.clone() }
}

pub fn total(b: &Vec<i64>) -> i64 {
    b.iter().sum()
}

#[derive(Clone)]
pub struct Outer {
    pub p: Pair,
    pub q: i64,
}

impl Drop for Outer {
    fn drop(&mut self) {}
}

pub fn mk_outer(n: i64) -> Outer {
    Outer { p: mk_pair(n), q: n + 1 }
}

pub enum Chain {
    End,
    Link(i64, Box<Chain>),
}

impl Clone for Chain {
    fn clone(&self) -> Chain {
        match self {
            Chain::End => Chain::End,
            Chain::Link(v, t) => Chain::Link(*v, t.clone()),
        }
    }
}

impl Drop for Chain {
    fn drop(&mut self) {}
}

pub fn mk_chain(n: i64) -> Chain {
    (1..=n).rev().fold(Chain::End, |acc, i| Chain::Link(i, Box::new(acc)))
}

pub fn chain_len(c: &Chain) -> i64 {
    let mut n = 0;
    let mut c = c;
    while let Chain::Link(_, t) = c {
        n += 1;
        c = t;
    }
    n
}

pub fn again(c: &Chain) -> Chain {
    c.clone()
}
