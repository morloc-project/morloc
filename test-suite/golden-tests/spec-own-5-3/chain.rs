use std::sync::atomic::{AtomicI64, Ordering};

static CLONES: AtomicI64 = AtomicI64::new(0);

// A clone recurses down the chain and counts each link it copies.
pub enum Chain {
    End,
    Link(i64, Box<Chain>),
}

impl Clone for Chain {
    fn clone(&self) -> Chain {
        match self {
            Chain::End => Chain::End,
            Chain::Link(v, t) => {
                CLONES.fetch_add(1, Ordering::SeqCst);
                Chain::Link(*v, t.clone())
            }
        }
    }
}

impl Drop for Chain {
    fn drop(&mut self) {
        let mut next = match self {
            Chain::Link(_, t) => std::mem::replace(&mut **t, Chain::End),
            Chain::End => return,
        };
        while let Chain::Link(_, t) = &mut next {
            let tail = std::mem::replace(&mut **t, Chain::End);
            next = tail;
        }
    }
}

// The count it is given, then the links copied so far.
pub fn clones(counted: i64) -> (i64, i64) {
    (counted, CLONES.load(Ordering::SeqCst))
}

pub fn mk_chain(n: i64) -> Chain {
    let mut c = Chain::End;
    for i in (1..=n).rev() { c = Chain::Link(i, Box::new(c)); }
    c
}

pub fn chain_len(c: &Chain) -> i64 {
    let mut n = 0;
    let mut c = c;
    while let Chain::Link(_, t) = c { n += 1; c = t; }
    n
}

#[derive(Clone)]
pub struct Holder {
    pub c: Chain,
    pub k: i64,
}

pub fn mk_holder(n: i64) -> Holder { Holder { c: mk_chain(n), k: 1 } }

pub fn mk_tup(n: i64) -> (Chain, i64) { (mk_chain(n), 1) }
