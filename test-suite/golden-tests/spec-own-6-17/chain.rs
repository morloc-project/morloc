use std::sync::atomic::{AtomicI64, Ordering};

static CLONES: AtomicI64 = AtomicI64::new(0);

// A clone recurses down the chain and counts each link it copies.
pub enum Chain {
    End,
    Link(Box<(i64, Chain)>),
}

impl Clone for Chain {
    fn clone(&self) -> Chain {
        match self {
            Chain::End => Chain::End,
            Chain::Link(b) => {
                CLONES.fetch_add(1, Ordering::SeqCst);
                Chain::Link(Box::new((b.0, b.1.clone())))
            }
        }
    }
}

impl Drop for Chain {
    fn drop(&mut self) {
        let mut next = match self {
            Chain::Link(b) => std::mem::replace(&mut b.1, Chain::End),
            Chain::End => return,
        };
        while let Chain::Link(b) = &mut next {
            let tail = std::mem::replace(&mut b.1, Chain::End);
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
    for i in (1..=n).rev() { c = Chain::Link(Box::new((i, c))); }
    c
}

pub fn chain_len(c: &Chain) -> i64 {
    let mut n = 0;
    let mut c = c;
    while let Chain::Link(b) = c { n += 1; c = &b.1; }
    n
}

#[derive(Clone)]
pub struct Holder {
    pub c: Chain,
    pub k: i64,
}

pub fn mk_holder(n: i64) -> Holder { Holder { c: mk_chain(n), k: 1 } }

pub fn mk_tup(n: i64) -> (Chain, i64) { (mk_chain(n), 1) }
