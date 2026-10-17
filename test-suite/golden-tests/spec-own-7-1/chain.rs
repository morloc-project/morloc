use std::sync::atomic::{AtomicI64, Ordering};

static CLONES: AtomicI64 = AtomicI64::new(0);

// The layout sum-types.md documents for a user-mapped data type. A clone
// recurses down the chain, as a derived one would; a release walks it.
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
