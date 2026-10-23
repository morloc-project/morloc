use std::sync::atomic::{AtomicI64, Ordering};

static CLONES: AtomicI64 = AtomicI64::new(0);

// The layout sum-types.md documents for a user-mapped data type: one box
// holding each arm's fields. Every clone of a link is counted.
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

// The count it is given, then the links copied so far.
pub fn clones(counted: i64) -> (i64, i64) {
    (counted, CLONES.load(Ordering::SeqCst))
}
