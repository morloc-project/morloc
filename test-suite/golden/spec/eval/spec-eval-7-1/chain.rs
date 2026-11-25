use std::sync::atomic::{AtomicI64, Ordering};

static CLONES: AtomicI64 = AtomicI64::new(0);

// The layout sum-types.md documents for a user-mapped data type: one box
// holding each arm's fields. Every clone of a link is counted.
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

// The count it is given, then the links copied so far.
pub fn clones(counted: i64) -> (i64, i64) {
    (counted, CLONES.load(Ordering::SeqCst))
}
