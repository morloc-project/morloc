// The layout sum-types.md documents for a user-mapped data type, with a
// release that walks the chain instead of recursing down it.
#[derive(Clone)]
pub enum Chain {
    End,
    Link(Box<(i64, Chain)>),
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

pub fn size(n: i64) -> i64 {
    n
}
