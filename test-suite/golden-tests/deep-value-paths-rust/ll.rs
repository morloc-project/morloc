use rustmorloc::RecBox;

// The cycle field holds the runtime's deferred-release box, so a chain of
// any length is freed without one frame per link.
pub struct LL {
    pub head: i64,
    pub tail: Option<RecBox<LL>>,
}

pub fn build_ll(n: i64) -> LL {
    let mut acc = LL { head: 0, tail: None };
    for i in 1..n {
        acc = LL { head: i, tail: Some(RecBox::new(acc)) };
    }
    acc
}

pub fn ll_len(l: &LL) -> i64 {
    let mut n = 1;
    let mut cur = l;
    while let Some(next) = &cur.tail {
        n += 1;
        cur = next;
    }
    n
}
