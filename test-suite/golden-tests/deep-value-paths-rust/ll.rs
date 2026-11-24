// A user struct whose cycle field is a plain Box; its own Drop unlinks the
// chain iteratively, so a list of any length is freed without one frame per
// link.
pub struct LL {
    pub head: i64,
    pub tail: Option<Box<LL>>,
}

impl Drop for LL {
    fn drop(&mut self) {
        let mut next = self.tail.take();
        while let Some(mut b) = next {
            next = b.tail.take();
        }
    }
}

pub fn build_ll(n: i64) -> LL {
    let mut acc = LL { head: 0, tail: None };
    for i in 1..n {
        acc = LL { head: i, tail: Some(Box::new(acc)) };
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
