use crate::mlc::types::_local::main::{Chain, Holder, Tree};

pub fn sum_chain(c: &Chain) -> i64 { c.val + c.next.as_ref().map_or(0, |n| sum_chain(n)) }

pub fn count_tree(t: &Tree) -> i64 { 1 + t.kids.iter().map(count_tree).sum::<i64>() }

pub fn holder_sum(h: &Holder) -> i64 {
  sum_chain(&h.one) + h.maybe.as_ref().map_or(0, sum_chain) + h.many.iter().map(sum_chain).sum::<i64>() + count_tree(&h.tree)
}

pub fn deep_build(n: i64) -> i64 {
  let mut t: Option<Chain> = None;
  for _ in 0..n { t = Some(Chain::new(crate::mlc::types::_local::main::ChainFields { val: 1, next: t })); }
  let mut len = 0;
  let mut p = &t;
  while let Some(c) = p { len += 1; p = &c.next; }
  len
}
