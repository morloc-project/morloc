use crate::mlc::types::_local::main::{Expr, Expr_Lit, Pair, Seq, Term};

pub fn deep_len(n: i64) -> i64 {
  let mut t = Seq::Nil;
  for i in 0..n { t = Seq::Cons((i, t).into()); }
  let mut len = 0;
  let mut p = &t;
  while let Seq::Cons(c) = p { len += 1; p = &c.1; }
  len
}

pub fn make_pair(s: &String, n: i64) -> Pair {
  Pair::Pair((s.clone(), n).into())
}

fn lit(v: f64) -> Expr_Lit {
  Expr_Lit::new((v,))
}

pub fn build_term(v: f64) -> Term {
  Term::Twice((Expr::Neg((Expr::Lit(lit(v)),).into()),).into())
}
