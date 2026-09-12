pub fn ident(x: &Expr) -> Expr {
    x.clone()
}

pub fn ident_term(x: &Term) -> Term {
    x.clone()
}

// Twice (Wrap (Add (Lit v) (Neg Zero)))
pub fn build(v: f64) -> Term {
    let inner = Expr::Add(Box::new((
        Expr::Lit(Box::new((v,))),
        Expr::Neg(Box::new((Term::Zero,))),
    )));
    Term::Twice(Box::new((Term::Wrap(Box::new((inner,))),)))
}
