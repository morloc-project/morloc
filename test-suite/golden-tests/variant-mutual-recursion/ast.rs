pub fn ident(x: &Expr) -> Expr {
    x.clone()
}

pub fn ident_term(x: &Term) -> Term {
    x.clone()
}

// Twice (Wrap (Add (Lit v) (Neg Zero)))
//
// A generated arm holds its fields in the runtime's RecBox; a tuple of the
// fields converts into it.
pub fn build(v: f64) -> Term {
    let inner = Expr::Add((
        Expr::Lit((v,).into()),
        Expr::Neg((Term::Zero,).into()),
    ).into());
    Term::Twice((Term::Wrap((inner,).into()),).into())
}
