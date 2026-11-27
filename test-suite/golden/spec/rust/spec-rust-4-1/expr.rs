use std::sync::Arc;

#[derive(Clone)]
pub enum Expr {
    Num(f64),
    Neg(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Maybe1(Option<Box<Expr>>),
    Shared(Arc<Expr>, String),
}

pub fn eval(e: &Expr) -> f64 {
    match e {
        Expr::Num(x) => *x,
        Expr::Neg(a) => -eval(a),
        Expr::Add(a, b) => eval(a) + eval(b),
        Expr::Maybe1(m) => m.as_deref().map_or(0.0, eval),
        Expr::Shared(a, _) => eval(a),
    }
}

pub fn build(n: i64) -> Expr {
    let mut e = Expr::Num(1.0);
    for i in 0..n {
        e = match i % 4 {
            0 => Expr::Neg(Box::new(e)),
            1 => Expr::Add(Box::new(e), Box::new(Expr::Num(2.0))),
            2 => Expr::Maybe1(Some(Box::new(e))),
            _ => Expr::Shared(Arc::new(e), "x".to_string()),
        };
    }
    e
}

pub fn depth(e: &Expr) -> i64 {
    let mut d = 0;
    let mut p = e;
    loop {
        p = match p {
            Expr::Num(_) => return d,
            Expr::Neg(a) => a,
            Expr::Add(a, _) => a,
            Expr::Maybe1(Some(a)) => a,
            Expr::Maybe1(None) => return d,
            Expr::Shared(a, _) => a,
        };
        d += 1;
    }
}

pub fn maybes(n: i64) -> Expr {
    let mut e = Expr::Maybe1(None);
    for _ in 0..n { e = Expr::Maybe1(Some(Box::new(e))); }
    e
}
