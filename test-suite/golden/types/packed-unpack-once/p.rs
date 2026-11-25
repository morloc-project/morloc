use std::io::Write;

#[derive(Clone)]
pub struct PT {
    a: f64,
    b: f64,
    c: f64,
}

pub fn pack_inner(x: i64) -> f64 {
    x as f64
}

pub fn unpack_inner(x: f64) -> i64 {
    x as i64
}

pub fn pack_p(t: &(f64, f64, f64)) -> PT {
    PT { a: t.0, b: t.1, c: t.2 }
}

pub fn unpack_p(p: &PT) -> (f64, f64, f64) {
    let mut f = std::fs::OpenOptions::new().create(true).append(true).open("calls.log").unwrap();
    writeln!(f, "unpack_p").unwrap();
    (p.a, p.b, p.c)
}

pub fn mk_p(n: i64) -> PT {
    PT { a: n as f64, b: (n + 1) as f64, c: (n + 2) as f64 }
}
