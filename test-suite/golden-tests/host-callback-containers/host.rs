type Fn1 = std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>;

#[derive(Clone)]
pub struct Ops {
    pub get: Fn1,
}

#[derive(Clone)]
pub struct Wrap {
    pub inner: Ops,
}

pub fn tick(path: &String) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new()
        .append(true)
        .create(true)
        .open(path)
        .unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(path).unwrap().len() as i64
}

fn ok_(x: i64) -> String {
    format!("ok {}", x)
}

pub fn h_apply(f: impl rustmorloc::MorlocFn1<i64, i64>, n: i64) -> String {
    ok_(f.call1(&n))
}

pub fn h_ops(o: &Ops, n: i64) -> String {
    ok_(o.get.call1(&n))
}

pub fn h_wrap(w: &Wrap, n: i64) -> String {
    ok_(w.inner.get.call1(&n))
}

pub fn h_tup(p: &(Fn1, i64)) -> String {
    ok_(p.0.call1(&p.1))
}

pub fn h_list(fs: &Vec<Fn1>, n: i64) -> String {
    ok_(fs[0].call1(&n) + fs[1].call1(&n))
}

pub fn h_opt(f: &Option<Fn1>, n: i64) -> String {
    match f {
        Some(g) => ok_(g.call1(&n)),
        None => "none".to_string(),
    }
}

pub fn h_susp_ops(s: impl rustmorloc::MorlocFn0<Ops>, n: i64) -> String {
    ok_(s.call0().get.call1(&n))
}

pub fn h_make_ops(k: i64) -> Ops {
    Ops { get: std::rc::Rc::new(move |i: &i64| *i + k) }
}

pub fn h_make_list(k: i64) -> Vec<Fn1> {
    vec![
        std::rc::Rc::new(move |i: &i64| *i + k),
        std::rc::Rc::new(move |i: &i64| *i * k),
    ]
}

pub fn h_cb_ret(mk: impl rustmorloc::MorlocFn1<i64, Ops>, n: i64) -> String {
    ok_(mk.call1(&n).get.call1(&n))
}

pub fn h_cb_param(cb: impl rustmorloc::MorlocFn1<Ops, i64>) -> String {
    ok_(cb.call1(&Ops { get: std::rc::Rc::new(|i: &i64| *i * 10) }))
}
