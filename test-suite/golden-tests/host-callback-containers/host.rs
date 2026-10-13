type Fn1 = std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>;

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

pub fn h_apply(f: impl Fn(&i64) -> i64, n: i64) -> String {
    ok_(f(&n))
}

pub fn h_ops(o: &Ops, n: i64) -> String {
    ok_((o.get)(&n))
}

pub fn h_wrap(w: &Wrap, n: i64) -> String {
    ok_((w.inner.get)(&n))
}

pub fn h_tup(p: &(Fn1, i64)) -> String {
    ok_((p.0)(&p.1))
}

pub fn h_list(fs: &Vec<Fn1>, n: i64) -> String {
    ok_(fs[0](&n) + fs[1](&n))
}

pub fn h_opt(f: &Option<Fn1>, n: i64) -> String {
    match f {
        Some(g) => ok_(g(&n)),
        None => "none".to_string(),
    }
}

pub fn h_susp_ops(s: impl Fn() -> Ops, n: i64) -> String {
    ok_((s().get)(&n))
}

pub fn h_make_ops(k: i64) -> Ops {
    Ops { get: std::sync::Arc::new(move |i: &i64| *i + k) }
}

pub fn h_make_list(k: i64) -> Vec<Fn1> {
    vec![
        std::sync::Arc::new(move |i: &i64| *i + k),
        std::sync::Arc::new(move |i: &i64| *i * k),
    ]
}

pub fn h_cb_ret(mk: impl Fn(&i64) -> Ops, n: i64) -> String {
    ok_((mk(&n).get)(&n))
}

pub fn h_cb_param(cb: impl Fn(&Ops) -> i64) -> String {
    ok_(cb(&Ops { get: std::sync::Arc::new(|i: &i64| *i * 10) }))
}

pub fn rs_wrap(s: &String) -> String {
    s.clone()
}
