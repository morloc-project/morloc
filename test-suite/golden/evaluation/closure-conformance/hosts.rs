#[derive(Clone)]
pub struct Holder {
    pub run: std::sync::Arc<dyn Fn() -> i64 + Send + Sync>,
    pub tag: i64,
}

#[derive(Clone)]
pub struct Caller {
    pub inc: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
}

#[derive(Clone)]
pub struct Purer {
    pub apply: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
}


pub fn home_ident(x: i64) -> i64 {
    x
}

pub fn away_ident(x: i64) -> i64 {
    x
}

pub fn home_tick(path: &String) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new().append(true).create(true).open(path).unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(path).unwrap().len() as i64
}

pub fn home_mark(s: &String) -> String {
    s.clone()
}

pub fn home_str_len(s: &String) -> i64 {
    s.len() as i64
}

pub fn home_add(x: i64, y: i64) -> i64 {
    x + y
}

pub fn home_fail(path: &String) -> i64 {
    rustmorloc::morloc_throw(format!("failHome ran: {}", path))
}

pub fn home_mk(k: i64) -> std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync> {
    std::sync::Arc::new(move |x: &i64| x + k)
}

pub fn home_big(n: i64) -> Vec<i64> {
    (0..n).collect()
}

pub fn home_use_pure(f: impl Fn(&i64) -> i64, x: i64) -> i64 {
    f(&x)
}

pub fn home_take_thunk(t: impl Fn() -> i64) -> i64 {
    t();
    t();
    t()
}


pub fn away_bump(path: &String, _x: i64) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new().append(true).create(true).open(path).unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(path).unwrap().len() as i64
}

pub fn away_take_thunk(t: impl Fn() -> i64) -> i64 {
    t();
    t();
    t()
}

pub fn away_drop_thunk(_t: impl Fn() -> i64) -> i64 {
    0
}

pub fn away_use_record(h: &Holder) -> i64 {
    (h.run)();
    (h.run)();
    (h.run)() + h.tag
}

pub fn away_use_list(xs: &Vec<std::sync::Arc<dyn Fn() -> i64 + Send + Sync>>) -> i64 {
    xs[0]();
    xs[0]();
    xs[0]()
}

pub fn away_use_tuple(t: &(std::sync::Arc<dyn Fn() -> i64 + Send + Sync>, i64)) -> i64 {
    (t.0)();
    (t.0)();
    (t.0)() + t.1
}

pub fn away_use_opt(t: &Option<std::sync::Arc<dyn Fn() -> i64 + Send + Sync>>) -> i64 {
    match t {
        Some(f) => {
            f();
            f();
            f()
        }
        None => 0,
    }
}

pub fn away_use_nested(hs: &Vec<Holder>) -> i64 {
    (hs[0].run)();
    (hs[0].run)();
    (hs[0].run)()
}

pub fn away_use_callback(f: impl Fn(&i64) -> i64, x: i64) -> i64 {
    f(&x);
    f(&x);
    f(&x)
}

pub fn away_use_pure(f: impl Fn(&i64) -> i64, x: i64) -> i64 {
    f(&x)
}

pub fn away_use_arity2(f: impl Fn(&i64, &i64) -> i64, x: i64, y: i64) -> i64 {
    f(&x, &y)
}

pub fn away_use_pinned(c: &Caller, x: i64) -> i64 {
    (c.inc)(&x);
    (c.inc)(&x);
    (c.inc)(&x)
}

pub fn away_use_hof(
    f: impl Fn(&std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>) -> i64,
    g: impl Fn(&i64) -> i64 + Send + Sync + 'static,
) -> i64 {
    let boxed: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync> = std::sync::Arc::new(g);
    f(&boxed)
}

pub fn away_use_list_pure(fs: &Vec<std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>>, x: i64) -> i64 {
    fs.iter().map(|f| f(&x)).sum()
}

pub fn away_use_mk(
    f: impl Fn(&i64) -> std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>,
    a: i64,
    b: i64,
) -> i64 {
    f(&a)(&b)
}

pub fn away_use_thunk2(t: impl Fn() -> std::sync::Arc<dyn Fn() -> i64 + Send + Sync>) -> i64 {
    let inner = t();
    inner();
    inner();
    inner()
}

pub fn away_use_thunk_fn(t: impl Fn() -> std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>, x: i64) -> i64 {
    t()(&x)
}

pub fn away_use_fn_thunk(
    f: impl Fn(&std::sync::Arc<dyn Fn() -> i64 + Send + Sync>) -> i64,
    t: impl Fn() -> i64 + Send + Sync + 'static,
) -> i64 {
    let boxed: std::sync::Arc<dyn Fn() -> i64 + Send + Sync> = std::sync::Arc::new(t);
    f(&boxed)
}

pub fn away_pass_through(f: impl Fn(&i64) -> i64 + Send + Sync + 'static) -> std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync> {
    std::sync::Arc::new(f)
}

pub fn away_map_away<F: Fn(&i64) -> i64>(f: F, xs: &Vec<i64>) -> Vec<i64> {
    xs.iter().map(|x| f(x)).collect()
}

pub fn away_use_hof_eff(
    f: impl Fn(&std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>) -> i64,
    g: impl Fn(&i64) -> i64 + Send + Sync + 'static,
) -> i64 {
    let boxed: std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync> = std::sync::Arc::new(g);
    f(&boxed)
}

pub fn away_use_hof2(
    f: impl Fn(&std::sync::Arc<dyn Fn(&std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>) -> i64 + Send + Sync>) -> i64,
) -> i64 {
    let h: std::sync::Arc<dyn Fn(&std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>) -> i64 + Send + Sync> =
        std::sync::Arc::new(move |g: &std::sync::Arc<dyn Fn(&i64) -> i64 + Send + Sync>| g(&3));
    f(&h)
}

pub fn away_two(a: &String, b: &String, _x: i64) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new().append(true).create(true).open(a).unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(a).unwrap().len() as i64 + b.len() as i64
}

pub fn away_mix(x: i64, a: &String) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new().append(true).create(true).open(a).unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(a).unwrap().len() as i64 + x
}
