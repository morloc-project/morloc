use std::rc::Rc;

#[derive(Clone)]
pub struct Holder {
    pub run: std::rc::Rc<dyn rustmorloc::MorlocFn0<i64>>,
    pub tag: i64,
}

#[derive(Clone)]
pub struct Caller {
    pub inc: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
}

#[derive(Clone)]
pub struct Purer {
    pub apply: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
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

pub fn home_mk(k: i64) -> std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>> {
    std::rc::Rc::new(move |x: &i64| x + k)
}

pub fn home_big(n: i64) -> Vec<i64> {
    (0..n).collect()
}

pub fn home_use_pure(f: impl rustmorloc::MorlocFn1<i64, i64>, x: i64) -> i64 {
    f.call1(&x)
}

pub fn home_take_thunk(t: impl rustmorloc::MorlocFn0<i64>) -> i64 {
    t.call0();
    t.call0();
    t.call0()
}


pub fn away_bump(path: &String, _x: i64) -> i64 {
    use std::io::Write;
    let mut f = std::fs::OpenOptions::new().append(true).create(true).open(path).unwrap();
    f.write_all(b"x").unwrap();
    std::fs::metadata(path).unwrap().len() as i64
}

pub fn away_take_thunk(t: impl rustmorloc::MorlocFn0<i64>) -> i64 {
    t.call0();
    t.call0();
    t.call0()
}

pub fn away_drop_thunk(_t: impl rustmorloc::MorlocFn0<i64>) -> i64 {
    0
}

pub fn away_use_record(h: &Holder) -> i64 {
    h.run.call0();
    h.run.call0();
    h.run.call0() + h.tag
}

pub fn away_use_list(xs: &Vec<Rc<dyn rustmorloc::MorlocFn0<i64>>>) -> i64 {
    xs[0].call0();
    xs[0].call0();
    xs[0].call0()
}

pub fn away_use_tuple(t: &(Rc<dyn rustmorloc::MorlocFn0<i64>>, i64)) -> i64 {
    t.0.call0();
    t.0.call0();
    t.0.call0() + t.1
}

pub fn away_use_opt(t: &Option<Rc<dyn rustmorloc::MorlocFn0<i64>>>) -> i64 {
    match t {
        Some(f) => {
            f.call0();
            f.call0();
            f.call0()
        }
        None => 0,
    }
}

pub fn away_use_nested(hs: &Vec<Holder>) -> i64 {
    hs[0].run.call0();
    hs[0].run.call0();
    hs[0].run.call0()
}

pub fn away_use_callback(f: impl rustmorloc::MorlocFn1<i64, i64>, x: i64) -> i64 {
    f.call1(&x);
    f.call1(&x);
    f.call1(&x)
}

pub fn away_use_pure(f: impl rustmorloc::MorlocFn1<i64, i64>, x: i64) -> i64 {
    f.call1(&x)
}

pub fn away_use_arity2(f: impl rustmorloc::MorlocFn2<i64, i64, i64>, x: i64, y: i64) -> i64 {
    f.call2(&x, &y)
}

pub fn away_use_pinned(c: &Caller, x: i64) -> i64 {
    c.inc.call1(&x);
    c.inc.call1(&x);
    c.inc.call1(&x)
}

pub fn away_use_hof(
    f: impl rustmorloc::MorlocFn1<Rc<dyn rustmorloc::MorlocFn1<i64, i64>>, i64>,
    g: impl rustmorloc::MorlocFn1<i64, i64> + 'static,
) -> i64 {
    let boxed: Rc<dyn rustmorloc::MorlocFn1<i64, i64>> = Rc::new(g);
    f.call1(&boxed)
}

pub fn away_use_list_pure(fs: &Vec<Rc<dyn rustmorloc::MorlocFn1<i64, i64>>>, x: i64) -> i64 {
    fs.iter().map(|f| f.call1(&x)).sum()
}

pub fn away_use_mk(
    f: impl rustmorloc::MorlocFn1<i64, Rc<dyn rustmorloc::MorlocFn1<i64, i64>>>,
    a: i64,
    b: i64,
) -> i64 {
    f.call1(&a).call1(&b)
}

pub fn away_use_thunk2(t: impl rustmorloc::MorlocFn0<Rc<dyn rustmorloc::MorlocFn0<i64>>>) -> i64 {
    let inner = t.call0();
    inner.call0();
    inner.call0();
    inner.call0()
}

pub fn away_use_thunk_fn(t: impl rustmorloc::MorlocFn0<Rc<dyn rustmorloc::MorlocFn1<i64, i64>>>, x: i64) -> i64 {
    t.call0().call1(&x)
}

pub fn away_use_fn_thunk(
    f: impl rustmorloc::MorlocFn1<Rc<dyn rustmorloc::MorlocFn0<i64>>, i64>,
    t: impl rustmorloc::MorlocFn0<i64> + 'static,
) -> i64 {
    let boxed: Rc<dyn rustmorloc::MorlocFn0<i64>> = Rc::new(t);
    f.call1(&boxed)
}

pub fn away_pass_through(f: impl rustmorloc::MorlocFn1<i64, i64> + 'static) -> Rc<dyn rustmorloc::MorlocFn1<i64, i64>> {
    Rc::new(f)
}

pub fn away_map_away<F: Fn(&i64) -> i64>(f: F, xs: &Vec<i64>) -> Vec<i64> {
    xs.iter().map(|x| f(x)).collect()
}

pub fn away_use_hof_eff(
    f: impl rustmorloc::MorlocFn1<Rc<dyn rustmorloc::MorlocFn1<i64, i64>>, i64>,
    g: impl rustmorloc::MorlocFn1<i64, i64> + 'static,
) -> i64 {
    let boxed: Rc<dyn rustmorloc::MorlocFn1<i64, i64>> = Rc::new(g);
    f.call1(&boxed)
}

pub fn away_use_hof2(
    f: impl rustmorloc::MorlocFn1<Rc<dyn rustmorloc::MorlocFn1<Rc<dyn rustmorloc::MorlocFn1<i64, i64>>, i64>>, i64>,
) -> i64 {
    let h: Rc<dyn rustmorloc::MorlocFn1<Rc<dyn rustmorloc::MorlocFn1<i64, i64>>, i64>> =
        Rc::new(move |g: &Rc<dyn rustmorloc::MorlocFn1<i64, i64>>| g.call1(&3));
    f.call1(&h)
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
