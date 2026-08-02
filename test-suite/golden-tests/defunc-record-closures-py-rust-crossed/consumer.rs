pub fn use_list(fs: &Vec<std::rc::Rc<dyn rustmorloc::MorlocFn1<i64,i64>>>, x: i64) -> i64 { fs[0].call1(&x) + fs[1].call1(&x) }
pub fn use_tuple(t: &(std::rc::Rc<dyn rustmorloc::MorlocFn1<i64,i64>>, std::rc::Rc<dyn rustmorloc::MorlocFn1<i64,i64>>), x: i64) -> i64 { t.0.call1(&x) + t.1.call1(&x) }
pub fn use_opt(f: &Option<std::rc::Rc<dyn rustmorloc::MorlocFn1<i64,i64>>>, x: i64) -> i64 { match f { Some(g) => g.call1(&x), None => x } }
