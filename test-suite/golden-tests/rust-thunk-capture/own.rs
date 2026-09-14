pub fn rt_len(s: &String) -> i64 {
    s.len() as i64
}

// Throws when handed an empty list, so a @catch has something to catch.
pub fn rt_nonempty(xs: &Vec<i64>) -> Vec<i64> {
    if xs.is_empty() {
        rustmorloc::morloc_throw("rt_nonempty: empty");
    }
    xs.clone()
}

// A record of function values, so the frame holding the thunk also holds a
// closure. Without this the reuse-after-a-thunk shapes above do not reproduce
// the whole-frame capture defect.
pub struct Cmds {
    pub step: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
    pub view: std::rc::Rc<dyn rustmorloc::MorlocFn1<i64, i64>>,
}
pub fn rt_bump(x: i64) -> i64 { x + 1 }
pub fn rt_peek(x: i64) -> i64 { x }
pub fn rt_use_cmds(c: &Cmds, x: i64) -> i64 { c.view.call1(&c.step.call1(&x)) }
