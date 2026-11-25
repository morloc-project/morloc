pub fn from_str(s: &String) -> Vec<i64> {
    if s.is_empty() { rustmorloc::morloc_throw("from_str: empty"); }
    vec![s.len() as i64]
}
pub fn from_two(s: &String, v: &Vec<i64>) -> Vec<i64> {
    if s.is_empty() { rustmorloc::morloc_throw("from_two: empty"); }
    let mut o = v.clone(); o.push(s.len() as i64); o
}
pub fn from_mixed(n: i64, s: &String) -> Vec<i64> {
    if s.is_empty() { rustmorloc::morloc_throw("from_mixed: empty"); }
    vec![n, s.len() as i64]
}
pub fn from_copy(n: i64) -> Vec<i64> {
    if n == 0 { rustmorloc::morloc_throw("from_copy: zero"); }
    vec![n]
}
pub fn from_list(v: &Vec<i64>) -> Vec<i64> {
    if v.is_empty() { rustmorloc::morloc_throw("from_list: empty"); }
    let mut o = v.clone(); o.push(99); o
}
