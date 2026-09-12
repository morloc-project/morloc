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
