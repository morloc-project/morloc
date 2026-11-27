// `make_list` produces the list that `map` runs over; because it is a sourced
// call it lowers to a manifold, so `map`'s list argument is a manifold result.
pub fn inc(x: i64) -> i64 {
    x + 1
}

pub fn make_list(n: i64) -> Vec<i64> {
    (0..n).collect()
}
