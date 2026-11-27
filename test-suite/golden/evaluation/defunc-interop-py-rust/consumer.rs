// A morloc function value reaches a sourced Rust higher-order function as a
// native `Fn(&Int) -> Int`. A closure crossing from another pool is reflected
// into exactly this shape (a bare closure that RPCs home on application), so it
// passes by value like any in-pool closure.
pub fn applyIt<F: Fn(&i64) -> i64>(f: F, x: i64) -> i64 {
    f(&x)
}
