// Rust functions composed across the socket boundary with Python.
pub fn rust_double(x: i64) -> i64 {
    x * 2
}

pub fn rust_inc(x: i64) -> i64 {
    x + 1
}
