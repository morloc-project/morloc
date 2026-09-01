// Sourced Rust for the intrinsic-catch-do-fallback-rust golden.
// morloc `Str` (non-Copy) is passed by reference as `&String`.
pub fn rust_len(s: &String) -> i64 {
    s.len() as i64
}

pub fn rust_empty(s: &String) -> bool {
    s.is_empty()
}
