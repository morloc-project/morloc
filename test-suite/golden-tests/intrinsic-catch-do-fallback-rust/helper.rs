// Sourced Rust for the intrinsic-catch-do-fallback-rust golden.
// Parallel to the cpp/py helpers: returns a single-element list holding the
// string length, or raises (a MorlocThrow panic that `@catch` intercepts) on
// empty input so the fallback path is exercised.
// morloc `Str` (non-Copy) is passed by reference as `&String`; morloc `Int`
// maps to Rust `i64`.
pub fn rust_maybe_list(s: &String) -> Vec<i64> {
    if s.is_empty() {
        rustmorloc::morloc_throw("rust_maybe_list: empty input");
    }
    vec![s.len() as i64]
}
