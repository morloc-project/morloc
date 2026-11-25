// Sourced Rust for the rust-runtime-panic golden: it misuses a runtime
// function so that the runtime panics on the user's behalf.
pub fn r_weave(n: i64) -> i64 {
    if n == 0 { rustmorloc::interweave_strings(&[], &["x"]).len() as i64 } else { n }
}
