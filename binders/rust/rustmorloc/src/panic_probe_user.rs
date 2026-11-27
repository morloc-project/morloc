// A stand-in for a user's sourced file in the classifier tests.
pub const USER_FILE: &str = file!();
pub const USER_FILE_ABS: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/src/panic_probe_user.rs");
pub fn user_steps(n: usize) -> usize { (0..10).step_by(std::hint::black_box(n)).count() }
pub fn user_apply<F: Fn() -> usize>(f: F) -> usize { f() + 1 }
pub fn user_index(v: &[usize], i: usize) -> usize { v[i] }
