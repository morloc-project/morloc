// A stand-in for a pool's generated source in the classifier tests: the
// hook's classifier frame lives here, as mlc_classify_panic does in a pool.
pub const POOL_FILE: &str = file!();
pub const POOL_FILE_ABS: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/src/panic_probe_pool.rs");
#[inline(never)]
pub extern "C" fn probe_classify_panic(file: *const u8, len: usize) -> bool {
    std::hint::black_box(panic_is_runtime(file, len))
}
