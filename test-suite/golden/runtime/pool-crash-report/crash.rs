pub fn crash(x: i64) -> i64 {
    // A real fault, not a raised signal: a raised SIGSEGV is swallowed by
    // the Rust runtime's guard-page handler, which returns when the address
    // is not a guard page.
    let p: *mut i64 = std::ptr::null_mut();
    unsafe { p.write_volatile(x) };
    x
}
