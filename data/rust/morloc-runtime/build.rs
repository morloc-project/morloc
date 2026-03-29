fn main() {
    let target = std::env::var("TARGET").unwrap_or_default();

    // Link pthread (needed by pool, daemon thread pools)
    println!("cargo:rustc-link-lib=pthread");
    // Link rt (needed by shm_open/shm_unlink) — Linux only
    if target.contains("linux") {
        println!("cargo:rustc-link-lib=rt");
    }
}
