fn main() {
    let target = std::env::var("TARGET").unwrap_or_default();

    // Export ALL symbols when building as cdylib (glibc targets).
    // On musl, cdylib is not supported — the Dockerfile links the staticlib
    // into a .so manually with gcc --whole-archive.
    if !target.contains("musl") && !target.contains("apple") {
        let out_dir = std::env::var("OUT_DIR").unwrap();
        let vs_path = std::path::PathBuf::from(&out_dir).join("export_all.ver");
        std::fs::write(&vs_path, "{ global: *; };").unwrap();
        println!("cargo:rustc-cdylib-link-arg=-Wl,--version-script={}", vs_path.display());
    }

    // Link pthread (needed by pool, daemon thread pools)
    println!("cargo:rustc-link-lib=pthread");
    // Link rt (needed by shm_open/shm_unlink) — Linux only
    if target.contains("linux") {
        println!("cargo:rustc-link-lib=rt");
    }
}
