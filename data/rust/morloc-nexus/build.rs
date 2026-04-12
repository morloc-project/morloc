fn main() {
    let morloc_lib = format!(
        "{}/.local/share/morloc/lib",
        std::env::var("HOME").unwrap_or_else(|_| "/root".into())
    );
    println!("cargo:rustc-link-search=native={}", morloc_lib);
    println!("cargo:rustc-link-lib=dylib=morloc");

    // Embed rpath so the nexus finds libmorloc at runtime
    println!("cargo:rustc-link-arg=-Wl,-rpath,{}", morloc_lib);

    // The morloc compiler version is sourced from CARGO_PKG_VERSION
    // (this crate's Cargo.toml), which is intentionally kept in
    // lockstep with the morloc Haskell package.yaml. No build-time
    // extraction needed -- Cargo guarantees CARGO_PKG_VERSION is set
    // and rebuilds when Cargo.toml changes.
}
