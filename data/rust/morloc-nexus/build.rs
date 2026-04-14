fn main() {
    // Use MORLOC_HOME at build time if set, else fall back to $HOME default.
    // This is only for the compile-time link search path.
    let morloc_lib = std::env::var("MORLOC_HOME")
        .map(|h| format!("{}/lib", h))
        .unwrap_or_else(|_| {
            format!(
                "{}/.local/share/morloc/lib",
                std::env::var("HOME").unwrap_or_else(|_| "/root".into())
            )
        });
    println!("cargo:rustc-link-search=native={}", morloc_lib);
    println!("cargo:rustc-link-lib=dylib=morloc");

    // Embed $ORIGIN-relative rpaths so the nexus finds libmorloc.so
    // regardless of install location:
    //   $ORIGIN/../lib           covers /opt/morloc/bin -> /opt/morloc/lib
    //   $ORIGIN/../share/morloc/lib  covers ~/.local/bin -> ~/.local/share/morloc/lib
    println!("cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/../lib");
    println!("cargo:rustc-link-arg=-Wl,-rpath,$ORIGIN/../share/morloc/lib");

    // The morloc compiler version is sourced from CARGO_PKG_VERSION
    // (this crate's Cargo.toml), which is intentionally kept in
    // lockstep with the morloc Haskell package.yaml. No build-time
    // extraction needed -- Cargo guarantees CARGO_PKG_VERSION is set
    // and rebuilds when Cargo.toml changes.
}
