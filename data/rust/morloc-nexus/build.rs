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

    // Embed loader-relative rpaths so the nexus finds libmorloc (.so/.dylib)
    // regardless of install location:
    //   <origin>/../lib               covers /opt/morloc/bin -> /opt/morloc/lib
    //   <origin>/../share/morloc/lib  covers ~/.local/bin -> ~/.local/share/morloc/lib
    // The loader's origin token is platform-specific: ELF expands $ORIGIN,
    // Mach-O (macOS) expands @loader_path.
    let origin = match std::env::var("CARGO_CFG_TARGET_OS").as_deref() {
        Ok("macos") => "@loader_path",
        _ => "$ORIGIN",
    };
    println!("cargo:rustc-link-arg=-Wl,-rpath,{}/../lib", origin);
    println!("cargo:rustc-link-arg=-Wl,-rpath,{}/../share/morloc/lib", origin);

    // Test and other dev binaries run out of target/debug/deps, where neither
    // loader-relative rpath resolves, so they cannot load libmorloc at all.
    // Add the absolute library directory for those builds; release binaries
    // keep only the relative rpaths and stay relocatable.
    if std::env::var("PROFILE").as_deref() == Ok("debug") {
        println!("cargo:rustc-link-arg=-Wl,-rpath,{}", morloc_lib);
    }

    // The morloc compiler version is sourced from CARGO_PKG_VERSION
    // (this crate's Cargo.toml), which is intentionally kept in
    // lockstep with the morloc Haskell package.yaml. No build-time
    // extraction needed -- Cargo guarantees CARGO_PKG_VERSION is set
    // and rebuilds when Cargo.toml changes.
}
