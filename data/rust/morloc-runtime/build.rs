fn main() {
    let target = std::env::var("TARGET").unwrap_or_default();
    println!("cargo:rustc-link-lib=pthread");
    if target.contains("linux") {
        println!("cargo:rustc-link-lib=rt");
    }
    if target.contains("apple") || target.contains("darwin") {
        // Cargo's default cdylib install_name is the absolute build-tree
        // path, so consumers (nexus, the language shims) would record that
        // path and fail to load a relocated libmorloc. Advertise the id the
        // shims expect instead; `morloc init` renames the file to
        // libmorloc.dylib and consumers resolve it via their @loader_path
        // rpaths. `rustc-cdylib-link-arg` scopes this to the cdylib only
        // (not the crate's test binaries).
        println!("cargo:rustc-cdylib-link-arg=-Wl,-install_name,@rpath/libmorloc.dylib");
    }
    // The morloc compiler version is sourced from CARGO_PKG_VERSION
    // (this crate's Cargo.toml), which is intentionally kept in
    // lockstep with the morloc Haskell package.yaml.
}
