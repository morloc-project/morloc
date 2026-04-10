fn main() {
    let target = std::env::var("TARGET").unwrap_or_default();
    println!("cargo:rustc-link-lib=pthread");
    if target.contains("linux") {
        println!("cargo:rustc-link-lib=rt");
    }
    // The morloc compiler version is sourced from CARGO_PKG_VERSION
    // (this crate's Cargo.toml), which is intentionally kept in
    // lockstep with the morloc Haskell package.yaml.
}
