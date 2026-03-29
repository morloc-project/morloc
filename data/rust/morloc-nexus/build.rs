fn main() {
    let morloc_lib = format!(
        "{}/.local/share/morloc/lib",
        std::env::var("HOME").unwrap_or_else(|_| "/root".into())
    );
    println!("cargo:rustc-link-search=native={}", morloc_lib);
    println!("cargo:rustc-link-lib=dylib=morloc");

    // Embed rpath so the nexus finds libmorloc at runtime
    let target = std::env::var("TARGET").unwrap_or_default();
    if target.contains("apple") {
        println!("cargo:rustc-link-arg=-Wl,-rpath,{}", morloc_lib);
    } else {
        println!("cargo:rustc-link-arg=-Wl,-rpath,{}", morloc_lib);
    }
}
