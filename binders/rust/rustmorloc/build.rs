// Link libmorloc.so so rustmorloc's own test/bench binaries resolve the C ABI
// symbols (shmalloc, abs2rel, make_data_packet_auto, ...). A generated pool
// links libmorloc through its own build script. An rlib carries these as
// undefined references; only executables need the link.
fn main() {
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
    println!("cargo:rustc-link-arg=-Wl,-rpath,{}", morloc_lib);
}
