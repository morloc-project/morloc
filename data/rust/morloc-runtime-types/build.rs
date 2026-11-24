include!("build_id.rs");

fn main() {
    let here = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let root = here.parent().unwrap().to_path_buf();
    for c in SOURCE_CRATES {
        println!("cargo:rerun-if-changed={}", root.join(c).display());
    }
    println!("cargo:rerun-if-changed={}", root.join("Cargo.toml").display());
    println!("cargo:rerun-if-changed={}", root.join("Cargo.lock").display());
    let id = source_id(&root);
    let out = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("build_id.rs");
    std::fs::write(out, format!("0x{id:016x}u64\n")).unwrap();
}
