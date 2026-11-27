include!("build_id.rs");

fn main() {
    let here = std::path::PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());
    let root = here.parent().unwrap().to_path_buf();
    for c in SOURCE_CRATES {
        println!("cargo:rerun-if-changed={}", root.join(c).display());
    }
    println!("cargo:rerun-if-changed={}", root.join("Cargo.toml").display());
    println!("cargo:rerun-if-changed={}", root.join("Cargo.lock").display());
    println!("cargo:rerun-if-changed={}", root.join(HEADER).display());
    let id = source_id(&root);
    let out_dir = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap());
    std::fs::write(out_dir.join("build_id.rs"), format!("0x{id:016x}u64\n")).unwrap();
    let header = std::fs::read_to_string(root.join(HEADER)).unwrap();
    let abi: u32 = header
        .lines()
        .find_map(|l| match l.split_whitespace().collect::<Vec<_>>()[..] {
            ["#define", "MORLOC_ABI_VERSION", v, ..] => v.parse().ok(),
            _ => None,
        })
        .expect("include/morloc.h must define MORLOC_ABI_VERSION as an integer");
    std::fs::write(out_dir.join("abi_version.rs"), format!("{abi}u32\n")).unwrap();
}
