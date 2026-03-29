fn main() {
    let target = std::env::var("TARGET").unwrap_or_default();
    println!("cargo:rustc-link-lib=pthread");
    if target.contains("linux") {
        println!("cargo:rustc-link-lib=rt");
    }
}
