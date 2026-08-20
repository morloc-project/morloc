//! Minimal host-platform detection for the dependency kernel: just the conda
//! platform tag the pixi manifest needs. (The manager's `hostprobe` does richer
//! backend classification; this is the subset the in-env agent needs.)

/// The conda platform string for an (os, arch) pair (the values of
/// `std::env::consts::{OS,ARCH}`), e.g. "linux-64", "osx-arm64", or "unknown".
pub fn conda_platform_for(os: &str, arch: &str) -> String {
    match (os, arch) {
        ("linux", "x86_64") => "linux-64",
        ("linux", "aarch64") => "linux-aarch64",
        ("macos", "x86_64") => "osx-64",
        ("macos", "aarch64") => "osx-arm64",
        _ => "unknown",
    }
    .to_string()
}

/// The conda platform string for the current host.
pub fn conda_platform() -> String {
    conda_platform_for(std::env::consts::OS, std::env::consts::ARCH)
}
