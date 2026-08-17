//! Host capability probe: decide whether the native (no-container) backend is
//! viable on this machine, and report the conda platform string.
//!
//! The native backend runs pool processes directly on the host against a
//! pixi/conda-provided toolchain. conda binaries bake the glibc dynamic loader
//! at its conventional FHS path (`/lib64/ld-linux-x86-64.so.2` on x86_64), so a
//! host can run them natively only if it is macOS, or a glibc + FHS Linux.
//! NixOS (no standard FHS) and musl systems (Alpine) cannot, and route to a
//! container (or, later, the Nix backend).

use std::path::Path;

use crate::types::{Backend, ContainerEngine};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HostProfile {
    /// Whether the native backend can run on this host.
    pub native_capable: bool,
    /// Human-readable justification (for messaging / `--backend` errors).
    pub reason: String,
    /// conda platform string, e.g. "linux-64" | "osx-arm64" | "unknown".
    pub platform: String,
}

/// The conda platform string for an (os, arch) pair.
fn conda_platform(os: &str, arch: &str) -> String {
    match (os, arch) {
        ("linux", "x86_64") => "linux-64",
        ("linux", "aarch64") => "linux-aarch64",
        ("macos", "x86_64") => "osx-64",
        ("macos", "aarch64") => "osx-arm64",
        _ => "unknown",
    }
    .to_string()
}

/// The conventional FHS path of the glibc dynamic loader for an architecture,
/// or None for architectures we don't map. Its presence is the direct test for
/// "can a stock conda binary launch here".
fn glibc_loader_path(arch: &str) -> Option<&'static str> {
    match arch {
        "x86_64" => Some("/lib64/ld-linux-x86-64.so.2"),
        "aarch64" => Some("/lib/ld-linux-aarch64.so.1"),
        _ => None,
    }
}

/// Pure classifier: given the observable facts, decide native capability. Kept
/// separate from the filesystem/env reads in `probe_host` so it is unit-testable.
fn classify(os: &str, arch: &str, glibc_loader_present: bool, is_nixos: bool) -> HostProfile {
    let platform = conda_platform(os, arch);
    match os {
        // Apple Silicon only: there is no prebuilt native toolchain for Intel
        // macOS (GitHub dropped free Intel runners, and the Haskell compiler
        // cannot be cross-produced for it), so Intel Macs route to a container.
        "macos" if arch == "aarch64" => HostProfile {
            native_capable: true,
            reason: "macOS on Apple Silicon (native backend supported)".to_string(),
            platform,
        },
        "macos" => HostProfile {
            native_capable: false,
            reason: "Intel macOS has no prebuilt native toolchain (Apple Silicon \
                     only); use a container backend"
                .to_string(),
            platform,
        },
        "linux" => {
            if is_nixos {
                HostProfile {
                    native_capable: false,
                    reason: "NixOS has no standard FHS; use a container backend \
                             (or the Nix backend when available)"
                        .to_string(),
                    platform,
                }
            } else if glibc_loader_present {
                HostProfile {
                    native_capable: true,
                    reason: "glibc + FHS Linux (native backend supported)".to_string(),
                    platform,
                }
            } else {
                HostProfile {
                    native_capable: false,
                    reason: "no standard glibc dynamic loader (musl or non-FHS host); \
                             use a container backend"
                        .to_string(),
                    platform,
                }
            }
        }
        other => HostProfile {
            native_capable: false,
            reason: format!("{other} is not supported for the native backend"),
            platform,
        },
    }
}

/// Probe the current host.
pub fn probe_host() -> HostProfile {
    let os = std::env::consts::OS;
    let arch = std::env::consts::ARCH;
    let is_nixos = Path::new("/etc/NIXOS").exists();
    let glibc_loader_present = glibc_loader_path(arch)
        .map(|p| Path::new(p).exists())
        .unwrap_or(false);
    classify(os, arch, glibc_loader_present, is_nixos)
}

/// The default backend for a host when the user gives no explicit `--backend`:
/// native where viable, otherwise a container under the given fallback engine.
pub fn default_backend(profile: &HostProfile, fallback_engine: ContainerEngine) -> Backend {
    if profile.native_capable {
        Backend::Native
    } else {
        Backend::Container(fallback_engine)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn apple_silicon_macos_is_native_capable() {
        let p = classify("macos", "aarch64", false, false);
        assert!(p.native_capable);
        assert_eq!(p.platform, "osx-arm64");
    }

    #[test]
    fn intel_macos_is_not_native_capable() {
        // No prebuilt native toolchain exists for Intel macOS.
        let p = classify("macos", "x86_64", false, false);
        assert!(!p.native_capable);
        assert_eq!(p.platform, "osx-64");
    }

    #[test]
    fn glibc_fhs_linux_is_native_capable() {
        let p = classify("linux", "x86_64", true, false);
        assert!(p.native_capable);
        assert_eq!(p.platform, "linux-64");
    }

    #[test]
    fn nixos_is_not_native_capable_even_with_loader() {
        // nix-ld can place a loader, but NixOS still routes away from native.
        let p = classify("linux", "x86_64", true, true);
        assert!(!p.native_capable);
        assert!(p.reason.contains("NixOS"));
    }

    #[test]
    fn musl_or_nonfhs_linux_is_not_native_capable() {
        let p = classify("linux", "x86_64", false, false);
        assert!(!p.native_capable);
    }

    #[test]
    fn windows_is_not_native_capable() {
        let p = classify("windows", "x86_64", false, false);
        assert!(!p.native_capable);
        assert_eq!(p.platform, "unknown");
    }

    #[test]
    fn default_backend_picks_native_or_container() {
        let native = HostProfile {
            native_capable: true,
            reason: String::new(),
            platform: "linux-64".to_string(),
        };
        assert_eq!(default_backend(&native, ContainerEngine::Podman), Backend::Native);

        let contained = HostProfile {
            native_capable: false,
            reason: String::new(),
            platform: "linux-64".to_string(),
        };
        assert_eq!(
            default_backend(&contained, ContainerEngine::Docker),
            Backend::Container(ContainerEngine::Docker)
        );
    }
}
