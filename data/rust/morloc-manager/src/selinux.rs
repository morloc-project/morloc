use std::path::Path;
use std::process::Command;

use crate::error::{ManagerError, Result};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SELinuxMode {
    Enforcing,
    Permissive,
    Disabled,
}

pub fn detect_selinux() -> SELinuxMode {
    if !Path::new("/usr/sbin/getenforce").exists() {
        return SELinuxMode::Disabled;
    }
    let Ok(output) = Command::new("getenforce").output() else {
        return SELinuxMode::Disabled;
    };
    if !output.status.success() {
        return SELinuxMode::Disabled;
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    let first_line = stdout.lines().next().unwrap_or("");
    match first_line {
        "Enforcing" => SELinuxMode::Enforcing,
        "Permissive" => SELinuxMode::Permissive,
        _ => SELinuxMode::Disabled,
    }
}

pub fn volume_suffix(mode: SELinuxMode) -> &'static str {
    match mode {
        SELinuxMode::Enforcing => ":z",
        SELinuxMode::Permissive | SELinuxMode::Disabled => "",
    }
}

pub fn is_safe_to_relabel(path: &str) -> bool {
    let home = dirs::home_dir().unwrap_or_default();
    let norm = normalize(path);
    let home_norm = normalize_trailing(&home.to_string_lossy());
    let is_home_root = normalize_trailing(&norm) == home_norm;
    !is_unsafe_system_path(&norm) && !is_home_root
}

pub fn validate_mount_path(path: &str) -> Result<()> {
    if is_safe_to_relabel(path) {
        Ok(())
    } else {
        Err(ManagerError::SELinuxError(format!(
            "Cannot bind-mount {path} with SELinux relabeling. \
             This path is unsafe to relabel. \
             Use a subdirectory instead (e.g., {path}/project/)."
        )))
    }
}

fn is_unsafe_system_path(p: &str) -> bool {
    let norm = normalize_trailing(p);
    norm == "/" || norm.starts_with("/tmp/") || norm == "/tmp/" || norm.starts_with("/var/tmp/") || norm == "/var/tmp/"
}

fn normalize(p: &str) -> String {
    // Simple normalization: resolve . and remove trailing /
    let path = Path::new(p);
    path.to_string_lossy().to_string()
}

fn normalize_trailing(p: &str) -> String {
    let mut s = normalize(p);
    if !s.ends_with('/') {
        s.push('/');
    }
    s
}
