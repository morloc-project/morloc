//! Directories only the running user can enter (model/runtime/network.md NET-4).

use std::io;
use std::os::unix::fs::{DirBuilderExt, MetadataExt};
use std::path::{Path, PathBuf};

/// The per-user runtime directory: `$XDG_RUNTIME_DIR/morloc`, else
/// `/tmp/morloc-<uid>`, created if missing and checked before use.
pub fn runtime_dir() -> io::Result<PathBuf> {
    let dir = match std::env::var_os("XDG_RUNTIME_DIR").filter(|v| !v.is_empty()) {
        Some(base) => PathBuf::from(base).join("morloc"),
        None => PathBuf::from(format!("/tmp/morloc-{}", unsafe { libc::getuid() })),
    };
    ensure_private(&dir)?;
    Ok(dir)
}

/// Create `dir` with mode 0700 if it is missing, then refuse it unless it is
/// a real directory, owned by this user, that no one else may enter.
pub fn ensure_private(dir: &Path) -> io::Result<()> {
    match std::fs::DirBuilder::new().mode(0o700).create(dir) {
        Ok(()) => {}
        Err(e) if e.kind() == io::ErrorKind::AlreadyExists => {}
        Err(e) => return Err(e),
    }
    let md = std::fs::symlink_metadata(dir)?;
    let refuse = |why: &str| Err(io::Error::new(io::ErrorKind::PermissionDenied, format!("{}: {why}", dir.display())));
    if !md.file_type().is_dir() {
        return refuse("not a directory");
    }
    if md.uid() != unsafe { libc::getuid() } {
        return refuse("owned by another user");
    }
    if md.mode() & 0o077 != 0 {
        return refuse("other users may enter it");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::ensure_private;
    use std::os::unix::fs::PermissionsExt;

    fn scratch(name: &str) -> std::path::PathBuf {
        let p = std::env::temp_dir().join(format!("mlc-private-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&p);
        let _ = std::fs::remove_file(&p);
        p
    }

    #[test]
    fn a_missing_directory_is_created_private() {
        let d = scratch("new");
        ensure_private(&d).unwrap();
        assert_eq!(std::fs::metadata(&d).unwrap().permissions().mode() & 0o777, 0o700);
        std::fs::remove_dir(&d).unwrap();
    }

    #[test]
    fn a_directory_others_may_enter_is_refused() {
        let d = scratch("open");
        std::fs::create_dir(&d).unwrap();
        std::fs::set_permissions(&d, std::fs::Permissions::from_mode(0o755)).unwrap();
        assert!(ensure_private(&d).is_err());
        std::fs::remove_dir(&d).unwrap();
    }

    #[test]
    fn a_symlink_is_refused_even_to_a_private_directory() {
        let target = scratch("target");
        ensure_private(&target).unwrap();
        let link = scratch("link");
        std::os::unix::fs::symlink(&target, &link).unwrap();
        assert!(ensure_private(&link).is_err());
        std::fs::remove_file(&link).unwrap();
        std::fs::remove_dir(&target).unwrap();
    }
}
