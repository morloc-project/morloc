//! The PATH a program's pools start with (PKG-17).

use std::ffi::OsString;
use std::path::{Path, PathBuf};

/// `path` with `prefix/bin` first, and nowhere else.
pub(crate) fn with_env_bin_first(path: Option<OsString>, prefix: &Path) -> Option<OsString> {
    let bin = prefix.join("bin");
    let rest: Vec<PathBuf> = path
        .map(|p| std::env::split_paths(&p).filter(|d| d != &bin).collect())
        .unwrap_or_default();
    std::env::join_paths(std::iter::once(bin).chain(rest)).ok()
}

#[cfg(test)]
mod tests {
    use super::with_env_bin_first;
    use std::ffi::OsString;
    use std::path::Path;

    #[test]
    fn the_environment_bin_comes_first_exactly_once() {
        let got = with_env_bin_first(Some(OsString::from("/usr/bin:/p/bin:/bin")), Path::new("/p"));
        assert_eq!(got, Some(OsString::from("/p/bin:/usr/bin:/bin")));
    }

    #[test]
    fn an_unset_path_becomes_the_environment_bin() {
        assert_eq!(with_env_bin_first(None, Path::new("/p")), Some(OsString::from("/p/bin")));
    }

    #[test]
    fn a_prefix_holding_a_separator_is_refused() {
        assert_eq!(with_env_bin_first(Some(OsString::from("/usr/bin")), Path::new("/a:b")), None);
    }
}
