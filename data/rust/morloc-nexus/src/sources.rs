//! Refusal to start a pool whose sourced files changed after the build (SRC-8).

use morloc_runtime_types::hash::xxh64;

use crate::manifest::SourceFile;

/// Check that every file a pool sources still holds the contents it was
/// built from.
pub fn verify(sources: &[SourceFile]) -> Result<(), String> {
    for source in sources {
        let data = std::fs::read(&source.path).map_err(|e| {
            format!(
                "cannot read sourced file {}: {}; rebuild the program with `morloc make`",
                source.path, e
            )
        })?;
        if format!("{:016x}", xxh64(&data)) != source.hash {
            return Err(format!(
                "sourced file {} has changed since the program was built; rebuild the program with `morloc make`",
                source.path
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(path: &std::path::Path, contents: &[u8]) -> SourceFile {
        SourceFile {
            path: path.to_string_lossy().into_owned(),
            hash: format!("{:016x}", xxh64(contents)),
        }
    }

    #[test]
    fn unchanged_file_passes() {
        let dir = std::env::temp_dir().join(format!("mlc-src8-a-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("f.py");
        std::fs::write(&path, b"x = 1\n").unwrap();
        assert!(verify(&[entry(&path, b"x = 1\n")]).is_ok());
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn edited_file_is_refused_by_name() {
        let dir = std::env::temp_dir().join(format!("mlc-src8-b-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("f.py");
        std::fs::write(&path, b"x = 2\n").unwrap();
        let err = verify(&[entry(&path, b"x = 1\n")]).unwrap_err();
        assert!(err.contains(&*path.to_string_lossy()) && err.contains("changed"), "{}", err);
        std::fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn missing_file_is_refused_by_name() {
        let path = std::env::temp_dir().join(format!("mlc-src8-c-{}/f.py", std::process::id()));
        let err = verify(&[entry(&path, b"x = 1\n")]).unwrap_err();
        assert!(err.contains(&*path.to_string_lossy()) && err.contains("cannot read"), "{}", err);
    }
}
