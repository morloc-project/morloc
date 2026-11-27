// LIFE-6: the build id is a hash of the runtime source, so the same source
// gives the same id wherever it is built (the workspace or its persisted copy).
// Only Rust sources, the Cargo files and the C header count, so editor files
// do not.

pub const SOURCE_CRATES: &[&str] = &["morloc-runtime-types", "morloc-runtime", "morloc-manifest"];

pub const HEADER: &str = "include/morloc.h";

fn walk(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for e in entries.flatten() {
        let p = e.path();
        if p.is_dir() {
            walk(&p, out);
        } else if p.extension().is_some_and(|x| x == "rs") {
            out.push(p);
        }
    }
}

fn fnv(h: &mut u64, bytes: &[u8]) {
    for b in bytes {
        *h ^= *b as u64;
        *h = h.wrapping_mul(0x100000001b3);
    }
}

pub fn source_id(root: &std::path::Path) -> u64 {
    let mut all = vec![root.join("Cargo.toml"), root.join("Cargo.lock"), root.join(HEADER)];
    for c in SOURCE_CRATES {
        let dir = root.join(c);
        all.push(dir.join("Cargo.toml"));
        all.push(dir.join("build.rs"));
        all.push(dir.join("build_id.rs"));
        walk(&dir.join("src"), &mut all);
    }
    let mut named: Vec<(String, std::path::PathBuf)> = all
        .into_iter()
        .filter(|p| p.is_file())
        .map(|p| (p.strip_prefix(root).unwrap().to_string_lossy().replace('\\', "/"), p))
        .collect();
    named.sort();
    let mut h: u64 = 0xcbf29ce484222325;
    for (name, path) in &named {
        let body = std::fs::read(path).unwrap();
        fnv(&mut h, name.as_bytes());
        fnv(&mut h, &[0]);
        fnv(&mut h, &(body.len() as u64).to_le_bytes());
        fnv(&mut h, &body);
    }
    h
}
