//! The function declarations in morloc.h, and the `extern "C"` blocks in
//! which morloc-nexus and rustmorloc declare the libmorloc functions they
//! call, are hand-kept copies of the Rust exports; both are checked here
//! against prototypes cbindgen generates from the Rust definitions.
//!
//! morloc.h is compiled together with them: C requires every redeclaration
//! of a function to be compatible, so a drifted argument width, pointer
//! constness or return type fails to compile. The Rust copies are compared
//! at the level the calling convention sees -- arity and the class of each
//! argument and result (a pointer, or an integer of a given width and
//! signedness) -- since those crates name the runtime's structs as opaque
//! `c_void` handles.

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use std::path::Path;
    use std::process::Command;

    /// Rust type names whose morloc.h name does not follow the
    /// `FooBar` -> `foo_bar_t` rule, including aliases defined in the types
    /// crate, which cbindgen does not parse.
    const RENAMES: &[(&str, &str)] = &[
        ("RelPtr", "relptr_t"),
        ("VolPtr", "volptr_t"),
        ("IFileWalkArg", "mlc_ifile_walk_arg"),
        ("CSchema", "Schema"),
        ("ArgumentT", "argument_t"),
        ("FFI_ArrowArray", "struct ArrowArray"),
        ("FFI_ArrowSchema", "struct ArrowSchema"),
        ("FFI_ArrowArrayStream", "struct ArrowArrayStream"),
        ("ShmHeader", "shm_t"),
        ("PacketHeader", "morloc_packet_header_t"),
    ];

    /// `FooBar` -> `foo_bar_t`.
    fn c_name(rust: &str) -> String {
        let mut out = String::new();
        for (i, ch) in rust.chars().enumerate() {
            if ch.is_ascii_uppercase() && i > 0 {
                out.push('_');
            }
            out.push(ch.to_ascii_lowercase());
        }
        out + "_t"
    }

    /// Prototypes cbindgen generates from the runtime's exports, with Rust
    /// struct names mapped to morloc.h's, and the struct names morloc.h does
    /// not declare (left opaque).
    fn generated_prototypes(crate_dir: &Path, header: &str) -> (String, Vec<String>) {
        // First pass: find the struct names the exports use.
        let raw = run_cbindgen(crate_dir, HashMap::new());
        let declared = |name: &str| {
            header.contains(&format!("}} {name};"))
                || header.contains(&format!(" {name};\n"))
                || header.contains(&format!("struct {name} "))
                || header.contains(&format!("struct {name}\n"))
        };
        let mut rename = HashMap::new();
        let mut opaque = Vec::new();
        for name in struct_names(&raw) {
            if let Some((_, to)) = RENAMES.iter().find(|(from, _)| *from == name) {
                rename.insert(name.clone(), to.to_string());
            } else if declared(&c_name(&name)) {
                rename.insert(name.clone(), c_name(&name));
            } else if !declared(&name) {
                opaque.push(name);
            }
        }
        (run_cbindgen(crate_dir, rename), opaque)
    }

    fn run_cbindgen(crate_dir: &Path, rename: HashMap<String, String>) -> String {
        let config = cbindgen::Config {
            language: cbindgen::Language::C,
            no_includes: true,
            usize_is_size_t: true,
            documentation: false,
            style: cbindgen::Style::Type,
            export: cbindgen::ExportConfig {
                item_types: vec![cbindgen::ItemType::Functions, cbindgen::ItemType::Typedefs],
                rename,
                ..Default::default()
            },
            ..Default::default()
        };
        let bindings = cbindgen::Builder::new()
            .with_crate(crate_dir)
            .with_config(config)
            .generate()
            .expect("cbindgen parses the runtime crate");
        let mut out = Vec::new();
        bindings.write(&mut out);
        String::from_utf8(out).unwrap()
    }

    /// Capitalized identifiers in `text` that cbindgen did not define as
    /// typedefs: the Rust structs the prototypes name.
    fn struct_names(text: &str) -> Vec<String> {
        // A typedef's name is the last identifier of a plain typedef, or the
        // one inside `(*Name)` of a function-pointer typedef.
        let typedefs: Vec<&str> = text
            .lines()
            .filter_map(|l| l.strip_prefix("typedef "))
            .filter_map(|l| match l.find("(*") {
                Some(i) => l[i + 2..].split(')').next(),
                None => l.trim_end_matches(';').rsplit([' ', '*']).find(|w| !w.is_empty()),
            })
            .collect();
        let mut names: Vec<String> = Vec::new();
        for word in text.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_')) {
            let is_type = word.chars().next().is_some_and(|c| c.is_ascii_uppercase())
                && word.chars().any(|c| c.is_ascii_lowercase());
            if is_type && !typedefs.contains(&word) && !names.iter().any(|n| n == word) {
                names.push(word.to_string());
            }
        }
        names
    }

    /// A Rust type from an `extern "C"` declaration, spelled in C, or `None`
    /// for a shape this translation does not cover.
    fn c_type(rust: &str, rename: &dyn Fn(&str) -> String) -> Option<String> {
        let t = rust.trim();
        if let Some(inner) = t.strip_prefix("*const ") {
            let inner = c_type(inner, rename)?;
            // `const T*`, or `T* const*`-style for a pointer pointee.
            return Some(if inner.ends_with('*') { format!("{inner} const*") } else { format!("const {inner}*") });
        }
        if let Some(inner) = t.strip_prefix("*mut ") {
            return Some(format!("{}*", c_type(inner, rename)?));
        }
        // A function pointer, optional or not, is a pointer to the ABI.
        if t.contains("fn(") {
            return Some("void*".to_string());
        }
        if t.contains('(') || t.contains('<') || t.contains('[') {
            return None;
        }
        let last = t.rsplit("::").next().unwrap_or(t);
        Some(match last {
            "u8" => "uint8_t", "i8" => "int8_t", "u16" => "uint16_t", "i16" => "int16_t",
            "u32" => "uint32_t", "i32" => "int32_t", "u64" => "uint64_t", "i64" => "int64_t",
            "usize" | "size_t" => "size_t", "isize" | "ssize_t" => "intptr_t",
            "f32" => "float", "f64" => "double", "bool" => "bool",
            "c_char" => "char", "c_void" => "void", "c_int" => "int", "c_uint" => "unsigned int",
            "c_long" => "long", "c_ulong" => "unsigned long",
            other => return Some(rename(other)),
        }.to_string())
    }

    /// The `(name, prototype)` pairs of every function declared in an
    /// `extern "C"` block of the Rust sources under `dir`, and the
    /// declarations that could not be translated.
    fn foreign_prototypes(dir: &Path, rename: &dyn Fn(&str) -> String) -> (Vec<(String, String)>, Vec<String>) {
        let mut out = Vec::new();
        let mut skipped = Vec::new();
        let mut files = Vec::new();
        let mut stack = vec![dir.to_path_buf()];
        while let Some(d) = stack.pop() {
            for e in std::fs::read_dir(&d).unwrap().flatten() {
                let p = e.path();
                if p.is_dir() && !p.ends_with("target") {
                    stack.push(p);
                } else if p.extension().is_some_and(|x| x == "rs") {
                    files.push(p);
                }
            }
        }
        for f in files {
            let src = std::fs::read_to_string(&f).unwrap();
            let src: String = src
                .lines()
                .map(|l| l.split("//").next().unwrap_or(""))
                .collect::<Vec<_>>()
                .join("\n");
            let mut rest = src.as_str();
            while let Some(i) = rest.find("extern \"C\" {") {
                let body_start = i + "extern \"C\" {".len();
                let mut depth = 1;
                let mut end = body_start;
                for (k, c) in rest[body_start..].char_indices() {
                    match c {
                        '{' => depth += 1,
                        '}' => {
                            depth -= 1;
                            if depth == 0 {
                                end = body_start + k;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
                for decl in rest[body_start..end].split(';') {
                    let d = decl.split_whitespace().collect::<Vec<_>>().join(" ");
                    let Some(fi) = d.find("fn ") else { continue };
                    let d = &d[fi + 3..];
                    let (Some(lp), Some(rp)) = (d.find('('), d.rfind(')')) else { continue };
                    let name = d[..lp].trim().to_string();
                    let ret = d[rp + 1..].trim().strip_prefix("->").map(str::trim);
                    let params: Vec<&str> = d[lp + 1..rp].split(',').map(str::trim).filter(|p| !p.is_empty()).collect();
                    let mut cparams = Vec::new();
                    let mut ok = true;
                    for p in &params {
                        match p.split_once(':').and_then(|(_, t)| c_type(t, rename)) {
                            Some(t) => cparams.push(t),
                            None => ok = false,
                        }
                    }
                    let cret = match ret {
                        None => Some("void".to_string()),
                        Some(r) => c_type(r, rename),
                    };
                    match (ok, cret) {
                        (true, Some(r)) => {
                            let args = if cparams.is_empty() { "void".to_string() } else { cparams.join(", ") };
                            out.push((name.clone(), format!("{r} {name}({args});")));
                        }
                        _ => skipped.push(format!("{}: {}", f.display(), name)),
                    }
                }
                rest = &rest[end..];
            }
        }
        (out, skipped)
    }

    /// Each function prototype in C `text`, as its result and argument
    /// classes: `ptr` for any pointer, else the scalar's width and
    /// signedness.
    fn abi_signatures(text: &str) -> HashMap<String, Vec<String>> {
        // Names of function-pointer typedefs, which pass as pointers.
        let fn_ptrs: Vec<&str> = text
            .lines()
            .filter(|l| l.starts_with("typedef "))
            .filter_map(|l| l.find("(*").map(|i| &l[i + 2..]))
            .filter_map(|l| l.split(')').next())
            .collect();
        let class = |t: &str| -> String {
            if t.contains('*') || fn_ptrs.contains(&t.trim()) {
                return "ptr".into();
            }
            let t = t.replace("const ", "").replace("struct ", "");
            let t = t.split_whitespace().collect::<Vec<_>>().join(" ");
            match t.as_str() {
                "uint8_t" | "unsigned char" => "u8",
                "int8_t" | "char" | "signed char" => "i8",
                "uint16_t" => "u16", "int16_t" => "i16",
                "uint32_t" | "unsigned int" => "u32", "int32_t" | "int" => "i32",
                "uint64_t" | "size_t" | "unsigned long" => "u64",
                "int64_t" | "intptr_t" | "ssize_t" | "long" | "relptr_t" | "volptr_t" => "i64",
                "float" => "f32", "double" => "f64", "bool" | "_Bool" => "bool", "void" => "void",
                other => other,
            }
            .to_string()
        };
        let mut out = HashMap::new();
        let flat = text.split_whitespace().collect::<Vec<_>>().join(" ");
        for item in flat.split(';') {
            let item = item.trim();
            if item.starts_with("typedef") || item.starts_with('#') {
                continue;
            }
            let (Some(lp), Some(rp)) = (item.find('('), item.rfind(')')) else { continue };
            let head = &item[..lp];
            let Some(name) = head.rsplit([' ', '*']).find(|w| !w.is_empty()) else { continue };
            let ret = head[..head.len() - name.len()].trim();
            let mut sig = vec![class(ret)];
            for p in item[lp + 1..rp].split(',') {
                let p = p.trim();
                if p.is_empty() || p == "void" {
                    continue;
                }
                // Drop a trailing parameter name: the last word, when the
                // part before it still names a type.
                let ty = match p.rsplit_once([' ', '*']) {
                    Some((before, last))
                        if !before.trim().is_empty()
                            && last.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
                            && !matches!(last, "char" | "int" | "long" | "void" | "_Bool" | "bool") =>
                    {
                        if p.as_bytes()[before.len()] == b'*' { format!("{before}*") } else { before.to_string() }
                    }
                    _ => p.to_string(),
                };
                sig.push(class(&ty));
            }
            out.insert(name.to_string(), sig);
        }
        out
    }

    #[test]
    fn morloc_h_agrees_with_the_rust_exports() {
        let crate_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let header_dir = crate_dir.join("../../morloc");
        let header = std::fs::read_to_string(header_dir.join("morloc.h")).unwrap();
        let (prototypes, opaque) = generated_prototypes(crate_dir, &header);

        let mut tu = String::from("#include \"morloc.h\"\n");
        for name in &opaque {
            tu.push_str(&format!("typedef struct {name} {name};\n"));
        }
        tu.push_str(&prototypes);

        // The hand-written declarations in the crates that link libmorloc.
        // Only libmorloc's own exports are checked; other foreign functions
        // (libc, zstd) have no definition here to agree with.
        // A struct passed by value is compared under its C name, as the
        // definitions spell it.
        let rename = |name: &str| {
            RENAMES.iter().find(|(r, _)| *r == name).map_or_else(|| c_name(name), |(_, c)| c.to_string())
        };
        let definitions = abi_signatures(&prototypes);
        let mut checked = 0;
        let mut disagreements = Vec::new();
        for krate in ["../morloc-nexus/src", "../rustmorloc/src"] {
            let (decls, skipped) = foreign_prototypes(&crate_dir.join(krate), &rename);
            for (name, proto) in decls {
                let Some(def) = definitions.get(&name) else { continue };
                let copy = abi_signatures(&proto).remove(&name).unwrap();
                if &copy != def {
                    disagreements.push(format!("{krate}: {name}: declared {copy:?}, defined {def:?}"));
                }
                checked += 1;
            }
            for s in skipped {
                if definitions.keys().any(|k| s.ends_with(&format!(": {k}"))) {
                    disagreements.push(format!("{s}: declaration not translated; extend c_type"));
                }
            }
        }
        assert!(disagreements.is_empty(), "declarations disagree with libmorloc:\n{}", disagreements.join("\n"));
        assert!(checked > 0, "no foreign declarations were found to check");

        let dir = std::env::temp_dir().join(format!("morloc_abi_check_{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let tu_path = dir.join("abi_check.c");
        std::fs::write(&tu_path, &tu).unwrap();
        let out = Command::new("cc")
            .args(["-fsyntax-only", "-std=gnu11"])
            .arg("-I")
            .arg(&header_dir)
            .arg(&tu_path)
            .output()
            .expect("a C compiler (cc) is required for this test");
        let _ = std::fs::remove_dir_all(&dir);
        assert!(
            out.status.success(),
            "morloc.h disagrees with the Rust exports:\n{}",
            String::from_utf8_lossy(&out.stderr)
        );
    }
}
