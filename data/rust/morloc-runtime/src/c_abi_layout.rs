//! Guard that `data/morloc/morloc.h` describes the same memory layout as the
//! `#[repr(C)]` mirrors in this crate.
//!
//! `libmorloc.so` hands out pointers to the structs defined in
//! [`crate::manifest_ffi`], and the header is what a C or C++ consumer
//! compiles against to read them. The two are written by hand in different
//! languages with nothing tying them together, so a field added on one side
//! and forgotten on the other is invisible: the header still compiles, the
//! library still links, and every read past the first divergence returns
//! whatever bytes happen to sit at the wrong offset.
//!
//! The test below closes that gap by parsing the header, computing the layout
//! a C compiler would give each struct, and comparing it against the layout
//! Rust gives the mirror. Field names, field order, per-field offsets,
//! per-field sizes, the field count, and the struct's total size and alignment
//! are all compared, which covers each way the two can drift apart:
//!
//!   * a field added to the Rust struct alone changes its size
//!   * a field added to the header alone changes the field count
//!   * a reordering moves names, and usually offsets
//!   * a retyping changes that field's size, and usually the offsets after it
//!
//! The last two need both checks. Two same-typed neighbours can be swapped
//! without moving any offset, so names are compared; and a narrowing retype
//! can be swallowed whole by the padding in front of the next field, leaving
//! offsets and total size untouched, so per-field sizes are compared.
//!
//! Where C reserves the name the Rust field uses (`type`, `short`, `long`,
//! `default`), the header spells it differently and the expectation records
//! both names.

#[cfg(test)]
mod tests {
    use crate::manifest_ffi::*;
    use std::mem::{align_of, offset_of, size_of};

    const HEADER: &str = include_str!("../../../morloc/morloc.h");

    /// Strip `//` line comments; the header uses no block comments inside a
    /// struct body.
    fn strip_comments(src: &str) -> String {
        src.lines()
            .map(|l| match l.find("//") {
                Some(i) => &l[..i],
                None => l,
            })
            .collect::<Vec<_>>()
            .join("\n")
    }

    /// The `{ ... }` body of an aggregate, given the byte offset of its
    /// opening brace.
    fn brace_body(src: &str, open: usize) -> &str {
        let bytes = src.as_bytes();
        assert_eq!(bytes[open], b'{');
        let mut depth = 0usize;
        for (i, b) in bytes.iter().enumerate().skip(open) {
            match b {
                b'{' => depth += 1,
                b'}' => {
                    depth -= 1;
                    if depth == 0 {
                        return &src[open + 1..i];
                    }
                }
                _ => {}
            }
        }
        panic!("unterminated aggregate body in morloc.h at byte {open}");
    }

    /// Body of `typedef struct [tag] { ... } <name>;` or
    /// `typedef union { ... } <name>;`, located by its trailing name.
    fn body_of_typedef(src: &str, name: &str) -> String {
        let close = src
            .find(&format!("}} {name};"))
            .unwrap_or_else(|| panic!("morloc.h declares no typedef named `{name}`"));
        // Walk back to the brace matching this closer.
        let bytes = src.as_bytes();
        let mut depth = 0usize;
        for i in (0..=close).rev() {
            match bytes[i] {
                b'}' => depth += 1,
                b'{' => {
                    depth -= 1;
                    if depth == 0 {
                        return brace_body(src, i).to_string();
                    }
                }
                _ => {}
            }
        }
        panic!("unbalanced braces before `}} {name};` in morloc.h");
    }

    /// Body of `struct <tag> { ... };`, located by its tag.
    fn body_of_struct_tag(src: &str, tag: &str) -> String {
        let decl = format!("struct {tag} {{");
        let at = src
            .find(&decl)
            .unwrap_or_else(|| panic!("morloc.h declares no `struct {tag}`"));
        let open = at + decl.len() - 1;
        brace_body(src, open).to_string()
    }

    /// Whether an aggregate is laid out as a struct or a union.
    #[derive(PartialEq, Clone, Copy)]
    enum Kind {
        Struct,
        Union,
    }

    /// Locate an aggregate by name, accepting either spelling, and report
    /// which keyword introduced it. The keyword decides the layout rule, so
    /// it is read from the declaration rather than guessed from the name.
    fn aggregate_of(name: &str) -> (Kind, String) {
        let src = strip_comments(HEADER);
        let (open, body) = if src.contains(&format!("}} {name};")) {
            let body = body_of_typedef(&src, name);
            let open = src.find(&body).expect("body came from this source");
            (open, body)
        } else {
            let decl = format!("struct {name} {{");
            let at = src
                .find(&decl)
                .unwrap_or_else(|| panic!("morloc.h declares no `struct {name}`"));
            (at + decl.len(), body_of_struct_tag(&src, name))
        };
        // The keyword sits just before the opening brace of this body.
        let head = &src[..open];
        let kind = match (head.rfind("union"), head.rfind("struct")) {
            (Some(u), Some(st)) if u > st => Kind::Union,
            (Some(_), None) => Kind::Union,
            _ => Kind::Struct,
        };
        (kind, body)
    }

    fn body_of(name: &str) -> String {
        aggregate_of(name).1
    }

    /// Size and alignment of a C type as written in a field declaration.
    fn size_align(ty: &str) -> (usize, usize) {
        let ty = ty.trim();
        if ty.ends_with('*') {
            return (8, 8);
        }
        match ty {
            "bool" | "char" | "uint8_t" | "int8_t" => (1, 1),
            "uint16_t" | "int16_t" => (2, 2),
            "int" | "int32_t" | "uint32_t" | "float" => (4, 4),
            "size_t" | "uint64_t" | "int64_t" | "double" => (8, 8),
            // Every enum in the header uses the default underlying type,
            // which is what `#[repr(C)]` gives a fieldless Rust enum.
            "manifest_arg_kind_t"
            | "morloc_expression_type"
            | "morloc_app_expression_type"
            | "morloc_pattern_type" => (4, 4),
            // A composite embedded by value: measure it from the header too.
            _ => match aggregate_of(ty) {
                (Kind::Union, body) => union_layout(&body),
                (Kind::Struct, body) => {
                    let l = struct_layout(&body);
                    (l.size, l.align)
                }
            },
        }
    }

    /// Split an aggregate body into its top-level field declarations.
    /// A nested anonymous `union { .. } name;` comes back whole.
    fn declarations(body: &str) -> Vec<String> {
        let mut out = Vec::new();
        let mut depth = 0usize;
        let mut cur = String::new();
        for ch in body.chars() {
            match ch {
                '{' => {
                    depth += 1;
                    cur.push(ch);
                }
                '}' => {
                    depth -= 1;
                    cur.push(ch);
                }
                ';' if depth == 0 => {
                    if !cur.trim().is_empty() {
                        out.push(cur.trim().to_string());
                    }
                    cur.clear();
                }
                _ => cur.push(ch),
            }
        }
        assert!(cur.trim().is_empty(), "trailing text in aggregate body");
        out
    }

    /// The (type, name) of one field declaration. A pointer's stars bind to
    /// the type, so `char** desc` and `char **desc` both read as `char**`.
    fn split_decl(decl: &str) -> (String, String) {
        if let Some(open) = decl.find('{') {
            // Anonymous nested union: `union { .. } fieldname`.
            let body = brace_body(decl, open);
            let name = decl[decl.rfind('}').unwrap() + 1..].trim().to_string();
            let (size, align) = union_layout(body);
            return (format!("__anon:{size}:{align}"), name);
        }
        let decl = decl.trim();
        let split = decl
            .rfind(|c: char| c.is_whitespace() || c == '*')
            .expect("field declaration must have a type and a name");
        let (ty, name) = decl.split_at(split + 1);
        (ty.trim().to_string(), name.trim().to_string())
    }

    fn size_align_of_decl(ty: &str) -> (usize, usize) {
        if let Some(rest) = ty.strip_prefix("__anon:") {
            let mut it = rest.split(':');
            let size = it.next().unwrap().parse().unwrap();
            let align = it.next().unwrap().parse().unwrap();
            return (size, align);
        }
        size_align(ty)
    }

    struct Layout {
        /// (name, byte offset, byte size) per field, in declaration order.
        fields: Vec<(String, usize, usize)>,
        size: usize,
        align: usize,
    }

    fn struct_layout(body: &str) -> Layout {
        let mut fields = Vec::new();
        let mut offset = 0usize;
        let mut align = 1usize;
        for decl in declarations(body) {
            let (ty, name) = split_decl(&decl);
            let (fsize, falign) = size_align_of_decl(&ty);
            offset = offset.div_ceil(falign) * falign;
            fields.push((name, offset, fsize));
            offset += fsize;
            align = align.max(falign);
        }
        Layout {
            fields,
            size: offset.div_ceil(align) * align,
            align,
        }
    }

    fn union_layout(body: &str) -> (usize, usize) {
        let mut size = 0usize;
        let mut align = 1usize;
        for decl in declarations(body) {
            let (ty, _) = split_decl(&decl);
            let (fsize, falign) = size_align_of_decl(&ty);
            size = size.max(fsize);
            align = align.max(falign);
        }
        (size.div_ceil(align) * align, align)
    }

    /// The byte size of one field of a `#[repr(C)]` struct, without
    /// constructing or reading the struct. `addr_of!` on an uninitialized
    /// place computes an address only; nothing is dereferenced.
    macro_rules! rust_field_size {
        ($t:ty, $f:tt) => {{
            fn size_of_pointee<T>(_: *const T) -> usize {
                size_of::<T>()
            }
            let uninit = std::mem::MaybeUninit::<$t>::uninit();
            let base = uninit.as_ptr();
            size_of_pointee(unsafe { std::ptr::addr_of!((*base).$f) })
        }};
    }

    /// Assert that a C aggregate and its Rust mirror agree field for field.
    ///
    /// Each entry is `rust_field => "c_field_name"`. The two names differ only
    /// where C reserves the Rust spelling.
    ///
    /// Sizes are compared per field as well as in total, because a narrowing
    /// retype can be swallowed by the padding in front of the next field: the
    /// offsets and the struct size both stay put while the field silently
    /// carries fewer bytes than the other side writes.
    macro_rules! assert_same_layout {
        ($rust:ty, $c:literal, [ $( $field:tt => $cname:literal ),* $(,)? ]) => {{
            let layout = struct_layout(&body_of($c));
            let mut n = 0usize;
            $(
                let (ref got_name, got_offset, got_size) = layout.fields[n];
                assert_eq!(
                    got_name, $cname,
                    "{}: field {} is `{}` in morloc.h, expected `{}`",
                    $c, n, got_name, $cname
                );
                assert_eq!(
                    got_offset,
                    offset_of!($rust, $field),
                    "{}: field `{}` sits at byte {} in morloc.h but {} in Rust",
                    $c, $cname, got_offset, offset_of!($rust, $field)
                );
                assert_eq!(
                    got_size,
                    rust_field_size!($rust, $field),
                    "{}: field `{}` is {} bytes in morloc.h but {} in Rust",
                    $c, $cname, got_size, rust_field_size!($rust, $field)
                );
                n += 1;
            )*
            assert_eq!(
                layout.fields.len(), n,
                "{}: morloc.h declares {} fields, the Rust mirror {}; extra: {:?}",
                $c, layout.fields.len(), n,
                &layout.fields[n.min(layout.fields.len())..]
            );
            assert_eq!(
                layout.size, size_of::<$rust>(),
                "{}: sizeof is {} in morloc.h but {} in Rust",
                $c, layout.size, size_of::<$rust>()
            );
            assert_eq!(
                layout.align, align_of::<$rust>(),
                "{}: alignment is {} in morloc.h but {} in Rust",
                $c, layout.align, align_of::<$rust>()
            );
        }};
    }

    #[test]
    fn manifest_structs_match_the_header() {
        assert_same_layout!(ManifestBuild, "manifest_build_t", [
            path => "path",
            time => "time",
            morloc_version => "morloc_version",
        ]);
        assert_same_layout!(ManifestConstraint, "manifest_constraint_t", [
            ctype => "ctype",
            value_json => "value_json",
        ]);
        assert_same_layout!(ManifestPool, "manifest_pool_t", [
            lang => "lang",
            exec => "exec",
            socket => "socket",
            metadata_json => "metadata_json",
            allow_string_null => "allow_string_null",
        ]);
        assert_same_layout!(ManifestGrpEntry, "manifest_grp_entry_t", [
            key => "key",
            arg => "arg",
        ]);
        assert_same_layout!(ManifestArg, "manifest_arg_s", [
            kind => "kind",
            schema => "schema",
            type_desc => "type_desc",
            metavar => "metavar",
            quoted => "quoted",
            short_opt => "short_opt",
            long_opt => "long_opt",
            long_rev => "long_rev",
            short_rev => "short_rev",
            default_val => "default_val",
            desc => "desc",
            n_desc => "n_desc",
            constraints => "constraints",
            n_constraints => "n_constraints",
            grp_short => "grp_short",
            grp_long => "grp_long",
            entries => "entries",
            n_entries => "n_entries",
            metadata_json => "metadata_json",
        ]);
        assert_same_layout!(ManifestReturn, "manifest_return_t", [
            schema => "schema",
            type_desc => "type_desc",
            desc => "desc",
            n_desc => "n_desc",
            constraints => "constraints",
            n_constraints => "n_constraints",
            metadata_json => "metadata_json",
            mime => "mime",
        ]);
        assert_same_layout!(ManifestCmdGroup, "manifest_cmd_group_t", [
            name => "name",
            desc => "desc",
            n_desc => "n_desc",
            metadata_json => "metadata_json",
        ]);
        assert_same_layout!(ManifestTerminal, "manifest_terminal_t", [
            short => "short_flag",
            long => "long_flag",
            entry => "entry",
            description => "description",
            render => "render",
            default => "is_default",
        ]);
        assert_same_layout!(ManifestCommand, "manifest_command_t", [
            name => "name",
            is_pure => "is_pure",
            mid => "mid",
            pool_index => "pool_index",
            needed_pools => "needed_pools",
            n_needed_pools => "n_needed_pools",
            desc => "desc",
            n_desc => "n_desc",
            args => "args",
            n_args => "n_args",
            ret => "ret",
            constraints => "constraints",
            n_constraints => "n_constraints",
            expr => "expr",
            group => "group",
            metadata_json => "metadata_json",
            terminals => "terminals",
            n_terminals => "n_terminals",
            internal => "internal",
        ]);
        assert_same_layout!(ManifestService, "manifest_service_t", [
            stype => "stype",
            host => "host",
            port => "port",
            socket => "socket",
            metadata_json => "metadata_json",
        ]);
        assert_same_layout!(Manifest, "manifest_t", [
            name => "name",
            build => "build",
            pools => "pools",
            n_pools => "n_pools",
            commands => "commands",
            n_commands => "n_commands",
            groups => "groups",
            n_groups => "n_groups",
            service => "service",
            metadata_json => "metadata_json",
            unsafe_skip_null_check => "unsafe_skip_null_check",
        ]);
    }

    #[test]
    fn expression_structs_match_the_header() {
        assert_same_layout!(MorlocString, "morloc_string_s", [
            data => "data",
            size => "size",
        ]);
        assert_same_layout!(MorlocDataArray, "morloc_data_array_s", [
            schema => "schema",
            size => "size",
            values => "values",
        ]);
        assert_same_layout!(MorlocData, "morloc_data_s", [
            is_voidstar => "is_voidstar",
            data => "data",
        ]);
        assert_same_layout!(MorlocAppExpression, "morloc_app_expression_s", [
            atype => "type",
            function => "function",
            args => "args",
            nargs => "nargs",
        ]);
        assert_same_layout!(MorlocLamExpression, "morloc_lam_expression_s", [
            nargs => "nargs",
            args => "args",
            body => "body",
        ]);
        assert_same_layout!(MorlocPattern, "morloc_pattern_s", [
            ptype => "type",
            size => "size",
            fields => "fields",
            selectors => "selectors",
        ]);
        assert_same_layout!(MorlocSaveExpression, "morloc_save_expression_s", [
            format => "format",
            level => "level",
            value => "value",
            path => "path",
        ]);
        assert_same_layout!(MorlocMapExpression, "morloc_map_expression_s", [
            func => "func",
            list => "list",
        ]);
        assert_same_layout!(MorlocCatchExpression, "morloc_catch_expression_s", [
            fallible => "fallible",
            fallback => "fallback",
        ]);
        assert_same_layout!(MorlocIfExpression, "morloc_if_expression_s", [
            cond => "cond",
            then_branch => "then_branch",
            else_branch => "else_branch",
        ]);
        assert_same_layout!(MorlocOpenExpression, "morloc_open_expression_s", [
            kind => "kind",
            path => "path",
        ]);
        assert_same_layout!(MorlocIFileWalkExpression, "morloc_ifile_walk_expression_s", [
            handle => "handle",
            path => "path",
            args => "args",
            n_args => "n_args",
        ]);
        assert_same_layout!(MorlocExpression, "morloc_expression_s", [
            etype => "type",
            schema => "schema",
            expr => "expr",
        ]);
    }

    /// An enum's variants are the values that cross the boundary, so the two
    /// lists must agree in order and length. A C consumer that switches on a
    /// discriminant the header never named cannot handle it.
    fn header_enum_variants(typedef_name: &str) -> Vec<String> {
        let src = strip_comments(HEADER);
        body_of_typedef(&src, typedef_name)
            .split(',')
            .map(|v| v.trim().to_string())
            .filter(|v| !v.is_empty())
            .collect()
    }

    #[test]
    fn expression_enums_match_the_header() {
        let expr = header_enum_variants("morloc_expression_type");
        assert_eq!(
            expr.len(),
            MorlocExpressionType::StreamLayout as usize + 1,
            "morloc.h names {} expression types, the Rust enum has {}: {:?}",
            expr.len(),
            MorlocExpressionType::StreamLayout as usize + 1,
            expr
        );
        assert_eq!(expr.first().map(String::as_str), Some("MORLOC_X_DAT"));
        assert_eq!(
            expr.last().map(String::as_str),
            Some("MORLOC_X_STREAM_LAYOUT")
        );

        let pat = header_enum_variants("morloc_pattern_type");
        assert_eq!(
            pat.len(),
            MorlocPatternType::BracketSlice as usize + 1,
            "morloc.h names {} pattern types, the Rust enum has {}: {:?}",
            pat.len(),
            MorlocPatternType::BracketSlice as usize + 1,
            pat
        );

        let app = header_enum_variants("morloc_app_expression_type");
        assert_eq!(
            app.len(),
            MorlocAppExpressionType::Format as usize + 1,
            "morloc.h names {} application types, the Rust enum has {}: {:?}",
            app.len(),
            MorlocAppExpressionType::Format as usize + 1,
            app
        );

        let kind = header_enum_variants("manifest_arg_kind_t");
        assert_eq!(kind, ["MARG_POS = 0", "MARG_OPT", "MARG_FLAG", "MARG_GRP"]);
        assert_eq!(ManifestArgKind::Grp as usize, kind.len() - 1);
    }

    /// The union in an expression node must be wide enough for every payload
    /// the Rust side can store in it, or a C read of the widest member walks
    /// off the end of the allocation.
    #[test]
    fn expression_union_is_wide_enough() {
        for (c_name, rust_size, rust_align) in [
            (
                "morloc_expression_s",
                size_of::<ExprUnion>(),
                align_of::<ExprUnion>(),
            ),
            (
                "morloc_app_expression_s",
                size_of::<AppFunction>(),
                align_of::<AppFunction>(),
            ),
            (
                "morloc_pattern_s",
                size_of::<PatternFields>(),
                align_of::<PatternFields>(),
            ),
            (
                "morloc_data_s",
                size_of::<DataUnion>(),
                align_of::<DataUnion>(),
            ),
        ] {
            let body = body_of(c_name);
            let decl = declarations(&body)
                .into_iter()
                .find(|d| d.contains('{'))
                .unwrap_or_else(|| panic!("{c_name} declares no nested union"));
            let (ty, _) = split_decl(&decl);
            let (size, align) = size_align_of_decl(&ty);
            assert_eq!(
                (size, align),
                (rust_size, rust_align),
                "{c_name}: the nested union is {size}/{align} in morloc.h but \
                 {rust_size}/{rust_align} in Rust"
            );
        }
    }
}
