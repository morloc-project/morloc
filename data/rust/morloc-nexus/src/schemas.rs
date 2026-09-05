//! Renderer for the Record / Table Schemas blocks shown under
//! per-command `--help` output.
//!
//! The layouts come from the command's `named_types`, which the
//! compiler fills by walking the whole signature. They used to be
//! reconstructed here from each argument's wire schema, keyed on its
//! rendered type name -- which only worked when the named type *was*
//! the whole type. Wrap a record in a list and the name still appeared
//! in the type while its definition silently vanished, because a wire
//! schema cannot carry a morloc type name and `[Hit]` is not a name to
//! key on. The compiler has the type; it says what the names are.
//!
//! The renderer is parser-agnostic -- it walks the manifest, not
//! argv -- and the rendered block is plumbed into clap's
//! `after_help` slot by [`crate::phase2::build_root`] so each
//! per-command `--help` shows the named-type field layouts it
//! references.
//!
//! The public surface is [`render_command_schemas`]: given a
//! manifest [`Command`], it returns the rendered block as an owned
//! `String` (or `None` when the signature names no such type).

use morloc_manifest::{Command, NamedType};

/// Pretty-render a parsed `Schema` as a morloc-flavored type string,
/// suitable for the field-type column in the schemas block.
///
/// `self_ref`, when set, is a `(short_name, full_name)` pair for the
/// record currently being rendered. Any `Recur` back-reference whose
/// short name matches `short_name` is expanded to `full_name` so the
/// parameterization of the outer type (`Container Int`) surfaces on
/// self-referential fields, rather than collapsing to the bare
/// constructor name (`Container`). Parens are added when the
/// parameterized name contains a space so the surrounding context
/// (e.g. an `?` wrapper) binds correctly.
pub(crate) fn render_schema_type(
    s: &morloc_runtime_types::schema::Schema,
    self_ref: Option<(&str, &str)>,
) -> String {
    use morloc_runtime_types::schema::SerialType::*;
    match s.serial_type {
        Nil => "()".into(),
        Bool => "Bool".into(),
        Sint8 => "I8".into(),
        Sint16 => "I16".into(),
        Sint32 => "Int".into(),
        Sint64 => "I64".into(),
        Uint8 => "U8".into(),
        Uint16 => "U16".into(),
        Uint32 => "U32".into(),
        Uint64 => "U64".into(),
        Float32 => "F32".into(),
        Float64 => "F64".into(),
        String => "Str".into(),
        Array => format!(
            "[{}]",
            s.parameters
                .first()
                .map(|p| render_schema_type(p, self_ref))
                .unwrap_or_else(|| "?".into())
        ),
        Tuple => {
            // `String` here is fully qualified because the surrounding
            // match brings `SerialType::String` into scope as a variant,
            // shadowing the std `String` type.
            let inner: Vec<std::string::String> = s
                .parameters
                .iter()
                .map(|p| render_schema_type(p, self_ref))
                .collect();
            format!("({})", inner.join(", "))
        }
        Map => {
            // A nested record-ish thing. Use the hint string when
            // present (which carries the language-specific concrete
            // type name); otherwise show an inline placeholder. The
            // nested record will be listed separately in the same
            // schema block if its name appears as another arg's type.
            s.hint.clone().unwrap_or_else(|| "{..}".into())
        }
        Optional => {
            let inner = s
                .parameters
                .first()
                .map(|p| render_schema_type(p, self_ref))
                .unwrap_or_else(|| "?".into());
            if inner.contains(' ') {
                format!("?({})", inner)
            } else {
                format!("?{}", inner)
            }
        }
        Int => "Int".into(),
        Table => {
            // Table primitive: bare `T` renders as `Table` (any
            // schema); `T:K<entries>` renders as `Table {k1=t1, ...}`
            // so help text shows the declared columns the same way
            // the user wrote them.
            if s.parameters.is_empty() {
                "Table".into()
            } else {
                let cols: Vec<std::string::String> = s
                    .parameters
                    .iter()
                    .enumerate()
                    .map(|(i, p)| {
                        let key = s.keys.get(i).cloned().unwrap_or_default();
                        format!("{}={}", key, render_schema_type(p, self_ref))
                    })
                    .collect();
                format!("Table {{{}}}", cols.join(", "))
            }
        }
        // Recursive back-reference. Wire schemas carry only the bare
        // constructor name here (parameter applications are not
        // encoded), so substitute the outer record's full parameterized
        // display name when the reference points back at it.
        Recur => {
            let raw = s.name.clone().unwrap_or_else(|| "?".into());
            match self_ref {
                Some((short, full)) if raw == short => full.into(),
                _ => raw,
            }
        }
        // Cross-pool stream handles: surface as their user-facing morloc
        // type. The wire is a tagged union of path / handle; help text
        // shows only the type layer.
        IFile => "IFile a".into(),
        OStream => "OStream a".into(),
        IStream => "IStream a".into(),
    }
}

/// Render the Record Schemas / Table Schemas sections for any named
/// types referenced in this command's signature. Returns `None` when
/// there are no named types (so callers can suppress the whole
/// "after help" block).
pub fn render_command_schemas(cmd: &Command) -> Option<String> {
    if cmd.named_types.is_empty() {
        return None;
    }
    let records: Vec<&NamedType> = cmd
        .named_types
        .iter()
        .filter(|t| t.kind != "table" && t.kind != "packable")
        .collect();
    let tables: Vec<&NamedType> =
        cmd.named_types.iter().filter(|t| t.kind == "table").collect();
    let packables: Vec<&NamedType> = cmd
        .named_types
        .iter()
        .filter(|t| t.kind == "packable")
        .collect();

    let mut out = String::new();
    if !records.is_empty() {
        out.push_str("Record Schemas:\n");
        out.push_str(&render_named(&records));
    }
    if !tables.is_empty() {
        if !out.is_empty() {
            out.push('\n');
        }
        out.push_str("Table Schemas:\n");
        out.push_str(&render_named(&tables));
    }
    if !packables.is_empty() {
        if !out.is_empty() {
            out.push('\n');
        }
        out.push_str("Wire Forms:\n");
        out.push_str(&render_packables(&packables));
    }
    // Drop the trailing newline so clap can append its own block
    // separator cleanly.
    if out.ends_with('\n') {
        out.pop();
    }
    Some(out)
}

/// Render types whose definition is a wire form rather than a field
/// list: `Name p1 p2 = <form>`, stated in the constructor's own
/// parameters so one line covers every use of it.
fn render_packables(defs: &[&NamedType]) -> String {
    let mut out = String::new();
    for def in defs {
        let head = if def.parameters.is_empty() {
            def.name.clone()
        } else {
            format!("{} {}", def.name, def.parameters.join(" "))
        };
        out.push_str(&format!("  {} = {}\n", head, def.equals));
    }
    out
}

/// Render a list of named types, one block each: the name on its own
/// line, then its fields with the `::` column aligned.
fn render_named(defs: &[&NamedType]) -> String {
    let mut out = String::new();
    for (i, def) in defs.iter().enumerate() {
        if i > 0 {
            out.push('\n');
        }
        out.push_str(&format!("  {}\n", def.name));
        let width = def.fields.iter().map(|f| f.key.len()).max().unwrap_or(0);
        for f in &def.fields {
            out.push_str(&format!(
                "    {:width$} :: {}\n",
                f.key,
                f.type_desc,
                width = width
            ));
        }
    }
    out
}
